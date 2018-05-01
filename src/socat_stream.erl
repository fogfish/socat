%%
-module(socat_stream).
-behaviour(pipe).

-export([
   start_link/0,
   init/1,
   free/2,
   idle/3,
   pipe/3,

   connect/2,
   send/2,
   close/1
]).

%%%------------------------------------------------------------------
%%%
%%% Interface
%%%
%%%------------------------------------------------------------------   


connect(Sock, Uri) ->
   pipe:call(Sock, {connect, Uri}, 30000).

send(Sock, Pckt) ->
   pipe:call(Sock, {packet, Pckt}, infinity).

close(Sock) ->
   pipe:free(Sock).


%%%------------------------------------------------------------------
%%%
%%% Factory
%%%
%%%------------------------------------------------------------------   

-record(idle, {pipe}).
-record(pipe, {sock, pipe}).

start_link() ->
   pipe:start_link(?MODULE, [], []).

init([]) ->
   {ok, idle, #idle{}}.

free(_, #pipe{sock = Sock}) ->
   knet:close(Sock);

free(_, #idle{}) ->
   ok.
 
%%%------------------------------------------------------------------
%%%
%%% State machine
%%%
%%%------------------------------------------------------------------   

idle({connect, Uri}, Pipe, #idle{}) ->
   {ok, _} = knet:connect(Uri, [
      {active,  1024},
      {sndbuf,  256 * 1024 * 1024},
      {recbuf,   64 * 1024 * 1024},
      {queue,    10 * 1024}
   ]),
   {next_state, idle, #idle{pipe = Pipe}};

idle({_, Sock, {established, _}}, _, #idle{pipe = Pipe}) ->
   pipe:ack(Pipe, ok),
   {next_state, pipe, #pipe{sock = Sock}};

idle({ws, Sock, {101, _, _}}, _, #idle{pipe = Pipe}) ->
   pipe:ack(Pipe, ok),
   {next_state, pipe, #pipe{sock = Sock}};

idle({_, _, {error, Reason}}, _, #idle{pipe = Pipe} = State) ->
   pipe:ack(Pipe, {error, Reason}),
   {stop, Reason, State}.

%%
%%
pipe({_, Sock, passive}, _, #pipe{} = State) ->
   knet:ioctl(Sock, {active, 1024}),
   {next_state, pipe, State};

pipe({_, _, eof}, _, #pipe{} = State) ->
   {stop, normal, State};

pipe({_, _, {error, Reason}}, _, #pipe{} = State) ->
   {stop, Reason, State};

pipe({_, _, _}, _, #pipe{} = State) ->
   {next_state, pipe, State};

pipe({packet, Pckt}, Pipe, #pipe{sock = Sock} = State) ->
   pipe:a(Pipe, knet:send(Sock, Pckt)),
   clue:inc({sent, pack}),
   clue:inc({sent, byte}, size(Pckt)),
   {next_state, pipe, State#pipe{pipe = Pipe}}.

