%%
%%
-module(socat).
-compile({parse_transform, category}).

-export([main/1]).

%%
%%
opts() ->
   [
      {help,   $h,   "help",     undefined,  "Print usage"}
   ].

%%
%%
main(Args) ->
   {ok, {Opts, Uri}} = getopt:parse(opts(), Args),
   case 
      lists:member(help, Opts)
   of
      true ->
         getopt:usage(opts(), escript:script_name(), "URL"),
         halt(0);
      _    ->
         case main(Opts, Uri) of
            {error, eof} ->
               io_progress(),
               halt(0);
            Error ->
               error_logger:error_report([Error])
         end
   end.

main(Opts, Uri) ->
   [either ||
      application:ensure_all_started(socat),
      cats:unit(clue:define({ewma, 5}, {sent, byte})),
      cats:unit(clue:define({ewma, 5}, {sent, pack})),
      io:setopts([binary]),
      Sock <- socat_stream:start_link(),
      socat_stream:connect(Sock, Uri),
      socat_stdin(Sock, 0, tempus:add(os:timestamp(), 10)),
      socat_stream:close(Sock)
   ].

socat_stdin(Sock, N, T) ->
   [either ||
      stdin(),
      socat_stream:send(Sock, _),
      cats:unit( progress(N, T) ),
      socat_stdin(Sock, N + 1, _)
   ].

stdin() ->
   case file:read_line(standard_io) of
      eof ->
         {error, eof};
      Any ->
         Any
   end.

progress(N, T)
 when N rem 10000 =:= 0 ->
   T;
progress(N, T) ->
   case os:timestamp() of
      X when X > T ->
         io_progress(),
         tempus:add(os:timestamp(), 10);
      _ ->
         T
   end.

io_progress() ->
   error_logger:info_report([
      {{pps, 'K'},         clue:get({sent, pack}) / 1000},
      {{throughput, 'MB'}, clue:get({sent, byte}) / 1024 / 1024}
   ]).


