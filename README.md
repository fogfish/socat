# socat

Erlang native command line utility to cat files via network socket.


## Key features

* Input text content via standard i/o, each line is transmitted as separate packet
* Output to plain socket either `tcp://` or `ssl://`
* Output to web socket either plain `ws://` or secure `wss://`


## Getting started


```
cat FILE | socat URL
```

The output endpoint is defined by URL. Its schema identifiers protocol, authority host and port and path is the endpoint.


```
cat file.txt | socat ws://localhost:8080/ws
```

