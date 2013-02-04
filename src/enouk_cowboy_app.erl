-module(enouk_cowboy_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
  ok = application:start(lager),
  ok = application:start(cowboy),
  ok = application:start(enouk_cowboy).

start(_StartType, _StartArgs) ->
%% {Host, list({Path, Handler, Opts})}
%% Dispatch the requests (whatever the host is) to
%% erws_handler, without any additional options.
  Dispatch = [{'_', [
    {'_', enouk_cowboy_websocket, []}
  ]}],
%% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
%% Listen in 10100/tcp for http connections.
  cowboy:start_listener(enouk_cowboy_websocket, 100,
    cowboy_tcp_transport, [{port, 10100}],
    cowboy_http_protocol, [{dispatch, Dispatch}]
  ),

enouk_cowboy_sup:start_link().

stop(_State) ->
  ok.
