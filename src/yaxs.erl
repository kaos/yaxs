-module(yaxs).
-export([ 
	  start/0, 
	  stop/0,

	  get_attr/2,
	  get_attr/3

	 ]).

-include("yaxs.hrl").

start() ->
    application:start(yaxs).

stop() ->
    application:stop(yaxs).

get_attr(Key, Attrs) -> get_attr(Key, Attrs, undefined).
get_attr(Key, Attrs, Default) ->
    case [V || #attribute{ name=K, value=V } <- Attrs, K == Key] of
	[Val] ->
	    Val;
	_ ->
	    Default
    end.

