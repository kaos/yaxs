-module(yaxs).
-export([ 
	  start/0, 
	  stop/0,

	  get_attr/2,
	  get_attr/3,

	  to_jid/1

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

to_jid(To) when is_list(To) ->
    {Node, Rest} = case string:tokens(To, "@") of
		       [N, R1] ->
			   {N, R1};
		       [R2] ->
			   {undefined, R2};
		       _ ->
			   exit({bad_jid, To})
		   end,
    {Domain, Resource} = case string:tokens(Rest, "/") of
			     [D, R] ->
				 {D, R};
			     [D] ->
				 {D, undefined};
			     _ ->
				 exit({bad_jid, To})
			 end,
    #jid{ node=Node, domain=Domain, resource=Resource }.
