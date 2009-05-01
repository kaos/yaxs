%%%-------------------------------------------------------------------
%%% File    : yaxs_core_jabberclient.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created : 27 Apr 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(yaxs_core_iq).

-include("yaxs.hrl").

%% API
-behaviour(yaxs_mod).
-export([
	 init/0,
	 handle/2
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

init() ->
    yaxs_mod:register(?MODULE, [
				"iq"
			       ]).

handle(#stanza{ id=Id, body=[Tag] },
       #yaxs_client{ response=R } = Client) ->
    
    PubRes = yaxs_event:publish(Tag, Client),
    IqRes = [Res || Res <- PubRes,
		    is_tuple(Res),
		    tuple_size(Res) == 2,
		    element(1, Res) == result 
			orelse element(1, Res) == error
		     ],
    [{Type, Res}] = IqRes,

    R(io_lib:format(
	"<iq type='~s' id='~s'>"
	"~s"
	"</iq>",
	[Type, Id, Res]
       )
     ),
    PubRes -- IqRes.


%%====================================================================
%% Internal functions
%%====================================================================
