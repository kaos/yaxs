%%%-------------------------------------------------------------------
%%% File    : yaxs_core_jabberclient.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created : 27 Apr 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(yaxs_core_jabberclient).

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
				"jabber:client"
			       ]).

handle(#tag{ namespace = "jabber:client",
	     name = Type, 
%	     attrs = Attrs,
	     body = Body },
       #yaxs_client{ response=_R } = Client) ->
    
    yaxs_event:publish(#stanza{ type=Type, body=Body}, Client).


%%====================================================================
%% Internal functions
%%====================================================================
