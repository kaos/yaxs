%%%-------------------------------------------------------------------
%%% File    : yaxs_core_stream.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created : 22 Apr 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(yaxs_core_sasl).

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
				stream_features,
				"urn:ietf:params:xml:ns:xmpp-sasl"
			       ]).

handle(stream_features, #yaxs_client{ response=R } = Client) ->
    R("<mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>"),
    Tags = yaxs_event:publish(sasl_mechanisms, Client),
    R("</mechanisms>"),
    {tag, Tags};

handle(#tag{ tag = {"urn:ietf:params:xml:ns:xmpp-sasl", 
		    "", "auth", Attrs} } = Event, Client) ->
    [Mechanism] = [list_to_atom(
		     "SASL/" ++ Val) || {attribute, Key, _, _, Val} <- Attrs,
					Key == "mechanism"],
    Tags = yaxs_event:publish({Mechanism, Event}, Client),
    {tag, [{mechanism, Mechanism}|Tags]}.



%%====================================================================
%% Internal functions
%%====================================================================
