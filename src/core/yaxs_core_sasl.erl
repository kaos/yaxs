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

handle(stream_features,
       #yaxs_client{ response=R, tags=Tags } = Client) ->
    case proplists:get_value(sasl, Tags) of
	ok ->
	    ok;
	_ ->
	    R("<mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>"),
	    Res = yaxs_event:publish(sasl_mechanisms, Client),
	    R("</mechanisms>"),
	    Res
    end;

handle(#tag{ name="auth", attrs=Attrs } = Event, Client) ->
    [Mechanism] = [list_to_atom("SASL/" ++ Val)
		   || #attribute{ name=Key, value=Val } <- Attrs,
		      Key == "mechanism"],
    Res = yaxs_event:publish({Mechanism, Event}, Client),
    [{tag, {mechanism, Mechanism}}|Res].



%%====================================================================
%% Internal functions
%%====================================================================
