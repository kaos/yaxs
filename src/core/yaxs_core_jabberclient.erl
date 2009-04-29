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

-import(yaxs, [get_attr/2, get_attr/3]).

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
	     name = Kind, 
	     attrs = Attrs,
	     body = Body },
       #yaxs_client{ response=_R } = Client) ->

    Stanza = #stanza{ kind = Kind,
		      type = get_attr("type", Attrs),
		      to = get_attr("to", Attrs, local),
		      from = get_attr("from", Attrs),
		      id = get_attr("id", Attrs),
		      xml_lang = get_attr("xml:lang", Attrs),
		      body = Body },

    case Stanza#stanza.to of
	local ->
	    yaxs_event:publish(Stanza, Client);
	_ ->
	    yaxs_core:route_stanza(Stanza, Client)
    end.


%%====================================================================
%% Internal functions
%%====================================================================

