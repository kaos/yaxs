%%%-------------------------------------------------------------------
%%% File    : yaxs_core_stream.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created : 22 Apr 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(yaxs_core_tls).

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
				"urn:ietf:params:xml:ns:xmpp-tls"
			       ]).

handle(stream_features, 
       #yaxs_client{ response=R, tags=Tags } = _Client) ->
    case proplists:get_value(tls, Tags) of
	ok ->
	    ok;
	_ ->
	    R("<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'>"),
	    R("<required/>"),
	    R("</starttls>")
    end;

handle(#tag{ name="starttls" },
       #yaxs_client{ response=R } = _Client) ->
    R("<proceed xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>"),
    R(reset_stream),
    {tag, {tls, ok}}.


%%====================================================================
%% Internal functions
%%====================================================================
