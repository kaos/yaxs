%%%-------------------------------------------------------------------
%%% File    : yaxs_core_stream.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created : 22 Apr 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(yaxs_core_bind).

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
				"urn:ietf:params:xml:ns:xmpp-bind"
			       ]).

handle(stream_features, 
       #yaxs_client{ response=R, tags=Tags } = _Client) ->
    case proplists:get_value(sasl, Tags) of
	ok ->
	    R("<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>");
	_ ->
	    ok
    end;

handle(#tag{ name="bind", body=Body },
       #yaxs_client{ response=_R, tags=Tags } = Client ) ->
    case proplists:get_value(bind, Tags) of
	undefined ->
	    bind_to_resource(Body, Client);
	Res ->
	    {tag, {already_bound, Res}}
    end.


%%====================================================================
%% Internal functions
%%====================================================================

bind_to_resource([#tag{ name="resource", 
			body=[Resource] }],
		 _Client) ->
    {tag, {bind, Resource}};
bind_to_resource(_, _Client) ->
    {tag, {bind, "generated-resource-name"}}.

