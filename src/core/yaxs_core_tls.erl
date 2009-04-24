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
    yaxs_mod:register(?MODULE, [stream_features]).

handle(stream_features, #yaxs_client{ response=R } = _Client) ->
    R("<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'>"),
    R("<required/>"),
    R("</starttls>").


%%====================================================================
%% Internal functions
%%====================================================================
