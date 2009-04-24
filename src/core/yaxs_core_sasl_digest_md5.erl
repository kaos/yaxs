%%%-------------------------------------------------------------------
%%% File    : yaxs_core_stream.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created : 22 Apr 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(yaxs_core_sasl_digest_md5).

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
				sasl_mechanisms,
				'SASL/DIGEST-MD5'
			       ]).

handle(sasl_mechanisms, #yaxs_client{ response=R } = _Client) ->
    R("<mechanism>DIGEST-MD5</mechanism>");
handle({'SASL/DIGEST-MD5', _Tag}, 
       #yaxs_client{ response=R } = _Client) ->
    R("challenge").


%%====================================================================
%% Internal functions
%%====================================================================
