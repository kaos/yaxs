%%%-------------------------------------------------------------------
%%% File    : yaxs_core_stream.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created : 22 Apr 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(yaxs_core_stream).

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
    error_logger:info_msg("core init"),
    yaxs_mod:register(?MODULE, [open_stream]).

handle({open_stream, Attrs}, Client) ->
    error_logger:info_msg("core open_stream: ~p~n", [Attrs]),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
