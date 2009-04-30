%%%-------------------------------------------------------------------
%%% File    : yaxs_sup.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created : 21 Apr 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(yaxs_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [?SERVER, 5222, yaxs_client]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([client_sup, Module]) ->
    {ok, 
     {
       {simple_one_for_one, 0, 1},
       [{undefined,
	 {Module, start_link, []},
	 temporary,
	 2000,
	 worker,
	 []
	}]
      }
    };

init([?SERVER, Port, Module]) ->
    {ok,
     {
       {one_for_one, 0, 1}, 
       [
	{client_sup,
	 {supervisor, start_link, [{local, yaxs_client_sup}, ?MODULE, [client_sup, Module]]},
	 permanent,
	 infinity,
	 supervisor,
	 []
	},
	{con,
	 {yaxs_con, start_link, [Port, Module]},
	 permanent,
	 2000,
	 worker,
	 [yaxs_con]
	},
	{event,
	 {yaxs_event, start_link, []},
	 permanent,
	 2000,
	 worker,
	 [yaxs_event]
	},
	{core,
	 {yaxs_core, start_link, []},
	 permanent,
	 2000,
	 worker,
	 [yaxs_core]
	}
       ]
      }
    }.

%%====================================================================
%% Internal functions
%%====================================================================
