%%%-------------------------------------------------------------------
%%% File    : yaxs_con.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created : 21 Apr 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(yaxs_con).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	  sock,
	  accept,
	  client
	 }).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Port, Module) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port, Module], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Port, Module]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, 
			[list, 
			 {active, false}, 
			 {reuseaddr, true}
			]) of
	{ok, Sock} ->
	    {ok, Ref} = prim_inet:async_accept(Sock, -1),
	    {ok, #state{ sock=Sock,
			 accept=Ref,
			 client=Module
			}};
	{error, Reason} ->
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({inet_async, Sock, _Ref, {ok, Client}}, State) ->
    try
	case set_sockopt(Sock, Client) of
	    ok ->
		ok;
	    {error, Reason} ->
		exit({set_sockopt, Reason})
	end,
	
	{ok, Pid} = supervisor:start_child(yaxs_client_sup, []),
	sys:trace(Pid, true),
	gen_tcp:controlling_process(Client, Pid),
	(State#state.client):set_socket(Pid, Client),
	
	case prim_inet:async_accept(Sock, -1) of
	    {ok, NewRef} ->
		{noreply, State#state{ accept=NewRef }};
	    {error, Error} ->
		exit({async_accept, inet:format_error(Error)})
	end
	
    catch 
	exit:Why ->
	    error_logger:error_msg("Error in async accept: ~p.~n", [Why]),
	    {stop, Why, State}
    end;

handle_info({inet_async, _Sock, _Ref, Error}, State) ->
    error_logger:error_msg("Error in async accept: ~p.~n", [Error]),
    {stop, Error, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    error_logger:info_msg("Terminate yaxs_con.~nReason: ~p~n", 
			  [_Reason]),
    gen_tcp:close(State#state.sock),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

set_sockopt(Sock, Client) ->
    true = inet_db:register_socket(Client, inet_tcp),
    case prim_inet:getopts(Sock, [active, 
				  nodelay,
				  keepalive,
				  delay_send,
				  priority,
				  tos]) of
	{ok, Opts} ->
	    case prim_inet:setopts(Client, Opts) of
		ok ->
		    ok;
		Error ->
		    gen_tcp:close(Client),
		    Error
	    end;
	Error ->
	    gen_tcp:close(Client),
	    Error
    end.
