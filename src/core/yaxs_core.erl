%%%-------------------------------------------------------------------
%%% File    : yaxs_core.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created : 29 Apr 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(yaxs_core).

-behaviour(gen_server).

%% API
-export([
	 start_link/0,

	 new_session/2,
	 route_stanza/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DICT, orddict).

-include("yaxs.hrl").

-record(session, {
	  pid,
	  ref,
	  jid,
	  cb
	 }).

-record(state, {
	  sessions = ?DICT:new()
	 }).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_session(Jid, Cb) ->
    gen_server:call(?SERVER, #session{ pid=self(), jid=Jid, cb=Cb }).

route_stanza(Stanza) ->
    gen_server:cast(?SERVER, {route, Stanza}).

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
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(#session{ pid=Pid, jid=#jid{ node=Node } } = Session,
	    _From,
	    #state{ sessions=Sessions } = State) ->

    error_logger:info_msg("new_session: ~p~n", [Session]),
    State1 = State#state{
	       sessions=?DICT:append(
			   Node,
			   Session#session{
			     ref=erlang:monitor(
				   process,
				   Pid)
			    },
			   Sessions
			  )
	      },
    
    % May want to return a {error, error-type} here...
    %Res = {error, conflict},
    Res = ok,
    {reply, Res, State1};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({route, #stanza{ to=To } = Stanza}, #state{ sessions=Sessions } = State) ->
    case find_session(To, Sessions) of
	no_session ->
	    error_logger:info_msg("can't route: ~p~n", [Stanza]);
	#session{ cb=F } ->
	    error_logger:info_msg("route: ~p~n", [Stanza]),
	    F({route, Stanza})
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', Ref, process, _Pid, _Reason},
	    #state{ sessions=Sessions } = State) ->
    State1 = case lists:keytake(Ref, #session.ref, Sessions) of
		 {value, Session, S1} ->
		     error_logger:info_msg("End session: ~p~nReason: ~p~n", 
					   [Session, _Reason]),
		     S1;
		 false ->
		     State
	     end,
    {noreply, State1};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
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

find_session(Jid, Dict) ->
    case ?DICT:find(Jid#jid.node, Dict) of
	{ok, Sessions} when length(Sessions) > 0 ->
	    case Jid#jid.resource of
		undefined ->
		    hd(Sessions);
		_ ->		    
		    case [S || S <- Sessions,
			       S#session.jid == Jid] of
			[S|_] ->
			    S;
			[] ->
			    no_session
		    end
		end;
	{ok, _} ->
	    no_session;
	error ->
	    no_session
    end.
