%%%-------------------------------------------------------------------
%%% File    : yaxs_event.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created : 22 Apr 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(yaxs_event).

-behaviour(gen_server).

%% API
-export([
	 start_link/0,

	 register/2,
	 publish/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("yaxs.hrl").

-record(state, {
%	  mods = [],
	  events = []
	 }).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register(Module, Events) ->
    gen_server:cast(?SERVER, {register, Module, Events}).

publish(Event, Client) when is_record(Event, tag) ->
    Mods = list_mods(element(1, Event#tag.tag)),
    do_publish(Event, Client, Mods);
publish(Event, Client) when is_tuple(Event) ->
    Mods = list_mods(element(1, Event)),
    do_publish(Event, Client, Mods);
publish(Event, Client) when is_atom(Event) ->
    Mods = list_mods(Event),
    do_publish(Event, Client, Mods).


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
    {ok, #state{}, 0}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({list_mods, Event}, _From, #state{ events=Events } = State) ->
    Mods = [Mod || {E, Mod} <- Events, E == Event],
    {reply, Mods, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({register, Module, Events}, #state{ events=ModsEvents } = State ) ->
    ModEvents = [{Event, Module} || Event <- Events],
    {noreply, State#state{ events=ModsEvents++ModEvents }};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    yaxs_mod:init(),
    {noreply, State};

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
    error_logger:info_msg("Terminate yaxs_event.~nReason: ~p~n", 
			  [_Reason]),
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

list_mods(Event) ->
    gen_server:call(?SERVER, {list_mods, Event}).

do_publish(Event, Client, Mods) ->
    error_logger:info_msg("do publish:~nEvent:~p~nClient:~p~nMods:~p~n", [Event, Client, Mods]),
    [element(2, Tag) || Tag <- lists:flatten(
				 [Mod:handle(Event, Client) || Mod <- Mods]
				),
			is_tuple(Tag),
			tuple_size(Tag) == 2,
			element(1, Tag) == tag
			   ].
