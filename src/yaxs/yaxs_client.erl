%%%-------------------------------------------------------------------
%%% File    : yaxs_client.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created : 21 Apr 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(yaxs_client).

-behaviour(gen_fsm).

%% API
-export([start_link/0, set_socket/2, sax_event/2]).

%% gen_fsm callbacks
-export([
	 init/1, 
	 handle_event/3,
	 handle_sync_event/4, 
	 handle_info/3, 
	 terminate/3, 
	 code_change/4
	]).

%% FSM states
-export([
	 wait_for_socket/2,
	 wait_for_stream/2,
	 setup_stream/2,
	 streaming/2,
	 stopped/2
	]).

-define(SERVER, ?MODULE).

-include("yaxs.hrl").

-record(state, {
	  sax,
	  client = #yaxs_client{},
	  response=[],
	  open_tags=[]
	 }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Sock) ->
    gen_fsm:send_event(Pid, {socket_ready, Sock}).

sax_event(Pid, Event) ->
    gen_fsm:send_event(Pid, {sax, Event}).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([]) ->
    Pid=self(),
    {ok, wait_for_socket,
     #state{ 
	   client=#yaxs_client{ 
	     pid=Pid,
	     response=fun(reset_stream) -> gen_fsm:send_event(Pid, reset_stream);
			 ({route, _}=Stanza) -> gen_fsm:send_event(Pid, Stanza);
			 ({jid, _}=Jid) -> gen_fsm:send_event(Pid, Jid);
			 (Data) -> gen_fsm:send_all_state_event(Pid, {response, Data}) end
	    }
	  }
    }.

%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------
wait_for_socket({socket_ready, Sock}, #state{ client=Client} = State) ->
    ok = inet:setopts(Sock, [{active, once}]),
    {ok, {IP, Port}} = inet:peername(Sock),
    Addr = io_lib:format("~s:~p", [inet_parse:ntoa(IP), Port]),
    error_logger:info_msg("Client connected: ~s~n", [Addr]),
    
    {next_state, wait_for_stream,
     State#state{
       client = Client#yaxs_client{
		  sock = Sock,
		  addr = Addr
		 }
      }
    }.

wait_for_stream({sax, {open, {"http://etherx.jabber.org/streams"=NS, 
			      "stream"=P, "stream"=N, Attrs}
		      }
		}, 
		#state{ open_tags = [Tag] } = State) ->

    Tag1 = Tag#tag{ namespace=NS,
		    prefix=P,
		    name=N,
		    attrs=Attrs },
    
    State1 = publish(Tag1, State#state{ open_tags = [Tag1] }),
    {next_state, setup_stream, State1};

wait_for_stream({sax, _Event}, State) ->
    {next_state, wait_for_stream, State}.

setup_stream({jid, Jid}, #state{ client=Client } = State) ->
    {next_state, setup_stream,
     State#state{ 
       client=Client#yaxs_client{ 
		jid=Jid 
	       } 
      }
    };

setup_stream({route, #stanza{ xml = Data }}, #state{ client=#yaxs_client{ sock=S } } = State ) ->
    gen_tcp:send(S, Data),
    {next_state, setup_stream, State};

setup_stream(reset_stream, #state{ client=#yaxs_client{ tags=Tags }=Client } = State) ->
    {next_state, wait_for_stream, 
     State#state{ sax=undefined, 
		  open_tags=[],
		  client=Client#yaxs_client{ tags=proplists:delete(domain, Tags) }
		 }
    };

setup_stream({sax, {open, {NS, P, N, A}}}, #state{ open_tags = [Tag|Tags] } = State) ->
    Tags1 = case Tag#tag.name of
		undefined ->
		    [Tag#tag{ namespace=NS, prefix=P, name=N, attrs=A }
		     |Tags];
		_ ->
		    [#tag{ namespace=NS, prefix=P, name=N, attrs=A },
		     Tag | Tags]
	    end,
    {next_state, setup_stream, State#state{ open_tags = Tags1 }};

setup_stream({sax, {close, _Tag}}, #state{ open_tags = [OTag|Tags] } = State) ->
    State1 = case Tags of
		 [] -> State;
		 [_] ->
		     publish(OTag, State#state{ open_tags = Tags });
		 [#tag{ body=B } = Parent|T] ->
		     State#state{ open_tags = [Parent#tag{ body = B ++ [OTag] }|T] }
	     end,
    {next_state, setup_stream, State1};

setup_stream({sax, {characters, Data}}, #state{ open_tags = [#tag{ body=B } = C|T] } = State) ->
    {next_state, setup_stream, State#state{ open_tags = [C#tag{ body = B ++ [Data] }|T] }};

setup_stream({sax, close}, #state{ client=Client } = State) ->
    ok = gen_tcp:send(Client#yaxs_client.sock, "</stream:stream>"),
    {next_state, stopped, State, 100};

setup_stream({sax, _Event}, State) ->
    {next_state, setup_stream, State}.

streaming({sax, close}, #state{ client=Client } = State) ->
    ok = gen_tcp:send(Client#yaxs_client.sock, "</stream:stream>"),
    {next_state, stopped, State, 100}.

stopped(timeout, State) ->
    {stop, normal, State};
stopped(_, State) ->
    {next_state, stopped, State, 100}.


%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, 
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, 
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
%% state_name(_Event, _From, State) ->
%%     Reply = ok,
%%     {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event({data, Data},
	     StateName,
	     #state{ 
	       client = #yaxs_client{ sock=Sock },
	       open_tags = Tags
	      } = State) ->
    
    inet:setopts(Sock, [{active, once}]),
    try
	{next_state, StateName, 
	 State#state{ 
	   sax = yaxs_sax:parse(Data, 
				fun sax_event/2,
				State#state.sax),
	   open_tags = [#tag{ data=Data }|Tags]
	  }
	}
    catch
	throw:Error ->
	    {stop, {sax_error, Error}, State}
    end;

    
handle_event(_, stopped, State) ->
    {next_state, stopped, State, 100};

handle_event(send_response, StateName, #state{ response=Response, client=Client } = State) ->
    gen_tcp:send(Client#yaxs_client.sock, lists:reverse(Response)),
    {next_state, StateName, State#state{ response=[] }};

handle_event({response, Data}, StateName, #state{ response=Response } = State) ->
    {next_state, StateName, State#state{ response=[Data|Response] }};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info({tcp, _Sock, Data}, StateName, State) ->
    gen_fsm:send_all_state_event(self(), {data, Data}),
    {next_state, StateName, State};

handle_info({tcp_closed, _Sock}, _StateName, State) ->
    error_logger:info_msg("Client disconnected: ~s~n", [(State#state.client)#yaxs_client.addr]),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, State) ->
    error_logger:info_msg("Terminate yaxs_client in state ~p.~nReason: ~p~nState:~p~n", 
			  [_StateName, _Reason, State]),
    case (State#state.client)#yaxs_client.sock of
	undefined ->
	    ok;
	Sock ->
	    catch gen_tcp:close(Sock),
	    ok
    end.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

publish(Event, #state{ client=#yaxs_client{ tags=Tags } = Client } = State) ->
    NewTags = [element(2, Tag) || Tag <- yaxs_event:publish(Event, Client),
				  is_tuple(Tag),
				  tuple_size(Tag) == 2,
				  element(1, Tag) == tag
				     ],
    gen_fsm:send_all_state_event(self(), send_response),
    State#state{ client = Client#yaxs_client{ tags=lists:flatten(NewTags) ++ Tags }}.
