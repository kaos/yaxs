%%%-------------------------------------------------------------------
%%% File    : core_SUITE.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created :  7 May 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(core_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{minutes,10}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.
%%     case application:start(yaxs) of
%%  	ok ->
%%  	    Config;

%% 	{error, {already_started, yaxs}} ->
%% 	    Config;

%%  	Res ->
%%  	    ct:fail( Res )
%%     end.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.
%%     case application:stop(yaxs) of
%%  	ok ->
%%  	    ok;
	
%%  	Res ->
%%  	    ct:fail( Res )
%%     end.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: sequences() -> Sequences
%%
%% Sequences = [{SeqName,TestCases}]
%% SeqName = atom()
%%   Name of a sequence.
%% TestCases = [atom()]
%%   List of test cases that are part of the sequence
%%
%% Description: Specifies test case sequences.
%%--------------------------------------------------------------------
sequences() -> 
    [].

%%--------------------------------------------------------------------
%% Function: all() -> TestCases | {skip,Reason}
%%
%% TestCases = [TestCase | {sequence,SeqName}]
%% TestCase = atom()
%%   Name of a test case.
%% SeqName = atom()
%%   Name of a test case sequence.
%% Reason = term()
%%   The reason for skipping all test cases.
%%
%% Description: Returns the list of test cases that are to be executed.
%%--------------------------------------------------------------------
all() -> 
    [
     'core-5.3-stream-attrs',
     'core-5.4-stream-features'
    ].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Test case info function - returns list of tuples to set
%%              properties for the test case.
%%
%% Note: This function is only meant to be used to return a list of
%% values, not perform any other operations.
%%--------------------------------------------------------------------
%a_test_case() -> 
%    [].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% Description: Test case function. (The name of it must be specified in
%%              the all/0 list for the test case to be executed).
%%--------------------------------------------------------------------
'core-5.3-stream-attrs'(Config) -> 
    {ok, Sock} = gen_tcp:connect("localhost", 5222, [list, {active, false}]),
    {ok,{"{http://etherx.jabber.org/streams}stream",
	 [{"version","1.0"},
	  {"id","someid"},
	  {"from","example.com"}],
	 _Body
%% 	 [{"{http://etherx.jabber.org/streams}features",[],
%% 	   [{"{urn:ietf:params:xml:ns:xmpp-tls}starttls",[],
%% 	     [{"{urn:ietf:params:xml:ns:xmpp-tls}required",[],
%% 	       []}]},
%% 	    {"{urn:ietf:params:xml:ns:xmpp-sasl}mechanisms",[],
%% 	     [{"{urn:ietf:params:xml:ns:xmpp-sasl}mechanism",[],
%% 	       ["DIGEST-MD5"]}]}]}]
	},
     []}
	= erlsom:simple_form(
	    send(Sock, open_stream())
	    ++ "</stream:stream>"
	   ),
    ok = gen_tcp:close(Sock).

'core-5.4-stream-features'(Config) -> 
    {ok, Sock} = gen_tcp:connect("localhost", 5222, [list, {active, false}]),
    {ok,{"{http://etherx.jabber.org/streams}stream",
	 _Attrs,
%% 	 [{"version","1.0"},
%% 	  {"id","someid"},
%% 	  {"from","example.com"}],

 	 [{"{http://etherx.jabber.org/streams}features",[],
 	   [{"{urn:ietf:params:xml:ns:xmpp-tls}starttls",[],
 	     [{"{urn:ietf:params:xml:ns:xmpp-tls}required",[],
 	       []}]},
 	    {"{urn:ietf:params:xml:ns:xmpp-sasl}mechanisms",[],
 	     [{"{urn:ietf:params:xml:ns:xmpp-sasl}mechanism",[],
 	       ["DIGEST-MD5"]}]}]}]
	},
     []}
	= erlsom:simple_form(
	    send(Sock, open_stream())
	    ++ "</stream:stream>"
	   ),
    ok = gen_tcp:close(Sock).


%% Utils

open_stream() ->
    "<?xml version='1.0'?>"
	"<stream:stream"
	" to='example.com'"
	" xmlns='jabber:client'"
	" xmlns:stream='http://etherx.jabber.org/streams'"
	" version='1.0'>".


send(Sock, Data) ->
    ok = gen_tcp:send(Sock, Data),
    case gen_tcp:recv(Sock, 0, 1000) of
	{ok, Resp} ->
	    Resp;
	{error, Why} ->
	    exit({bad_response, Why, {request, Data}})
    end.
