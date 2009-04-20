-module(yaxs).
-behaviour(application).
-export([ 
	  start/2, 
	  stop/1

	  %% API
%	  register_feature/1
	 ]).

%-include("xmpp.hrl").

start(_Type, _StartArgs) ->
    yaxs_sup:start_link([]).

stop(_State) ->
    ok.


%% API

%% register_feature(F) when is_record(F, feature) ->
%%     xmpp_core_stream_feature:register(F);
%% register_feature(F) ->
%%     register_feature(
%%       #feature{ states = proplists:get_value(
%% 			   states, F, [bound]),
%% 		module = proplists:get_value(
%% 			   module, F),
%% 		namespace = proplists:get_value(
%% 			      namespace, F, 'jabber:client')
%% 	       }).


