#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname ct

main(_Args) ->
    application:start(sasl),
    application:start(erlsom),
    code:add_path(filename:absname("../ebin")),
    {ok, Spec} = file:consult("test.spec"),
    ct:run_testspec(Spec).
