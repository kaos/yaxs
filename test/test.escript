#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname ct

main(_Args) ->
    code:add_path(filename:absname("../ebin")),
    application:start(sasl),
    application:start(erlsom),
    application:start(yaxs),

    {ok, Spec} = file:consult("test.spec"),
    ct:run_testspec(Spec).
