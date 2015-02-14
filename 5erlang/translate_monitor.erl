-module(translate_monitor).
-export([loop/0]).

loop() ->
    process_flag(trap_exit, true),
    receive
        new ->
            io:format("Creating and monitoring translator.~n"),
            register(translator, spawn_link(fun translate_service:loop/0)),
            loop();
        {'EXIT', From, Reason} ->
            io:format("The translator ~p exited with reason ~p.", [From, Reason]),
            io:format(" Restarting.~n"),
            self() ! new,
            loop()
    end.