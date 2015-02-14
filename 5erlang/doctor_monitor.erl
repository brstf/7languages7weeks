-module(doctor_monitor).
-export([init/0, loop/0]).

% Day 3 - Make a monitor for the Doctor monitor. If either monitor dies, restart it

init() ->
    % Start a doctor monitor 
    self() ! new,
    loop().

% Doctor monitor monitor loop
loop() ->
    process_flag(trap_exit, true),
    receive
        new -> 
            % Create and attach to a new zombie doctor
            register(zombie_doc, spawn_link(undead_doctor2, loop, [[]])),
            zombie_doc ! new,
            zombie_doc ! {link, self()},
            loop();
        {link, PID} ->
            % Before registering, make sure the atom zombie_doc is not already
            % registered
            case lists:member(zombie_doc, registered()) of
                false ->
                    register(zombie_doc, PID);
                true ->
                    ok
            end,
            link(whereis(zombie_doc)),
            loop();
        kill_doctor ->
            % For testing, kill the doctor
            zombie_doc ! kill_doctor,
            loop();
        kill_zombie_doc ->
            % For testing, kill the doctor's necromancer
            zombie_doc ! kill_self,
            loop();
        kill_self ->
            % For testing, kill this process
            exit({doctor_monitor,die,at,erlang:time()});
        {'EXIT', _, _} ->
            % The zombie doctor died, restart it
            io:format("Zombie doctor monitor died, restarting..~n"),
            self() ! new,
            loop()
    end.