-module(undead_doctor2).
-export([loop/1]).

% Day 3 - Make the Doctor process restart itself should it die
%       - For use with part 2, doctor_monitor monitors this doctor, and 
%       - If either monitor dies, they restart each other

% Doctor monitor, outer loop of sorts
loop(PIDs) ->
    process_flag(trap_exit, true),
    receive
        new ->
            % Make a new doctor
            io:format("Creating and monitoring doctor.~n"),

            % If we've been restarted, we need to check to see if
            % doctor already exists
            case lists:member(doctor, registered()) of
                false ->
                    % If not, create and link as normal
                    Doctor = spawn_link(fun doctor_loop/0),
                    register(doctor, Doctor),
                    doctor ! new,
                    loop(lists:keystore("doctor", 1, PIDs, {"doctor", Doctor}));
                true ->
                    % Otherwise link to its PID and add it to the list
                    link(whereis(doctor)),
                    loop(lists:keystore("doctor", 1, PIDs, {"doctor", whereis(doctor)}))
            end;
        {link, PID} ->
            io:format("Linking to PID: ~p.~n", [PID]),
            link(PID),
            loop(lists:keystore("monitor", 1, PIDs, {"monitor", PID}));
        kill_doctor ->
            % Send a message to the doctor to die for testing
            doctor ! die,
            loop(PIDs);
        kill_self ->
            exit({zombie_doc,die,at,erlang:time()});
        {'EXIT', From, _} ->
            {_, DoctorPID} = lists:keyfind("doctor", 1, PIDs),
            {_, MonitorPID} = lists:keyfind("monitor", 1, PIDs),
            case From of
                DoctorPID ->                    
                    % Doctor died, restart the doctor
                    io:format("The doctor has died, raising him from the dead..~n"),
                    self() ! new,
                    loop(lists:keydelete("doctor", 1, PIDs));
                MonitorPID ->
                    % Monitor died, restart the monitor
                    io:format("Hang on, our monitor died, restarting it.~n"),
                    Monpid = spawn(fun doctor_monitor:loop/0),
                    Monpid ! {link, self()},
                    self() ! {link, Monpid},
                    loop(lists:keydelete("monitor", 1, PIDs))
            end
    end.

% Original Doctor Loop  
doctor_loop() ->
    % Have to monitor the shooter for exiting
    process_flag(trap_exit, true),
    receive
        new ->
            % New doctor was made
            case lists:member(revolver, registered()) of
                true -> 
                    % If revolver was already registered, re-link it to this process
                    io:format("Reattaching to existing shooter.~n"),
                    link(whereis(revolver));
                false ->
                    % Otherwise, spawn and link a new revolver process
                    io:format("Creating and monitoring shooter.~n"),
                    register(revolver, spawn_link(fun roulette:loop/0))
            end,
            doctor_loop();
        die ->
            % We've been told to die, dutifully exit
            io:format("Killing the doctor..~n"),
            exit({doctor,die,at,erlang:time()});
        {'EXIT', _, _} ->
            % Shooter process has exited, restart the process
            io:format("Shooter died, restarting..~n"),
            self() ! new,
            doctor_loop()
    end.
