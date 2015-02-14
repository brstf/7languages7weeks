-module(undead_doctor).
-export([loop/0]).

% Day 2 - Make the Doctor process restart itself should it die

% Doctor monitor, outer loop of sort
loop() ->
    process_flag(trap_exit, true),
    receive
        new ->
            % Make a new doctor
            io:format("Creating and monitoring doctor.~n"),
            register(doctor, spawn_link(fun doctor_loop/0)),
            doctor ! new,
            loop();
        kill_doctor ->
            % Send a message to the doctor to die for testing
            doctor ! die,
            loop();
        {'EXIT', _, _} ->
            % Doctor died, restart the doctor
            io:format("The doctor has died, raising him from the dead..~n"),
            self() ! new,
            loop()
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
