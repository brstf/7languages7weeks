-module(day1).
-export([word_count/1]).
-export([count/0]).
-export([error/1]).

% Count the number of words in a string using recursion
% Two functions, count/2 and count/1 exist to solve the problem of overcounting
%  if the string starts with a whitespace character.  N keeps track of the number
%  of words.
word_count("", N) -> N;
word_count(S, N)  -> word_count(string:strip(string:substr(S, string:cspan(S, " \t\n\r") + 1)), N + 1).
word_count(S)     -> word_count(string:strip(S), 0).

% Use recursion to count to ten
% ~B in erlang is the equivalent to what I was used to as %d in formatted output
% functions from other languages
count(Max, Max) -> io:format("~B\n", [Max]);
count(C,   Max) -> 
	io:format("~B ", [C]),
	count(C+1, Max).
count() -> count(0, 10).

% Error function: prints error and a message on the input {error, Message}, or
% success if given the input success.
error({error, Message}) -> io:format("error: ~p\n", [Message]);
error(success)          -> io:format("success\n").