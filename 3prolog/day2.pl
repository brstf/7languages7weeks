% Reverse the elements of a list
reverseList([], []).
reverseList([Head|[]], [Head]).
reverseList([Head|Tail], Reversed) :- 
        append(Reversed2, [Head], Reversed), reverseList(Tail, Reversed2).

% Find the smallest element of a list
smallest([Head], Head).
smallest([Head|Tail], Head) :- smallest(Tail, M), Head =< M.
smallest([Head|Tail], M) :- smallest(Tail, M), M =< Head.

% Unifies with the base case statement if there's only one element
%   in the list, Head is the smallest element.
% Unifies with the first rule if Head is smaller than the smallest 
%   element in Tail.
% Unifies with the second rule if Head is not smaller than the smallest 
%   element in Tail.

% Sort the elements of a list
% Remove a single element of a list
% Remove rem(X, L1, L2) -
%       unifies if L2 is L1 with the first instance of X removed
rem(X, [X|Tail], Tail).
rem(X, [Y|Tail], [Y|Tail1]) :- rem(X, Tail, Tail1).

% Do a simple selection sort to sort the list
sortList([], []).
sortList([Head|[]], [Head]).
sortList(L, [H2|T2]) :- smallest(L, H2), rem(H2, L, L2), sortList(L2, T2).
