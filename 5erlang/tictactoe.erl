-module(tictactoe).
-export([check_board/1]).
-export([count_list/1]).

% function to check the given tic tac toe board.
% return x if x is a winner, o if o is a winner, cat if there are no 
%  winners and no more valid movies, and no_winner if there is no winner
check_board(Board) -> 
    Indexed_list = tuple_lists(count_list(9), Board),
    % Gather all Rows, Columns, and Diagonals into a list (Lists)
    Rows  = [[ V || {I, V} <- Indexed_list, I < Mx, I >= Mn ] || {Mn, Mx} <- [{0,3}, {3,6}, {6,9}] ],
    Cols  = [[ V || {I, V} <- Indexed_list, (I + Mod) rem 3 == 0 ] || Mod <- [0, 1, 2] ],
    Diags = [[ V || {I, V} <- Indexed_list, lists:member(I, Idxs) ] || Idxs <- [[0,4,8],[2,4,6]] ],
    Lists = lists:append(lists:append(Rows, Cols), Diags),

    % Function to test if each element in a list is the same
    SameF = fun(X, Same) -> 
        case X of
            Same -> X;
            _ -> case Same of
                none -> X;
                _ -> false
            end
        end
    end,

    % Use Map to compile a list of results if each element is the same
    Res = lists:map(fun(L) -> lists:foldl(SameF, none, L) end, Lists),
    Winners = lists:filter(fun(X) -> (X /= false) and (X /= n) end, Res),

    % Now we can just check if there were any winners
    % TODO: Is there a better way to do mutliple "if" statements like this?
    case lists:member(x, Winners) of
        true -> x;
        false -> case lists:member(y, Winners) of
            true -> y;
            false -> case lists:member(n, Board) of
                % If there are no empty spaces (marked by n), no more moves remain,
                % cat's game
                false -> cat;

                % Otherwise, there are still more moves to be made, declare no_winner
                true -> no_winner
            end
        end
    end.

% Create a list from 0 to Max
count_list(Max, Max) -> [];
count_list(N, Max) -> [N | count_list(N + 1, Max)].
count_list(Max) -> count_list(0, Max).

% Zip two lists together into one list of tuples
tuple_lists([], _) -> [];
tuple_lists(_, []) -> [];
tuple_lists([H1|T1], [H2|T2]) -> [{H1,H2} | tuple_lists(T1, T2)].