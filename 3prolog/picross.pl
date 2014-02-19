% Bonus - Prolog Picross Solver

% Solver function, solves an NxN square picross puzzle
%    Cc - Column constraints, constraints on the columns from
%         left to right
%    Rc - Row constraints, constraints on the rows from top to
%         bottom
%  Solution - the solved puzzle with 1 to fill in a tile, 0 to 
%             leave blank
%
%
% For the puzzle:
%
%         1
%     3 4 3 4 3
%   1
% 1 1
%   5
%   5
%   5
%
%
% picross([[3], [4], [1,3], [4], [3]], 
%         [[1], [1, 1], [5], [5], [5]], Board).
%
%
%         1
%     3 4 3 4 3
%   1     X
% 1 1   X   X
%   5 X X X X X
%   5 X X X X X
%   5 X X X X X


% General Solution:
picross(Cc, Rc, Solution) :-
    length(Rc, CSize),
    length(Cc, RSize),
    SolutionSize is CSize * RSize,
    length(Solution, SolutionSize),

    % Limit the solution to be 0 or 1 in each tile
    fd_domain(Solution, 0, 1),

    % Rows and columns
    rows(RSize, Solution, Rows),
    cols(RSize, Rows, Cols),

    % Apply a rule to rows and columns to ensure validitiy:
    valid(Rc, Rows),
    valid(Cc, Cols),

    format("\n", []),
    formatRows(Rows).

valid([],[]).
valid([Ch|Ct], [Lh|Lt]) :-
    % Check if Lh follows rule Ch
    followsRules(Ch, Lh),
    % Recurse
    valid(Ct,Lt).

%----------------------%
% Follow Rules:        %
%   R - Rule to follow %
%   B - The board      % 
%----------------------%

followsRules([], []).
followsRules([], [Lh|Lt]) :-
    Lh = 0,
    followsRules([], Lt).
followsRules(R, [Lh|Lt]) :- 
    Lh = 0,
    followsRules(R, Lt).
followsRules([Rh|Rt], L) :- 
    % Sequentially fill this rule
    sqtFill(Rh, L, Ls),
    followsRules(Rt, Ls).

%--------------------------------------%
% Sequential Fill:                     %
%   R - Rule (amount of tiles to fill) %
%   B - The board                      %
%   Bl - The board left after fulfill- %
%        ing this rule                 %
%--------------------------------------%

sqtFill(0,[],[]).
sqtFill(0,[Lh|Lt], L) :-
    Lh = 0,
    L = Lt.
sqtFill(R,[Lh|Lt], L) :-
    Nr is R - 1,
    Lh = 1,
    sqtFill(Nr, Lt, L).
    
%------------------------%
% Rules to format output %
%------------------------%

stringify([], []).
stringify([1|T], [H|T2]) :-
    H = "X",
    stringify(T,T2).
stringify([0|T], [H|T2]) :-
    H = " ",
    stringify(T,T2).

formatRow([]) :-
    format("\n", []).
formatRow([H|T]) :-
    format("%c ", H),
    formatRow(T).

formatRows([]).
formatRows([Row|T1]) :-
    stringify(Row, SRow),
    formatRow(SRow),
    formatRows(T1).

% Unifies rows with a grid:
%  Size - length of a row in the grid
%  Grid - Array with Size * Size elements
%  Rows - list of Size lists with Size elements in
%         in each list that represents rows in Grid
rows(_, [], []).
rows(Size, Grid, [Row|T]) :-
    append(Row, NGrid, Grid),
    length(Row, Size),
    rows(Size, NGrid, T).

% Unifies cols with a list of rows:
%  Size - length of a row in the grid 
%  Rows - list of rows from a grid
%  Cols - list of columns from the list of rows
cols(Size, Rows, Cols) :- 
    columns(Size, 0, Rows, Cols).

% Helper rule for cols:
%  Size - length of a row in the grid 
%  I    - index of row to construct the column from
%  Rows - list of rows from a grid
%  Cols - list of columns from the list of rows
columns(Size, Size, _, []).
columns(Size, I, Rows, [Col|T]) :-
    Ni is I + 1,
    col(Ni, Rows, Col),
    columns(Size, Ni, Rows, T).

% Another helper for cols:
%     I - index of row to construct the column from
%  Rows - rows to extract values from for columns
%   Col - Column from Rows
col(_, [], []).
col(I, [Row|Rows], [H|T]) :-
    nth(I,Row,H),
    col(I, Rows, T).