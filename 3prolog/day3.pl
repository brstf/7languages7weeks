% 4x4 Sudoku:
sudoku(Puzzle, Solution) :-
    % Make sure puzzle and solution unify:
    Solution = Puzzle,
    % Unify the format of the puzzle:
    Puzzle = [S11, S12, S13, S14,
              S21, S22, S23, S24,
              S31, S32, S33, S34,
              S41, S42, S43, S44],
    % Ensure all of our elements are within 1-4:
    fd_domain(Solution, 1, 4),
    % Unify the rows, columns, and squares
    Row1 = [S11, S12, S13, S14],
    Row2 = [S21, S22, S23, S24],
    Row3 = [S31, S32, S33, S34],
    Row4 = [S41, S42, S43, S44],
    Col1 = [S11, S21, S31, S41],
    Col2 = [S12, S22, S32, S42],
    Col3 = [S13, S23, S33, S43],
    Col4 = [S14, S24, S34, S44],
    Sqa1 = [S11, S12, S21, S22],
    Sqa2 = [S13, S14, S23, S24],
    Sqa3 = [S31, S32, S41, S42],
    Sqa4 = [S33, S34, S43, S44],
    % Validate all rows, columns, and squares:
    valid([
         Row1, Row2, Row3, Row4,
         Col1, Col2, Col3, Col4,
         Sqa1, Sqa2, Sqa3, Sqa4
         ]).
 
valid([]).
valid([Head | Tail]) :- fd_all_different(Head), valid(Tail).

% Full 9x9 Sudoku:
sudoku9(Puzzle, Solution) :-
    % Make sure puzzle and solution unify
    Solution = Puzzle,
    % Unify the format of the puzzle:
    Puzzle = [S11, S12, S13, S14, S15, S16, S17, S18, S19,
              S21, S22, S23, S24, S25, S26, S27, S28, S29,
              S31, S32, S33, S34, S35, S36, S37, S38, S39,
              S41, S42, S43, S44, S45, S46, S47, S48, S49,
              S51, S52, S53, S54, S55, S56, S57, S58, S59,
              S61, S62, S63, S64, S65, S66, S67, S68, S69,
              S71, S72, S73, S74, S75, S76, S77, S78, S79,
              S81, S82, S83, S84, S85, S86, S87, S88, S89,
              S91, S92, S93, S94, S95, S96, S97, S98, S99],
    % Ensure all of our elements are within 1-9:
    fd_domain(Solution, 1, 9),
    %Unify all of the rows, columns, and squares
    Row1 = [S11, S12, S13, S14, S15, S16, S17, S18, S19],
    Row2 = [S21, S22, S23, S24, S25, S26, S27, S28, S29],
    Row3 = [S31, S32, S33, S34, S35, S36, S37, S38, S39],
    Row4 = [S41, S42, S43, S44, S45, S46, S47, S48, S49],
    Row5 = [S51, S52, S53, S54, S55, S56, S57, S58, S59],
    Row6 = [S61, S62, S63, S64, S65, S66, S67, S68, S69],
    Row7 = [S71, S72, S73, S74, S75, S76, S77, S78, S79],
    Row8 = [S81, S82, S83, S84, S85, S86, S87, S88, S89],
    Row9 = [S91, S92, S93, S94, S95, S96, S97, S98, S99],

    Col1 = [S11, S21, S31, S41, S51, S61, S71, S81, S91],
    Col2 = [S12, S22, S32, S42, S52, S62, S72, S82, S92],
    Col3 = [S13, S23, S33, S43, S53, S63, S73, S83, S93],
    Col4 = [S14, S24, S34, S44, S54, S64, S74, S84, S94],
    Col5 = [S15, S25, S35, S45, S55, S65, S75, S85, S95],
    Col6 = [S16, S26, S36, S46, S56, S66, S76, S86, S96],
    Col7 = [S17, S27, S37, S47, S57, S67, S77, S87, S97],
    Col8 = [S18, S28, S38, S48, S58, S68, S78, S88, S98],
    Col9 = [S19, S29, S39, S49, S59, S69, S79, S89, S99],

    Sqa1 = [S11, S12, S13, S21, S22, S23, S31, S32, S33],
    Sqa2 = [S14, S15, S16, S24, S25, S26, S34, S35, S36],
    Sqa3 = [S17, S18, S19, S27, S28, S29, S37, S38, S39],
    Sqa4 = [S41, S42, S43, S51, S52, S53, S61, S62, S63],
    Sqa5 = [S44, S45, S46, S54, S55, S56, S64, S65, S66],
    Sqa6 = [S47, S48, S49, S57, S58, S59, S67, S68, S69],
    Sqa7 = [S71, S72, S73, S81, S82, S83, S91, S92, S93],
    Sqa8 = [S74, S75, S76, S84, S85, S86, S94, S95, S96],
    Sqa9 = [S77, S78, S79, S87, S88, S89, S97, S98, S99],
    % Validate all rows, columns, and squares:
    valid([
         Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8, Row9,
         Col1, Col2, Col3, Col4, Col5, Col6, Col7, Col8, Col9,
         Sqa1, Sqa2, Sqa3, Sqa4, Sqa5, Sqa6, Sqa7, Sqa8, Sqa9
         ]),
    % Use format to make the solution look nicer
    format('%d %d %d %d %d %d %d %d %d\n', Row1),
    format('%d %d %d %d %d %d %d %d %d\n', Row2),
    format('%d %d %d %d %d %d %d %d %d\n', Row3),
    format('%d %d %d %d %d %d %d %d %d\n', Row4),
    format('%d %d %d %d %d %d %d %d %d\n', Row5),
    format('%d %d %d %d %d %d %d %d %d\n', Row6),
    format('%d %d %d %d %d %d %d %d %d\n', Row7),
    format('%d %d %d %d %d %d %d %d %d\n', Row8),
    format('%d %d %d %d %d %d %d %d %d\n', Row9).
