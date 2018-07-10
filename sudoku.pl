% Sudoku Solver

% Instantiate the board.
board([
  [V00, V01, V02, V03, V04, V05, V06, V07, V08],
  [V10, V11, V12, V13, V14, V15, V16, V17, V18],
  [V20, V21, V22, V23, V24, V25, V26, V27, V28],
  [V30, V31, V32, V33, V34, V35, V36, V37, V38],
  [V40, V41, V42, V43, V44, V45, V46, V47, V48],
  [V50, V51, V52, V53, V54, V55, V56, V57, V58],
  [V60, V61, V62, V63, V64, V65, V66, V67, V68],
  [V70, V71, V72, V73, V74, V75, V76, V77, V78],
  [V80, V81, V82, V83, V84, V85, V86, V87, V88]
]).

% Pretty-print
display_board([]).
display_board([R|Rows]) :-
  display_row(R), nl, display_board(Rows).

display_row([]).
display_row([C|Cells]) :-
  display_cell(C), write(' '), display_row(Cells).

display_cell(X) :-
  var(X), write('.').

display_cell(X) :-
  number(X), write(X).

% Main.
% B is the complete grid. Bindings is a list of [Row, Column, Value] lists.
% Example Invocation:
% ?- sudoku(X, [[0,0,1], [4, 4, 9], ..., [8, 8, 6]]).
sudoku(B, Bindings) :-
  board(B),
  apply_bindings(B, Bindings),
  write('Sudoku Puzzle:'), nl,
  display_board(B), nl,
  write('solving...'), nl,

  % center square
  squares(B, S4, [4]),
  is_one_through_nine_map(S4),

  % middle three rows
  rows(B, R345, [3,4,5]),
  is_one_through_nine_map(R345),

  % middle three columns
  cols(B, C345, [3,4,5]),
  is_one_through_nine_map(C345),

  % edge squares
  squares(B, S1357, [1,3,5,7]),
  is_one_through_nine_map(S1357),

  % top three rows
  rows(B, R012, [0,1,2]),
  is_one_through_nine_map(R012),

  % left three columns
  cols(B, C012, [0,1,2]),
  is_one_through_nine_map(C012),

  % top left square
  squares(B, S0, [0]),
  is_one_through_nine_map(S0),

  % top right, bottom left squares
  squares(B, S26, [2,6]),
  is_one_through_nine_map(S26),

  % bottom three rows
  rows(B, R678, [6,7,8]),
  is_one_through_nine_map(R678),

  % right three columns
  cols(B, C678, [6,7,8]),
  is_one_through_nine_map(C678),

  % bottom right square
  squares(B, S8, [8]),
  is_one_through_nine_map(S8),

  write('Solution:'), nl,
  display_board(B).

% Applies [Row, Column, Value] bindings to board.
apply_bindings(_, []).
apply_bindings(B, [[R, C, V]|Rest]) :-
  index(B, V, R, C),
  apply_bindings(B, Rest).

% Rows in board for indexes I.
rows(_, [], []).
rows(B, R, I) :-
  rows_h(B, R, [], I).

rows_h(_, R, R, []).
rows_h(B, R, Acc, [Idx|Idxs]) :-
  row_index_list(Idx, Is),
  values_at_index_list(B, Vs, Is),
  append(Acc, [Vs], AccN),
  rows_h(B, R, AccN, Idxs).

row_index_list(R, Is) :-
  Is = [[R, 0], [R, 1], [R, 2], [R, 3], [R, 4], [R, 5], [R, 6], [R, 7], [R, 8]].

% Columns in board for indexes I.
cols(_, [], []).
cols(B, C, I) :-
  cols_h(B, C, [], I).

cols_h(_, C, C, []).
cols_h(B, C, Acc, [Idx|Idxs]) :-
  col_index_list(Idx, Is),
  values_at_index_list(B, Vs, Is),
  append(Acc, [Vs], AccN),
  cols_h(B, C, AccN, Idxs).

col_index_list(C, Is) :-
  Is = [[0, C], [1, C], [2, C], [3, C], [4, C], [5, C], [6, C], [7, C], [8, C]].

% Squares in board for indexes I.
squares(_, [], []).
squares(B, S, I) :-
  squares_h(B, S, [], I).

squares_h(_, S, S, []).
squares_h(B, S, Acc, [Idx|Idxs]) :-
  square_index_list(Idx, Is),
  values_at_index_list(B, Vs, Is),
  append(Acc, [Vs], AccN),
  squares_h(B, S, AccN, Idxs).

square_index_list(S, Is) :-
  TR is 3 * mod(S, 3), MR is TR + 1, BR is TR + 2,
  LC is S - mod(S, 3), MC is LC + 1, RC is LC + 2,
  Is = [[TR, LC], [TR, MC], [TR, RC],
        [MR, LC], [MR, MC], [MR, RC],
        [BR, LC], [BR, MC], [BR, RC]].

% Values at [Row, Column] indexes.
values_at_index_list(_, [], []).
values_at_index_list(B, [V|Vs], [[R,C]|Is]) :-
  index(B, V, R, C),
  values_at_index_list(B, Vs, Is).

index([[K|_]|_], K, 0, 0).

index([[_|R]|_], K, 0, C) :-
  C > 0,
  Cd is C - 1,
  index([R], K, 0, Cd).

index([_|Rows], K, R, C) :-
  R > 0,
  Rd is R - 1,
  index(Rows, K, Rd, C).

% Evaluates that each element is a list containing one each of the digits 1-9.
is_one_through_nine_map([]).
is_one_through_nine_map([Z|Rest]) :-
  is_one_through_nine(Z),
  is_one_through_nine_map(Rest).

is_one_through_nine(L) :-
  length(L, 9),
  member(1, L),
  member(2, L),
  member(3, L),
  member(4, L),
  member(5, L),
  member(6, L),
  member(7, L),
  member(8, L),
  member(9, L).
