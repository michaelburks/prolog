% Sudoku Solver

% Instantiate the 9 x 9 board.
board([
  [_, _, _, _, _, _, _, _, _],
  [_, _, _, _, _, _, _, _, _],
  [_, _, _, _, _, _, _, _, _],
  [_, _, _, _, _, _, _, _, _],
  [_, _, _, _, _, _, _, _, _],
  [_, _, _, _, _, _, _, _, _],
  [_, _, _, _, _, _, _, _, _],
  [_, _, _, _, _, _, _, _, _],
  [_, _, _, _, _, _, _, _, _]
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

% Gets regions at indexes.
regions(_, R, R, [], _).
regions(B, Regions, Acc, [Idx|Idxs], RegFunc) :-
   call(RegFunc, Idx, Is),
   values_at_index_list(B, Vs, Is),
   append(Acc, [Vs], AccN),
   regions(B, Regions, AccN, Idxs, RegFunc).

% Rows in board for indexes in I.
rows(_, [], []).
rows(B, R, I) :-
  regions(B, R, [], I, row_indexes).

row_indexes(R, Is) :-
  Is = [[R, 0], [R, 1], [R, 2], [R, 3], [R, 4], [R, 5], [R, 6], [R, 7], [R, 8]].

% Columns in board for indexes in I.
cols(_, [], []).
cols(B, C, I) :-
  regions(B, C, [], I, col_indexes).

col_indexes(C, Is) :-
  Is = [[0, C], [1, C], [2, C], [3, C], [4, C], [5, C], [6, C], [7, C], [8, C]].

% Squares in board for indexes in I.
squares(_, [], []).
squares(B, S, I) :-
  regions(B, S, [], I, square_indexes).

square_indexes(S, Is) :-
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
