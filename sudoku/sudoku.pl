% Sudoku Solver

% Shorthand for solving a sample puzzle.
% ?- solve_puzzle(worlds_hardest_sudoku).
solve_puzzle(S) :-
  call(S, P),
  sudoku(_, P).

% Sample puzzles.
% https://www.telegraph.co.uk/news/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html
% World's hardest.
worlds_hardest_sudoku([
  [0,0,8], [1,2,3], [1,3,6], [2,1,7], [2,4,9], [2,6,2], [3,1,5], [3,5,7],
  [4,4,4], [4,5,5], [4,6,7], [5,3,1], [5,7,3], [6,2,1], [6,7,6], [6,8,8],
  [7,2,8], [7,3,5], [7,7,1], [8,1,9], [8,6,4]
]).

% https://theconversation.com/good-at-sudoku-heres-some-youll-never-complete-5234
% 17 is the minimum number of bindings needed for a valid puzzle.
sudoku_17([
  [0,3,7], [1,0,1], [2,3,4], [2,4,3], [2,6,2], [3,8,6], [4,3,5], [4,5,9],
  [5,6,4], [5,7,1], [5,8,8], [6,4,8], [6,5,1], [7,2,2], [7,7,5], [8,1,4],
  [8,6,3]
]).

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

% Pretty printing.
is_3_or_6(3).
is_3_or_6(6).

display_board([]).

% Insert row breaks.
display_board(R) :-
  length(R, L), is_3_or_6(L), write('-------+-------+-------'), nl, false.

display_board([R|Rows]) :-
 write(' '), display_row(R), nl, display_board(Rows).

display_row([]).

% Insert column breaks.
display_row(C) :-
  length(C, L), is_3_or_6(L), write('| '), false.

display_row([C|Cells]) :-
  display_cell(C), write(' '), display_row(Cells).

display_cell(X) :-
  var(X), write('.').

display_cell(X) :-
  number(X), write(X).

% Main.
% B is the complete grid. Bindings is a list of [Row, Column, Value] lists.
% Example Invocation:
% ?- easy_puzzle(P), sudoku(X, P).
sudoku(B, Bindings) :-
  statistics(walltime, _),

  board(B),
  apply_bindings(B, Bindings),
  nl, write('Sudoku Puzzle:'), nl,
  display_board(B), nl,

  % Gather all the regions.
  regions(B, AllRegions),

  % Fill in regions.
  fill_in(AllRegions),
  write('Solution:'), nl,
  display_board(B), nl, nl,

  statistics(walltime, [_| [ElapsedTimeMs]]),
  ElapsedTimeSec is ElapsedTimeMs / 1000,
  write('Time elapsed: '), write(ElapsedTimeSec), write(' seconds'), nl.


regions(B, R) :-
  % Gather all the regions.
  Range = [0, 1, 2, 3, 4, 5, 6, 7, 8],
  squares(B, Squares, Range),
  rows(B, Rows, Range),
  cols(B, Cols, Range),
  append(Rows, Cols, Lattice),
  append(Lattice, Squares, R).

% Fill the region with the most numbers (least variables) and repeat.
fill_in([]):-
  write('\r  '), nl.
fill_in(AllRegions) :-
  most_constrained(AllRegions, Best, Others),
  length(Others, OL),
   % Write length of remaining to loosely display progress.
  write('\r'), write_two_digits(OL), write(' '),
  is_one_through_nine(Best),
  % Verify that new bindings do not conflict in other regions.
  verify_regions(Others),
  fill_in(Others).

write_two_digits(N) :- N < 10, write(0), write(N).
write_two_digits(N) :- not(N < 10), write(N).

most_constrained([X], X, []).
most_constrained([First|Rest], Best, Other) :-
  count_numbers(First, FC),
  mc_helper(Rest, Best, Other, First, [], FC).

mc_helper([], Best, Other, Best, Other, _).

mc_helper([Next|Rest], BestF, OtherF, Best, OAcc, BC) :-
  count_numbers(Next, NC),
  NC > BC,
  mc_helper(Rest, BestF, OtherF, Next, [Best | OAcc], NC).

mc_helper([Next|Rest], BestF, OtherF, Best, OAcc, BC) :-
  count_numbers(Next, NC),
  not(NC > BC),
  mc_helper(Rest, BestF, OtherF, Best, [Next | OAcc], BC).

count_numbers([], 0).
count_numbers([X|R], C) :-
  number(X),
  count_numbers(R, RC),
  C is RC + 1.

count_numbers([X|R], C) :-
  not(number(X)),
  count_numbers(R, C).

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

% Checks that L is list containing one each of the digits 1-9.
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

% check all regions for conflicts
verify_regions([]).
verify_regions([R|Rest]) :-
  verify_legal_bindings_set(R, []),
  verify_regions(Rest).

% Verify that a region does not contain duplicated numbers.
verify_legal_bindings_set([], _).
verify_legal_bindings_set([X|Rest], N) :-
  number(X),
  not(member(X, N)),
  verify_legal_bindings_set(Rest, [X|N]).

verify_legal_bindings_set([X|Rest], N) :-
  not(number(X)),
  verify_legal_bindings_set(Rest, N).
