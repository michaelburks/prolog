:- use_module(library(clpfd)).

% 1.01
last_item(X, [X]).

last_item(X, [_|Rest]) :-
  last_item(X, Rest).

% 1.02
second_to_last_item(X, [X, _]).

second_to_last_item(X, [_|Rest]) :-
  second_to_last_item(X, Rest).

% 1.03
element_at_index(X, [X|_], 0).

element_at_index(X, [_|Rest], Idx) :-
  element_at_index(X, Rest, NextIdx),
  NextIdx #= Idx - 1.

% 1.04
count([], 0).

count([_|Rest], L) :-
  count(Rest, NextL),
  NextL #= L - 1.

% 1.05
reverse([], []).

reverse(X, Y) :-
  rev_help(X, Y, []).

rev_help([], [Y|R], [Y|R]).

rev_help([X1|R], Y, A) :-
  rev_help(R, Y, [X1|A]).

% 1.06
is_palindrome(L) :-
  reverse(L, L).

% 1.07
flatten([], []).

flatten(X, Y) :-
  flat_help(X, Y, []).

flat_help([], [Y|R], [Y|R]).

flat_help([X|Rest], Y, A) :-
  not(is_list(X)),
  append(A, [X], NextA),
  flat_help(Rest, Y, NextA).

flat_help([X|Rest], Y, A) :-
  is_list(X),
  append(X, Rest, NextX),
  flat_help(NextX, Y, A).

% 1.08
compress([], []).

compress([X|Rest], Y) :-
  compress_help(Rest, Y, [X], X).

compress_help([], Y, Y, _).

compress_help([X|Rest], Y, A, X) :-
  compress_help(Rest, Y, A, X).

compress_help([X|Rest], Y, A, Z) :-
  not(Z = X),
  append(A, [X], NextA),
  compress_help(Rest, Y, NextA, X).

% 1.09
pack([], []).

pack([X|Rest], Y) :-
  pack_help(Rest, Y, [], [X], X).

pack_help([], Y, Y, [], _).

pack_help([], Y, A, I, X) :-
  append(A, [I], Out),
  pack_help([], Y, Out, [], X).

pack_help([X|Rest], Y, A, I, X) :-
  pack_help(Rest, Y, A, [X|I], X).

pack_help([X|Rest], Y, A, I, Z) :-
  not(Z = X),
  append(A, [I], NextA),
  pack_help(Rest, Y, NextA, [X], X).

%  1.10
encode([], []).

encode([X|Rest], Y) :-
  encode_help(Rest, Y, [], [1, X]).

encode_help([], Y, Y, []).

encode_help([], Y, Z, [C, X]) :-
  append(Z, [[C,X]], NextZ),
  encode_help([], Y, NextZ, []).

encode_help([X|Rest], Y, A, [C, X]) :-
  NextC is C + 1,
  encode_help(Rest, Y, A, [NextC, X]).

encode_help([X|Rest], Y, A, [C, K]) :-
  not(K = X),
  append(A, [[C, K]], NextA),
  encode_help(Rest, Y, NextA, [1, X]).
