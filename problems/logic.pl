:- use_module(library(clpfd)).

% 3.01
and(A, B) :- A, B.

or(A, _) :- A.
or(_, B) :- B.

nand(A, B) :- not(and(A,B)).

nor(A, B) :- not(or(A, B)).

xor(A, B) :- A, not(B).
xor(A, B) :- not(A), B.

impl(_, B) :- B.
impl(A, B) :- not(A), not(B).

equ(A, B) :- A, B.
equ(A, B) :- not(A), not(B).

bind(true).
bind(false).

table(X, Y, F) :-
  bind(X), bind(Y),
  do(X, Y, F).

do(X, Y, _) :-
  print(X), write(' '), print(Y), write(' '), false.

do(_, _, F) :- F, !, print(true), nl, false.
do(_, _, _) :- print(false), nl, false.

print(true) :- write('true ').
print(false) :- write('false').

% 3.02

:- op(801, xfx, equ).

:- op(800, xfx, impl).

:- op(799, xfx, or).
:- op(799, xfx, nor).
:- op(799, xfx, xor).

:- op(798, xfx, and).
:- op(798, xfx, nand).

:- op(796, fx, not).

% 3.03
print_list([]).
print_list([X|Rest]) :- print(X), write(' '), print_list(Rest).

table(L, Expr) :-
  table_help(L, Expr, []).

table_help([], Expr, Z) :-
  print_list(Z), do(Expr), nl, false.

table_help([X|Rest], Expr, Z) :-
  bind(X),
  append(Z, [X], Zs),
  table_help(Rest, Expr, Zs).

do(F) :- F, !, print(true), nl, false.
do(_) :- print(false), nl, false.
