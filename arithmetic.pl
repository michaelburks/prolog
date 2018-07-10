:- use_module(library(clpfd)).
:- [lists].

% 2.01
is_prime(2).

is_prime(X) :-
  X > 2,
  prime_help(X, 2).

prime_help(X, F) :-
  (F * F) > X.

prime_help(X, F) :-
  R is mod(X, F),
  not(R = 0),
  F1 is F + 1,
  prime_help(X, F1).

% 2.02
prime_factors(N, Fs) :-
  pf_help(N, Fs, [], 2).

pf_help(1, Y, Y, _).

pf_help(N, Fs, A, P) :-
  not(is_prime(P)),
  NextP is P + 1,
  pf_help(N, Fs, A, NextP).

pf_help(N, Fs, A, P) :-
  is_prime(P),
  0 is mod(N, P),
  R is N / P,
  append(A, [P], NextA),
  pf_help(R, Fs, NextA, P).

pf_help(N, Fs, A, P) :-
  is_prime(P),
  not(0 is mod(N, P)),
  NextP is P + 1,
  pf_help(N, Fs, A, NextP).

% 2.03
prime_factors_mult(N, L) :-
  prime_factors(N, Fs),
  encode(Fs, J),
  swap_elems(J, L, []).

swap_elems([], Z, Z).

swap_elems([[X,Y]|Rest], Z, A) :-
  append(A, [[Y, X]], NextA),
  swap_elems(Rest, Z, NextA).

% 2.04
prime_list(L, U, []) :-
  U < L.

prime_list(L, U, [L|Ps]) :-
  is_prime(L),
  NextL is L + 1,
  prime_list(NextL, U, Ps).

prime_list(L, U, Ps) :-
  not(is_prime(L)),
  NextL is L + 1,
  prime_list(NextL, U, Ps).

% 2.05
goldbach(N, [X,Y]) :-
  0 is mod(N, 2),
  gb_help(N, [X,Y], 3).

gb_help(N, [X, Y], X) :-
  is_prime(X),
  Y is N - X,
  is_prime(Y).

gb_help(N, [X, Y], O) :-
  NextO is O + 2,
  S is NextO + NextO,
  not(S > N),
  gb_help(N, [X, Y], NextO).


% 2.07
arithmetic_function(gcd/2).

gcd(X, 0, X).
gcd(0, X, X).
gcd(X, X, X).

gcd(X, Y, Z) :-
  X > Y,
  Y > 0,
  D is X - Y,
  gcd(D, Y, Z).

gcd(X, Y, Z) :-
  X < Y,
  X > 0,
  D is Y - X,
  gcd(X, D, Z).

% 2.08
coprime(X, Y) :- gcd(X, Y, 1).
