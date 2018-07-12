% 5.01
is_mtree(t(_, L)):-
  is_mtree_list(L).

is_mtree_list([]).
is_mtree_list([X|R]):-
  is_mtree(X), is_mtree_list(R).

% 5.02
nnodes(t(_, []), 1).
nnodes(T, N):-
  t(_, L) = T,
  nnodes_list(L, K, 0),
  N is K + 1.

nnodes_list([], S, S).

nnodes_list([X|Rest], K, A) :-
  nnodes(X, Z),
  S is A + Z,
  nnodes_list(Rest, K, S).
