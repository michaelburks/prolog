% 4.01
is_btree(nil).

is_btree(t(_, L, R)) :-
  is_btree(L), is_btree(R).

% 4.02
cbal_btree(0, nil).

cbal_btree(D, T) :-
  D > 0,
  1 is mod(D, 2),
  DH is (D - 1) / 2,
  cbal_btree(DH, L),
  cbal_btree(DH, R),
  T = t(x, L, R).

cbal_btree(D, T) :-
  D > 0,
  0 is mod(D, 2),
  DL is (D / 2),
  DR is DL - 1,
  cbal_btree(DL, L),
  cbal_btree(DR, R),
  T = t(x, L, R).

cbal_btree(D, T) :-
  D > 0,
  0 is mod(D, 2),
  DR is (D / 2),
  DL is DR - 1,
  cbal_btree(DL, L),
  cbal_btree(DR, R),
  T = t(x, L, R).

% 4.03
mirror(nil, nil).
mirror(T1, T2) :-
  t(_, L1, R1) = T1,
  t(_, L2, R2) = T2,
  mirror(L1, R2),
  mirror(L2, R1).

is_symmetric(T) :- mirror(T, T).
