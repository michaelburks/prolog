% Poker Hands

% Rank is one of 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 (jack), 12 (queen), 13 (king), 14 (ace).
rank(X) :-
  member(X, [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]).

% Suit is one of c(lub), d(iamond), h(eart), s(pade)
suit(X) :- member(X, [c, d, h, s]).

% Card is a rank-suit dyad.
card(C) :-
  C = [R, S],
  rank(R),
  suit(S).

card(C) :- card(C, C).

% Hand is a list of 5 cards.
hand(H) :-
  H = [A, B, C, D, E],
  card(A),
  card(B),
  card(C),
  card(D),
  card(E).

% ==============
% | Hand Types |
% ==============

type_rank(royal_flush, 1).
type_rank(straight_flush, 2).
type_rank(four, 3).
type_rank(full_house, 4).
type_rank(flush, 5).
type_rank(straight, 6).
type_rank(three, 7).
type_rank(two_pair, 8).
type_rank(one_pair, 9).
type_rank(high_card, 10).

% Royal Flush
hand_type(H, royal_flush) :-
  hand_type(H, flush),
  member([10, _], H),
  member([11, _], H),
  member([12, _], H),
  member([13, _], H),
  member([14, _], H).

% Straight Flush
hand_type(H, straight_flush) :-
  hand_type(H, flush),
  hand_type(H, straight).

% Four of a Kind
hand_type(H, four) :-
  select([Z, _], H, [[X, _], [X, _], [X, _], [X, _]]),
  Z \= X.

% Full House
hand_type(H, full_house) :-
  select([Y, _], H, D),
  select([Y, _], D, [[X, _], [X, _], [X, _]]),
  Y \= X.

% Flush
hand_type([[_, X], [_, X], [_, X], [_, X], [_, X]], flush).

% Straight
hand_type(H, straight) :-
   select([Z, _], H, H4),
   Z1 is Z+1,
   select([Z1, _], H4, H3),
   Z2 is Z+2,
   select([Z2, _], H3, H2),
   Z3 is Z+3, Z4 is Z+4,
   select([Z3, _], H2, [[Z4, _]]).

hand_type(H, straight) :-
   select([14, _], H, H4),
   select([2, _], H4, H3),
   select([3, _], H3, H2),
   select([4, _], H2, [[5, _]]).

% Three of a Kind
hand_type(H, three) :-
  select([Y, _], H, D),
  select([Z, _], D, [[X, _], [X, _], [X, _]]),
  Y \= X,
  Z \= X,
  Y \= Z.

% Two Pair
hand_type(H, two_pair) :-
  select([Y, _], H, H4),
  select([Y, _], H4, H3),
  select(Z, H3, [[X, _], [X, _]]),
  Y \= X,
  Z \= X,
  Y \= Z.

% One Pair
hand_type(H, one_pair) :-
  select([X, _], H, H4),
  select([X, _], H4, _).

% High Card
hand_type(_, high_card).

% ================
% | Sample Hands |
% ================

sample([[14, s], [11, s], [13, s], [12, s], [10, s]], royal_sf).
sample([[10, s], [11, s], [13, s], [12, s], [9, s]], high_sf).
sample([[3, s], [4, s], [5, s], [6, s], [7, s]], low_sf).
sample([[3, c], [11, h], [11, d], [11, c], [11, s]], high_four).
sample([[3, c], [4, h], [3, d], [3, h], [3, s]], low_four).



% ============
% | Showdown |
% ============

winner(H1, H2) :-
  hand_type(H1, T1),
  !,
  hand_type(H2, T2),
  !,
  hand_compare([H1, T1], [H2, T2]).

hand_compare([H1, T], [H2, T]) :-
  tiebreak(H1, H2, T).

hand_compare([_, T1], [_, T2]) :-
  type_rank(T1, R1),
  type_rank(T2, R2),
  R1 < R2.

% This would be unexpected.
% tiebreak(_, _, royal_flush).

tiebreak(H1, H2, straight_flush) :-
  tiebreak(H1, H2, straight).

tiebreak(H1, H2, four) :-
   select(_, H1, [[X1, _], [X1, _], [X1, _], [X1, _]]),
   select(_, H2, [[X2, _], [X2, _], [X2, _], [X2, _]]),
   X1 > X2.

tiebreak(H1, H2, full_house) :-
   tiebreak(H1, H2, three).

tiebreak(H1, H2, flush) :-
  tiebreak(H1, H2, high_card).

tiebreak([[A1, _], [A2, _], [A3, _], [A4, _], [A5, _]],
          [[B1, _], [B2, _], [B3, _], [B4, _], [B5, _]],
          straight) :-
   max_list([A1, A2, A3, A4, A5], A),
   max_list([B1, B2, B3, B4, B5], B),
   A > B.

tiebreak(H1, H2, three) :-
   select([_,_], H1, H14),
   select([_,_], H14, [[X1, _], [X1, _], [X1, _]]),
   select([_,_], H2, H24),
   select([_,_], H24, [[X2, _], [X2, _], [X2, _]]),
   X1 > X2.

tiebreak(H1, H2, two_pair) :-
  select([Y1, _], H1, H14),
  select([Y1, _], H14, H13),
  select(Z1, H13, [[X1, _], [X1, _]]),
  max_list([X1, Y1], A1),
  min_list([X1, Y1], B1),
  select([Y2, _], H2, H24),
  select([Y2, _], H24, H23),
  select(Z2, H23, [[X2, _], [X2, _]]),
  max_list([X2, Y2], A2),
  min_list([X2, Y2], B2),
  ordered_break([A1, B1, Z1], [A2, B2, Z2]).

tiebreak(H1, H2, one_pair) :-
  select([X1, _], H1, H14),
  select([X1, _], H14, _),
  select([X2, _], H2, H24),
  select([X2, _], H24, _),
  X1 > X2.

tiebreak(H1, H2, one_pair) :-
  select([X1, _], H1, H14),
  select([X1, _], H14, _),
  select([X2, _], H2, H24),
  select([X2, _], H24, _),
  X1 = X2,
  tiebreak(H1, H2, high_card).

tiebreak([[A1, _], [A2, _], [A3, _], [A4, _], [A5, _]],
         [[B1, _], [B2, _], [B3, _], [B4, _], [B5, _]],
         high_card) :-
   break_lists([A1, A2, A3, A4, A5], [B1, B2, B3, B4, B5]).


ordered_break([A1, _, _], [A2, _, _]) :-
  A1 > A2.

ordered_break([A1, B1, _], [A2, B2, _]) :-
  A1 = A2,
  B1 > B2.

ordered_break([A1, B1, Z1], [A2, B2, Z2]) :-
  A1 = A2,
  B1 = B2,
  Z1 > Z2.


break_lists(As, Bs) :-
  max_list(As, Amax),
  max_list(Bs, Bmax),
  Amax > Bmax.

break_lists(As, Bs) :-
  max_list(As, Amax),
  max_list(Bs, Bmax),
  Amax = Bmax,
  select(Amax, As, Arest),
  select(Bmax, Bs, Brest),
  break_lists(Arest, Brest).
