%%%%%%%%%%%%%%%%%%%%%%%
%% FACTORIAL EXAMPLE %%
%%%%%%%%%%%%%%%%%%%%%%%

%% factorial(N, F) <-
%%   F is the factorial of N.
%
factorial(0, 1) :- !.
factorial(N, F) :-
  N > 0,
  Nx is N - 1,
  factorial(Nx, Fx),
  F is N * Fx.

factorial2(N, F) :-
  N > 0,
 ( N=0 -> F=1
    ;
   Nx is N - 1,
   factorial2(Nx, Fx),
   F is N * Fx
  ).


%%%%%%%%%%%%
%% REPEAT %%
%%%%%%%%%%%%

persistent :-
  repeat2,
  writeln("Should I stop?"),
  read(S),
  S == yes,
  writeln("Goodbye!").

persistent2 :-
  repeat,
  writeln("Should I stop?"),
  read(S),
  (S \== yes ->
    fail % backtrack to repeat
  ; writeln("Goodbye!"),
    ! % cut, we won't backtrack to repeat anymore
  ).

repeat2.
repeat2 :- writeln("repeating"), repeat2.


%%%%%%%%%%%%%%%%%%
%% DICE EXAMPLE %%
%%%%%%%%%%%%%%%%%%

dice(1, 1/6).
dice(2, 1/6).
dice(3, 1/6).
dice(4, 1/6).
dice(5, 1/6).
dice(6, 1/6).

combDice(D, D1, D2) :-
  between(1, D, D1),
  D2 is D - D1.

combProb(D1, D2, P) :-
  dice(D1, P1),
  dice(D2, P2),
  P is P1 * P2.

probSum(S, Prob) :-
  findall(
    P,
    (combDice(S, D1, D2),
     combProb(D1, D2, P)),
    L),
    sum_list(L, Prob).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INDEPENDENCE ASSUMPTION %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% probability of intersection of events (independence assumption)
indep_and([P], P).
indep_and([H|T], P) :-
  indep_and(T, PT),
  P is H*PT.

% probability of union of events (independence assumption)
indep_or([P], P).
indep_or([H|T]], P) :-
  indep_or(T, PT),
  P is 1-((1-H)*(1-PT)).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%% COSERVATIVE APPROACH %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

likes(john, coffee, 0.5).
likes(john, redwine, 0.7).

and(ListP, P) :-
  min_list(ListP, P).
or(ListP, P) :-
  max_list(ListP, P).

likes_all(Person, List, Prob) :-
  findall(P, (member(Obj, List), likes(Person, Obj, P)), ListP),
  and(ListP, Prob).

likes_any(Person, List, Prob) :-
  findall(P, (member(Obj, List), likes(Person, Obj, P)), ListP),
  or(ListP, Prob).
