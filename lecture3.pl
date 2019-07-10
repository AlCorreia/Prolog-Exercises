%%%%%%%%%%%%%%%%%%%%%%%%
%% COMPARISON EXAMPLE %%
%%%%%%%%%%%%%%%%%%%%%%%%

arrangeMatch(X, Y) :-
  player(X),
  player(Y),
  X \= Y.

sell(Bid, Ask) :- Bid >= Ask.
buy(Bid, Ask) :- Bid =< Ask.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FAIL AND FORALL EXAMPLES %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

player(federer).
player(nadal).
player(djokovic).

%% print_all(Goal)
%% Prints all occurrences of Goal.
%% Throws an error if predicate of Goal does not exit.
%
print_all(Goal):-
  Goal,
  writeln(Goal),
  fail.
print_all(_). %base case

print_all_2(Goal) :-
  forall(Goal, writeln(Goal)).

%% print_all_3(Pred)
%% Prints all occurrences of predicate Pred.
%% Throws an error if Pred does not exit.
%
print_all_3(Pred) :-
  forall(call(Pred, X), format("~w(~w)~n", [Pred, X])).


%%%%%%%%%%%%%%%%%%
%% GATE EXAMPLE %%
%%%%%%%%%%%%%%%%%%

%% gate(X) <-
%%  True if X is 0 or X is a free variable
%
%% We add the cut to avoid instantiating X to 0
%% in the case where it is a free variable
%
gate(X) :- var(X), !.
gate(X) :- X=0.




%%%%%%%%%%%%%%%%%%%%%%%
%% MINIMUM/3 EXAMPLE %%
%%%%%%%%%%%%%%%%%%%%%%%

%% minimum(X, Y, Z) <-
%%  Z is the minimum between X and Y.
%
%% Note that either Z = X or Z = Y.
%
minimum(X, Y, X):-
  X =< Y.
minimum(X, Y, Y):-
  X > Y.

%% Green Cut version of minimum/3
%
gc_minimum(X, Y, X):-
  X =< Y, !.
gc_minimum(X, Y, Y):-
  X > Y.

%% Red Cut version of minimum/3
%
rc_minimum(X, Y, X):-
    X =< Y, !.
rc_minimum(_, Y, Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UNMARRIED STUDENT EXAMPLE %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

student(john).
student(mike).
married(mike).

%% unmarried_student(X) <-
%%  True if X is both a student and not married.
%
%% Note that the negation should come second.
%
unmarried_student(X):-
  student(X), \+ married(X).


%%%%%%%%%%%%%%%%%%%%
%% SUBWAY EXAMPLE %%
%%%%%%%%%%%%%%%%%%%%

picks(john, bread(brown)).
picks(john, bread(white)).
picks(john, filling(melt)).
picks(john, salad(tomato)).
picks(john, salad(lettuce)).

order(X, Sub) :-
  picks(X, bread(B)),
  picks(X, filling(F)), !,
  picks(X, salad(S)),
  Sub = (B, F, S).


%%%%%%%%%%%%%%%%%%%%%
%% DIVISOR EXAMPLE %%
%%%%%%%%%%%%%%%%%%%%%

%% divisor(N, D) <-
%%  True if D is a divisor of N.
%
divisor(N, D) :-
  between(1, N, D),
  0 is N mod D.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CUSTOM NEGATION OPERATOR %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neg(Goal) :- call(Goal), !, fail.
neg(_).


%%%%%%%%%%%%%%%%%%%%%
%% SWITCH EXAMPLES %%
%%%%%%%%%%%%%%%%%%%%%

switch(1):-
  writeln("Case 1"), !.
switch(2):-
  writeln("Case 2"), !.
switch(3):-
  writeln("Case 3"), !.
switch(_) :-
  writeln("Invalid option"),
  fail.


switch2(X) :-
    switch_case(X, [
      1 : writeln("Case 1"),
      2 : writeln("Case 2"),
      3 : writeln("Case 3")
    ]).

switch_case(X, [Val:Goal|Cases]) :-
    ( X=Val ->
        call(Goal)
    ;
        switch_case(X, Cases)
    ).


passed :- writeln('got through!').
test(X) :-
  X = 4 -> writeln('got through!')
  ;
  writeln('did not get through!'), fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FIBONACCI NUMBERS EXAMPLE %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fibo(N, F) <-
%%  True if F is the Nth Fibonacci number.
%
%% Implementation with two recursive calls.
%
:- dynamic fibo/2.
fibo(0, 0).
fibo(1, 1).
fibo(N, F) :-
  N > 1,
  N1 is N - 1,
  N2 is N - 2,
  fibo(N1, F1),
  fibo(N2, F2),
  F is F1 + F2,
  asserta(fibo(N, F):-!). % assert as first clause


%% fib(N, F) <-
%%  True if F is the Nth Fibonacci number.
%
%% Implementation with a single recursive call.
%
fib(0, 0).
fib(N, F) :-
  N > 0,
  fib(N, F, _).

%% fib(N, F, F_1) <-
%%  True if F is the Nth Fibonacci number and
%%  F_1 is the (N-1)th Fibonacci number.
%
fib(1, 1, 0).
fib(N, F, F_1) :-
  N > 1,
  N_1 is N - 1,
  fib(N_1, F_1, F_2),
  F is F_1 + F_2,
  asserta(fib(N, F):-!). % assert as first clause


%%%%%%%%%%%%%%%%%%%%
%% COFFEE EXAMPLE %%
%%%%%%%%%%%%%%%%%%%%


:- dynamic coffee/3.

makeCoffee(Type, Sugar, Size) :-
  \+ coffee(Type, Sugar, Size),
  writeln("Preparing your coffee."),
  assertz(coffee(Type, Sugar, Size)), !.

makeCoffee(Type, Sugar, Size) :-
  coffee(Type, Sugar, Size),
  writeln("You have already ordered.").

takeCoffee(Type, Sugar, Size) :-
  coffee(Type, Sugar, Size),
  writeln("Here you go!"),
  retract(coffee(Type, Sugar, Size)), !.

takeCoffee(Type, Sugar, Size) :-
  \+ coffee(Type, Sugar, Size),
  writeln("I am sorry, your coffee is not ready yet."),
  makeCoffee(Type, Sugar, Size).
