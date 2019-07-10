%%%%%%%%%%%%%%%%%%%%%%%
%% ANCESTOR EXAMPLES %%
%%%%%%%%%%%%%%%%%%%%%%%

parent(rik, rob).
parent(rob, ann).
parent(ann, sam).
parent(sam, jon).

%% ancestor(X, Y) <-
%% X is the ancestor of Y.
%
ancestor(X, Y) :-
  parent(X, Y).
ancestor(X, Y) :-
  parent(X, Z),
  ancestor(Z, Y).


%%%%%%%%%%%%%%%%%%%%
%% PRINT EXAMPLES %%
%%%%%%%%%%%%%%%%%%%%

%% print_down(N) <-
%%   prints every integer from N to 1.
%
print_down(N) :-
  N > 0,  % stop condition
  write(N),  % statement
  N2 is N - 1,  % increment
  print_down(N2).  % recursion
print_down(_).  %  we need this to prevent prolog returning false at the end.

%% print_up(N) <-
%%   prints every integer from 1 to N.
%
print_up(N) :-
    printer1(1, N).

%% printer1 and printer2 are two different ways to solve the same problem.

%% printer1(X, Y) <-
%%   prints every integer from X to Y.
printer1(X, Y) :- % general case X must be lower than Y
    X =< Y,  % stop condition
    write(X),  % statement
    X1 is X + 1,  % increment
    printer1(X1, Y).  % recursion
printer1(_, _).  %  we need this to prevent prolog returning false at the end.


%% printer2(X, Y) <-
%%   prints every integer from X to Y.
printer2(X, Y) :- X > Y.  % base case
printer2(X, Y) :- % general case X must be lower than Y
    write(X),  % statement
    X1 is X + 1,  % increment
    printer2(X1, Y).  % recursion


%%%%%%%%%%%%%%%%%%%%%%%%
%% FACTORIAL EXAMPLES %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% factorial(N, F) <-
%%   F is the factorial of N.
%
factorial(0, 1).
factorial(N, F) :-
  N > 0,
  Nx is N - 1,
  factorial(Nx, Fx),
  F is N * Fx.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SQUARE AND SUM EXAMPLES %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% addup(List, Sum) <-
%%   Sum is sum of all the elements in List.
%
addup([], 0).  % base case: empty list sums up to zero
addup([H|T], S) :-
  addup(T, Ss),  % we take the sum of the tail
  S is H + Ss.  % and add the head


%% square(List1, List2) <-
%%   Each element of List2 is the square of the corresponding element in List1.
%
square([], []).  % base case
square([First | Rest], [SquareFirst | SquareRest]) :-
  SquareFirst is First * First,
  square(Rest, SquareRest).


%%%%%%%%%%%%%%%%%%%%%%%%
%% MANIPULATING LISTS %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% lastitem(List, X) <-
%%   X is the last item of List.
%
lastitem([X], X).  % base case: the element of a unitary list is the last item
lastitem([_|T], X) :-
  lastitem(T, X).


%% len(List, L) <-
%%   L is the length (number of elements) of List.
%
len([], 0).  % base case: the empty list has length zero

len([_|T], L) :-
  len(T, Lx),
  L is Lx + 1.  % the length of a list is the length of its tail plus one.


%% mem(L, List) <-
%%   True if List contains L.
%
mem(X, [X|_]).  % base case: X is in L if X is the head of L.
mem(X, [_|T]) :-
  mem(X, T).


%% reverse(List1, List2, Acc) <-
%%   List2 contains the elements of List1 in reversed order followed by Acc.
%
%% e.g. to reverse list [1,2,3] we call reverse([1,2,3], L, []).
%
reverse([], L, L).  % base case: when we get [], we copy Acc to List2
reverse([H|T], L, Acc) :-
  reverse(T, L, [H|Acc]). % we add the head to Acc, and repeat for the tail.


%% zip(List1, List2, List3) <-
%%   List3 contains the elements of List1 and List2 paired in lists.
%
%% e.g. ?- zip([1,2], [3,4], L).
%%         L = [[1, 3], [2, 4]].
zip([], [], []).  % base case
zip([], [_|_], []).  % base case for lists of different lenghts
zip([_|_], [], []).  % base case for lists of different lenghts
zip([X|Xs], [Y|Ys], [[X,Y]|XYs]) :-
   zip(Xs, Ys, XYs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RETURNING LISTS EXAMPLES %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% printList(List) <-
%%   Prints each element of List.
%
printList([]).
printList([H|T]):-
 writeln(H),
 printList(T).


%% returnList(List1, List2) <-
%%   Copies List1 in List2, or vice-versa.
%
returnList([], []).
returnList([H|T], [H2|T2]):-
 H = H2,
 returnList(T, T2).


price(apple, 0.5).
price(banana, 0.25).
price(lime, 1.0).
%
%% getPrice(List) <-
%%   Prints the price of each element in List.
getPrice([]).
getPrice([H|T]):-
  price(H, X),
  writeln(X),
  getPrice(T).

%% getPrice(List1, List2) <-
%%   List2 contains the price of each element in List1.
%
getPrice2([], []).
getPrice2([H|T], [H2|T2]):-
  price(H, H2),
  getPrice2(T, T2).


%%%%%%%%%%%%%%%%%%%
%% PATH EXAMPLES %%
%%%%%%%%%%%%%%%%%%%

%% Path example 1 - duplicates and infinite loop due to cycle in the graph
arc(a, b).
arc(b, c).
arc(c, d).
arc(d, e).
arc(e, a).

%% Symmetric closure
path(X, Y) :- arc(X, Y).
path(X, Y) :- arc(Y, X).
%% Transitive closure
path(X, Y) :-
  arc(X, Z),
  path(Z, Y).

%% Path example 2 - removing duplicates and showing the visited nodes.
arcs(a, [b]).
arcs(b, [c]).
arcs(c, [d]).
arcs(d, [e]).
arcs(e, [a]).

%% Symmetric closure
path( X, Y, P ) :- % path/3 redefined
    path( X, Y, [X], RP ),
    reverse(RP, P, []),  % we reverse the list so we start in X.
    X\=Y.
path( X, Y, P ) :- % path/3 redefined
    path( Y, X, [Y], P ),
    X\=Y.

path( X, X, Path, Path ).  % base case
%% Transitive closure
path( X, Y, PIn, POut ) :-
    arcs( X, L ),
    member( Z, L ),
    not(member( Z, PIn )), % Been here before?
    path( Z, Y, [Z|PIn], POut ).

printer(N) :- N =< 0.
printer(N):-
  write(N),
  N2 is N - 1,
  printer(N2).
