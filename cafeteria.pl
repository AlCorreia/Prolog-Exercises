%% An example of an expert system for recommending coffee.
%% It is organised in three main parts:
%%  1. The Knowledge Base
%%     This is where we have the rules and facts elicited from a (human)
%%     expert.
%%  2. The Inference Engine
%%     The core of the inference engine is the Prolog interpreter itself.
%%     However, in this part of the code we add rules that use the expert
%%     knowledge in the KB to come up with a suggestion, given user input.
%%     There are also rules for providing an explanation for each suggestion.
%%  3. The User Interface
%%     This part of the code creates the interface through which we request
%%     and provide information to the user. That is done mainly through
%%     multiple choice questions to simplify the interaction.


%%%%%%%%%%%%%%%%%%%%
%% KNOWLEDGE BASE %%
%%%%%%%%%%%%%%%%%%%%

% Expert rules
coffee(espresso) :-
  strength(strong), milk(no).

coffee(filter) :-
  strength(light), milk(no).

coffee(cappuccino) :-
  strength(light), milk(foam).

coffee(macchiato) :-
  strength(strong), milk(foam).

coffee(latte) :-
  strength(light), milk(regular).

% Map options to properties
map(strength, "1", strong).
map(strength, "2", light).
map(strength, "0", _).

map(milk, "1", no).
map(milk, "2", regular).
map(milk, "3", foam).
map(milk, "0", _).

% Declare dynamic predicates for user input
:- dynamic strength/1.
:- dynamic milk/1.

clear :-
  retractall(milk(_)),
  asserta(milk(_)),
  retractall(strength(_)),
  asserta(strength(_)).
:-clear.

%%%%%%%%%%%%%%%%%%%%
%% USER INTERFACE %%
%%%%%%%%%%%%%%%%%%%%

:- discontiguous menu/0. % just to avoid warnings

menu :-
  % tty_clear, % only works on unix (mac, linux)
  write('\e[H\e[2J'), % should work for every OS
  draw_coffee,
	repeat,		% backtrack to this position whenever something below fails.
  nl,
  print_preferences,
  format('~46t~72|~n'),
  writeln("How can I help you?"),
	writeln("1. Suggest me a coffee..."),
  writeln("2. Ask me about my preferences"),
  writeln("3. Changed my mind. Clear my preferences."),
	writeln("0. Exit"),
  format('~46t~72|~n'),
	read_line_to_string(user_input, Choice),
	process_choice(Choice),
	Choice == "0". % if Choice is not 0: fail, backtrack to repeat.


process_choice("0") :-	% user choice is 0: succeed.
  format("Thank you and see you next time!~n~n~n"), clear.

% Run inference
process_choice("1") :-
  % tty_clear, % only works on unix (mac, linux)
  write('\e[H\e[2J'),
  nl,
	writeln("Current suggestion:"),
	find_suggestions(Coffee),
  string_upper(Coffee, UpCoffee),
	format("I personally recommend a nice ~w ", UpCoffee),
  explain(coffee(Coffee), Reasons),
  print_reasons(Reasons).

% Gather information from the user
process_choice("2") :-
	ask_question,
	!.

% Gather information from the user
process_choice("3") :-
  format("~nNo worries, we can restart.~n~n"),
  format("You currently have no preferences.~n"),
	clear,
	!.


% Questions are layed out in fixed order
% First ask about the strength and then about the milk
ask_question :-
  strength(S),
  var(S),
  write('\e[H\e[2J'),
  nl,
  format("~46t~72|~n"),
  writeln("How do you like your coffee?"),
  writeln("1. Strong"),
	writeln("2. Light"),
  writeln("0. Never mind"),
  format("~46t~72|~n"),
  read_line_to_string(user_input, Choice),
  map(strength, Choice, S),
  retractall(strength(_)),
  assertz(strength(S)).

ask_question :-
  milk(M),
  var(M),
  write('\e[H\e[2J'),
  nl,
  format("~46t~72|~n"),
  writeln("Would you like any milk at all?"),
  writeln("1. None"),
	writeln("2. Regular"),
  writeln("3. Foam"),
  writeln("0. Never mind"),
  format("~46t~72|~n"),
  read_line_to_string(user_input, Choice),
  map(milk, Choice, M),
  retractall(milk(_)),
  assertz(milk(M)).

ask_question :-
  % tty_clear, % only works on unix (mac, linux)
  write('\e[H\e[2J'),
  nl,
  writeln("You already picked the strength and milk type."),
  writeln("I am ready to give you a suggestion.").


%%%%%%%%%%%%%%%
%% INFERENCE %%
%%%%%%%%%%%%%%%

% Gets all coffee alternatives and pick one at random
find_suggestions(Coffee) :-
  bagof(C, coffee(C), Suggestions),
  random_member(Coffee, Suggestions), !.
% If there is no option, write an apology.
find_suggestions(_) :-
  writeln("Sorry, I am afraid I have no suggestions for you..."),
  clear,
  fail.


% Provide explanations
explain(Conclusion, Reasons):-
  clause(Conclusion, Goals), % returns the subgoals of a rule.
	Goals,
	!,
	get_reasons(Goals, Reasons).


% Parse the goals into Prop-Value pairs.
% Here we are only concerned with the Values actually.
get_reasons(Goal, Reasons) :-
	functor(Goal, Pred, _),  % gets the predicate of a goal
	Pred \= ,,  % comma (conjunction), the goals are split in the next rule
  write_reason(Pred, Reasons), !.
get_reasons(Goal, Reasons) :-
	arg(1, Goal, Subgoal_1), get_reasons(Subgoal_1, R1),
	arg(2, Goal, Subgoal_2), get_reasons(Subgoal_2, R2),
	append(R1, R2, Reasons).


% write_reason(Pred, [Reason]) <-
%   Reason is a string with a explanation related to predicate Pred.
%   If predicate Pred is not defined (free variable), Reason is empty.
%
write_reason(strength, [Reason]) :-
  strength(Value),
  nonvar(Value), !,
  string_upper(Value, UpValue),
  swritef(Reason, "%w", [UpValue]).
write_reason(milk, [Reason]) :-
  milk(Value),
  nonvar(Value), !,
  string_upper(Value, UpValue),
  swritef(Reason, "with %w milk", [UpValue]).
write_reason(_, []).


% Prints the reason for a selection
%
% No input from the user
print_reasons([]) :-
  writeln("just because I feel you will love it!"), nl, !.
% One input from the user
print_reasons(Reasons) :-
  write("because you ordered something "),
  prin(Reasons).


% Go through the list of reasons, printing them to screen
%
prin([P]) :- format("~w. ~n~n", P), !.
prin([H|T]) :- format("~w ", H), prin(T).


% Prints the current user's preference to the screen
%
print_preferences :-
  write_reason(strength, RS),
  write_reason(milk, RM),
  append(RS, RM, Pref),
  print_preferences(Pref).

% No input from the user
print_preferences([]) :- !.
% One input from the user
print_preferences(Pref) :-
  write("Your current preferences are "),
  prin(Pref).


%% Just some nice ASCII art :-)
draw_coffee :-
  writeln('       ,-"-.'),
  writeln('     _r-----i          _'),
  writeln('     \\      |-.      ,###.'),
  writeln('      |     | |    ,-------.'),
  writeln('      |     | |   c|       |                       ,--.'),
  writeln("      |     |'     |       |      _______________ C|  |"),
  writeln("      (=====)      =========      \\_____________/  `=='   cww"),
  writeln("(HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH)").

:- menu.
