pizza(marg).  % margherita
pizza(mari).  % marinara
pizza(napo).  % napoletana

contains(marg, moz).  % mozzarella
contains(marg, bas).  % basil

contains(mari, gar).  % garlic
contains(mari, ore).  % oregano
contains(mari, bas).

contains(napo, moz).
contains(napo, ore).
contains(napo, anc).  % anchovies

%% If X is a pizza, then X contains tomato_sauce
contains(X, tomato_sauce) :- pizza(X).

%% If X is a pizza and X contains mozzarella, then X is a cheese_pizza
cheese_pizza(X) :- pizza(X), contains(X, moz).
