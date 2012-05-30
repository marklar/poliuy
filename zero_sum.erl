-module(zero_sum).
-export([start/0, has_zero_sum/1]).
-revision('Revision: 0.1').
-created('Date: 2012/05/30 03:21:00').
-created_by('markwvh@gmail.com').

%% Problem
%% =======
%% Given a list of Integers in the range [-65000, 65000],
%% return true iff any _subset_ of the list sums to zero,
%% false otherwise.
%%
%% [0, 1, 2, -3] => true
%% [1, 2, 3, -8] => false
%% [1, 4, 5, 2, -3] => true


%% Implementation Notes
%% ====================
%%
%% Split the N elements into two ordered sets: positive & negative ints.
%% (Optional: Use bitvectors to store the Ints.  Smaller, and obviates need to sort.)
%% If any element is 0, short-circuit.
%% Implemented in separate module: splits_ints:pos_neg_ints/1.
%%
%% For each set (pos/neg), *lazily* enumerate an ordered set of all possible sums.
%% (We could do an even lazier job of this if we used a manual merging of
%% sorted lists, but the code would be more complex, and it's unclear what
%% performance impact it may have.)
%%
%% Move through the lists of sums in tandem, looking for a match, if any.
%% If find one -> short-curcuit true.
%% If get to end of either set -> false.


%% [Int], [Int] -> [Int]
merge(L1, L2) ->
    %% Already ordered.
    %% Faster to merge manually (as opposed to converting to/from ordsets)?
    %% If so, would have logic similar to first_matching_subset_sum/3.
    ordsets:to_list(ordsets:union(ordsets:from_list(L1),
    				  ordsets:from_list(L2))).

%% Int, [Int] -> [Int]
mk_new_sums(I, Sums) ->
    [ I + S || S <- [0|Sums] ].

%% Int, [Int] -> [Int]
add_new_sums(I, Sums) ->
    merge(Sums, mk_new_sums(I, Sums)).

%%----------------

%% next_sum: (mostly) lazily iterate through all possible sums.
%%
%% Args:
%%   Min : the min value of the next desired sum
%%   TraversalState:
%%     {Ints,       : unused Ints
%%      Sums,       : unused Sums for previous Ints -- a subset of AllSums
%%      AllSums,    : all Sums computed so far (for all previous Ints)
%%     }
%%
%% Int, TraversalState -> none || {NextSum, TraversalState}

%% We've exhausted this set of numbers.  We're done.
next_sum(_, {[], [], _}) -> 
    none;

%% Move to next _Int_.
next_sum(Min, {[I|Ints], [], AllSums}) ->
    %% No more Sums to be considered.
    NewAllSums = add_new_sums(I, AllSums),
    NewSums = lists:dropwhile(fun (S) -> S < I end, NewAllSums),
    next_sum(Min, {Ints, NewSums, NewAllSums});
next_sum(Min, {[I|Ints], [S|_], AllSums}) when S > I ->
    %% The next Int is low enough that it must be considered.
    next_sum(Min, {[I|Ints], [], AllSums});
next_sum(Min, {[I|Ints], _, AllSums}) when Min > I ->
    %% The next Int is low enough that it must be considered.
    next_sum(Min, {[I|Ints], [], AllSums});

%% Move to next _Sum_.
next_sum(Min, {Ints, [S|Sums], AllSums}) when Min > S ->
    next_sum(Min, {Ints, Sums, AllSums});

%% We've found a good Sum.  Return it.
%% (This is like a 'pop' -- no need to update TraversalState.)
next_sum(_, {Ints, [S|Sums], AllSums}) ->
    {S, {Ints, [S|Sums], AllSums}}.

%%----------------

%% [Int], [Int] -> Int
first_matching_subset_sum(Poss, Negs) ->
    first_matching_subset_sum(1, {Poss, [], []}, {Negs, [], []}).

%% Helper version.
%% Int, TraversalState, TraversalState -> Int
first_matching_subset_sum(Next, StateA, StateB) -> 
    case next_sum(Next, StateA) of
	none -> 0;
	{NextA, NewStateA} -> 
	    case next_sum(NextA, StateB) of
		none -> 0;
		{NextA, _} -> NextA;
		{NextB, NewStateB} ->
		    first_matching_subset_sum(
		      lists:max([NextA, NextB]), NewStateA, NewStateB)
	    end
    end.

%%----------------

%% [Int] -> Bool
has_zero_sum(Ints) ->
    case split_ints:pos_neg_ints(Ints) of
	found_zero -> true;
	{Ps, Ns} ->
	    case first_matching_subset_sum(Ps, Ns) of
		0 -> false;
		_ -> true
	    end
    end.

%%----------------

start() ->
    %% [0, 1, 2, -3] => true
    List1 = [0, 1, 2, -3],
    One = has_zero_sum(List1),
    io:format("~w: ~w~n", [List1, One]),
    
    %% [1, 2, 3, -8] => false
    List2 = [1, 2, 3, -8],
    Two = has_zero_sum(List2),
    io:format("~w: ~w~n", [List2, Two]),

    %% [1, 4, 5, 2, -3] => true
    List3 = [1, 4, 5, 2, -3],
    Three = has_zero_sum(List3),
    io:format("~w: ~w~n", [List3, Three]).

