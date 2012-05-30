-module(subset).
-export([start/0, has_zero_sum/1]).
-revision('Revision: 0.1 ').
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


%% Implementation
%% ==============
%% Split the N elements into two sets: positive and negative ints.
%% These must be sorted.
%% (Optional: Use bitvectors to store the Ints.  Smaller, and obviates need to sort.)
%% If any element is 0, short-circuit (true).
%%
%% For each set (pos/neg), *lazily* enumerate list of all possible sums.
%% (These enumerations are naturally sorted.)
%%
%% Find the first matching sum from the two lists, if any.
%% If find one -> short-curcuit true.
%% If get to end of either set -> false.


%% Int, [Int] -> [Int]
new_sums(I, Sums) ->
    [ I + S || S <- [0|Sums] ].

%% [Int], [Int] -> [Int]
merge(L1, L2) ->
    %% Already ordered.
    %% Faster not to convert to ordsets, and instead manually merge?
    ordsets:to_list(ordsets:union(ordsets:from_list(L1),
    				  ordsets:from_list(L2))).

%% Int, [Int] -> [Int]
add_new_sums(I, Sums) ->
    merge(Sums, new_sums(I, Sums)).

%%----------------

%% next_sum: (mostly) lazily iterate through all possible sums.
%%
%% Args:
%%   Min:  want next sum >= Min
%%   TraversalState:
%%   {Ints,        unused Ints
%%    NextSums,    subset of AllSums
%%    AllSums,     computed so far
%%   }:    
%%
%% Int, TraversalState -> none || {NextSum, NextTraversalState}
next_sum(_, {[], [], _}) -> 
    none;
next_sum(Min, {[I|Ints], [], AllSums}) ->
    %% Move to next _Int_.
    NewAllSums = add_new_sums(I, AllSums),
    NewNextSums = lists:dropwhile(fun (S) -> S < I end, NewAllSums),
    next_sum(Min, {Ints, NewNextSums, NewAllSums});
next_sum(Min, {[I|Ints], [S|_], AllSums}) when S >= I ->
    %% Move to next _Int_.
    next_sum(Min, {[I|Ints], [], AllSums});
next_sum(Min, {[I|Ints], _, AllSums}) when Min > I ->
    %% Move to next _Int_.
    next_sum(Min, {[I|Ints], [], AllSums});
next_sum(Min, {Ints, [S|NextSums], AllSums}) when Min > S ->
    %% Move to next _Sum_.
    next_sum(Min, {Ints, NextSums, AllSums});
next_sum(_, {Ints, [S|NextSums], AllSums}) ->
    %% Return this Sum, and update state.
    {S, {Ints, [S|NextSums], AllSums}}.

%%----------------

%% Int, TraversalState, TraversalState -> Int
first_matching_subset_sum_impl(Next, StateA, StateB) -> 
    case next_sum(Next, StateA) of
	none -> 0;
	{NextA, NewStateA} -> 
	    case next_sum(NextA, StateB) of
		none -> 0;
		{NextA, _} -> NextA;
		{NextB, NewStateB} ->
		    first_matching_subset_sum_impl(
		      lists:max([NextA, NextB]), NewStateA, NewStateB)
	    end
    end.

%% [Int], [Int] -> Int
first_matching_subset_sum(Poss, Negs) ->
    first_matching_subset_sum_impl(1, {Poss, [], []}, {Negs, [], []}).

%%----------------

%% Int -> BitVector
just_this_bit(Int) when Int =< 0 ->
    %% This shouldn't happen.  Just being paranoid.
    2#0;
just_this_bit(Int) ->
    2#1 bsl (Int-1).

%% Int, BitVector -> BitVector
flip_bit(0, BV) ->
    %% This shouldn't happen.  Just being paranoid.
    BV;
flip_bit(I, BV) ->
    BV bor just_this_bit(I).

%%----------------

%% [Int], BitVector, BitVector -> found_zero | {[Int], [Int]}
pos_neg_ints_impl([0|_], _, _) ->
    found_zero;
pos_neg_ints_impl([], PosBV, NegBV) ->
    {bv_to_list(PosBV), bv_to_list(NegBV)};
pos_neg_ints_impl([I|Ints], PosBV, NegBV) ->
    {Ps, Ns} =
	case I > 0 of
	    true  -> {flip_bit(I, PosBV), NegBV};
	    false -> {PosBV, flip_bit(-I, NegBV)}
        end,
    pos_neg_ints_impl(Ints, Ps, Ns).

%% use bitvectors to create pos/neg lists.
%% [Int] -> found_zero | {[Int], [Int]}
pos_neg_ints(Ints) ->
    pos_neg_ints_impl(Ints, 2#0, 2#0).

%%----------------

%% [Int], Int, BitVector -> [Int]
bv_to_list_impl(Acc, _, BV) when BV == 0 ->
    lists:reverse(Acc);
bv_to_list_impl(Acc, Num, BV) when (BV band 1) == 1 ->
    bv_to_list_impl([Num+1|Acc], Num+1, BV bsr 1);
bv_to_list_impl(Acc, Num, BV) ->
    bv_to_list_impl(Acc, Num+1, BV bsr 1).

%% BitVector -> [Int]
bv_to_list(BV) ->
    bv_to_list_impl([], 0, BV).

%%----------------

%% [Int], [Int], [Int] -> bool
hzs_impl([0|_], _, _) ->
    true;
hzs_impl([I|Ints], Poss, Negs) ->
    {Ps, Ns} =
	case I > 0 of
	    true  -> {[I|Poss], Negs};
	    false -> {Poss, [-I|Negs]}
        end,
    hzs_impl(Ints, Ps, Ns);
hzs_impl([], Poss, Negs) ->
    %% Rather than use lists:sort, build up ordsets?
    case first_matching_subset_sum(lists:sort(Poss), lists:sort(Negs)) of
	0 -> false;
	_ -> true
    end.

%% [Int] -> Bool
has_zero_sum(Ints) ->
    %% hzs_impl(Ints, [], []).
    case pos_neg_ints(Ints) of
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

