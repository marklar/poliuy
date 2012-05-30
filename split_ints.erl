-module(split_ints).
-export([pos_neg_ints/1]).
-revision('Revision: 0.1').
-created('Date: 2012/05/30 03:21:00').
-created_by('markwvh@gmail.com').

%% Split Ints into two sorted lists:
%%   * positives and
%%   * the abs value of the negatives.
%% If encounter 0 along the way, short-circuit.
%%
%% Two different impls:
%%   * using bitvectors (probably faster),
%%   * using lists (simpler).
%%
%% [Int] -> found_zero | {[Int], [Int]}
pos_neg_ints(Ints) ->
    pos_neg_ints(Ints, 2#0, 2#0).
    %% pos_neg_ints_lists(Ints, [], []).

%%----------------

%% Use _lists_.

%% [Int], [Int], [Int] -> bool
pos_neg_ints_lists([0|_], _, _) ->
    found_zero;
pos_neg_ints_lists([I|Ints], Poss, Negs) ->
    {Ps, Ns} =
	case I > 0 of
	    true  -> {[I|Poss], Negs};
	    false -> {Poss, [-I|Negs]}
        end,
    pos_neg_ints_lists(Ints, Ps, Ns);
pos_neg_ints_lists([], Poss, Negs) ->
    %% Rather than use lists:sort, build up ordsets?
    {lists:sort(Poss), lists:sort(Negs)}.

%%----------------

%% Use _bitvectors_.

%% [Int], BitVector, BitVector -> found_zero | {[Int], [Int]}
pos_neg_ints([0|_], _, _) ->
    found_zero;
pos_neg_ints([I|Ints], PosBV, NegBV) ->
    {Ps, Ns} =
	case I > 0 of
	    true  -> {flip_bit(I, PosBV), NegBV};
	    false -> {PosBV, flip_bit(-I, NegBV)}
        end,
    pos_neg_ints(Ints, Ps, Ns);
pos_neg_ints([], PosBV, NegBV) ->
    {bv_to_list(PosBV), bv_to_list(NegBV)}.

%%----------------

%% Int -> BitVector
%% Expects Int to be >0.
just_this_bit(Int) ->
    2#1 bsl (Int-1).

%% Int, BitVector -> BitVector
%% Expects Int to be >0.
flip_bit(I, BV) ->
    BV bor just_this_bit(I).

%%----------------

%% Take bitvector representing a set of Natural numbers
%% and turn into list of Ints.
%%
%% BitVector -> [Int]
bv_to_list(BV) ->
    bv_to_list([], 0, BV).

%% [Int], Int, BitVector -> [Int]
bv_to_list(Acc, _, BV) when BV == 0 ->
    lists:reverse(Acc);
bv_to_list(Acc, Num, BV) when (BV band 1) == 1 ->
    bv_to_list([Num+1|Acc], Num+1, BV bsr 1);
bv_to_list(Acc, Num, BV) ->
    bv_to_list(Acc, Num+1, BV bsr 1).
