-module(sum0).
-export([start/0, has_zero_sum/1]).

%% Problem
%% =======
%% Given a list of Integers in the range [-65000,65000],
%% return true iff any _subset_ of the list sums to zero,
%% false otherwise.
%%
%% [0, 1, 2, -3] => true
%% [1, 2, 3, -8] => false
%% [1, 4, 5, 2, -3] => true

%% Notes
%% =====
%% Immutable data -> re-allocate a new Sums bitvector w/ each new Num.
%%
%% If the list is long, or the numbers are (abs) large,
%% then the Sums bitvectors can get rather long.
%%
%% More efficient if we first sort the lists?
%% 
%% Or perhaps if we first create just num bitvectors (and compare them),
%% then iteratively create sum bitvectors (and compare those)?


%% int -> bitvector
just_this_bit(Num) when Num =< 0 ->
    2#0;
just_this_bit(Num) ->
    2#1 bsl (Num-1).

%% int, bitvector -> bitvector
mk_new_sums(0, Sums) ->
    Sums;
mk_new_sums(Num, Sums) ->
    (Sums bsl Num) bor just_this_bit(Num).

%% int, bitvector -> bitvector
add_to_sums(0, Sums) ->
    Sums;
add_to_sums(Num, Sums) ->
    mk_new_sums(Num, Sums) bor Sums.

%% [int], bitvector, bitvector -> bool
hzs_impl([], _, _) ->
    false;
hzs_impl([0|_], _, _) ->
    true;
hzs_impl([N|Nums], PosSums, NegSums) ->
    {Ps, Ns} =
	case N > 0 of
	    true  -> {add_to_sums(N, PosSums), NegSums};
	    false -> {PosSums, add_to_sums(-N, NegSums)}
	end,
    case Ps band Ns of
	0 -> hzs_impl(Nums, Ps, Ns);
	_ -> true
    end.

%% [int] -> bool
has_zero_sum(Nums) ->
    hzs_impl(Nums, 2#0, 2#0).

start() ->
    %% [0, 1, 2, -3] => true
    One = has_zero_sum([0, 1, 2, -3]),
    io:format("~w~n", [One]),
    
    %% [1, 2, 3, -8] => false
    Two = has_zero_sum([1, 2, 3, -8]),
    io:format("~w~n", [Two]),
    
    %% [1, 4, 5, 2, -3] => true
    Three = has_zero_sum([1, 4, 5, 2, -3]),
    io:format("~w~n", [Three]).