-module(sum0).
-export([start/0, has_zero_sum/1]).

%% Problem
%% =======
%% Given a list of Integers in the range [-65000, 65000],
%% return true iff any _subset_ of the list sums to zero,
%% false otherwise.
%%
%% [0, 1, 2, -3] => true
%% [1, 2, 3, -8] => false
%% [1, 4, 5, 2, -3] => true

%% Notes
%% =====
%%
%% There are two serious downsides to this implementation.
%%
%% First, because it's in Erlang, where all data is immutable,
%% we must continually re-allocate new bitvectors, rather than
%% re-use a pair of them (as we could in an imperative language).
%%
%% Also, this code does nothing to prevent the bitvectors from growing
%% without bounds.  If the list contains many integers, or the
%% integers have large absolute values, then the bitvectors can get
%% rather long.
%%
%% More efficient if we first create just num bitvectors (and compare them),
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

%% [int] -> bool
has_zero_sum(Nums) ->
    has_zero_sum(Nums, 2#0, 2#0).

%% [int], bitvector, bitvector -> bool
has_zero_sum([], _, _) ->
    false;
has_zero_sum([0|_], _, _) ->
    true;
has_zero_sum([N|Nums], PosSums, NegSums) ->
    {Ps, Ns} =
	case N > 0 of
	    true  -> {add_to_sums(N, PosSums), NegSums};
	    false -> {PosSums, add_to_sums(-N, NegSums)}
	end,
    case Ps band Ns of
	0 -> has_zero_sum(Nums, Ps, Ns);
	_ -> true
    end.

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
