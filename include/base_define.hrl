%%%-------------------------------------------------------------------
%%% @author leonwly
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 五月 2017 17:34
%%%-------------------------------------------------------------------
-author("leonwly").

-define(ERLNULL,undefined).
-define(NUM_ZERO,0).

%% ?:
-define(IF(C, T, F), (case (C) of true -> (T); false -> (F) end)).
-define(IFDO(C, T),(case (C) of true -> (T); false -> nothing end)).
