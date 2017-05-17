%%%-------------------------------------------------------------------
%%% @author leonwly
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 五月 2017 17:22
%%%-------------------------------------------------------------------
-author("leonwly").

-define(INIT_PACKET,{packet,0}).

-define(TCP_OPTIONS,[binary,?INIT_PACKET, {reuseaddr, true},{keepalive, true}, {backlog, 256}, {active, false}]).
