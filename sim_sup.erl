%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/05/07
%%% -------------------------------------------------------------------
-module(sim_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0,start_link/1,start/0,init/1,init_sim/0]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

-define(NODE1,	'node1@127.0.0.1').
-define(NODE2,	'node2@127.0.0.1').



%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start() ->
    spawn(fun() ->
		  supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
	  end).

start_link() ->
%%supervisor:start_link({local,?MODULE},?MODULE, []).
	supervisor:start_link({global,?MODULE},?MODULE, []).
start_link(Args) ->
	supervisor:start_link({global,?MODULE},?MODULE, Args).

init_sim()->
	sim_db:init_mnesia([?NODE1,?NODE2]).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	io:format("sim_sup init start~n"),

    Dcs = {{dcs,?NODE1}, 
		{sim_part, start_link,[dcs,?NODE1,dcs,start,[]]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    WheelFright = {{wheelFright,?NODE1}, 
		{sim_part, start_link,[wheelFright,?NODE1, wheel, start, [wheelFright]]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    WheelRright = {{wheelRright,?NODE1}, 
		{sim_part, start_link,[wheelRright,?NODE1, wheel, start, [wheelRright]]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    WheelFleft = {{wheelFleft,?NODE1}, 
		{sim_part, start_link,[wheelFleft,?NODE1, wheel, start, [wheelFleft]]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    WheelRleft = {{wheelRleft,?NODE1}, 
		{sim_part, start_link,[wheelRleft,?NODE1, wheel, start, [wheelRleft]]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    Frsensor = {{frsensor,?NODE1}, 
		{sim_part, start_link,[frsensor,?NODE1,distance, start, [frsensor]]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    Bksensor = {{bksensor,?NODE1}, 
		{sim_part, start_link,[bksensor,?NODE1,distance, start, [bksensor]]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    Risensor = {{risensor,?NODE1}, 
		{sim_part, start_link,[risensor,?NODE1,distance, start, [risensor]]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    Lfsensor = {{lfsensor,?NODE1}, 
		{sim_part, start_link,[lfsensor,?NODE1,distance, start, [lfsensor]]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    Throttle = {{throttle,?NODE1}, 
		{sim_part, start_link,[throttle,?NODE1,throttle, start, []]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    Brake = {{brake,?NODE1}, 
		{sim_part, start_link,[brake,?NODE1,brake, start, []]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    Output = {{output,?NODE1}, 
		{sim_part, start_link,[output,?NODE1,output, start, []]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    Loadenv = {{loadenv,?NODE1}, 
		{sim_part, start_link,[loadenv,?NODE1,loadenv, start, []]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    Sim_db = {{sim_db,?NODE1}, 
		{sim_part, start_link,[sim_db,?NODE1,sim_db, start, [[?NODE1,?NODE2]]]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    Balancectl = {{balancectl,?NODE1}, 
		{sim_part, start_link,[balancectl,?NODE1,balancectl, start, []]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    Transmission = {{transmission,?NODE1}, 
		{sim_part, start_link,[transmission,?NODE1,transmission, start, []]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    Scenario = {{scenario,?NODE1}, 
		{sim_part, start_link,[scenario,?NODE1,scenario, start, []]},
		permanent,
		brutal_kill,
		worker,
		[sim_part]},
    {ok,{{one_for_one,1,10},
	 [Scenario,Transmission,Balancectl,Sim_db,Loadenv,Dcs,Output, WheelFright,WheelRright,WheelFleft,WheelRleft,Frsensor,Bksensor,Risensor,Lfsensor,Throttle,Brake]
	}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

