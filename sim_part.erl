%%%-------------------------------------------------------------------
%%% File    : sim_part.erl
%%% Author  : Masahiko KAGAWA <info@protgram.com>
%%% Description : 
%%%
%%% Created : 2010/05/07
%%%-------------------------------------------------------------------
-module(sim_part).

-behaviour(gen_server).

%% API
-export([start_link/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start_link(Name,  Node,Module, Function, Arguments) ->
	gen_server:start_link({global, Name}, ?MODULE, [Node,Module,Function,Arguments], [{timeout, 6000000}]).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Node,Module,Function,Arguments]) ->
	io:format("sim_part ~p,~p ~p ~p init start~n",[Node,Module,Function,Arguments]),
	Pid = spawn_link( Node,Module, Function, Arguments),
	put(childPid, Pid),
	put(mod, Module),
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
	io:format("handle_call received in sub from ~p: ~p~n", [From, Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({state_inf, Orig,Parameter}, State) ->
%%	io:format("sim_part state_inf  ~p From = ~p ~n",[get(mod),Orig]),
	get(childPid) !{state_inf, Orig,Parameter},
    {noreply, State};

handle_cast({msgsav, To,Message}, State) ->
	get(childPid) !{msgsav, To,Message},
    {noreply, State};

handle_cast({update_ind, Orig,Parameter}, State) ->
%%	io:format("sim_part update_ind From = ~p ~n",[Orig]),
	get(childPid) !{update_ind, Orig,Parameter},
    {noreply, State};

handle_cast({getinfo_req, To,Message}, State) ->
	get(childPid) !{getinfo_req, To,Message},
    {noreply, State};

handle_cast({getinfo_res, To,Message}, State) ->
	get(childPid) !{getinfo_res, To,Message},
    {noreply, State};

handle_cast({scenario_req, To,Message}, State) ->
	get(childPid) !{scenario_req, To,Message},
    {noreply, State};

handle_cast({scenario_res, To,Message}, State) ->
	get(childPid) !{scenario_res, To,Message},
    {noreply, State};

handle_cast({scenario_can, To,Message}, State) ->
	get(childPid) !{scenario_can, To,Message},
    {noreply, State};

handle_cast({logstart_ind, To,Message}, State) ->
	get(childPid) !{logstart_ind, To,Message},
    {noreply, State};

handle_cast({log_inf, To,Message}, State) ->
	get(childPid) !{log_inf, To,Message},
    {noreply, State};

handle_cast({logstop_ind, To,Message}, State) ->
	get(childPid) !{logstop_ind, To,Message},
    {noreply, State};


handle_cast(_Msg, State) ->
	io:format("sim_part unknown  Msg = ~p ~n",[_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
