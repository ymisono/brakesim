%%%-------------------------------------------------------------------
%%% File    : 
%%% Author  :   
%%% Description : 
%%%
%%% Created : 
%%%-------------------------------------------------------------------
-module(cruisectl_gui).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/1]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start(Parent) ->
	io:format("check~n"),
	put(val,0),
	GSHandle = gs:start(),
	{WinX, WinY} = {800, 30},
	WinHandle = gs:create(window, GSHandle,
						  [
						   {title, "Cruise Control"},
						   {width, 230},
						   {height,200},
						   {x, WinX},
						   {y, WinY},
						   {bg, black},
						   {keypress, true}]),
	put(mainWindow, WinHandle),
	gs:create(label, WinHandle, [{x, 10}, {y, 5}, {align, w}, {bw, 1}, {fg, green}, {bg, black}, {height, 20}, {width, 100}, {label, {text, "[Parameters]"}}, {font, {screen, bold, 10}}]),
	put(pedalScale, gs:create(scale, WinHandle, [{x, 10}, {y, 20}, {orient, horizontal}, {fg, green}, {bg, black}, {pos, 0}, {range, {0, 180}}, {showvalue, true}, {text, "Cruise Speed"}, {data, pedal}, {pos, 0}, {buttonpress, true}, {buttonrelease, true},{width, 200}])),
	put(quitButton, gs:create(button, quit, WinHandle, [{label, {text,"quit"}} ,{width,80},{height,25}, {x,10},{y,170}])),
	put(parentPid, Parent),
	gs:config(WinHandle, {map, true}),
	event_loop(none),
	ok.



%% ====================================================================
%% Internal functions
%% ====================================================================
event_loop(Option) ->
	receive
		{gs, quit, _, _, _} ->
			gs:destroy(get(mainWindow));
		{gs, _, destroy, _, _} ->
			true;

		{gs, _, click, pedal, [CruiseSpeed|_]} ->
%%	io:format("throttle_gui ~p  ~n", [Angle]),
			PPid = get(parentPid),

			case get(val) of
				Old when Old =:= CruiseSpeed -> 
					false;
				Old when is_pid(PPid) ->
					io:format("in event_loop(cruisectl_gui)~n"),
					gen_server:cast({global,dcs},{state_inf, cruisespeed, CruiseSpeed}),
					PPid!{state_inf,chgui,CruiseSpeed};
				_->false
			end,
			put(val,CruiseSpeed),
			event_loop(Option);

		{state_inf,parent,Toru} ->
			gs:config(get(pedalScale),{pos, Toru} ),
			event_loop(Option);
		_Any ->
		%%	erlang:display(_Any),
			event_loop(Option)
	end.

