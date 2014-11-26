%% This is the application resource file (.app file) for the 'base'
%% application.
{application, brakesim, 
 [{description, "OSS study Brake Simulator"},
  {vsn, "2.2"},
  {modules, [sim_app, sim_sup, sim_part, sim_db,loadenv,
	     brake, brake_gui, dcs,distance,distance_gui,
		output,output_gui,throttle,throttle_gui,wheel,
		wheel_gui,balancectl,scenario]},	
  {registered,[dcs, wheelFright, wheelRright,wheelFleft,wheelRleft,frsensor,bksensor,risensor,lfsensor,throttle,brake,output,loadenv,sim_db,balancectl,transmission,scenario]},
  {applications, [kernel,stdlib]},
  {mod, {sim_app,[]}},
  {start_phases, []}
 ]}.
