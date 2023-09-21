:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(_event, angle_max) :- deep_subdict(_{'angle':X}, _event), =<(X, 0.0872665).
match(_event, angle_min) :- deep_subdict(_{'angle':X}, _event), >=(X, -(0.0872665)).
match(_event, pos_max) :- deep_subdict(_{'pos':Y}, _event), <(Y, 0.5).
match(_event, pos_min) :- deep_subdict(_{'pos':Y}, _event), >(Y, -(0.5)).
match(_event, any) :- deep_subdict(_, _event).
match(_, any).
trace_expression('Main', Main) :- Main=(Good\/Bad), Good=(((((angle_max:eps)/\(angle_min:eps))/\(pos_max:eps))/\(pos_min:eps))*(Main\/eps)), Bad=((any:eps)*Main).
