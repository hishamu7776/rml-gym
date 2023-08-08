:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(_event, good_theta1) :- deep_subdict(_{'theta':X}, _event), =<(X, 0.5).
match(_event, good_theta2) :- deep_subdict(_{'theta':X}, _event), >=(X, -(0.5)).
match(_event, any) :- deep_subdict(_, _event).
match(_, any).
trace_expression('Main', Main) :- Main=(Good\/Bad), Good=(((good_theta1:eps)/\(good_theta2:eps))*(Main\/eps)), Bad=((any:eps)*Main).
