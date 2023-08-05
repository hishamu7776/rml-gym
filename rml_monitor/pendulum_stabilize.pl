:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(_event, good_theta1) :- deep_subdict(_{'theta':X}, _event), =<(X, 0.5).
match(_event, good_theta2) :- deep_subdict(_{'theta':X}, _event), >=(X, -(0.5)).
match(_, any).
trace_expression('Main', Main) :- Main=star(((good_theta1:eps)/\(good_theta2:eps))).
