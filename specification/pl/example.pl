:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(_event, verify_x) :- deep_subdict(_{'x':X}, _event), <(X, 5).
match(_event, any) :- deep_subdict(_, _event).
match(_, any).
trace_expression('Main', Main) :- Main=(Satisfied\/NotSatisfied), Satisfied=((verify_x:eps)*(Main\/eps)), NotSatisfied=((any:eps)*Main).
