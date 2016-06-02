%
% Utilities
%

% Operators

:- include('..\\jigs\\kw_ops.pro').

/*
:- op(900, fy, [?, bug]).
:- op(790, fx, if).     	% prefix operator
:- op(780, xfx, then).  	% infix operator
:- op(775, xfy, or).    	% infix that can be linked
:- op(770, xfy, and).   	% infix that can be linked
:- op(680, xfx, ==>).		% infix operator
:- op(650, xfx, <=).		% infix operator
:- op(650, xfx, =>).		% infix operator
:- op(650, xfx, include).	% infix operator
:- op(650, xfx, exclude).	% infix operator
:- op(650, xfx, include_only).	% infix operator
:- op(650, xfx, number_of_items).  % infix operator
:- op(650, xfx, contains).	% infix operator
:- op(650, xfx, contains_nocase). % infix operator
:- op(650, xfx, is).		% infix operator
:- op(650, xf, is_known).	% postfix operator
:- op(650, xfx, from).          % infix operator, used in db queries
:- op(400, fx, last).           % prefix operator, used to modify db record name
:- op(400, fx, <).              % used in tables, where other arg is understood
:- op(400, fx, >).              % used in tables, where other arg is understood
:- op(400, fx, <=).             % used in tables, where other arg is understood
:- op(400, fx, =<).             % used in tables, where other arg is understood
:- op(400, fx, >=).             % used in tables, where other arg is understood
:- op(400, fx, include).        % used in tables, where other arg is understood
:- op(400, fx, exclude).        % used in tables, where other arg is understood
:- op(400, fx, include_only).   % used in tables, where other arg is understood
:- op(400, fx, number_of_items). % used in tables, where other arg is understood
:- op(400, fx, contains).       % used in tables, where other arg is understood
:- op(400, fx, contains_nocase). % used in tables, where other arg is understood
:- op(400, fx, exists).         % used for db records
:- op(200, fx, /).              % used for folders
:- op(50, xf, days).
:- op(50, xf, months).
:- op(50, xf, weeks).
:- op(50, xf, years).
*/

% get_slots(+ENTITY, +PATH, -SLOTS)
% ENTITY - entity(name), e.g. question(what).
% PATH - path for the entity.
% SLOTS - the list of slots for the entity.

get_slots(ENTITY, PATH, SLOTS) :-
   ENTITY =.. [TYPE, INSTANCE],
   CALL =.. [TYPE, INSTANCE, PATH, SLOTS],
   call(CALL).

get_slots(ENTITY, SLOTS) :-
   ENTITY =.. [TYPE, INSTANCE],
   CALL =.. [TYPE, INSTANCE, _, SLOTS],
   call(CALL).

% Get a property value from a property list of
% PROPERTY = VALUE pairs.

get_property(LIST, PROPERTY, VALUE) :-
   get_av(PROPERTY, VALUE, LIST), !.

% Get a slot value from a database entity that
% is in the frame format.  For example:
% get_slot(solution(S), output, OUT).

get_slot(ENTITY, SLOT, VALUE) :-
   ENTITY =.. [TYPE, INSTANCE],
   CALL =.. [TYPE, INSTANCE, PATH, SLOTS],
   call(CALL),
   get_av(SLOT, VALUE, SLOTS).

get_slot(ENTITY, PATH, SLOT, VALUE) :-
   ENTITY =.. [TYPE, INSTANCE],
   CALL =.. [TYPE, INSTANCE, PATH, SLOTS],
   call(CALL),
   get_av(SLOT, VALUE, SLOTS).

frame_exists(ENTITY) :-
   ENTITY =.. [TYPE, INSTANCE],
   CALL =.. [TYPE, INSTANCE, _, _],
   call(CALL).

% a special version of member, non-backtracking,
% for a=v pairs in lists.

get_av(A, V, [A = V2|_]) :-
   !,
   V = V2.
get_av(A, V, [_|Z]) :-
   get_av(A, V, Z).

/*
remove_dups([], []).
remove_dups([A|Y], [A|Z]) :-
   remove_all(A,Y,Y2),
   remove_dups(Y2,Z).

remove_all(A, [], []).
remove_all(A, [A|Y], Z) :-
   !, remove_all(A, Y, Z).
remove_all(A, [B|Y], [B|Z]) :-
   remove_all(A, Y, Z).
*/

remove_last([X], X, []) :- !.
remove_last([X|Y], Z, [X|ZZ]) :-
   remove_last(Y, Z, ZZ).

% tfm values can be true, maybe, or false. so
% the minimum (worst case) is false.
   
tfm_min(X, Y, false) :- (X = false; Y = false), !.
tfm_min(X, Y, maybe) :- (X = maybe; Y = maybe), !.
tfm_min(_, _, true).

stub(X) :-
   throw(error(not_implemented, [predicate=X])).


% a fixed version of flatten, deals with empty lists.
      
flaten([], []).
flaten( [X|T], [X|T2] ) :-
  var(X),
  !, flaten(T, T2).
flaten([ [] | T], T2) :-
  !, flaten(T, T2).
flaten([ [H|T] | T2], T3) :-
  flaten([H|T], []),
  !, flaten(T2, T3).
flaten([ [H|T] | T2], [H1|T3]) :-
  flaten([H|T], [H1|T1]),
  !, flaten([T1|T2], T3).
flaten([H|T], [H|T2]) :-
  flaten(T, T2).

