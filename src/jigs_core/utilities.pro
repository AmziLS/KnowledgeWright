%------------------------------------------------
% CVS history
%
% $Log: utilities.pro,v $
% Revision 1.1.1.1  2002/12/10 16:49:52  dennis
% Moved to jigs_core for Eclipse.
%
% Revision 1.8  2002/02/22 18:27:48  dennis
% Add data_table and find() function. Rename table to rules_table.
%
% Revision 1.7  2002/02/21 04:39:14  dennis
% added data_table query support, used new :- include directive for
% operators in each file
%
% Revision 1.6  2002/01/29 21:56:03  mary
% Add operators for command line build. Other makefile nits.
%
% Revision 1.5  2001/02/15 00:12:29  dennis
% added axrf files
%
% Revision 1.4  2000/12/07 04:01:00  dennis
% save sessions to disk
%
% Revision 1.3  2000/11/29 19:09:50  dennis
% Added CVS logging to source files
%
%
%------------------------------------------------

% Operators

:- include('kw_ops.pro').


%
% Utilities
%

get_init_parm(PARM, VAL) :-
   initial_parms(IPARMS),
   get_property(IPARMS, PARM, VAL).

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

% defined in lists
%remove_dups([], []).
%remove_dups([A|Y], [A|Z]) :-
%   remove_all(A,Y,Y2),
%   remove_dups(Y2,Z).

remove_all(A, [], []).
remove_all(A, [A|Y], Z) :-
   !, remove_all(A, Y, Z).
remove_all(A, [B|Y], [B|Z]) :-
   remove_all(A, Y, Z).

remove_last([X], X, []) :- !.
remove_last([X|Y], Z, [X|ZZ]) :-
   remove_last(Y, Z, ZZ).

% tfm values can be true, maybe, or false. so
% the minimum (worst case) is false.
   
tfm_min(X, Y, false) :- (X = false; Y = false), !.
tfm_min(X, Y, maybe) :- (X = maybe; Y = maybe), !.
tfm_min(_, _, true).

tfm_not(true, false).
tfm_not(false, true).
tfm_not(maybe, maybe).

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

