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

%:- load(list).
/*
s(unique_rule, $poo poo$).
s(unique_rule, $a + b$).
s(unique_rule, $[if a = b then x = y]$).
s(list_rule, $[if a = b then x = y]$).
s(list_rule, $wow this is a bunch of crap$).
s(term, $hows(this, for, a, problem$).
s(term, $and what's this?$).
s(term, $[or,[miss,matched] say what$).
s(term, $end(ok)$).

test :-
   s(CHK,S),
   writeq(checking:CHK:S), nl,
   error_check(CHK, S, OUT),
   out_list(OUT), nl,
   fail.
test.

out_list([]) :- !.
out_list([X|Y]) :-
   tab(2), writeq(X), nl,
   !, out_list(Y).
out_list(X) :-
   tab(2), writeq(X), nl.
*/

auth_check_slot(MODULE, TYPE, ID, SLOT, SVALUE, ERROR_LIST) :-
   sch_get_slot_facet(TYPE, SLOT, type, SLOTTYPE),
   (SLOTTYPE = number ; SLOTTYPE = list_of_numbers),
   !,
   error_check(number, SVALUE, ERROR_LIST0),
   (ERROR_LIST0 = ok -> ERROR_LIST = ERROR_LIST0
   ;append([slot = SLOT], ERROR_LIST0, ERROR_LIST) ).
auth_check_slot(MODULE, TYPE, ID, SLOT, SVALUE, ERROR_LIST) :-
   sch_get_slot_facet(TYPE, SLOT, type, SLOTTYPE),
   SLOTTYPE = rules,
   !,
   error_check(unique_rule, SVALUE, ERROR_LIST0),
   (ERROR_LIST0 = ok -> ERROR_LIST = ERROR_LIST0
   ;append([slot = SLOT], ERROR_LIST0, ERROR_LIST) ).
auth_check_slot(MODULE, TYPE, ID, SLOT, SVALUE, ERROR_LIST) :-
   sch_get_slot_facet(TYPE, SLOT, type, SLOTTYPE),
   (SLOTTYPE = term ; SLOTTYPE = list_of_terms),
   !,
   error_check(term, SVALUE, ERROR_LIST0),
   (ERROR_LIST0 = ok -> ERROR_LIST = ERROR_LIST0
   ;append([slot = SLOT], ERROR_LIST0, ERROR_LIST) ).

error_check(CHK, S, OUT) :-
   % string_term GPFs when string is empty and error is not thrown
   catch( string_term(S,T), E, except$(E,[test=CHK],OUT) ),
   (var(OUT) ->
      syntax_check(CHK, T, OUT)
      ;
      true).

syntax_check(number, T, ok) :- (integer(T) ; float(T)).
syntax_check(number, T, [message = $Invalid number format$]).
syntax_check(unique_rule, T, E) :- !, syntax_unique(T, E).
syntax_check(list_rule, T, E) :- !, syntax_multi(T, E).
syntax_check(_, _, ok).

syntax_unique([], ok) :- !.
syntax_unique([if COND then FACT = VAL|REST], E) :-
   !,
   syntax_unique(REST, E).
syntax_unique([FACT = VAL|REST], E) :-
   !,
   syntax_unique(REST, E).
syntax_unique([if COND then FACT include VAL|REST],
      [message = $Cannot use 'include' on the then-side of rules$,
       type = read, read_buffer = if COND then FACT include VAL]) :- !.
syntax_unique([X|REST],
      [message = $Invalid rule syntax$, type = read,
       read_buffer = X]) :- !.
syntax_unique(X, [message = $rules must be in a list$, type = read, read_buffer = X]).

syntax_multi([], ok) :- !.
syntax_multi([if COND then FACT include VAL|REST], E) :-
   !,
   syntax_multi(REST, E).
syntax_multi([if COND then FACT = VAL|REST],
      [message = $Cannot use '=' for multi-valued rule$, type = read,
       read_buffer = if COND then FACT = VAL]) :- !.
syntax_multi([X|REST],
      [message = $Invalid rule syntax$, type = read,
       read_buffer = X]) :- !.
syntax_multi(X, [message = $rules must be in a list$, type = read, read_buffer = X]).

%except$(E,_,_) :-
%  write($---caught---$), nl,
%  write(E), nl,
%  write($--- ---$), nl,
%  fail.
except$(error(Err, Attrs),START,OUT) :-
  member(rc=RC, Attrs),
  member(type=TYPE, Attrs),
  member(message=MSG, Attrs),
  OUTa = [message=MSG,type=TYPE,rc=RC,error=Err|START],
  (member(read_buffer=RB, Attrs) ->
      OUTb = [read_buffer=RB|OUTa]
      ;
      OUTb = OUTa),
  reverse(OUTb, OUT),
  !.
except$(error(Err, Attrs),START,OUT) :-
  append(START, [alarm= $unexpected error$, error=Err|Attrs], OUT),
  !.
except$(E,START,[strange_catch = E|START]).
