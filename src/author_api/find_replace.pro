%------------------------------------------------
% CVS history
%
% $Log: find_replace.pro,v $
% Revision 1.9  2002/02/21 04:39:13  dennis
% added data_table query support, used new :- include directive for
% operators in each file
%
% Revision 1.8  2002/01/29 21:56:03  mary
% Add operators for command line build. Other makefile nits.
%
% Revision 1.7  2001/11/11 14:50:35  mary
% Fixed bugs replacing column names and variables. Disallow two folders with
% the same name. Allow two variables next to each other.
%
% Revision 1.6  2001/02/09 01:10:55  mary
% Implement replace object name for list of atoms and list of terms slots.
% Moved flaten from xref to utilities.
%
% Revision 1.4  2001/02/08 03:33:08  mary
% Added case matching option to auth_find.
%
% Revision 1.3  2001/01/30 15:01:26  mary
% Return icon name in find.
%
% Revision 1.2  2001/01/28 18:14:57  mary
% Add joblook 3->4 convert. Return full slot/cell value in find.
%
%
%
%------------------------------------------------

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

%-------------------------
% find
%

% Find text in a knowledgebase (using string<->term)
% Returns found(path, name, slot, string, loc)
% loc is index(I) or list(row,index) or table(row,col,index)
auth_find($$, _, []) :- !.
auth_find(TEXT, MATCHCASE, LIST) :-
   findall(found(PATH, NAME, SLOT, STRING, LOC, ICON), auth_find_text(TEXT, MATCHCASE, PATH, NAME, SLOT, STRING, LOC, ICON), LIST).

% auth_find_text($colors$, C, P, N, S, T, L).

auth_find_text(TEXT, MATCHCASE, PATH, NAME, SLOT, STRING, LOC, ICON) :-
   once auth_get_classes(CLASSES),
   member(CLASS, CLASSES),
   once auth_get_schema_slots(CLASS, SLOTS),
   FRAME =.. [CLASS, NAME, PATH, _],
   call(FRAME),
%write('TRYING':FRAME),nl,
   member(SLOT, SLOTS),
   once auth_get_slot(PATH, CLASS, NAME, SLOT, VALUE),
   once auth_schema_slot_facet(CLASS, SLOT, type, TYPE),
   find_string_slot(TEXT, TYPE, VALUE, MATCHCASE, STRING, LOC),
   (auth_schema_slot_facet(CLASS, 'id', icon, ICON) -> true ; ICON = $$ ).

find_string_slot(_, id, _, _, _, _) :- fail.
find_string_slot(TEXT, string, VALUE, true, VALUE, index(INDEX)) :-
   substring(VALUE, INDEX, _, TEXT).
find_string_slot(TEXT, string, VALUE, false, VALUE, index(INDEX)) :-
   substring_nocase(VALUE, INDEX, _, TEXT).
find_string_slot(TEXT, atom, VALUE, true, STRING, index(INDEX)) :-
   string_term(STRING, VALUE),
   substring(STRING, INDEX, _, TEXT).
find_string_slot(TEXT, atom, VALUE, false, STRING, index(INDEX)) :-
   string_term(STRING, VALUE),
   substring_nocase(STRING, INDEX, _, TEXT).
find_string_slot(TEXT, number, VALUE, true, STRING, index(INDEX)) :-
   string_term(STRING, VALUE),
   substring(STRING, INDEX, _, TEXT).
find_string_slot(TEXT, number, VALUE, false, STRING, index(INDEX)) :-
   string_term(STRING, VALUE),
   substring_nocase(STRING, INDEX, _, TEXT).
find_string_slot(TEXT, term, VALUE, true, STRING, index(INDEX)) :-
   string_term(STRING, VALUE),
   substring(STRING, INDEX, _, TEXT).
find_string_slot(TEXT, term, VALUE, true, STRING, index(INDEX)) :-
   string_term(STRING, VALUE),
   substring_nocase(STRING, INDEX, _, TEXT).
find_string_slot(TEXT, list_of_terms, [HEAD | TAIL], MATCHCASE, STRING, table(ROW,COL,INDEX)) :-
   islist(HEAD),
   !,
   find_string_in_table(TEXT, [HEAD | TAIL], MATCHCASE, 1, STRING, table(ROW,COL,INDEX)).
find_string_slot(TEXT, TYPE, [HEAD | TAIL], MATCHCASE, STRING, list(ROW,INDEX)) :-
   (TYPE == list_of_atoms ; TYPE == list_of_strings ; 
    TYPE == list_of_numbers ; TYPE == list_of_terms),
   find_string_in_list(TEXT, [HEAD | TAIL], MATCHCASE, 1, STRING, list(ROW,INDEX)).

find_string_in_table(TEXT, [], _, _, _, _) :- !, fail.
find_string_in_table(TEXT, [HEAD | TAIL], MATCHCASE, ROW, STRING, table(ROW,COL,INDEX)) :-
   find_string_in_list(TEXT, HEAD, MATCHCASE, 1, STRING, list(COL,INDEX)).
find_string_in_table(TEXT, [HEAD | TAIL], MATCHCASE, R, STRING, table(ROW,COL,INDEX)) :-
   RR is R + 1,
   find_string_in_table(TEXT, TAIL, MATCHCASE, RR, STRING, table(ROW,COL,INDEX)).

find_string_in_list(TEXT, [], _, _, _, _) :- !, fail.
find_string_in_list(TEXT, [HEAD | TAIL], true, ROW, STRING, list(ROW,INDEX)) :-
   string_term(STRING, HEAD),
   substring(STRING, INDEX, _, TEXT).
find_string_in_list(TEXT, [HEAD | TAIL], false, ROW, STRING, list(ROW,INDEX)) :-
   string_term(STRING, HEAD),
   substring_nocase(STRING, INDEX, _, TEXT).
find_string_in_list(TEXT, [HEAD | TAIL], MATCHCASE, R, STRING, list(ROW,INDEX)) :-
   RR is R + 1,
   find_string_in_list(TEXT, TAIL, MATCHCASE, RR, STRING, list(ROW,INDEX)).

substring_nocase(STRING, INDEX, LENGTH, TEXT) :-
   string_atom(STRING, ASTRING),
   atom_uplow(ASTRINGUP, ASTRING),
   string_atom(STRINGUP, ASTRINGUP),
   string_atom(TEXT, ATEXT),
   atom_uplow(ATEXTUP, ATEXT),
   string_atom(TEXTUP, ATEXTUP),
   substring(STRINGUP, INDEX, LENGTH, TEXTUP).

%-------------------------
% replace
%

% Returns found(path, class, name, slot, value, newvalue, icon)
auth_replace_object_name(X, X, []) :- !.
auth_replace_object_name(OLDNAME, NEWNAME, RLIST) :-
   findall(found(PATH, CLASS, NAME, SLOT, VALUE, NEWVALUE, ICON), find_object_name(PATH, CLASS, NAME, SLOT, OLDNAME, NEWNAME, VALUE, NEWVALUE, ICON), LIST),
   once filter_frames(LIST, FLIST),
   once replace_frames(FLIST, RLIST).

% Get the new slot value
find_object_name(PATH, CLASS, NAME, SLOT, OLDNAME, NEWNAME, VALUE, NEWVALUE, ICON) :-
   % Get a class to process
   once auth_get_classes(CLASSES),
   member(CLASS, CLASSES),
   once auth_get_schema_slots(CLASS, SLOTS),

   % Get an object to process
   FRAME =.. [CLASS, NAME, PATH, SLOTLIST],
   call(FRAME),

   % Get a slot to process
   member(SLOT, SLOTS),

   % Process all non-id slots
   once auth_schema_slot_facet(CLASS, SLOT, type, TYPE),
   (TYPE == id -> 
      fail
   ;
      once member(SLOT = VALUE, SLOTLIST),
      once replace_object_name(CLASS, SLOT, TYPE, VALUE, OLDNAME, NEWNAME, NEWVALUE),
      ((once auth_schema_slot_facet(CLASS, 'id', icon, ICON)) -> true ; ICON = $$ )
   ).

% Only handle the cases that need replacing, the rest will fail which
% is fine, it just proceeds to the next case.

% Replace strings with vars
replace_object_name(CLASS, SLOT, string, text(TEXT_EXP), OLDNAME, NEWNAME, text(NEW_EXP)) :-
   auth_schema_slot_facet(CLASS, SLOT, vars, true),
   replace_var_object_name(TEXT_EXP, OLDNAME, NEWNAME, NEW_EXP).
% Replace atoms in lists
replace_object_name(CLASS, SLOT, list_of_atoms, LIST, OLDNAME, NEWNAME, NEWLIST) :-
   replace_list_elements(OLDNAME, NEWNAME, LIST, NEWLIST).
% Replace object names in conditions and table header
replace_object_name(CLASS, SLOT, list_of_terms, TABLE, OLDNAME, NEWNAME, NEWTABLE) :-
   TABLE = [FIRSTROW | _],
   islist(FIRSTROW),
   !,
   replace_name_in_table(OLDNAME, NEWNAME, TABLE, NEWTABLE).
replace_object_name(CLASS, SLOT, list_of_atoms, [HEAD | TAIL], OLDNAME, NEWNAME, NEWTABLE) :-
   replace_name_in_list(OLDNAME, NEWNAME, [HEAD | TAIL], NEWLIST).

% Replace elements in a list
replace_list_elements(_, _, [], []) :-!.
replace_list_elements(OLD, NEW, [OLD | REST], [NEW | REST2]) :-
   replace_list_elements(OLD, NEW, REST, REST2).
replace_list_elements(OLD, NEW, [NOTOLD | REST], [NOTOLD | REST2]) :-
   replace_list_elements(OLD, NEW, REST, REST2).

% Replace an object name referred to in percents in a text value
replace_var_object_name(T1 + T2, OLDNAME, NEWNAME, EXP1 + EXP2) :-
    !,
   replace_var_object_name(T1, OLDNAME, NEWNAME, EXP1),
   replace_var_object_name(T2, OLDNAME, NEWNAME, EXP2).
replace_var_object_name(date(Y,M,D), _, _, date(Y,M,D)) :- !.
replace_var_object_name(system(PARM), _, _, system(PARM)) :- !.
replace_var_object_name(STRING, _, _, STRING) :-
   string(STRING), 
   !.
replace_var_object_name(ATOM, OLDNAME, NEWNAME, NEWNAME) :-
   atom(ATOM),
   ATOM == OLDNAME,
   !.
replace_var_object_name(X, _, _, X).

% Get rid of the replaced items where nothing was replace
filter_frames([], []) :- !.
filter_frames([found(PATH, CLASS, NAME, SLOT, VALUE, NEWVALUE, ICON) | REST], REST2) :-
   VALUE == NEWVALUE,
   filter_frames(REST, REST2).
filter_frames([found(PATH, CLASS, NAME, SLOT, VALUE, NEWVALUE, ICON) | REST], [found(PATH, CLASS, NAME, SLOT, VALUE, NEWVALUE, ICON) | REST2]) :-
   filter_frames(REST, REST2).

% Replace the slots
replace_frames([], []) :- !.
replace_frames([found(PATH, CLASS, NAME, SLOT, VALUE, NEWVALUE, ICON) | REST], [found(PATH, NAME, SLOT, SLOTVALUE, ICON) | REST2]) :-
   replace_slot_no_trans(PATH, CLASS, NAME, SLOT, NEWVALUE),
   auth_get_slot(PATH, CLASS, NAME, SLOT, SLOTVALUE),
   replace_frames(REST, REST2).

% Replaces a slot without translation
replace_slot_no_trans(FOLDER_PATH, TYPE, ID, SLOT, VALUE) :-
   TERM =.. [TYPE, ID, FOLDER_PATH, SLOTLIST],
   call(TERM),
   replace_elem(SLOT = _, SLOT = VALUE, SLOTLIST, NEWSLOTLIST),
   retract(TERM),
   NEWTERM =.. [TYPE, ID, FOLDER_PATH, NEWSLOTLIST],
   assert(NEWTERM), !.

% Replace object names in terms
replace_name_in_table(OLDNAME, NEWNAME, [], []) :- !.
replace_name_in_table(OLDNAME, NEWNAME, [ROW | TAIL], [NEWROW | TAIL2]) :-
   replace_name_in_list(OLDNAME, NEWNAME, ROW, NEWROW),
   replace_name_in_table(OLDNAME, NEWNAME, TAIL, TAIL2).

replace_name_in_list(OLDNAME, NEWNAME, [], []) :- !.
replace_name_in_list(OLDNAME, NEWNAME, [TERM | TAIL], [NEWTERM | TAIL2]) :-
   replace_name_in_term(OLDNAME, NEWNAME, TERM, NEWTERM),
   replace_name_in_list(OLDNAME, NEWNAME, TAIL, TAIL2).

replace_name_in_term(OLDNAME, NEWNAME, text(TEXT_EXP), text(NEW_EXP)) :-
   replace_var_object_name(TEXT_EXP, OLDNAME, NEWNAME, NEW_EXP).
replace_name_in_term(OLDNAME, NEWNAME, not C1, not C2) :-
   replace_name_in_term(OLDNAME, NEWNAME, C1, C2).
replace_name_in_term(OLDNAME, NEWNAME, C1 or C2, C11 or C22) :-
   replace_name_in_term(OLDNAME, NEWNAME, C1, C11),
   replace_name_in_term(OLDNAME, NEWNAME, C2, C22).
replace_name_in_term(OLDNAME, NEWNAME, C1 and C2, C11 and C22) :-
   replace_name_in_term(OLDNAME, NEWNAME, C1, C11),
   replace_name_in_term(OLDNAME, NEWNAME, C2, C22).
replace_name_in_term(OLDNAME, NEWNAME, CIN, COUT) :-
   CIN =.. [OP, C1, C2],
   replace_name_in_term(OLDNAME, NEWNAME, C1, C11),
   replace_name_in_term(OLDNAME, NEWNAME, C2, C22),
   COUT =.. [OP, C11, C22].
replace_name_in_term(OLDNAME, NEWNAME, CIN, COUT) :-
   CIN =.. [OP, C1],
   replace_name_in_term(OLDNAME, NEWNAME, C1, C11),
   COUT =.. [OP, C11].
replace_name_in_term(OLDNAME, NEWNAME, VALUE, NEWNAME) :-
   atom(VALUE),
   OLDNAME == VALUE.
replace_name_in_term(_, _, X, X).

