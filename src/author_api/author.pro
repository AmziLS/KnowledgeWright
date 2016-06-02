% Operators

:- include('..\\jigs\\kw_ops.pro').

% Libraries

:- import(list).
:- import(register).
:- import(amzi_compiler).


test :-
   auth_init,
   once auth_open_check('\\amzi\\dev\\kw4\\src\\samples\\basic\\amzi_website_advisor.kb', '\\amzi\\dev\\kw4\\src\\ide\\jigs\\', FILEVER, JIGVER),
   auth_open('c:\\amzi\\dev\\kw4\\src\\samples\\basic\\amzi_website_advisor.kb', 'c:\\amzi\\dev\\kw4\\src\\ide\\jigs\\'),
%   knowledgebase(X, Y, Z),
%   writeq(knowledgebase(X,Y)),nl,
   auth_get_paths(_, 'knowledgebase', _VAR_X),
   writeq(_VAR_X),nl.
test2 :-
   write($test2$),nl,
   auth_init,
   write($auth_open_check$),nl,
   auth_open_check('../samples/basic/amzi_website_advisor.kb', './jigs/', FILEVER, JIGVER),
   write(FILEVER),nl,
   write(JIGVER),nl,
   write($auth_open$),nl,
   auth_open('../samples/basic/amzi_website_advisor.kb', './jigs/'),
   auth_get_root_folders(FOLDERS),
   write(FOLDERS),nl,
   auth_get_folderless_objects(OBJECTS),
   write(OBJECTS),nl.
test3 :-
   auth_init,
   auth_compile('e:\\amzi\\dev\\kw4\\src\\tests\\colors2.kb', 'e:\\amzi\\dev\\kw4\\src\\tests\\colors2.kbc').
test4 :-
   auth_init,
   consult('E:\\amzi\\dev\\kw4\\src\\ide\\jigs\\joblook.jig'),
   auth_new('E:\\amzi\\dev\\kw4\\src\\tests\\', 'jobtest.kb', 'joblook', 'E:\\amzi\\dev\\kw4\\src\\ide\\jigs\\').
test5 :-
   write($test5$),nl,
   once auth_init,
   once auth_open_check('\\amzi\\dev\\kw4\\src\\tests\\test.kb', '\\amzi\\dev\\kw4\\src\\ide\\jigs\\', FILEVER, JIGVER),
   once auth_open('\\amzi\\dev\\kw4\\src\\tests\\test.kb', '\\amzi\\dev\\kw4\\src\\ide\\jigs\\'),
%   consult('\\amzi\\dev\\kw4\\src\\tests\\test.kb'),
%   consult('\\amzi\\dev\\kw4\\src\\ide\\jigs\\basic.jig'),
   auth_new_object(/, 'text', 'foo'),
   auth_delete_object(/, 'text', 'foo'),
   auth_new_object(/, 'fact', 'foo'),
   auth_delete_object(/, 'fact', 'foo'),
   auth_save('\\amzi\\dev\\kw4\\src\\tests\\', 'test.kb', false),
   write($done$), nl.
test5 :-
   write($test5 failed$), nl.
test6 :-
   auth_init,
   once auth_open_check('\\amzi\\dev\\kw4\\src\\tests\\Diagnostic_Commerce_Detail.kb', '\\amzi\\dev\\kw4\\src\\ide\\jigs\\', FILEVER, JIGVER),
   auth_open('c:\\amzi\\dev\\kw4\\src\\tests\\Diagnostic_Commerce_Detail.kb', 'c:\\amzi\\dev\\kw4\\src\\ide\\jigs\\'),
   auth_save('c:\\amzi\\dev\\kw4\\src\\tests\\', 'Diagnostic_Commerce_Detail.kb', false).

%   auth_open('\\amzi\\dev\\kw4\\src\\tests\\colors2.kb', '\\amzi\\dev\\kw4\\src\\ide\\jigs\\'),
%   write($auth_set_slot$), nl,
%   auth_set_slot(/, text, color_report, text, $hi %foo%%bar%$),
%   write($done$), nl.

main :-
   test.

%-------------------------
% author interface
%

% Compile a knowledgebase
auth_compile(INPATHNAME, OUTPATHNAME) :-
   amzi_compiler:xcompile(INPATHNAME, OUTPATHNAME, null).

% Is required by xcompile. Can be implemented as an extended predicate.
% --> Uncomment to run in the listener.
report(X).

% Initialize the authoring interface
%   Called once after loading the XPL file
%   Called BEFORE schema is loaded!!!
auth_init :-
% The following line breaks Win98!!!
%   set_mode(string_esc, off),

   set_mode(upper_case_atoms, on),
   set_mode(double_quote_strings, on).

% Create new logicbase. 
%   Must have at least one object so that the name of the root 
%   folder can be determined.
auth_new(PATH, FILE, KBTYPE, JIGPATH) :-
   % Assert the type and version
   schema_global(version, VERSION),
   asserta(knowledgewright_jig(KBTYPE, VERSION)),

   % Get the first frame
   schema_global(first_object, TYPE),
   schema_global(first_object_name, NAME),

   % Create it
   auth_new_object(/, TYPE, NAME),

   % Save it
   auth_save(PATH, FILE, false),
   atomlist_concat([PATH, FILE], FILEPATH),
   auth_close(FILEPATH),

   % Load it
   auth_open(FILEPATH, JIGPATH).

% Check if the existing logicbase needs conversion
auth_open_check(FILE, JIGPATH, FILEVERSION, JIGVERSION) :-
   string_atom(SJIGPATH, JIGPATH),
   kw_consult_vdiff(FILE, SJIGPATH, FILEVERSION, JIGVERSION).

% Open an existing logicbase in the full pathname specified
%   Called BEFORE schema is loaded!!!
auth_open(FILE, JIGPATH) :-
   string_atom(SJIGPATH, JIGPATH),
   kw_consult(FILE, SJIGPATH).

% Close the open logicbase corresponding to the pathname specified
auth_close(FILE) :-
   sch_get_classes(CLASSES),
   retract_classes(CLASSES).

   retract_classes([]) :- !.
   retract_classes([CLASS | REST]) :-
      abolish(CLASS/3).

% Save the logicbase corresponding to the pathname specified
auth_save(PATH, FILE, UNICODE) :-
   auth_save_as(_, PATH, FILE, UNICODE).

% Save the logicbase in a new full pathname
%   Future references in save and close will use the new pathname
auth_save_as(FILE1, PATH2, FILE2, UNICODE) :-
   atomlist_concat([PATH2, 'kwtemp.kb'], TEMPFILE),
%   delfile(TEMPFILE, _),
   (UNICODE == true ->
      open(TEMPFILE, write, H, [type(wide_text)])
   ;
      open(TEMPFILE, write, H)
   ),

   % Save the jig and version
   knowledgewright_jig(NAME, VERSION),
   writeq(H, knowledgewright_jig(NAME, VERSION)),
   write(H, $.\n\n$),

   % Save the license type
   (register:unl$gri(User, Org, _, Prod, _) ->
      sub_string(Prod, 3, 1, ProdType),
      write_license_type(H, Prod, ProdType)
   ;
      writeq(H, knowledgewright_license(academic_personal, [])),
      write(H, $.\n\n$) ),    

   % Get all the classes and write them out
   sch_get_classes(CLASSES),
   append([folder], CLASSES, ALLCLASSES),
   write_file(H, ALLCLASSES),
   close(H),
   atomlist_concat([PATH2, FILE2], FILENAME),
   string_atom(SFILENAME, FILENAME),
   delfile(SFILENAME, _),
   rename(TEMPFILE, FILENAME, ERR),
   ERR = 0.

   write_license_type(H, Prod, $1$) :-
      writeq(H, knowledgewright_license(academic_personal, [serial = Prod])),
      write(H, $.\n\n$).
   write_license_type(H, Prod, $V$) :-
      writeq(H, knowledgewright_license(evaluation, [serial = Prod])),
      write(H, $.\n\n$).
   write_license_type(H, Prod, $X$) :-
      writeq(H, knowledgewright_license(professional, [serial = Prod])),
      write(H, $.\n\n$).
   write_license_type(H, Prod, $E$) :-
      writeq(H, knowledgewright_license(professional, [serial = Prod])),
      write(H, $.\n\n$).
   write_license_type(H, Prod, $S$) :-
      writeq(H, knowledgewright_license(standard, [serial = Prod])),
      write(H, $.\n\n$).
   write_license_type(H, Prod, Type) :-
      writeq(H, knowledgewright_license(unknown, [type = Type, serial = Prod])),
      write(H, $.\n\n$).

   write_file(H, []) :- !.
   write_file(H, [CLASS | REST]) :-
      write(H, $\n\n% $),
      write(H, CLASS),
      write(H, $\n\n$),
      write(H, $:- indexed $),
      write(H, CLASS),
      write(H, $(1,0,0).\n\n$),
%      write(H, $:- sorted($),
%      write(H, CLASS),
%      write(H, $/3).\n\n$),
      write_objects(H, CLASS),
      write_file(H, REST).

   write_objects(H, CLASS) :-
      TERM =.. [CLASS, NAME, FOLDER_PATH, ATTRLIST],
      call(TERM),
      writeq(H, CLASS), write(H, $($),
      writeq(H, NAME), write(H, $, $),
      writeq(H, FOLDER_PATH), write(H, $, [\n   $),
      (CLASS = folder ->
         FILTLIST = ATTRLIST
      ;
         once sch_get_slots(CLASS, SLOT_LIST),
         once filter_slots(SLOT_LIST, ATTRLIST, FILTLIST)
      ),
      once write_slots(H, FILTLIST),
%      writeq(H, TERM),
      write(H, $]).\n\n$),
      fail.
   write_objects(_, _).

   filter_slots(_, [], []) :- !.
   filter_slots(SLOT_LIST, [SLOT = VALUE | REST], [SLOT = VALUE | REST2]) :-
      member(SLOT, SLOT_LIST),
      filter_slots(SLOT_LIST, REST, REST2).
   filter_slots(SLOT_LIST, [SLOT = VALUE | REST], REST2) :-
      filter_slots(SLOT_LIST, REST, REST2).

   write_slots(H, []) :- !.
   write_slots(H, [SLOT = VALUE | []]) :- 
      writeq(H, SLOT = VALUE), write(H, $\n   $),
      !.
   write_slots(H, [SLOT = VALUE | REST]) :-
      writeq(H, SLOT = VALUE), write(H, $,\n   $),
      write_slots(H, REST).

% Get the names of all objects in a specified folder and specified type
auth_get_ids(FOLDER_PATH, CLASS, LIST) :-
   TERM =.. [CLASS, ID, FOLDER_PATH, ATTRLIST],
   findall(ID, TERM, IDS),
   values_strings(IDS, SIDS),
   sort(SIDS, LIST).

% Get the pathnames of all objects in a particular class
auth_get_paths(FOLDER_PATH, CLASS, SLIST) :-
   findall(PATHNAME, get_pathname(CLASS, FOLDER_PATH, PATHNAME), TLIST),
   values_strings(TLIST, SLIST0),
   sort(SLIST0, SLIST).

   get_pathname(CLASS, /, PATHNAME) :-
      FRAME =.. [CLASS, NAME, /, SLOTLIST],
      call(FRAME),
      PATHNAME = / NAME.
   get_pathname(CLASS, PATH, PATHNAME) :-
      FRAME =.. [CLASS, NAME, PATH, SLOTLIST],
      call(FRAME),
      PATH \= /,
      PATHNAME = PATH / NAME.

% Split a pathname into its path and object name components
auth_split_path(/NAME, /, NAME).
auth_split_path(PATH / NAME, PATH, NAME).

% Create a new object from another object puts it in root
auth_copy_object(FOLDER_PATH, CLASS, NEW_ID, OLD_ID) :-
   % Make sure one of the same name does not exist
   % (even if in a different folder)
   not(auth_get_object_class(_, NEW_ID, _)),

   FRAME =.. [CLASS, OLD_ID, FOLDER_PATH, FILLED_LIST],
   call(FRAME),
   NEWFRAME =.. [CLASS, NEW_ID, /, FILLED_LIST],
   assertz(NEWFRAME).
   
% Create a new object in the specified folder with the specified name and type
auth_new_object(FOLDER_PATH, CLASS, ID) :-
   % Make sure one of the same name does not exist
   % (even if in a different folder)
   not(auth_get_object_class(_, ID, _)),

%   OLDFRAME =.. [CLASS, ID, _, _],
%   (call(OLDFRAME) -> fail ; true),

   % Now build it
   auth_get_schema_slots(CLASS, SLOTLIST),
   build_slots(CLASS, ID, SLOTLIST, FILLED_LIST),
   FRAME =.. [CLASS, ID, FOLDER_PATH, FILLED_LIST],
   assertz(FRAME).

   build_slots(CLASS, ID, [], []) :- !.
   build_slots(CLASS, ID, [SLOT | REST1], [SLOT = VALUE | REST2]) :-
      auth_schema_slot_facet(CLASS, SLOT, default, VALUE), !,
      build_slots(CLASS, ID, REST1, REST2).
   build_slots(CLASS, ID, [SLOT | REST1], REST2) :-
      schema_definition(id, SLOT), !,
      build_slots(CLASS, ID, REST1, REST2).
   build_slots(CLASS, ID, [SLOT | REST1], REST2) :-
      schema_definition(path, SLOT), !,
      build_slots(CLASS, ID, REST1, REST2).
   build_slots(CLASS, ID, [SLOT | REST1], [SLOT = $$ | REST2]) :-
      auth_schema_slot_facet(CLASS, SLOT, type, string), !,
      build_slots(CLASS, ID, REST1, REST2).
   build_slots(CLASS, ID, [SLOT | REST1], [SLOT = '' | REST2]) :-
      auth_schema_slot_facet(CLASS, SLOT, type, atom), !,
      build_slots(CLASS, ID, REST1, REST2).
   build_slots(CLASS, ID, [SLOT | REST1], [SLOT = '' | REST2]) :-
      auth_schema_slot_facet(CLASS, SLOT, type, term), !,
      build_slots(CLASS, ID, REST1, REST2).
   build_slots(CLASS, ID, [SLOT | REST1], [SLOT = 0 | REST2]) :-
      auth_schema_slot_facet(CLASS, SLOT, type, number), !,
      build_slots(CLASS, ID, REST1, REST2).
   % All the remaining types are lists
   build_slots(CLASS, ID, [SLOT | REST1], [SLOT = [[ID]] | REST2]) :-
      auth_schema_slot_facet(CLASS, SLOT, create_column, true), 
      !,
      build_slots(CLASS, ID, REST1, REST2).
   build_slots(CLASS, ID, [SLOT | REST1], [SLOT = [] | REST2]) :-
      build_slots(CLASS, ID, REST1, REST2).

% Rename an object
auth_rename_object(PATH, CLASS, OLDID, NEWID) :-
   % See if the frame already exists
   FRAME =.. [CLASS, NEWID, PATH, _],
   call(FRAME),
   !, fail.
auth_rename_object(PATH, CLASS, OLDID, NEWID) :-
   FRAME =.. [CLASS, OLDID, PATH, SLOTLIST],
   retract(FRAME),
   NEWFRAME =.. [CLASS, NEWID, PATH, SLOTLIST],
   assert(NEWFRAME).

% Delete an object
auth_delete_object(PATH, CLASS, ID) :-
   FRAME =.. [CLASS, ID, PATH, _],
   retract(FRAME).

% Move an object to a new folder
auth_move_object(OLDPATH, CLASS, ID, NEWPATH) :-
   FRAME =.. [CLASS, ID, OLDPATH, SLOTLIST],
   retract(FRAME),
   NEWFRAME =.. [CLASS, ID, NEWPATH, SLOTLIST],
   assert(NEWFRAME).

% Get the type for the specified object name and path
auth_get_object_class(FOLDER_PATH, ID, CLASS) :-
   sch_get_classes(CLASSES),
   append([folder], CLASSES, ALLCLASSES),
   find_object(FOLDER_PATH, ID, ALLCLASSES, CLASS).

   find_object(_, _, [], unknown) :- !, fail.
   find_object(FOLDER_PATH, ID, [TYPE | REST], TYPE) :-
      TERM =.. [TYPE, ID, FOLDER_PATH, _],
      call(TERM), !.
   find_object(FOLDER_PATH, ID, [TYPE | REST], CLASS) :-
      find_object(FOLDER_PATH, ID, REST, CLASS).

% Get a slot value for an object
auth_get_slot(FOLDER_PATH, TYPE, ID, SLOT, ID) :-
   schema_definition(id, SLOT), !.
auth_get_slot(FOLDER_PATH, TYPE, ID, SLOT, PATH) :-
   schema_definition(path, SLOT), !,
   CALL =.. [TYPE, ID, PATH, _],
   call(CALL).
auth_get_slot(FOLDER_PATH, TYPE, ID, SLOT, TVALUE) :-
   CALL =.. [TYPE, ID, FOLDER_PATH, SLOTS],
   call(CALL),
   member(SLOT = VALUE, SLOTS),
   translate_slot(TYPE, SLOT, VALUE, TVALUE).
auth_get_slot(FOLDER_PATH, TYPE, ID, SLOT, TVALUE) :-
   CALL =.. [TYPE, ID, FOLDER_PATH, SLOTS],
   call(CALL),
   sch_get_slot_facet(TYPE, SLOT, default, VALUE),
   translate_slot(TYPE, SLOT, VALUE, TVALUE).

   translate_slot(TYPE, SLOT, VALUE, VALUE) :-
      (sch_get_slot_facet(TYPE, SLOT, vars, true) -> fail
      ; true), !.
   translate_slot(TYPE, SLOT, EXP, STRING) :-
      (list(EXP) ; list(STRING)),
      translate_list(EXP, STRING).
   translate_slot(TYPE, SLOT, EXP, STRING) :-
      string_expression(STRING, EXP).

   translate_list([], []) :- !.
   translate_list([H|T], [H2|T2]) :-
      (list(H) ; list(H2)),
      translate_list(H, H2),
      translate_list(T, T2).
   translate_list([H|T], [H2|T2]) :-
      string_expression(H2, H),
      translate_list(T, T2).

% Get a formatted rule slot
auth_get_rule_slot(FOLDER_PATH, TYPE, ID, SLOT, STRINGLIST) :-
   FRAME =.. [TYPE, ID, FOLDER_PATH, SLOTLIST],
   call(FRAME),
   member(SLOT = RULES, SLOTLIST),
   format_rules(RULES, NESTED_STRINGS),
   flatten(NESTED_STRINGS, STRINGLIST).

% Set a slot value (assumes value is valid)
auth_set_slot(FOLDER_PATH, TYPE, ID, SLOT, VALUE) :-
   schema_definition(path, SLOT),
   !,
   TERM =.. [TYPE, ID, FOLDER_PATH, SLOTLIST],
   retract(TERM),
   NEWTERM =.. [TYPE, ID, VALUE, SLOTLIST],
   assert(NEWTERM).
% Replace the slot if it exists
auth_set_slot(FOLDER_PATH, TYPE, ID, SLOT, VALUE) :-
   translate_slot(TYPE, SLOT, EXP, VALUE),
   TERM =.. [TYPE, ID, FOLDER_PATH, SLOTLIST],
   call(TERM),
   replace_elem(SLOT = _, SLOT = EXP, SLOTLIST, NEWSLOTLIST),
   retract(TERM),
   NEWTERM =.. [TYPE, ID, FOLDER_PATH, NEWSLOTLIST],
   assert(NEWTERM), !.
% Otherwise add the slot
auth_set_slot(FOLDER_PATH, TYPE, ID, SLOT, VALUE) :-
   translate_slot(TYPE, SLOT, EXP, VALUE),
   TERM =.. [TYPE, ID, FOLDER_PATH, SLOTLIST],
   call(TERM),
   append([SLOT = EXP], SLOTLIST, NEWSLOTLIST),
   retract(TERM),
   NEWTERM =.. [TYPE, ID, FOLDER_PATH, NEWSLOTLIST],
   assert(NEWTERM), !.

% Get a list of all the object types
auth_get_classes(CLASSES) :- 
   sch_get_classes(CLASSES).

% Get a list of all the slots for an object type
auth_get_schema_slots(CLASS, SLOTS) :-
   sch_get_slots(CLASS, SLOTS).

% Get a property of a slot for an object type
auth_schema_slot_facet(CLASS, SLOT, FACET, X) :-
   sch_get_slot_facet(CLASS, SLOT, FACET, X).

% Get the lists of slots related and unrelated to the specified slot
auth_schema_slot_related(CLASS, SLOT, FACET, VALUE, L1, L2) :-
   sch_get_slot_facet(CLASS, SLOT, FACET, X),
   member(VALUE : L1 : L2, X).
auth_schema_slot_related(_, _, _, _, [], []).

% Get the list of possible list choices by object type
auth_schema_slot_list_choices(CLASS, SLOT, LIST) :-
  sch_get_slot_facet(CLASS, SLOT, listof, LISTOF),
  findall(NAMES, get_list_choices(LISTOF, NAMES), NAMELIST),
  flaten(NAMELIST, LIST).

  get_list_choices([], []) :- !.
  get_list_choices([CLASS | REST], [NAMES | REST2]) :-
     auth_get_ids(_, CLASS, NAMES),
     get_list_choices(REST, REST2).     

% Get a global schema value
auth_schema_global(NAME, VALUE) :-
   schema_global(NAME, VALUE).

% Create a new folder in the specified path with the specified name
auth_folder_new(FOLDER_PATH, FID) :-
   folder(FID, FOLDER_PATH, _),
   !, fail.
auth_folder_new(FOLDER_PATH, FID) :-
   assertz(folder(FID, FOLDER_PATH, [])).

auth_folder_add(FOLDER_PATH, SFID, SID) :-
   id_sid(FID, SFID),
   fold_add(FOLDER_PATH, FID, SID).

auth_folder_delete(PATHNAME) :-
   % Make sure the folder is empty
   once get_object_paths(PATHS),
   once member(PATHNAME, PATHS),
   !, fail.
auth_folder_delete(PATHNAME) :- 
   auth_split_path(PATHNAME, PATH, NAME),
   retract(folder(NAME, PATH, _)).

auth_folder_rename(OLDPATHNAME, NEWNAME) :-
   % Rename the folder
   auth_split_path(OLDPATHNAME, OLDPATH, OLDNAME),
   retract(folder(OLDNAME, OLDPATH, SLOTLIST)),
   assert(folder(NEWNAME, OLDPATH, SLOTLIST)),

   % And move its contents (folders and objects)
   sch_get_classes(CLASSES),
   append([folder], CLASSES, ALLCLASSES),
   auth_split_path(NEWPATHNAME, OLDPATH, NEWNAME),
   move_things(ALLCLASSES, OLDPATHNAME, NEWPATHNAME).
   
auth_folder_move(OLDPATHNAME, NEWPATH) :-
   % Move the folder first
   auth_split_path(OLDPATHNAME, OLDPATH, OLDNAME),
   retract(folder(OLDNAME, OLDPATH, SLOTLIST)),
   assert(folder(OLDNAME, NEWPATH, SLOTLIST)),

   % And its contents (folders and objects)
   sch_get_classes(CLASSES),
   append([folder], CLASSES, ALLCLASSES),
   move_things(ALLCLASSES, OLDPATHNAME, NEWPATH).

   move_things([], _, _) :- !.
   move_things([CLASS | REST], OLDPATH, NEWPATH) :-
      move_full_class(CLASS, OLDPATH, NEWPATH),
      move_things(REST, OLDPATH, NEWPATH).

   move_full_class(CLASS, OLDPATH, NEWPATH) :-
      FRAME =.. [CLASS, NAME, PATH, _],
      findall([CLASS, NAME, PATH], call(FRAME), LIST),
      move_each_item(LIST, OLDPATH, NEWPATH).

   move_each_item([], _, _).
   move_each_item([[CLASS, NAME, PATH]|REST], OLDPATH, NEWPATH) :-
      path_list(PATH, LIST),
      path_list(OLDPATH, OLDLIST),

%      write($Comparing $ : NAME : $ in $ : PATH : $ to $ : OLDPATH), nl,
      compare_ordered_lists(NEWPATH, OLDLIST, LIST, DIFF),

      path_list(NEWPATH, NEWLIST),
      append(NEWLIST, DIFF, NEWFOLDERLIST),
      path_list(NEWFOLDER, NEWFOLDERLIST),

%      write($Moving $ : NAME : $ in $ : PATH : $ to $ : NEWFOLDER), nl,
      FRAME =.. [CLASS, NAME, PATH, SLOTLIST],
      retract(FRAME),
      NEWFRAME =.. [CLASS, NAME, NEWFOLDER, SLOTLIST],
      assert(NEWFRAME),
      !, 
      move_each_item(REST, OLDPATH, NEWPATH).
   move_each_item([[CLASS, NAME, PATH]|REST], OLDPATH, NEWPATH) :-
      move_each_item(REST, OLDPATH, NEWPATH).

   compare_ordered_lists(_, [], REST, REST) :- !.
   compare_ordered_lists(/, [/ | R1], [/ | R2], DIFF) :-
      compare_ordered_lists(/, R1, R2, DIFF).
   compare_ordered_lists(NEWPATH, [F | R1], [F | R2], DIFF) :-
      DIFF = R2,
      compare_ordered_lists(NEWPATH, R1, R2, NEWDIFF).

auth_get_folderless_objects(SIDs) :-
   get_objects(/, IDs),
   ids_sids(IDs, USIDs),
   sort(USIDs, SIDs).

auth_get_root_folders(ROOTS) :-
   auth_get_subfolders(/, UROOTS),
   sort(UROOTS, ROOTS).


% PATH = /a/b SUBFOLDERS = [d, e]
auth_get_subfolders(PATH, SUBFOLDERS) :-
   path_list(PATH, PATHLIST),
   (dirtree(DT) -> true; create_dirtree(DT)),
   tree_get_subs(PATHLIST, DT, USUBFOLDERS),
   sort(USUBFOLDERS, SUBFOLDERS).

auth_get_folder_contents(PATH, CONTENTS) :-
   get_objects(PATH, UCONTENTS),
   sort(UCONTENTS, CONTENTS).

auth_get_folderpaths(PATHS) :-
   get_folderpaths(UPATHS),
   sort(UPATHS, PATHS).

   get_folderpaths(PATHS) :-
      get_paths(PATHLIST),
      (member(/, PATHLIST) -> PATHS = PATHLIST 
      ; append([/], PATHLIST, PATHS) ).


%-------------------------
% rule formatting
%

format_rules([], [$$]) :- !.
format_rules([RULE], [STRING]) :-
   !, format_rule(RULE, STRING).
format_rules([RULE|RULES], [STRING, $,\n\n$|STRINGS]) :-
   format_rule(RULE, STRING),
   format_rules(RULES, STRINGS).

format_rule(if CONDITIONS then ACTIONS,
            [$if\n$,S_COND,$\nthen\n$,S_ACT]) :-
   !,
   format_condition(CONDITIONS, S_COND),
   format_action(ACTIONS, S_ACT).
format_rule(FACT = EXPRESSION, [S_FACT, $ =\n$, S_EXP]) :-
   !,
   string_termq(S_FACT, FACT),
   format_expression(EXPRESSION, S_EXP).
format_rule(R, S_R) :-
   string_termq(S_R, R).

format_condition(C1 and C2, [S_C1, $ and\n$, S_C2]) :-
   !,
   format_condition(C1, S_C1),
   format_condition(C2, S_C2).
format_condition(C, INDENT_S_C) :-
   string_termq(S_C, C),
   strcat($   $, S_C, INDENT_S_C).

format_action(A, [$   $, S_A]) :-
   string_termq(S_A, A).

format_expression(EXP, [$   $, S_EXP]) :-
   string_termq(S_EXP, EXP).

% set utilities

union([], L, L).
union([X|Y], L, U) :-
   member(X, L),
   !, union(Y, L, U).
union([X|Y], L, [X|U]) :-
   union(Y, L, U).

intersection([], _, []).
intersection([X|Y], L, [X|I]) :-
   member(X, L),
   !, intersection(Y, L, I).
intersection([_|Y], L, I) :-
   intersection(Y, L, I).

difference(X, Y, D) :-
   union(X, Y, U),
   intersection(X, Y, I),
   remove_list(I, U, D).

remove_list([], L, L).
remove_list([X|Y], L, L3) :-
   remove(X, L, L2),
   remove_list(Y, L2, L3).


%-------------------------
% lbapi replacements
%

cgiLog(Msg) :-
   assertz(log_message(Msg)).


%-------------------------
% schemas
%

sch_get_classes(CLASSES) :-
   findall(CLASS, schema(CLASS, _), CLASSES).

sch_get_slots(CLASS, SLOTS) :-
   schema(CLASS, PROPS),
   findall(SLOT, member(SLOT:_, PROPS), SLOTS).

sch_get_slot_facet(CLASS, SLOT, FACET, X) :-
   schema(CLASS, AVS),
   sch_get_ap(AVS, SLOT, PROPS),
   member(FACET = X, PROPS).
  
   sch_get_ap([A:P|_], A, P) :- !.
   sch_get_ap([_|Z], A, P) :-
      sch_get_ap(Z, A, P).

% There are some special formatting cases,
% such as adding headers to fixed tables.
% But still, all is passed back and forth as strings.

sch_format_value(CLASS, SLOT, VAL, VALUE) :-
   sch_get_slot_facet(CLASS, SLOT, type, TYPE),
   sch_form(TYPE, CLASS, SLOT, VAL, VALUE).

sch_form(list_object, _, _, nu$ll, []) :- !.
sch_form(table, _, _, nu$ll, []) :- !.
sch_form(fixed_table, CLASS, SLOT, nu$ll, SHEADS) :-
   sch_get_slot_facet(CLASS, SLOT, headings, HEADS),
   string_termq(SHEADS, [HEADS]).
sch_form(_, _, _, nu$ll, $$) :- !.
% if its a fixed table, add/delete headers
sch_form(fixed_table, _, _, VAL, SVAL_HEAD_ROWS) :-
   var(VAL), !,
   string_termq(SVAL_HEAD_ROWS, [HEADS|ROWS]),
   string_termq(VAL, ROWS).
sch_form(fixed_table, CLASS, SLOT, VAL, SVAL_HEAD_ROWS) :-
   !,
   string_termq(VAL, ROWS),
   sch_get_slot_facet(CLASS, SLOT, headings, HEADS),
   string_termq(SVAL_HEAD_ROWS, [HEADS|ROWS]).
% if its a string, remove the outer layer of string
% delimiters
sch_form(string, _, _, VAL, VALUE) :-
   string_termq(VAL, VALUE).
sch_form(_, _, _, V, V) :- !.


%------------------
% folders
%

% Convert between path and list notation

path_list(/, [/]) :-
   !.
path_list(PATH, LIST) :-
   var(LIST), !,
   path_rlist(PATH, [], LIST).
path_list(PATH, [/,A|Z]) :-
   var(PATH), !,
   rlist_path(Z, /A, PATH).

rlist_path([], ACC, ACC).
rlist_path([A|Z], ACC, ANS) :-
   rlist_path(Z, ACC/A, ANS).

% Get a list of all objects in the path

get_objects(PATH, Ns) :-
   sch_get_classes(Cs),
   findall(N, get_object(PATH, Cs, N), Ns).

   get_object(PATH, [C|Cs], N) :-
      FRAME =.. [C, N, PATH, _],
      call(FRAME).
   get_object(PATH, [C|Cs], N) :-
      get_object(PATH, Cs, N).

% Get a list of all folder paths

get_paths(PATHS) :-
   findall(F, get_folder(F), PATHS).

   get_folder(F) :-
      folder(NAME, /, _),
      F = /NAME.
   get_folder(F) :-
      folder(NAME, PATH, _),
      PATH \= /,
      F = PATH / NAME.

% Get a list of all folder paths from the
% objects themselves, not the folder frames

get_object_paths(PATHS) :-
   sch_get_classes(Cs),
   setof(P, get_obj_path(Cs,P), PATHS).

   get_obj_path([C|Cs], P) :-
      FRAME =.. [C, _, P, _],
      call(FRAME).
   get_obj_path([C|Cs], P) :-
      get_obj_path(Cs, P).

% Utilities to build directory trees

create_dirtree(T) :-
   once get_paths(Ps),
%   write(Ps), nl,
   get_rlists(Ps, RLs),
%   write(RLs), nl,
   build_tree(RLs, dir(/,X), T),
%   write(T), nl,
   assert(dirtree(T)).

get_rlists([], []).
get_rlists([P|Ps], [RL|RLs]) :-
   path_rlist(P, [], RL),
   !, get_rlists(Ps, RLs).

path_rlist(X/Y, ACC, ANS) :-
   !,
   path_rlist(X, [Y|ACC], ANS).
path_rlist(/, ACC, [/|ACC]) :-
   !.
path_rlist(/X, ACC, [/,X|ACC]).

build_tree([], D, T) :-
   close_dir(D,T), !.
build_tree([P|Ps], D, T) :-
   add_to_dir(P, D),
   !, build_tree(Ps, D, T).

add_to_dir([A,B|Z], dir(A,X)) :-
   member(dir(B,BX), X),
   !,
   add_to_dir([B|Z], dir(B,BX)).
add_to_dir(_, D).

close_dir(dir(D,X), dir(D,Z)) :-
   close_list(X,Y),
   close_dirs(Y,Z).
   
close_list(X, []) :- var(X).
close_list([A|X], [A|Z]) :- close_list(X,Z).

close_dirs([], []).
close_dirs([D_OPEN|DOs], [D_CLOSED|DCs]) :-
   close_dir(D_OPEN, D_CLOSED),
   close_dirs(DOs, DCs).

tree_get_subs([A], dir(A, DIRS), SUBS) :-
   dirs_list(DIRS, SUBS).
tree_get_subs([A,B|Z], dir(A, DIRS), SUBS) :-
   member(dir(B,SDIRS), DIRS),
   tree_get_subs([B|Z], dir(B,SDIRS), SUBS).
  
dirs_list([], []).
dirs_list([dir(D,_)|X], [D|Y]) :-
   dirs_list(X,Y).

/*   
% get a list of folderless items
% BUG!!! missing rules

fold_erless(FOLDER_PATH, IDs) :-
   findall(ID,
     ( getModule(ID),
       not fold_where(FOLDER_PATH, ID, _) ),
     IDMs),
   findall(ID,
     ( getQuestion(ID),
       not fold_where(FOLDER_PATH, ID, _) ),
     IDQs),
   findall(ID,
     ( getAnswer(ID),
       not fold_where(FOLDER_PATH, ID, _) ),
     IDAs),
   findall(ID,
     ( getNote(ID),
       not fold_where(FOLDER_PATH, ID, _) ),
     IDNs),
   findall(ID,
     ( getHeader(ID),
       not fold_where(FOLDER_PATH, ID, _) ),
     IDHs),
   findall(ID,
     ( getFooter(ID),
       not fold_where(FOLDER_PATH, ID, _) ),
     IDFs),
   append(IDMs, IDQs, IDL1),
   append(IDAs, IDNs, IDL2),
   append(IDHs, IDFs, IDL3),
   append(IDL1, IDL2, IDL4),
   append(IDL4, IDL3, IDs).

% find the folder a given item is in,
% backtrackable when item in multiple folders.
% note that the contents must be converted
% back to real IDs for finding IDs.  Note,
% input ids can be either strings or atoms here.

fold_where(FOLDER_PATH, ID, FID) :-
   id_sid(ID, SID),
   folder(FID, FOLDER_PATH, SCONTENTS),
   member(SID, SCONTENTS),
   !.

% add an item to a folder

fold_add(FOLDER_PATH, FID, SID) :-
   retract(folder(FID, FOLDER_PATH, X)),
   assert(folder(FID, FOLDER_PATH, [SID|X])).

% del an item from a folder

fold_delete(FOLDER_PATH, FID, SID) :-
   retract(folder(FID, FOLDER_PATH, X)),
   remove(SID, X, X2),
   assert(folder(FID, FOLDER_PATH, X2)).

% move an item to a folder from a folder

fold_move(FOLDER_PATH, FID1, FID2, SID) :-
   fold_del(FOLDER_PATH, FID1, SID),
   fold_add(FOLDER_PATH, FID2, SID).

% get the contents of a folder, the contents
% are strings, must convert to find frames.

fold_contents(FOLDER_PATH, FID, IDs) :-
   folder(FID, FOLDER_PATH, CONTENTS),
   findall(ID,
     ( member(SID, CONTENTS),
       id_sid(ID, SID),
       not folder(ID, FOLDER_PATH, _) ),
     IDs).
   

% get all the root level folders in a list

fold_root_level(FOLDER_PATH, FIDs) :-
   findall(FID,
     ( folder(FID, FOLDER_PATH, _),
       not fold_where(FOLDER_PATH, FID, _) ),
     FIDs).

% get the folders in a folder

fold_folders(FOLDER_PATH, FID, FIDs) :-
   folder(FID, FOLDER_PATH, CONTENTS),
   findall(FID,
     ( id_sid(FID, SFID),
       member(SFID, CONTENTS),
       folder(FID, FOLDER_PATH, _) ),
     FIDs).

% rename a frame that might be in folders

fold_rename_contents(FOLDER_PATH, OLDID, NEWID) :-
   findall(FID, (folder(FID, FOLDER_PATH, CONTENTS), member(OLDID, CONTENTS)), FLIST),
   fold_rename_fr(FLIST, FOLDER_PATH, OLDID, NEWID).

   fold_rename_fr([], _, _, _).
   fold_rename_fr([FID|REST], FOLDER_PATH, OLDID, NEWID) :-
      retract(folder(FID, FOLDER_PATH, OLD_CONTENTS)),
      remove(OLDID, OLD_CONTENTS, CONTENTS),
      asserta(folder(FID, FOLDER_PATH, [NEWID|CONTENTS])),
      !, fold_rename_fr(REST, FOLDER_PATH, OLDID, NEWID).
*/

%-------------------------
% variable replacement
%

string_expression(STRING, text(EXP)) :-
   string(STRING),
   var(EXP), !,
   string_list(STRING, CODES),
   codes_exp(LEXP, CODES, []),
   list_expression(LEXP, EXP).
string_expression(STRING, text(EXP)) :-
   var(STRING), !,
   list_expression(LEXP, EXP),
   codes_exp(LEXP, CODES, []),
   string_list(STRING, CODES).
string_expression(X,X).

% take a text expression of strings and atoms
% separated by + operators, and make it simply
% a list.

list_expression(LEXP, EXP) :-
   var(EXP), !,
   reverse(LEXP, RLEXP),
   listexp(RLEXP, EXP).
list_expression(LEXP, EXP) :-
   var(LEXP),
   listexp(RLEXP, EXP),
   reverse(RLEXP, LEXP).

   listexp([], $$).
   listexp([A], A) :-
      A \= _+_.
   listexp([A|Y], Z+A) :-
      listexp(Y,Z).

codes_exp([]) --> [], !.
codes_exp([A|B]) --> codethg(A), !, codes_exp(B).

codethg(T) --> codeatm(T).
codethg(T) --> codestr(T).

codestr(STR) --> {var(STR)}, codeslist(SLST), {string_list(STR,SLST)}.
codestr(STR) --> {string(STR), string_list(STR,SLST)}, codeslist(SLST).

codeatm(ATM) -->
   {nonvar(ATM), (atom(ATM); structure(ATM)), !, term_codes(ATM, Y)},
   [D1], {delim(D1)},
   codeslist2(Y),
   [D2], {delim(D2)},
   !.
codeatm(ATM) -->
   {var(ATM)},
   [D1], {delim(D1)},
   codeslist2(Y),
   [D2], {delim(D2), term_codes(ATM, Y)},
   !.
codeatm(ATM) -->
   {var(ATM)},
   [D], {delim(D)},
   dcgerr($Unbalanced % delimiter in text--use %% instead for percent sign$).

term_codes(TERM, CODES) :-
   var(TERM),
   !,
   string_list(S, CODES),
   string_to_kwterm(S, TERM).
term_codes(TERM, CODES) :-
   var(CODES),
   !,
   string_term(S, TERM),
   string_list(S, CODES).

string_to_kwterm(S, system(X)) :-
   catch( string_termq(S,T), _, fail ),
   nonvar(T),
   T = system(X),
   !.
string_to_kwterm(S, T) :-
   string_list(S,L),
   atom_codes(T,L).
   
% for use in strings, where %% is a %
codeslist([X|Y]) --> achar(X), codeslist(Y).
codeslist([]) --> [].

% for use inside delimiters
codeslist2([X|Y]) --> [X], { not(delim(X)) }, codeslist2(Y).
codeslist2([]) --> [].

achar(X) --> [X,X], {delim(X)}.
% Following line does not work
achar(X) --> [0'\,X], {delim(X)}.
achar(X) --> [X], {not(delim(X))}.

delim(0'%).

dcgerr(Msg,X,_) :-
  (X = [] -> S = X; string_list(S,X)),
  throw(error(Msg, [text = S])).


%-------------------------
% utilities
%

values_strings([], []) :- !.
values_strings([V|X], [SV|Y]) :-
   !,
   string_termq(SV, V),
   values_strings(X, Y).
values_strings(V, SV) :-
   string_termq(SV, V).

ids_sids([], []).
ids_sids([ID|IDs], [SID|SIDs]) :-
   id_sid(ID, SID),
   ids_sids(IDs, SIDs).

id_sid(ID, SID) :-
   var(ID), string(SID),
   !,
   string_termq(SID, ID).
id_sid(ID, SID) :-
   var(SID), not string(ID),
   !,
   string_termq(SID, ID).
id_sid(ID, SID) :-
   throw(interface_internal_error(bad_id)).
