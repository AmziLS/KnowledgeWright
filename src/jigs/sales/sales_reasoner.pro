%
% KnowledgeWright Sales Reasoning Engine
%
% Copyright (c) 2000, Amzi! inc.
% All rights reserved
%
% The reasoning engine is called by the logic
% base interface, to perform the various
% services of a consultation, the most important
% one being 'resolve', which starts the main
% solve loop.
%
% The reasoning engine maintains the state of
% a session in the session object that is
% passed as a parameter to the various predicates
% involved in finding a solution.
%
% The engine is designed to be embedded in a
% calling program that carries on a dialog
% with it.  The caller asks for resolution,
% the engine either returns the answer(s) or
% asks the caller to perform actions, such as
% ask the user a question.  The caller asserts
% the new information and then asks for
% resolution again.
%
% Multiple callers can use the same reasoning
% engine, as the information specific to each
% session is independent of other sessions.

% The reasoner is the portion of the system
% that will most likely be modified for different
% implementations.

%------------------------------------------------
% CVS history
%
% $Log: sales_reasoner.pro,v $
% Revision 1.18  2001/05/10 15:14:52  mary
% Final touches on 4.1.16
%
% Revision 1.17  2001/04/22 10:33:52  mary
% Fixed backslashes in pathnames for Windows 98 again.
%
% Revision 1.16  2001/04/04 13:57:42  mary
% Minor 4.1.12 changes.
%
% Revision 1.15  2001/02/12 22:08:46  mary
% Unified all the jigs. Fixed question order for ask also. Moved log file
% to session_directory.
%
% Revision 1.14  2001/01/24 19:36:42  mary
% Fixed default value for basic jig question top + bottom.
% Small edits to sales jig.
%
% Revision 1.12  2001/01/18 22:15:43  mary
% Merged document_format object with knowledgebase object.
%
% Revision 1.11  2001/01/05 20:31:37  dennis
% fixed sales looping problem
%
% Revision 1.10  2001/01/05 20:19:44  mary
% Fixed the fix to return no_more when the goal list is empty.
%
% Revision 1.8  2001/01/05 16:59:59  dennis
% made no_more come out one step sooner from resolver
%
% Revision 1.7  2000/12/07 04:01:00  dennis
% save sessions to disk
%
% Revision 1.6  2000/12/04 18:59:38  dennis
% fixed compare_lists and string_expressions in author
%
% Revision 1.5  2000/12/04 03:11:24  dennis
% added parm checking to question bottoms as well
%
% Revision 1.4  2000/11/29 22:25:06  dennis
% Changed question_document object to document_format object,
% modified slots to reflect questionness of items.
%
% Revision 1.3  2000/11/29 19:09:51  dennis
% Added CVS logging to source files
%
%
%------------------------------------------------


system_name($KnowledgeWright Sales$).
system_version($4-1-16$).

%-----------------------------------------------
% Logic Base Interface (LBI) predicates,
% called from lbi.pro
%
% lb_initialize(+PROPERTY_LIST, -INFO_LIST)
%    initialize the reasoning engine
% lb_create_session(+ID)
%    creates a new session for given ID
% lb_open(+ID, +LB_FILE)
%    open a logic base for an session
% lb_assert(+ID, +NEW_DATA)
%    asserts new data
% lb_resolve(+ID, -ACTIONS)
%    starts reasoning, returns next actions
%    (questions, answers and the like)
% lb_get_parm(+ID, +FRAME, +INSTANCE, +SLOT, -VALUE)
%    used to query slots of logic base frames
% lb_close(+ID)
%    closes an session
% lb_log_session(+ID)
%    logs the status of an session
% lb_get_session(+ID, +SLOT, -VALUE)
%    returns information from an session
%

lb_initialize(ID, INIT_LIST, [system = SNAME, version = SVER]) :-
   assert(initial_parms(INIT_LIST)),
   system_name(SNAME),
   system_version(SVER),
   log_initialize.

lb_open(ID, LB_FILE) :-
   logicbase_open(ID, LB_FILE),
   dbq_initialize.

lb_new_session(ID) :-
   session_new(ID, S1),
   reasoner_initialize(S1>S2),
   session_save(S2).

lb_assert(ID, NEW_DATA) :-
   reasoner_assert_facts(ID, NEW_DATA).

lb_resolve(ID, MORE) :-
   reasoner_resolve(ID, MORE).

lb_get_action(ID, ACTION) :-
   session_restore(ID, S1),
   session_extract_item(actions < ACTION, S1>S2, true),
   !, session_save(S2).
lb_get_action(ID, none).

lb_get_parm(_, webls, system, version, V) :-
   !, webls_version(V).
lb_get_parm(_, webls, system, build, B) :-
   !, build(B).
lb_get_parm(ID, FRAME, INSTANCE, SLOT, VALUE) :-
   FRAME_INSTANCE =.. [FRAME, INSTANCE],
   get_slot(FRAME_INSTANCE, SLOT, VALUE).

lb_close(ID) :-
   session_close(ID).

lb_log_session(ID) :-
   ID \= 0,
   !,
   session_log(ID).
lb_log_session(_).

lb_get_session(ID, IP) :-
   session_package(ID, IP).


%------------------------------------------
% Logicbase file handling
%

logicbase_open(SID, LB_FILE) :-
   initial_parms(IPARMS),
   get_property(IPARMS, directory, DIR),
   stringlist_concat([DIR, LB_FILE], LB_PATH),
   logicbase_consult(LB_PATH).

   logicbase_consult(LB_PATH) :-
      log_trace(0-0, [$Consulting Knowledgebase: $, LB_PATH]),
      set_mode(string_esc, off),
      reconsult(LB_PATH),
      set_mode(string_esc, on),
      !.
   logicbase_consult(LB_FILE) :-
      throw(error(no_logicbase_file,
         [message = $Can't open knowledgebase file: $,
          file = LB_FILE])).

%----------------------------------------
% Reasoner predicates
%

reasoner_initialize(S1>S2) :-
   get_slot(knowledgebase(_), goals, GOALS),
   session_set(goals = GOALS, indent(0), S1>S2),
   !.
reasoner_initialize(S>S) :-
   throw(error(initialization_fault,
      [message = $Unable to initialize reasoner, knowledgebase object problem?$]) ).

reasoner_resolve(ID, MORE) :-
   session_restore(ID, S),
   solve(S>Sr),
   reasoner_more(Sr, MORE),
   session_save(Sr),
   !.

reasoner_more(S, more) :-
   session_get(goals = G, S),
   G \= [],
   !.
reasoner_more(S, no_more).


reasoner_assert_facts(ID, FACT_LIST) :-
   log_trace(2-0, [$Asserting new facts:$]),
   session_restore(ID, S1),
   assert_factlist(FACT_LIST, S1>S2),
   session_save(S2).

   assert_factlist([], S>S).
   assert_factlist([fact(FACT,VAL)|FACTS], S1>S2) :-
      session_add(known < fact(FACT, VAL), indent(1), S1>Sx),
      !, assert_factlist(FACTS, Sx>S2).
   assert_factlist(fact(FACT,VAL), S1>S2) :-
      session_add(known < fact(FACT, VAL), indent(1), S1>S2).


%------------------------------------------
% solver
%

solve(S1>S2) :-
   session_get(goals = GOALS, S1),
   log_trace(2-0, [$Starting with goal list: $, GOALS]),
   solve_goals(GOALS, S1>Sx),
   format_questions(Sx>S2).

   solve_goals([], S>S).
   solve_goals([GOAL|GOALS], S1>S2) :-
      solve_goal(GOAL, TF, S1>Sx),
      (TF == true ->
         solve_goals(GOALS, Sx>S2)
         ;
         S2 = Sx).

   % when the benefits are all done, and return false,
   % then the goal is solved, so the answer is true.

   solve_goal(benefits, TF, S1>S2) :-
      benefits_findall(TF2, S1>Sy),
      (TF2 == false ->
         TF = true,
         session_del(goals < benefits, indent(0), Sy>S2)
         ;
         S2 = Sy,
         TF = maybe),
      !.
   solve_goal(GOAL, TF, S1>S2) :-
      fact_get_value(GOAL, VAL, TF, S1>Sx),
      (TF == true ->
         action_tell(user, [goal=GOAL, text=VAL], indent(0), Sx>Sy),
         session_del(goals < GOAL, indent(0), Sy>S2)
         ;
         format_questions(Sx>S2) ).

benefits_findall(TF, S1>S2) :-
   ( session_get(all_benefits = BENEFITS, S1),
     Sx = S1
     ;
     benefits_list(BENEFITS, S1>Sx) ),
   !,
   log_trace(2-0, [$Starting with benefit list: $, BENEFITS]),
   benefits_find(BENEFITS, TF, Sx>Sy),
   (TF == maybe ->
      format_questions(Sy>S2)
      ;
      S2 = Sy).

benefits_list(BENEFITS, S1>S2) :-
   findall(key(GROUP,COMPONENT)-BENEFIT,
      ( frame_exists(benefit(BENEFIT)),
        get_slot(benefit(BENEFIT), group, GROUP),
        get_slot(benefit(BENEFIT), component, COMPONENT) ),
      BENES1),
   get_slot(knowledgebase(_), group_order, GROUPS),
   get_slot(knowledgebase(_), component_order, COMPONENTS),
   benefits_number_keys(BENES1, BENES, GROUPS, COMPONENTS),
   keysort(BENES, SBENES),
   benefits_clean(SBENES, BENEFITS),
   session_set(all_benefits = BENEFITS, indent(0), S1>S2).

   benefits_number_keys([], [], _, _) :- !.
   benefits_number_keys([key(G,C)-B|Y], [key(GN,CN)-B|Z], GS, CS) :-
      nth_elem(GS, G, GN),
      nth_elem(CS, C, CN),
      !,
      benefits_number_keys(Y, Z, GS, CS).
   benefits_number_keys([key(G,_)-B|_], _, GS, _) :-
      not nth_elem(GS, G, _),
      throw(error(undefined_group, [
         message = $benefit has group not defined in knowledgebase$,
         benefit = B,
         group = G ])).
   benefits_number_keys([key(_,C)-B|_], _, _, CS) :-
      not nth_elem(CS, C, _),
      throw(error(undefined_component, [
         message = $benefit has component not defined in knowledgebase$,
         benefit = B,
         component = C ])).

   benefits_clean([], []) :- !.
   benefits_clean([K-B|X], [B|Y]) :-
      benefits_clean(X,Y).

benefits_find([], false, S>S) :- !.
benefits_find([BENEFIT|BENEFITS], TF, S1>S2) :-
   once benefit_find(BENEFIT, TF, S1>S2),
   (TF == true; TF == maybe),
   !.
benefits_find([BENEFIT|BENEFITS], TF, S1>S2) :-
   session_del(all_benefits < BENEFIT, indent(1), S1>Sx),
   benefits_find(BENEFITS, TF, Sx>S2).

   benefit_find(BENEFIT, TF, S1>S2) :-
      log_trace(3-0, [$Looking at benefit: $, BENEFIT]),
      benefit_match(BENEFIT, OUTPUT, TF, S1>Sx),
      (TF == true ->
         action_tell(user, [goal = benefit, text=OUTPUT], indent(1), Sx>Sy),
         session_del(all_benefits < BENEFIT, indent(1), Sy>S2)
%         ;
%         (TF == maybe ->
%            format_questions(Sx>S2) 
%         ;  true)
         ; S2 = Sx
      ).
   benefit_find(BENEFIT, _, _) :-
      throw(error(benefit_error, [
         message = $error processing benefit$,
         benefit = BENEFIT]) ).

   benefit_match(BENEFIT, OUTPUT, TF, S1>S2) :-
      benefit_constraints(BENEFIT, TF, S1>S2),
      !,
      get_slot(benefit(BENEFIT),benefit,BTEXT1),
      format_add_parms(BTEXT1, BTEXT),
      get_slot(benefit(BENEFIT),feature,FTEXT1),
      format_add_parms(FTEXT1, FTEXT),
      get_slot(knowledgebase(_),value_separator,SEP),
      stringlist_concat([$Benefit: $, BTEXT, SEP, $Feature: $, FTEXT], OUTPUT).
   benefit_match(_, _, false, S>S).

   benefit_constraints(BENEFIT, TF, S1>S2) :-
      get_slot(benefit(BENEFIT), conditions, CONDS),
      log_trace(2-1, [$matching benefit conditions: $, CONDS]),
      conditions_prove(CONDS, TF, indent(2), S1>S2).

%-----------------------------------------
% actions
%

action_ask(TYPE, FACT, VAL, PROPERTIES, true, indent(L), S1>S2) :-
   caller_ask(TYPE, FACT, VAL, PROPERTIES),
   log_trace(2-L, [$Queried caller to ask $, TYPE, $. Learned $, FACT = VAL, nl]),
   !,
   session_add(known < fact(FACT,VAL), indent(L), S1>S2).
action_ask(TYPE, FACT, _, PROPERTIES, maybe, indent(L), S1>S2) :-
   session_add(actions < ask(user,FACT,PROPERTIES), indent(L), S1>Sx),
   get_property(PROPERTIES, ask_also, ASK_ALSOS),
   (ASK_ALSOS = [] ->
      S2 = Sx
      ;
      session_get(actions = ACTIONS, Sx),
      action_ask_alsos(ASK_ALSOS, ACTIONS, Sx>S2) ).

action_ask_alsos([], ACTIONS, S1>S2) :-
   session_set(actions = ACTIONS, S1>S2).
action_ask_alsos([ASK_ALSO|REST], ACTIONS, S1>S2) :-
   ( member(ask(_,ASK_ALSO,_), ACTIONS)
     ;
     session_find(known < fact(ASK_ALSO,_), S1, true) ),
   !, action_ask_alsos(REST, ACTIONS, S1>S2).
action_ask_alsos([ASK_ALSO|REST], ACTIONS, S1>S2) :-
   get_slots(question(ASK_ALSO), PROPERTIES),
   action_ask_alsos(REST, [ask(user,ASK_ALSO,PROPERTIES)|ACTIONS], S1>S2).


action_tell(TYPE, PROPERTIES, indent(L), S>S) :-
   caller_tell(TYPE, PROPERTIES),
   !,
   log_trace(2-L, [$Told caller to tell $, TYPE, $ directly, with $, PROPERTIES]).
action_tell(TYPE, PROPERTIES, indent(L), S1>S2) :-
   session_add(actions < tell(TYPE, PROPERTIES), indent(L), S1>S2).

