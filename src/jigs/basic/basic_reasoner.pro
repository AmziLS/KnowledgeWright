%
% KnowledgeWright Basic Reasoning Engine
%
% Copyright (c) 2000-2002, Amzi! inc.
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
% $Log: basic_reasoner.pro,v $
% Revision 1.35  2002/12/08 21:29:19  dennis
% Added length() function for lists, strings, atoms and terms.
%
% Revision 1.34  2002/10/20 14:53:42  dennis
% Updated joblook.
%
% Revision 1.33  2002/05/18 23:44:56  mary
% More tweaks to licensing
%
% Revision 1.32  2002/05/04 19:09:23  dennis
% Change initial session id from 0 to kw_init. Fix bug inserting KW ad.
%
% Revision 1.31  2002/03/17 19:52:16  mary
% Lots of small enhancements/bug fixes.
%
% Revision 1.30  2002/02/24 04:34:38  dennis
% Minor 4.1.24 changes.
%
% Revision 1.29  2002/02/22 18:27:48  dennis
% Add data_table and find() function. Rename table to rules_table.
%
% Revision 1.28  2002/02/15 20:02:08  dennis
% Use Unicode files for knowledgebases.
%
% Revision 1.27  2002/01/29 21:56:04  mary
% Add operators for command line build. Other makefile nits.
%
% Revision 1.26  2002/01/26 19:19:56  mary
% For CGI interface, added KWI call to backup inferencing by removing knowns
% and resetting the goal list.
% Removed Unicode file format preference.
%
% Revision 1.25  2002/01/16 15:44:53  dennis
% 4.1.21 changes.
%
% Revision 1.24  2001/12/17 20:02:24  mary
% Fixed bug for resubmitting CGI forms. Other minor fixes.
%
% Revision 1.23  2001/11/30 01:05:14  mary
% Fix bug to include output_top, output_continue and output_bottom.
% Don't delete session file so the 'back' browser button works.
%
% Revision 1.22  2001/06/03 16:12:20  mary
% Fixed Basic jig to allow the goal list to be set during KWI initialize.
%
% Revision 1.21  2001/04/22 10:33:51  mary
% Fixed backslashes in pathnames for Windows 98 again.
%
% Revision 1.20  2001/04/06 17:57:25  mary
% Supported file as an option in text objects.
%
% Revision 1.19  2001/03/30 17:01:24  mary
% Added support for number_of_items.
%
% Revision 1.18  2001/03/16 02:49:15  mary
% Fixed bug that returned a variable and true when there was no row to match
% in a table. (Actually failed on backtracking.)
%
% Revision 1.17  2001/03/08 05:01:06  mary
% Make format more forgiving about question formats. Adopt main to mary.
%
% Revision 1.16  2001/02/27 19:18:07  mary
% Removed ?.
%
% Revision 1.15  2001/02/23 00:50:07  mary
% Allow variables in prompts (again). New method for ask-also processing.
%
% Revision 1.14  2001/02/15 00:12:29  dennis
% added axrf files
%
% Revision 1.13  2001/02/12 22:08:46  mary
% Unified all the jigs. Fixed question order for ask also. Moved log file
% to session_directory.
%
% Revision 1.12  2001/02/10 00:00:32  mary
% Fixed bug in ask_also.
%
% Revision 1.10  2001/01/05 18:37:00  mary
% Fixed bug to properly return value of more based on goal list.
% Fixed bug when a multi-valued fact has the value [] to return [].
%
% Revision 1.9  2001/01/05 16:59:59  dennis
% made no_more come out one step sooner from resolver
%
% Revision 1.8  2000/12/14 05:27:08  dennis
% Added diagnose samples and early tutorial chapters,
% and fixed some bugs in reasoner, and improved tracing
% output.
%
% Revision 1.7  2000/12/10 02:46:22  mary
% Took out hardwired \ appended to end of directory names.
%
% Revision 1.6  2000/12/04 03:11:23  dennis
% added parm checking to question bottoms as well
%
% Revision 1.5  2000/11/29 22:25:06  dennis
% Changed question_document object to document_format object,
% modified slots to reflect questionness of items.
%
% Revision 1.4  2000/11/29 19:09:51  dennis
% Added CVS logging to source files
%
%
%------------------------------------------------

% Operators

:- include('../../jigs_core/kw_ops.pro').

% Libraries

:- import(list).

system_name($KnowledgeWright Basic$).
system_version($4-2-0$).

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
% lb_backup+ID, +OLD_DATA, -SUCCESS)
%    backs up reasoning to given facts undoing their values
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

lb_backup(ID, NEW_DATA, SUCCESS) :-
   reasoner_backup_facts(ID, NEW_DATA, SUCCESS).

lb_resolve(ID, MORE) :-
   reasoner_resolve(ID, MORE).

lb_get_action(ID, ACTION) :-
   session_restore(ID, S1),
   session_extract_item(actions < ACTION, S1>S2, true),
   !, session_save(S2).
lb_get_action(ID, none).

lb_get_parm(ID, FRAME, INSTANCE, SLOT, VALUE) :-
   FRAME_INSTANCE =.. [FRAME, INSTANCE],
   get_slot(FRAME_INSTANCE, SLOT, VALUE).

lb_close(ID) :-
   session_close(ID).

lb_log_session(ID) :-
   ID \= kw_init,
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
   logicbase_consult(LB_PATH),

   % Load user's extensions
   stringlist_concat([DIR, $kwextensions.plm$], EXT_PATH),
   (load(EXT_PATH) ; true).

   logicbase_consult(LB_PATH) :-
      log_trace(3-0, [$Consulting KnowledgeBase: $, LB_PATH, nl]),
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
   initial_parms(IPARMS),
   ( get_property(IPARMS, goals, GOALS) ; get_slot(knowledgebase(main), goals, GOALS) ),
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
reasoner_more(_, no_more).

reasoner_assert_facts(ID, FACT_LIST) :-
   log_trace(3-0, [$Asserting new facts:$]),
   session_restore(ID, S1),
   assert_factlist(FACT_LIST, S1>S2),
   log_trace(3-0, [$----------- pause ------------$,nl]),
   session_save(S2).

   assert_factlist([], S>S).
   assert_factlist([fact(FACT,VAL)|FACTS], S1>S2) :-
      session_add(known < fact(FACT, VAL), indent(1), S1>Sx),
      !, assert_factlist(FACTS, Sx>S2).
   assert_factlist(fact(FACT,VAL), S1>S2) :-
      session_add(known < fact(FACT, VAL), indent(1), S1>S2).

reasoner_backup_facts(ID, fact(NAME, VALUE), RETRACTED) :-
   session_restore(ID, S1),
   session_truncate_knowns(fact(NAME, _), S1>S2, RETRACTED),
   (RETRACTED = true -> 
      log_trace(3-0, [$Backing up reasoning to: $, NAME, $ = $, VALUE, nl]),
      reasoner_initialize(S2>S3)
   ; S3 = S2 ),
   session_save(S3).


%------------------------------------------
% solver
%

solve(S1>S2) :-
   session_get(goals = GOALS, S1),
   log_trace(2-0, [$Solving goal list: $, GOALS, nl]),
   solve_goals(GOALS, S1>S2),
   session_get(goals = Gx, S2),
   (Gx == [] ->
      log_trace(1-0, [$----------- done ------------$,nl])
      ;
      log_trace(1-0, [$----------- pause ------------$,nl]) ).

   solve_goals([], S>S).
   solve_goals([GOAL|GOALS], S1>S2) :-
      solve_goal(GOAL, TF, S1>Sx),
      (TF == true ->
         solve_goals(GOALS, Sx>S2)
         ;
         S2 = Sx).

   solve_goal(GOAL, TF, S1>S2) :-
      fact_get_value(GOAL, VAL, TF, S1>Sx),
      (TF == true ->
         session_del(goals < GOAL, indent(0), Sx>Sy),
         reasoner_more(Sy, MORE),
         string_term(SVAL, VAL),
         (MORE == more ->
            action_tell(user, true, [goal=GOAL, text=SVAL], indent(0), Sy>S2)
         ;  action_tell(user, false, [goal=GOAL, text=SVAL], indent(0), Sy>S2) 
         )
      ;
         % The actions are added onto the front, reverse them so the
         % ask also's appear after the question.
         session_get(actions = ACTIONS, Sx),
         reverse(ACTIONS, RACTIONS),
         session_set(actions = RACTIONS, Sx>Sy),
         format_questions(Sy>S2)
      ).

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
      action_ask_alsos(ASK_ALSOS, ACTIONS, indent(L), Sx>S2)
   ).

action_ask_alsos([], ACTIONS, indent(L), S1>S2) :-
   session_set(actions = ACTIONS, S1>S2).
action_ask_alsos([ASK_ALSO|REST], ACTIONS, indent(L), S1>S2) :-
   ( member(ask(_,ASK_ALSO,_), ACTIONS)
     ;
     session_find(known < fact(ASK_ALSO,_), S1, true) ),
   !, action_ask_alsos(REST, ACTIONS, indent(L), S1>S2).
action_ask_alsos([ASK_ALSO|REST], ACTIONS, indent(L), S1>S2) :-
   once get_slot(question(ASK_ALSO), prompt, PROMPT),
   log_trace(2-L, [$Trying ask-also: $, question(ASK_ALSO)]),
   once text_get(PROMPT, PVAL, TF, indent(L), S1>Sx),

   % Only ask ask-alsos that can be evaluated without further questions
   TF == true,
   get_slots(question(ASK_ALSO), PROPERTIES),
   replace_elem(prompt = _, prompt = PVAL, PROPERTIES, PROPS2),
   log_trace(2-L, [$Ask-also user: $, ASK_ALSO, $...$]),
   action_ask_alsos(REST, [ask(user,ASK_ALSO,PROPS2)|ACTIONS], indent(L), Sx>S2).
action_ask_alsos([ASK_ALSO|REST], ACTIONS, indent(L), S1>S2) :-
   action_ask_alsos(REST, ACTIONS, indent(L), S1>S2).

action_tell(TYPE, _, PROPERTIES, indent(L), S>S) :-
   caller_tell(TYPE, PROPERTIES),
   !,
   log_trace(2-L, [$Told caller to tell $, TYPE, $ directly, with $, PROPERTIES]).
action_tell(TYPE, MORE, PROPERTIES, indent(L), S1>S2) :-
   (TYPE == user ->
      format_output(MORE, PROPERTIES, PROPS2)
   ;  PROPS2 = PROPERTIES ),
   session_add(actions < tell(TYPE, PROPS2), indent(L), S1>S2).

