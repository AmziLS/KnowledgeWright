%
% KnowledgeWright Support Reasoning Engine
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
% $Log: support_reasoner.pro,v $
% Revision 1.23  2002/05/18 23:44:57  mary
% More tweaks to licensing
%
% Revision 1.22  2002/05/04 19:09:23  dennis
% Change initial session id from 0 to kw_init. Fix bug inserting KW ad.
%
% Revision 1.21  2002/03/17 19:52:16  mary
% Lots of small enhancements/bug fixes.
%
% Revision 1.20  2002/02/24 04:34:38  dennis
% Minor 4.1.24 changes.
%
% Revision 1.19  2002/02/22 18:27:48  dennis
% Add data_table and find() function. Rename table to rules_table.
%
% Revision 1.18  2002/02/15 20:02:08  dennis
% Use Unicode files for knowledgebases.
%
% Revision 1.17  2002/01/29 21:56:04  mary
% Add operators for command line build. Other makefile nits.
%
% Revision 1.16  2002/01/26 23:06:57  mary
% Allowed Support jig to backup under CGI.
%
% Revision 1.15  2002/01/16 15:44:53  dennis
% 4.1.21 changes.
%
% Revision 1.14  2001/06/03 16:12:20  mary
% Fixed Basic jig to allow the goal list to be set during KWI initialize.
%
% Revision 1.13  2001/05/10 15:14:52  mary
% Final touches on 4.1.16
%
% Revision 1.12  2001/04/22 10:33:52  mary
% Fixed backslashes in pathnames for Windows 98 again.
%
% Revision 1.11  2001/04/04 13:57:42  mary
% Minor 4.1.12 changes.
%
% Revision 1.10  2001/03/04 21:48:31  mary
% Renamed solution_top/bottom to output_top/bottom.
% Fixed default for question_top.
%
% Revision 1.9  2001/02/27 19:18:34  mary
% Fixed ask-also. Formatted solution with solution_top/bottom.
%
% Revision 1.8  2001/02/12 22:08:46  mary
% Unified all the jigs. Fixed question order for ask also. Moved log file
% to session_directory.
%
% Revision 1.7  2001/02/11 17:45:44  mary
% Added external actions and implemented them.
%
% Revision 1.6  2001/02/09 23:53:06  mary
% Changed prompts to not have vars. Added ask_also support to Basic.
% Fixed support reasoner to not return a blank solution when asking questions.
%
% Revision 1.5  2001/02/06 00:21:46  mary
% For menus, allow display text separate from text value used in rules.
%
% Revision 1.4  2001/01/31 15:36:19  mary
% Merged format_parms with get_text. Format_parms throws an error if called.
%
% Revision 1.3  2001/01/26 17:04:14  mary
% Added xref entries to jig. Took out ?'s from reasoner.
%
% Revision 1.2  2001/01/25 18:12:15  mary
% Added debugging ?'s.
%
% Revision 1.1.1.1  2001/01/22 04:40:29  dennis
% added support jig
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

% Operators

:- include('kw_ops.pro').


system_name($KnowledgeWright Support$).
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
   solutions_initialize(S1>S2).

reasoner_resolve(ID, MORE) :-
   session_restore(ID, S),
   solve(S>Sr),
   reasoner_more(Sr, MORE),
   session_save(Sr),
   !.

reasoner_more(S, no_more) :-
   session_get(solution_found = true, S),
   !.
reasoner_more(S, no_more) :-
   session_get(all_solutions = [], S),
   !.
reasoner_more(_, more).

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
   session_get(all_solutions = SOLUTIONS, S1),
   log_trace(2-0, [$Starting with solution list: $, SOLUTIONS]),
   solutions_find(SOLUTIONS, TF, S1>S2).
%   solutions_find(SOLUTIONS, TF, S1>Sx),
%   format_questions(Sx>S2).

solutions_initialize(S1>S2) :-
   findall(PRIORITY-SOLUTION,
      ( frame_exists(solution(SOLUTION)),
        get_slot(solution(SOLUTION), priority, PRIORITY) ),
      SOLS),
   keysort(SOLS, SSOLS),
   solutions_clean(SSOLS, SOLUTIONS),
   session_set(all_solutions = SOLUTIONS, indent(0), S1>S2).

   solutions_clean([], []) :- !.
   solutions_clean([K-B|X], [B|Y]) :-
      solutions_clean(X,Y).

solutions_find([], false, S>S).
solutions_find([SOLUTION|SOLUTIONS], TF, S1>S2) :-
   solution_find(SOLUTION, TF, S1>S2),
   (TF == true; TF == maybe),
   !.
solutions_find([SOLUTION|SOLUTIONS], TF, S1>S2) :-
   session_del(all_solutions < SOLUTION, indent(1), S1>Sx),
   solutions_find(SOLUTIONS, TF, Sx>S2).

   solution_find(SOLUTION, TF, S1>S2) :-
      log_trace(3-0, [$Looking at solution: $, SOLUTION]),
      solution_match(SOLUTION, OUTPUT, TF, S1>Sx),
      (TF == true ->
         action_tell(user, [goal = solution, text=OUTPUT], indent(1), Sx>Sy),
         session_del(all_solutions < SOLUTION, indent(1), Sy>Sz),
         session_set(solution_found = true, indent(1), Sz>S2)
         ;
%         format_questions(Sx>S2) 
         % The actions are added onto the front, reverse them so the
         % ask also's appear after the question.
         session_get(actions = ACTIONS, Sx),
         reverse(ACTIONS, RACTIONS),
         session_set(actions = RACTIONS, Sx>Sy),
         format_questions(Sy>S2)
      ).

   solution_match(SOLUTION, OUTPUT, TF, S1>S2) :-
      solution_environment(SOLUTION, TF1, S1>Sx),

      % Only need to check maybe as solution_environment fails on false
      (TF1 == maybe ->
         OUTPUT = $$,
         TF = TF1,
         S2 = Sx
         ;
         solution_constraints(SOLUTION, TF2, Sx>Sy),

         % Only need to check maybe as solution_constraints fails on false
         (TF2 == maybe ->
            OUTPUT = $$,
            TF = TF2,
            S2 = Sy
            ;
            solution_output(SOLUTION, OUTPUT, TF, Sy>S2),

            % Only need to check maybe as solution_output fails on false
            (TF == maybe ->
               OUTPUT = $$
            ;
               true
            )
         )
      ).

   solution_environment(SOLUTION, TF, S1>S2) :-
      get_slot(solution(SOLUTION), environment, CONDS),
      log_trace(2-1, [$matching solution environment: $, CONDS]),
      conditions_prove(CONDS, TF, indent(2), S1>S2),
      TF \= false.

   solution_constraints(SOLUTION, TF, S1>S2) :-
      get_slot(solution(SOLUTION), conditions, CONDS),
      log_trace(2-1, [$matching solution conditions: $, CONDS]),
      conditions_prove(CONDS, TF, indent(2), S1>S2),
      TF \= false.

   solution_output(SOLUTION, OUTPUT, TF, S1>S2) :-
      get_slot(solution(SOLUTION),problem,PTEXT1),
      text_get(PTEXT1, PTEXT, TF1, indent(2), S1>Sx),

      % text_get doesn't fail, must check for false
      ((TF1 == maybe ; TF1 == false)->
         OUTPUT = $$,
         TF = TF1,
         S2 = Sx
         ;
         get_slot(solution(SOLUTION),fix,FTEXT1),
         text_get(FTEXT1, FTEXT, TF2, indent(2), Sx>Sy),

         % text_get doesn't fail, must check for false
         ((TF2 == maybe ; TF2 == false) ->
            OUTPUT = $$,
            TF = TF2,
            S2 = Sy
         ;
            get_slot(solution(SOLUTION),actions,ACTIONS),
            solution_actions(ACTIONS, TF, Sy>S2),
            (TF == maybe ->
               OUTPUT = $$
            ;
               get_slot(knowledgebase(_),value_separator,VSEP),
               stringlist_concat([$Problem: $, PTEXT, VSEP, $Fix: $, FTEXT, VSEP], OUTPUT)
            )
         )
      ),
      TF \= false.

   solution_actions([], true, S1>S1) :- !.
   solution_actions([ACTION | REST], TF, S1>S2) :-
      get_slot(external_action(ACTION),parameters,PARAMTABLE),

      % xlate_text_table doesn't fail, must check for false
      xlate_text_table(PARAMTABLE, [HEADER | NEWTABLE], TF, S1>Sx),
      ((TF == maybe ; TF == false) ->
         S2 = Sx
      ;
         log_trace(2-1, [$matching solution action: $, ACTION]),
         paramtable2proplist(NEWTABLE, PROPLIST),
         append([goal = ACTION], PROPLIST, PROPLIST2),
         action_tell(external, PROPLIST2, indent(1), Sx>Sy),
         solution_actions(REST, TF, Sy>S2)
      ),
      TF \= false.

   xlate_text_table([], [], true, S1>S1) :- !.
   xlate_text_table([ROW | REST], [NEWROW | REST2], TF, S1>S2) :-
      xlate_text_list(ROW, NEWROW, TF1, S1>Sx),
      ((TF1 == maybe ; TF1 == false) ->
         TF = TF1,
         S2 = Sx
      ;
         xlate_text_table(REST, REST2, TF, Sx>S2)
      ).

   xlate_text_list([], [], true, S1>S1) :- !.
   xlate_text_list([ITEM | REST], [NEWITEM | REST2], TF, S1>S2) :-
      text_get(ITEM, NEWITEM, TF1, indent(2), S1>Sx),
      ((TF1 == maybe ; TF1 == false) ->
         TF = TF1,
         S2 = Sx
      ;
         xlate_text_list(REST, REST2, TF, Sx>S2)
      ).

   % Convert a table with 2 columns to list of firstcol = secondcol
   paramtable2proplist([], []) :- !.
   paramtable2proplist([ [PARAMNAME, VALUE] | REST], [PARAMNAME = VALUE | REST2]) :-
      paramtable2proplist(REST, REST2).

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
      action_ask_alsos(ASK_ALSOS, ACTIONS, indent(L), Sx>S2) ).

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


action_tell(TYPE, PROPERTIES, indent(L), S>S) :-
   caller_tell(TYPE, PROPERTIES),
   !,
   log_trace(2-L, [$Told caller to tell $, TYPE, $ directly, with $, PROPERTIES]).
action_tell(TYPE, PROPERTIES, indent(L), S1>S2) :-
   (TYPE == user ->
      format_output(false, PROPERTIES, PROPS2)
   ;  PROPS2 = PROPERTIES ),
   session_add(actions < tell(TYPE, PROPS2), indent(L), S1>S2).
