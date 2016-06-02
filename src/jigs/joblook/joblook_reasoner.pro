%
% KnowledgeWright JobLook Reasoning Engine
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
% $Log: joblook_reasoner.pro,v $
% Revision 1.12  2002/10/27 16:29:00  dennis
% Changed preferences to questions. Fixed job tell problem.
%
% Revision 1.11  2002/10/20 14:53:42  dennis
% Updated joblook.
%
% Revision 1.10  2001/08/02 01:00:59  mary
% Final changes for 4.1.18.
%
% Revision 1.9  2001/06/06 13:01:45  dennis
% added escape_condition_prove for customization, and used
% it to add 'preference' handling in joblook
%
% Revision 1.8  2001/04/22 10:33:52  mary
% Fixed backslashes in pathnames for Windows 98 again.
%
% Revision 1.7  2001/04/04 13:57:42  mary
% Minor 4.1.12 changes.
%
% Revision 1.6  2001/02/12 22:08:46  mary
% Unified all the jigs. Fixed question order for ask also. Moved log file
% to session_directory.
%
% Revision 1.5  2001/01/12 20:55:17  mary
% Expire the product in June 2001.
%
% Revision 1.4  2001/01/05 16:59:59  dennis
% made no_more come out one step sooner from resolver
%
% Revision 1.3  2001/01/03 01:40:50  mary
% Split constraints into 4 sets. Added secondary holland traits.
% Added xref entries to schema.
%
% Revision 1.2  2000/12/31 17:21:01  dennis
% checking in whatever latest changes were
%
% Revision 1.1.1.1  2000/12/01 06:35:26  dennis
% added joblook
%
%
%
%------------------------------------------------


system_name($JobLook$).
system_version($4-2-7$).

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
% lb_backup(+ID, +OLD_DATA, -SUCCESS)
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
      log_trace(0-0, [$Consulting KnowledgeBase: $, LB_PATH, nl]),
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
   findall(JOB, frame_exists(job(JOB)), JOBS),
   session_set(alljobs = JOBS, indent(0), S1>S2),
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
   session_get(alljobs = J, S),
   J \= [],
   !.
reasoner_more(S, no_more).

reasoner_assert_facts(ID, FACT_LIST) :-
   log_trace(3-0, [$Asserting new facts:$]),
   session_restore(ID, S1),
   assert_factlist(FACT_LIST, S1>S2),
   session_save(S2).

   assert_factlist([], S>S).
   assert_factlist([fact(FACT,VAL)|FACTS], S1>S2) :-
      session_add(known < fact(FACT, VAL), indent(1), S1>Sx),
      !, assert_factlist(FACTS, Sx>S2).
   assert_factlist(fact(FACT,VAL), S1>S2) :-
      session_add(known < fact(FACT, VAL), indent(1), S1>S2).

% Mary wonders why add_known isn't used above (doesn't seem to work)
%   assert_factlist([fact(FACT,VAL)|FACTS], S>S) :-
%      session_add_known(S, fact(FACT, VAL)),
%      !, assert_factlist(FACTS, S>S).
%   assert_factlist(fact(FACT,VAL), S>S) :-
%      session_add_known(S, fact(FACT, VAL)).

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
   jobs_findall(S1>S2).

%-----------------------------------------
% jobs
%

jobs_findall(S1>S2) :-
   session_get(alljobs = JOBS, S1),
   log_trace(3-0, [$Starting with job list: $, JOBS]),
   fact_get_value(holland_traits, CLIENT_HOLLAND, TF1, indent(1), S1>Sx),

   % Assumes false is never returned
   (TF1 ==  maybe ->
      Sz = Sx
   ;
      fact_get_value(secondary_holland_traits, CLIENT_HOLLAND2, TF2, indent(1), Sx>Sy),
      (TF2 == maybe ->
         Sz = Sy
      ;
         jobs_fix_holland(CLIENT_HOLLAND, CH),
         jobs_fix_holland(CLIENT_HOLLAND2, CH2),
         jobs_find(JOBS, CH, CH2, Sy>Sz)
      )
   ),
   session_get(alljobs = JOBSMATCH, Sz),
   (JOBSMATCH == [] -> 
      S2 = Sz
   ;
      format_questions(Sz>S2)
   ).

   jobs_fix_holland([], []).
   jobs_fix_holland([LETTER|LETTERS], [ATOM|ATOMS]) :-
      string_atom(LETTER,ATOM),
      jobs_fix_holland(LETTERS, ATOMS).

   jobs_find([], _, _, S>S).
   jobs_find([JOB|JOBS], CH, CH2, S1>S2) :-
      job_find(JOB, CH, CH2, TF, S1>S2),
      (TF == true; TF == maybe),
      (TF == true ->
         log_trace(2-0, [$Found job: $, JOB])
         ;
         true),
      !.
   jobs_find([JOB|JOBS], CH, CH2, S1>S2) :-
      session_del(alljobs < JOB, indent(1), S1>Sx),
      jobs_find(JOBS, CH, CH2, Sx>S2).

   job_find(JOB, CH, CH2, TF, S1>S2) :-
      log_trace(3-0, [$Looking at job: $, JOB]),
      job_match(JOB, CH, CH2, TF, S1>Sx),
      (TF == true ->
%         action_tell(user, [goal = job, text=JOB], indent(1), Sx>Sy),
         reasoner_more(Sx, MORE),
         (MORE == more ->
            action_tell(user, true, [goal=job, text=JOB], indent(0), Sx>Sy)
         ;  action_tell(user, false, [goal=job, text=JOB], indent(0), Sx>Sy) 
         ),

         session_del(alljobs < JOB, indent(1), Sy>S2)
         ;
%         format_questions(Sx>S2) 
         S2 = Sx).

   job_match(JOB, CH, CH2, TF, S1>S2) :-
      get_slot(job(JOB), holland_traits, JOB_HOLLAND),
      log_trace(3-1, [$matching job holland traits: $, JOB_HOLLAND]),
      job_holland(CH, JOB_HOLLAND),

      get_slot(job(JOB), secondary_holland_traits, JOB_HOLLAND2),
      log_trace(3-1, [$matching job secondary holland traits: $, JOB_HOLLAND2]),
      job_holland2(CH2, JOB_HOLLAND2),
      
      job_education_constraints(JOB, TFe, S1>Se),
      (TFe == true ->
         job_skills_constraints(JOB, TFs, Se>Ss),
         (TFs == true ->
            job_motivation_constraints(JOB, TFm, Ss>Sm),
            (TFm == true ->
               job_preferences_constraints(JOB, TF, Sm>S2)
            ;
               S2 = Sm,
               TF = TFm)
         ;
            S2 = Ss,
            TF = TFs)
      ;
         S2 = Se,
         TF = TFe),
      !.
   job_match(_, _, _, false, S>S).

   job_holland(_, []) :- !, fail.
   job_holland(CH, [CH|_]) :- !.
   job_holland([A,B], [[B,A]|REST]) :- !.
   job_holland([A,B,C], [[X,Y,Z]|REST]) :-
      ( [X,Y,Z] = [A,C,B];
        [X,Y,Z] = [B,A,C]; [X,Y,Z] = [B,C,A];
        [X,Y,Z] = [C,A,B]; [X,Y,Z] = [C,B,A] ),
      !.
   job_holland(CH, [_|REST]) :-
      job_holland(CH, REST).

   job_holland2([none], _) :- !.
   job_holland2(_, []) :- !, fail.
   job_holland2(CH, [CH|_]) :- !.
   job_holland2([A,B], [[B,A]|REST]) :- !.
   job_holland2([A,B,C], [[X,Y,Z]|REST]) :-
      ( [X,Y,Z] = [A,C,B];
        [X,Y,Z] = [B,A,C]; [X,Y,Z] = [B,C,A];
        [X,Y,Z] = [C,A,B]; [X,Y,Z] = [C,B,A] ),
      !.
   job_holland2(CH, [_|REST]) :-
      job_holland2(CH, REST).

   job_education_constraints(JOB, TF, S1>S2) :-
      get_slot(job(JOB), education, CONSTRAINTS),
      log_trace(3-1, [$matching job education constraints: $, CONSTRAINTS]),
      conditions_prove(CONSTRAINTS, TF, indent(2), S1>S2).

   job_skills_constraints(JOB, TF, S1>S2) :-
      get_slot(job(JOB), skills, CONSTRAINTS),
      log_trace(3-1, [$matching job skills constraints: $, CONSTRAINTS]),
      conditions_prove(CONSTRAINTS, TF, indent(2), S1>S2).

   job_motivation_constraints(JOB, TF, S1>S2) :-
      get_slot(job(JOB), motivation, CONSTRAINTS),
      log_trace(3-1, [$matching job motivation constraints: $, CONSTRAINTS]),
      conditions_prove(CONSTRAINTS, TF, indent(2), S1>S2).

   job_preferences_constraints(JOB, TF, S1>S2) :-
      get_slot(job(JOB), preferences, CONSTRAINTS),
      log_trace(3-1, [$matching job preferences, constraints: $, CONSTRAINTS]),
      conditions_prove(CONSTRAINTS, TF, indent(2), S1>S2).

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


%---------------------------------------------------------
% escapes from normal fact finder predicates
%

% If a condition references a 'preference' then, that preference
% will be asserted by the calling program if it wants it
% considered.  If it hasn't been asserted, then any condition
% referencing it will be true.

escape_condition_prove(C, true, indent(L), S>S) :-
   C =.. [_, FACT, _],
   frame_exists(preference(FACT)),
   not(session_is_known(fact(FACT,_), S)).
