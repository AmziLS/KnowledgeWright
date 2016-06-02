%
% KnowledgeWright Ultra Coach Workout Advisor
% Reasoning Engine
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
% $Log: ucwa_reasoner.pro,v $
% Revision 1.8  2002/03/23 16:20:15  mary
% Clean-up to-do list.
%
% Revision 1.6  2002/02/24 04:34:38  dennis
% Minor 4.1.24 changes.
%
% Revision 1.5  2002/02/22 18:27:48  dennis
% Add data_table and find() function. Rename table to rules_table.
%
% Revision 1.4  2002/02/21 04:39:14  dennis
% added data_table query support, used new :- include directive for
% operators in each file
%
% Revision 1.3  2002/02/12 23:04:20  dennis
% Updated to current release.
%
% Revision 1.2  2001/01/05 17:00:00  dennis
% made no_more come out one step sooner from resolver
%
% Revision 1.1.1.1  2000/12/15 18:27:11  dennis
% added Fitcentric's Ultra Coach Workout Advisor jig
%
%
%
%------------------------------------------------

% Operators

:- include('kw_ops.pro').

% Libraries

:- import(list).

system_name($KnowledgeWright Ultra Coach Workout Advisor$).
system_version($4-1-3$).

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

/*lb_get_parm(_, webls, system, version, V) :-
   !, webls_version(V).
lb_get_parm(_, webls, system, build, B) :-
   !, build(B).
*/
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
   logicbase_consult(LB_PATH),

   % Load user's extensions
   stringlist_concat([DIR, $kwextensions.plm$], EXT_PATH),
   (load(EXT_PATH) ; true).

   logicbase_consult(LB_PATH) :-
      log_trace(0-0, [$Consulting Knowledgebase: $, LB_PATH, nl]),
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

/*   solve_goal(GOAL, TF, S1>S2) :-
      fact_get_value(GOAL, VAL, TF, S1>Sx),
      (TF == true ->
         action_tell(user, [goal=GOAL, text=VAL], indent(0), Sx>Sy),
         session_del(goals < GOAL, indent(0), Sy>S2)
         ;
         format_questions(Sx>S2) ).
*/

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

/*
action_tell(TYPE, PROPERTIES, indent(L), S>S) :-
   caller_tell(TYPE, PROPERTIES),
   !,
   log_trace(2-L, [$Told caller to tell $, TYPE, $ directly, with $, PROPERTIES]).
action_tell(TYPE, PROPERTIES, indent(L), S1>S2) :-
   session_add(actions < tell(TYPE, PROPERTIES), indent(L), S1>S2).
*/

%--------------------------------------------------
% Special calculations using factor objects
%

escape_get_value(calculate(GOAL), VAL, TF, indent(L), S1>S2) :-
   !,
   calculate(GOAL, VAL, TF, indent(L), S1>S2).

% calculate(+GOAL, -TFMout, +indent(L), +I1 > - I2)
%   GOAL - the goal/fact being calculated
%   TFM - the true/false/maybe result returned
%   indent(L) - formatting indent for trace
%   I1 - input state of incident
%   I2 - output state of incident

calculate(GOAL, ROUND_VAL, TFM, indent(L), I1>I2) :-
   log_trace(2-L, [$Calculating: $, GOAL]),
   calc_get_factors(GOAL, FACTORS, indent(L), I1>Ix),
   log_trace(2-L, [$Calc factors: $, FACTORS]),
   log_inc_indent(L, LL),
   calc_build_list(GOAL, FACTORS, TFM2, indent(LL), Ix>I2),
   (TFM2 = maybe ->
      TFM = TFM2
      ;
      calc_sum_list(GOAL, VAL, TFM, indent(LL), I2),
      ROUND_VAL is VAL ).
   

calc_get_factors(GOAL, FACTORS, indent(L), I>I) :-
   session_get(factors(GOAL) = FACTORS, I), !.
calc_get_factors(GOAL, FACTORS, indent(L), I1>I2) :-
   findall(FACTOR,
      ( factor(FACTOR, _, AVS), get_property(AVS, goal, GOAL) ),
      FACTORS),
   session_set(factors(GOAL) = FACTORS, indent(L), I1>Ix),
   session_set(weights(GOAL) = [], indent(L), Ix>I2).

calc_build_list(_, [], true, _, I>I).
calc_build_list(GOAL, [FACTOR|FACTORS], TF, indent(L), I1>I2) :-
   log_trace(3-L, [$Trying factor: $, factor(FACTOR)]),
   calc_check_conditions(FACTOR, TF1, indent(L), I1>Ix),
   !,
   (TF1 = maybe ->
      TF = maybe,
      I2 = Ix
      ;
      calc_get_weight(GOAL, FACTOR, TF2, indent(L), Ix>Iy),
      (TF2 = maybe ->
         TF = maybe,
         I2 = Iy
         ;
         log_trace(2-L, [$Using: $, factor(FACTOR)]),
         calc_build_list(GOAL, FACTORS, TF, indent(L), Iy>I2) ) ).
calc_build_list(GOAL, [FACTOR|FACTORS], TF, indent(L), I1>I2) :-
   log_trace(3-L, [$Skipping (doesn't apply) factor: $, FACTOR]),
   session_del(factors(GOAL) < FACTOR, indent(L), I1>Ix),
   calc_build_list(GOAL, FACTORS, TF, indent(L), Ix>I2).

% fail if false, return maybe or true

calc_check_conditions(FACTOR, TF, indent(L), I1>I2) :-
   get_slot(factor(FACTOR), conditions, CONDITIONS),
   conditions_prove(CONDITIONS, TF, indent(L), I1>I2),
   TF \= false.
   
calc_get_weight(GOAL, FACTOR, TF, indent(L), I1>I2) :-
   get_slot(factor(FACTOR), weight, WRULES),
   rule_ifthen_unique(WRULES, WVAL, TF2, 1, indent(L), I1>Ix),
   (TF2 = maybe ->
      TF = TF2,
      I2 = Ix
      ;
      session_add(known < fact(weight(FACTOR), WVAL), indent(L), Ix>Iy),
      calc_apply_table(GOAL, FACTOR, WVAL, TF, indent(L), Iy>I2) ).

calc_apply_table(GOAL, FACTOR, W, TF, L, I1>I2) :-
   get_slot(factor(FACTOR), data, TABLE),
   table_evaluate(TABLE, VAL, TF1, L, I1>Ix),
   (TF1 = maybe ->
      TF = maybe,
      I2 = Ix
      ;
      TF = TF1,
      session_del(factors(GOAL) < FACTOR, L, Ix>Iy),
      session_add(weights(GOAL) < weight(FACTOR, W, VAL), L, Iy>I2) ).

prune_zero_values([], []) :- !.
prune_zero_values([weight(_, _, 0) | Rest], Result) :-
   !,
   prune_zero_values(Rest, Result).
prune_zero_values([X | Rest], [X | Result]) :-
   prune_zero_values(Rest, Result).

% prune any weight(F, W, V) entries that have V == 0
prune_sum_list(distance_adjustment, In, Out) :-
   prune_zero_values(In, Out).
prune_sum_list(intensity_adjustment, In, Out) :-
   prune_zero_values(In, Out).
prune_sum_list(_, X, X).


calc_sum_list(GOAL, VAL, true, indent(L), I) :-
   session_get(weights(GOAL) = WL, I),
   prune_sum_list(GOAL, WL, WLIST),
   log_trace(2-L, [$Summing weights: $, WLIST]),
   calc_sum(WLIST, 0, 0, VAL, I).

   calc_sum([], 0, _, 0, _) :- !.
   calc_sum([], WEIGHTS, VALUES, VAL, _) :-
      VAL is VALUES / WEIGHTS.
   calc_sum([weight(_,W,F*M)|WS], WACC, VACC, VAL, I) :-
      !,
      (isfact(F) ->
         fact_get_value(F, V, TF, I>_),
         (TF \= true ->
            throw(error($Summing, expect value for fact$, [fact = F]))
            ;
            true)
         ;
         V = F),
      W2 is WACC + W * M,
      V2 is VACC + W * M * V,
      calc_sum(WS, W2, V2, VAL, I).
   calc_sum([weight(_,W,V)|WS], WACC, VACC, VAL, I) :-
      W2 is WACC + W,
      V2 is VACC + W * V,
      calc_sum(WS, W2, V2, VAL, I).
