%
% Fact Finder
%
% Copyright (c) Amzi! inc. 1996-2002
% All Rights Reserved
%

% The fact finder gets values for facts.  It is
% the heart of goal-driven fact finding, and uses
% the various knowledge frames as it needs them.
%
% The fact finder finishes in one of three states,
% being true, maybe, or false.  It always succeeds,
% returning one of these states.
%
% true means it found a value for the fact, and
% false means it wasn't able to find a value.
% maybe means the rules/data/text/whatever it was
% using to find the fact, depended on finding a
% different fact first, and the system needs to
% return to the calling program to gather that
% new information.
%
% The new information required is added to the
% session object, which is passed back up to the
% caller.

%------------------------------------------------
% CVS history
%
% $Log: fact_finder.pro,v $
% Revision 1.5  2002/12/12 20:49:54  dennis
% and not added to rule table conditions
%
% Revision 1.4  2002/12/12 20:42:30  dennis
% allowed or and and in rule tables
%
% Revision 1.3  2002/12/12 20:25:04  dennis
% made multi-value rule tables work
%
% Revision 1.2  2002/12/11 21:43:25  dennis
% Fix include of kw_ops to current directory.
%
% Revision 1.41  2002/12/10 16:45:19  dennis
% Tweaks
%
% Revision 1.40  2002/12/08 21:29:18  dennis
% Added length() function for lists, strings, atoms and terms.
%
% Revision 1.39  2002/04/22 20:41:43  dennis
% Fixed bug comparing a fact = [list].
%
% Revision 1.38  2002/04/16 20:27:13  dennis
% Added advertisement to HTML for personal license.
%
% Revision 1.37  2002/02/24 04:05:17  dennis
% Minor changes.
%
% Revision 1.36  2002/02/22 18:27:48  dennis
% Add data_table and find() function. Rename table to rules_table.
%
% Revision 1.35  2002/02/21 15:48:58  dennis
% added not to data-table processing
%
% Revision 1.34  2002/02/21 04:39:14  dennis
% added data_table query support, used new :- include directive for
% operators in each file
%
% Revision 1.33  2002/02/13 23:33:04  dennis
% Replaced date_time with library. Added date_format to knowledgebase.
%
% Revision 1.32  2002/01/29 21:56:03  mary
% Add operators for command line build. Other makefile nits.
%
% Revision 1.31  2001/08/02 01:00:59  mary
% Final changes for 4.1.18.
%
% Revision 1.30  2001/06/06 13:01:44  dennis
% added escape_condition_prove for customization, and used
% it to add 'preference' handling in joblook
%
% Revision 1.29  2001/06/03 15:56:52  mary
% Fixed contains and contains_nocase.
%
% Revision 1.28  2001/04/20 23:13:17  mary
% Bug fixes for 4.1.14
%
% Revision 1.27  2001/04/12 20:02:40  dennis
% fixed true/default maybe
%
% Revision 1.26  2001/04/06 17:57:25  mary
% Supported file as an option in text objects.
%
% Revision 1.25  2001/04/04 13:57:41  mary
% Minor 4.1.12 changes.
%
% Revision 1.24  2001/03/30 17:01:24  mary
% Added support for number_of_items.
%
% Revision 1.23  2001/03/16 02:49:15  mary
% Fixed bug that returned a variable and true when there was no row to match
% in a table. (Actually failed on backtracking.)
%
% Revision 1.22  2001/03/04 23:17:41  mary
% Fixed SQL types, removed number, boolean, added integer, float.
% Added throw in case these get out of sync again in future.
%
% Revision 1.21  2001/03/04 22:05:38  mary
% Added format_ouput to handle 3 new output fields in knowledgebase objects.
%
% Revision 1.20  2001/03/04 04:03:55  mary
% Added date expression handling.
%
% Revision 1.19  2001/02/15 00:12:29  dennis
% added axrf files
%
% Revision 1.18  2001/02/14 17:57:43  mary
% Changed to simple scrolling so that text will not be garbled (Java bug).
%
% Revision 1.17  2001/01/31 15:36:18  mary
% Merged format_parms with get_text. Format_parms throws an error if called.
%
% Revision 1.16  2001/01/05 18:37:00  mary
% Fixed bug to properly return value of more based on goal list.
% Fixed bug when a multi-valued fact has the value [] to return [].
%
% Revision 1.15  2000/12/15 18:16:24  dennis
% adding minor fixes
%
% Revision 1.14  2000/12/14 22:02:56  dennis
% Changed rule to rule_set in hopefully all the places
%
% Revision 1.13  2000/12/14 21:44:01  dennis
% Fixed minor bug in expressions.
%
% Revision 1.12  2000/12/14 05:27:08  dennis
% Added diagnose samples and early tutorial chapters,
% and fixed some bugs in reasoner, and improved tracing
% output.
%
% Revision 1.11  2000/12/10 15:06:10  dennis
% reversed rule and table rows, value is last column now
%
% Revision 1.10  2000/12/02 06:07:40  dennis
% some performance changes
%
% Revision 1.9  2000/12/02 04:57:27  dennis
% got dates and automatic fact conversion going
%
% Revision 1.8  2000/12/01 05:03:44  dennis
% added hidden fields, also conditions in a list
%
% Revision 1.7  2000/12/01 02:15:10  dennis
% fixed separators, other minutia
%
% Revision 1.6  2000/11/29 22:55:03  dennis
% changed old text to translation, but commented out
% of .jig for now, and made simpler new text object
%
% Revision 1.5  2000/11/29 21:47:21  dennis
% got colors working again
%
% Revision 1.4  2000/11/29 19:09:50  dennis
% Added CVS logging to source files
%
%
%------------------------------------------------

% Operators

:- include('kw_ops.pro').

% Libraries

:- import(aodbc).
:- import(date_time).


%---------------------------
% fact objects
%

% get the value of this fact, checking first if its
% known in the session, then infering if possible,
% finally putting it on the action list.

isfact(F) :-
   atom(F),
   F \= [].

fact_get_value(FACT, VAL, TF, S1>S2) :-
   fact_get_value(FACT, VAL, TF, indent(0), S1>S2).

fact_get_value(FACT, VAL, true, indent(L), I>I) :-
   session_find(known < fact(FACT,VAL), I, true),
   log_trace(3-L, [$Known: $, fact(FACT, VAL)]),
   !.
fact_get_value(FACT, VAL, TF, indent(L), S1>S2) :-
   log_trace(1-L, [$Looking for: $, FACT]),
   fact_get_val(FACT, VAL, TF, indent(L), S1>S2),
   (TF == true ->
      log_trace(1-L, [$Found: $, FACT = VAL])
      ;
      true ).
   
fact_get_val(true, true, true, _, S>S) :-
   !.
fact_get_val([], [], true, _, S>S) :-
   !.
fact_get_val(default, default, true, _, S>S) :-
   !.
fact_get_val(FACT, VAL, TF, indent(L), S1>S2) :-
   get_slot(fact(FACT), value, EXP),
   log_inc_indent(L,LL),
   log_trace(2-LL, [$Using constant fact: $, fact(FACT, EXP)]),
   expression_get_value(EXP, VAL, TF, indent(LL), S1>Sx),
   (TF == true ->
      session_add(known < fact(FACT,VAL), indent(L), Sx>S2)
      ;
      S2 = Sx),
   !.
fact_get_val(FACT, VAL, TF, indent(L), S1>S2) :-
   frame_exists(text(FACT)),
   log_inc_indent(L, LL),
   text_get_value(FACT, VAL, TF, indent(LL), S1>S2),
   TF \= false,
   !.
%fact_get_val(FACT, VAL, TF, indent(L), S1>S2) :-
%   frame_exists(document(FACT)),
%   log_trace(2-L, [$Using: $, document(FACT)]),
%   log_inc_indent(L, LL),
%   document_get_value(FACT, VAL, TF, indent(LL), S1>S2),
%   TF \= false,
%   !.
fact_get_val(FACT, VAL, TF, indent(L), S1>S2) :-
   frame_exists(rule_set(FACT)),
   log_inc_indent(L, LL),
   rule_get_value(FACT, VAL, TF, indent(LL), S1>S2),
   TF \= false,
   !.
fact_get_val(FACT, VAL, TF, indent(L), S1>S2) :-
   frame_exists(rules_table(FACT)),
   log_inc_indent(L, LL),
   table_get_value(FACT, VAL, TF, indent(LL), S1>S2),
   TF \= false,
   !.
fact_get_val(FACT, VAL, TF, indent(L), S1>S2) :-
   frame_exists(sql(FACT)),
   log_trace(2-L, [$Using: $, sql(FACT)]),
   log_inc_indent(L, LL),
   dbq_get_value(FACT, VAL, TF, indent(LL), S1>S2),
   TF \= false,
   !.
fact_get_val(FACT, VAL, TF, indent(L), S1>S2) :-
   frame_exists(tree(FACT)),
   log_trace(2-L, [$Using: $, tree(FACT)]),
   log_inc_indent(L, LL),
   tree_get_value(FACT, VAL, TF, indent(LL), S1>S2),
   TF \= false,
   !.
fact_get_val(FACT, VAL, TF, indent(L), S1>S2) :-
   frame_exists(question(FACT)),
   log_inc_indent(L, LL),
   question_get_value(FACT, VAL, TF, indent(LL), S1>S2),
   TF \= false,
   !.
fact_get_val(find(Q), VAL, TF, indent(L), S1>S2) :-
log_trace(2-L, [$Data Table Query: $, Q]),
   log_inc_indent(L, LL),
   data_table_get_value(Q, VAL, TF, indent(LL), S1>S2),
   TF \= false,
   !.
fact_get_val(FACT, VAL, TF, indent(L), S1>S2) :-
   escape_get_value(FACT, VAL, TF, indent(L), S1>S2),
   !.
fact_get_val(national_language, $unknown$, true, indent(L), S1>S2) :-
   session_add(known < fact(national_language,$unknown$), S1>S2),
   !.
fact_get_val(FACT, _, _, _, I>I) :-
   throw(error($Unable to get value for fact.$, [fact = FACT])).


%-------------------------------------
% question processing
%

/* with ask also:
question_get_value(FACT, VAL, TF, indent(L), S1>S2) :-
   % create ask_also list
   % walk check prompts and questions
   % if ok, create ask_also list
   % walk ask_also list, getting
      or
   % maybe just some clever recursive call to question_get_value
   % for each ask_also?  Just need to make sure not already known or
   % on list.
*/

question_get_value(FACT, VAL, TF, indent(L), S1>S2) :-
   get_slot(question(FACT), prompt, PROMPT),
   log_trace(2-L, [$Trying: $, question(FACT)]),
   text_get(PROMPT, PVAL, TF1, indent(L), S1>Sx),
   (TF1 == maybe ->
      TF = TF1,
      S2 = Sx
      ;
      get_slots(question(FACT), PROPERTIES),
      remove(prompt = _, PROPERTIES, PROP2),
      log_trace(2-L, [$Asking user: $, FACT, $...$]),
      action_ask(user, FACT, VAL, [prompt=PVAL|PROP2], TF, indent(L), Sx>S2) ).

question_answer_type(Q, TYPE, VALUES) :-
   get_slot(question(Q), answer_type, TYPE),
   get_slot(question(Q), question_type, QTYPE),
   !,
   ((QTYPE == menu_multiple_choices ; QTYPE == menu_multiple_choices_display_separate) ->
      VALUES = multiple
      ;
      VALUES = single ).

/*
question_format_answer(Q, V, VF) :-
   question_answer_type(Q, TYPE, _),
   (islist(V) ->
      question_form_items(V, VF, TYPE)
      ;
      question_form_item(TYPE, V, VF) ).
*/

question_format_answer(Q, V, VF) :-
   islist(V),
   !,
   get_slot(question(Q), answer_type, TYPE),
   question_form_items(V, VF, TYPE).
question_format_answer(Q, V, VF) :-
   get_slot(question(Q), answer_type, TYPE),
   question_form_item(TYPE, V, VF),
   log_trace(3-1, [$Converted $, TYPE, $ $, Q, $ fact $, V, $ to $, VF]).


   question_form_items([], [], _).
   question_form_items([V|VS], [VF|VFS], TYPE) :-
      question_form_item(TYPE, V, VF),
      question_form_items(VS, VFS, TYPE).

   question_form_item(text, V, V) :-
      (string(V) -> true ;
      throw(error(invalid_fact_value,
         [message = $Invalid text value: $, text = V]) )
      ), !.
   question_form_item(number, V, VN) :-
      string(V),
      catch(
         ( 
         string_term(V, VN), 
         number(VN)
         ),
         _,     % any error will do
         throw(error(invalid_fact_value, 
            [message = $Invalid numeric value: $, number = V]) )
         ), !.
%   question_form_item(date, Y-M-D, date(Y,M,D)) :-
%      !.
   question_form_item(date, V, VD) :-
      string(V),
      get_slot(knowledgebase(main), date_format, DF),
      (date_string(VD, DF, V) -> true ;
      throw(error(invalid_fact_value,
         [message = $Invalid date value: $, date = V]) )
      ), !.
   question_form_item(time, V, VT) :-
      string(V),
      (string_time(V, VT) -> true ;
      throw(error(invalid_fact_value,
         [message = $Invalid time value: $, time = V]) )
      ), !.
   question_form_item(boolean, V, VB) :-
      string(V),
      ( ( string_atom(V, VB), (VB == true ; VB == false) ) -> true ;
      throw(error(invalid_fact_value,
         [message = $Invalid boolean value: $, boolean = V]) )
      ), !.
   question_form_item(datetime, V, VDT) :-
      string(V),
      string_datetime(V, VDT),		% BUG not defined!
      !.
   question_form_item(_, V, V).


%-----------------------------------
% rule processing
%

rule_get_value(FACT, VAL, TF, indent(L), S1>S2) :-
   get_slot(rule_set(FACT), rules, [_|RULES]),
   get_slot(rule_set(FACT), type, TYPE),
   log_inc_indent(L,LL),
   (TYPE = multiple_values ->
      log_trace(1-L, [$Using: multi-valued $, rule_set(FACT)]),
      rule_infer_list(RULES, VAL, TF, 1, indent(LL), S1>Sx)
      ;
      log_trace(1-L, [$Using: single-valued $, rule_set(FACT)]),
      rule_infer_unique(RULES, VAL, TF, 1, indent(LL), S1>Sx) ),
   (TF == true ->
      session_add(known < fact(FACT,VAL), indent(L), Sx>S2)
      ;
      S2 = Sx).

rule_infer_unique([], _, false, _, _, I>I).
rule_infer_unique([[CONDITIONS, EXP]|REST], VAL, TF, N, indent(L), S1>S2) :-
   !,
   log_trace(3-L, [$Trying rule #$, N, $: $, if CONDITIONS then EXP]),
   conditions_prove(CONDITIONS, TF1, indent(L), S1>Sx),
   (TF1 == false ->
      log_trace(3-L, [$Failing rule #$, N, $: $, if CONDITIONS then EXP]),
      NN is N + 1,
      rule_infer_unique(REST, VAL, TF, NN, indent(L), Sx>S2)
      ;
      (TF1 == true ->
         log_trace(2-L, [$Using rule #$, N, $: $, if CONDITIONS then EXP]),
         expression_get_value(EXP, VAL, TF, indent(L), Sx>S2)
         ;
         TF = TF1,
         S2 = Sx ) ).
rule_infer_unique([RULE|_], _, _, _, _, S>S) :-
   throw(error(invalid_unique_rule, [
      message = $Invalid rule/expression for single_valued fact$,
      rule = RULE])).

rule_infer_list([], [], true, _, _, I>I).
rule_infer_list([[CONDITIONS, EXP]|REST], [VAL|VALS], TF, N, indent(L), S1>S2) :-
   log_trace(3-L, [$Trying rule #$, N, $: $, if CONDITIONS then EXP]),
   conditions_prove(CONDITIONS, TF1, indent(L), S1>Sx),
   TF1 \= false,
   !,
   (TF1 == maybe ->
      TF = TF1,
      S2 = Sx
      ;
      log_trace(2-L, [$Using rule #$, N, $: $, if CONDITIONS then EXP]),
      expression_get_value(EXP, VAL, TF2, indent(L), Sx>Sy),
      (TF2 == maybe ->
         TF = TF2,
         S2 = Sy
         ;
         NN is N + 1,
         rule_infer_list(REST, VALS, TF, NN, indent(L), Sy>S2) ) ).
rule_infer_list([_|REST], VAL, TF, N, indent(L), S1>S2) :-
   NN is N + 1,
   rule_infer_list(REST, VAL, TF, NN, indent(L), S1>S2).

% rule_ifthen.. used for fields that might have a list of rules
% in the if-then syntax.

rule_ifthen_unique([], _, false, _, _, I>I).
rule_ifthen_unique([if CONDITIONS then FACT = EXP|REST], VAL, TF, N, indent(L), S1>S2) :-
   !,
   log_trace(3-L, [$Trying rule #$, N, $: $, if CONDITIONS then EXP]),
   conditions_prove(CONDITIONS, TF1, indent(L), S1>Sx),
   (TF1 == false ->
      log_trace(3-L, [$Failing rule #$, N, $: $, if CONDITIONS then EXP]),
      NN is N + 1,
      rule_ifthen_unique(REST, VAL, TF, NN, indent(L), Sx>S2)
      ;
      (TF1 == true ->
         log_trace(2-L, [$Using rule #$, N, $: $, if CONDITIONS then EXP]),
         expression_get_value(EXP, VAL, TF, indent(L), Sx>S2)
         ;
         TF = TF1,
         S2 = Sx ) ).
rule_ifthen_unique([(FACT=EXP)|REST], VAL, TF, N, indent(L), I1>I2) :-
   !,
   log_trace(3-L, [$using default: $, FACT=EXP]),
   expression_get_value(EXP, VAL, TF, indent(L), I1>I2).
rule_ifthen_unique([RULE|_], _, _, _, _, S>S) :-
   throw(error(invalid_ifthen_rule, [
      message = $Invalid rule/expression for if-then rule$,
      rule = RULE])).

%------------------------------
% rule_table object
%
% the last element of a row is the value
%

table_get_value(FACT, VAL, TF, indent(L), S1>S2) :-
   get_slot(rules_table(FACT), rules, TABLE),
   get_slot(rules_table(FACT), type, TYPE),
   log_trace(2-L, [$Using: $, rules_table(FACT)]),
   log_inc_indent(L,LL),
   (TYPE = multiple_values ->
      log_trace(1-L, [$Using: multi-valued $, rules_table(FACT)]),
      table_infer_list(TABLE, VAL, TF, indent(LL), S1>Sx)
      ;
      log_trace(1-L, [$Using: single-valued $, rules_table(FACT)]),
      table_infer_unique(TABLE, VAL, TF, indent(LL), S1>Sx) ),
   (TF == true ->
      session_add(known < fact(FACT,VAL), indent(L), Sx>S2)
      ;
      S2 = Sx).

table_infer_list(TABLE, VAL, TF, indent(L), S1>S2) :-
   table_heads_rows(TABLE, HEADS, ROWS),
   table_get_heads(HEADS, VALS, TF1, indent(L), S1>Sx),
   (TF1 == true ->
      log_trace(3-L, [$Table heads: $, HEADS, $ and values: $, VALS]),
      table_find_all_rows(HEADS, ROWS, VAL, 1, TF, indent(L), Sx>S2)
      ;
      TF = TF1,
      S2 = Sx ).

table_infer_unique(TABLE, VAL, TF, indent(L), S1>S2) :-
   table_heads_rows(TABLE, HEADS, ROWS),
   table_get_heads(HEADS, VALS, TF1, indent(L), S1>Sx),
   (TF1 == true ->
      log_trace(3-L, [$Table heads: $, HEADS, $ and values: $, VALS])
      ;
      true),
   table_find_row(HEADS, ROWS, VAL, 1, TF1, TF, indent(L), Sx>S2).

   table_heads_rows([HEADSF|ROWS], HEADS, ROWS) :-
      remove_last(HEADSF, _, HEADS).

   table_format_row(ROWV, ROW, VAL) :-
      remove_last(ROWV, VAL, ROW).

   % get the values for all of the heads, rather than one at a time,
   % which is why we continue when we get a maybe.

   table_get_heads([], [], true, _, I>I).
   table_get_heads([HEAD|HEADS], [VAL|VALS], TF, indent(L), S1>S2) :-
      fact_get_value(HEAD, VAL, TF2, indent(L), S1>Sx),
      (TF2 = maybe ->
         TF = maybe,
         table_get_heads(HEADS, VALS, _, indent(L), Sx>S2)
         ;
         table_get_heads(HEADS, VALS, TF, indent(L), Sx>S2) ).

   % because we've got the conditions all built, conditions_prove
   % will only be true/false, so if not true then go on.
   
   table_find_row(_, [], _, _, _, false, _, I>I) :- !.
   table_find_row(HEADS, [ROWVAL|ROWS], VAL, N, true, TF, indent(L), S1>S2) :-
      table_format_row(ROWVAL, ROW, VALX),
      table_build_conditions(HEADS, ROW, CONDITIONS),
      log_trace(3-L, [$Trying row: $, CONDITIONS]),
      conditions_prove(CONDITIONS, TF1, indent(L), S1>Sx),
      !,
      (TF1 = true ->
         log_trace(2-L, [$Using row #$, N, $: $, CONDITIONS]),
         expression_get_value(VALX, VAL, TF, indent(L), Sx>S2)
         ;
         NN is N + 1,
         table_find_row(HEADS, ROWS, VAL, NN, true, TF, indent(L), Sx>S2) ).
   table_find_row(_, _, _, _, TF, TF, _, I>I).

   
   table_find_all_rows(_, [], [], _, true, _, I>I) :- !.
   table_find_all_rows(HEADS, [ROWVAL|ROWS], VALLIST, N, TF, indent(L), S1>S2) :-
      table_format_row(ROWVAL, ROW, VALX),
      table_build_conditions(HEADS, ROW, CONDITIONS),
      log_trace(3-L, [$Trying row: $, CONDITIONS]),
      conditions_prove(CONDITIONS, TF1, indent(L), S1>Sx),
      !,
      (TF1 = true ->
         log_trace(2-L, [$Using row #$, N, $: $, CONDITIONS]),
         VALLIST = [VAL|VALS],
         expression_get_value(VALX, VAL, TF2, indent(L), Sx>S2),
         (TF2 = true ->
            NN is N + 1,
            table_find_all_rows(HEADS, ROWS, VALS, NN, TF, indent(L), Sx>S2)
            ;
            TF = TF2,
            S2 = Sx )
         ;
         NN is N + 1,
         table_find_all_rows(HEADS, ROWS, VALLIST, NN, TF, indent(L), Sx>S2) ).

   table_build_conditions([HEAD], [COL], COND) :-
      !, 
      table_to_condition(HEAD, COL, COND).
   table_build_conditions([HEAD|HEADS], [COL|COLS], COND and CONDS) :-
      table_to_condition(HEAD, COL, COND),
      !, table_build_conditions(HEADS, COLS, CONDS).

   table_to_condition(   _,   *,   true).
   table_to_condition(HEAD, T1 and T2, C1 and C2) :-
      table_to_condition(HEAD, T1, C1),
      table_to_condition(HEAD, T2, C2).
   table_to_condition(HEAD, T1 or T2, C1 or C2) :-
      table_to_condition(HEAD, T1, C1),
      table_to_condition(HEAD, T2, C2).
   table_to_condition(HEAD, not T, not C) :-
      table_to_condition(HEAD, T, C).
   table_to_condition(HEAD, < VAL, HEAD < VAL).
   table_to_condition(HEAD, <= VAL, HEAD <= VAL).
   table_to_condition(HEAD, > VAL, HEAD > VAL).
   table_to_condition(HEAD, >= VAL, HEAD >= VAL).
   table_to_condition(HEAD, V1 - V2, HEAD >= V1 and HEAD <= V2).
   table_to_condition(HEAD, include VAL, HEAD include VAL).
   table_to_condition(HEAD, exclude VAL, HEAD exclude VAL).
   table_to_condition(HEAD, VAL, HEAD = VAL).


%------------------------------
% data_table object
%
% a table viewed as a normal table, and the
% query is a normal db query
%

data_table_get_value(COL from DATA_TABLE where CONDS, VAL, TF, indent(L), S1>S2) :-
   get_slot(data_table(DATA_TABLE), data, TABLE),
   log_trace(2-L, [$data table query: find $, COL from DATA_TABLE where CONDS]),
   data_table_query(COL, VAL, CONDS, TABLE, TF, indent(L), S1>S2).

data_table_query(COL, VAL, CONDS, [HEAD|ROWS], TF, indent(L), S1>S2) :-
   %log_trace(2-L, [`data_table_query`, sp, CONDS]),
   data_table_rows(ROWS, HEAD, COL, VAL, CONDS, TF, indent(L), S1>S2),
   ( TF == true ->
      log_trace(2-L, [`query result`, sp, COL=VAL])
      ;
      true ).

data_table_rows([ROW|ROWS], HEAD, COL, VAL, CONDS, TF, indent(L), S1>S2) :-
   %log_trace(2-L, [`data_table_rows`,CONDS, HEAD, [ROW|ROWS]]),
   data_table_conds(CONDS, HEAD, ROW, TF1, indent(L), S1>Sx),
   (TF1 == true ->
      data_table_col(COL, HEAD, ROW, VAL, TF, indent(L), Sx>S2)
      ;
      TF = TF1,
      S2 = Sx ),
   TF \= false.
data_table_rows([_|ROWS], HEAD, COL, VAL, CONDS, TF, indent(L), S1>S2) :-
   data_table_rows(ROWS, HEAD, COL, VAL, CONDS, TF, indent(L), S1>S2).

data_table_col(COL, [COL|_], [EXP|_], VAL, TF, indent(L), S1>S2) :-
   %log_trace(2-L, [`data_table_col`, sp, col=COL, sp, exp=EXP]),
   expression_get_value(EXP, VAL, TF, indent(L), S1>S2),
   !.
data_table_col(COL, [_|COLS], [_|VALS], VAL, TF, indent(L), S1>S2) :-
   data_table_col(COL, COLS, VALS, VAL, TF, indent(L), S1>S2).

data_table_conds(C1 and C2, HEAD, ROW, TF, indent(L), S1>S2) :-
   !,
   %log_trace(2-L, [`data_table_conds `,C1 and C2, HEAD, ROW]),
   data_table_conds(C1, HEAD, ROW, TFx, indent(L), S1>Sx),
   (TFx \= false ->
      data_table_conds(C2, HEAD, ROW, TF, indent(L), Sx>S2)
      ;
      TF = TFx,
      S2 = Sx).
data_table_conds(C1 or C2, HEAD, ROW, TF, indent(L), S1>S2) :-
   !,
   %log_trace(2-L, [`data_table_conds `,C1 and C2, HEAD, ROW]),
   data_table_conds(C1, HEAD, ROW, TFx, indent(L), S1>Sx),
   (TFx \= false ->
      TF = TFx,
      S2 = Sx
      ;
      data_table_conds(C2, HEAD, ROW, TF, indent(L), Sx>S2) ).
data_table_conds(not C, HEAD, ROW, TF, indent(L), S1>S2) :-
   !,
   data_table_conds(C, HEAD, ROW, TFx, indent(L), S1>S2),
   ( TFx == false ->
      TF = true
      ;
      ( TFx == true ->
         TF = false
         ;
         TF = TFx ) ).
data_table_conds(COND, HEAD, ROW, TF, indent(L), S1>S2) :-
   COND =.. [OP, COL, EXP],
   %log_trace(2-L, [`data_table_conds`, sp, COND, sp, HEAD, ROW]),
   expression_get_value(EXP, VAL, TFx, indent(L), S1>Sx),
   ( TFx == true ->
      data_table_col(COL, HEAD, ROW, V, TFy, indent(L), Sx>S2),
      ( TFy == true ->
          condition_test(OP, V, VAL, TF)
          ;
          TF = TFy )
      ;
      TF = TFx,
      S2 = Sx ).

%--------------------
% condition object
%

% Prove a boolean condition to be either true or
% false, or maybe maybe meaning it needs more
% information to be sure.  But this needs to be
% a full backtracking search type thing.

conditions_prove(C, TF, S1>S2) :-
   conditions_prove(C, TF, indent(0), S1>S2).

conditions_prove(C, TF, indent(L), S1>S2) :-
   log_trace(3-L, [$Checking condition: $, C]),
   condition_prove(C, TF, indent(L), S1>S2),
   (TF == false ->
      log_trace(3-L, [$Failed condition: $, C])
      ;
      (TF == true ->
         log_trace(3-L, [$Success condition: $, C])
         ;
         log_trace(3-L, [$Waiting condition: $, C]) ) ).

condition_prove(not C, TF, indent(L), S1>S2) :-
   !,
   conditions_prove(C, TF1, indent(L), S1>S2),
   (TF1 == maybe ->
      TF = maybe
      ;
      tfm_not(TF1, TF) ).
condition_prove(C1 or C2, TF, indent(L), S1>S2) :-
   !,
   conditions_prove(C1, TF1, indent(L), S1>Sx),
   (TF1 \= false ->
      TF = TF1, S2 = Sx
      ;
      conditions_prove(C2, TF, indent(L), Sx>S2) ).
condition_prove(C1 and C2, TF, indent(L), S1>S2) :-
   !,
   conditions_prove(C1, TF1, indent(L), S1>Sx),
   (TF1 \= true ->
      TF = TF1, S2 = Sx
      ;
      conditions_prove(C2, TF, indent(L), Sx>S2)).
condition_prove([C1|C2], TF, indent(L), S1>S2) :-
   !,
   conditions_prove(C1, TF1, indent(L), S1>Sx),
   (TF1 \= true ->
      TF = TF1, S2 = Sx
      ;
      conditions_prove(C2, TF, indent(L), Sx>S2)).
condition_prove([], true, indent(L), I>I) :-
   !.
condition_prove(true, true, indent(L), I>I) :-
   !.
condition_prove(default, true, indent(L), I>I) :-
   !.
condition_prove(C, TF, indent(L), S1>S2) :-
   escape_condition_prove(C, TF, indent(L), S1>S2),
   !.
condition_prove(C, TF, indent(L), S1>S2) :-
   C =.. [OP, X, Y],
   current_op(_,_,OP),
   !,
   % Get the actual values for both sides of the operator
   condition_values(X, Y, VX, VY, TFXY, indent(L), S1>S2),
   (TFXY = true ->
      condition_test(OP, VX, VY, TF)
      ;
      TF = TFXY).
condition_prove(PROLOG, TF, indent(L), I>I) :-
   (call(PROLOG) -> TF = true; TF = false),
   !.
condition_prove(C, _, _, _) :-
   throw(error(unknown_condition, [condition = C])).

condition_values(X, Y, VX, VY, TF, S1>S2) :-
   condition_values(X, Y, VX, VY, TF, indent(0), S1>S2).

condition_values(X, Y, VX, VY, TF, indent(L), S1>S2) :-
   condition_value(X, VX, TFX, indent(L), S1>Sx),
   !,
   (TFX = true ->
      condition_value(Y, VY, TF, indent(L), Sx>S2)
      ;
      TF = TFX,
      S2 = Sx),
   !.

condition_value(X, VX, TF, S1>S2) :-
   condition_value(X, VX, TF, indent(0), S1>S2).

condition_value(X, VX, TF, indent(L), S1>S2) :-
   expression_get_value(X, VX, TF, indent(L), S1>S2).

condition_test(=, X, Y, TF) :-
   number(X), number(Y), !,
   (X =:= Y -> TF = true; TF = false).
%condition_test(=, X, Y, TF) :-
%   islist(X), !,
%   (member(Y,X) -> TF = true; TF = false).
condition_test(=, X, Y, TF) :-
   (X == Y -> TF = true; TF = false).
condition_test(\=, X, Y, TF) :-
   (X \= Y -> TF = true; TF = false).
condition_test(<, X, Y, TF) :-
   number(X), number(Y), !,
   (X < Y -> TF = true; TF = false).
condition_test(>, X, Y, TF) :-
   number(X), number(Y), !,
   (X > Y -> TF = true; TF = false).
condition_test(=<, X, Y, TF) :-
   number(X), number(Y), !,
   (X =< Y -> TF = true; TF = false).
condition_test(>=, X, Y, TF) :-
   number(X), number(Y), !,
   (X >= Y -> TF = true; TF = false).
condition_test(<, X, Y, TF) :-
   (X @< Y -> TF = true; TF = false).
condition_test(>, X, Y, TF) :-
   (X @> Y -> TF = true; TF = false).
condition_test(=<, X, Y, TF) :-
   (X @=< Y -> TF = true; TF = false).
condition_test(<=, X, Y, TF) :-
   (X @=< Y -> TF = true; TF = false).
condition_test(>=, X, Y, TF) :-
   (X @>= Y -> TF = true; TF = false).

condition_test(include, X, Y, TF) :-
   (member(Y,X) -> TF = true; TF = false).
condition_test(exclude, X, Y, TF) :-
   (member(Y,X) -> TF = false; TF = true).
% replaced by length()
%condition_test(number_of_items, X, Y, TF) :-
%   (length(X,Y) -> TF = true; TF = false).

condition_test(eq, X, Y, TF) :-
   (X = Y -> TF = true; TF = false).
condition_test(ne, X, Y, TF) :-
   (X \= Y -> TF = true; TF = false).
condition_test(lt, X, Y, TF) :-
   number(X), number(Y),
   (X < Y -> TF = true; TF = false).
condition_test(gt, X, Y, TF) :-
   number(X), number(Y),
   (X > Y -> TF = true; TF = false).
condition_test(lte, X, Y, TF) :-
   number(X), number(Y),
   (X =< Y -> TF = true; TF = false).
condition_test(gte, X, Y, TF) :-
   number(X), number(Y),
   (X >= Y -> TF = true; TF = false).
condition_test(lt, X, Y, TF) :-
   (X @< Y -> TF = true; TF = false).
condition_test(gt, X, Y, TF) :-
   (X @> Y -> TF = true; TF = false).
condition_test(lte, X, Y, TF) :-
   (X @=< Y -> TF = true; TF = false).
condition_test(gte, X, Y, TF) :-
   (X @>= Y -> TF = true; TF = false).

condition_test(contains_nocase, X, Y, TF) :-
   string(X), string(Y), !,
   string_atom(X, AX),
   string_atom(Y, AY),
   atom_uplow(AXU, AX),
   atom_uplow(AYU, AY),
   string_atom(SX, AXU),
   string_atom(SY, AYU),
   (sub_string(SX, _, _, SY) -> TF = true; TF = false).
condition_test(contains, X, Y, TF) :-
   string(X), string(Y), !,
   (sub_string(X, _, _, Y) -> TF = true; TF = false).

%----------------------
% expression object
%

expression_get_value(F, VAL, TF, S1>S2) :-
   expression_get_value(F, VAL, TF, indent(0), S1>S2).

%expression_get_value(EXP, VAL, TF, indent(L), S>S) :-
%   log_trace(2-L, [`expression_get_value`, sp, EXP]),
%   fail.

expression_get_value(true, true, true, _, S>S) :-
   !.
expression_get_value(default, default, true, _, S>S) :-
   !.
expression_get_value(VAL, VAL, true, _, S>S) :-
   (string(VAL) ; number(VAL) ; is_list(VAL)),
   !.
% Must be before clause with isfact because constants look like facts
expression_get_value(DATE_CONSTANT, VAL, true, indent(L), S>S) :-
   date_get(DATE_CONSTANT, VAL), 
   !.
expression_get_value(find(Q), VAL, TF, indent(L), S1>S2) :-
   %log_trace(2-L, [expression_get_value(query(Q))]),
   data_table_get_value(Q, VAL, TF, indent(L), S1>S2),
   !.
expression_get_value(F, VAL, TF, indent(L), S1>S2) :-
   nonvar(F),
   isfact(F),
   !,
   fact_get_value(F, VAL, TF, indent(L), S1>S2).
expression_get_value(ST, VAL, TF, indent(L), I>I) :-
   nonvar(ST),
   expression_known_structure(ST, VAL),
   !.
expression_get_value(text(X), VAL, TF, indent(L), S1>S2) :-
   %log_trace(2-L, [`expression_get_value of text`, sp, X]),
   !,
   text_get(X, VAL, TF, indent(L), S1>S2).
expression_get_value(string_to_date(X), VAL, TF, indent(L), S1>S2) :-
   !,
   fact_get_value(X, SX, TF1, indent(L), S1>S2),
   (TF1 = maybe ->
      TF = maybe
      ;
      TF = TF1,
      get_slot(knowledgebase(main), date_format, DF),
      date_string(VAL, DF, SX) ).
expression_get_value(length(X), VAL, TF, indent(L), S1>S2) :-
   !,
   fact_get_value(X, VX, TF1, indent(L), S1>S2),
   (TF1 = maybe ->
      TF = maybe
      ;
      TF = TF1,
      expression_length_func(VX, VAL) ).
expression_get_value(age(BDAY), VAL, TF, indent(L), S1>S2) :-
   !,
   fact_get_value(BDAY, BDATE, TF1, indent(L), S1>S2),
   (TF1 = maybe ->
      TF = maybe
      ;
      TF = TF1,
      date_age(BDATE, VAL) ).
expression_get_value(COL from RECORD, VAL, TF, indent(L), I>I) :-
   !,
   dbs_get_value(RECORD, COL, VAL, TF, indent(L), I).
expression_get_value(integer(EXP), VAL, TF, indent(L), S1>S2) :-
   !,
   expression_get_value(EXP, VALX, TF, indent(L), S1>S2),
   (TF = true ->
      VAL is integer(VALX)
      ;
      true).
expression_get_value(E, VAL, TF, indent(L), S1>S2) :-
   nonvar(E),
   E =.. [OP, A, B],
   !,
   expression_get_value(A, VALA, TFA, indent(L), S1>Sx),
   (TFA == true ->
      expression_get_value(B, VALB, TFB, indent(L), Sx>S2),
      TF = TFB,
      (TFB == true ->
         expression_eval(OP, VALA, VALB, VAL)
         ;
         true)
      ;
      TF = TFA,
      S2 = Sx).
expression_get_value(OK, OK, true, _, S>S) :-
   is_date_expression(OK),
   !.
expression_get_value(EXP, VAL, TF, indent(L), S1>S2) :-
   escape_get_value(EXP, VAL, TF, indent(L), S1>S2),
   !.
expression_get_value(EXP, _, _, _, _) :-
   throw(error(expression_evaluation, [
         message = $unable to get value for expression$,
         expression = EXP]) ).

expression_eval(+, A, B, V) :-
   string(A), string(B),
   !,
   strcat(A, B, V).
expression_eval(+, A, B, V) :-
   number(A), number(B),
   !,
   V is A + B.
expression_eval(+, A, B, V) :-
   is_date_expression(A), is_date_expression(B),
   !,
   date_add(A, B, V).
expression_eval(-, A, B, V) :-
   number(A), number(B),
   !,
   V is A - B.
expression_eval(-, A, B, V) :-
   is_date_expression(A), is_date_expression(B),
   !,
   date_add(A, -B, V).
expression_eval(*, A, B, V) :-
   number(A), number(B),
   !,
   V is A * B.
expression_eval(/, A, B, V) :-
   number(A), number(B),
   !,
   V is A / B.
expression_eval(**, A, B, V) :-
   number(A), number(B),
   !,
   V is A ** B.
expression_eval(OP, A, B, V) :-
   escape_expression_eval(OP, A, B, V),
   !.
expression_eval(OP, A, B, _) :-
   throw(error(bad_operator_arguments, [
      message = $Bad arguments for operator$,
      operator = OP, arg1 = A, arg2 = B]) ).

expression_length_func(ITEM, LEN) :-
   list(ITEM), !,
   length(ITEM, LEN).
expression_length_func(ITEM, LEN) :-
   string(ITEM), !,
   string_length(ITEM, LEN).
expression_length_func(ITEM, LEN) :-
   atom(ITEM), !,
   atom_length(ITEM, LEN).
expression_length_func(ITEM, LEN) :-
   string_term(STR, ITEM),
   string_length(STR, LEN).
   
expression_known_structure(date(YEAR,MON,DAY,HOUR,MIN,SEC), date(YEAR,MON,DAY,HOUR,MIN,SEC)).
expression_known_structure(date(YEAR,MON,DAY), date(YEAR,MON,DAY,0,0,0)).

%---------------------------------------------------
% dbq functions
%

dbq_initialize :-
   get_slot(knowledgebase(main), odbc, DATASOURCE),
   DATASOURCE \= $$,
   !,
   dbq_open(DATASOURCE).
dbq_initialize.

dbq_open(DATASOURCE) :-
   db_open(DATASOURCE),
   log_trace(1-0, [$Opened ODBC data source: $, DATASOURCE]),
   !.
dbq_open(DATASOURCE) :-
   throw(error(unfound_odbc, [
      message = $Failed to open ODBC data source $,
      datasource = DATASOURCE]) ).

dbq_get_value(FACT, VAL, TFM, indent(L), S1>S2) :-
   get_slot(sql(FACT), query, SQL1),
   sql_text_get(SQL1, SQL, TFM1, indent(L), S1>Sx),
   (TFM1 == maybe ->
      S2 = Sx,
      TFM = TFM1
      ;
      get_slot(sql(FACT), type, TYPE),
      get_slot(sql(FACT), length, LEN),
      dbq_typelen(TYPE, LEN, TYPELEN),
      log_trace(2-L, [$Posing SQL query: $, SQL]),
      dbq_queryonce(TYPELEN, SQL, VAL, TFM),
      session_add(known < fact(FACT,VAL), indent(L), Sx>S2) ).
dbq_get_value(FACT, _, _, _, _) :-
   throw(error($Failed db query for fact $, [fact = FACT])).

   dbq_typelen(text, LEN, TL) :-
      string_integer(SL, LEN),
      strcat($s$, SL, TL).
   dbq_typelen(date, LEN, $d$).
   dbq_typelen(integer, LEN, $i$).
   dbq_typelen(float, LEN, $f$).
   dbq_typelen(TYPE, _, _) :-
      throw(error($Unrecognized SQL data type $, [type = TYPE])).

   dbq_queryonce(TYPE, SQL, VAL, true) :-
      db_queryonce(SQL, [], [TYPE], [VAL]).
   dbq_queryonce(TYPE, SQL, _, _) :-
      throw(error($Failed db query $, [sql = SQL, type = TYPE])).

sql_text_get(text(EXP), TEXT, TF, indent(L), S1>S2) :-
   sql_text_get(EXP, TEXT, TF, indent(L), S1>S2),
   !.
sql_text_get(STRING, STRING, true, indent(L), I>I) :-
   string(STRING),
   !.
sql_text_get(+(T1, T2), TEXT, TFM, indent(L), S1>S2) :-
   !,
   sql_text_get(T1, TEXT1, TFM1, indent(L), S1>Sx),
   sql_text_get(T2, TEXT2, TFM2, indent(L), Sx>S2),
   tfm_min(TFM1, TFM2, TFM),
   (TFM = true ->
      strcat(TEXT1, TEXT2, TEXT)
      ;
      true).
sql_text_get(date(Y,M,D), YMD, true, _, I>I) :-
   !,
   text_year(Y, YYYY),
   text_monday(M, MM),
   text_monday(D, DD),
   stringlist_concat([${d '$, YYYY, $-$, MM, $-$, DD, $'}$], YMD).
sql_text_get(FACT, TEXT, TFM, indent(L), S1>S2) :-
   isfact(FACT),
   !,
   fact_get_value(FACT, VAL, TFM1, indent(L), S1>Sx),
   (TFM1 = maybe ->
      TFM = TFM1,
      S2 = Sx
      ;
      sql_text_get(VAL, TEXT, TFM, indent(L), Sx>S2) ).
sql_text_get(BADTEXT, _, _, _, _) :-
   throw(error($Bad SQL text input $, [input = BADTEXT])).


%---------------------------------------------------
% db functions (simulated db)
%

dbs_open(true, S1>S2) :-
   session_get(dbs_file = DB_FILE, S1),
   reconsult(DB_FILE),
   dbs_get_row(header, AVS),
   log_trace(1-0, [$Using DB $, DB_FILE, $ with headers $, AVS]),
   dbs_assert_headers(AVS, S1>S2),
   !.
dbs_open(TF, S1>S2) :-
   get_slot(logic_base(main), dbs_header, HEADS),
   dbs_get_headers(HEADS, AVLIST, TF, S1>S2),
   (TF = true ->
      dbs_create(AVLIST, S2)
      ;
      true).

   dbs_get_headers([], [], true, I>I) :- !.
   dbs_get_headers([Q|Z], [Q=V|AVS], TF, S1>S2) :-
      session_find(known < fact(Q,V), S1, true),
      !, dbs_get_headers(Z, AVS, TF, S1>S2).
   dbs_get_headers([Q|Z], [Q=VAL|AVS], TF, S1>S2) :-
      fact_get_value(Q, VAL, TF1, indent(1), S1>Sx),
      (TF1 = true -> TF = TF2; TF = TF1),
      dbs_get_headers(Z, AVS, TF2, Sx>S2).

   dbs_assert_headers([], I>I).
   dbs_assert_headers([FACT=VAL|FACTS], S1>S2) :-
      session_add(known < fact(FACT, VAL), indent(1), S1 > Sx),
      !, dbs_assert_headers(FACTS, Sx>S2).

dbs_create(AVLIST, I) :-
   session_get(dbs_file = DB_FILE, I),
   assert( db(header, AVLIST) ),
   log_trace(1-0, [$Creating DB $, DB_FILE, $ with headers $, AVLIST]),
   tell(DB_FILE),
   listing(db),
   told.

dbs_get_row(OBJECT, AVS) :-
   db(OBJECT, AVS).

dbs_get_value(OBJECT, ATTR, VAL, true, indent(L), I) :-
   log_trace(2-L, [$Getting $, ATTR, $ from DB record $, OBJECT]),
   db(OBJECT, AVS),
   member(ATTR=VAL, AVS), !.
dbs_get_value(OBJECT, ATTR, VAL, false, indent(L), I).

dbs_exists(OBJECT, true, indent(L), I>I) :-
   dbs_get_row(OBJECT, _), !.
dbs_exists(OBJECT, false, indent(L), I>I).

dbs_update_recommendations(I) :-
   get_slot(logic_base(main), goals, GOALS),
   dbs_recommendation_avs(GOALS, AVS, I),
   asserta(db(recommendation, AVS)),
   session_get(dbs_file = DB_FILE, I),
   tell(DB_FILE),
   listing(db),
   told.
dbs_update_recommendations(_) :-
   throw(error($db update failed for recommendations$, [])).

   dbs_recommendation_avs([], [], _).
   dbs_recommendation_avs([GOAL|GOALS], [GOAL=VAL|VALS], I) :-
      fact_get_value(GOAL, VAL, _, I>_),
      !, dbs_recommendation_avs(GOALS, VALS, I).

%------------------
% document object
%

document_get_value(FACT, VAL, TFM, indent(L), S1>S2) :-
   get_slot(document(FACT), contents, TEXT),
   text_get(TEXT, VAL, TFM, indent(L), S1>Sx),
   (TFM == maybe ->
      S2 = Sx
      ;
      session_add(known < fact(FACT,VAL), indent(L), Sx>S2) ).

%-----------------
% text object
%

text_get_value(FACT, VAL, TFM, indent(L), S1>S2) :-
   get_slot(text(FACT), type, text),
   get_slot(text(FACT), text, TEXT),
   log_trace(2-L, [$Trying: $, text(FACT)]),
   text_get(TEXT, VAL, TFM, indent(L), S1>Sy),
   (TFM == maybe ->
      S2 = Sy
      ;
      session_add(known < fact(FACT,VAL), indent(L), Sy>S2) ).
text_get_value(FACT, VAL, true, indent(L), S1>S2) :-
   get_slot(text(FACT), type, file),
   get_slot(text(FACT), file, FILE),
   text_get_file_pathname(FILE, TEXT_PATH),
   text_file_string(TEXT_PATH, VAL),
   session_add(known < fact(FACT,VAL), indent(L), S1>S2).
text_get_value(FACT, VAL, TFM, indent(L), S1>S1) :-
   throw(error($Unable to open text file or get text$, [fact=FACT])).

text_get_file_pathname(FILE, FILE) :-
   ( sub_string(FILE, _, _, $/$) ; sub_string(FILE, _, _, $\\$) ).
text_get_file_pathname(FILE, TEXT_PATH) :-
   initial_parms(IPARMS),
   get_property(IPARMS, directory, DIR),
   stringlist_concat([DIR, FILE], TEXT_PATH).

text_file_string(FILE, STRING) :-
   text_read_file_strings(FILE),
   findall(S, fstring(S), L),
   stringlist_concat(L, $\n$, STRING),
   !.

text_read_file_strings(FILE) :-
   retractall(fstring(_)),
   fopen(H, FILE, r),
   repeat,
   read_string(H, S),
   (S == end_of_file ->
      true
      ;
      assertz(fstring(S)),
      fail).

text_get(text(EXP), TEXT, TF, indent(L), S1>S2) :-
   %log_trace(2-L, [`text_get`, sp, EXP]),
   !,
   text_get(EXP, TEXT, TF, indent(L), S1>S2).
text_get(TEXT, TEXT, true, indent(L), I>I) :-
   %log_trace(2-L, [`text_get`, sp, is_string, sp, TEXT]),
   string(TEXT),
   %log_trace(2-L, [`text_get`, sp, was_string, sp, TEXT]),
   !.
text_get(+(T1, T2), TEXT, TFM, indent(L), S1>S2) :-
   !,
   text_get(T1, TEXT1, TFM1, indent(L), S1>Sx),
   text_get(T2, TEXT2, TFM2, indent(L), Sx>S2),
   tfm_min(TFM1, TFM2, TFM),
   (TFM = true ->
      strcat(TEXT1, TEXT2, TEXT)
      ;
      true).
text_get(system(PARM), TEXT, true, _, I>I) :-
   !,
   initial_parms(PARMS),
   (get_property(PARMS, PARM, TEXT) ->
      true
      ;
      TEXT = $$).
text_get(date(Y,M,D), YMD, true, _, I>I) :-
   !,
   date_string(date(Y,M,D), YMD).
text_get(FACT, TEXT, TFM, indent(L), S1>S2) :-
   isfact(FACT),
   !,
   fact_get_value(FACT, VAL, TFM1, indent(L), S1>Sx),
   (TFM1 = maybe ->
      TFM = TFM1,
      S2 = Sx
      ;
      text_get(VAL, TEXT, TFM, indent(L), Sx>S2) ).
text_get([EXP], TEXT, TFM, indent(L), S1>S2) :-
   !,
   text_get(EXP, TEXT, TFM, indent(L), S1>S2).
text_get([EXP|REST], TEXT, TFM, indent(L), S1>S2) :-
   !,
   text_get(EXP, T1, TF, indent(L), S1>Sx),
   (TF == maybe ->
      TFM = TF,
      S2 = Sx
      ;
      text_get(REST, T2, TFM, indent(L), Sx>S2),
      (TF2 == maybe ->
         true
         ;
         get_slot(knowledgebase(_), value_separator, SEP),
         stringlist_concat([T1, SEP, T2], TEXT) ) ).
text_get(TERM, TEXT, true, indent(L), I>I) :-
   string_term(TEXT, TERM),
   !.
text_get(BADTEXT, _, _, _, _) :-
   throw(error($Bad text input $, [input = BADTEXT])).

text_year(Y, YYYY) :-
   Y > 1000, !,
   string_term(YYYY, Y).
text_year(Y, YYYY) :-
   Y > 50,
   !,
   YY is Y + 1900,
   string_term(YYYY, YY).
text_year(Y, YYYY) :-
   Y =< 50,
   YY is Y + 2000,
   string_term(YYYY, YY).

text_monday(M, MM) :-
   M >= 10, !,
   string_term(MM, M).
text_monday(M, MM) :-
   string_term(M1, M),
   strcat($0$, M1, MM).

%-------------------------
% translation object
%

trans_get_value(FACT, VAL, TFM, indent(L), S1>S2) :-
   get_slot(translation(FACT), text, TEXT),
   (list(TEXT) ->
      trans_language(TEXT, LANG_TEXT, TFM1, indent(L), S1>Sx),
      (TFM1 == maybe ->
         TFM = TFM1,
         Sy = Sx
         ;
         text_get(LANG_TEXT, VAL, TFM, indent(L), Sx>Sy) )
      ;
      text_get(TEXT, VAL, TFM, indent(L), S1>Sy) ),
   (TFM == maybe ->
      S2 = Sy
      ;
      session_add(known < fact(FACT,VAL), indent(L), Sy>S2) ).

trans_language([_|TABLE], LANG_TEXT, TFM, indent(L), S1>S2) :-
   fact_get_value(national_language, LANG, TFM, indent(L), S1>S2),
   (TFM == true ->
      trans_language_lookup(LANG, TABLE, LANG_TEXT)
      ;
      true ).

trans_language_lookup($unknown$, [[_, LANG_TEXT] | _], LANG_TEXT) :- !.
trans_language_lookup(LANG, TABLE, LANG_TEXT) :-
   reverse(TABLE, RTABLE),
   trans_table(LANG, RTABLE, LANG_TEXT).

trans_table(_, [[_,LANG_TEXT]], LANG_TEXT) :- !.
trans_table(LANG, [[LANG, LANG_TEXT] | _], LANG_TEXT) :- !.
trans_table(LANG, [_|REST], LANG_TEXT) :-
   trans_table(LANG, REST, LANG_TEXT).

%-----------------
% tree & branch objects
%

tree_get_value(FACT, VAL, TF, indent(L), S1>S2) :-
   get_slot(tree(FACT), object, OBJECT),
   fact_get_value(OBJECT, BRANCH_VAL, TF1, S1>Sx),
   (TF1 == true ->
      get_slot(tree(FACT), branch_table, [_|RULES]),
      tree_get_branch(OBJECT, RULES, NEXTOBJ, TF2, 1, indent(0), Sx>Sy),

      % If there's no where to go, that's a problem
      (TF2 == false -> 
         throw(error(no_branch, [message = $No branch to follow in tree or branch object$, 
                                 object = FACT])) 
      ;  true),

      % If there is a next object figure out what kind
      (TF2 == true ->
         % If its a branch, loop
         (frame_exists(branch(NEXTOBJ)) ->
            branch_get_value(NEXTOBJ, VAL, TF, indent(L), Sy>S2)
         ;
            % Otherwise that is the value
            (NEXTOBJ = text(VAL) -> true ; VAL = NEXTOBJ),
            TF = true,
            S2 = Sy
         )
      ;
         TF = TF2,
         S2 = Sy
      )
   ;
      % Else fact_get_value
      TF = TF1,
      S2 = Sx 
   ).

branch_get_value(FACT, VAL, TF, indent(L), S1>S2) :-
   get_slot(branch(FACT), object, OBJECT),
   fact_get_value(OBJECT, BRANCH_VAL, TF1, S1>Sx),
   (TF1 == true ->
      get_slot(branch(FACT), branch_table, [_|RULES]),
      tree_get_branch(OBJECT, RULES, NEXTOBJ, TF2, 1, indent(0), Sx>Sy),

      % If there's no where to go, that's a problem
      (TF2 == false -> 
         throw(error(no_branch, [message = $No branch to follow in tree or branch object$, 
                                 object = FACT])) 
      ;  true),

      % If there is a next object figure out what kind
      (TF2 == true ->
         % If its a branch, loop
         (frame_exists(branch(NEXTOBJ)) ->
            branch_get_value(NEXTOBJ, VAL, TF, indent(L), Sy>S2)
         ;
            % Otherwise that is the value
            (NEXTOBJ = text(VAL) -> true ; VAL = NEXTOBJ),
            TF = true,
            S2 = Sy
         )
      ;
         TF = TF2,
         S2 = Sy
      )
   ;
      % Else fact_get_value
      TF = TF1,
      S2 = Sx 
   ).

   tree_get_branch(_, [], _, false, _, _, I>I).
   tree_get_branch(OBJECT, [[CONDITIONS, EXP]|REST], VAL, TF, N, indent(L), S1>S2) :-
      !,
      table_to_condition(OBJECT, CONDITIONS, CONDS),
      log_trace(3-L, [$Trying rule #$, N, $: $, if CONDS then EXP]),

      conditions_prove(CONDS, TF1, indent(L), S1>Sx),
      (TF1 == false ->
         log_trace(3-L, [$Failing rule #$, N, $: $, if CONDS then EXP]),
         NN is N + 1,
         tree_get_branch(OBJECT, REST, VAL, TF, NN, indent(L), Sx>S2)
         ;
         (TF1 == true ->
            VAL = EXP,
            log_trace(2-L, [$Using condition #$, N, $: $, if CONDS then VAL]),
            TF = TF1,
            S2 = Sx
            ;
            TF = TF1,
            S2 = Sx ) ).
   tree_get_branch(OBJECT, [RULE|_], _, _, _, _, S>S) :-
      throw(error(invalid_branch_table_condition, [
         message = $Invalid condition in branch_table$,
         rule = RULE])).
