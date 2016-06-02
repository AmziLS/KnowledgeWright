% Operators

:- include('..\\jigs\\kw_ops.pro').


/* For stand-alone testing:

:- load(list).
:- load(bug).
:- reconsult(kw_ops).

*/
/*
kwc1 :-
   kw_consult_vdiff(
      $\\amzi\\dev\\kw4\\src\\tests\\shapes.kb$,
      $\\amzi\\dev\\kw4\\workshop\\jigs\\$,
      V1, V2),
   write(v1 = V1), nl,
   write(v2 = V2), nl.

kwc2 :-
   kw_consult(
      $\\amzi\\dev\\kw4\\src\\tests\\shapes.kb$,
      $\\amzi\\dev\\kw4\\workshop\\jigs\\$),
   !,
   listing(rule_set).

kwc3 :-
   kw_consult(
      $\\amzi\\dev\\kw4\\src\\tests\\jobsmall9.kb$,
      $\\amzi\\dev\\kw4\\workshop\\jigs\\$).
*/

% This just lets you know the versions for jig and file.
% You can call it first to warn someone you're about
% to convert their file, or just to make sure all the
% ducks are in a row for opening this thing.

kw_consult_vdiff(FILE, JIG_DIRECTORY, FILE_VERSION, JIG_VERSION) :-
   open(FILE, read, H1),
   read(H1, knowledgewright_jig(JIG,FILE_VERSION)),
   stringlist_concat([JIG_DIRECTORY, JIG, $.jig$], JIG_FILE),
   open(JIG_FILE, read, H2),
   read(H2, schema_global(version,JIG_VERSION)),
   close(H1),
   close(H2),
   (FILE_VERSION > JIG_VERSION ->
      throw(error(bad_jig, [
         message = $Jig is older than the knowledgebase--upgrade KnowledgeWright$,
         jig_file = JIG_FILE,
         jig_version = JIG_VERSION,
         file_version = FILE_VERSION]))
   ;
      true).

% This does the actual consult and conversion if necessary.

kw_consult(FILE, JIG_DIRECTORY) :-
   fopen(H, FILE, r),
   read(H, knowledgewright_jig(JIG,FILE_VERSION)),
   stringlist_concat([JIG_DIRECTORY, JIG, $.jig$], JIG_FILE),
   reconsult(JIG_FILE),
   schema_global(version,JIG_VERSION),
   set_mode(string_esc, off),
   kw_jig_consult(H, JIG, FILE_VERSION, JIG_VERSION),
   set_mode(string_esc, on),
   assert(knowledgewright_jig(JIG,JIG_VERSION)),
   fclose(H).
kw_consult(FILE, JIG_DIRECTORY) :-
   throw(error(unable_to_kw_consult, [
      message = $Unable to consult KW file$,
      file = FILE,
      jig_directory = JIG_DIRECTORY])).

kw_jig_consult(H, JIG, V1, V2) :-
   repeat,
   read(H, X1),
   (X1 == 'end_of_file' ->
      true
      ;
      upgrade(JIG,V1,V2, X1,X2),
% writeq('X2' = X2), nl,
      kw$cons(X2) ).
kw_jig_consult(H, _, _, _) :-
   fclose(H).

kw$cons( (:-(sorted(P)) ) :-
   set$sorted(P), !, fail.
kw$cons( (:-(indexed(P)) ) :-
   set$$indexed(P), !, fail.
kw$cons( (:-(X)) ) :-
   call(X), !, fail.
kw$cons(X) :-
% writeq(asserting:X), nl,
   assert(X), !, fail.

upgrade(JIG,V,V,X,X) :- !.
upgrade(JIG,V1,V2,X1,X2) :-
   convert(JIG,V1,Vn,X1,Xn),
   !,
   upgrade(JIG,Vn,V2,Xn,X2).


convert(basic,1,2, rule(I,P,S1), rule(I,P,S2)) :- !, convert_b1_rule(S1,S2).
convert(basic,1,2, table(I,P,S1), table(I,P,S2)) :- !, convert_b1_table(S1,S2).
convert(basic,1,2, X, X) :- !.

convert(basic,2,3, rule(I,P,S), rule_set(I,P,S)) :- !.
convert(basic,2,3, X, X) :- !.

convert(basic,3,4, knowledgebase(I,P,S1), knowledgebase(I,P,S2) :- !, convert_b3_knowledgebase(S1,S2).
convert(basic,3,4, document_format(I,P,S1), unused_document_format(I,P,S2) :- !, convert_b3_document_format(S1,S2).
convert(basic,3,4, X, X) :- !.

convert(basic,4,5, question(I,P,S1), question(I,P,S2)) :- convert_b5_question_prompt(S1,S2).
convert(basic,4,5, X, X) :- !.

convert(basic,5,6, question(I,P,S1), question(I,P,S2)) :- convert_b6_question_prompt(S1,S2).
convert(basic,5,6, X, X) :- !.

convert(basic,6,7, knowledgebase(I,P,S1), knowledgebase(I,P,S2)) :- convert_b7_kb_topbot(S1,S2).
convert(basic,6,7, X, X) :- !.

convert(basic,7,8, knowledgebase(I,P,S1), knowledgebase(I,P,S2)) :- append(S1, [date_format = 'm/d/y'], S2).
convert(basic,7,8, X, X) :- !.

convert(basic,8,9, table(I,P,S), rules_table(I,P,S)) :- !.
convert(basic,8,9, X, X) :- !.

convert(basic,9,10, knowledgebase(I,P,S1), knowledgebase(I,P,S2)) :- append(S1, [charset = $$], S2).
convert(basic,9,10, X, X) :- !.

convert(basic,10,11, question(I,P,S1), question(I,P,S2)) :- append(S1, [answer_type = text], S2).
convert(basic,10,11, X, X) :- !.


convert(support,1,2, question(I,P,S1), question(I,P,S2)) :- convert_b5_question_prompt(S1,S2).
convert(support,1,2, solution(I,P,S1), solution(I,P,S2)) :- append(S1, [actions = []], S2).
convert(support,1,2, X, X) :- !.

convert(support,2,3, question(I,P,S1), question(I,P,S2)) :- convert_b6_question_prompt(S1,S2).
convert(support,2,3, knowledgebase(I,P,S1), knowledgebase(I,P,S2)) :- convert_s3_kb_topbot(S1,S2).
convert(support,2,3, X, X) :- !.

convert(support,3,4, knowledgebase(I,P,S1), knowledgebase(I,P,S2)) :- convert_s4_kb_topbot(S1,S2).
convert(support,3,4, X, X) :- !.

convert(support,4,5, knowledgebase(I,P,S1), knowledgebase(I,P,S2)) :- append(S1, [date_format = 'm/d/y'], S2).
convert(support,4,5, X, X) :- !.

convert(support,5,6, table(I,P,S), rules_table(I,P,S)) :- !.
convert(support,5,6, X, X) :- !.

convert(support,6,7, knowledgebase(I,P,S1), knowledgebase(I,P,S2)) :- append(S1, [charset = $$], S2).
convert(support,6,7, X, X) :- !.

convert(support,7,8, question(I,P,S1), question(I,P,S2)) :- append(S1, [answer_type = text], S2).
convert(support,7,8, X, X) :- !.


convert(sales,1,2, rule(I,P,S1), rule(I,P,S2)) :- !, convert_b1_rule(S1,S2).
convert(sales,1,2, table(I,P,S1), table(I,P,S2)) :- !, convert_b1_table(S1,S2).
convert(sales,1,2, X, X) :- !.

convert(sales,2,3, rule(I,P,S), rule_set(I,P,S)) :- !.
convert(sales,2,3, X, X) :- !.

convert(sales,3,4, knowledgebase(I,P,S1), knowledgebase(I,P,S2) :- !, convert_b3_knowledgebase(S1,S2).
convert(sales,3,4, document_format(I,P,S1), unused_document_format(I,P,S2) :- !, convert_b3_document_format(S1,S2).
convert(sales,3,4, X, X) :- !.

convert(sales,4,5, question(I,P,S1), question(I,P,S2)) :- convert_b5_question_prompt(S1,S2).
convert(sales,4,5, X, X) :- !.

convert(sales,5,6, question(I,P,S1), question(I,P,S2)) :- convert_b6_question_prompt(S1,S2).
convert(sales,5,6, X, X) :- !.

convert(sales,6,7, knowledgebase(I,P,S1), knowledgebase(I,P,S2)) :- convert_s7_kb_topbot(S1,S2).
convert(sales,6,7, X, X) :- !.

convert(sales,7,8, question(I,P,S1), question(I,P,S2)) :- append(S1, [answer_type = text], S2).
convert(sales,7,8, X, X) :- !.


convert(joblook,3,4, knowledgebase(I,P,S1), knowledgebase(I,P,S2)) :- !, convert_b3_knowledgebase(S1,S2).
convert(joblook,3,4, document_format(I,P,S1), unused_document_format(I,P,S2)) :- !, convert_b3_document_format(S1,S2).
convert(joblook,3,4, X, X) :- !.

convert(joblook,4,5, question(I,P,S1), question(I,P,S2)) :- convert_b5_question_prompt(S1,S2).
convert(joblook,4,5, X, X) :- !.

convert(joblook,5,6, question(I,P,S1), question(I,P,S2)) :- convert_b6_question_prompt(S1,S2).
convert(joblook,5,6, knowledgebase(I,P,S1), knowledgebase(I,P,S2)) :- convert_j6_kb_primary(S1,S2).
convert(joblook,5,6, X, X) :- !.

convert(joblook,6,7, knowledgebase(I,P,S1), knowledgebase(I,P,S2)) :- append(S1, [date_format = 'm/d/y'], S2).
convert(joblook,6,7, table(I,P,S), rules_table(I,P,S)) :- !.
convert(joblook,6,7, knowledgebase(I,P,S1), knowledgebase(I,P,S2)) :- append(S1, [charset = $$], S2).
convert(joblook,6,7, X, X) :- !.

convert(joblook,7,8, question(I,P,S1), question(I,P,S2)) :- append(S1, [answer_type = text], S2).
convert(joblook,7,8, preference(I,P,S1), question(I,P,S2)) :- convert_j8_preference(S1,S2). 
convert(joblook,7,8, X, X) :- !.


convert(ucwa,3,4, knowledgebase(I,P,S1), knowledgebase(I,P,S2) :- !, convert_b3_knowledgebase(S1,Sx), append(Sx, [date_format = 'm/d/y'], S2).
convert(ucwa,3,4, document_format(I,P,S1), unused_document_format(I,P,S2) :- !, convert_b3_document_format(S1,S2).
convert(ucwa,3,4, X, X) :- !.

convert(ucwa,4,5, table(I,P,S), rules_table(I,P,S)) :- !.
convert(ucwa,4,5, X, X) :- !.

convert(ucwa,5,6, knowledgebase(I,P,S1), knowledgebase(I,P,S2)) :- append(S1, [charset = $$], S2).
convert(ucwa,5,6, X, X) :- !.

%-------------------------------------------------------
% basic 1 -> 2
%
% Reversed the format of rules and tables, so
% the action/value is last, not first.
%

convert_b1_rule([], []).
convert_b1_rule([rules = RTAB_1|Z1], [rules = RTAB_2|Z2]) :-
   !,
   convert_b1_rule_tab(RTAB_1, RTAB_2),
   convert_b1_rule(Z1, Z2).
convert_b1_rule([X|Z1], [X|Z2]) :-
   convert_b1_rule(Z1, Z2).

   convert_b1_rule_tab([], []).
   convert_b1_rule_tab([[VAL,CONDS]|Z1], [[CONDS,VAL]|Z2]) :-
      convert_b1_rule_tab(Z1,Z2).

convert_b1_table([], []).
convert_b1_table([rules = TAB_1|Z1], [rules = TAB_2|Z2]) :-
   !,
   convert_b1_table_row(TAB_1, TAB_2),
   convert_b1_table(Z1, Z2).
convert_b1_table([X|Z1], [X|Z2]) :-
   convert_b1_table(Z1, Z2).

   convert_b1_table_row([], []).
   convert_b1_table_row([[VAL|CONDS]|Z1], [ROW2|Z2]) :-
      reverse(CONDS,RCONDS),
      reverse([VAL|RCONDS], ROW2),
      convert_b1_table_row(Z1,Z2).

%-------------------------------------------------------
% basic 3 -> 4
%
% Move the document_format object into the 
% knowledgebase object.
%

convert_b3_knowledgebase(S1, S2) :-
   auth_schema_slot_facet(knowledgebase, question_separator, default, QSEP),
   auth_schema_slot_facet(knowledgebase, menu_separator, default, MSEP),
   auth_schema_slot_facet(knowledgebase, value_separator, default, VSEP),
   auth_schema_slot_facet(knowledgebase, question_top, default, QTOP),
   auth_schema_slot_facet(knowledgebase, question_bottom, default, QBOT),
   append(S1, [question_separator = QSEP, menu_separator = MSEP,
               value_separator = VSEP, question_top = QTOP,
               question_bottom = QBOT], S2).
 
convert_b3_document_format(S1, S1) :-
   member(question_separator = QSEP, S1),
   auth_set_slot(_, knowledgebase, _, question_separator, QSEP),
   member(menu_separator = MSEP, S1),
   auth_set_slot(_, knowledgebase, _, menu_separator, MSEP), 
   member(value_separator = VSEP, S1),
   auth_set_slot(_, knowledgebase, _, value_separator, VSEP),
   member(question_top = QTOP, S1),
   auth_set_slot(_, knowledgebase, _, question_top, QTOP),
   member(question_bottom = QBOT, S1),
   auth_set_slot(_, knowledgebase, _, question_bottom, QBOT).
convert_b3_document_format(S1, S1).

%-------------------------------------------------------
% basic 4 -> 5
%
% Convert the question prompt back to plain text
%

convert_b5_question_prompt(S1,S2) :-
   member(prompt = PROMPT, S1),
   string_expression(SPROMPT, PROMPT),
   replace_elem(prompt = PROMPT, prompt = SPROMPT, S1, S2).

%-------------------------------------------------------
% basic 5 -> 6
%
% Convert the question prompt back to an expression
%

convert_b6_question_prompt(S1,S2) :-
   member(prompt = SPROMPT, S1),
   string_expression(SPROMPT, PROMPT),
   replace_elem(prompt = SPROMPT, prompt = PROMPT, S1, S2).

%-------------------------------------------------------
% basic 6 -> 7, sales 6 -> 7
%
% Rename solution top and bottom to output
%

convert_b7_kb_topbot(S1,S2) :-
   append(S1, [output_top = text($<HTML><HEAD></HEAD><BODY>$)], Sx),
   append(Sx, [output_continue = text($<FORM METHOD="POST" ACTION="/cgi-bin/kwcgibasic.exe$ + system(cgi_parameters) + $"><INPUT NAME="Continue" TYPE="Submit" VALUE="Submit"></INPUT></FORM>$)], Sy),
   append(Sy, [output_bottom = text($</BODY></HTML>$)], S2).

convert_s7_kb_topbot(S1,S2) :-
   append(S1, [output_top = text($<HTML><HEAD></HEAD><BODY>$)], Sx),
   append(Sx, [output_continue = text($<FORM METHOD="POST" ACTION="/cgi-bin/kwcgisales.exe$ + system(cgi_parameters) + $"><INPUT NAME="Continue" TYPE="Submit" VALUE="Submit"></INPUT></FORM>$)], Sy),
   append(Sy, [output_bottom = text($</BODY></HTML>$)], S2).

%-------------------------------------------------------
% support 2 -> 3
%
% Add solution top and bottom to knowledgebase
%

convert_s3_kb_topbot(S1,S2) :-
   (member(solution_top = _, S1) ->
      Sx = S1
   ;  append(S1, [solution_top = $<HTML><HEAD></HEAD><BODY>$], Sx) ),
   (member(solution_bottom = _, Sx) ->
      S2 = Sx
   ;  append(Sx, [solution_bottom = $</BODY></HTML>$], S2) ).

%-------------------------------------------------------
% support 3 -> 4
%
% Rename solution top and bottom to output
%

convert_s4_kb_topbot(S1,S2) :-
   replace_elem(solution_top = TOP, output_top = TOP, S1, Sx),
   replace_elem(solution_bottom = BOT, output_bottom = BOT, Sx, S2).

%-------------------------------------------------------
% joblook 5 -> 6
%
% Add primary_questions to knowledgebase
%

convert_j6_kb_primary(S1,S2) :-
   (member(primary_questions = _, S1) ->
      S2 = S1
   ;  append(S1, [primary_questions = [holland_traits, secondary_holland_traits]], S2) ).

%-------------------------------------------------------
% joblook 7 -> 8
%
% Change preferences into questions
%

convert_j8_preference(S1,S2) :-
   member(preference_type = PTYPE, S1),
   member(choices = CHOICES, S1),
   member(answer_type = ATYPE, S1),
   S2 = [prompt = $Pick one$, question_type = PTYPE, question_style = listbox, 
      choices = CHOICES, answer_type = ATYPE, length = 20, default = $$, ask_also = []].

