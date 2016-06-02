%
% Old WebLS to New WebLS translator
%

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

%:- reconsult(lbops).
%:- load(list).
%:- load(bug).

test_directory($e:\\amzi\\dev\\kw4\\src\\tests\\$).

test_file_in($amzitech.lb$).
test_file_out($testtech.kb$).

webls324 :-
   test_directory(DIR),
   test_file_in(FIN),
   strcat(DIR, FIN, FPIN),
   test_file_out(FOUT),
   strcat(DIR, FOUT, FPOUT),
   consult($e:\\amzi\\dev\\kw4\\src\\jigs\\basic\\basic.jig$),
   convert324(FPIN, FPOUT).

convert324(FPIN, FPOUT) :-
   reconsult(FPIN),
   fopen(H, FPOUT, w),
   write(H, $knowledgewright_jig(basic,8).\n\n$),
   write(H, $:- discontiguous text/3.\n\n$),
   (types324(H) -> 
      true
   ; 
%     write($\noops, types324 failed\n$),
     fail),
   fclose(H),
%   nl, write(done), nl,
   true.

types324(H) :-
   modules324(H),
   questions324(H),
   rules324(H),
   answers324(H),
%   headers324(H),
%   footers324(H),
   notes324(H).
%   document_format324(H).

%-------------------------------
% modules
%

modules324(H) :-
%  write($doing modules:$), nl,
   module(NAME, PROPS3),
%  tab(2), write(module(NAME)), nl,
   props324(module, PROPS3, PROPS4),
   once build_format324(H, PROPS3, FORMATLIST),
   (member(goals = GOALLIST, PROPS3) ->
%  tab(2), write(GOALLIST), nl,
      atomlist_concat([NAME, '_doc'], DOCNAME),
      once build_goaldoc324(H, GOALLIST, DOCNAME),
%  tab(2), write($done build_goaldoc324$), nl,
      once replace_elem(goals = GOALLIST, goals = [DOCNAME], PROPS4, PROPS5),
%  tab(2), write($replace goals$), nl,
      once append(PROPS5, FORMATLIST, PROPS6),
      output(H, knowledgebase(NAME, /, PROPS6))
   ;
%  tab(2), write($no goals in $), write(PROPS3), nl,
      once append(PROPS4, FORMATLIST, PROPS5),
      output(H, knowledgebase(NAME, /, PROPS5))
   ),
   fail.
modules324(H) :-
%   write($no more\n$),
   true.

build_goaldoc324(H, GOALLIST, DOCNAME) :-
   get_docparts324(GOALLIST, STRINGLIST),
   (header(system, HPROPS) ; HPROPS = []),
   (member(text = HTEXT, HPROPS) ->
      (islist(HTEXT) ->
         stringlist_concat(HTEXT, HSTRING)
      ;  HSTRING = HTEXT )
   ;
      HSTRING = $$
   ),
   (footer(system, FPROPS) ; FPROPS = []),
   (member(text = FTEXT, FPROPS) ->
      (islist(FTEXT) ->
         stringlist_concat(FTEXT, FSTRING)
      ;  FSTRING = FTEXT )
   ;
      FSTRING = $$
   ),
   stringlist_concat(STRINGLIST, BIGSTRING),
   stringlist_concat([HSTRING, $\n$, BIGSTRING, $\n$, FSTRING], STRINGALL),
   string_expression(STRINGALL, TEXTTERM),
   output(H, text(DOCNAME, /, [description = $$, type = text, text = TEXTTERM])).


get_docparts324([], []) :- !.
get_docparts324([GOAL | REST1], [STRING | REST2]) :-
   (header(GOAL, HPROPS) ; HPROPS = []),
   (member(text = HTEXT, HPROPS) ->
      (islist(HTEXT) ->
         stringlist_concat(HTEXT, HSTRING)
      ;  HSTRING = HTEXT )
   ;
      HSTRING = $$
   ),
   (footer(GOAL, FPROPS) ; FPROPS = []),
   (member(text = FTEXT, FPROPS) ->
      (islist(FTEXT) ->
         stringlist_concat(FTEXT, FSTRING)
      ;  FSTRING = FTEXT )
   ;
      FSTRING = $$
   ),
   string_termq(SGOAL, GOAL),
   stringlist_concat([HSTRING, $\n%$, SGOAL, $%\n$, FSTRING, $\n$], STRING),
   get_docparts324(REST1, REST2).

build_format324(H, MODATTRS, FORMATLIST) :-
   % Get the separators
   (once member(defaultQuestionSeparator = QSEP, MODATTRS) ; QSEP = $<P>$),
   (once member(defaultItemSeparator = ISEP, MODATTRS) ; ISEP = $<BR>$),
   (once member(answerSeparator = ASEP, MODATTRS) ; ASEP = $<P>$),

   % Get the header and footer
   (once header(system, HPROPS) ; HPROPS = []),
   (once member(text = HTEXT, HPROPS) ->
      (islist(HTEXT) ->
         stringlist_concat(HTEXT, HSTRING)
      ;  HSTRING = HTEXT )
   ;
      HSTRING = $<HTML><HEAD></HEAD><BODY>$
   ),
   (once header(question, QHPROPS) ; QHPROPS = []),
   (once member(text = QHTEXT, QHPROPS) ->
      (islist(QHTEXT) ->
         stringlist_concat(QHTEXT, QHSTRING)
      ;  QHSTRING = QHTEXT )
   ;
      QHSTRING = $$
   ),
   (once footer(system, FPROPS) ; FPROPS = []),
   (once member(text = FTEXT, FPROPS) ->
      (islist(FTEXT) ->
         stringlist_concat(FTEXT, FSTRING)
      ;  FSTRING = FTEXT )
   ;
      FSTRING = $</BODY></HTML>$
   ),
   (once footer(system, QFPROPS) ; QFPROPS = []),
   (once member(text = QFTEXT, QFPROPS) ->
      (islist(QFTEXT) ->
         stringlist_concat(QFTEXT, QFSTRING)
      ;
         QFSTRING = QFTEXT )
   ;  QFSTRING = $$
   ),

   % Get the buttons
   (once member(submitButton = SUBMIT, MODATTRS) ; SUBMIT = $Submit$),
   (once member(resetButton = RESET, MODATTRS) ; RESET = $Reset$),
   stringlist_concat([$<HTML><HEAD></HEAD>\n$, HSTRING, $\n$, QHSTRING, $\n$, $<FORM METHOD="POST" ACTION="/cgi-bin/kwcgibasic.exe">$], QTOP),
   (string_length(RESET, 0) ->
      stringlist_concat([$<INPUT NAME="Submit" TYPE="Submit" VALUE="$, SUBMIT, $"></INPUT></FORM>\n$, FSTRING, $\n$, QFSTRING], QBOTTOM)
   ;
      stringlist_concat([$<INPUT NAME="Submit" TYPE="Submit" VALUE="$, SUBMIT, $"></INPUT>\n<INPUT TYPE="Reset" VALUE="$, RESET, $"></INPUT></FORM>\n$, FSTRING, $\n$, QFSTRING], QBOTTOM)
   ),

   FORMATLIST = [question_separator = QSEP,
      menu_separator = ISEP, value_separator = ASEP,
      question_top = text(QTOP + system(cgi_parameters)), 
      question_bottom = text(QBOTTOM),
      output_top = text($$),
      output_bottom = text($$),
      date_format = 'm/d/y'].


%----------------------------
% questions
%

questions324(H) :-
%   write($doing questions:$), nl,
   question(NAME, PROPS3),
%   tab(2), write(question(NAME)), nl,
   question_props(PROPS3, PROPS3b, PROPS4Q),
   props324(question, PROPS3b, PROPS4),
   append(PROPS4, PROPS4Q, PROPS4b),
   (member(default = _, PROPS4b) ->
      PROPS4c = PROPS4b
   ;
      append([default = $$], PROPS4b, PROPS4c)
   ),
   once add_defaults324(question, PROPS4c, PROPS4d),
   output(H, question(NAME, /, PROPS4d)),
   fail.
questions324(H) :-
%   write($no more\n$),
   true.

% add default values for missing slots
add_defaults324(CLASS, INLIST, OUTLIST) :-
   auth_get_schema_slots(CLASS, SLOTLIST),
   add_slots324(CLASS, SLOTLIST, INLIST, NEWSLOTS),
   append(INLIST, NEWSLOTS, OUTLIST).

   add_slots324(_, [], _, []) :- !.
   add_slots324(CLASS, [id | REST], INLIST, REST2) :-
      add_slots324(CLASS, REST, INLIST, REST2).
   add_slots324(CLASS, [path | REST], INLIST, REST2) :-
      add_slots324(CLASS, REST, INLIST, REST2).
   add_slots324(CLASS, [SLOT | REST], INLIST, REST2) :-
      member(SLOT = _, INLIST),
      add_slots324(CLASS, REST, INLIST, REST2).
   add_slots324(CLASS, [SLOT | REST], INLIST, [SLOT = VALUE | REST2]) :-
      auth_schema_slot_facet(CLASS, SLOT, default, VALUE),
      add_slots324(CLASS, REST, INLIST, REST2).

% take out the tricky ones, leaving the easy ones
% for the generic props324 to convert

question_props(P3in, P3out, P4Q) :-
   remove(ask = ASK, P3in, P3x),
   ( remove(style = STYLE, P3x, P3out),
     !
     ;
     STYLE = radio,
     P3out = P3x ),
   qprop(ASK, STYLE, P4Q).

qprop(menu(MENU), checkbox,
      [question_type = menu_multiple_choices_display_separate,
       'rule-display_choices' = CHOICES,
       choices = [],
       question_style = checkbox_radio]) :-
   member(item(_,_), MENU),
   !, qprop_menu(MENU, CHOICES0),
   append([[rule_text, display_text]], CHOICES0, CHOICES).
qprop(menu(MENU), listboxMultiple,
      [question_type = menu_multiple_choices_display_separate,
       'rule-display_choices' = CHOICES,
       choices = [],
       question_style = listbox]) :-
   member(item(_,_), MENU),
   !, qprop_menu(MENU, CHOICES0),
   append([[rule_text, display_text]], CHOICES0, CHOICES).
qprop(menu(MENU), radio,
      [question_type = menu_single_choice_display_separate,
       'rule-display_choices' = CHOICES,
       choices = [],
       question_style = checkbox_radio]) :-
   member(item(_,_), MENU),
   !, qprop_menu(MENU, CHOICES0),
   append([[rule_text, display_text]], CHOICES0, CHOICES).
qprop(menu(MENU), listbox,
      [question_type = menu_single_choice_display_separate,
       'rule-display_choices' = CHOICES,
       choices = [],
       question_style = listbox]) :-
   member(item(_,_), MENU),
   !, qprop_menu(MENU, CHOICES0),
   append([[rule_text, display_text]], CHOICES0, CHOICES).
qprop(menu(MENU), checkbox,
      [question_type = menu_multiple_choices,
       'rule-display_choices' = [[rule_text, display_text]],
       choices = CHOICES,
       question_style = checkbox_radio]) :-
   !, qprop_menu(MENU, CHOICES).
qprop(menu(MENU), listboxMultiple,
      [question_type = menu_multiple_choices,
       'rule-display_choices' = [[rule_text, display_text]],
       choices = CHOICES,
       question_style = listbox]) :-
   !, qprop_menu(MENU, CHOICES).
qprop(menu(MENU), radio,
      [question_type = menu_single_choice,
       'rule-display_choices' = [[rule_text, display_text]],
       choices = CHOICES,
       question_style = checkbox_radio]) :-
   !, qprop_menu(MENU, CHOICES).
qprop(menu(MENU), listbox,
      [question_type = menu_single_choice,
       'rule-display_choices' = [[rule_text, display_text]],
       choices = CHOICES,
       question_style = listbox]) :-
   !, qprop_menu(MENU, CHOICES).
qprop(field, _,
       [question_type = fill_in_the_blank,
        answer_type = string]).
qprop(yes_no, _,
      [question_type = menu_single_choice,
       'rule-display_choices' = [[rule_text, display_text]],
       choices = [$yes$, $no$],
       question_style = checkbox_radio ]).

qprop_menu(MENU, TABLE) :-
   member(item(_, _), MENU),
   !,
   qprop_menu_table(MENU, TABLE).
qprop_menu(MENU, LIST) :-
   atoms_strings(MENU, LIST).

qprop_menu_table([], []) :- !.
qprop_menu_table([item(NAME, LIST) | REST], [ROWLIST | REST2]) :-
   member(text = TEXT, LIST), !,
   string_atom(SNAME, NAME),
   ROWLIST = [SNAME, TEXT],
   qprop_menu_table(REST, REST2).
qprop_menu_table([ITEM | REST], [[ITEM, ITEM] | REST2]) :-
   qprop_menu_table(REST, REST2).
   

%----------------------------------------
% rules
%

rules324(H) :-
%   write($doing rules:$), nl,
   bagof(ifthen(C,V), if C then A = V, BAG),
%   tab(2), write(rule(A)), nl,
   (is_answer_vals(BAG) ->
      rulebag_ans(A, BAG, RULES),
      output(H, rule_set(A, /, [type = multiple_values, rules = [[conditions,value]|RULES]]))
      ;
      rulebag(A, BAG, RULES),
      output(H, rule_set(A, /, [type = single_value, rules = [[conditions,value]|RULES]])) ),
   fail.
rules324(_) :-
%   write($no more\n$),
   true.

is_answer_vals([ifthen(_,V)|_]) :-
   answer(V,_,_), !.

rulebag_ans(_, [], []) :- !.
rulebag_ans(A, [ifthen(C3,V3)|Y], [[C4,V4]|Z]) :-
   cond324(C3, C4),
   answer_name324(V3, A, V4),
   rulebag_ans(A,Y,Z).

rulebag(A, [], [[true,$unknown$]]) :- !.
rulebag(A, [ifthen(C3,V3)|Y], [[C4,V4]|Z]) :-
   cond324(C3, C4),
   val324(V3, V4),
   rulebag(A,Y,Z).

%---------------------------------------
% conditions
%

cond324(not C3, not C4) :-
   !,
   cond324(C3,C4).
cond324(C3a and C3b, C4a and C4b) :-
   !,
   cond324(C3a, C4a),
   cond324(C3b, C4b).
cond324(C3a or C3b, C4a or C4b) :-
   !,
   cond324(C3a, C4a),
   cond324(C3b, C4b).
cond324(C3, C4) :-
   C3 =.. [OP,A,V3],
   val324(V3,V4),
   C4 =.. [OP,A,V4].

val324(V, V) :-
   (number(V); string(V)),
   !.
val324(V3,[STR]) :-
   [ITEM] = V3,
   !,
   string_term(STR,ITEM).
val324(V3,V4) :-
   string_term(V4,V3).


%----------------------------------------
% answers
%

answers324(H) :-
%  write($doing answers:$), nl,
   answer(A, G, PROPS3),
   answer_name324(A, G, AA),
%  tab(2), write(answers(AA)), nl,
   props324(answer, PROPS3, PROPS4),

   (member(note = NOTESLIST, PROPS3) ->
      member(text = TEXT, PROPS4),
      separate_list([TEXT|NOTESLIST], $<p>$, SEPLIST),
      list_expression324(SEPLIST, LEXP),
      replace_elem(text = TEXT, text = text(LEXP), PROPS4, PROPS5)
   ;
      PROPS5 = PROPS4),

   (member(file = _, PROPS5 ->
      append([type = file], PROPS5, PROPS6)
   ;  append([type = text], PROPS5, PROPS6) ),

   output(H, text(AA, /, PROPS6)),
   fail.
answers324(_) :-
%   write($no more\n$),
   true.

answer_name324(A3, G3, A4) :-
   findall(G, answer(A3, G, _), GS),
   (GS \= [G] ; GS \= []),
   !,
   (var(G3) ->
      A4 = A3
   ;
      atomlist_concat([A3, '_', G3], A4)
   ).
answer_name324(A, _, A).


/*
%----------------------------------------
% headers
%

headers324(H) :-
%   write($doing headers:$), nl,
   header(A, PROPS3),
%   tab(2), write(headers(A)), nl,
   props324(header, PROPS3, PROPS4),
   output(H, header(A, /, PROPS4)),
   fail.
headers324(_) :-
%   write($no more\n$),
   true.

%----------------------------------------
% footers
%

footers324(H) :-
%   write($doing footers:$), nl,
   footer(A, PROPS3),
%   tab(2), write(footers(A)), nl,
   props324(footer, PROPS3, PROPS4),
   output(H, footer(A, /, PROPS4)),
   fail.
footers324(_) :-
%   write($no more\n$),
   true.
*/

%----------------------------------------
% notes
%

notes324(H) :-
%   write($doing notes:$), nl,
   note(A, PROPS3),
%   tab(2), write(notes(A)), nl,
   props324(note, PROPS3, PROPS4),
   output(H, text(A, /, PROPS4)),
   fail.
notes324(_) :-
%   write($no more\n$),
   true.

%-----------------------------------------
% The work of converting properties
%

%props324(TYPE, PROPS3, PROPS4) :-
%   ps324(TYPE, PROPS3, PROPS4a),
%   flatten(PROPS4a, PROPS4).

props324(TYPE, [], []).
props324(TYPE, [P3|P3s], [P4|P4s]) :-
   prop324(TYPE, P3, P4),
   !,
   props324(TYPE, P3s, P4s).
props324(TYPE, [_|P3s], P4s) :-
   props324(TYPE, P3s, P4s).


prop324(module, title = TITLE, title = TITLE).
prop324(module, goals = GOALS, goals = GOALS).

prop324(question, prompt = PROMPT, prompt = PROMPT).
prop324(question, length = L, length = L).
prop324(question, askAlso = L, ask_also = L).
prop324(question, default = D, default = D).

prop324(answer, text = TLIST, text = text(TTERM)) :-
   list_concat324(TLIST, TTERM).
prop324(answer, htmlFile = FILE, file = FILE).

prop324(header, text = TLIST, text = text(TTERM)) :-
   list_concat324(TLIST, TTERM).
prop324(footer, text = TLIST, text = text(TTERM)) :-
   list_concat324(TLIST, TTERM).
prop324(note, text = TLIST, text = text(TTERM)) :-
   list_concat324(TLIST, TTERM).


%-------------------------------
% Output a finished frame
%

output(H, FRAME) :-
   FRAME =.. [TYPE, NAME, PATH, PROPS],
   writeq(H, TYPE), write(H,'('),
      writeq(H,NAME), write(H, ', '),
      writeq(H,PATH), write(H, ', ['), nl(H),
   output_props(H,PROPS),
   write(H, '  ]).'), nl(H), nl(H).

output_props(H, [PROP=VAL]) :-
   !, tab(H,2), output_pv(H, PROP=VAL), nl(H).
output_props(H, [PROP=VAL|REST]) :-
   tab(H,2), output_pv(H, PROP=VAL), write(H,','), nl(H),
   output_props(H, REST).

output_pv(H, P=[X|Y]) :-
   !,
   writeq(H, P), write(H, ' = ['), nl(H),
   output_list(H, [X|Y], ','),
   write(H, ' ]').
output_pv(H, P=X+Y) :-
   !,
   writeq(H, P), write(H, ' ='), nl(H),
   output_exp(H, X+Y).
output_pv(H, P=V) :-
   writeq(H,P=V).

output_list(H, [X], SEP) :-
   !, tab(H,4), writeq(H,X).
output_list(H, [X|Y], SEP) :-
   tab(H,4), writeq(H,X), write(H,SEP), nl(H),
   output_list(H,Y, SEP).

output_exp(H, X+Y) :-
   !,
   output_exp(H, X),
   write(H, ' +'), nl(H),
   output_exp(H,Y).
output_exp(H, X) :-
   tab(H,4), writeq(H,X).

%-------------------------------------
% utilities
%

atoms_strings([], []).
atoms_strings([A|As], [S|Ss]) :-
   string_term(S,A),
   atoms_strings(As, Ss).

list_concat324(EXP, EXP) :-
   string(EXP), !.
list_concat324(LEXP, EXP) :-
   stringlist_concat(LEXP, EXP).


list_expression324(LEXP, EXP) :-
   var(EXP), !,
   reverse(LEXP, RLEXP),
   listexp324(RLEXP, EXP).
list_expression324(LEXP, EXP) :-
   var(LEXP),
   listexp324(RLEXP, EXP),
   reverse(RLEXP, LEXP).

   listexp324([A], A) :-
      A \= _+_.
   listexp324([A|Y], Z+A) :-
      listexp(Y,Z).
  
separate_list([], _, []) :- !.
separate_list([X], _, [X]) :- !.
separate_list([X|Y], S, [X,S|Z]) :-
   separate_list(Y, S, Z).

