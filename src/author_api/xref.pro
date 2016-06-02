% xref

% Operators

:- include('..\\jigs\\kw_ops.pro').


%------------------------------------------
% testing tools
%

xropen :-
%   reconsult($\\amzi\\dev\\kw4\\src\\tests\\colors.kb$).
%   auth_open('\\amzi\\dev\\kw4\\src\\tests\\badcolors.kb',
   auth_open('\\amzi\\dev\\kw4\\src\\samples\\support\\amzi_tech_support.kb',
             '\\amzi\\dev\\kw4\\workshop\\jigs\\').

xref_out('xref.txt').

xrtest :-
   xropen,
   auth_xref,
   xrreport.

xrreport :-
   listing(xr_value_use/3),
   listing(xr_uses/2),
   listing(xr_usedby/2),
   listing(xr_undefined/2),
   listing(xr_unused/1),
   listing(xr_circle/1).

%------------------------------------------

auth_xref_uses(TYPE, NAME, LIST) :-
%   xref,
%   !,
   FRAME =.. [TYPE,NAME],
   xr_uses(FRAME, LIST).
auth_xref_uses(_, _, []).

auth_xref_usedby(TYPE, NAME, LIST) :-
%   xref,
%   !,
   FRAME =.. [TYPE,NAME],
   xr_usedby(FRAME, LIST).
auth_xref_usedby(_, _, []).

auth_xref_undefined(ULIST) :-
%   xref,
   findall(LIST : FRAME, xr_undefined(FRAME,LIST), TLIST),
   undefined_convert(TLIST, ULIST).

   undefined_convert([], []) :- !.
   undefined_convert([TLIST : TFRAME | REST], [STRING | REST2]) :-
      ulist_convert(TLIST, SLIST),
      stringlist_concat(SLIST, SNAMES),      
      TFRAME =.. [OBJECT,NAME],
      string_term(SOBJECT, OBJECT),
      string_term(SNAME, NAME),
      stringlist_concat([$object(s) $, SNAMES, $ are undefined in $, SOBJECT, $ $,SNAME], STRING),
      undefined_convert(REST, REST2).

   ulist_convert([NAME], [STRING]) :-
      string_term(SNAME, NAME),
      stringlist_concat([$"$, SNAME, $"$], STRING).
   ulist_convert([NAME | REST], [STRING | REST2]) :-
      string_term(SNAME, NAME),
      stringlist_concat([$"$, SNAME, $", $], STRING),
      ulist_convert(REST, REST2).

auth_xref_bad_values(LIST) :-
   findall(FR : C : MSG, xr_value_use(FR, C, MSG), FLIST),
   bad_values_convert(FLIST, LIST).

   bad_values_convert([], []) :- !.
   bad_values_convert([FRAME : COND : MSG | REST], [STRING | REST2]) :-
      FRAME =.. [OBJECT,NAME],
      string_term(SOBJECT, OBJECT),
      string_term(SNAME, NAME),
      string_termq(SCOND, COND),
      stringlist_concat([MSG, `: `, SCOND, `, in `, SOBJECT, ` `, SNAME], STRING),
      bad_values_convert(REST, REST2).

auth_xref_unused(ULIST) :-
%   xref,
   findall(FRAME, xr_unused(FRAME), TLIST),
   unused_convert(TLIST, ULIST).

   unused_convert([], []) :- !.
   unused_convert([TFRAME | REST], [STRING | REST2]) :-
      TFRAME =.. [OBJECT, NAME],
      string_term(SOBJECT, OBJECT),
      string_term(SNAME, NAME),
      stringlist_concat([SOBJECT, $ "$, SNAME, $" is not used$], STRING),
      unused_convert(REST, REST2).
      
auth_xref_circles(CLIST) :-
%   xref,
   findall(LIST, xr_circle(LIST), TLIST),
   circle_convert(TLIST, CLIST).

   circle_convert([], []) :- !.
   circle_convert([LIST | REST], [STRING | REST2]) :-
      clist_convert(LIST, STRINGLIST),
      stringlist_concat(STRINGLIST, STRING),
      circle_convert(REST, REST2).

   clist_convert([TFRAME], [STRING]) :-
      TFRAME =.. [OBJECT, NAME],
      string_term(SOBJECT, OBJECT),
      string_term(SNAME, NAME),
      stringlist_concat([SOBJECT, $ "$, SNAME, $"$], STRING).
   clist_convert([TFRAME | REST], [STRING | REST2]) :-
      TFRAME =.. [OBJECT, NAME],
      string_term(SOBJECT, OBJECT),
      string_term(SNAME, NAME),
      stringlist_concat([SOBJECT, $ "$, SNAME, $" calls $], STRING),
      clist_convert(REST, REST2).

auth_xref :-
   % Generate the schema
   abolish(xref_entities/1),
   abolish(xref_scheme/1),
   xref_build_scheme,

   abolish(xr_uses/2),
   abolish(xr_usedby/2),
   abolish(xr_undefined/2),
   abolish(xr_unused/1),
   abolish(xr_circle/1),
   abolish(xr_value_use/3),
   xref_scheme(XSCHEME),
%   (xref_out(XOUT) -> tell(XOUT); true),
%   write($---USES---$),nl,
   member(ENT:SLOTS, XSCHEME),
% ENT = table,
   FR1 =.. [ENT,X],
   FR3 =.. [ENT,X,_,_],
%   table(X,Y,Z),
   call(FR3),
   xref_uses(FR1, SLOTS, UL),
   assert(xr_uses(FR1,UL)),
%   writeq(FR1:UL), nl,
   fail.
auth_xref :-
%   nl,write($---USED BY---$),nl,
   xref_usedby,
%   nl,write($---Undefined---$), nl,
   xref_undefined,
%   nl,write($---Unused---$), nl,
   xref_unused,
%   nl,write($---Circles---$), nl,
   xref_circles,
%   nl,write($--- ---$), nl, nl,
%   told,
%   listing(xr_uses),
%   listing(xr_usedby),
%   listing(xr_undefined),
%   listing(xr_unused),
%   listing(xr_circle),
    true.
auth_xref :-
%   told,
    true.

xref_build_scheme :-
   auth_get_classes(CLASSES),
   assert(xref_entities(CLASSES)),
   xref_get_entries(CLASSES, SCHEME_LIST),
   assert(xref_scheme(SCHEME_LIST)).

   xref_get_entries([], []) :- !.
   xref_get_entries([CLASS | REST1], [CLASS:XREF_LIST | REST2]) :-
      auth_get_schema_slots(CLASS, SLOT_LIST),
      xref_get_slots(CLASS, SLOT_LIST, XREF_LIST),
      list(XREF_LIST), !,
      xref_get_entries(REST1, REST2).
   xref_get_entries([CLASS | REST1], REST2) :-
      xref_get_entries(REST1, REST2).

   xref_get_slots(CLASS, [], []) :- !.
   xref_get_slots(CLASS, [SLOT | REST1], [SLOT = XREF | REST2]) :-
      auth_schema_slot_facet(CLASS, SLOT, xref, XREF), !,
      xref_get_slots(CLASS, REST1, REST2).
   xref_get_slots(CLASS, [SLOT | REST1], REST2) :-
      xref_get_slots(CLASS, REST1, REST2).
      
/*
xref_entities([knowledgebase,text,rule_set,question,fact,table,sql]).

xref_scheme([
      knowledgebase:[goals = list],
      rule_set:[rules = rules],
      question:[ask_also = list, prompt = text],
      sql:[query = text],
      table:[rules = table],
      text:[text = text]
   ]).
*/

xref_usedby :-
   xref_entities(XENTS),
   member(ENT, XENTS),
   FR1 =.. [ENT,X],
   FR3 =.. [ENT,X,_,_],
   call(FR3),
   findall(FR, (xr_uses(FR,L), member(FR1,L)), UBL),
   sort(UBL, SUBL),
   assert(xr_usedby(FR1,SUBL)),
%   writeq(FR1:SUBL), nl,
   fail.
xref_usedby.
   

xref_uses(FR, SLOTS, USES_LIST) :-
   xref_slots(FR, SLOTS, UL1),
   flaten(UL1, UL2),
   remove_dups(UL2, UL3),
   sort(UL3, USES_LIST),
   !.

xref_slots(FR, [], []).
xref_slots(FR, [SLOT = TYPE|REST], [UL1|UL]) :-
   get_slot(FR, SLOT, CONTENTS),
   !,
   xref_contents(TYPE, CONTENTS, UL1, FR),
   xref_slots(FR, REST, UL).
xref_slots(FR, [SLOT = _|REST], UL) :-
   xref_slots(FR, REST, UL).

   xref_contents(rules, [_|CONTENTS], UL, FR) :-
      xref_rules(CONTENTS, UL, FR).
   xref_contents(conditions, CONTENTS, UL, FR) :-
      xref_conds(CONTENTS, UL, FR).
   xref_contents(text, CONTENTS, UL, FR) :-
      xref_exp(CONTENTS, UL, FR).
   xref_contents(expression, CONTENTS, UL, FR) :-
      xref_exp(CONTENTS, UL, FR).
   xref_contents(table, CONTENTS, UL, FR) :-
      xref_table(CONTENTS, UL, FR).
   xref_contents(data_table, [_|CONTENTS], UL, FR) :-
      xref_rows(CONTENTS, UL, FR).
   xref_contents(data_table, [], UL, FR).
   xref_contents(list, CONTENTS, UL, FR) :-
      xref_list(CONTENTS, UL).
   xref_contents(term, CONTENTS, UL, FR) :- % Mary tried this for UCWA
      xref_item(CONTENTS, UL).
   xref_contents(TYPE, CONTENTS, UL, FR) :-
      throw(internal_error($Bad XRef$, TYPE, CONTENTS, FR)).

   xref_list([], []).
   xref_list([X|Y], [XI|Z]) :-
      xref_item(X,XI),
      xref_list(Y,Z).

   xref_table([HEADS|ROWS], [UL1,UL2], FR) :-
      xref_header(HEADS, UL1),
      xref_rows(ROWS, UL2, FR).

   xref_header([VAL], []).
   xref_header([H|T], [XH|Z]) :-
      xref_item(H, XH),
      xref_header(T, Z).

   xref_rows([], [], _).
   xref_rows([ROW|REST], [UL1|UL], FR) :-
      xref_row(ROW, UL1, FR),
      xref_rows(REST, UL, FR).

   xref_row([], [], _).
   xref_row([H|T], [UL1|UL], FR) :-
      xref_exp(H, UL1, FR),
      xref_row(T, UL, FR).

   xref_rules([], [], _).
   xref_rules([[CONDS,ACT]|REST], [UL1,UL2|UL], FR) :-
      xref_conds(CONDS, UL1, FR),
      xref_act(ACT, UL2, FR),
      xref_rules(REST, UL, FR).

   xref_conds([], [], _) :- !.
   xref_conds([C | Cs], [UL | ULs], FR) :-
      !,
      xref_conds(C, UL, FR),
      xref_conds(Cs, ULs, FR).
   xref_conds(C1 and C2, [UL1,UL2], FR) :-
      !,
      xref_conds(C1, UL1, FR),
      xref_conds(C2, UL2, FR).
   xref_conds(C1 or C2, [UL1,UL2], FR) :-
      !,
      xref_conds(C1, UL1, FR),
      xref_conds(C2, UL2, FR).
   xref_conds(not C, [UL], FR) :-
      !,
      xref_conds(C, UL, FR).
   xref_conds(C, [U1,U2], FR) :-
      C =.. [OP, T1, T2],
      xref_value_use(OP, T1, T2, FR),
%      xref_item(T1, U1),
%      xref_item(T2, U2).
       xref_exp(T1, U1, FR),
       xref_exp(T2, U2, FR).
   xref_conds(true, [], _).
   xref_conds(default, [], _).

   xref_act(ACT, UL, FR) :-
       xref_exp(ACT, UL, FR).

   xref_exp(*, [], _) :- !.
   xref_exp(E, [], _) :-
      ( string(E); number(E); is_constant(E); E = date(_,_,_); E = system(_); E = true ),
      !.
   xref_exp(find(COL from TABLE where CONDS), [data_table(TABLE)|UL], FR) :-
      xref_conds(CONDS, UL1, FR),
      flaten(UL1, UL2),
      remove_dups(UL2, UL3),
      get_slot(data_table(TABLE), data, [COLNAMES | REST]),
      xref_remove_colnames(COLNAMES, UL3, UL),
      !.
   xref_exp(E, UL, FR) :-
      atom(E),
      !,
      xref_item(E, UL).
   xref_exp(E, [UL1,UL2], FR) :-
      E =.. [OP, E1, E2],
      !,
      xref_exp(E1, UL1, FR),
      xref_exp(E2, UL2, FR).
   xref_exp(E, [UL1], FR) :-
      E =.. [OP, E1],
      !,
      xref_exp(E1, UL1, FR).

   xref_item(T, []) :-
      ( string(T); number(T); T = date(_,_,_); is_constant(T); T = [] ),
      !.
   xref_item(T, FRSTR) :-
      xref_entities(ENTS),
      member(FR, ENTS),
      FRSTR =.. [FR,T],
      frame_exists(FRSTR), !.
   xref_item(T, undefined(T)).

   is_constant(today).
   is_constant(tomorrow).
   is_constant(yesterday).
   is_constant(last_week).
   is_constant(next_week).
   is_constant(last_month).
   is_constant(next_month).
   is_constant(last_year).
   is_constant(next_year).

   xref_remove_colnames([], X, X).
   xref_remove_colnames([NAME | REST], INLIST, OUTLIST) :-
      remove(undefined(NAME), INLIST, LIST),
      xref_remove_colnames(REST, LIST, OUTLIST).
   xref_remove_colnames([NAME | REST], INLIST, OUTLIST) :-
      xref_remove_colnames(REST, INLIST, OUTLIST).      
      
%--------------------------
% Warnings
%

% This should be schema-driven, rather than
% hard coded.  Look at schema and see if the
% entity in first arg has a value list.

xref_value_use(_, _, ATOM_VALUE, _) :-
   atom(ATOM_VALUE),
   !.
xref_value_use(OP, Q, V, FR) :-
   get_slot(question(Q), question_type, TYPE),
   (  TYPE = menu_single_choice_display_separate;
      TYPE = menu_multiple_choices_display_separate ),
   OP \== contains,
   OP \== contains_nocase,
   get_slot(question(Q), 'rule-display_choices', CH),
   !,
   ( ( member([V,_], CH) ; ([ITEM] = V, member([ITEM,_],CH) ) ) ->
      true
      ;
      C =.. [OP,Q,V],
      assert(xr_value_use(FR, C, `Undefined value in condition`)) ).
xref_value_use(OP, Q, V, FR) :-
   get_slot(question(Q), question_type, TYPE),
   (  TYPE = menu_single_choice;
      TYPE = menu_multiple_choices ),
   OP \== contains,
   OP \== contains_nocase,
   get_slot(question(Q), choices, CH),
   !,
   ( ( member(V, CH) ; ([ITEM] = V, member(ITEM,CH) ) ) ->
      true
      ;
      C =.. [OP,Q,V],
      assert(xr_value_use(FR, C, `Undefined value in condition`)) ).
xref_value_use(OP, Q, V, FR) :-
   numeric(V),
   !,
   ( get_slot(question(Q), answer_type, number) ->
      true
      ;
      C =.. [OP,Q,V],
      assert(xr_value_use(FR, C, `Inappropriate numeric value in condition`)) ).
xref_value_use(_,_,_,_).


xref_undefined :-
   setof(X,
         (xr_uses(ENT, L), member(undefined(X),L)),
         UL),
   (UL \= [] ->
%      writeq(ENT:undefined(UL)), nl,
      assert(xr_undefined(ENT, UL))
      ;
      true),
   fail.
xref_undefined.

xref_unused :-
   schema_global(xref_roots, ROOTS),
   xr_usedby(ENT, []),
   ENT =.. [CLASS, NAME],
   once not(member(CLASS, ROOTS)),
%   writeq(ENT:unused), nl,
   assert(xr_unused(ENT)),
   fail.
xref_unused.

xref_circles :-
   xr_uses(knowledgebase(LB), LIST),
%   writeq(knowledgebase(LB)), nl,
   xref_path(2, knowledgebase(LB), LIST, [knowledgebase(LB)], PATH),
   fail.
xref_circles.

xref_path(I, ENT, [], PATH, RPATH) :- 
   !,
   reverse(PATH, RPATH).
xref_path(I, ENT, LIST, ACC, PATH) :-
   member(E2, LIST),
%   tab(I), writeq(E2), nl,
   xrp_next(I, E2, ACC, PATH).

xrp_next(I, E2, ACC, PATH) :-
   is_member(E2, ACC),
   !,
   reverse([E2|ACC], PATH),
   assert(xr_circle(PATH)),
%   writeq($***circling***$), nl,
   true.
xrp_next(I, E2, ACC, PATH) :-
   xr_uses(E2, L2),
   !,
   II is I + 2,
   xref_path(II, E2, L2, [E2|ACC], PATH).
xrp_next(I, E2, ACC, PATH) :-
   reverse([E2|ACC], PATH).

output_path(N, []).
output_path(N, [P|Ps]) :-
   tab(N),
   writeq(P), nl,
   NN is N + 2,
   output_path(NN, Ps).
