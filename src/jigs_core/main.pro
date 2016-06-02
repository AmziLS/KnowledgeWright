%
% main.pro - a command line interface to kwi
%

%------------------------------------------------
% CVS history
%
% $Log: main.pro,v $
% Revision 1.1.1.1  2002/12/10 16:49:52  dennis
% Moved to jigs_core for Eclipse.
%
% Revision 1.35  2002/04/25 01:38:50  mary
% Give up trying to make clicking on a kb file open KnowledgeWright.
%
% Revision 1.34  2002/02/22 18:27:48  dennis
% Add data_table and find() function. Rename table to rules_table.
%
% Revision 1.33  2002/02/21 04:39:14  dennis
% added data_table query support, used new :- include directive for
% operators in each file
%
% Revision 1.32  2002/02/13 23:33:04  dennis
% Replaced date_time with library. Added date_format to knowledgebase.
%
% Revision 1.31  2002/01/29 21:56:03  mary
% Add operators for command line build. Other makefile nits.
%
% Revision 1.30  2001/12/23 19:50:02  mary
% Move environment to portable.
%
% Revision 1.29  2001/10/29 22:30:02  mary
% Various changes.
%
% Revision 1.28  2001/04/12 20:02:40  dennis
% fixed true/default maybe
%
% Revision 1.27  2001/04/04 13:57:41  mary
% Minor 4.1.12 changes.
%
% Revision 1.26  2001/03/08 05:01:06  mary
% Make format more forgiving about question formats. Adopt main to mary.
%
% Revision 1.25  2001/03/04 21:18:13  dennis
% changed convert, updated sales
%
% Revision 1.24  2001/02/25 20:58:27  dennis
% fixed xref bugs for author xref
%
% Revision 1.23  2001/02/15 00:12:29  dennis
% added axrf files
%
% Revision 1.22  2001/02/14 17:57:44  mary
% Changed to simple scrolling so that text will not be garbled (Java bug).
%
% Revision 1.21  2001/01/05 20:31:37  dennis
% fixed sales looping problem
%
% Revision 1.20  2001/01/05 16:59:59  dennis
% made no_more come out one step sooner from resolver
%
% Revision 1.19  2001/01/03 20:58:08  mary
% Fixed to match Mary's machine.
%
% Revision 1.18  2000/12/31 17:21:00  dennis
% checking in whatever latest changes were
%
% Revision 1.17  2000/12/15 18:16:24  dennis
% adding minor fixes
%
% Revision 1.16  2000/12/14 21:44:01  dennis
% Fixed minor bug in expressions.
%
% Revision 1.15  2000/12/12 06:15:16  dennis
% fixed variety of xref bugs
%
% Revision 1.14  2000/12/08 23:53:19  dennis
% added debug to file option
%
% Revision 1.13  2000/12/07 04:01:00  dennis
% save sessions to disk
%
% Revision 1.12  2000/12/04 18:59:38  dennis
% fixed compare_lists and string_expressions in author
%
% Revision 1.11  2000/12/04 13:57:46  mary
% Re-added the hidden facts output code. Have mary rule main.
%
% Revision 1.10  2000/12/04 03:11:23  dennis
% added parm checking to question bottoms as well
%
% Revision 1.9  2000/12/02 06:07:40  dennis
% some performance changes
%
% Revision 1.8  2000/12/02 04:57:27  dennis
% got dates and automatic fact conversion going
%
% Revision 1.7  2000/12/01 05:03:44  dennis
% added hidden fields, also conditions in a list
%
% Revision 1.6  2000/12/01 02:15:10  dennis
% fixed separators, other minutia
%
% Revision 1.5  2000/11/29 19:09:50  dennis
% Added CVS logging to source files
%
%
%------------------------------------------------

% Operators

:- include('kw_ops.pro').


%#define cf(X) (write(call:X),nl;write(fail:X),nl,fail)
%#define er(X) (write(exit:X),nl;write(redo:X),nl,fail)

dennis :-
%   assert(main_directory($d:\\amzi\\dev\\kw4\\src\\samples\\support\\$)),
%   assert(main_directory($d:\\amzi\\dev\\kw4\\src\\samples\\joblook\\$)),
%   assert(main_directory($d:\\amzi\\dev\\kw4\\src\\samples\\ucwa\\$)),
   assert(main_directory($d:\\amzi\\dev\\kw4\\src\\tests\\$)),
%   assert(main_directory($\\amzi\\dev\\kw4\\src\\tests\\$)),
%   assert(main_directory($\\amzi\\dev\\kw4\\src\\samples\\sales\\$)),
%   assert(main_directory($d:\\amzi\\dev\\kw4\\src\\samples\\basic\\tutorial\\$)),

   assert(main_logicbase($colors2.kb$)).
%   assert(main_logicbase($joblook.kb$)).
%   assert(main_logicbase($kw_sales.kb$)).
%   assert(main_logicbase($amzisite.kb$)).
%   assert(main_logicbase($shapes.kb$)).
%   assert(main_logicbase($ucwa.kb$)).
%   assert(main_logicbase($support_test.kb$)).
%   assert(main_logicbase($amzi_tech_support.kb$)).

mary :-
   assert(main_directory($c:\\amzi\\dev\\kw4\\src\\samples\\basic\\$)),
%   assert(main_directory($c:\\amzi\\dev\\kw4\\src\\samples\\ucwa\\$)),
%   assert(main_logicbase($modifiers.kb$)).
%   assert(main_logicbase($jobsmall9.kb$)).
%   assert(main_logicbase($kw_sales.kb$)).
%   assert(main_logicbase($amzi_tech_support.kb$)).
   assert(main_logicbase($amzi_website_advisor.kb$)).
%   assert(main_logicbase($shapes.kb$)).
%   assert(main_logicbase($ucwa.kb$)).

main :-
%   dennis,
   mary,
   write($Welcome$), nl, nl,
   catch(
      (
         main_directory(D),
         kwi(0, initialize([
                   directory = D,
                   log_file = $kwlog.txt$,
                   message_level = max,
                   session_directory = $\\amzi\\dev\\kw4\\src\\tests\\$,
%                   debug_msgs = $\\amzi\\dev\\kw4\\src\\tests\\kw_dbg.txt$]),  % local, logfile, filename
                   debug_msgs = local]),  % local, logfile, filename
             INFO),
         output_list(INFO, $\n$),
         output(nl), output(nl),
         (  main_logicbase(LB_FILE)
            ;
            prompt($Enter Logic Base: $, LBNAME),
            strcat(LBNAME, $.kb$, LB_FILE) ),
         !,
         kwi(0, open(LB_FILE), _),
         go

         %continue(i1)
      ),
      ERROR,
      process_error(ERROR) ),
   output([$done$, nl]).

process_error(kwi_error(SID)) :-
   kwi(SID, get_error, ERROR),
   output([$***Error***$, nl, $   $]),
   output_list(ERROR, $\n   $),
   output(nl).
process_error(ERROR) :-
   output([$non-KWI error: $, ERROR]),
   output(nl).
   
go :-
   gensym(i, SID),
   kwi(SID, new_session, _),
   resolve(SID, more).

go(SID) :-
   kwi(SID, new_session, _),
   resolve(SID, more).

continue(SID) :-
   resolve(SID, more).

resolve(SID, more) :-
   kwi(SID, solve, MORE),
   take_actions(SID),
   resolve(SID, MORE).
resolve(SID, no_more) :-
   output([nl, $no more for $, SID, nl]),
   kwi(SID, close, _).

take_actions(SID) :-
   once kwi(SID, get_action, ACTION),
   ACTION \= none,
   take_action(SID, ACTION),
   take_actions(SID).
take_actions(_).

take_action(SID, ask(user,F,PROPS)) :-
   get_property(PROPS, prompt, PROMPT),
   get_property(PROPS, question_type, QTYPE),
   ask_user(QTYPE, PROMPT, VAL, PROPS),
   kwi(SID, assert(fact(F,VAL)), _).
take_action(SID, ask(html,F,PROPS)) :-
   get_property(PROPS, html, HTML),
   output([$HTML Asking: $, F, nl]),
   output(HTML).
take_action(SID, ask(X,F,PROPS)) :-
   throw(error(unknown_ask, [action=X, fact=F, properties=PROPS])).
take_action(SID, tell(user,PROPS)) :-
   tell_user(PROPS).
take_action(SID, tell(email, PROPS)) :-
   tell_email(PROPS).
take_action(SID, tell(PARTY, PROPS)) :-
   tell_party(PARTY, PROPS).
take_action(SID, X) :-
   output([$unknown action: $, X, nl]).


xcaller_ask(user, FACT, VAL, PROPS) :-
   output($eeeeyyyhaaaa\n$),
   get_property(PROPS, prompt, PROMPT),
   get_property(PROPS, question_type, QTYPE),
   ask_user(QTYPE, PROMPT, VAL, PROPS).

xcaller_tell(user, PROPS) :-
   output($eeeeyyyhaaaa\n$),
   tell_user(PROPS).
xcaller_tell(PARTY, PROPS) :-
   output($eeeeyyyhaaaa\n$),
   tell_party(PARTY, PROPS).

tell_user(PROPS) :-
   member(text=TEXT, PROPS),
   (member(goal=GOAL, PROPS) ->
      output(GOAL), nl
      ;
      true),
   (islist(TEXT) ->
      output($  $),
      output_list(TEXT, $\n\n  $)
      ;
      output(TEXT) ),
   output(nl), output(nl).

tell_email(PROPS) :-
   member(data=DATA, PROPS),
   output($e-mailing: $), output(nl),
   (member(to=TO, DATA) -> output([$  TO: $, TO, nl]); true),
   (member(subject=RE, DATA) -> output([$  RE: $, RE, nl]); true),
   (member(text=TEXT, DATA) -> output([$  TEXT: $, TEXT, nl]); true),
   output(nl).

tell_party(PARTY, PROPS) :-
   output([PARTY, $ing$, nl]),
   output([tab(2), PROPS, nl]).
   

ask_user(menu_multiple_choices, PROMPT, VAL, PROPS) :-
   output(nl),
   get_property(PROPS, choices, MENU),
   menuask_list(PROMPT, MENU, VAL),
   output(nl).
ask_user(menu_single_choice, PROMPT, VAL, PROPS) :-
   output(nl),
   get_property(PROPS, choices, MENU),
   menuask(PROMPT, MENU, VAL),
   output(nl).
ask_user(menu_single_choice_display_separate, PROMPT, VAL, PROPS) :-
   output(nl),
   get_property(PROPS, 'rule-display_choices', MENU),
   menuask(PROMPT, MENU, [VAL,_]),
   output(nl).
ask_user(fill_in_the_blank, PROMPT, VAL, PROPS) :-
   output(nl),
   prompt(PROMPT, SVAL),
   (get_property(PROPS, answer_type, text) ->
      VAL = SVAL
      ;
      string_term(SVAL, VAL) ),
   output(nl).

act(ID, ask_user(PROMPT, menu(MENU)), VAL) :-
   output(nl),
   menuask(PROMPT, MENU, VAL),
   output(nl).
act(ID, ask_user(PROMPT, field(LENGTH)), VAL) :-
   output(nl),
   prompt(PROMPT, VAL),
   output(nl).

act(ID, solution(S, TEXT)) :-
   output(nl),
   output([$Solution: $, S, nl, TEXT]).

%------
% io
%

output(nl) :- nl, !.
output(sp) :- write($ $), !.
output(tab(N)) :- tab(N), !.
output(A:B) :- output(A), output($: $), output(B), !.
output([]) :- !.
output([H|T]) :- output(H), !, output(T).
output(X) :- write(X).

input(X) :- read_string(X).

prompt(PROMPT, REPLY) :- output(PROMPT), output(tab(1)), input(REPLY).

output_list([], _) :- output($Empty List$), !.
output_list([X], _) :- output(X), !.
output_list([X|Y], SEP) :-
   output([X, SEP]),
   !, output_list(Y, SEP).
output_list(X, _) :-
   output(X).

%-------------------
% asking the user
%

menuask_list(PROMPT, MENU, CHOICE) :-
   output([PROMPT, nl]),
   display_menu(MENU),
   prompt($Enter your choice(s) (or list i.e. 3,5,2) > $, SITEM),
   string_term(SITEM, ITEM),
   !, pick_list_menu(ITEM, MENU, CHOICE).

pick_list_menu((A,B), MENU, [CHA|CHOICES]) :-
   !,
   pick_menu(A, MENU, CHA),
   pick_list_menu(B, MENU, CHOICES).
pick_list_menu(A, MENU, [CHOICE]) :-
   pick_menu(A, MENU, CHOICE).

menuask(PROMPT, MENU, CHOICE) :-
   output([PROMPT, nl]),
   display_menu(MENU),
   prompt($Enter your choice > $, SITEM),
   string_term(SITEM, ITEM),
   !, pick_menu(ITEM, MENU, CHOICE).

display_menu(MENU) :-
   disp_menu(1, MENU), !.

disp_menu(_, []).
disp_menu(N, [[_,ITEM] | REST]) :-
   output([N, $: $, ITEM, nl]),
   NN is N + 1,
   disp_menu(NN, REST).
disp_menu(N, [ITEM | REST]) :-
   output([N, $: $, ITEM, nl]),
   NN is N + 1,
   disp_menu(NN, REST).

pick_menu(ITEM, MENU, CHOICE) :-
   ITEM > 0,
   !,
   pic_menu(1, ITEM, MENU, CHOICE).
pick_menu(ITEM_CHOICE, _, ITEM_CHOICE).

pic_menu(_, _, [], other).
pic_menu(N, N, [ITEM|_], ITEM) :- !.
pic_menu(I, N, [_|REST], ITEM) :-
   II is I + 1,
   !, pic_menu(II, N, REST, ITEM).

