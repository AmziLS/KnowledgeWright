%------------------------------------------------
% CVS history
%
% $Log: formats.pro,v $
% Revision 1.1.1.1  2002/12/10 16:49:52  dennis
% Moved to jigs_core for Eclipse.
%
% Revision 1.34  2002/12/10 16:45:19  dennis
% Tweaks
%
% Revision 1.33  2002/08/15 16:33:38  mary
% Don't call substring with bad arguments. Added log flushing (commented out).
%
% Revision 1.32  2002/06/08 21:08:49  dennis
% Repackaged and changed serial numbers.
%
% Revision 1.31  2002/05/31 16:44:17  dennis
% Fix personal ad.
%
% Revision 1.30  2002/05/29 21:58:06  dennis
% Add license types to kb files and change commercial->professional.
%
% Revision 1.29  2002/05/04 19:09:23  dennis
% Change initial session id from 0 to kw_init. Fix bug inserting KW ad.
%
% Revision 1.28  2002/04/16 20:27:13  dennis
% Added advertisement to HTML for personal license.
%
% Revision 1.27  2002/02/22 18:27:48  dennis
% Add data_table and find() function. Rename table to rules_table.
%
% Revision 1.26  2002/02/21 04:39:14  dennis
% added data_table query support, used new :- include directive for
% operators in each file
%
% Revision 1.25  2002/01/29 21:56:03  mary
% Add operators for command line build. Other makefile nits.
%
% Revision 1.24  2001/03/16 02:49:15  mary
% Fixed bug that returned a variable and true when there was no row to match
% in a table. (Actually failed on backtracking.)
%
% Revision 1.23  2001/03/08 05:01:06  mary
% Make format more forgiving about question formats. Adopt main to mary.
%
% Revision 1.22  2001/03/04 22:05:38  mary
% Added format_ouput to handle 3 new output fields in knowledgebase objects.
%
% Revision 1.21  2001/02/15 00:12:29  dennis
% added axrf files
%
% Revision 1.20  2001/02/06 00:21:45  mary
% For menus, allow display text separate from text value used in rules.
%
% Revision 1.18  2001/01/19 03:48:25  mary
% Throw error if bad message_level is provided.
% Fix sales to handle failure on a benefit properly and format questions
% only once.
%
% Revision 1.17  2001/01/18 22:15:43  mary
% Merged document_format object with knowledgebase object.
%
% Revision 1.16  2000/12/12 05:51:16  mary
% Stop outputting hidden facts. Removed ?'s.
%
% Revision 1.15  2000/12/05 19:43:21  mary
% Take out debugging stuff.
%
% Revision 1.14  2000/12/04 19:27:31  mary
% Merge in Mary's changes.
%
% Revision 1.13  2000/12/04 13:57:46  mary
% Re-added the hidden facts output code. Have mary rule main.
%
% Revision 1.12  2000/12/04 03:11:23  dennis
% added parm checking to question bottoms as well
%
% Revision 1.11  2000/12/03 20:10:14  dennis
% added system(parm) inserts for question tops
%
% Revision 1.10  2000/12/01 05:03:44  dennis
% added hidden fields, also conditions in a list
%
% Revision 1.9  2000/12/01 02:15:10  dennis
% fixed separators, other minutia
%
% Revision 1.8  2000/11/29 22:25:05  dennis
% Changed question_document object to document_format object,
% modified slots to reflect questionness of items.
%
% Revision 1.7  2000/11/29 19:09:50  dennis
% Added CVS logging to source files
%
%
%------------------------------------------------

% Operators

:- include('kw_ops.pro').


%------------------------------------------------
% format questions
%

%format_questions(S>S) :-
%   not frame_exists(document_format(_)),
%   !.
format_questions(S1>S2) :-
   session_get(actions = ACTIONS, S1),
   get_slot(knowledgebase(_), question_top, TOP1),
   format_add_parms(TOP1, TOP),
   get_slot(knowledgebase(_), question_separator, SEP),
   format_get_quests(ACTIONS, QUESTS),
   (QUESTS == [] ->
      S2 = S1
      ;
      format_quests(QUESTS, SEP, [TOP], HTMLQS, [], RFACTS),
      get_slot(knowledgebase(_), question_bottom, BOTTOM1),
      format_add_parms(BOTTOM1, BOTTOM2),
      once insert_personal_footer(BOTTOM2, BOTTOM),

%      format_hidden_facts(HIDDEN, S1),
%      reverse([BOTTOM,HIDDEN|HTMLQS], HTML_LIST),

      reverse([BOTTOM|HTMLQS], HTML_LIST),
      reverse(RFACTS, FACTS),
      stringlist_concat(HTML_LIST, HTML),
      session_add(actions < ask(html,FACTS,[html=HTML]), indent(0), S1>S2) ).
format_questions(_) :-
   throw(error(bad_html_question, [message = $Unable to format HTML for questions$])).

format_get_quests([], []).
format_get_quests([ask(user,X,Y)|ACTIONS], [ask(user,X,Y)|QUESTS]) :-
   !, format_get_quests(ACTIONS, QUESTS).
format_get_quests([_|ACTIONS], QUESTS) :-
   format_get_quests(ACTIONS, QUESTS).

format_quests([], _, [SEP|QUESTS], QUESTS, FACTS, FACTS).
format_quests([ask(user,FACT,PROPERTIES)|REST], SEP, ACC, QUESTS, FACC, FACTS) :-
   format_question(FACT,PROPERTIES,QUEST),
   !, format_quests(REST, SEP, [SEP,QUEST|ACC], QUESTS, [FACT|FACC], FACTS).

format_hidden_facts(HIDDEN, S) :-
   session_get(known = KNOWNS, S),
   format_hidden(KNOWNS, HIDDENS),
   stringlist_concat(HIDDENS, HIDDEN).

format_hidden([], []).
format_hidden([fact(A, V) | T], [I | T2]) :-
   list(V),
   string_termq(SLIST, V),
   string_atom(SA, A),
   stringlist_concat([$<INPUT TYPE=HIDDEN NAME="$, SA, $", VALUE="$, SLIST, $">\n$], I),
   format_hidden(T, T2).
format_hidden([fact(A, V) | T], [I | T2]) :-
   string_termq(TV, V),
   string_atom(SA, A),
   stringlist_concat([$<INPUT TYPE=HIDDEN NAME="$, SA, $", VALUE="$, TV, $">\n$], I),
   format_hidden(T, T2).

format_add_parms(text(TEXT_EXP), TEXT) :-
   !,
   format_add_parms(TEXT_EXP, TEXT).
format_add_parms(T1+T2, TEXT) :-
   !,
   format_add_parms(T1, TXT1),
   format_add_parms(T2, TXT2),
   strcat(TXT1,TXT2,TEXT).
format_add_parms(system(PARM), TEXT) :-
   !,
   initial_parms(PARMS),
   (get_property(PARMS, PARM, TEXT) ->
      true
      ;
      TEXT = $$).
format_add_parms(TEXT, TEXT) :-
   string(TEXT),
   !.
format_add_parms(X, _) :-
   throw(error(bad_format, [
      message = $Invalid system parameter in text$,
      entity = X ]) ).

%----

% <INPUT NAME=name SIZE=n VALUE="value" DEFAULT="default"></INPUT>
format_question(FACT, PROPERTIES, QDOC) :-
   get_property(PROPERTIES, question_type, fill_in_the_blank),
   !,
   get_property(PROPERTIES, prompt, PROMPT),
   get_property(PROPERTIES, length, LENGTH),
   string_integer(SLENGTH, LENGTH),
   get_slot(knowledgebase(_), menu_separator, SEPARATOR),
   string_atom(SFACT, FACT),
   get_property(PROPERTIES, default, DEFAULT),
   stringlist_concat([PROMPT, SEPARATOR, $<INPUT NAME="$, SFACT, $" SIZE=$,
      SLENGTH, $ VALUE="$, DEFAULT, $"></INPUT>$], QDOC).
% <TEXTAREA NAME=address ROWS=4 COLS=40>default</TEXTAREA>
format_question(FACT, PROPERTIES, QDOC) :-
   get_property(PROPERTIES, question_type, fill_in_the_box),
   !,
   get_property(PROPERTIES, prompt, PROMPT),
   get_property(PROPERTIES, length, LENGTH),
   get_property(PROPERTIES, height, HEIGHT),
   string_integer(SLENGTH, LENGTH),
   string_integer(SHEIGHT, HEIGHT),
   get_slot(knowledgebase(_), menu_separator, SEPARATOR),
   string_atom(SFACT, FACT),
   get_property(PROPERTIES, default, DEFAULT),
   stringlist_concat([PROMPT, SEPARATOR, $<TEXTAREA NAME="$, SFACT, $" COLS=$,
      SLENGTH, $ ROWS=$, SHEIGHT, $>$, DEFAULT, $</TEXTAREA>$], QDOC).
format_question(FACT, PROPERTIES, QDOC) :-
   get_property(PROPERTIES, question_type, QTYPE),
   get_property(PROPERTIES, question_style, QSTYLE),
   get_property(PROPERTIES, prompt, PROMPT),
   ((QTYPE == menu_single_choice_display_separate ; QTYPE == menu_multiple_choices_display_separate) ->
      get_property(PROPERTIES, 'rule-display_choices', [COLNAMES | CHOICES])
   ;
      get_property(PROPERTIES, choices, CHOICES) ),
   (sub_atom(QTYPE, I, _, 'single_choice') ->
      QTYPE2 = single_choice
   ;
      QTYPE2 = multiple_choices ),
   get_property(PROPERTIES, default, DEFAULT),
   get_slot(knowledgebase(_), menu_separator, SEPARATOR),
   string_atom(SFACT, FACT),
   format_choices(SFACT, CHOICES, DEFAULT, QTYPE2, QSTYLE, SEPARATOR, OUTLIST),
   append([PROMPT, SEPARATOR], OUTLIST, OUTLIST2),
   flaten(OUTLIST2, OUTLIST3),
   stringlist_concat(OUTLIST3, QDOC).

format_choices(FACT, CHOICES, DEFAULT, single_choice, checkbox_radio, SEPARATOR, OUTLIST) :-
   format_input_choices(CHOICES, FACT, DEFAULT, $RADIO$, SEPARATOR, OUTLIST).
format_choices(FACT, CHOICES, DEFAULT, single_choice, listbox, SEPARATOR, OUTLIST) :-
   format_list_choices(CHOICES, DEFAULT, SEPARATOR, CHOICELIST),
   append([$<SELECT NAME="$, FACT, $">$], CHOICELIST, LIST1),
   append(LIST1, [$</SELECT>$], LIST2),
   flaten(LIST2, OUTLIST).  
format_choices(FACT, CHOICES, DEFAULT, multiple_choices, checkbox_radio, SEPARATOR, OUTLIST) :-
   format_input_choices(CHOICES, FACT, DEFAULT, $CHECKBOX$, SEPARATOR, OUTLIST).
format_choices(FACT, CHOICES, DEFAULT, multiple_choices, listbox, SEPARATOR, OUTLIST) :-
   format_list_choices(CHOICES, DEFAULT, SEPARATOR, CHOICELIST),
   append([$<SELECT NAME="$, FACT, $" MULTIPLE>$], CHOICELIST, LIST1),
   append(LIST1, [$</SELECT>$], LIST2),
   flaten(LIST2, OUTLIST). 

format_input_choices([], _, _, _, _, []) :- !.
format_input_choices([ ROW | REST], FACT, DEFAULT, TYPE, SEPARATOR, [CHOICETEXT | REST2]) :-
   islist(ROW),
   nth_elem(ROW, CHVALUE, 1),
   nth_elem(ROW, CHTEXT, 2),
   (CHVALUE = DEFAULT ->
      stringlist_concat([$<INPUT CHECKED NAME="$, FACT, $" TYPE=$, TYPE, $ VALUE="$, CHVALUE, $">$, 
         CHTEXT, $</INPUT>$, SEPARATOR], CHOICETEXT)
   ;
      stringlist_concat([$<INPUT NAME="$, FACT, $" TYPE=$, TYPE, $ VALUE="$, CHVALUE, $">$, 
         CHTEXT, $</INPUT>$, SEPARATOR], CHOICETEXT)
   ),
   format_input_choices(REST, FACT, DEFAULT, TYPE, SEPARATOR, REST2).   
format_input_choices([CHOICE | REST], FACT, DEFAULT, TYPE, SEPARATOR, [CHOICETEXT | REST2]) :-
   (CHOICE = DEFAULT ->
      stringlist_concat([$<INPUT CHECKED NAME="$, FACT, $" TYPE=$, TYPE, $ VALUE="$, CHOICE, $">$, 
         CHOICE, $</INPUT>$, SEPARATOR], CHOICETEXT)
   ;
      stringlist_concat([$<INPUT NAME="$, FACT, $" TYPE=$, TYPE, $ VALUE="$, CHOICE, $">$, 
         CHOICE, $</INPUT>$, SEPARATOR], CHOICETEXT)
   ),
   format_input_choices(REST, FACT, DEFAULT, TYPE, SEPARATOR, REST2).   


format_list_choices([], _, _, []) :- !.
format_list_choices([ ROW | REST], DEFAULT, SEPARATOR, [CHOICETEXT | REST2]) :-
   islist(ROW),
   nth_elem(ROW, CHVALUE, 1),
   nth_elem(ROW, CHTEXT, 2),
   (CHOICE = DEFAULT ->
      stringlist_concat([$<OPTION SELECTED VALUE="$, CHVALUE, $">$, CHTEXT, $</OPTION>$], CHOICETEXT)
   ;
      stringlist_concat([$<OPTION VALUE="$, CHVALUE, $">$, CHTEXT, $</OPTION>$], CHOICETEXT)
   ),
   format_list_choices(REST, DEFAULT, SEPARATOR, REST2).   
format_list_choices([CHOICE | REST], DEFAULT, SEPARATOR, [CHOICETEXT | REST2]) :-
   (CHOICE = DEFAULT ->
      stringlist_concat([$<OPTION SELECTED VALUE="$, CHOICE, $">$, CHOICE, $</OPTION>$], CHOICETEXT)
   ;
      stringlist_concat([$<OPTION VALUE="$, CHOICE, $">$, CHOICE, $</OPTION>$], CHOICETEXT)
   ),
   format_list_choices(REST, DEFAULT, SEPARATOR, REST2).   

%------------------------------------------------
% format outputs
%

format_output(MORE, PROPS1, PROPS2) :-
   member(text = TEXT, PROPS1),
   get_slot(knowledgebase(_), output_top, TOP),
   format_add_parms(TOP, TOP1),
   % Some jigs don't have continue
   (get_slot(knowledgebase(_), output_continue, CON) ->
     format_add_parms(CON, CON1)
   ; CON1 = $$ ),
   get_slot(knowledgebase(_), output_bottom, BOT),
   format_add_parms(BOT, BOT0),
   insert_personal_footer(BOT0, BOT1),
   (MORE == true ->
      stringlist_concat([TOP1, TEXT, CON1, BOT1], HTML)
   ;  
      stringlist_concat([TOP1, TEXT, BOT1], HTML) ),
   replace_elem(text = _, text = HTML, PROPS1, PROPS2).
format_output(_, PROPS, PROPS).


%------------------------------------------------
% insert an ad for personal knowledgebases
%

insert_personal_footer(S, S) :-
   ( knowledgewright_license(professional, [serial = SERIAL]) ; knowledgewright_license(standard, [serial=SERIAL]) ),
   string(SERIAL),
   string_length(SERIAL, 23),
   string_atom(SERIAL, ASERIAL),
   atom_uplow(ASU, ASERIAL),
   string_atom(SU, ASU),
   ( sub_string(SU, 1, 2, $KW$) ; sub_string(SU, 1, 2, $KS$) ; sub_string(SU, 1, 2, $KG$) ),
   ( sub_string(SU, 3, 1, $X$) ; sub_string(SU, 3, 1, $S$) ; sub_string(SU, 3, 1, $E$) ),
   !.
insert_personal_footer(IN, OUT) :-
   string_atom(IN, AIN),
   atom_uplow(AINU, AIN),
   string_atom(INU, AINU),
   (once sub_string(INU, IDX, _, $</BODY>$) ; once string_length(IN, IDX) ),
   (IDX =< 1 -> START = $$ ; IDX2 is IDX -1, sub_string(IN, 1, IDX2, START) ),
   string_length(IN, LEN),
   LEN2 is LEN - IDX + 1,
   (IDX = LEN -> END = $$ ; sub_string(IN, IDX, LEN2, END) ),
   stringlist_concat([START, $<p></p><hr><center><i><font size=-1>Created with <a href="http://www.amzi.com">Amzi!'s KnowledgeWright&reg;</a></i></font></center>$, 
      END], OUT), !.
insert_personal_footer(S, S).

