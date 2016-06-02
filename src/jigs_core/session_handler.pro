%
% Session handling
%
% Copyright (c) Amzi! inc. 1996-2000
% All Rights Reserved
%

% A session is a data structure that holds the
% status/state of a consultation session. The idea
% is that there might be multiple consultation
% sessions happening at the same time.
%
% Predicates that deal with various parts of the
% reasoning process can change the state of the
% session.  The predicates in this module define
% the various update operations you can perform
% on a session.
%
% The implementation details of sessions can be
% changed without affecting other parts of an
% application.  For this implementation, the
% state is maintained primarily in attribute-value
% lists, with the exception of known, used for
% facts, which are stored in the dynamic database.
%
% By convention, the last argument of predicates
% that use sessions have either the session, or
% an input & output session joined by the > operator.
% for example: do_something(ARG1, ARG2, ..., S_IN > S_OUT).
%
% The session is then maintained on the heap, with
% all the associated advantages of that approach for
% backtracking and looping predicates.  The session_save
% and session_restore predicates keep that session in the
% dynamic database for safe keeping between invokations
% of a consultation session.

%------------------------------------------------
% CVS history
%
% $Log: session_handler.pro,v $
% Revision 1.1.1.1  2002/12/10 16:49:52  dennis
% Moved to jigs_core for Eclipse.
%
% Revision 1.28  2002/03/23 03:37:23  dennis
% Fixed bad initialization parameters bug in CGI.
%
% Revision 1.27  2002/03/17 19:52:16  mary
% Lots of small enhancements/bug fixes.
%
% Revision 1.26  2002/03/09 23:41:15  mary
% Put back unicode option on kb files. Sessions/logs are always unicode.
%
% Revision 1.25  2002/03/09 21:12:15  mary
% Fixed display of tables. Added row/expand all command.
%
% Revision 1.24  2002/02/22 18:27:48  dennis
% Add data_table and find() function. Rename table to rules_table.
%
% Revision 1.23  2002/02/21 04:39:14  dennis
% added data_table query support, used new :- include directive for
% operators in each file
%
% Revision 1.22  2002/02/15 20:03:51  mary
% Use Unicode for log and session files.
%
% Revision 1.21  2002/02/13 23:33:04  dennis
% Replaced date_time with library. Added date_format to knowledgebase.
%
% Revision 1.20  2002/01/29 21:56:03  mary
% Add operators for command line build. Other makefile nits.
%
% Revision 1.19  2002/01/26 19:19:56  mary
% For CGI interface, added KWI call to backup inferencing by removing knowns
% and resetting the goal list.
% Removed Unicode file format preference.
%
% Revision 1.18  2001/12/17 20:02:24  mary
% Fixed bug for resubmitting CGI forms. Other minor fixes.
%
% Revision 1.17  2001/11/30 01:05:14  mary
% Fix bug to include output_top, output_continue and output_bottom.
% Don't delete session file so the 'back' browser button works.
%
% Revision 1.16  2001/10/29 22:30:02  mary
% Various changes.
%
% Revision 1.15  2001/08/02 01:00:59  mary
% Final changes for 4.1.18.
%
% Revision 1.14  2001/05/10 15:14:52  mary
% Final touches on 4.1.16
%
% Revision 1.13  2001/01/12 20:55:16  mary
% Expire the product in June 2001.
%
% Revision 1.12  2001/01/05 18:37:00  mary
% Fixed bug to properly return value of more based on goal list.
% Fixed bug when a multi-valued fact has the value [] to return [].
%
% Revision 1.11  2001/01/05 16:15:45  mary
% Changes for debugging purposes.
%
% Revision 1.10  2000/12/14 05:27:08  dennis
% Added diagnose samples and early tutorial chapters,
% and fixed some bugs in reasoner, and improved tracing
% output.
%
% Revision 1.9  2000/12/12 05:51:16  mary
% Stop outputting hidden facts. Removed ?'s.
%
% Revision 1.8  2000/12/10 02:46:22  mary
% Took out hardwired \ appended to end of directory names.
%
% Revision 1.7  2000/12/07 04:01:00  dennis
% save sessions to disk
%
% Revision 1.6  2000/12/02 06:07:40  dennis
% some performance changes
%
% Revision 1.5  2000/12/02 04:57:27  dennis
% got dates and automatic fact conversion going
%
% Revision 1.4  2000/12/01 05:03:44  dennis
% added hidden fields, also conditions in a list
%
% Revision 1.3  2000/11/29 19:09:50  dennis
% Added CVS logging to source files
%
%
%------------------------------------------------

% Operators

:- include('kw_ops.pro').

%session_new(ID, session(ID, [])) :-
%   date(M, D, Y),
%   Y >= 2002,
%   M >= 1,
%   throw(error(product_expired, [message = $This beta version of KnowledgeWright has expired. Please visit www.amzi.com for an update.$])).
session_new(ID, session(ID, [])).

session_exists(ID) :-
   session(ID, _).

% Write the session file to disk if save_session is set on init
session_save(session(ID,A)) :-
   (get_init_parm(save_session, true) ; get_init_parm(save_session, yes)),
   retractall(session(ID, _)),
   asserta(session(ID,A)),
   session_file_name(ID, SFILE),
   open(SFILE, write, H, [type(wide_text)]),
   tell(H),
   session_write(ID),
   told.
session_save(session(ID,A)) :-
   retractall(session(ID, _)),
   asserta(session(ID,A)).

   session_write(ID) :-
      retract(session(ID,A)),
      writeq(session(ID,A)),
      write($.\n$),
      retract(known(ID,K)),
      writeq(known(ID,K)),
      write($.\n$),
      fail.
   session_write(_).

   session_file_name(ID, SFILE) :-
      get_init_parm(session_directory, DIR),
      stringlist_concat([DIR,ID,$.kws$], SFILE).

session_restore(ID, session(ID, ATTRS)) :-
   session(ID, ATTRS),
   !.
session_restore(ID, session(ID, ATTRS)) :-
   session_file_name(ID, SFILE),
   consult(SFILE),
   session(ID, ATTRS), !.
session_restore(ID, _) :-
   throw(error(unknown_session, [message = $Unable to find session$:ID])).

session_close(ID) :-
   retractall(session(ID, _)),
   retractall(known(ID, _)),
   session_file_name(ID, SFILE),
   (delfile(SFILE,_) -> true; true).


%
% updating session information
% an session is an a-v pair, except for 'known'
% which is stored in dynamic db.
%

% add something to the session list

session_add(A < V, S1 > S2) :-
   session_add(A < V, indent(0), S1 > S2).

session_add(A < V, indent(L), S1>S2) :-
   log_trace(3-L, [$Session: adding $, A=V]),
   fail.
session_add(known < V, indent(L), session(S, AV) > session(S, AV)) :-
   session_add_format(S, V, VF),
   !.
session_add(A < V, _, session(I, AL1) > session(I, [A=VL2|ALx])) :-
   remove(A=VL, AL1, ALx),
   !,
   (member(V, VL) -> VL2 = VL; VL2 = [V|VL]).
session_add(A < V, _, S1 > S2) :-
   session_set(A = [V], S1 > S2).

% an add might be from CGI or someplace that forgot
% what type the answer should be, as specified in
% the question, so we double check here.

session_add_format(S, fact(Q,V), fact(Q,VF)) :-
   question_answer_type(Q, _, multiple),
   !,
   question_format_answer(Q,V,VF),
   (islist(VF) ->
      session_add_known(S, fact(Q,VF))
      ;
      (known(S, fact(Q,_)) ->
         session_add_known(S, fact(Q,VF))
         ;
         session_add_known(S, fact(Q, [VF])) ) ).
session_add_format(S, fact(Q,V), fact(Q,VF)) :-
   question_format_answer(Q, V, VF),
   !, session_add_known(S, fact(Q, VF)).
session_add_format(S, fact(Q,V), fact(Q,V)) :-
   session_add_known(S, fact(Q,V)).

session_add_known(S, fact(F,I)) :-
   known(S, fact(F,L)),
   islist(L), !,
   retract(known(S, fact(F,L))),
   assertz(known(S, fact(F,[I|L]))).
session_add_known(S, V) :-
   known(S,V), !.
session_add_known(S, fact(F,V)) :-
   known(S,fact(F,X)),
   throw(error(changing_known_value, [
      message = $Cannot change the value of a known fact.$,
      current_value = X,
      new_value = V ])).
session_add_known(S, V) :-
   assertz(known(S,V)).

session_extract_item(A < X, S1>S2, true) :-
   session_get(A = [X|Y], S1),
   session_del(A < X, S1>S2).
session_extract_item(_, I>I, false).
   
% Truncate the known list by removing the named fact and all
% subsequent knowns
session_truncate_knowns(FACT, session(S, AV) > session(S, AV), RETRACTED) :-
   findall(known(S,X), known(S,X), L),
   append(_, [known(S,FACT)|Z], L),
   !,
   RETRACTED = true,
   retract_knowns([known(S,FACT)|Z]).
session_truncate_knowns(FACT, session(S, AV) > session(S, AV), false).

   retract_knowns([]).
   retract_knowns([K|Z]) :-
      retract(K),
      retract_knowns(Z).

% delete something from an session

session_del(A < V, S1 > S2) :-
   session_del(A < V, indent(0), S1 > S2).

session_del(A < V, indent(L), S1 > S2) :-
   log_trace(3-L, [$Session: deleting $, A=V]),
   fail.
session_del(known < V, _, session(I, AV) > session(I, AV)) :-
   retract(known(I, V)),
   !.
session_del(A < V, _, session(I, AL1) > session(I, AL2)) :-
   remove(A=VL, AL1, ALx),
   !,
   remove(V, VL, VL2),
   AL2 = [A=VL2|ALx].
session_del(A < V, _, S1>S2) :-
   throw(error(session_delete_fails,
     [message = $request to delete non-exisitent element from session attribute$,
      attribute = A,
      element = V]) ).

% find an element in an attribute list, allow for backtracking
% for multivalued and/or similar facts.

session_find(known < V, session(S, _), true) :-
   known(S, V).
session_find(known < V, session(S, _), true) :-
   V =.. [DATATYPE, NAME, VALUE],
   PRED =.. [DATATYPE, NAME],
   get_slot(PRED, data, VALUE).
session_find(A < V, session(S, AL), true) :-
   not A=known,
   member(A=VL, AL),
   member(V,VL).
session_find(_, _, false).

session_is_known(V, session(S, _)) :-
   known(S, V), !.

% get and set an session attribute

session_get(known = KNOWNS, session(S, _)) :-
   !, findall(V, known(S,V), KNOWNS).
session_get(A = V, session(S, AL)) :-
   member(A=V, AL), !.

session_set(A = V, indent(L), S1 > S2) :-
   log_trace(3-L, [$Setting $, A = V]),
   session_set(A = V, S1 > S2).

session_set(A = V, session(S, AL) > session(S, [A=V|AL2])) :-
   (remove(A=_, AL, AL2); AL2=AL),
   !.

session_remove(A = V, session(S, AL) > session(S, AL2)) :-
   remove(A=V, AL, AL2),
   !.
session_remove(A = V, S>S).

session_log(ID) :-
   session_restore(ID, session(ID, STATE)),
   log_trace(4-0, [nl, $--- Session Status ---$]),
   log_trace(4-0, 'State: '),  % NOT $State, which is CVS keyword
   session_report_details(STATE),
   log_trace(4-0, $Known:$),
   session_report_known(ID),
   log_trace(4-0, $$),
   !.
session_log(ID).

   session_report_details([]).
   session_report_details([ITEM|REST]) :-
      session_report_detail(ITEM),
      !, session_report_details(REST).

   session_report_detail(X) :- log_trace(4-1, X).

   session_report_known(ID) :-
      known(ID, V),
      log_trace(4-1, V),
      fail.
   session_report_known(_).

% package a session for sending back to user
% for display

session_package(ID, [known=KLIST|IATTRS]) :-
   session_restore(ID, session(ID, IATTRS)),
   findall(K, known(ID,K), KLIST).
