%------------------------------------------------
% CVS history
%
% $Log: kwi.pro,v $
% Revision 1.1.1.1  2002/12/10 16:49:52  dennis
% Moved to jigs_core for Eclipse.
%
% Revision 1.26  2002/04/22 20:41:43  dennis
% Fixed bug comparing a fact = [list].
%
% Revision 1.25  2002/03/22 19:57:10  dennis
% Small enhancements.
%
% Revision 1.24  2002/03/17 19:52:16  mary
% Lots of small enhancements/bug fixes.
%
% Revision 1.23  2002/02/22 18:27:48  dennis
% Add data_table and find() function. Rename table to rules_table.
%
% Revision 1.22  2002/02/21 04:39:14  dennis
% added data_table query support, used new :- include directive for
% operators in each file
%
% Revision 1.21  2002/02/14 20:36:19  dennis
% Jiggle number flags.
%
% Revision 1.20  2002/02/13 23:33:04  dennis
% Replaced date_time with library. Added date_format to knowledgebase.
%
% Revision 1.19  2002/01/29 21:56:03  mary
% Add operators for command line build. Other makefile nits.
%
% Revision 1.18  2002/01/28 18:07:37  mary
% Take out debugging lines.
%
% Revision 1.17  2002/01/28 18:06:42  mary
% Minor changes.
%
% Revision 1.16  2002/01/26 19:19:56  mary
% For CGI interface, added KWI call to backup inferencing by removing knowns
% and resetting the goal list.
% Removed Unicode file format preference.
%
% Revision 1.15  2001/12/23 19:50:02  mary
% Move environment to portable.
%
% Revision 1.14  2001/12/17 20:02:24  mary
% Fixed bug for resubmitting CGI forms. Other minor fixes.
%
% Revision 1.13  2001/04/22 10:33:51  mary
% Fixed backslashes in pathnames for Windows 98 again.
%
% Revision 1.12  2001/04/20 23:13:18  mary
% Bug fixes for 4.1.14
%
% Revision 1.11  2001/02/28 05:26:14  mary
% Added primary_questions and prompt vars to joblook. Fixed comments in basic.
%
% Revision 1.10  2001/02/14 17:57:44  mary
% Changed to simple scrolling so that text will not be garbled (Java bug).
%
% Revision 1.9  2001/01/24 19:36:41  mary
% Fixed default value for basic jig question top + bottom.
% Small edits to sales jig.
%
% Revision 1.8  2000/12/14 05:27:08  dennis
% Added diagnose samples and early tutorial chapters,
% and fixed some bugs in reasoner, and improved tracing
% output.
%
% Revision 1.7  2000/12/12 06:15:16  dennis
% fixed variety of xref bugs
%
% Revision 1.6  2000/12/10 01:54:28  dennis
% added debugging stuff
%
% Revision 1.5  2000/12/08 23:53:19  dennis
% added debug to file option
%
% Revision 1.4  2000/12/07 04:01:00  dennis
% save sessions to disk
%
% Revision 1.3  2000/11/29 19:09:50  dennis
% Added CVS logging to source files
%
%
%------------------------------------------------

% Operators

:- include('kw_ops.pro').

% Libraries

:- import(list).
:- import(aodbc).
:- import(acgi).

%-----------------------------
% Logic Base Interface
%

rlb(IN_XML, OUT_XML) :-
   xml_to_input(IN_XML, INPUT),
   process(INPUT, OUTPUT),
   output_to_xml(OUTPUT, OUT_XML).

% initialize, note that can't use 'do' here
% because log not initialized yet.

kwi( ID, initialize(INIT_LIST), INFO_LIST ) :-
   catch(
      (
         set_prolog_flag(upper_case_atoms, on),
         set_prolog_flag(double_quote_strings, on),
         set_prolog_flag(decimals, float),	% float or real
         set_prolog_flag(floats, single),	% single or double
% The following line breaks Win98!!!
%         set_mode(string_esc, off),
         lb_initialize(ID, INIT_LIST, INFO_LIST)
      ),
      ERROR,
      (
         error_set(ID, ERROR),
         log_save,
         throw(kwi_error(ID))
      ) ),
   !, log_save.

kwi( ID, new_session, [] ) :-
   do(ID,
      (
         lb_new_session(ID)
      ) ).

kwi( ID, assert(NEW_FACTS), [] ) :-
   do(ID,
      (
         lb_assert(ID, NEW_FACTS)
      ) ).

kwi( ID, backup(FACT), SUCCESS ) :-
   do(ID,
      (
         lb_backup(ID, FACT, SUCCESS)
      ) ).

kwi( ID, solve, DONE ) :-
   do(ID,
      (
         lb_resolve(ID, DONE)
      ) ).

kwi( ID, get_action, ACTION ) :-
   do(ID,
      (
         lb_get_action(ID, ACTION)
      ) ).

kwi( ID, open(LB_FILE), _ ) :-
   do(ID,
      (
         lb_open(ID, LB_FILE)
      ) ).

kwi( ID, close, [] ) :-
   catch(
      (
         log_open,
         lb_close(ID),
         report_times
      ),
      ERROR,
      (
         error_set(ID, ERROR),
         log_save,
         throw(kwi_error(ID))
      ) ),
   !, log_save.

kwi( ID, get_parm(FRAME, INSTANCE, SLOT), VALUE ) :-
   do(ID,
      (
         lb_get_parm(ID, FRAME, INSTANCE, SLOT, VALUE)
      ) ).

kwi( ID, get_session, IP ) :-
   do(ID,
      (
         lb_get_session(ID, IP)
      ) ).

kwi( ID, get_error, ERROR ) :-
   error_get(ID, ERROR).

do(ID, FUNCTIONS) :-
   catch(
      (
         log_open,
         T1 is cpuclock,
         call(FUNCTIONS),
         ( FUNCTIONS = lb_get_session(_, _) ->
            true
            ;
            T is cpuclock - T1,
            functor(FUNCTIONS, NAME, _),
            ( time_total(NAME, TAMT) ->
               retractall(time_total(NAME, _)),
               T2 is TAMT + T,
               asserta(time_total(NAME, T2))
            ;
               asserta(time_total(NAME, T))
            ),

            log(4, [nl, $--- KWI Call: $]),
            log(4, [session:ID, nl, $    $]),
            log(4, [FUNCTIONS, nl]),
            log(4, [time = T, nl]),
            lb_log_session(ID) 
         )
      ),
      ERROR,
      (
         error_set(ID, ERROR),
         (session_exists(ID) ->
            lb_log_session(ID)
            ;
            true),
         log_save,
         throw(kwi_error(ID))
      ) ),
   !,
   log_save.
do(ID, FUNCTIONS) :-
   error_set(ID, error(kwi_failure, [
          message = $Logic Base function failed to complete.$,
          functions = FUNCTIONS]) ),
   log_save,
   throw(kwi_error(ID)).


report_times :-
   log(3, [nl, $KWI Timing Totals:$, nl]),
   time_total(NAME, TIME),
   log(3, [$   $, NAME = TIME, nl]),
   fail.
report_times.
