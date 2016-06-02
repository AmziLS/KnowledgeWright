%------------------------------------------------
% CVS history
%
% $Log: logging.pro,v $
% Revision 1.1.1.1  2002/12/10 16:49:52  dennis
% Moved to jigs_core for Eclipse.
%
% Revision 1.26  2002/08/15 16:33:38  mary
% Don't call substring with bad arguments. Added log flushing (commented out).
%
% Revision 1.25  2002/05/18 15:01:51  dennis
% Remove duplicates of system predicates
%
% Revision 1.24  2002/03/23 03:37:23  dennis
% Fixed bad initialization parameters bug in CGI.
%
% Revision 1.23  2002/03/17 19:52:16  mary
% Lots of small enhancements/bug fixes.
%
% Revision 1.22  2002/03/09 23:41:15  mary
% Put back unicode option on kb files. Sessions/logs are always unicode.
%
% Revision 1.21  2002/03/09 21:12:15  mary
% Fixed display of tables. Added row/expand all command.
%
% Revision 1.20  2002/02/22 18:27:48  dennis
% Add data_table and find() function. Rename table to rules_table.
%
% Revision 1.19  2002/02/21 04:39:14  dennis
% added data_table query support, used new :- include directive for
% operators in each file
%
% Revision 1.18  2002/02/15 20:03:51  mary
% Use Unicode for log and session files.
%
% Revision 1.17  2002/01/30 16:44:25  mary
% Took out debugging cgiSends.
%
% Revision 1.16  2002/01/30 16:39:46  dennis
% Created makefiles for Linux
%
% Revision 1.15  2002/01/29 21:56:03  mary
% Add operators for command line build. Other makefile nits.
%
% Revision 1.14  2001/12/23 19:50:02  mary
% Move environment to portable.
%
% Revision 1.13  2001/02/12 22:08:45  mary
% Unified all the jigs. Fixed question order for ask also. Moved log file
% to session_directory.
%
% Revision 1.12  2001/01/19 03:48:25  mary
% Throw error if bad message_level is provided.
% Fix sales to handle failure on a benefit properly and format questions
% only once.
%
% Revision 1.11  2000/12/14 05:27:08  dennis
% Added diagnose samples and early tutorial chapters,
% and fixed some bugs in reasoner, and improved tracing
% output.
%
% Revision 1.10  2000/12/10 02:46:22  mary
% Took out hardwired \ appended to end of directory names.
%
% Revision 1.9  2000/12/08 23:59:05  dennis
% fixed debug bug
%
% Revision 1.8  2000/12/08 23:53:19  dennis
% added debug to file option
%
% Revision 1.7  2000/12/07 04:01:00  dennis
% save sessions to disk
%
% Revision 1.6  2000/12/05 19:43:21  mary
% Take out debugging stuff.
%
% Revision 1.5  2000/12/04 19:27:31  mary
% Merge in Mary's changes.
%
% Revision 1.4  2000/12/04 03:11:23  dennis
% added parm checking to question bottoms as well
%
% Revision 1.3  2000/12/02 04:57:27  dennis
% got dates and automatic fact conversion going
%
% Revision 1.2  2000/11/29 19:09:50  dennis
% Added CVS logging to source files
%
%
%------------------------------------------------

% Operators

:- include('kw_ops.pro').

log_message_level(none, 0).
log_message_level(low, 1).
log_message_level(medium, 2).
log_message_level(high, 3).
log_message_level(max, 4).
log_message_level(X, _) :-
   throw(error(bad_message_level, [
      message = $Invalid log message level$,
      level = X])).

log_message_default(none).

log_initialize :-
   % If no logging, we're done
   initial_parms(IPARMS),
   ( get_property(IPARMS, message_level, ML) ->
        true
        ;
        log_message_default(ML) ),
   ML == none, !.
log_initialize :-
   initial_parms(IPARMS),
   ( get_property(IPARMS, message_level, ML) ->
        true
        ;
        log_message_default(ML) ),
   once log_message_level(ML, ML_NUMBER),

   % Debugging may require log_file
   debug_initialize,

   % Open the log file
   get_property(IPARMS, log_file, LOGNAME),
   get_property(IPARMS, session_directory, DIRECTORY),
   stringlist_concat([DIRECTORY, LOGNAME], LOGFILE),
   assert(logfile(LOGFILE, ML_NUMBER)),
   open(LOGFILE, write, H, [type(wide_text)]),
%   open(LOGFILE, write, H),
   assert(loghandle(H, ML_NUMBER)),
   log(2, $Log Open: $),
   date(DY,DM,DD), time(TH,TM,TS),
   log(2, [DY/DM/DD, tab(2), TH:TM:TS, nl, nl]),
   (system_name(SYSNAME) -> log(2, SYSNAME); true),
   (system_version(SYSVER) -> log(2, [tab(2), SYSVER, nl, nl]); true),
   log(3, [$Initial parameters: $, $\n  $]),
   log_list(3, IPARMS, $\n  $),
   log(3, nl), log(3, nl).
log_initialize :-
   initial_parms(IPARMS),
   throw(error(bad_initialization_parameters, [
      message = $Invalid initialization parameters or unable to open log file$,
      parameters = IPARMS])).

log_close :-
   log_save,
   log(3, [$--- closing Log ---$, nl, nl]),
   retract(logfile(_)).
log_close.
   
log_open :-
   logfile(LOGFILE, MSG_LEVEL),
   !,
   open(LOGFILE, append, H, [type(wide_text)]),
%   open(LOGFILE, append, H),
   retractall(loghandle(_,_)),
   assert(loghandle(H, MSG_LEVEL)).
log_open.

log_save :-
   log(4, [nl, $****** Saving Log ******$, nl, nl]),
   retract(loghandle(H,_)),
   !,
   close(H).
log_save.

log_flush :- !.
%log_flush :-
%   retract(loghandle(H,_)),
%   close(H),
%   logfile(LOGFILE, MSG_LEVEL),
%   !,
%   open(LOGFILE, append, H2, [type(wide_text)]),
%   assert(loghandle(H2, MSG_LEVEL)).
log_flush.

% Logging is controlled both by indent and
% message level.  A negative indent represses
% messages.  This overloaded use of indentation
% is used because it makes it easy for code to
% suppress logging for an entire tree of reasoning.

log(LEVEL, X) :-
   loghandle(H, MSG_LEVEL),
   LEVEL =< MSG_LEVEL,
   logwrite(H,X),
   !.
log(_, _).

log_trace(LEVEL-INDENT, X) :-
   loghandle(H, MSG_LEVEL),
   LEVEL =< MSG_LEVEL,
   INDENT >= 0,
   log_indent(H, INDENT),
   logwrite(H, X),
   logwrite(H, nl),
   !.
log_trace(_, _).

log_list(LEVEL, X, SEP) :-
   loghandle(H, MSG_LEVEL),
   LEVEL =< MSG_LEVEL,
   !,
   logwrite_list(H, X, SEP).
log_list(_, _, _).


% negative levels mean don't print.

log_inc_indent(L, LL) :-
   (L >= 0 ->
      LL is L + 1
      ;
      LL = L).

log_indent(_,0) :- !.
log_indent(H,L) :-
   logwrite(H,tab(2)),
   LL is L - 1,
   !, log_indent(H,LL).

logwrite(H, X) :- var(X), !, write(H, var(X)), log_flush.
logwrite(_, []) :- !.
logwrite(H, [X|Y]) :- ln(H,X), !, logwrite(H,Y).
logwrite(H, nl) :- nl(H), log_flush.
logwrite(H, sp) :- write(H, $ $), log_flush.
logwrite(H, tab(N)) :- tab(H,N), log_flush.
logwrite(H, X) :- ln(H,X), log_flush.

ln(H, X) :- var(X), !, write(H, var(X)).
ln(H, nl) :- nl(H).
ln(H, sp) :- write(H, $ $).
ln(H, tab(N)) :- tab(H,N).
ln(H, A:B) :- ln(H,A), ln(H,$: $), ln(H,B).
ln(H, X) :- (list(X);structure(X)), !, writeq(H,X).
ln(H, X) :- write(H,X).

logwrite_list(H, [], _) :- logwrite(H,$Empty List$), !.
logwrite_list(H, [X], _) :- logwrite(H,X), !.
logwrite_list(H, [X|Y], SEP) :-
   logwrite(H,[X, SEP]),
   !, logwrite_list(H, Y, SEP).
logwrite_list(H, X) :-
   logwrite(H, X).

%----------------------------------------
% Debugging output for ? and other
% bug.plm features
%

debug_initialize :-
   debug_output(_), !.
debug_initialize :-
   get_init_parm(debug_msgs, DM),
   debug_init(DM).
debug_initialize.

debug_init(local) :- !, asserta(debug_output(local)).
debug_init(logfile) :- 
   !,  asserta(debug_output(logfile)),
   % Make sure a log_file was specified
   (get_init_parm(log_file, _) -> true ;
      throw(error(missing_log_file, [
         message = $Must specify log_file with debug msgs = logfile$])) ).
debug_init(log_file) :- 
   !,  asserta(debug_output(logfile)),
   % Make sure a log_file was specified
   (get_init_parm(log_file, _) -> true ;
      throw(error(missing_log_file, [
         message = $Must specify log_file with debug msgs = log_file$])) ).
debug_init(FILE) :-
   fopen(H, FILE, w),
   date(MON,DAY,YEAR),
   time(HOUR,MIN,SEC),
   write(H, $Debug Output - $),
   write(H, MON/DAY/YEAR), tab(H, 2),
   write(H, HOUR:MIN:SEC), nl(H), nl(H),
   asserta(debug_output(file(H))).
debug_init(X) :-
   throw(error(bad_debug_file, [
      message = $Error opening debug_msgs file$,
      file = X ])).

user_bugwrite(T) :-
   debug_output(logfile),
   logfile(_,_),
   !,
   log(4,'*'), log(4,T), log(4,nl).
user_bugwrite(T) :-
   debug_output(local),
   !,
   write(T), nl.
user_bugwrite(T) :-
   debug_output(file(H)),
   !,
   write(H,T), nl(H).
user_bugwrite(T) :-
   write(T), nl.

