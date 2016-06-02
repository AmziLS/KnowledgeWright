%------------------------------------------------
% CVS history
%
% $Log: exception.pro,v $
% Revision 1.1.1.1  2002/12/10 16:49:52  dennis
% Moved to jigs_core for Eclipse.
%
% Revision 1.6  2002/02/22 18:27:48  dennis
% Add data_table and find() function. Rename table to rules_table.
%
% Revision 1.5  2002/02/21 04:39:14  dennis
% added data_table query support, used new :- include directive for
% operators in each file
%
% Revision 1.4  2002/01/29 21:56:03  mary
% Add operators for command line build. Other makefile nits.
%
% Revision 1.3  2000/12/14 05:27:08  dennis
% Added diagnose samples and early tutorial chapters,
% and fixed some bugs in reasoner, and improved tracing
% output.
%
% Revision 1.2  2000/11/29 19:09:50  dennis
% Added CVS logging to source files
%
%
%------------------------------------------------

% Operators

:- include('kw_ops.pro').


%----------
% Error
%

error_set(SID, error(Err, Attrs)) :-
   !,
   asserta(last_error(SID, [error = Err|Attrs])),
   log(1, [nl, $*** Error ***\n   $]),
   log_list(1, [error=Err|Attrs], $\n   $),
   log(1, [nl, $***$, nl]).
error_set(SID, X) :-
   asserta(last_error(SID, [error = unknown|X])),
   log(1, [nl, $*** Error ***\n   $, nl]),
   log_list(0, [error = unknown|X], $\n   $),
   log(1, [nl, $***$, nl]).

error_get(SID, ERROR) :-
   last_error(SID, ERROR),
   !.
error_get(SID, [message = $No error info available.$]).

