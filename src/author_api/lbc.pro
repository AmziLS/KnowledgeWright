%
% LBC - WebLS Logic Base Compiler
%   Copyright 1997 Amzi! inc.
%
% Bugs:
%   Must use full pathnames for xcompile and the temp file
%   Must declare answer$1 etc as discontiguous
%

:- discontiguous question/2, rule/3, answer/3, note/2, header/2, footer/2, module/2.
:- multifile question/2, rule/3, answer/3, note/2, header/2, footer/2, module/2.

weblsVersion($WebLS&#153; 3.2.0 Aug00$).
weblsCopyright($Copyright &copy;1996-2000 <A HREF="http://www.amzi.com">Amzi! inc.</A> All Rights Reserved.$).

% -------------------------- WebLS Operators ------------------------ %

:- op(900, fy, [?, bug]).

:- op(790, fx, if).     	% prefix operator
:- op(780, xfx, then).  	% infix operator
:- op(775, xfy, or).    	% infix that can be linked
:- op(770, xfy, and).   	% infix that can be linked
:- op(700, xfx, <=).		% infix operator
:- op(700, xfx, include).	% infix operator
:- op(700, xfx, exclude).	% infix operator
:- op(700, xfx, include_only).	% infix operator
:- op(700, xfx, number_of_items).	% infix operator
:- op(700, xfx, is).		% infix operator
:- op(700, xf, is_known).	% postfix operator


cgiLoad :-
%  asserta(system('Log File', 'F:\\Netscape\\SuiteSpot\\docs\\trace.html')),
%  asserta(system('Log File URL', '/trace.html')),
  true.

cgiMain :-
  cgi(request_method, RM),
  processMethod(RM).
cgiMain :-
  throw($cgiMain failed\n$).

processMethod('GET') :-
  sendHeader,

  % Load the main LB file (passed as a query string parameter)
  cgi(query_string, QStr),
  lbLoad(QStr, _),

  % Pass it again to the POST portion
  cgi(script_name, EP),
  cgiSend([$<FORM METHOD=POST ACTION="$, EP, $?$, QStr, $">$]),

  % Display all the LB file names from the modules
  findall(Mod, getModule(Mod), ModList0),
  string_atom(QStr, LBNameA),
  replace_elem(main, LBNameA, ModList0, ModList),
  getFiles(ModList, FileList),
  cgiAskMenu($lbFile$, $Select the name of the logic-base to compile:<P>$, FileList, checkbox, _, $<BR>$),

  cgiSend($<P><INPUT NAME="submit" TYPE="Submit" VALUE="Compile"></FORM>$),
  cgiSend($<P><I>Please wait...this may take a few minutes...</I>$),
  sendFooter.
processMethod('GET') :-
  throw($ERROR: Unable to load the logic-base specified as a query string parameter$).


processMethod('POST') :-
  sendHeader,
  version(AmziVer),
  cgiSend([$<P>$, AmziVer]),

  % Load the main LB file (passed as a query string parameter)
  once cgi(query_string, QStr),
  once lbLoad(QStr, _),

  % Put the temp file in the run directory
  once (getModule(main, runDirectory, RunDir) -> true ;
    throw($ERROR: runDirectory= is not defined in module main$) ),
  once atom_concat(RunDir, 'lbctemp.pro', TempFile),

  % Compile all the checked files
  compile_files(TempFile).

compile_files(TempFile) :-
  fact(lbFile, LBFile),

  % Get the module name from the .lb name
  once cgiExtractFileName(LBFile, LBFileOnly),
  once string_atom(LBFileS, LBFileOnly),
  once string_split(LBFileS, $.$, NameList),
  once [ModuleT | _] = NameList,
  once stringlist_concat([ModuleT, $.EXE$], EXEName),
  once ((file_exists(ModuleT) ; file_exists(EXEName)) -> Module = main ; Module = ModuleT),

  % Get the EXE file from the .lb name
  cgiExtractPath(TempFile, RunPath),
  cgiExtractFileName(LBFile, LBFileOnly),
  atomlist_concat([RunPath, LBFileOnly, '.lbc'], LBCFile),

  compile_one(Module, LBFile, LBCFile, TempFile),
  fail.
compile_files(TempFile) :-
  cgiSend('Done!').

compile_one(Module, LBFile, LBCFile, TempFile) :-
  cgiSend([$<P>Compiling $, LBFile]),
  T1 is cputime,
  create_tempfile(Module, LBFile, TempFile),
  T2 is cputime,
  TTEMPFILE is T2 - T1,
  compile_it(LBCFile, TempFile),
  T3 is cputime,
  TCOMPILE is T3 - T2,
  TALL is T3 - T1,
  cgiSend([$<P>Successfully Compiled $, LBFile, $ time(seconds) = $, TALL, $<P>$]),
  delfile(TempFile, _),
  !.

compile_it(LBCFile, TempFile) :-
  xcompile(TempFile, LBCFile, null).

% report is required by xcompile!
report(X) :-
  cgiLog(X).

getFiles([], []).
getFiles([Mod | ModRest], [Path | PathRest]) :-
  getModule(Mod, file, File),
  getModule(main, sourceDirectory, Dir), 
  atomlist_concat([Dir, File, '.lb'], Path),
  getFiles(ModRest, PathRest).
% Must be the main file, use the same name for the file
getFiles([Mod | ModRest], [Path | PathRest]) :-
  getModule(main, sourceDirectory, Dir),
  atomlist_concat([Dir, Mod, '.lb'], Path),
  getFiles(ModRest, PathRest).
getFiles(_, _) :-
  throw($ERROR: sourceDirectory= is not defined in module main$).

sendHeader :-
  cgiSend($Content-type: text/html$),
  cgiSend($$),
  cgiSend($<HTML><HEAD><TITLE>WebLS Logic-Base Compiler</TITLE></HEAD><BODY bgcolor=#FFFFFF text=#000000>$),
  cgiSend($<H2><FONT COLOR=blue>Web</FONT><FONT COLOR=magenta>LS</FONT> Logic-Base Compiler</H2><PRE>$).
sendFooter :-
  weblsVersion(Version),
  weblsCopyright(Copyright),
  cgiSend([$</PRE><P><HR><BR><CENTER><FONT SIZE=-1>$, Version, $<BR>$, Copyright, $</FONT></CENTER>$]),
  cgiSend($</BODY></HTML>$).

create_tempfile(MODULE, LBFILE, TEMPFILE) :-
  fopen(HIN, LBFILE, r),
  fopen(HOUT, TEMPFILE, w),

  write(HOUT, $:- discontiguous module/2, question/3, rule/4, answer/4, header/3, footer/3, note/3.\n$),
  write(HOUT, $:- discontiguous module\$1/2, module\$2/2, module\$3/2, module\$4/2, module\$5/2.\n$),
  write(HOUT, $:- discontiguous question\$1/3, question\$2/3, question\$3/3, question\$4/3, question\$5/3.\n$),
  write(HOUT, $:- discontiguous rule\$1/4, rule\$2/4, rule\$3/4, rule\$4/4, rule\$5/4.\n$),
  write(HOUT, $:- discontiguous answer\$1/4, answer\$2/4, answer\$3/4, answer\$4/4, answer\$5/4.\n$),
  write(HOUT, $:- discontiguous header\$1/3, header\$2/3, header\$3/3, header\$4/3, header\$5/3.\n$),
  write(HOUT, $:- discontiguous footer\$1/3, footer\$2/3, footer\$3/3, footer\$4/3, footer\$5/3.\n$),
  write(HOUT, $:- discontiguous note\$1/3, note\$2/3, note\$3/3, note\$4/3, note\$5/3.\n$),

  % Pathnames get messed up if escape processing is on
  set_mode(string_esc, off),

  cntr_set(1,0), % questions
  cntr_set(2,0), % rules
  cntr_set(3,0), % answers
  cntr_set(4,0), % notes
  cntr_set(5,0), % headers
  cntr_set(6,0), % footers
  cntr_set(7,0), % modules

  init_gensym('question$', Q1),
  init_gensym('rule$', R1),
  init_gensym('answer$', A1),
  init_gensym('note$', N1),
  init_gensym('header$', H1),
  init_gensym('footer$', F1),
  init_gensym('module$', M1),
  retractall(functors(_,_)),
  assert(functors(1, [Q1])),
  assert(functors(2, [R1])),
  assert(functors(3, [A1])),
  assert(functors(4, [N1])),
  assert(functors(5, [H1])),
  assert(functors(6, [F1])),
  assert(functors(7, [M1])),
  repeat,
  read(HIN, X),
  once output(X, MODULE, HOUT),
  X == 'end_of_file',
  !,
  masters(HOUT),
  fclose(HIN),
  fclose(HOUT),
  set_mode(string_esc, on).

/* Kludge to reset gensym counter */
init_gensym(ROOT, SYM) :-
  retractall( '{sys}current_num'(ROOT,_) ),
  gensym(ROOT, SYM).

output('end_of_file', _, _) :- !.
output(question(ATTR, LIST), LB, H) :-
  !,
  get_functor(1, 'question$', QF),
  Q =.. [QF, ATTR, LB, LIST],
  writeq(H, Q),
  write(H, $.\n$).
output(if CONDS then ATTR = VAL, LB, H) :-
  !,
  get_functor(2, 'rule$', RF),
  R =.. [RF, ATTR, LB, VAL, CONDS],
  writeq(H, R),
  write(H, $.\n$).
output(answer(VAL, GOAL, LIST), LB, H) :-
  !,
  get_functor(3, 'answer$', AF),
  A =.. [AF, VAL, LB, GOAL, LIST],
  writeq(H, A),
  write(H, $.\n$).
output(note(VAL, LIST), LB, H) :-
  !,
  get_functor(4, 'note$', NF),
  N =.. [NF, VAL, LB, LIST],
  writeq(H, N),
  write(H, $.\n$).
output(header(VAL, LIST), LB, H) :-
  !,
  get_functor(5, 'header$', HF),
  HH =.. [HF, VAL, LB, LIST],
  writeq(H, HH),
  write(H, $.\n$).
output(footer(VAL, LIST), LB, H) :-
  !,
  get_functor(6, 'footer$', FF),
  F =.. [FF, VAL, LB, LIST],
  writeq(H, F),
  write(H, $.\n$).
output(module(VAL, LIST), LB, H) :-
  !,
  get_functor(7, 'module$', MF),
  M =.. [MF, VAL, LIST],
  writeq(H, M),
  write(H, $.\n$).
  
/* LB files can have large numbers of rules, more than
   the compiler/loader can handle, so this logic breaks
   each of the functors (question, rule, answer) into
   chunks of 100 clauses each. */

get_functor(IFunctor, FRoot, Functor) :-
  cntr_inc(IFunctor, I),
  I < 100,
  !,
  functors(IFunctor, [Functor|_]).
get_functor(IFunctor, FRoot, Functor) :-
  cntr_set(IFunctor, 0),
  gensym(FRoot, Functor),
  retract(functors(IFunctor, L)),
  assert(functors(IFunctor, [Functor|L])).

masters(H) :-
  functors(1, QFsR),
  reverse(QFsR, QFs),
  master_questions(H, QFs),
  functors(2, RFsR),
  reverse(RFsR, RFs),
  master_rules(H, RFs),
  functors(3, AFsR),
  reverse(AFsR, AFs),
  master_answers(H, AFs),
  functors(4, NFsR),
  reverse(NFsR, NFs),
  master_notes(H, NFs),
  functors(5, HFsR),
  reverse(HFsR, HFs),
  master_headers(H, HFs),
  functors(6, FFsR),
  reverse(FFsR, FFs),
  master_footers(H, FFs),
  functors(7, MFsR),
  reverse(MFsR, MFs),
  master_modules(H, MFs).

master_questions(H, []).
master_questions(H, [F|Z]) :-
  Q =.. [F, Attr, Mod, List],
  writeq(H, (question(Attr, Mod, List) :- Q)),
  write(H, $.\n$),
  master_questions(H, Z).

master_rules(H, []).
master_rules(H, [F|Z]) :-
  R =.. [F, Attr, Mod, Val, List],
  writeq(H, (rule(Attr, Mod, Val, List) :- R)),
  write(H, $.\n$),
  master_rules(H, Z).

master_answers(H, []).
master_answers(H, [F|Z]) :-
  A =.. [F, Val, Mod, Goal, List],
  writeq(H, (answer(Val, Mod, Goal, List) :- A)),
  write(H, $.\n$),
  master_answers(H, Z).

master_notes(H, []).
master_notes(H, [F|Z]) :-
  N =.. [F, Val, Mod, List],
  writeq(H, (note(Val, Mod, List) :- N)),
  write(H, $.\n$),
  master_notes(H, Z).

master_headers(H, []).
master_headers(H, [F|Z]) :-
  HH =.. [F, Val, Mod, List],
  writeq(H, (header(Val, Mod, List) :- HH)),
  write(H, $.\n$),
  master_headers(H, Z).

master_footers(H, []).
master_footers(H, [F|Z]) :-
  FF =.. [F, Val, Mod, List],
  writeq(H, (footer(Val, Mod, List) :- FF)),
  write(H, $.\n$),
  master_footers(H, Z).

master_modules(H, []).
master_modules(H, [F|Z]) :-
  M =.. [F, Val, List],
  writeq(H, (module(Val, List) :- M)),
  write(H, $.\n$),
  master_modules(H, Z).

%
% Extremely handy debugging tool.  Just put a ? in front of any Prolog predicate
% you want to trace (and enable logging).  See op definition at top of file.
% Modified to expand HTML characters e.g. > becomes &gt;
%

bugwrite(X) :-
  string_termq(S, X),
  string_list(S, L),
  cgiExpandChars(L, L2),
  string_list(S2, L2),
  cgiLog(S2),
  cgiLog($\n$).

bugcallfail(X) :-
  bugwrite('CALL':X).
bugcallfail(X) :-
  bugwrite('FAIL':X),
  fail.

bugexitredo(X) :-
  bugwrite('EXIT':X).
bugexitredo(X) :-
  bugwrite('REDO':X),
  fail.

? X :- bug(X).

bug(X) :-
  bugcallfail(X),
  call(X),
  bugexitredo(X).
