%
% register.pro
%
% User registration and unlocking routines
%
% Main Entry Points:
%   unl$gid - Gets Product ID component parts
%   unl$pid - Parses a product ID (instead of returning the registered one like unl$gid)
%   unl$gri - Gets all registration information (user, org, info, prod, key)
%   unl$rid - Registers/unlocks the product
%   unl$ctm - Checks if timelock has expired
%   unl$sta - Returns status: l=locked, r=registered, t=unlocked til date, x=time expired
%

:- module(register).
:- export([unl$gid/5, unl$pid/10, unl$gri/5, unl$rid/6, unl$ctm/4, unl$sta/1,
   unl$uptr/2, unl$upfx/2, unl$gke/5, unl$asp/3, unl$m1/5, unl$gck/2, unl$c1/3,
   unl$x1/2, unl$t1/4]).
:- end_module(register).

:- body(register).
:- import(list).

% For debugging (include bug predicates)

unl$un($VVVVVVVVVVVVVVVVVVVVVVVVVVVVVV$).  % User Name
unl$on($WWWWWWWWWWWWWWWWWWWWWWWWWWWWWW$).  % Organization Name
unl$in($XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX$).  % Information
unl$pi($YYYYYYYYYYYYYYYYYYYYYYYYYYYYYY$).  % PPPSS-VVUUU-FDIC0-##### (Serial Number)
                                           %          11111111112222
                                           % 12345678901234567890123
unl$uk($ZZZZZZZZZZZZZZZZZZZZ$).            % RXXX-XXXX-XXXX-XXXX (Unlock Code)

% See PRODIDLEN for code dependent upon its length (23)
% See CHKSUMIDX for code dependent upon the location of the C checksum (16)
% See DASHIDX1,2,3 for position of the dashes (6,12,18)
% See FORMATIDX for position of the format code (13)
% See SEQNUMIDX for position of the serial number (19)
% See unl$gid for all positional information

%
% unl$gid
% Get product ID components
%
unl$gid(Product, Platform, Version, Users, SerialNum) :-
  unl$pi(PID),
  sub_string(PID, 1, 3, Product),
  sub_string(PID, 4, 2, Platform),
  sub_string(PID, 7, 1, StrMajVer),
  sub_string(PID, 8, 1, StrMinVer),
  stringlist_concat([StrMajVer, $.$, StrMinVer], Version),
  unl$cid,     % Right in the middle should confuse them
  sub_string(PID, 9, 3, StrUsers),
  string_integer(StrUsers, Users),
  sub_string(PID, 19, 5, SerialNum), !.

%
% unl$pid
% Parse a product ID that is not the registered one
%
unl$pid(ProductID, Product, Platform, Version, Users, SerialNum, XMonth, XMonthName, XDay, XYear) :-
  unl$uptr(ProductID, PID),
  sub_string(PID, 1, 3, Product),
  sub_string(PID, 4, 2, Platform),
  sub_string(PID, 7, 1, StrMajVer),
  sub_string(PID, 8, 1, StrMinVer),
  stringlist_concat([StrMajVer, $.$, StrMinVer], Version),
  sub_string(PID, 9, 3, StrUsers),
  string_integer(StrUsers, Users),
  sub_string(PID, 19, 5, SerialNum), 

  % If its timelocked return that information too
  (sub_string(PID, 13, 1, $T$) ->			% FORMATIDX
    % First three digits are Day*13 + Month
    sub_string(PID, 19, 3, DayMonS),			% SEQNUMIDX
    string_integer(DayMonS, DayMon),
    XDay is DayMon divu 13,
    XMonth is DayMon mod 13,

    % Last two digits are years since 1998
    sub_string(PID, 22, 2, Year98S),			% SEQNUMIDX+3
    string_integer(Year98S, Year98),
    XYear is Year98 + 1998,

    member(XMonth : MonthName, [1:'Jan',2:'Feb',3:'Mar',4:'Apr',5:'May',6:'Jun',7:'Jul',8:'Aug',9:'Sep',10:'Oct',11:'Nov',12:'Dec']),
    string_atom(XMonthName, MonthName)
  ;
    XMonth = 0,
    XMonthName = $$,
    XDay = 0,
    XYear = 0
  ), !.

%
% unl$gri
% Get all the registration information
%
unl$gri(User, Org, Info, Prod, Key) :-
  unl$un(UserT),
  (unl$v1(UserT, 0'V) -> User0 = $$ ; User0 = UserT),
  unl$upfx(User0, User),
 note($gri user $),
  unl$on(OrgT),
  (unl$v1(OrgT, 0'W) -> Org0 = $$ ; Org0 = OrgT),
  unl$upfx(Org0, Org),
 note($gri org $),
  unl$in(InfoT),
  (unl$v1(InfoT, 0'X) -> Info0 = $$ ; Info0 = InfoT),
  unl$upfx(Info0, Info),
 note($gri info $),
  unl$cid,     % Right in the middle should confuse them
 note($gri cid $),
  unl$pi(ProdT),
  (unl$v1(ProdT, 0'Y) -> Prod0 = $$ ; Prod0 = ProdT),
  unl$uptr(Prod0, Prod),
 note($gri prod $),
  unl$uk(KeyT),
  (unl$v1(KeyT, 0'Z) -> Key0 = $$ ; Key0 = KeyT),
  unl$uptr(Key0, Key),
 note($gri key $),
  !.


%
% check if a string contains all of the same character
%
unl$v1(Str, Char) :-
  string_list(Str, List),
  unl$v2(List, Char).
unl$v2([], _).
unl$v2([Char | T], Char) :-
  unl$v2(T, Char).
unl$v2([H | T], Char) :-
  !, fail.

%
% unl$sta
% Returns status: l=locked, r=registered, t=unlocked til date, x=time expired
%
unl$sta(l) :-
  not unl$cid, !.
unl$sta(r) :-
  unl$cid,
  not unl$tim, !.
unl$sta(t) :-
  unl$cid,
  unl$tim,
  unl$ctm(_, _, _, _), !.
unl$sta(x) :-
  unl$cid,
  unl$tim,
  not unl$ctm(_, _, _, _).

%
% unl$tim
% Succeed if this is a timelocked version
%
unl$tim :-
  unl$pi(ProdStr),
  sub_string(ProdStr, 13, 1, Format),		% FORMATIDX
  Format = $T$.

%
% unl$ctm
% Suceed if not expired and return expiration date
%
unl$ctm(XMonth, XMonthName, XDay, XYear) :-
  unl$tim,
  unl$pi(ProdStr),

  % First three digits are Day*13 + Month
  sub_string(ProdStr, 19, 3, DayMonS),		% SEQNUMIDX
  string_integer(DayMonS, DayMon),
  XDay is DayMon divu 13,
  XMonth is DayMon mod 13,

  % Last two digits are years since 1998
  sub_string(ProdStr, 22, 2, Year98S),		% SEQNUMIDX+3
  string_integer(Year98S, Year98),
  XYear is Year98 + 1998,

  date(CMonth, CDay, CYear),
  !,
  ( (XYear > CYear) ; (XYear = CYear, XMonth > CMonth) ; 
    (XYear = CYear, XMonth = CMonth, XDay >= CDay) ),

  member(XMonth : MonthName, [1:'Jan',2:'Feb',3:'Mar',4:'Apr',5:'May',6:'Jun',7:'Jul',8:'Aug',9:'Sep',10:'Oct',11:'Nov',12:'Dec']),
  string_atom(XMonthName, MonthName).

%
% unl$cid
% Check user/organization and product ID to unlock key
%

unl$cid :-
  unl$un(UserStr),
 note(UserStr),
  unl$on(OrgStr),
 note(OrgStr),
  unl$pi(ProdStr),
 note(ProdStr),
  unl$in(InfoStr),
 note(InfoStr),
  unl$uk(UnlockKey),
 note(UnlockKey),

  once(unl$m1(UserStr, OrgStr, InfoStr, ProdStr, UnlockKey)).

unl$m1(UserStr, OrgStr,  InfoStr, ProdStr, UnlockKey) :-
 note($entering m1$),
  % Check the User
  string_length(UserStr, ULen),
  ULen = 30,
  nonblank_string(UserStr),
 note(UserStr),

  % Check the Organization
  string_length(OrgStr, OLen),
  OLen = 30,
 note(OrgStr),

  % Check the Product ID
  string_length(ProdStr, PLen),
  PLen = 30,
  nonblank_string(ProdStr),
 note(ProdStr),
  sub_string(ProdStr, 6, 1, Dash1),		% DASHIDX1
  Dash1 = $-$,
  sub_string(ProdStr, 12, 1, Dash2),		% DASHIDX2
  Dash2 = $-$,
  sub_string(ProdStr, 18, 1, Dash3),		% DASHIDX3
  Dash3 = $-$,
  sub_string(ProdStr, 24, 1, Space),		% PRODIDLEN+1
  Space = $ $,
  sub_string(ProdStr, 13, 1, Format),		% FORMATIDX
  (Format = $J$ ; Format = $T$),
  sub_string(ProdStr, 16, 1, PIDChkChr),	% CHKSUMIDX
 note(PIDChkChr),
  unl$gck(ProdStr, CheckChar),
 note(CheckChar),
  CheckChar = PIDChkChr,

  % Check the Unlock Key
  unl$gke(UserStr, OrgStr, InfoStr, ProdStr, KeyStr),
  string_length(KeyStr, KLen),
  KLen = 19,
  unl$asp(KeyStr, 20, KeyStrP),
  nonblank_string(UnlockKey),
  KeyStrP = UnlockKey,
  !.

%
% unl$rid
% Register the user
%
unl$rid(File, UserS, OrgS, InfoS, ProdS, KeyS) :-
  % Uppercase and fix up the User, Org and Info
  unl$upfx(UserS, UserS0),
  unl$upfx(OrgS, OrgS0),
  unl$upfx(InfoS, InfoS0),

  % Uppercase and trim the Product ID and unlock code
  unl$uptr(ProdS, ProdS0),
  unl$uptr(KeyS, KeyS0),

  % Pad everything out to its correct length
  unl$asp(UserS0, 30, User),
  unl$asp(OrgS0, 30, Org),
  unl$asp(InfoS0, 30, Info),
  unl$asp(ProdS0, 30, Prod),
  unl$asp(KeyS0, 20, Key),

  % Make sure the unlock code and product id are valid
 note(User), note(Org), note(Info), note(Prod), note(Key),
  once(unl$m1(User, Org, Info, Prod, Key)),

  % Open the file
%  get_mode(file_errors, FileMode),
%  asserta('{sys}filemode'(FileMode)),
%  set_mode(file_errors, off),
 note(File),
%  fopen(H, File, 'rb+'),
  open(File, readwrite, H, [type(binary)]),
 note($File Opened$),
  asserta('{sys}filehand'(H)),

  % Ensure all the strings can be found and return their offsets
  catch(unl$fst(H, 0'V, 30, UserOffset), fileReadErr, fail),
  catch(unl$fst(H, 0'W, 30, OrgOffset), fileReadErr, fail),
  catch(unl$fst(H, 0'X, 30, InfoOffset), fileReadErr, fail),
  catch(unl$fst(H, 0'Y, 30, ProdOffset), fileReadErr, fail),
  catch(unl$fst(H, 0'Z, 20, KeyOffset), fileReadErr, fail),

  % Write the user name
%  fseek(H, UserOffset, 0, NewUserOffset),
  set_stream_position(H, binary(beginning, UserOffset, UserOffset)),
%  UserOffset = NewUserOffset,
  string_list(User, UserList),
 note(writing-User),
  unl$wrl(H, UserList),

  % Write the organization name
%  fseek(H, OrgOffset, 0, NewOrgOffset),
  set_stream_position(H, binary(beginning, OrgOffset, OrgOffset)),
%  OrgOffset = NewOrgOffset,
  string_list(Org, OrgList),
 note(writing-Org),
  unl$wrl(H, OrgList),

  % Write the extra information
%  fseek(H, InfoOffset, 0, NewInfoOffset),
  set_stream_position(H, binary(beginning, InfoOffset, InfoOffset)),
%  InfoOffset = NewInfoOffset,
  string_list(Info, InfoList),
 note(writing-Info),
  unl$wrl(H, InfoList),

   % Write the product id
%  fseek(H, ProdOffset, 0, NewProdOffset),
  set_stream_position(H, binary(beginning, ProdOffset, ProdOffset)),
%  ProdOffset = NewProdOffset,
  string_list(Prod, ProdList),
 note(writing-Prod),
  unl$wrl(H, ProdList),

  % Write the unlock code
%  fseek(H, KeyOffset, 0, NewKeyOffset),
  set_stream_position(H, binary(beginning, KeyOffset, KeyOffset)),
%  KeyOffset = NewKeyOffset,
  string_list(Key, KeyList),
 note(writing-Key),
  unl$wrl(H, KeyList),
 note(done),

  % Return file_errors to its default
%  set_mode(file_errors, FileMode),
  close(H), !,
 note($Registered!$).  
unl$rid(_, _, _, _, _, _) :-
  % Clean up on failure
%  (retract('{sys}filemode'(FileMode)) -> set_mode(file_errors, FileMode) ; true),
  (retract('{sys}filehand'(H)) -> close(H) ; true),
  !, 
  fail.

unl$st([_,_,_,_,_,0'-,_,_,_,_,_,0'-,_,_,_,_,_,0'-,_,_,_,_,_]).
unl$kt([_,_,_,_,0'-,_,_,_,_,0'-,_,_,_,_,0'-,_,_,_,_]).

%
% Write a list to the file at the current offset
%
unl$wrl(_, []).
unl$wrl(Handle, [H | T]) :-
%  fwrite(Handle, H, 1),  % this is a 0 for 3.3, a 1 for 4.0
  write_binary(Handle, char, H),
  write_binary(Handle, char, 0),	% Unicode write the 2nd byte
  unl$wrl(Handle, T).

%
% Find a string in the file (from the current offset)
%
unl$fst(H, Char, NumTimes, Offset) :-
  cntr_set(1, 1),
  NumTimes1 is NumTimes - 1,
  repeat,
  read_binary(H, char, FileChar),
  (FileChar == end_of_file -> throw(fileReadErr) ; true),
  (FileChar = Char ->
    % If we found it before, keep counting
    ('{sys}loff'(Off) -> 
      true 
    % Otherwise save our offset
    ; 
      stream_property(H, position(binary(Orig, In, Out))),
      Off2 is Out - 1,
      asserta('{sys}loff'(binary(Orig, In, Off2))) 
    ),
    cntr_inc(1, SLen),
    read_binary(H, char, 0),	% Unicode, get the 2nd byte
    SLen = NumTimes1
  ;
    retractall('{sys}loff'(_)),
    cntr_set(1, 1),
    fail
  ),
  retract('{sys}loff'(binary(_, _, Offset))).

%
% Pad a string with spaces to the specified length
%
unl$asp(Str, Len, StrPad) :-
  string_length(Str, CurLen),
  NumSpaces is Len - CurLen,
  NumSpaces > 0,
  sub_string($                              $, 1, NumSpaces, Spaces),
  strcat(Str, Spaces, StrPad), !.
unl$asp(Str, _, Str).

%
% Convert a string to upper case and trim out all the spaces, ctrl chars, etc.
% Change O's into 0's and I's into 1's. Used for serial numbers and unlock codes.
%
unl$uptr($$, $$) :- !.
unl$uptr(StrIn, StrOut) :-
  string_list(StrIn, LstIn),
  unl$u0(LstIn, [], LstOut),
  reverse(LstOut, LstOut0),
  string_list(StrOut, LstOut0), !.
unl$u0([], L, L).
unl$u0([H | T], La, L) :-
  (H > 96, H < 123 -> X is H - 32 ; X is H ),
  unl$x2(X, X2),
  (X2 < 33 -> La2 = La ;  append([X2], La, La2) ),
  unl$u0(T, La2, L).  

%
% Convert a string to upper case, trim leading and trailing spaces and
% convert all multiple spaces into a single space. Remove all control
% characters. Used for user names, organizations and information strings.
%
unl$upfx($$, $$) :- !.
unl$upfx(StrIn, StrOut) :-
  string_list(StrIn, LstIn),
  unl$u1(LstIn, [], LstOut1),
  (LstOut1 = [0' | T] -> LstOut2 = T ; LstOut2 = LstOut1),
  reverse(LstOut2, LstOutR2),
  (LstOutR2 = [0'  | T2] -> LstOutR3 = T2 ; LstOutR3 = LstOutR2),
  reverse(LstOutR3, LstOut3),
  string_list(StrOut, LstOut3), !.

unl$u1([], L, L) :- !.
unl$u1([0' , 0'  | T], La, L) :-
  unl$u1([0'  | T], La, L).
unl$u1([H | T], La, L) :-
  (H > 96, H < 123 -> X is H -32 ; X is H ),
  (X < 32 -> La2 = La ; append(La, [X], La2) ),
  unl$u1(T, La2, L).

%
% unl$gke
% Generate unlock key
%
unl$gke(UserS, OrgS, InfoS, ProdS, UOIPStr) :-
  unl$upfx(UserS, UserS0),
  unl$upfx(OrgS, OrgS0),
  unl$upfx(InfoS, InfoS0),
  unl$uptr(ProdS, ProdS0),

  unl$asp(UserS0, 30, UserStr),
  unl$asp(OrgS0, 30, OrgStr),
  unl$asp(InfoS0, 30, InfoStr),
  unl$asp(ProdS0, 30, ProdStr),

  string_list(UserStr, UserLst),
  string_list(OrgStr, OrgLst),
  string_list(InfoStr, InfoLst),
  sub_string(ProdStr, 1, 23, ProdIDStr),	% PRODIDLEN
  sub_string(ProdStr, 1, 7, ProdXStr),		% 30-PRODIDLEN
  stringlist_concat([ProdIDStr, ProdXStr], NewProdStr),
  string_list(NewProdStr, ProdLst),
  reverse(OrgLst, OrgRev),
  unl$t1(\, ProdLst, [], ProdLst2),
  unl$t2(+, OrgRev, UserLst, [], UOLst),
  unl$t2(-, UOLst, InfoLst, [], UOILst),
  unl$t3(+, UOILst, ProdLst2, [], UOIPLst),
  unl$t4(UOIPLst, [], UOIPLstChar),
  append([0'R], UOIPLstChar, UOIPLstC2),  
  string_list(UOIPStrAll, UOIPLstC2),
  sub_string(UOIPStrAll, 1, 4, UOIP1),
  sub_string(UOIPStrAll, 5, 4, UOIP2),
  sub_string(UOIPStrAll, 9, 4, UOIP3),
  sub_string(UOIPStrAll, 13, 4, UOIP4),
  stringlist_concat([UOIP1, $-$, UOIP2, $-$, UOIP3, $-$, UOIP4], UOIPStr).

%
% Permute one list via some operator
%  
unl$t1(_, [], L, L).
unl$t1(Op, [H | T], La, L) :-
  AE =..[Op, H],
  X is AE,
  append([X], La, La2),
  unl$t1(Op, T, La2, L).  

%
% Combine two lists via some operator
%
unl$t2(_, [], _, L, L).
unl$t2(_, _, [], L, L).
unl$t2(Op, [H1 | T1], [H2 | T2], La, L) :-
  AE =..[Op, H1, H2],
  X is AE,
  append([X], La, La2),
  unl$t2(Op, T1, T2, La2, L).

%
% Make a list half its length by combining adjoining bytes
%
unl$t3(_, [], _, L, L).
unl$t3(_, _, [], L, L).
unl$t3(Op, [H1,I1 | T1], [H2,I2 | T2], La, L) :-
  AE =..[Op, H1, H2],
  X is AE,
  AE2 =..[Op, I1, I2],
  Y is AE2,
  AE3 =..[Op, X, Y],
  Z is AE3,
  append([Z], La, La2),
  unl$t3(Op, T1, T2, La2, L).

%
% Convert a list to displayable ASCII only (1-9, A-Z)
%   turn all O's into M's, I's into 1's
%
unl$t4([], L, L).
unl$t4([H | T], La, L) :-
  (H < 0 -> 
    X0 is (-H mod 35) + 49 
  ; 
    X0 is (H mod 35) + 49 
  ),
  ( X0 > 57 -> 
    X is X0 + 7
  ;
    X is X0
  ),
  unl$x1(X, X2),
  append([X2], La, La2),
  unl$t4(T, La2, L).

unl$x1(0'O, 0'M) :- !.
unl$x1(0'I, 0'1) :- !.
unl$x1(C, C).

unl$x2(0'O, 0'0) :- !.
unl$x2(0'I, 0'1) :- !.
unl$x2(C, C).

%
% generate product ID checksum (1-9, A-Z, except O becomes M, I becomes 1)
%   uses all characters in the product ID except the checksum char 
%   in position CHKSUMIDX
%
% call with PPPSS-VVUUU-FDI 0-NNNNN
%
unl$gck(PID, CheckChar) :-
  sub_string(PID, 1, 15, PID1),		% CHKSUMIDX-1
  unl$uptr(PID1, PIDUP1),
  string_list(PIDUP1, PIDLst1),
  unl$c1(PIDLst1, 0, Chk1),
  sub_string(PID, 17, 2, PID2),		% CHKSUMIDX+1
  unl$uptr(PID2, PIDUP2),
  string_list(PIDUP2, PIDLst2),
  unl$c1(PIDLst2, 0, Chk2),
  sub_string(PID, 19, 5, PID3),		% CHKSUMIDX+3
  string_integer(PID3, Num),
  ChkTot is Chk1 /\ Chk2 + Num,
  ( ChkTot < 0 -> 
    ChkChr is (-ChkTot mod 35) + 49 
  ;
    ChkChr is (ChkTot mod 35) + 49
  ),
  ( ChkChr > 57 -> 
    ChkChr2 is ChkChr + 7
  ;
    ChkChr2 is ChkChr
  ),
  unl$x1(ChkChr2, ChkChr3),
  string_list(CheckChar, [ChkChr3]).

%
% sum all the characters in the list
%
unl$c1([], C, C).
unl$c1([H | T], Ca, C) :-
  Ca2 is Ca + H,
  unl$c1(T, Ca2, C).


% Utilities

note(_).
%note(X) :- nl, write(X), nl.

:- end_body(register).
