%
% regamzi.pro
%
% Front-end for registration routines, never linked into a product
% Only used internally in the database and web site.
%
% To run from the listener, just consult this file and call unl$test.
%
% Requires register.pro (which is linked into the listener).
%

% See INTSUMIDX for internal checksum index (15)
% See CHKSUMIDX for code dependent upon the location of the C checksum (16)


:- import(list).
:- import(amzi_register).

main :- 
/*  unl$pid($KBXPC-42001-T1TA0-06904$, _Product, _Platform, _Version, _Users, _SerialNum, _XMonth, _XMonthName, _XDay, _XYear),
  write(_Product),nl,
  write(_Platform),nl,
  write(_Version),nl,
  write(_Users),nl,
  write(_SerialNum),nl,
  write(_XMonth),nl,
  write(_XMonthName),nl,
  write(_XDay),nl,
  write(_XYear),nl,
*/
%  unl$rid($C:\\amzi\\dev\\kw4\\src\\ide\\kwauthor.xpl$, $Mary Merritt$, $$, $$, $KBXPC-42001-T1TA0-06904$, $R9ND-NEUW-12XZ-6G2A$),
  unl$rid($C:\\amzi\\dev\\kw4\\src\\ide\\kwauthor.xpl$, $Mary Merritt$, $$, $$, $KB1PC-42001-J1W40-39359$, $R9N9-1FMW-8RXZ-6GXA$),
  write($success$).
main :-
  write($failure$).

unl$idx(intsum, 15).	% INTSUMIDX
unl$idx(extsum, 16).	% CHKSUMIDX

unl$test :-
  write($User: $), read_string(UserS),
  unl$upfx(UserS, User),
  write($Organization: $), read_string(OrgS),
  unl$upfx(OrgS, Org),
  write($Info: $), read_string(InfoS),
  unl$upfx(InfoS, Info),
  write($Product [PPPSS-VVUUU-FD]: $), read_string(ProdS),
  unl$uptr(ProdS, Prod),
%  write($Serial# [0-NNNNN]: $), read_string(Serial),
  write($Exp Month: $), read(Month),
  write($Exp Day:   $), read(Day),
  write($Exp Year:  $), read(Year),
  First3 is Day*13 + Month,
  Last2 is Year - 1998,
  string_integer(SFirst3, First3),
  string_integer(SLast2, Last2),
  string_length(SLast2, SLast2Len),
  (SLast2Len = 1 -> stringlist_concat([$0$, SLast2], SLast22) ; SLast22 = SLast2),
  stringlist_concat([$0-$, SFirst3, SLast22], Serial),

  % Generate serial number and check it
  write($Generating checksums...$), nl,
  generate_checksums(Prod, Serial, PIDS),
  write($Checking checksums...$), nl,
  validate_checksums(PIDS),

  % Generate unlock code and check it
  write($Generating unlock code...$), nl,
  unl$gke(User, Org, Info, PIDS, UnlockKeyS),
  unl$uptr(PIDS, PIDS0),
  unl$asp(UserS, 30, UserStr),
  unl$asp(OrgS, 30, OrgStr),
  unl$asp(InfoS, 30, InfoStr),
  unl$asp(PIDS0, 30, PIDStr),
  unl$asp(UnlockKeyS, 20, UnlockKey),
  write($Checking unlock code...$), nl,
  unl$m1(UserStr, OrgStr, InfoStr, PIDStr, UnlockKey),

  write($Product ID = $), write(PIDS), nl,
  write($Unlock Key = $), write(UnlockKey), nl,

  retractall(unl$un(_)),
  asserta(unl$un(UserStr)),
  retractall(unl$on(_)),
  asserta(unl$on(OrgStr)),
  retractall(unl$in(_)),
  asserta(unl$in(InfoStr)),
  retractall(unl$pi(_)),
  asserta(unl$pi(PIDStr)),
  retractall(unl$uk(_)),
  asserta(unl$uk(UnlockKey)),

  write($Checking status...$), nl,
  unl$sta(Status),
  write_status(Status),
  (unl$ctm(Mon, MonName, Day, Year) ->
    write($Expires: $), write(Day), write(MonName), write(Year), nl
    ; true).

write_status(l) :-
  write($Locked\n$).
write_status(r) :-
  write($Registered\n$).
write_status(t) :-
  unl$ctm(XMon, XMonthName, XDay, XYear),
  write($Timelocked Expires: $),  
  write(XMon), write($/$), write(XDay), write($/$), write(XYear), nl.
write_status(x) :-
  write($Timelocked Expired\n$).

%
% generate serial number checksums
%
% call with strings: PPPSS-VVUUU-FD, 0-NNNNN
%
generate_checksums(ProdToDistS, SequenceS, FullSerial) :-
  unl$uptr(ProdToDistS, ProdToDist),
  unl$uptr(SequenceS, Sequence),
  stringlist_concat([ProdToDist, $  $, Sequence], S1),
  unl$gic(S1, IntChk),
  stringlist_concat([ProdToDist, IntChk, $ $, Sequence], S2),
  unl$gck(S2, ExtChk),
  stringlist_concat([ProdToDist, IntChk, ExtChk, Sequence], FullSerial).

%
% validate serial number checksums
%
% call with PPPSS-VVUUU-FDIC0-NNNNN
%
validate_checksums(SerialS) :-
  unl$uptr(SerialS, Serial),
  sub_string(Serial, 1, 14, S1),	% INTSUMIDX-1
  sub_string(Serial, 17, 7, S2),	% CHKSUMIDX+1, PROIDLEN-CHKSUMIDX
  stringlist_concat([S1, $  $, S2], Serial2),
  unl$gic(Serial2, IntChk),
  unl$idx(intsum, IP),
  sub_string(Serial, IP, 1, IntStr),
  IntStr = IntChk,
  stringlist_concat([S1, IntStr, $ $, S2], Serial3),
  unl$gck(Serial3, ExtChk),
  unl$idx(extsum, EP),
  sub_string(Serial, EP, 1, ExtStr),
  ExtStr = ExtChk.

%
% generate product ID internal checksum (A-Z, except O becomes M, I becomes 1)
%   uses all the characters in the product ID except the
%   internal positions INTSUMIDX and CHKSUMIDX
%
% call with PPPSS-VVUUU-FD  0-NNNNN
%
unl$gic(PID, CheckChar) :-
  sub_string(PID, 1, 14, PID1),		% INTSUMIDX-1
  unl$uptr(PID1, PIDUP1),
  string_list(PIDUP1, PIDLst1),
  unl$c1(PIDLst1, 0, Chk1),
  sub_string(PID, 17, 2, PID2),		% CHKSUMIDX+1
  unl$uptr(PID2, PIDUP2),
  string_list(PIDUP2, PIDLst2),
  unl$c1(PIDLst2, 0, Chk2),
  sub_string(PID, 19, 5, PID3),		% CHKSUMIDX+3
  string_integer(PID3, Num),
  ChkTot is Chk1 \/ Chk2 + Num,
  ( ChkTot < 0 -> 
    ChkChr is (-ChkTot mod 26) + 65 
  ;
    ChkChr is (ChkTot mod 26) + 65
  ),
  unl$x1(ChkChr, ChkChr2),
  string_list(CheckChar, [ChkChr2]).
