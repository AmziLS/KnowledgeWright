:- load(axrf).
:- consult('..\\jigs\\kw_ops.pro').
:- load(list).
:- loadlsx('aosutils.lsx').
:- import(axrf).

main :- axrf:xref([
  'author.pro',
  'convert.pro',
  'error_check.pro',
  'find_replace.pro',
  'utilities.pro',
  'webls324.pro',
  'xref.pro'],
     'author.xrf').
