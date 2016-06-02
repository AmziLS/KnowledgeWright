
%
% KnowledgeWright Operators
%
% Copyright (c)1996-2002 Amzi! inc. All Rights Reserved.
%

%:- op(900, fy, [?, bug]).

:- op(790, fx, if).     	% prefix operator
:- op(790, xfx, where).         % for queries
:- op(780, xfx, then).  	% infix operator
%:- op(780, fx, find).           % for queries: find a in b where conditions
:- op(775, xfy, or).    	% infix that can be linked
:- op(770, xfy, and).   	% infix that can be linked
:- op(770, xfx, from).          % for queries
:- op(680, xfx, ==>).		% infix operator
:- op(650, xfx, <=).		% infix operator
:- op(650, xfx, =>).		% infix operator
:- op(650, xfx, include).	% infix operator
:- op(650, xfx, exclude).	% infix operator
:- op(650, xfx, include_only).	% infix operator
:- op(650, xfx, number_of_items).  % infix operator
:- op(650, xfx, contains).	% infix operator
:- op(650, xfx, contains_nocase). % infix operator
:- op(650, xfx, is).		% infix operator
:- op(650, xf, is_known).	% postfix operator
:- op(650, xfx, from).          % infix operator, used in db queries
:- op(400, fx, last).           % prefix operator, used to modify db record name
:- op(400, fx, <).              % used in tables, where other arg is understood
:- op(400, fx, >).              % used in tables, where other arg is understood
:- op(400, fx, <=).             % used in tables, where other arg is understood
:- op(400, fx, =<).             % used in tables, where other arg is understood
:- op(400, fx, >=).             % used in tables, where other arg is understood
:- op(400, fx, include).        % used in tables, where other arg is understood
:- op(400, fx, exclude).        % used in tables, where other arg is understood
:- op(400, fx, include_only).   % used in tables, where other arg is understood
:- op(400, fx, number_of_items). % used in tables, where other arg is understood
:- op(400, fx, contains).       % used in tables, where other arg is understood
:- op(400, fx, contains_nocase). % used in tables, where other arg is understood
:- op(400, fx, exists).         % used for db records
:- op(200, fx, /).              % used for folders
:- op(50, xf, days).
:- op(50, xf, months).
:- op(50, xf, weeks).
:- op(50, xf, years).


