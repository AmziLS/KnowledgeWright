unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Amzi, Buttons;

type
  TPropStringArray = array[0..999,0..1] of string;
  TMainForm = class(TForm)
    Solution: TMemo;
    RunButton: TButton;
    ls: TLSEngine;
    Exit: TBitBtn;

    procedure RunButtonClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    function DoubleSlashes(s: string): string;
    function GetProperty(arr: TPropStringArray; name: string): string;
    function PrologListToProperties(tlist: TTERM; len: integer; var arr: TPropStringArray): boolean;
    function PrologListToStrings(tlist: TTERM; var ts: TStrings): boolean;

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses askquestion;

{$R *.DFM}

procedure TMainForm.RunButtonClick(Sender: TObject);
var
  term, tlist, solveTerm, actionTerm, responseTerm: TTerm;
  path, id, name, solveStr, responseFunctor, responseType, s,
    factName, prompt, questionType, defaultValue, answerType,
    delimiter, userAnswer, factList, answerName, jig,
    newSolution: string;
  info, question, answer, error: TPropStringArray;
  menu, userAnswers: TStrings;
  more, oneAnswer: boolean;
  len, rc, i: integer;
  msg: array[0..10000] of char;
  label close;
begin
  try
  begin
    menu := TStringList.Create;
    userAnswers := TStringList.Create;

    { Set this for proper exception handling }
    id := 'none';

    { Initialize a new Logic Server }
    jig := InputBox('Open Jig', 'Enter name of jig to open (no extension):', '');
    jig := jig + '.xpl';
    ls.InitLS(jig);

    { Load OS Utils }
    ls.AddLSX('aosutils');
    
    { Load ODBC (optional, you can take this out if you don't use it)
      Alternatively you could call ls.InitLSX() which will load any
      LSXs specified in amzi.cfg }
    ls.AddLSX('aodbc');

    { Load the jig }
    ls.LoadXPL(jig);

    { Initialize the runtime with directory and logfile
      Prior to starting a new KWI session the session id must be kw_init. }
    GetDir(0, path);
    path := path + '\';
    id := 'kw_init';

    { The directory is where the kb file is located
      The session_directory is where a temporary file is created to maintain the state
      of the reasoning process
      The log_file is a log of the reasoning process and is created in the directory
      The message level is the level of detail in the log_file }
    s := 'kwi(' + id + ', initialize([directory = $' + DoubleSlashes(path) + '$, ' +
          'session_directory = $' + DoubleSlashes(path) + '$, ' +
          'log_file = $kwrun.log$, message_level = high]), _INFO)';
    if ls.ExecPStr(term, s) = False then
    begin
      Application.MessageBox('kwi / initialize failed', 'Error', MB_OK);
      goto close;
    end;

    { Get the name, version and build }
    ls.GetArg(term, 3, dTERM, @tlist);
    PrologListToProperties(tlist, 1000, info);
    s := GetProperty(info, 'system');
    s := s + ' version ' + GetProperty(info, 'version');
    solution.Text := s;

    { Open the knowledgebase }
    name := InputBox('Open Knowledgebase', 'Enter name of knowledgebase to open:', '');
    if length(name) = 0 then goto close;
    s := 'kwi(' + id + ', open($' + name + '$), _)';
    if ls.ExecPStr(term, s) = False then
    begin
      Application.MessageBox('kwi / open failed', 'Error', MB_OK);
      goto close;
    end;

    { Start a new session
      Now the id can be anything but kw_init }
    id := 'session_0';
    s := 'kwi(''' + id + ''', new_session, _)';
    if ls.ExecPStr(term, s) = False then
    begin
      Application.MessageBox('kwi / new_session failed', 'Error', MB_OK);
      goto close;
    end;

    { Keep calling kwi / solve until there is nothing else to do }
    solveStr := 'kwi(''' + id + ''', solve, _MORE)';
    while ls.ExecPStr(solveTerm, solveStr) do
    begin

      { Check if we're done }
      if ls.GetPStrArg(solveTerm, 3) <> 'more' then more := False else more := True;

      { Process all the actions. Actions are either questions to
        ask the user or information to tell the user. }
      repeat
        { Get the action }
        if ls.ExecPStr(actionTerm, 'kwi(''' + id + ''', get_action, _ACTION)') = False then
        begin
          Application.MessageBox('kwi / get_action failed', 'Error', MB_OK);
          goto close;
        end;

        { The third parameter in the kwi call contains the type and
          parameters of the action to be performed
          Use the Amzi! Logic Server to retrieve the action }
        ls.GetArg(actionTerm, 3, dTERM, @responseTerm);
        responseFunctor := ls.GetFunctor(responseTerm);
        responseType := ls.GetPStrArg(responseTerm, 1);

        { Display the HTML question page (we ignore this...used by the web interface) }
        if (responseFunctor = 'ask') and (responseType = 'html') then
        begin
          { Do nothing }
        end;

        { Get a fact from the user }
        if (responseFunctor = 'ask') and (responseType = 'user') then
        begin

          { The name of the fact we are seeking a value for }
          factName := ls.GetPStrArg(responseTerm, 2);

          { Get all the slots from the question object into a Properties object }
          ls.GetArg(responseTerm, 3, dTERM, @tlist);
          PrologListToProperties(tlist, 10000, question);

          { Get the prompt }
          prompt := GetProperty(question, 'prompt');

          { Get the question type }
          questionType := GetProperty(question, 'question_type');

          { Handle fill in the blank separate from menu choices }
          if questionType = 'fill_in_the_blank' then
          begin
            { Get the field length -- not used }
            Val(GetProperty(question, 'length'), len, rc);

            { Get the default value }
            defaultValue := GetProperty(question, 'default');

            { Ask the user }
            userAnswer := defaultValue;
            if InputQuery('Question: ' + factName, prompt, userAnswer) = False then goto close;

            { Assert the new fact value using the KWI }
            answerType := GetProperty(question, 'answer_type');

            { Assert everything as strings, the KWI will check and convert }
            delimiter := '$';

            s := 'kwi(''' + id + ''', assert(fact(''' + factName + ''', ' + delimiter + userAnswer + delimiter + ')), _)';
            if ls.ExecPStr(term, s) = False then
            begin
              Application.MessageBox('kwi / assert failed', 'Error', MB_OK);
              goto close;
            end;
          end
          { Menu single and multiple choice }
          else
          begin
            { Get all the menu choices into a vector
              Note this code will not work if you have a separate display string }
            s := GetProperty(question, 'choices');
            ls.PStrToTerm(term, GetProperty(question, 'choices'));
            PrologListToStrings(term, menu);
            if GetProperty(question, 'question_type') = 'menu_multiple_choices' then
            begin
              oneAnswer := False;
              prompt := prompt + ' Enter one or more values from the following list: ';
            end
            else
            begin
              oneAnswer := True;
              prompt := prompt + ' Enter one value from the following list: ';
            end;

            { Ask the user }
            AskMenuForm.Setup(prompt, menu, not(oneAnswer));
            AskMenuForm.ShowModal();
            userAnswers := AskMenuForm.GetSelected();
            if userAnswers.Count = 0 then goto close;

            { Assert the new fact or facts (for multiple choices) }
            if oneAnswer = True then
              s := 'kwi(''' + id + ''', assert(fact(''' + factName + ''', $' + userAnswers.Strings[0] + '$)), _)'
            { Multiple answers need to be asserted as a prolog list which is enclosed in []'s }
            else
            begin
              { Build the list from the TStrings returned from AskMenuForm }
              factList := '';
              for i := 0 to userAnswers.Count -1 do
              begin
                if i < userAnswers.Count - 1 then
                  factList := factList + '$' + userAnswers.Strings[i] + '$, '
                else
                  factList := factList + '$' + userAnswers.Strings[i] + '$';
              end;

              s := 'kwi(''' + id + ''', assert(fact(''' + factName + ''', [' + factList + '])), _)';
            end;

            if ls.ExecPStr(term, s) = False then
            begin
              Application.MessageBox('kwi / assert failed', 'Error', MB_OK);
              goto close;
            end;

          end; { fill_in_the_blank and menu }
        end; { ask user }

        { Display an answer }
        if (responseFunctor = 'tell') and (responseType = 'user') then
        begin
          { Get the type of the answer }
          answerType := ls.GetPStrArg(responseTerm, 1);

          { Get all the slots in the answer into a Properties object }
          ls.GetArg(responseTerm, 2, dTERM, @tlist);
          PrologListToProperties(tlist, 1000, answer);

          { Get the name of the answer }
          answerName := GetProperty(answer, 'goal');

          { Add new answer onto existing ones }
          newSolution := GetProperty(answer, 'text');
          if Length(newSolution) > 0 then
            Solution.Lines.Add(answerName + ': ' + newSolution)
          else
          begin
            Application.MessageBox('No text for goal', 'Error', MB_OK);
            goto close;
          end;

        end; { tell user }

      until responseFunctor = 'none';

      if not(more) then break;

    end; {while solveTerm}

    close:
    { Close the Logic Server }
    ls.Close;

    menu.Free;
    userAnswers.Free;
  end;
  except on ELogicServer do
    begin
      { Get all the properties of a KWI error }
      if id <> 'none' then
      begin
        s := 'kwi(''' + id + ''', get_error, _ERROR)';
        if ls.ExecPStr(term, s) = False then
        begin
          Application.MessageBox('Error Processing Logic Server Error', 'Error', MB_OK);
          abort;
        end;
          ls.GetArg(term, 3, dTERM, @tlist);
          PrologListToProperties(tlist, 1000, error);
          s := '';
          for i := 0 to 999 do
            if Length(error[i,0]) > 0 then
              s := s + error[i,0] + '=' + error[i,1] + ' ';

          StrPCopy(msg, s);
          Application.MessageBox(msg, 'Error', MB_OK);
      end
      { Otherwise it is an unexpected error in the Amzi! Logic Server }
      else
      begin
        ls.GetExceptMsg(msg, 10000);
        Application.MessageBox(msg, 'Error', MB_OK);
      end;
    end;
  end;
end;

procedure TMainForm.ExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

{ Utilities }

function TMainForm.DoubleSlashes(s: string): string;
var
  slashed: string;
  i: integer;
begin
  slashed := '';
  for i := 1 to length(s) do
     if s[i] = '\' then
        slashed := slashed + '\\'
     else
        slashed := slashed + s[i];

  Result := slashed;
end;

function TMainForm.GetProperty(arr: TPropStringArray; name: string): string;
var
  i: integer;
begin
  for i := 0 to 999 do
    if arr[i,0] = name then
      break;
  if (i = 999) and (arr[i,0] <> name) then
    Result := ''
  else
    Result := arr[i,1];
end;

function TMainForm.PrologListToProperties(tlist: TTERM; len: integer; var arr: TPropStringArray): boolean;
var
  element, name, value: TTERM;
  ttype: TPType;
  idx: integer;
  sname, svalue: string;
  label FuncEnd;
begin
  idx := 0;

  { Check for the empty list or an atom }
  ttype := ls.GetTermType(tlist);
  if ttype <> pLIST then
  begin
    result := False;
    goto FuncEnd;
  end;

  { Otherwise get the head }
  ls.GetHead(tlist, dTERM, @element);
  ls.GetArg(element, 1, dTERM, @name);
  ls.GetArg(element, 2, dTERM, @value);

  if ls.GetTermType(value) = pLIST then
  begin
    sname := ls.TermToPStr(name);
    svalue := ls.TermToPStrQ(value);
    arr[idx,0] := sname;
    arr[idx,1] := svalue;
  end
  else
  begin
    sname := ls.TermToPStr(name);
    svalue := ls.TermToPStr(value);
    arr[idx, 0] := sname;
    arr[idx, 1] := svalue;
  end;
  idx := idx + 1;

  { And the rest of the list }
  tlist := ls.GetTail(tlist);
  while tlist <> nil do
  begin
    ls.GetHead(tlist, dTERM, @element);
    ls.GetArg(element, 1, dTERM, @name);
    ls.GetArg(element, 2, dTERM, @value);
    if ls.GetTermType(value) = pLIST then
    begin
      sname := ls.TermToPStr(name);
      svalue := ls.TermToPStrQ(value);
      arr[idx,0] := sname;
      arr[idx,1] := svalue;
    end
    else
    begin
      sname := ls.TermToPStr(name);
      svalue := ls.TermToPStr(value);
      arr[idx, 0] := sname;
      arr[idx, 1] := svalue;
    end;
    idx := idx + 1;
    if idx >= len then
    begin
      Result := False;
      goto FuncEnd;
    end;

    tlist := ls.GetTail(tlist);
  end;

  Result := True;

FuncEnd:
end;

function TMainForm.PrologListToStrings(tlist: TTERM; var ts: TStrings): boolean;
var
  element: TTERM;
  ttype: TPType;
  svalue: string;
  label FuncEnd;
begin
  ts.Clear();
  
  { Check for the empty list or an atom }
  ttype := ls.GetTermType(tlist);
  if ttype <> pLIST then
  begin
    result := False;
    goto FuncEnd;
  end;

  { Otherwise get the head }
  ls.GetHead(tlist, dTERM, @element);
  svalue := ls.TermToPStr(element);
  ts.Add(svalue);

  { And the rest of the list }
  tlist := ls.GetTail(tlist);
  while tlist <> nil do
  begin
    ls.GetHead(tlist, dTERM, @element);
    ts.Add(ls.TermToPStr(element));
    tlist := ls.GetTail(tlist);
  end;

  Result := True;

FuncEnd:
end;

end.
