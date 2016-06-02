program delphi_run;

uses
  Forms,
  main in 'main.pas' {MainForm},
  Amzi in '..\..\..\a6\lsapis\delphi\amzi.pas',
  askquestion in 'askquestion.pas' {AskMenuForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAskMenuForm, AskMenuForm);
  Application.Run;
end.
