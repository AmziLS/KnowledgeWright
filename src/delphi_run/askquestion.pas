unit askquestion;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TAskMenuForm = class(TForm)
    PromptMemo: TMemo;
    MenuListBox: TListBox;
    OKButton: TButton;
    CancelButton: TButton;
    procedure Setup(prompt: string; list: TStrings; multi: boolean);
    function GetSelected(): TStrings;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AskMenuForm: TAskMenuForm;
  SelectedItems: TStrings;

implementation

{$R *.DFM}

procedure TAskMenuForm.Setup(prompt: string; list: TStrings; multi: boolean);
begin
  SelectedItems := TStringList.Create;
  SelectedItems.Clear();
  PromptMemo.Text := prompt;
  MenuListBox.Clear();
  MenuListBox.Items.AddStrings(list);
  MenuListBox.MultiSelect := multi;
end;

function TAskMenuForm.GetSelected(): TStrings;
begin
  Result := SelectedItems;
end;

procedure TAskMenuForm.OKButtonClick(Sender: TObject);
var
  i: integer;
  s: string;
begin
  SelectedItems.Clear();
  for i := 0 to (MenuListBox.Items.Count - 1) do
    if MenuListBox.Selected[i] then
    begin
      s := MenuListBox.Items.Strings[i];
      SelectedItems.Add(s);
    end;

  AskMenuForm.Close();
end;

procedure TAskMenuForm.CancelButtonClick(Sender: TObject);
begin
  SelectedItems.Clear();
  AskMenuForm.Close();
end;

end.
