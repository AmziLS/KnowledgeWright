object AskMenuForm: TAskMenuForm
  Left = 644
  Top = 126
  Width = 455
  Height = 372
  Caption = 'Question'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object PromptMemo: TMemo
    Left = 8
    Top = 8
    Width = 345
    Height = 113
    Lines.Strings = (
      'Prompt')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object MenuListBox: TListBox
    Left = 8
    Top = 128
    Width = 345
    Height = 201
    ItemHeight = 16
    TabOrder = 1
  end
  object OKButton: TButton
    Left = 360
    Top = 8
    Width = 81
    Height = 33
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 360
    Top = 48
    Width = 81
    Height = 33
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = CancelButtonClick
  end
end
