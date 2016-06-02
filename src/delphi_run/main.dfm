object MainForm: TMainForm
  Left = 191
  Top = 125
  Width = 452
  Height = 317
  Caption = 'MainForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Solution: TMemo
    Left = 0
    Top = 0
    Width = 337
    Height = 273
    Lines.Strings = (
      'Solution')
    TabOrder = 0
  end
  object RunButton: TButton
    Left = 352
    Top = 8
    Width = 81
    Height = 33
    Caption = 'Run'
    TabOrder = 1
    OnClick = RunButtonClick
  end
  object Exit: TBitBtn
    Left = 352
    Top = 48
    Width = 81
    Height = 33
    Caption = 'Exit'
    TabOrder = 2
    OnClick = ExitClick
  end
  object ls: TLSEngine
    Left = 352
    Top = 88
  end
end
