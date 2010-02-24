object Form1: TForm1
  Left = 0
  Top = 96
  Width = 800
  Height = 500
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button: TButton
    Left = 184
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Button'
    TabOrder = 0
    OnClick = ButtonClick
  end
  object edtStmt: TEdit
    Left = 40
    Top = 96
    Width = 729
    Height = 21
    TabOrder = 1
    Text = 
      '<Configuration Condition=" '#39'$(Configuration)'#39' == '#39#39' ">Debug</Con' +
      'figuration>'
  end
  object edtValue: TEdit
    Left = 40
    Top = 184
    Width = 737
    Height = 21
    TabOrder = 2
    Text = 'edtValue'
  end
end
