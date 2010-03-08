object Form1: TForm1
  Left = -4
  Top = -4
  Width = 1288
  Height = 780
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
  object Splitter1: TSplitter
    Left = 850
    Top = 201
    Height = 552
  end
  object mmoFile: TMemo
    Left = 0
    Top = 201
    Width = 425
    Height = 552
    Align = alLeft
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1280
    Height = 201
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 1
    object filename: TLabel
      Left = 16
      Top = 48
      Width = 39
      Height = 13
      Caption = 'filename'
    end
    object lblstatement: TLabel
      Left = 16
      Top = 96
      Width = 46
      Height = 13
      Caption = 'statement'
    end
    object lbldata: TLabel
      Left = 16
      Top = 144
      Width = 23
      Height = 13
      Caption = 'Data'
    end
    object Button: TButton
      Left = 16
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Button'
      TabOrder = 0
      OnClick = ButtonClick
    end
    object cbxfilename: TComboBox
      Left = 16
      Top = 64
      Width = 729
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = '..\..\D2010\project1\Project1.dproj'
      Items.Strings = (
        '..\..\D2010\project1\Project1.dproj'
        '..\..\D2007\project1\Project1.dproj'
        '..\..\D2006\Project1\Project1.bdsproj')
    end
    object edtValue: TEdit
      Left = 16
      Top = 160
      Width = 737
      Height = 21
      TabOrder = 2
    end
    object cbxstatement: TComboBox
      Left = 16
      Top = 112
      Width = 729
      Height = 21
      ItemHeight = 13
      TabOrder = 3
      Text = '(PropertyGroup Condition="'#39'$(Base)'#39'!='#39#39'").DCC_ExeOutput'
      Items.Strings = (
        'D2006: ')
    end
  end
  object mmoStatement: TMemo
    Left = 425
    Top = 201
    Width = 425
    Height = 552
    Align = alLeft
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object OpenDialog1: TOpenDialog
    Left = 80
    Top = 32
  end
end
