object Form1: TForm1
  Left = 0
  Top = 96
  Width = 944
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 936
    Height = 193
    ActivePage = tabD2009
    Align = alTop
    TabOrder = 0
    object tabD2006: TTabSheet
      Caption = 'tabD2006'
      object lblFilename2006: TLabel
        Left = 12
        Top = 8
        Width = 42
        Height = 13
        Caption = 'Filename'
      end
      object lblStmt2006: TLabel
        Left = 12
        Top = 62
        Width = 48
        Height = 13
        Caption = 'Statement'
      end
      object cbxFilename2006: TComboBox
        Left = 12
        Top = 27
        Width = 785
        Height = 21
        ItemHeight = 13
        TabOrder = 0
      end
      object edtStmt2006: TEdit
        Left = 12
        Top = 80
        Width = 809
        Height = 21
        TabOrder = 1
      end
    end
    object tabD2007: TTabSheet
      Caption = 'tabD2007'
      ImageIndex = 1
      object lblFilename2007: TLabel
        Left = 12
        Top = 8
        Width = 42
        Height = 13
        Caption = 'Filename'
      end
      object lblstmt2007: TLabel
        Left = 12
        Top = 62
        Width = 48
        Height = 13
        Caption = 'Statement'
      end
      object cbxFilename2007: TComboBox
        Left = 12
        Top = 27
        Width = 785
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = '..\..\d2007\test.groupproj'
      end
      object edtStmt2007: TEdit
        Left = 12
        Top = 80
        Width = 809
        Height = 21
        TabOrder = 1
        Text = '//ItemGroup[@Projects[0]]/'
      end
    end
    object tabD2009: TTabSheet
      Caption = 'tabD2009'
      ImageIndex = 2
      object lblFilename2009: TLabel
        Left = 12
        Top = 8
        Width = 42
        Height = 13
        Caption = 'Filename'
      end
      object lblStmt2009: TLabel
        Left = 12
        Top = 62
        Width = 48
        Height = 13
        Caption = 'Statement'
      end
      object cbxFilename2009: TComboBox
        Left = 12
        Top = 27
        Width = 785
        Height = 21
        ItemHeight = 13
        TabOrder = 0
      end
      object edtStmt2009: TEdit
        Left = 12
        Top = 80
        Width = 809
        Height = 21
        TabOrder = 1
      end
    end
    object tabD2010: TTabSheet
      Caption = 'tabD2010'
      ImageIndex = 3
      object lblFilename2010: TLabel
        Left = 12
        Top = 8
        Width = 42
        Height = 13
        Caption = 'Filename'
      end
      object lblStmt2010: TLabel
        Left = 12
        Top = 64
        Width = 48
        Height = 13
        Caption = 'Statement'
      end
      object edtStmt2010: TEdit
        Left = 12
        Top = 80
        Width = 809
        Height = 21
        TabOrder = 0
        Text = '//PropertyGroup[@Condition="'#39'$(Base)'#39'!='#39#39'"]/DCC_UnitSearchPath'
      end
      object cbxFilename2010: TComboBox
        Left = 12
        Top = 24
        Width = 785
        Height = 21
        ItemHeight = 13
        TabOrder = 1
        Text = '..\..\d2010\project1\project1.dproj'
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 193
    Width = 936
    Height = 41
    Align = alTop
    TabOrder = 1
    object btnExecute: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'btnExecute'
      TabOrder = 0
      OnClick = btnExecuteClick
    end
    object edtValue: TEdit
      Left = 112
      Top = 16
      Width = 713
      Height = 21
      TabOrder = 1
    end
  end
  object mmoFile: TMemo
    Left = 0
    Top = 234
    Width = 936
    Height = 239
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 2
  end
end
