object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Unit Test "Path and Filename"'
  ClientHeight = 477
  ClientWidth = 1147
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1147
    Height = 41
    Align = alTop
    Caption = 'pnlTop'
    TabOrder = 0
  end
  object mmoLog: TMemo
    Left = 0
    Top = 352
    Width = 1147
    Height = 125
    Align = alBottom
    Lines.Strings = (
      'mmoLog')
    TabOrder = 1
  end
  object pgcMain: TPageControl
    Left = 0
    Top = 41
    Width = 1147
    Height = 311
    ActivePage = tabAbsolutePath
    Align = alClient
    TabOrder = 2
    object tabAbsoluteFilename: TTabSheet
      Caption = 'AbsoluteFilename'
      object edtBasePath_abpf: TLabeledEdit
        Left = 20
        Top = 36
        Width = 397
        Height = 21
        EditLabel.Width = 48
        EditLabel.Height = 13
        EditLabel.Caption = 'Base Path'
        TabOrder = 0
        Text = 'C:\projects\mrp8'
      end
      object edtRelativeFilename_abpf: TLabeledEdit
        Left = 20
        Top = 96
        Width = 541
        Height = 21
        EditLabel.Width = 84
        EditLabel.Height = 13
        EditLabel.Caption = 'Relative Filename'
        TabOrder = 1
        Text = '..\..\Packages\Downloads\ShellControls\vclshlctrls.dpk'
      end
      object btnAbsoluteFilename: TButton
        Left = 24
        Top = 144
        Width = 129
        Height = 25
        Caption = 'btnAbsoluteFilename'
        TabOrder = 2
        OnClick = btnAbsoluteFilenameClick
      end
      object edtOutput_abpf: TLabeledEdit
        Left = 24
        Top = 216
        Width = 801
        Height = 21
        EditLabel.Width = 78
        EditLabel.Height = 13
        EditLabel.Caption = 'edtOutput_abpf'
        TabOrder = 3
      end
    end
    object tabAbsolutePath: TTabSheet
      Caption = 'tabAbsolutePath'
      ImageIndex = 1
      object edtBasePath_abp: TLabeledEdit
        Left = 24
        Top = 32
        Width = 345
        Height = 21
        EditLabel.Width = 48
        EditLabel.Height = 13
        EditLabel.Caption = 'Base Path'
        TabOrder = 0
        Text = 'C:\projects\mrp8'
      end
      object edtRelativePath_abp: TLabeledEdit
        Left = 24
        Top = 88
        Width = 345
        Height = 21
        EditLabel.Width = 64
        EditLabel.Height = 13
        EditLabel.Caption = 'Relative Path'
        TabOrder = 1
        Text = '..\..\Packages\Downloads\ShellControls\'
      end
    end
  end
  object btnAbsolutePath: TButton
    Left = 28
    Top = 200
    Width = 173
    Height = 25
    Caption = 'btnAbsolutePath'
    TabOrder = 3
    OnClick = btnAbsolutePathClick
  end
  object edtOutput_abp: TLabeledEdit
    Left = 28
    Top = 264
    Width = 573
    Height = 21
    EditLabel.Width = 34
    EditLabel.Height = 13
    EditLabel.Caption = 'Output'
    TabOrder = 4
  end
end
