object FrmOptions: TFrmOptions
  Left = 447
  Top = 343
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 280
  ClientWidth = 710
  Color = clBtnFace
  Constraints.MinHeight = 280
  Constraints.MinWidth = 710
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 710
    Height = 239
    Align = alClient
    TabOrder = 1
    object btnSelectCodeEditor: TSpeedButton
      Left = 479
      Top = 80
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = btnSelectCodeEditorClick
    end
    object lblSourceCodeEditor: TLabel
      Left = 8
      Top = 64
      Width = 92
      Height = 13
      Caption = 'Source Code Editor'
    end
    object lblCompilerSwitches: TLabel
      Left = 8
      Top = 8
      Width = 86
      Height = 13
      Caption = 'Compiler Switches'
    end
    object lblSourceEditorParams: TLabel
      Left = 520
      Top = 64
      Width = 53
      Height = 13
      Caption = 'Parameters'
    end
    object lblDiffTool: TLabel
      Left = 8
      Top = 104
      Width = 40
      Height = 13
      Caption = 'Diff-Tool'
    end
    object btnDiffTool: TSpeedButton
      Left = 656
      Top = 120
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = btnDiffToolClick
    end
    object edtCodeEditor: TEdit
      Left = 8
      Top = 80
      Width = 465
      Height = 21
      Hint = 'Define here your favorit source code editor.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
    object edtCompilerSwitches: TEdit
      Left = 8
      Top = 24
      Width = 641
      Height = 21
      TabOrder = 0
      Text = '-B -Q -W -H'
    end
    object cbxAutomaticShowAddPathDialog: TCheckBox
      Left = 272
      Top = 160
      Width = 185
      Height = 17
      Hint = 
        'If a compilation fails because a file was not found then the "Ad' +
        'd Path Dialog" opens automatically.'
      Caption = 'Automatic show Add Path Dialog '
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 5
    end
    object cbxAllowToChangeFiles: TCheckBox
      Left = 8
      Top = 160
      Width = 233
      Height = 17
      Hint = 
        'INFO: If this box is checked, then the Delphi Package Tool will ' +
        'change your cfg,dof,dproj-files.'
      Caption = 'Allow DelphiPackageTool to change files'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object edtSourceEditorParams: TEdit
      Left = 520
      Top = 80
      Width = 158
      Height = 21
      Hint = 
        'Define here the command line parameters. Valid Placeholders are ' +
        '%FILENAME% and %LINENO%.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = '%FILENAME%'
    end
    object edtDiffTool: TEdit
      Left = 8
      Top = 120
      Width = 641
      Height = 21
      Hint = 'Define here your favorite Diff-Tool.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object cbxTrace: TCheckBox
      Left = 544
      Top = 184
      Width = 120
      Height = 17
      Hint = 'Disable/Enable Trace'
      Caption = 'Trace'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
    end
    object cbxBackupSourceOnly: TCheckBox
      Left = 8
      Top = 208
      Width = 185
      Height = 17
      Hint = 'Only backup source files. No compiled stuff.'
      Caption = 'Only add source-file to backup'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 7
    end
    object cbxShowChangedFileInDiffTool: TCheckBox
      Left = 8
      Top = 184
      Width = 233
      Height = 17
      Hint = 
        'INFO: If this box is checked, then the changed files will be dis' +
        'played in the external Diff-Tool.'
      Caption = 'Show changed files in Diff-Tool.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 239
    Width = 710
    Height = 41
    Align = alBottom
    TabOrder = 0
    object btnOk: TBitBtn
      Left = 512
      Top = 6
      Width = 90
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 608
      Top = 6
      Width = 90
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333333333000033338833333333333333333F333333333333
        0000333911833333983333333388F333333F3333000033391118333911833333
        38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
        911118111118333338F3338F833338F3000033333911111111833333338F3338
        3333F8330000333333911111183333333338F333333F83330000333333311111
        8333333333338F3333383333000033333339111183333333333338F333833333
        00003333339111118333333333333833338F3333000033333911181118333333
        33338333338F333300003333911183911183333333383338F338F33300003333
        9118333911183333338F33838F338F33000033333913333391113333338FF833
        38F338F300003333333333333919333333388333338FFF830000333333333333
        3333333333333333333888330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 344
    Top = 48
  end
end
