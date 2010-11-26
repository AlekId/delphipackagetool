object FrmOptions: TFrmOptions
  Left = 268
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 701
  ClientWidth = 707
  Color = clBtnFace
  Constraints.MinHeight = 612
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 58
    Height = 13
    Caption = 'Search path'
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 707
    Height = 660
    Align = alClient
    TabOrder = 4
    object btnSelectCodeEditor: TSpeedButton
      Left = 480
      Top = 408
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = btnSelectCodeEditorClick
    end
    object lblSourceCodeEditor: TLabel
      Left = 8
      Top = 392
      Width = 92
      Height = 13
      Caption = 'Source Code Editor'
    end
    object lblSearchPath: TLabel
      Left = 8
      Top = 8
      Width = 56
      Height = 13
      Caption = 'Library Path'
    end
    object lblCompilerSwitches: TLabel
      Left = 8
      Top = 312
      Width = 86
      Height = 13
      Caption = 'Compiler Switches'
    end
    object lblBeforeInstallAll: TLabel
      Left = 8
      Top = 472
      Width = 104
      Height = 13
      Caption = 'On Before <Install All>'
    end
    object btnSelectOnBeforeInstallAll: TSpeedButton
      Left = 656
      Top = 488
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = btnSelectOnBeforeInstallAllClick
    end
    object lblAfterInstallAll: TLabel
      Left = 8
      Top = 512
      Width = 95
      Height = 13
      Caption = 'On After <Install All>'
    end
    object btnOnAfterInstallAll: TSpeedButton
      Left = 656
      Top = 528
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = btnOnAfterInstallAllClick
    end
    object lblLibSuffix: TLabel
      Left = 8
      Top = 352
      Width = 43
      Height = 13
      Caption = 'Lib-Suffix'
    end
    object lblSourceEditorParams: TLabel
      Left = 520
      Top = 392
      Width = 53
      Height = 13
      Caption = 'Parameters'
    end
    object lblDiffTool: TLabel
      Left = 8
      Top = 432
      Width = 40
      Height = 13
      Caption = 'Diff-Tool'
    end
    object btnDiffTool: TSpeedButton
      Left = 656
      Top = 448
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = btnDiffToolClick
    end
    object lblOnBeforeBuildProject: TLabel
      Left = 8
      Top = 552
      Width = 187
      Height = 13
      Caption = 'On Before <Build Project/Package/Dll>'
    end
    object btnOnBeforeBuildProject: TSpeedButton
      Left = 656
      Top = 568
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = btnOnBeforeBuildProjectClick
    end
    object edtCodeEditor: TEdit
      Left = 8
      Top = 408
      Width = 465
      Height = 21
      Hint = 'Define here your favorit source code editor.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object edtCompilerSwitches: TEdit
      Left = 8
      Top = 328
      Width = 641
      Height = 21
      TabOrder = 1
      Text = '-B -Q -W -H'
    end
    object btnAddpath: TBitBtn
      Left = 8
      Top = 272
      Width = 233
      Height = 25
      Caption = 'Add Path'
      TabOrder = 2
      OnClick = btnAddPathClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
        333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
        300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
        333337F373F773333333303330033333333337F3377333333333303333333333
        333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
        333337777F337F33333330330BB00333333337F373F773333333303330033333
        333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
        333377777F77377733330BBB0333333333337F337F33333333330BB003333333
        333373F773333333333330033333333333333773333333333333}
      NumGlyphs = 2
    end
    object edtBeforeInstallAll: TEdit
      Left = 8
      Top = 488
      Width = 641
      Height = 21
      Hint = 
        'The file defined here will be executed when button <Install All>' +
        ' is pressed.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object edtAfterInstallAll: TEdit
      Left = 8
      Top = 528
      Width = 641
      Height = 21
      Hint = 
        'The file defined here will be executed when button <Install All>' +
        ' is pressed and all projects are compiled successfully.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object cbxAutomaticShowAddPathDialog: TCheckBox
      Left = 272
      Top = 608
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
    object cbxCreateBatchFile: TCheckBox
      Left = 544
      Top = 608
      Width = 145
      Height = 17
      Hint = 'Create the Install-Batch and .Reg-Files'
      Caption = 'Create Install-Batch File'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
    end
    object cbxChangeFiles: TCheckBox
      Left = 8
      Top = 608
      Width = 233
      Height = 17
      Hint = 
        'WARNING: If this box is checked, then the Delphi Package Tool wi' +
        'll change your cfg and dof-files.'
      Caption = 'Allow DelphiPackageTool to change files'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
    end
    object cbxModifyEnvironment: TCheckBox
      Left = 8
      Top = 632
      Width = 185
      Height = 17
      Hint = 
        'If enabled, then the PackageTool tries to add the path to the bp' +
        'l-files into the environment settings.'
      Caption = 'Modify Environment Settings.'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 8
    end
    object edtLibSuffix: TEdit
      Left = 8
      Top = 368
      Width = 121
      Height = 21
      TabOrder = 9
    end
    object edtSourceEditorParams: TEdit
      Left = 520
      Top = 408
      Width = 158
      Height = 21
      Hint = 
        'Define here the command line parameters. Valid Placeholders are ' +
        '%FILENAME% and %LINENO%.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      Text = '%FILENAME%'
    end
    object cbxAutoBackup: TCheckBox
      Left = 272
      Top = 632
      Width = 185
      Height = 17
      Hint = 
        'After a successfully "Install All", create a backup zip-file of ' +
        'the sources.'
      Caption = 'Create Backup zip-file'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 11
      OnExit = cbxAutoBackupExit
    end
    object edtDiffTool: TEdit
      Left = 8
      Top = 448
      Width = 641
      Height = 21
      Hint = 'Define here your favorit source code editor.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
    end
    object edtOnBeforeBuildProject: TEdit
      Left = 8
      Top = 568
      Width = 641
      Height = 21
      Hint = 
        'The file defined here will be executed when button <Install All>' +
        ' is pressed.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
    end
    object cbxTrace: TCheckBox
      Left = 544
      Top = 632
      Width = 185
      Height = 17
      Hint = 'Disable/Enable Trace'
      Caption = 'Trace'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 14
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 660
    Width = 707
    Height = 41
    Align = alBottom
    TabOrder = 3
    object btnOk: TBitBtn
      Left = 512
      Top = 8
      Width = 90
      Height = 25
      TabOrder = 0
      OnClick = btnOkClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 608
      Top = 8
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
    object cbxLanguage: TComboBox
      Left = 8
      Top = 8
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      Items.Strings = (
        'default'
        'english'
        'german'
        'french')
    end
  end
  object mmoSearchPath: TMemo
    Left = 8
    Top = 24
    Width = 689
    Height = 233
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btnAddDefaultPath: TBitBtn
    Left = 480
    Top = 272
    Width = 217
    Height = 25
    Caption = 'Add Delphi Default Directories'
    TabOrder = 1
    OnClick = btnAddDefaultPathClick
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
      333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
      300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
      333337F373F773333333303330033333333337F3377333333333303333333333
      333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
      333337777F337F33333330330BB00333333337F373F773333333303330033333
      333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
      333377777F77377733330BBB0333333333337F337F33333333330BB003333333
      333373F773333333333330033333333333333773333333333333}
    NumGlyphs = 2
  end
  object btnVerifyDirectories: TBitBtn
    Left = 248
    Top = 272
    Width = 225
    Height = 25
    Caption = 'Verify Directories'
    TabOrder = 2
    OnClick = btnVerifyDirectoriesClick
  end
  object OpenDialog1: TOpenDialog
    Left = 240
    Top = 360
  end
end
