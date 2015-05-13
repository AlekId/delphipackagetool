object FrmProjectOptions: TFrmProjectOptions
  Left = 651
  Top = 26
  BorderStyle = bsDialog
  Caption = 'Project Options'
  ClientHeight = 550
  ClientWidth = 710
  Color = clBtnFace
  Constraints.MinHeight = 550
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
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 710
    Height = 509
    Align = alClient
    TabOrder = 1
    object lblSearchPath: TLabel
      Left = 8
      Top = 8
      Width = 56
      Height = 13
      Caption = 'Library Path'
    end
    object lblCompilerSwitches: TLabel
      Left = 8
      Top = 328
      Width = 86
      Height = 13
      Caption = 'Compiler Switches'
    end
    object lblBeforeInstallAll: TLabel
      Left = 8
      Top = 408
      Width = 104
      Height = 13
      Caption = 'On Before <Install All>'
    end
    object btnSelectOnBeforeInstallAll: TSpeedButton
      Left = 656
      Top = 424
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = btnSelectOnBeforeInstallAllClick
    end
    object lblAfterInstallAll: TLabel
      Left = 8
      Top = 448
      Width = 95
      Height = 13
      Caption = 'On After <Install All>'
    end
    object btnOnAfterInstallAll: TSpeedButton
      Left = 656
      Top = 464
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = btnOnAfterInstallAllClick
    end
    object lblLibSuffix: TLabel
      Left = 8
      Top = 288
      Width = 43
      Height = 13
      Caption = 'Lib-Suffix'
    end
    object lblDebugCompilerSwitches: TLabel
      Left = 8
      Top = 368
      Width = 121
      Height = 13
      Caption = 'Debug Compiler Switches'
    end
    object lblReleaseCompilerSwitches: TLabel
      Left = 349
      Top = 368
      Width = 128
      Height = 13
      Caption = 'Release Compiler Switches'
    end
    object edtCompilerSwitches: TEdit
      Left = 8
      Top = 344
      Width = 641
      Height = 21
      TabOrder = 1
      Text = '-B -Q -W -H'
    end
    object btnAddpath: TBitBtn
      Left = 8
      Top = 256
      Width = 233
      Height = 25
      Caption = 'Add Path'
      TabOrder = 6
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
      Top = 424
      Width = 641
      Height = 21
      Hint = 
        'The file defined here will be executed when button <Install All>' +
        ' is pressed.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object edtAfterInstallAll: TEdit
      Left = 8
      Top = 464
      Width = 641
      Height = 21
      Hint = 
        'The file defined here will be executed when button <Install All>' +
        ' is pressed and all projects are compiled successfully.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
    end
    object edtLibSuffix: TEdit
      Left = 8
      Top = 304
      Width = 121
      Height = 21
      Hint = 'Enter the Lib-Suffix for the Package-Names.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object edtDebugCompilerSwitches: TEdit
      Left = 8
      Top = 384
      Width = 300
      Height = 21
      Hint = 
        'Enter the compiler switches used for debug config, if there is n' +
        'o *.cfg or *.dproj'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object edtReleaseCompilerSwitches: TEdit
      Left = 349
      Top = 384
      Width = 300
      Height = 21
      Hint = 
        'Enter the compiler switches used for release config, if there is' +
        ' no *.cfg or *.dproj'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object cbxAutoBackup: TCheckBox
      Left = 150
      Top = 304
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
      TabOrder = 7
      OnExit = cbxAutoBackupExit
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 509
    Width = 710
    Height = 41
    Align = alBottom
    TabOrder = 0
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
  end
  object mmoSearchPath: TMemo
    Left = 8
    Top = 24
    Width = 689
    Height = 225
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object btnAddDefaultPath: TBitBtn
    Left = 480
    Top = 256
    Width = 217
    Height = 25
    Caption = 'Add Delphi Default Directories'
    TabOrder = 3
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
    Top = 256
    Width = 225
    Height = 25
    Caption = 'Verify Directories'
    TabOrder = 2
    OnClick = btnVerifyDirectoriesClick
  end
  object OpenDialog1: TOpenDialog
    Left = 632
    Top = 296
  end
end
