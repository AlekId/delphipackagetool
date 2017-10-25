object FrmMain: TFrmMain
  Left = 262
  Top = 247
  Caption = 'Package Group Rebuilder/Installer'
  ClientHeight = 452
  ClientWidth = 1208
  Color = clBtnFace
  Constraints.MinHeight = 450
  Constraints.MinWidth = 572
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnHide = FormHide
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 247
    Width = 1208
    Height = 12
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 304
  end
  object stgFiles: TStringGrid
    Left = 0
    Top = 190
    Width = 1208
    Height = 57
    Align = alClient
    ColCount = 7
    Ctl3D = False
    Enabled = False
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goRowSelect]
    ParentCtl3D = False
    PopupMenu = ppmFilesGrid
    TabOrder = 0
    OnContextPopup = stgFilesContextPopup
    OnDblClick = stgFilesDblClick
    OnMouseMove = stgFilesMouseMove
    OnMouseUp = stgFilesMouseUp
    ColWidths = (
      64
      481
      110
      121
      118
      81
      499)
    RowHeights = (
      24
      24)
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1208
    Height = 190
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lblDelphiVersion: TLabel
      Left = 872
      Top = 0
      Width = 68
      Height = 13
      Caption = 'Delphi Version'
    end
    object lblOutputDirectory: TLabel
      Left = 240
      Top = 48
      Width = 117
      Height = 13
      Caption = 'Output Path (exe,dll files)'
    end
    object lblPackageGroupFile: TLabel
      Left = 240
      Top = 0
      Width = 236
      Height = 13
      Caption = 'Project-Group File <*.bpg/*.bdsgroup/*.groupproj>'
    end
    object lblPackageOutputDir: TLabel
      Left = 240
      Top = 96
      Width = 173
      Height = 13
      Caption = 'Output Path Packages (bpl,dcp files)'
    end
    object lblDcuPath: TLabel
      Left = 240
      Top = 144
      Width = 226
      Height = 13
      Caption = 'Output Path for Delphi Compiled Units (dcu files)'
    end
    object btnStart: TBitBtn
      Left = 8
      Top = 8
      Width = 209
      Height = 57
      Action = actRecompileAll
      Caption = 'Install All'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object cbxStartDelphi: TCheckBox
      Left = 8
      Top = 88
      Width = 217
      Height = 17
      Caption = 'Start Delphi when this application closes.'
      TabOrder = 1
      OnExit = cbxStartDelphiExit
    end
    object cbxSilentMode: TCheckBox
      Left = 8
      Top = 112
      Width = 209
      Height = 17
      Caption = 'Silent Mode'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnExit = cbxSilentModeExit
    end
    object cbxStopOnFailure: TCheckBox
      Left = 8
      Top = 136
      Width = 217
      Height = 17
      Caption = 'Stop on failure'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnExit = cbxStopOnFailureExit
    end
    object cbxDelphiVersions: TComboBox
      Left = 872
      Top = 16
      Width = 313
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      OnChange = cbxDelphiVersionsChange
    end
    object edtOutputDirectory: TEdit
      Left = 240
      Top = 64
      Width = 601
      Height = 21
      Hint = 'Enter/select the output path for exe/dll/bpl files.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnExit = edtPathExit
    end
    object btnLoadFile: TButton
      Left = 840
      Top = 16
      Width = 17
      Height = 21
      Action = actOpenProject
      TabOrder = 6
    end
    object btnSelectOutputDir: TButton
      Left = 840
      Top = 64
      Width = 17
      Height = 21
      Action = actSelectOutputDir
      TabOrder = 7
    end
    object edtPackageOutputDir: TEdit
      Left = 240
      Top = 112
      Width = 601
      Height = 21
      Hint = 'Enter/select the Package Path.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnExit = edtPathExit
    end
    object btnSelectPackageOutputDir: TButton
      Left = 840
      Top = 112
      Width = 17
      Height = 21
      Action = actSelectPackageOutputDir
      TabOrder = 9
    end
    object gbxPlatform: TGroupBox
      Left = 1040
      Top = 48
      Width = 145
      Height = 133
      Caption = 'Platform'
      TabOrder = 10
      DesignSize = (
        145
        133)
      object clbPlatform: TCheckListBox
        Left = 13
        Top = 21
        Width = 121
        Height = 100
        OnClickCheck = clbPlatformClickCheck
        Anchors = [akLeft, akTop, akRight, akBottom]
        BorderStyle = bsNone
        Color = clBtnFace
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object gbxConfig: TGroupBox
      Left = 872
      Top = 48
      Width = 145
      Height = 133
      Caption = 'Config'
      TabOrder = 11
      DesignSize = (
        145
        133)
      object clbConfig: TCheckListBox
        Left = 13
        Top = 21
        Width = 121
        Height = 100
        OnClickCheck = clbConfigClickCheck
        Anchors = [akLeft, akTop, akRight, akBottom]
        BorderStyle = bsNone
        Color = clBtnFace
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object edtDCUPath: TEdit
      Left = 240
      Top = 160
      Width = 601
      Height = 21
      Hint = 'Enter/select the Delphi DCU Path.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      OnExit = edtPathExit
    end
    object btnSelectDcuPath: TButton
      Left = 840
      Top = 160
      Width = 17
      Height = 21
      Action = actSelectDcuPath
      TabOrder = 13
    end
  end
  object pgcInfo: TPageControl
    Left = 0
    Top = 259
    Width = 1208
    Height = 193
    ActivePage = tabInfo
    Align = alBottom
    TabOrder = 3
    object tabInfo: TTabSheet
      Caption = 'Information'
      object mmoLogFile: TMemo
        Left = 0
        Top = 0
        Width = 1200
        Height = 165
        Align = alClient
        Ctl3D = False
        ParentCtl3D = False
        PopupMenu = pmnMessages
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        OnDblClick = mmoLogFileDblClick
        ExplicitLeft = -104
        ExplicitTop = -18
      end
    end
    object tabTrace: TTabSheet
      Caption = 'Trace'
      ImageIndex = 1
      object mmoTrace: TMemo
        Left = 0
        Top = 0
        Width = 1200
        Height = 165
        Align = alClient
        Ctl3D = False
        ParentCtl3D = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        OnDblClick = mmoTraceDblClick
      end
    end
  end
  object edtPackageBPGFile: TEdit
    Left = 240
    Top = 16
    Width = 601
    Height = 21
    TabOrder = 2
  end
  object ActionList1: TActionList
    Left = 1136
    Top = 8
    object actGetPackageListFromRegistry: TAction
      Caption = 'actGetPackageListFromRegistry'
      Enabled = False
    end
    object actOpenProject: TAction
      Caption = '...'
      Hint = 'Open a Project Group File (.bpg)'
      OnExecute = actOpenProjectExecute
    end
    object actSelectDelphiCompiler: TAction
      Caption = '...'
      Hint = 'Choose the Delphi compiler version.'
    end
    object actShowOptions: TAction
      Caption = 'Options'
      OnExecute = actShowOptionsExecute
    end
    object actShowProjectOptions: TAction
      Caption = 'Options'
      OnExecute = actShowProjectOptionsExecute
    end
    object actInitializeApp: TAction
      Caption = 'actInitializeApp'
    end
    object actCheckDelphiRunning: TAction
      Caption = 'actCheckDelphiRunning'
    end
    object actShowTraceFile: TAction
      Caption = 'Show Tracefile'
      ShortCut = 16468
      OnExecute = actShowTraceFileExecute
    end
    object actShowAbout: TAction
      Caption = 'About'
      OnExecute = actShowAboutExecute
    end
    object actFindFilePath: TAction
      Caption = 'Find'
      Hint = 'Find missing File'
      OnExecute = actFindFilePathExecute
    end
    object actOpenProjectWithDelphi: TAction
      Caption = 'Open Project in IDE'
      OnExecute = actOpenProjectWithDelphiExecute
    end
    object actShowFile: TAction
      Caption = 'Show File'
      Hint = 'Show File with External Editor'
      OnExecute = actShowFileExecute
    end
    object actCompileSelectedProjects: TAction
      Caption = 'Compile Selected Projects'
      ShortCut = 8312
      OnExecute = actCompileSelectedProjectsExecute
    end
    object actSelectOutputDir: TAction
      Caption = '...'
      OnExecute = actSelectOutputDirExecute
    end
    object actApplicationUpdate: TAction
      Caption = 'Web Update'
      OnExecute = actApplicationUpdateExecute
    end
    object actShowBPGEditor: TAction
      Caption = 'Edit'
      Hint = 'Edit an existing Package Group File. (.bpg)'
      OnExecute = actShowBPGEditorExecute
    end
    object actNewBPGFile: TAction
      Caption = 'New'
      Hint = 'Create a new Package Group File. (.bpg)'
      OnExecute = actNewBPGFileExecute
    end
    object actShowCFGFile: TAction
      Caption = 'Show cfg/bdsproj-file'
      OnExecute = actShowCFGFileExecute
    end
    object actShowProjectDir: TAction
      Caption = 'Show Project Directory'
      OnExecute = actShowProjectDirExecute
    end
    object actRemoveProject: TAction
      Caption = 'Remove Project/Package'
      OnExecute = actRemoveProjectExecute
    end
    object actCloseProject: TAction
      Caption = 'Close'
      Hint = 'Close the currently open BPG-File.'
      OnExecute = actCloseProjectExecute
    end
    object actExit: TAction
      Caption = 'Exit'
      OnExecute = actExitExecute
    end
    object actFindDCPandBPL: TAction
      Caption = 'Find Files *.dcp and *.bpl'
      OnExecute = actFindDCPandBPLExecute
    end
    object actResetDelphi: TAction
      Caption = 'Uninstall Packages'
      Hint = 'Uninstall Packages from the IDE.'
    end
    object actVerifyRegistry: TAction
      Caption = 'Verify Registry'
      Hint = 'Looks for inconistent registry keys in "Known Packages".'
      OnExecute = actVerifyRegistryExecute
    end
    object actSelectDcuPath: TAction
      Caption = '...'
      OnExecute = actSelectDcuPathExecute
    end
    object actShowDOFFile: TAction
      Caption = 'Show dof-file'
      OnExecute = actShowDOFFileExecute
    end
    object actSaveLog: TAction
      Caption = 'Save the Log-Output to a file.'
      OnExecute = actSaveLogExecute
    end
    object actBackupAll: TAction
      Caption = 'Backup All'
      OnExecute = actBackupAllExecute
    end
    object actShowOutputDir: TAction
      Caption = 'Show Output Directory'
      OnExecute = actShowOutputDirExecute
    end
    object actAutoBackup: TAction
      Caption = 'actAutoBackup'
    end
    object actRecompileAll: TAction
      Caption = 'actRecompileAll'
      OnExecute = actRecompileAllExecute
    end
    object actRevertChanges: TAction
      Caption = 'Revert Change'
      OnExecute = actRevertChangesExecute
    end
    object actSetVersionSelectedProjects: TAction
      Caption = 'Set Version'
      OnExecute = actSetVersionSelectedProjectsExecute
    end
    object actSelectAll: TAction
      Caption = 'actSelectAll'
      OnExecute = actSelectAllExecute
    end
    object actSelectPackageOutputDir: TAction
      Caption = '...'
      OnExecute = actSelectPackageOutputDirExecute
    end
    object actCompileProject: TAction
      Caption = 'Compile Project'
      ShortCut = 120
      OnExecute = actCompileProjectExecute
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.bpg'
    Filter = 
      'Delphi Package Group|*.bpg|BDS Group|*.bdsgroup|Group Files|*.bp' +
      'g;*.bdsgroup'
    Left = 1184
    Top = 8
  end
  object MainMenu1: TMainMenu
    Left = 1088
    Top = 16
    object F1: TMenuItem
      Caption = 'File'
      object NewPackageGroup1: TMenuItem
        Action = actNewBPGFile
      end
      object mitOpenFile: TMenuItem
        Caption = 'Load'
        Hint = 'Open a Project Group File (.bpg)'
        object mitRecentFiles: TMenuItem
          Action = actOpenProject
          Caption = 'Open...'
        end
      end
      object BPGEditor1: TMenuItem
        Action = actShowBPGEditor
      end
      object ProjectOptions2: TMenuItem
        Action = actShowProjectOptions
      end
      object actCloseProject1: TMenuItem
        Action = actCloseProject
      end
      object ShowProjectGroup1: TMenuItem
        Caption = 'Show'
        OnClick = ShowProjectGroup1Click
      end
      object ReInstallAllPackages1: TMenuItem
        Action = actRecompileAll
        Caption = 'Install All'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actExit
      end
    end
    object T1: TMenuItem
      Caption = 'Tools'
      object P1: TMenuItem
        Caption = 'Package'
        object U1: TMenuItem
          Action = DMMain.actUninstallPackage
        end
        object D1: TMenuItem
          Action = DMMain.actDeleteBPL
        end
        object C1: TMenuItem
          Action = actCompileProject
        end
        object I1: TMenuItem
          Action = DMMain.actInstallPackage
        end
      end
      object B1: TMenuItem
        Caption = 'Package Group'
        object UninstallAllPackages1: TMenuItem
          Action = DMMain.actUninstallAllPackages
        end
        object Deleteallfiles1: TMenuItem
          Action = DMMain.actDeleteFiles
        end
        object CompileAllPackages1: TMenuItem
          Action = DMMain.actCompileAllPackages
        end
        object InstallAllPackages1: TMenuItem
          Action = DMMain.actInstallAllPackages
        end
      end
      object CloseDelphi1: TMenuItem
        Action = DMMain.actShutDownDelphi
      end
      object StartDelphi1: TMenuItem
        Action = DMMain.actStartUpDelphi
      end
      object actResetDelphi1: TMenuItem
        Action = DMMain.actResetDelphi
      end
      object Find2: TMenuItem
        Action = actFindFilePath
      end
      object actVerifyRegistry1: TMenuItem
        Action = actVerifyRegistry
      end
    end
    object O1: TMenuItem
      Caption = 'Settings'
      object Options1: TMenuItem
        Action = actShowOptions
      end
      object LoadSkin1: TMenuItem
        Caption = 'Load Skin'
        Enabled = False
        Visible = False
      end
      object WebUpdate1: TMenuItem
        Action = actApplicationUpdate
      end
      object VersionHistory1: TMenuItem
        Caption = 'Read me'
        OnClick = VersionHistory1Click
      end
      object About1: TMenuItem
        Action = actShowAbout
      end
    end
  end
  object pmnMessages: TPopupMenu
    Left = 240
    Top = 368
    object ClearLog1: TMenuItem
      Caption = 'Clear Log'
      OnClick = ClearLog1Click
    end
    object Find1: TMenuItem
      Action = actFindFilePath
    end
    object RevertChange1: TMenuItem
      Action = actRevertChanges
    end
    object actSaveLog1: TMenuItem
      Action = actSaveLog
    end
    object actBackupAll1: TMenuItem
      Action = actBackupAll
    end
  end
  object ppmFilesGrid: TPopupMenu
    Left = 192
    Top = 232
    object OpenProjectinIDE1: TMenuItem
      Action = actOpenProjectWithDelphi
    end
    object ShowFile1: TMenuItem
      Action = actShowFile
      ShortCut = 8237
    end
    object Showcfgfile1: TMenuItem
      Action = actShowCFGFile
    end
    object actShowDOFFile1: TMenuItem
      Action = actShowDOFFile
    end
    object actCompileSelectedProjects1: TMenuItem
      Action = actCompileSelectedProjects
    end
    object InstallPackageinIDE1: TMenuItem
      Action = DMMain.actInstallPackage
    end
    object UninstallPackagefromIDE1: TMenuItem
      Action = DMMain.actUninstallPackage
    end
    object actWriteDPTPathsToProject1: TMenuItem
      Action = DMMain.actWriteDPTPathsToProject
    end
    object ExecuteApplication1: TMenuItem
      Action = DMMain.actExecuteApp
    end
    object FileExplorer1: TMenuItem
      Action = actShowProjectDir
    end
    object ShowOutputDirectory1: TMenuItem
      Action = actShowOutputDir
    end
    object RemoveProjectPackage1: TMenuItem
      Action = actRemoveProject
      ShortCut = 46
    end
    object actFindDCPandBPL1: TMenuItem
      Action = actFindDCPandBPL
    end
    object actCleanUpDelphi1: TMenuItem
      Action = actResetDelphi
    end
    object SetProjectVersion1: TMenuItem
      Action = actSetVersionSelectedProjects
    end
    object actRevertChanges1: TMenuItem
      Action = DMMain.actRevertChanges
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.ini'
    Filter = 'Configuration File|*.ini'
    Left = 280
    Top = 288
  end
end
