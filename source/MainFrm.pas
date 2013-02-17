{-----------------------------------------------------------------------------
 Unit Name: MainFrm
 Author:    Samuel Herzog
 Purpose:
 History:

1.9.1.10   ( 17.02.2013 )
-SH: fixes minor bug on updating the main-form.

1.9.1.9    ( 30.01.2013 )
- fixed bug in RelativePaths.

1.9.1.8    ( 22.01.2013 )
- huge rework received from M.Mueller to use
  msbuild instead of dcc32 for delphi 2007 and newer.

1.9.1.7    ( 27.12.2012 )
-SH: code-cleanup. removed/commented un-used code.
-SH: change about dcp-path to be backwards compatible with existing projects.

1.9.1.6    ( 21.12.2012 )
-patch from M.Mueller. Right-Click and Compile.

1.9.1.5    ( 19.12.2012 )
-SH: some code-cleanup and re-factoring.
-SH: Logic and GUI are now separated.
-SH: ESC is working again to abort "Compile All".

1.9.1.4    ( 14.12.2012 )
-another patch from M.Mueller. Code clean-up.
-Replace-Tag's right before feeding the command-line.

1.9.1.3    ( 11.12.2012 )
-SH: some code-cleanup and re-factoring.
 
1.9.1.2    ( 07.12.2012 )
-another patch from M.Mueller. Code clean-up.

1.9.1.1    ( 04.12.2012 )
-another patch from M.Mueller to make F9,Shift-F9 work again.

1.9.1.0    ( 30.11.2012 )
- huge rework received from M.Mueller to support platforms and build modes of newer delphi versions.
***********************************************
1.9.0.173  ( 19.11.2012 )
-SH: some code-cleanup and re-factoring.
-SH: fix in CleanupByRegistry.
1.9.0.172  ( 10.10.2012 )
- SH: some code-cleanup and re-factoring.

1.9.0.171  ( 08.10.2012 )
- SH: applied patch received from M.Mueller for detecting Delphi 2010
      and resolved some warnings in XE2.

1.9.0.170  ( 15.09.2012 )
- SH: add definitions for Delphi XE3. (not tested because I do not own XE3).

1.9.0.168  ( 15.02.2012 )
-SH: applied patch received from Andreas Heim about BDSBin-Tag for Delphi XE2.
 
1.9.0.167  ( 31.01.2012 )
-SH: added setting to determ if non-source-files (e.g. dcu,bpl,dcp) shall be added to backup-zip or not.

1.9.0.166  ( 27.01.2012 )
-SH: improvement in function <Get7zAppName> to find 7z.exe which is used for backup-task.

1.9.0.165  ( 01.11.2011 )
-SH: more changes when reading/writing registry settings. Try first <HKEY_LOCAL_MACHINE> and then <HKEY_CURRENT_USER>.
-SH: more cleanup of trace messages.
-----------------------------------------------------------------------------}

unit MainFrm;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  ActnList,
  StdCtrls,
  Buttons,
  Menus,
  Grids,
  uDPTSettings,
{$ifdef withTrace}
  cNVBTraceFile,
{$endif}  
  uDPTAppExec,
  ExtCtrls,
  uDPTDelphiPackage,
  uDPTDefinitions,
  ComCtrls,
  CheckLst;

type

  TFrmMain = class(TForm)
    ActionList1: TActionList;
    actGetPackageListFromRegistry: TAction;
    actOpenProject: TAction;
    actSelectDelphiCompiler: TAction;
    MainMenu1: TMainMenu;
    B1: TMenuItem;
    P1: TMenuItem;
    U1: TMenuItem;
    D1: TMenuItem;
    C1: TMenuItem;
    I1: TMenuItem;
    UninstallAllPackages1: TMenuItem;
    Deleteallfiles1: TMenuItem;
    CompileAllPackages1: TMenuItem;
    InstallAllPackages1: TMenuItem;
    T1: TMenuItem;
    F1: TMenuItem;
    mitOpenFile: TMenuItem;
    ReInstallAllPackages1: TMenuItem;
    OpenDialog1: TOpenDialog;
    actShowOptions: TAction;
    O1: TMenuItem;
    Options1: TMenuItem;
    CloseDelphi1: TMenuItem;
    StartDelphi1: TMenuItem;
    actCheckDelphiRunning: TAction;
    stgFiles: TStringGrid;
    actShowTraceFile: TAction;
    pmnMessages: TPopupMenu;
    ClearLog1: TMenuItem;
    About1: TMenuItem;
    actShowAbout: TAction;
    LoadSkin1: TMenuItem;
    actFindFilePath: TAction;
    Find1: TMenuItem;
    pnlTop: TPanel;
    btnStart: TBitBtn;
    cbxStartDelphi: TCheckBox;
    cbxSilentMode: TCheckBox;
    cbxStopOnFailure: TCheckBox;
    cbxDelphiVersions: TComboBox;
    lblDelphiVersion: TLabel;
    lblPackageDirectory: TLabel;
    edtPackageBPLDirectory: TEdit;
    lblPackageGroupFile: TLabel;
    btnLoadFile: TButton;
    Find2: TMenuItem;
    actOpenProjectWithDelphi: TAction;
    ppmFilesGrid: TPopupMenu;
    OpenProjectinIDE1: TMenuItem;
    actShowFile: TAction;
    ShowFile1: TMenuItem;
    Splitter1: TSplitter;
    actCompileSelectedProjects: TAction;
    actCompileSelectedProjects1: TMenuItem;
    btnSetPackagePath: TButton;
    actSelectPackageBPLPath: TAction;
    actApplicationUpdate: TAction;
    WebUpdate1: TMenuItem;
    VersionHistory1: TMenuItem;
    InstallPackageinIDE1: TMenuItem;
    UninstallPackagefromIDE1: TMenuItem;
    ExecuteApplication1: TMenuItem;
    SaveDialog1: TSaveDialog;
    actShowBPGEditor: TAction;
    BPGEditor1: TMenuItem;
    actNewBPGFile: TAction;
    NewPackageGroup1: TMenuItem;
    actShowCFGFile: TAction;
    Showcfgfile1: TMenuItem;
    actShowProjectDir: TAction;
    FileExplorer1: TMenuItem;
    actRemoveProject: TAction;
    RemoveProjectPackage1: TMenuItem;
    actCloseProject: TAction;
    actCloseProject1: TMenuItem;
    actExit: TAction;
    N2: TMenuItem;
    Exit1: TMenuItem;
    actFindDCPandBPL: TAction;
    actFindDCPandBPL1: TMenuItem;
    mitRecentFiles: TMenuItem;
    actResetDelphi: TAction;
    actCleanUpDelphi1: TMenuItem;
    actVerifyRegistry: TAction;
    actVerifyRegistry1: TMenuItem;
    edtDCPPath: TEdit;
    btnSelectDcpPath: TButton;
    lblDcpPath: TLabel;
    actSelectDcuPath: TAction;
    actShowDOFFile: TAction;
    actShowDOFFile1: TMenuItem;
    actSaveLog: TAction;
    actSaveLog1: TMenuItem;
    actBackupAll: TAction;
    actBackupAll1: TMenuItem;
    actShowOutputDir: TAction;
    ShowOutputDirectory1: TMenuItem;
    actResetDelphi1: TMenuItem;
    edtPackageBPGFile: TComboBox;
    ShowProjectGroup1: TMenuItem;
    actAutoBackup: TAction;
    actRecompileAll: TAction;
    actRevertChanges: TAction;
    RevertChange1: TMenuItem;
    SetProjectVersion1: TMenuItem;
    actSetVersionSelectedProjects: TAction;
    actSelectAll: TAction;
    actRevertChanges1: TMenuItem;
    actWriteDPTPathsToProject1: TMenuItem;
    pgcInfo: TPageControl;
    tabInfo: TTabSheet;
    tabTrace: TTabSheet;
    mmoLogFile: TMemo;
    mmoTrace: TMemo;
    gbxPlatform: TGroupBox;
    gbxConfig: TGroupBox;
    clbConfig: TCheckListBox;
    clbPlatform: TCheckListBox;
    edtDCUPath: TEdit;
    btnSelectDcuPath: TButton;
    lblDcuPath: TLabel;
    actSelectDcpPath: TAction;
    actCompileProject: TAction;
    procedure FormShow(Sender: TObject);
    procedure actOpenProjectExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actShowOptionsExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actShowTraceFileExecute(Sender: TObject);
    procedure ClearLog1Click(Sender: TObject);
    procedure actShowAboutExecute(Sender: TObject);
    procedure stgFilesDblClick(Sender: TObject);
    procedure actFindFilePathExecute(Sender: TObject);
    procedure mmoLogFileDblClick(Sender: TObject);
    procedure cbxDelphiVersionsChange(Sender: TObject);
    procedure actOpenProjectWithDelphiExecute(Sender: TObject);
    procedure actShowFileExecute(Sender: TObject);
    procedure actCompileSelectedProjectsExecute(Sender: TObject);
    procedure actSelectPackageBPLPathExecute(Sender: TObject);
    procedure actApplicationUpdateExecute(Sender: TObject);
    procedure VersionHistory1Click(Sender: TObject);
    procedure stgFilesClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure actShowBPGEditorExecute(Sender: TObject);
    procedure actNewBPGFileExecute(Sender: TObject);
    procedure actShowCFGFileExecute(Sender: TObject);
    procedure actShowProjectDirExecute(Sender: TObject);
    procedure edtPathExit(Sender: TObject);
    procedure actRemoveProjectExecute(Sender: TObject);
    procedure ProjectSettingsError(Sender: TObject; ErrorMsg: String;Id: Integer);
    procedure actCloseProjectExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actFindDCPandBPLExecute(Sender: TObject);
    procedure mitRecentFilesClick(Sender: TObject);
    procedure RecentFilesClick(Sender: TObject);
    procedure actVerifyRegistryExecute(Sender: TObject);
    procedure actSelectDcuPathExecute(Sender: TObject);
    procedure actShowDOFFileExecute(Sender: TObject);
    procedure actSaveLogExecute(Sender: TObject);
    procedure actBackupAllExecute(Sender: TObject);
    procedure actShowOutputDirExecute(Sender: TObject);
    procedure cbxStopOnFailureExit(Sender: TObject);
    procedure cbxSilentModeExit(Sender: TObject);
    procedure cbxStartDelphiExit(Sender: TObject);
    procedure edtPackageBPGFileChange(Sender: TObject);
    procedure ShowProjectGroup1Click(Sender: TObject);
    procedure actRecompileAllExecute(Sender: TObject);
    procedure actRevertChangesExecute(Sender: TObject);
    procedure actSetVersionSelectedProjectsExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure mmoTraceDblClick(Sender: TObject);
    procedure clbPlatformClickCheck(Sender: TObject);
    procedure clbConfigClickCheck(Sender: TObject);
    procedure stgFilesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure actSelectDcpPathExecute(Sender: TObject);
    procedure actCompileProjectExecute(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure stgFilesContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    FExternalEditorFilename:string;
    FExternalEditorLineNo:Integer;
    FFullTrace:boolean;
    FStgFilesLastRow: Integer;
    FStgFilesLastCol: Integer;
    procedure OpenBPGFileInEditor;
    procedure SetCurrentPackage(const _ProjectName:string);
    procedure PrepareGrid;
    procedure ResetGrid;
    procedure WriteLog(_msg: string;_params:array of const);
    procedure ApplicationSettingstoGUI;  // copy settings into GUI fields.
    procedure GUItoApplicationSettings;  // copy GUI fields into settings.
    procedure ProjectSettingstoGUI;  // copy settings into GUI fields.
    procedure GUItoProjectSettings;  // copy GUI fields into settings.
    procedure PrepareBPGEditBox(_BPGFilename:string);
    procedure SearchFileSelected;
    procedure SearchFile(_filename:string;_lineno:integer;_compilerOutput:string);
    procedure PrepareRecentFiles;
    procedure SetDelphiVersionCombobox(const _DelphiVersion:integer);
    procedure SetPlatformCheckListBox(const _Platforms: string);
    procedure SetConfigCheckListBox(const _Configs: string);
    procedure FillProjectGrid;
    function  ExtractFilenameFromLog:string;
    procedure StgFilesShowCellHint(X, Y: Integer);
    procedure DoWriteLog(Sender:TObject;const _msg:string);
    procedure DoDeleteLog(Sender: TObject);
    procedure DoDelphiVersionChangeEvent(Sender:TObject;const _DelphiVersion:integer);
    procedure DoProjectGroupOpen(Sender:TObject);
    procedure DoProjectGroupClose(Sender:TObject);
    procedure DoApplicationStateChange(Sender:TObject;const _OldState,_NewState:TApplicationState);
    procedure DoPackageInstallEvent(Sender:TObject;const _PackageName:string;const _Message:string;const _ProjectNumber:integer);
    procedure DoPackageUnInstallEvent(Sender:TObject;const _PackageName:string;const _Message:string;const _ProjectNumber:integer);
    procedure DoCurrentProjectCompileStateChanged(Sender:TObject;const _ProjectName:string;const _CompileState:string;const _CompileDateTime:string;const _ProjectVersion:string;const _ProjectNumber:integer;const _Description:string);
    procedure DoCurrentProjectChanged(Sender:TObject;const _ProjectName:string;const _ProjectNumber:integer);
    procedure DoWriteTrace(_level:byte;_msg:String;_params:Array of Const);
    procedure DoPlatformChangeEvent(Sender:TObject;const _Platforms:string);
    procedure ClearLog;
    procedure AttachGUIEvents;
    procedure DetachGUIEvents;
  public
    NVBAppExecExternalCommand: TNVBAppExec;
    procedure PrepareGUI(_BPGFilename:string);
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses
  OptionsFrm,
  uDPTMisc,
  uDPTStringGridExt,
  uDPTPathFilenameConvert,
  uDTPProjectData,
  AboutFrm,
  PathSelectionFrm,
  FileCtrl,
  ShellAPI,
  BPGEditorFrm,
  StartUpInfoFrm,
  BPLSearchFrm,
  MainDM;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.actSelectBPGFileExecute
  Author:    
  Date:      22-Aug-2002
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actOpenProjectExecute(Sender: TObject);
resourcestring
cPleaseSelectProjectGroup='Please select a Package Group File <%s>.';
cFilter='Delphi Group Files|%s';
begin
  OpenDialog1.InitialDir:=ExtractFilePath(DMMain.ApplicationSettings.StringValue('Application/LastUsedInputFile', 19));
  OpenDialog1.Title := format(cPleaseSelectProjectGroup,[cProjectGroupExtensions]);
  OpenDialog1.DefaultExt := DMMain.ApplicationSettings.StringValue('Application/LastUsedExtension',27);
  OpenDialog1.Filter := format(cFilter,[cProjectGroupFilter]);
  OpenDialog1.FileName := '';
  OpenDialog1.FilterIndex := 3;
  if not OpenDialog1.Execute then exit;
  DMMain.ApplicationSettings.SetString('Application/LastUsedExtension',27,OpenDialog1.DefaultExt);
  DMMain.LoadBPG(OpenDialog1.FileName);
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.FormShow
  Author:    
  Date:      05-Dez-2002
  Arguments: Sender: TObject
  Result:    None
  Purpose:
  History: 05.12.2002 SH - check if the package group file exits. If not, show the Opendialog.
                         - check if the compiler file exits. If not, show the OpenDialog.
-----------------------------------------------------------------------------}
procedure TFrmMain.FormShow(Sender: TObject);
begin
  caption:='Package Group Rebuilder/Installer '+Getversion;
  left                              :=DMMain.ApplicationSettings.IntegerValue('Application/Position/Left',23);
  top                               :=DMMain.ApplicationSettings.IntegerValue('Application/Position/Top',24);
  width                             :=DMMain.ApplicationSettings.IntegerValue('Application/Position/Width',25);
  height                            :=DMMain.ApplicationSettings.IntegerValue('Application/Position/Height',26);
  pgcInfo.ActivePage:=tabInfo;
  cbxDelphiVersions.Items.Assign(DMMain.InstalledDelphiVersionList);
  SetPlatformCheckListBox('');
  SetConfigCheckListBox('');
  ApplicationSettingstoGUI;
  PrepareGrid;
  PrepareGUI(DMMain.BPGFilename);
  AttachGUIEvents;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.FormCreate
  Author:    
  Date:      29-Aug-2002
  Arguments: Sender: TObject
  Result:    None
  Description: analyze commandline parameters if exists.
-----------------------------------------------------------------------------}
procedure TFrmMain.FormCreate(Sender: TObject);
begin
  FFullTrace:=DMMain.ProjectSettings.BoolValue('Application/Trace',13);
  NVBAppExecExternalCommand := TNVBAppExec.Create(Self);
  NVBAppExecExternalCommand.Name := 'NVBAppExecExternalCommand';
  NVBAppExecExternalCommand.Wait := True;
  NVBAppExecExternalCommand.WindowState := wsNormal;
  NVBAppExecExternalCommand.Priority := ppNormal;
  NVBAppExecExternalCommand.CloseRunningProcess := False;
  DMMain.OnDelphiVersionChange:=DoDelphiVersionChangeEvent;
  DMMain.OnPlatformChangeEvent:=DoPlatformChangeEvent;
  DMMain.OnBPGOpen:=DoProjectGroupOpen;
  DMMain.OnBPGClose:=DoProjectGroupClose;
  DMMain.OnApplicationStateChange:=DoApplicationStateChange;
  case DMMain.ApplicationState of
    tas_init: actCompileProject.Enabled := False;
    tas_open: actCompileProject.Enabled := True;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.actShowOptionsExecute
  Author:    Samuel Herzog
  Date:
  Arguments: Sender: TObject
  Result:    None
  Description: show the options dialog.
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowOptionsExecute(Sender: TObject);
var
_FrmOptions: TFrmOptions;
begin
  _FrmOptions := TFrmOptions.create(nil);
  try
    _FrmOptions.showmodal;
    FCreateBatchFile:=DMMain.ProjectSettings.BoolValue('Application/CreateInstallBatch',4);
    FFullTrace:=DMMain.ProjectSettings.BoolValue('Application/Trace',13);
  finally
    _FrmOptions.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.FormClose
  Author:    
  Date:      05-Dez-2002
  Arguments: Sender: TObject; var Action: TCloseAction
  Result:    None
  Purpose:
  History: 05.12.2002 -check if the directory exits before trying to save the file.
           06.05.2005 - do not write a <.log> file if filename is empty.
-----------------------------------------------------------------------------}
procedure TFrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
_filename:string;
begin
  GUItoProjectSettings;
  GUItoApplicationSettings;
  DMMain.CloseBPG;
  DMMain.OnWriteLog:=nil;
  DMMain.OnDelphiVersionChange:=nil;
  DMMain.OnPlatformChangeEvent:=nil;
  if not SysUtils.DirectoryExists(DMMain.BPGPath) then exit;
  _filename:=ChangeFileExt(DMMain.BPGFilename, '.log');
  try
    if ExtractFilenameOnly(DMMain.BPGFilename)='' then exit;
    mmoLogFile.Lines.SaveToFile(_filename);
    trace(5,'Wrote Logfile <%s>.',[_filename]);
  except
    trace(1,'Problem in FormClose: Could not write Logfile <%s>.',[_filename]);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.PrepareGrid
  Author:    Sam
  Date:      06-Sep-2003
  Arguments: None
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.PrepareGrid;
resourcestring
  cColNo = 'No.';
  cColProject = 'Project/Package';
  cColState = 'State';
  cColLastCompileDate = 'Last Compile Date';
  cColIDEInstall = 'IDE Install';
  cColFileVersion = 'File Version';
  cColDescription = 'Description';
begin
  stgFiles.FixedRows := 1;
  stgFiles.RowCount := 2;
  stgFiles.Cells[0, 0] := cColNo;
  stgFiles.Cells[1, 0] := cColProject;
  stgFiles.Cells[2, 0] := cColState;
  stgFiles.Cells[3, 0] := cColLastCompileDate;
  stgFiles.Cells[4, 0] := cColIDEInstall;
  stgFiles.Cells[5, 0] := cColFileVersion;
  stgFiles.Cells[6, 0] := cColDescription;
end;

procedure TFrmMain.actShowTraceFileExecute(Sender: TObject);
begin
  {$ifdef withTrace}
  DMMain.NVBTraceFile.ShowFile;
  {$endif}
end;

procedure TFrmMain.ClearLog;
begin
  mmoLogFile.Clear;
  mmoTrace.Clear;
end;

procedure TFrmMain.ClearLog1Click(Sender: TObject);
begin
  ClearLog;
end;

procedure TFrmMain.WriteLog(_msg: string;_params:array of const);
begin
  if length(_params)<>0 then _msg:=format(_msg,_params);
  mmoLogFile.Lines.add(_msg);
  trace(5, _msg, []);
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.actShowAboutExecute
  Author:    Sam
  Date:      06-Sep-2003
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowAboutExecute(Sender: TObject);
begin
  TFrmAbout.ShowDialog('About Dialog','','http://sourceforge.net/projects/delphipackageto/');
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.stgFilesDblClick
  Author:    Sam
  Date:      05-Sep-2003
  Arguments: Sender: TObject
  Result:    None
  Description: start external editor.
-----------------------------------------------------------------------------}
procedure TFrmMain.stgFilesDblClick(Sender: TObject);
begin
  actShowFile.execute;
end;

procedure TFrmMain.stgFilesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  StgFilesShowCellHint(X, Y);
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.ResetGrid
  Author:    Sam
  Date:      07-Sep-2003
  Arguments: None
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.ResetGrid;
var
i:integer;
begin
  stgfiles.RowCount:=2;
  for i:=0 to stgfiles.colcount-1 do stgfiles.Cells[i,1]:='';
end;

{-----------------------------------------------------------------------------
  Procedure: actFindFilePathExecute
  Author:    sam
  Date:      15-Feb-2005
  Arguments: Sender: TObject
  Result:    None
  Description: show search path selection dialog
-----------------------------------------------------------------------------}
procedure TFrmMain.actFindFilePathExecute(Sender: TObject);
begin
  ShowSelectPathDialog(DMMain.SearchPath,'',true);
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.mmoLogFileDblClick
  Author:    Samuel Herzog
  Date:      24-Dez-2003
  Arguments: Sender: TObject
  Result:    None
  Description: if a file was not found then try to find it
-----------------------------------------------------------------------------}
procedure TFrmMain.mmoLogFileDblClick(Sender: TObject);
begin
  SearchFileSelected;
end;

{-----------------------------------------------------------------------------
  Procedure: cbxDelphiVersionsChange
  Author:    sam
  Date:      15-Jul-2004
  Arguments: Sender: TObject
  Result:    None
  Description: if the user changes the delphi version.
-----------------------------------------------------------------------------}
procedure TFrmMain.cbxDelphiVersionsChange(Sender: TObject);
begin
  DMMain.CurrentDelphiVersion:=IDENameToVersionNo(cbxDelphiVersions.Text);
end;

{-----------------------------------------------------------------------------
  Procedure: actOpenProjectWithDelphiExecute
  Author:    sam
  Date:      04-Mai-2004
  Arguments: Sender: TObject
  Result:    None
  Description: open a project in the delphi IDE.
-----------------------------------------------------------------------------}
procedure TFrmMain.actOpenProjectWithDelphiExecute(Sender: TObject);
resourcestring
cIDEIsRunning='Delphi is already running. Do you want to open a second Delphi IDE ?';
cDidNotFindFile='Did not find the file <%s>.';
begin
  if not fileexists(DMMain.CurrentProjectFilename) then begin
    WriteLog(cDidNotFindFile,[DMMain.CurrentProjectFilename]);
    exit;
  end;

  if isDelphiStarted(DMMain.CurrentDelphiVersion) then begin
    if Application.MessageBox(pchar(cIDEIsRunning),'Confirma',MB_ICONQUESTION or MB_YESNO)=IDNo then exit;
  end;
  StartUpDelphi(DMMain.CurrentDelphiVersion, DMMain.CurrentProjectFilename);
end;

{-----------------------------------------------------------------------------
  Procedure: actShowFileExecute
  Author:    sam
  Date:      06-Mai-2004
  Arguments: Sender: TObject
  Result:    None
  Description: show a file in the external editor.
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowFileExecute(Sender: TObject);
begin
  DMMain.ShowFile(DMMain.CurrentProjectFilename,0);
end;

{-----------------------------------------------------------------------------
  Procedure: actCompileProjectExecute
  Author:    muem
  Date:      04-Dec-2012
  Arguments: Sender: TObject
  Result:    None
  Description: compile the current project.
-----------------------------------------------------------------------------}
procedure TFrmMain.actCompileProjectExecute(Sender: TObject);
var
  _ProjectsToCompile: TStringList;
begin
  if DMMain.CurrentProjectFilename = '' then exit;
  _ProjectsToCompile := TStringList.Create;
  try
    _ProjectsToCompile.Add(RelativeFilename(DMMain.BPGPath, DMMain.CurrentProjectFilename, DMMain.CurrentDelphiVersion));
    DMMain.CompileAndInstallProjects(_ProjectsToCompile);
  finally
    _ProjectsToCompile.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: actCompileSelectedProjectsExecute
  Author:    sam
  Date:      03-Jun-2004
  Arguments: Sender: TObject
  Result:    None
  Description: compile the selected projects.
-----------------------------------------------------------------------------}
procedure TFrmMain.actCompileSelectedProjectsExecute(Sender: TObject);
var
  i: Integer;
  _SelectedRows: TNVBRowArray;
  _ProjectsToCompile: TStringList;
begin
  _ProjectsToCompile := TStringList.Create;
  try
    _SelectedRows:=GetSelectedRows(stgFiles);
    for i := 0 to length(_SelectedRows)-1 do begin
      _ProjectsToCompile.Add(stgFiles.cells[1, _SelectedRows[i]]);
    end;
    DMMain.CompileAndInstallProjects(_ProjectsToCompile);
  finally
    _ProjectsToCompile.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: actSelectPackageBPLPathExecute
  Author:    sam
  Date:      03-Jun-2004
  Arguments: Sender: TObject
  Result:    None
  Description: set the delphi bpl-path. Default is Projects\BPL
-----------------------------------------------------------------------------}
procedure TFrmMain.actSelectPackageBPLPathExecute(Sender: TObject);
resourcestring
  cSelectBPLFolder='Select BPL-Directory';
var
  _Dir: string;
begin
  _Dir := '';
  if not SelectDirectory(cSelectBPLFolder, '', _Dir) then exit;
  edtPackageBPLDirectory.Text := RelativePath(DMMain.BPGPath, _Dir, DMMain.CurrentDelphiVersion);
  SetDelphiPackageDir(DMMain.CurrentDelphiVersion, _Dir,DMMain.ApplicationSettings.BoolValue('Application/SilentMode', 5));
  SetCurrentPackage(stgFiles.cells[1, stgFiles.row]);
end;

{-----------------------------------------------------------------------------
  Procedure: actApplicationUpdateExecute
  Author:    sam
  Date:      06-Jul-2004
  Arguments: Sender: TObject
  Result:    None
  Description: 20.09.2005 -SH the install path of the NVBUpdater is <..\NVBUpdater\>.
-----------------------------------------------------------------------------}
procedure TFrmMain.actApplicationUpdateExecute(Sender: TObject);
resourcestring
cCouldNotFindUpdater='Could not find Application <NVBUpdater.exe>. Can not check for a new Version.'+#13+#10+'Do you want to download the Update Application?';
var
_currentVersion:string;
_applicationName:string;
_path1,_path2,_path3:string;
begin
  _currentVersion:=GetVersion;
  _applicationName:=Application.ExeName;
  NVBAppExecExternalCommand.ExeName:='NVBUpdater.exe';
  NVBAppExecExternalCommand.ExePath:='';
  NVBAppExecExternalCommand.Wait:=false;
  NVBAppExecExternalCommand.ExeParams:=format('"-d" -h"www.novabit.ch" -p"/downloads/delphipackagetool/update/" -f"%s;%s;%s" -l"%s"',[_applicationName,'history.txt','setversion.exe',_currentVersion]);
  _path1:='..\NVBUpdater\';
  _path2:=extractFilePath(Application.ExeName);
  _path3:=GetSystemPath(spProgFiles)+'Novabit Software\NVBUpdater\';
  if FileExists(_path1+'NVBUpdater.exe') then NVBAppExecExternalCommand.ExePath:=_path1 else
  if FileExists(_path2+'NVBUpdater.exe') then NVBAppExecExternalCommand.ExePath:=_path2 else
  if FileExists(_path3+'NVBUpdater.exe') then NVBAppExecExternalCommand.ExePath:=_path3;

  if NVBAppExecExternalCommand.ExePath<>'' then begin
    NVBAppExecExternalCommand.Execute;
    close;
  end
  else begin
    if Application.MessageBox(pchar(cCouldNotFindUpdater),pchar(cInformation),MB_ICONQUESTION or MB_YESNO)=IDYes then begin
       ShellExecute(Application.Handle,
               PChar('open'),
               PChar('www.novabit.ch/downloads/nvbupdater/setup.exe'),
               PChar(0),
               nil,
               SW_NORMAL);
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: SetCurrentPackage
  Author:    sam
  Date:      15-Jul-2004
  Arguments: _ProjectName: string
  Result:    None
  Description:  gets a project name and converts it to a real name
                and stores the result into FCurrentProjectFilename.

  For instance the bpg file is
              c:\projects\myprojects.bpg

  Examples:   c:\temp\project1.dpr      --->  c:\temp\project1.dpr
              ..\package1.dpk           --->  c:\package1.dpk
              \components\package1.dpk  --->  c:\projects\components\package1.dpk
-----------------------------------------------------------------------------}
procedure TFrmMain.SetCurrentPackage(const _ProjectName: string);
begin
  GUItoApplicationSettings;
  GUItoProjectSettings;
  DMMain.InitCurrentProject(_ProjectName);
  DMMain.LoadCurrentProject;
end;

procedure TFrmMain.VersionHistory1Click(Sender: TObject);
var
_showagain:boolean;
begin
  ShowStartUpDlg(_showagain);
end;

procedure TFrmMain.stgFilesClick(Sender: TObject);
begin
  if DMMain.ApplicationState<>tas_working then SetCurrentPackage(stgFiles.cells[1, stgFiles.row]);
end;

procedure TFrmMain.stgFilesContextPopup(Sender: TObject;
                                        MousePos: TPoint;
                                        var Handled: Boolean);
var
  ACol,
  ARow: Integer;
begin
  stgFiles.MouseToCell(MousePos.X, MousePos.Y, ACol, ARow);
  if (ARow > 0) and (ARow < stgFiles.RowCount) then begin
    if ARow <= DMMain.BPGProjectList.Count  then begin
      stgFiles.Col := ACol;
      stgFiles.Row := ARow;
      if DMMain.ApplicationState<>tas_working then SetCurrentPackage(stgFiles.cells[1, stgFiles.Row]);
    end;
  end
  else begin
    Handled := True;
  end;
end;

procedure TFrmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if key=chr(vk_return) then DMMain.actExecuteApp.execute;
  if key=chr(vk_space)  then DMMain.ShowFile(DMMain.CurrentProjectFilename,0);
  if key=chr(vk_escape) then DMMain.AbortCompile;
end;

{*-----------------------------------------------------------------------------
  Procedure: SetDelphiVersionCombobox
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: const _DelphiVersion:integer
  Result:    None
  Description: set the correct delphi version in the combobox.
-----------------------------------------------------------------------------}
procedure TFrmMain.SetDelphiVersionCombobox(const _DelphiVersion:integer);
var
_sDelphiVersion:string;
_ItemIndex:integer;
begin
  cbxDelphiVersions.OnChange:=nil;
  _sDelphiVersion := VersionNoToIDEName(_DelphiVersion,tdn_long);
  _itemIndex:=cbxDelphiVersions.Items.IndexOf(_sDelphiVersion);
  if _ItemIndex>-1 then begin
    cbxDelphiVersions.ItemIndex:=_ItemIndex;
    cbxDelphiVersions.Hint:='IDE: '+VersionNoToIDEName(_DelphiVersion)+#10+#13+
                            'CompilerVersion: '+DelphiVersions[_DelphiVersion].CompilerVersionStr+#10+#13+
                            'Compiler: '+DMMain.Compiler;
  end
  else begin
    if cbxDelphiVersions.Items.Count>0 then begin
      cbxDelphiVersions.ItemIndex:=0;
      cbxDelphiVersions.Hint:='';
    end;
  end;
  cbxDelphiVersions.OnChange:=cbxDelphiVersionsChange;
end;

{-----------------------------------------------------------------------------
  Procedure: DMMain.ApplicationSettingstoGUI
  Author:    sam
  Date:      02-Feb-2005
  Arguments: None
  Result:    None
  Description: copy Application settings to GUI Fields
-----------------------------------------------------------------------------}
procedure TFrmMain.ApplicationSettingstoGUI;
begin
  SetDelphiVersionCombobox(DMMain.ApplicationSettings.IntegerValue('Compiler/DelphiVersion', 1));
  edtPackageBPLDirectory.Text := DMMain.ApplicationSettings.PathValue('Application/PackageOutputPath', 4);
  edtDCUPath.Text             := DMMain.ApplicationSettings.PathValue('Application/DCUOutputPath', 20);
  cbxSilentMode.checked       := DMMain.ApplicationSettings.BoolValue('Application/SilentMode', 5);
  cbxStopOnFailure.checked    := DMMain.ApplicationSettings.BoolValue('Application/StopOnFailure', 6);
  cbxStartDelphi.checked      := DMMain.ApplicationSettings.BoolValue('Application/StartDelphiOnClose', 7);
  PrepareRecentFiles;
end;

{-----------------------------------------------------------------------------
  Procedure:  GUItoApplicationSettings
  Author:    sam
  Date:      02-Feb-2005
  Arguments: None
  Result:    None
  Description: copy GUI fields to Application settings.
-----------------------------------------------------------------------------}
procedure TFrmMain.GUItoApplicationSettings;
begin
  DMMain.ApplicationSettings.SetInteger('Compiler/DelphiVersion', 1, DMMain.CurrentDelphiVersion);
  DMMain.ApplicationSettings.SetFile('Application/ProjectGroupFile', 3, edtPackageBPGFile.Text);
  DMMain.ApplicationSettings.SetPath('Application/PackageOutputPath', 4, edtPackageBPLDirectory.Text);
  DMMain.ApplicationSettings.SetBoolean('Application/SilentMode', 5, cbxSilentMode.checked);
  DMMain.ApplicationSettings.SetBoolean('Application/StopOnFailure', 6, cbxStopOnFailure.checked);
  DMMain.ApplicationSettings.SetBoolean('Application/StartDelphiOnClose', 7, cbxStartDelphi.checked);
{$ifdef withTrace}
  DMMain.ApplicationSettings.SetInteger('Application/Tracelevel',12,DMMain.NVBTraceFile.Level);
{$endif}  
  DMMain.ApplicationSettings.SetPath('Application/DCUOutputPath', 20,edtDcuPath.Text);
  DMMain.ApplicationSettings.SetInteger('Application/Position/Left',23,left);
  DMMain.ApplicationSettings.SetInteger('Application/Position/Top',24,top);
  DMMain.ApplicationSettings.SetInteger('Application/Position/Width',25,width);
  DMMain.ApplicationSettings.SetInteger('Application/Position/Height',26,height);
end;

{-----------------------------------------------------------------------------
  Procedure: GUItoProjectSettings
  Author:    sam
  Date:      03-Mrz-2006
  Arguments: None
  Result:    None
  Description: copy the settings from the gui to the project settings
-----------------------------------------------------------------------------}
procedure TFrmMain.GUItoProjectSettings;
begin
  if not DMMain.ProjectSettings.isLoaded then exit;
  DMMain.ProjectSettings.SetInteger('Application/DelphiVersion',5, DMMain.CurrentDelphiVersion);
  DMMain.ProjectSettings.SetString('Application/Platform',14, DMMain.CurrentBPGPlatformList.Commatext);
  DMMain.ProjectSettings.SetString('Application/Config',16, DMMain.CurrentBPGConfigList.Commatext);
  DMMain.ProjectSettings.SetPath('Application/PackageOutputPath',6,edtPackageBPLDirectory.Text);
  DMMain.ProjectSettings.SetPath('Application/DCPOutputPath',17,edtDcpPath.Text);
  DMMain.ProjectSettings.SetPath('Application/DCUOutputPath',7,edtDcuPath.Text);
end;

{-----------------------------------------------------------------------------
  Procedure: ProjectSettingstoGUI
  Author:    sam
  Date:      03-Mrz-2006
  Arguments: None
  Result:    None
  Description: copy the project setting to the gui components.
-----------------------------------------------------------------------------}
procedure TFrmMain.ProjectSettingstoGUI;
begin
  if DMMain.ProjectSettings.PathValue('Application/PackageOutputPath', 6) <> '' then edtPackageBPLDirectory.Text:= DMMain.ProjectSettings.PathValue('Application/PackageOutputPath', 6);
  if DMMain.ProjectSettings.PathValue('Application/DCPOutputPath', 17) <> ''    then edtDcpPath.Text:= DMMain.ProjectSettings.PathValue('Application/DCPOutputPath', 17);
  if DMMain.ProjectSettings.PathValue('Application/DCUOutputPath', 7) <> ''     then edtDcuPath.Text:= DMMain.ProjectSettings.PathValue('Application/DCUOutputPath', 7);
  SetDelphiVersionCombobox(DMMain.ProjectSettings.IntegerValue('Application/DelphiVersion',5));
  SetPlatformCheckListBox(DMMain.ProjectSettings.StringValue('Application/Platform',14));
  SetConfigCheckListBox(DMMain.ProjectSettings.StringValue('Application/Config',16));
end;

{-----------------------------------------------------------------------------
  Procedure: OpenBPGFileInEditor
  Author:    sam
  Date:      10-Mrz-2005
  Arguments: None
  Result:    None
  Description: show the bpg-file in the editor.
-----------------------------------------------------------------------------}
procedure TFrmMain.OpenBPGFileInEditor;
begin
  DMMain.ShowFile(DMMain.BPGFilename,0);
end;

{-----------------------------------------------------------------------------
  Procedure: actShowBPGEditorExecute
  Author:    sam
  Date:      14-Mrz-2005
  Arguments: Sender: TObject
  Result:    None
  Description: display the bpg-editor.
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowBPGEditorExecute(Sender: TObject);
var
_bpgFilename:string;
begin
  GUItoProjectSettings;
  _bpgFilename:=DMMain.BPGFilename;
  DMMain.CloseBPG;
  _bpgFilename:=ShowBPGEditor(_bpgFilename);
  DMMain.LoadBPG(_bpgFilename);
end;

{-----------------------------------------------------------------------------
  Procedure: actNewBPGFileExecute
  Author:    sam
  Date:      14-Mrz-2005
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actNewBPGFileExecute(Sender: TObject);
resourcestring
cPleaseDefineFilename='Please define a filename for the Package Group file <.bpg/.groupproj/.bdsgroup>.';
var
_bpgFilename:string;
begin
  SaveDialog1.Title:=cPleaseDefineFilename;
  SaveDialog1.InitialDir  := DMMain.ApplicationSettings.FileValue('Application/ProjectGroupFile', 3);
  SaveDialog1.FileName    := 'NewPackageGroup';
  case DMMain.CurrentDelphiVersion of
    1,2,3,4,5,6,7:begin
             SaveDialog1.Filter      := '*.bpg|*.bpg';
             SaveDialog1.DefaultExt  := '.bpg';
           end;
    8,9,10:begin
             SaveDialog1.Filter      := '*.bdsgroup|*.bdsgroup';
             SaveDialog1.DefaultExt  := '.bdsgroup';
           end;
    else   begin
             SaveDialog1.Filter      := '*.groupproj|*.groupproj';
             SaveDialog1.DefaultExt  := '.groupproj';
           end;
  end;
  SaveDialog1.FilterIndex:=0;
  if not SaveDialog1.Execute then exit;
  _bpgFilename:=ShowBPGEditor(SaveDialog1.FileName);
  DMMain.LoadBPG(_bpgFilename);
end;

{-----------------------------------------------------------------------------
  Procedure: actShowCFGFileExecute
  Author:    sam
  Date:      16-Mrz-2005
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowCFGFileExecute(Sender: TObject);
var
_filename:string;
begin
  _filename:='';
  case DMMain.CurrentDelphiVersion of
    5,6,7,8:   _filename:=changeFileExt(DMMain.CurrentProjectFilename,'.cfg');
    9,10,11,12:_filename:=changeFileExt(DMMain.CurrentProjectFilename,'.bdsproj');
    else       _filename:=changeFileExt(DMMain.CurrentProjectFilename,'.dproj');
  end;
  if not fileexists(_filename) then exit;
  DMMain.ShowFile(_filename,0);
end;

{-----------------------------------------------------------------------------
  Procedure: actShowFileExplorerExecute
  Author:    sam
  Date:      16-Mrz-2005
  Arguments: Sender: TObject
  Result:    None
  Description: open the file explorer and display the current project
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowProjectDirExecute(Sender: TObject);
begin
  DMMain.ShowProjectDir;
end;

{-----------------------------------------------------------------------------
  Procedure: edtPathExit
  Author:    muem
  Date:      30-Nov-2012
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.edtPathExit(Sender: TObject);
begin
  GUItoApplicationSettings;
  GUItoProjectSettings;
  if (Sender as TEdit) = edtPackageBPLDirectory then
    SetDelphiPackageDir(DMMain.CurrentDelphiVersion, (Sender as TEdit).Text, DMMain.ApplicationSettings.BoolValue('Application/SilentMode', 5));
  DMMain.InitProjectDataForHint;
  SetCurrentPackage(stgFiles.cells[1, stgFiles.row]);
end;

{-----------------------------------------------------------------------------
  Procedure: actRemoveProjectExecute
  Author:    sam
  Date:      05-Nov-2005
  Arguments: Sender: TObject
  Result:    None
  Description: remove the currently marked project/package from the .bpg-file.
-----------------------------------------------------------------------------}
procedure TFrmMain.actRemoveProjectExecute(Sender: TObject);
resourcestring
cDoYouReallyWanttoRemove='Do you really want to remove the Project/Package <%s>?';
cReadOnlyFile='The file <%s> is marked as read-only. Do you want to change it anyway ?';
cProblemToRemoveProject='Problem to remove the Project <%s>. See trace file for more info.';
var
_currentRow:integer;
begin
  if not fileexists(DMMain.BPGFilename) then exit;
  if Application.MessageBox(pchar(format(cDoYouReallyWanttoRemove,[DMMain.CurrentProjectFilename])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDNo then exit;
  if IsFileReadOnly(DMMain.BPGFilename) then begin
    if Application.MessageBox(pchar(format(cReadOnlyFile,[DMMain.BPGFilename])),pchar(cWarning),MB_ICONQUESTION or MB_YESNO)=IDNo then exit;
    if not RemoveReadOnlyFlag(DMMain.BPGFilename,true) then exit;
  end;
  _currentRow:=stgFiles.row;
  if DMMain.RemoveProjectFromProjectGroup then begin
    DMMain.LoadBPG(DMMain.BPGFilename);  // reload the file.
    if _currentRow<stgFiles.RowCount then stgFiles.Row:=_currentRow
                                     else stgFiles.Row:=stgFiles.RowCount-1;
  end else Application.MessageBox(pchar(format(cProblemToRemoveProject,[DMMain.CurrentProjectFilename])),pchar(cError),MB_ICONERROR or MB_OK);
end;

{*-----------------------------------------------------------------------------
  Procedure: ExtractFilenameFromLog
  Author:    sam
  Date:      09-Mai-2010
  Arguments: None
  Result:    string
  Description:
-----------------------------------------------------------------------------}
function TFrmMain.ExtractFilenameFromLog:string;
var
_pos:integer;
begin
  result:=mmoLogFile.SelText;
  if IsFilenameValid(result) then exit;
  _pos:=Pos('''',result);
  if _pos>0 then begin
    while _pos>0 do begin
      delete(result,_pos,1);
      _pos:=Pos('''',result);
    end;
    exit;
  end;
  _pos:=Pos('(',result);
  if _pos>0 then begin
    result:=Copy(result,1,_pos-1);
    _pos:=Pos(''+#$D,result);
    while _pos>0 do begin
      delete(result,_pos,1);
      _pos:=Pos(''+#$D,result);
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: SearchFileSelected
  Author:    sam
  Date:      17-Dez-2005
  Arguments: None
  Result:    None
  Description: the user selects the filename and doubleclicks
-----------------------------------------------------------------------------}
procedure TFrmMain.SearchFileSelected;
var
_filename:string;
_pos:integer;
_fileExt:string;
_LineNo:string;
begin
  FExternalEditorFilename:='';
  FExternalEditorLineNo:=0;
  _filename:=ExtractFilenameFromLog;
  FExternalEditorFilename:=trim(_filename);
  _fileExt:=ExtractFileExt(FExternalEditorFilename);
  _pos:=pos('(',_FileExt);
  if _pos>0 then begin
    _FileExt:=copy(_FileExt,1,_pos-1);
    FExternalEditorFilename:=ExtractFilenameOnly(FExternalEditorFilename)+_FileExt;
    _fileExt:=ExtractFileExt(trim(_filename));
    _lineNo:=copy(_FileExt,_pos+1,length(_FileExt)-_pos-1);
    StringToInteger(_lineNo,FExternalEditorLineNo);
    trace(5,'TFrmMain.SearchFileSelected: Extracted LineNo <%d>.',[FExternalEditorLineNo]);
  end;
  trace(5,'TFrmMain.SearchFileSelected: Extracted Filename <%s>.',[FExternalEditorFilename]);
  SearchFile(FExternalEditorFilename,FExternalEditorLineNo,mmoLogFile.Text);
end;

{-----------------------------------------------------------------------------
  Procedure: SearchFile
  Author:    sam
  Date:      17-Dez-2005
  Arguments: _filename:string
  Result:    None
  Description: shows the search dialog to add a search path.
-----------------------------------------------------------------------------}
procedure TFrmMain.SearchFile(_filename:string;_lineno:integer;_compilerOutput:string);
resourcestring
cFileNotFoundTagGerman='nicht gefunden';
cFileNotFoundTagEnglish='not found';
begin
  _filename:=lowercase(_filename);
  if ExtractFileExt(_filename)='' then _filename:=_filename+'*.*';
  if (Pos(cFileNotFoundTagGerman,_compilerOutput)>0) or  //TODO we need a better way to find out if the compilation was successfull.
     (Pos(cFileNotFoundTagEnglish,_compilerOutput)>0) then ShowSelectPathDialog(DMMain.SearchPath,_filename,true) else
  if (Pos('.dpr',_filename)>0) or
     (Pos('.dpk',_filename)>0) or
     (Pos('.pas',_filename)>0) then begin
    if Pos('<',_filename)=1 then Delete(_filename,1,1);
    if Pos('>.',_filename)=length(_filename)-1 then Delete(_filename,length(_filename)-1,2);
    _filename:=AbsoluteFilename(DMMain.BPGPath,_filename);
    DMMain.ShowFile(_filename,_lineNo);
  end;
end;

procedure TFrmMain.ProjectSettingsError(Sender: TObject; ErrorMsg: String;Id: Integer);
begin
  trace(5,'%s',[ErrorMsg]);
end;

procedure TFrmMain.actCloseProjectExecute(Sender: TObject);
begin
  DMMain.CloseBPG;
end;

procedure TFrmMain.actExitExecute(Sender: TObject);
begin
  close;
end;

{-----------------------------------------------------------------------------
  Procedure: actFindDCPandBPLExecute
  Author:    sam
  Date:      18-Mrz-2006
  Arguments: Sender: TObject
  Result:    None
  Description: search for the .dcp and .bpl file of the current package.
-----------------------------------------------------------------------------}
procedure TFrmMain.actFindDCPandBPLExecute(Sender: TObject);
begin
  ShowBPLSearchDialog(DMMain.ApplicationSettings.StringValue('Application/LastUsedSearchPath',15),ExtractFilenameOnly(DMMain.CurrentProjectFilename));
end;

{-----------------------------------------------------------------------------
  Procedure: RecentFilesClick
  Author:    sam
  Date:      24-Jul-2005
  Arguments: Sender: TObject
  Result:    None
  Description: handler if a recent file is choosen in the open dialog.
-----------------------------------------------------------------------------}
procedure TFrmMain.RecentFilesClick(Sender: TObject);
var
_filename:string;
_pos:integer;
begin
  _filename:=TMenuItem(Sender).Caption;
  if not fileexists(_filename) then begin
    _pos:=Pos('&',_filename);
    if _pos<>0 then Delete(_filename,_pos,1);
  end;
  DMMain.LoadBPG(_filename);
end;

{-----------------------------------------------------------------------------
  Procedure: PrepareRecentFiles
  Author:    sam
  Date:      24-Jul-2005
  Arguments: None
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.PrepareRecentFiles;
var
i:integer;
_filename:string;
_newMenuitem:TMenuItem;
_existingMenuitem:TMenuItem;
begin
// emtpy the current items.
  while mitOpenFile.Count>1 do begin
    _existingMenuitem:=mitOpenFile.Items[1];
    _existingMenuitem.Free;
  end;
// empty the file combobox.
  edtPackageBPGFile.Clear;
  edtPackageBPGFile.Items.add('');
// reload the new items.
  for i:=1 to 10 do begin // load recent used file history
    _filename:=DMMain.ApplicationSettings.StringValue(format('Application/FileHistory/Item%d',[i]),50+i);
    if _filename='' then continue;
    if not FileExists(_filename) then continue;
    _newMenuitem:=TMenuItem.Create(nil);
    _newMenuitem.Caption:=_filename;
    _newMenuitem.OnClick:=RecentFilesClick;
    mitOpenFile.Add(_newMenuitem);
    edtPackageBPGFile.Items.Add(_filename);
  end;
end;


{*-----------------------------------------------------------------------------
  Procedure: mitRecentFilesClick
  Author:    sam
  Date:      28-Feb-2010
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.mitRecentFilesClick(Sender: TObject);
begin
  OpenDialog1.InitialDir:=ExtractFilePath(DMMain.ApplicationSettings.StringValue('Application/LastUsedInputFile',19));
  if not OpenDialog1.Execute then exit;
  DMMain.LoadBPG(OpenDialog1.filename);
end;

{-----------------------------------------------------------------------------
  Procedure: actVerifyRegistryExecute
  Author:    sam
  Date:      23-Okt-2006
  Arguments: Sender: TObject
  Result:    None
  Description: check the registry "known packages" if the refrenced file
               really exists. If not then delete the registry key.
-----------------------------------------------------------------------------}
procedure TFrmMain.actVerifyRegistryExecute(Sender: TObject);
resourcestring
cDeletedWrongKeys='Removed some wrong Registry-Keys.';
cRegistryIsOk='Registry-Entries are ok. Nothing has been changed.';
cCouldNotCleanupRegistry='Could not delete some keys from the registery. You might need to run this action with admin-rights.';
var
_NoOfDeletedKeys:integer;
begin
  if not VerifyRegistry(DMMain.CurrentDelphiVersion,_NoOfDeletedKeys) then begin
    Application.MessageBox(pchar(cCouldNotCleanupRegistry),pchar(cError),MB_ICONINFORMATION);
    exit;
  end;

  if _NoOfDeletedKeys>0 then Application.MessageBox(pchar(cDeletedWrongKeys),pchar(cInformation),MB_ICONINFORMATION or MB_OK)
                        else Application.MessageBox(pchar(cRegistryIsOk),pchar(cInformation),MB_ICONINFORMATION or MB_OK);

end;

{-----------------------------------------------------------------------------
  Procedure: actSelectDcpPathExecute
  Author:    sam
  Date:      27-Nov-2012
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actSelectDcpPathExecute(Sender: TObject);
resourcestring
  cSelectDCPPath = 'Select DCP-Path';
var
  _Dir: string;
begin
  _Dir := '';
  if not SelectDirectory(cSelectDCPPath, '', _Dir) then Exit;
  edtDcpPath.Text := RelativePath(DMMain.BPGPath, _Dir,DMMain.CurrentDelphiVersion);
  SetCurrentPackage(stgFiles.cells[1, stgFiles.row]);
end;

{-----------------------------------------------------------------------------
  Procedure: actSelectDcuPathExecute
  Author:    sam
  Date:      13-Mrz-2007
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actSelectDcuPathExecute(Sender: TObject);
resourcestring
cSelectDCUPath='Select DCU-Path';
var
  _Dir: string;
begin
  _Dir := '';
  if not SelectDirectory(cSelectDCUPath, '', _Dir) then Exit;
  edtDcuPath.Text := RelativePath(DMMain.BPGPath, _Dir,DMMain.CurrentDelphiVersion);
  SetCurrentPackage(stgFiles.cells[1, stgFiles.row]);
end;

{-----------------------------------------------------------------------------
  Procedure: actShowDOFFileExecute
  Author:    sam
  Date:      06-Jun-2007
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowDOFFileExecute(Sender: TObject);
begin
  DMMain.ShowFile(changeFileExt(DMMain.CurrentProjectFilename,'.dof'),0);
end;

{-----------------------------------------------------------------------------
  Procedure: actSaveLogExecute
  Author:    sam
  Date:      16-Okt-2007
  Arguments: Sender: TObject
  Result:    None
  Description: save the log-output.
-----------------------------------------------------------------------------}
procedure TFrmMain.actSaveLogExecute(Sender: TObject);
resourcestring
cChooseFileSaveLog='Please choose a filename to save the log-output <.txt>.';
begin
  SaveDialog1.Title:=cChooseFileSaveLog;
  SaveDialog1.InitialDir:=DMMain.ApplicationSettings.PathValue('Application/LastLogOutputPath',21);
  SaveDialog1.DefaultExt := '.txt';
  SaveDialog1.Filter := 'Text-File(.txt)|*.txt';
  SaveDialog1.FileName := DMMain.BPGFilename+'_log.txt';
  SaveDialog1.FilterIndex := 0;
  if not SaveDialog1.Execute then exit;
  mmoLogFile.Lines.SaveToFile(SaveDialog1.filename);
  DMMain.ApplicationSettings.SetPath('Application/LastLogOutputPath',21,extractFilePath(SaveDialog1.filename));
end;

{-----------------------------------------------------------------------------
  Procedure: actBackupAllExecute
  Author:    
  Date:      16-Okt-2007
  Arguments: Sender: TObject
  Result:    None
  Description: extracts all filenames mentioned in the log-file and creates a
               zip-file.
-----------------------------------------------------------------------------}
procedure TFrmMain.actBackupAllExecute(Sender: TObject);
resourcestring
cChooseZipFilename='Please choose a filename to save the backup-file <.zip>.';
var
_filename:string;
_path:string;
begin
  _filename:=changefileExt(ExtractFilenameOnly(DMMain.BPGFilename)+'_'+BuildTimeStamp(now),'.zip');
  SaveDialog1.Title:=cChooseZipFilename;
  _path:=extractfilepath(DMMain.BPGFilename)+'backup\';
  if not CreateDirectory(_path) then _path:=DMMain.ProjectSettings.PathValue('Application/LastUsedBackupPath',11);
  SaveDialog1.InitialDir:=_path;
  SaveDialog1.DefaultExt := '.zip';
  SaveDialog1.Filter := 'Zip-File(.zip)|*.zip';
  SaveDialog1.FileName := _filename;
  SaveDialog1.FilterIndex := 0;
  if not SaveDialog1.Execute then exit;
  mmoLogFile.lines.Insert(0,DMMain.CurrentProjectFilename);
  DMMain.SaveBackup(SaveDialog1.FileName,mmoLogFile.lines);
end;

{-----------------------------------------------------------------------------
  Procedure: actShowOutputDirExecute
  Author:    sam
  Date:      08-Nov-2007
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowOutputDirExecute(Sender: TObject);
begin
  DMMain.ShowOutputDir;
end;

{*-----------------------------------------------------------------------------
  Procedure: DoWriteLog
  Author:    sam
  Date:      09-Feb-2008
  Arguments: Sender: TObject; const _msg: string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.DoWriteLog(Sender: TObject; const _msg: string);
begin
  WriteLog(_msg,[]);
end;

procedure TFrmMain.DoDeleteLog(Sender: TObject);
begin
  mmoLogFile.clear;
  mmoTrace.Clear;
end;

{*-----------------------------------------------------------------------------
  Procedure: DoDelphiVersionChangeEvent
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject;const _DelphiVersion: integer
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.DoDelphiVersionChangeEvent(Sender: TObject;const _DelphiVersion: integer);
begin
  SetDelphiVersionCombobox(_DelphiVersion);
  if DMMain.ApplicationState=tas_init then edtPackageBPLDirectory.Text:=GetDelphiPackageDir(_DelphiVersion);
  actShowDOFFile.Visible:=(_DelphiVersion<8);
  if _DelphiVersion>=15 then begin                 // since Delphi XE are different platforms possible.
    gbxPlatform.Visible:=(True);
    SetPlatformCheckListBox('');
  end
  else begin
    gbxPlatform.Visible:=(False);
  end;
  gbxConfig.Visible:=(_DelphiVersion>=12);
end;

{*-----------------------------------------------------------------------------
  Procedure: DoProjectGroupClose
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description: this event is fired after the project group has been closed.
-----------------------------------------------------------------------------}
procedure TFrmMain.DoProjectGroupClose(Sender: TObject);
begin
  edtPackageBPGFile.ItemIndex := 0;
  edtPackageBPLDirectory.Text := '';
  edtDcpPath.Text := '';
  edtDcuPath.Text := '';
  ResetGrid;
end;

{*-----------------------------------------------------------------------------
  Procedure: DoProjectGroupOpen
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description: this event is fired when the project group is loaded.
-----------------------------------------------------------------------------}
procedure TFrmMain.DoProjectGroupOpen(Sender: TObject);
begin
  ClearLog;
  FCreateBatchFile:=DMMain.ProjectSettings.BoolValue('Application/CreateInstallBatch',4);
  PrepareGUI(DMMain.BPGFilename);
end;

{*-----------------------------------------------------------------------------
  Procedure: DoApplicationStateChange
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject;const _OldState, _NewState: TApplicationState
  Result:    None
  Description: is fired when the application state changes.
-----------------------------------------------------------------------------}
procedure TFrmMain.DoApplicationStateChange(Sender: TObject;const _OldState, _NewState: TApplicationState);
begin
  case _NewState of
    tas_init: begin
      stgFiles.Enabled:=false;
      actCompileProject.Enabled := False;
    end;
    tas_working: begin
      stgFiles.Enabled:=false;
    end;
    tas_open: begin
      stgFiles.Enabled:=true;
      actCompileProject.Enabled := True;
    end;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: FillProjectGrid
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: None
  Result:    None
  Description: fill projects into the grid.
-----------------------------------------------------------------------------}
procedure TFrmMain.FillProjectGrid;
var
  i: Integer;
begin
  stgFiles.RowCount := 2;
  stgFiles.FixedRows := 1;
  if DMMain.BPGProjectList.Count = 0 then exit;
  for i := 1 to DMMain.BPGProjectList.Count do begin
    stgFiles.cells[0,i] := inttostr(i);
    stgFiles.cells[1,i] := DMMain.BPGProjectList.Strings[i-1];     // project-filename
    stgFiles.cells[2,i] := TProjectData(DMMain.BPGProjectList.Objects[i-1]).CompileState;
    stgFiles.cells[3,i] := TProjectData(DMMain.BPGProjectList.Objects[i-1]).CompileDate;
    stgFiles.cells[4,i] := TProjectData(DMMain.BPGProjectList.Objects[i-1]).IDEInstall;
    stgFiles.cells[5,i] := TProjectData(DMMain.BPGProjectList.Objects[i-1]).Version;
    stgFiles.cells[6,i] := TProjectData(DMMain.BPGProjectList.Objects[i-1]).Description;
  end;
  stgFiles.RowCount := DMMain.BPGProjectList.Count+1;
end;

{*-----------------------------------------------------------------------------
  Procedure: DoPackageInstallEvent
  Author:    sam
  Date:      14-Mrz-2008
  Arguments: Sender: TObject;const _PackageName, _Message: string;const _ProjectNumber:integer
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.DoPackageInstallEvent(Sender: TObject;const _PackageName, _Message: string;const _ProjectNumber:integer);
begin
  stgFiles.cells[4,_ProjectNumber+1]:=_Message;
end;

{*-----------------------------------------------------------------------------
  Procedure: DoPackageUnInstallEvent
  Author:    sam
  Date:      14-Mrz-2008
  Arguments: Sender: TObject;const _PackageName, _Message: string;const _ProjectNumber:integer
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.DoPackageUnInstallEvent(Sender: TObject;const _PackageName, _Message: string;const _ProjectNumber:integer);
begin
  stgFiles.cells[4,_ProjectNumber+1]:=_Message;
end;

{*-----------------------------------------------------------------------------
  Procedure: DoCurrentProjectCompileStateChanged
  Author:    sam
  Date:      14-Mrz-2008
  Arguments: Sender:TObject;const _ProjectName:string;const _CompileState:string;const _CompileDateTime:string;const _ProjectVersion:string;const _ProjectSize:string;const _ProjectNumber:integer
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.DoCurrentProjectCompileStateChanged(Sender:TObject;const _ProjectName:string;const _CompileState:string;const _CompileDateTime:string;const _ProjectVersion:string;const _ProjectNumber:integer;const _Description:string);
begin
  stgFiles.Cells[2, _ProjectNumber+1] := _CompileState;
  stgFiles.Cells[3, _ProjectNumber+1] := _CompileDateTime;
  stgFiles.Cells[5, _ProjectNumber+1] := _ProjectVersion;
  stgFiles.Cells[6, _ProjectNumber+1] := _Description;
end;

{*-----------------------------------------------------------------------------
  Procedure: DoCurrentProjectChanged
  Author:    sam
  Date:      13-Mrz-2008
  Arguments: Sender: TObject;const _ProjectName: string; const _ProjectNumber: integer
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.DoCurrentProjectChanged(Sender: TObject;const _ProjectName: string; const _ProjectNumber: integer);
begin
  stgFiles.Row:=_ProjectNumber+1;
end;

{-----------------------------------------------------------------------------
  Procedure: ShowCellHint
  Author:    muem
  Date:      23-Nov-2012
  Arguments: X, Y: Integer    Mouse position over string grid
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.StgFilesShowCellHint(X, Y: Integer);
var
  ACol,
  ARow,
  i: Integer;
begin
  // set ShowHint
  stgFiles.ShowHint := True;
  // read col und row position
  stgFiles.MouseToCell(X, Y, ACol, ARow);
  // show hint if row in valid range
  if (ARow > 0) and (ARow < stgFiles.RowCount) then begin
    if ARow <= DMMain.BPGProjectList.Count  then begin
      case ACol of
        1: begin
          stgFiles.Hint := 'Output Filename    := '+extractfilename(TProjectData(DMMain.BPGProjectList.Objects[ARow-1]).OutputFilename)+#10#13+
                           'Project Output Path:='+TProjectData(DMMain.BPGProjectList.Objects[ARow-1]).OutputPath+#10#13+
                           'BPL Output Path    :='+TProjectData(DMMain.BPGProjectList.Objects[ARow-1]).BPLOutputPath+#10#13+
                           'DCP Output Path    :='+TProjectData(DMMain.BPGProjectList.Objects[ARow-1]).DCPOutputPath+#10#13+
                           'DCU Output Path    :='+TProjectData(DMMain.BPGProjectList.Objects[ARow-1]).DCUOutputPath+#10#13+
                           'Project Search Path:='+TProjectData(DMMain.BPGProjectList.Objects[ARow-1]).SearchPath+#10#13+
                           'DPT Search Path    :='+TProjectData(DMMain.BPGProjectList.Objects[ARow-1]).DPTSearchPath;
        end;
        2: begin
          stgFiles.Hint := '';
          for i := 0 to TProjectData(DMMain.BPGProjectList.Objects[ARow-1]).CompileResultsList.Count-1 do begin
            if stgFiles.Hint <> '' then stgFiles.Hint := stgFiles.Hint + #10#13;
            stgFiles.Hint := stgFiles.Hint + TProjectData(DMMain.BPGProjectList.Objects[ARow-1]).CompileResultsList[i];
          end;
        end;
        5: begin
         stgFiles.Hint := '';
          for i := 0 to TProjectData(DMMain.BPGProjectList.Objects[ARow-1]).VersionsList.Count-1 do begin
            if stgFiles.Hint <> '' then stgFiles.Hint := stgFiles.Hint + #10#13;
            stgFiles.Hint := stgFiles.Hint + TProjectData(DMMain.BPGProjectList.Objects[ARow-1]).VersionsList[i];
          end;
        end;
      else
        stgFiles.Hint := '';
      end;
    end;
  end;
  if (ACol <> FStgFilesLastCol) or (ARow <> FStgFilesLastRow) then begin
    Application.CancelHint;
    FStgFilesLastCol := ACol;
    FStgFilesLastRow := ARow;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: ShowProjectGroup1Click
  Author:    sam
  Date:      24-Feb-2010
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.ShowProjectGroup1Click(Sender: TObject);
begin
  OpenBPGFileInEditor;
end;

{-----------------------------------------------------------------------------
  Procedure: actRecompileAllExecute
  Author:    s.herzog
  Date:      24-Feb-2010
  Arguments: Sender: TObject
  Result:    None
  Description: recompile all.
-----------------------------------------------------------------------------}
procedure TFrmMain.actRecompileAllExecute(Sender: TObject);
begin
  if not DMMain.ReCompileAndInstallAll then exit;
  DMMain.AutoSaveBackup(mmoLogFile.lines);
end;

{-----------------------------------------------------------------------------
  Procedure: actRevertChangesExecute
  Author:    sam
  Date:      07-Mai-2010
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actRevertChangesExecute(Sender: TObject);
var
_filename:string;
begin
  _filename:=ExtractFilenameFromLog;
  DMMain.RevertChange(_filename);
end;

{-----------------------------------------------------------------------------
  Procedure: actSetVersionSelectedProjectsExecute
  Author:    sam
  Date:      05-Aug-2010
  Arguments: Sender: TObject
  Result:    None
  Description: set the version for the selected projects/packages.
-----------------------------------------------------------------------------}
procedure TFrmMain.actSetVersionSelectedProjectsExecute(Sender: TObject);
var
i:integer;
_SelectedRows:TNVBRowArray;
_ShowVersionDialog:boolean;
_Files:TStringList;
_ProjectName:string;
_filename:string;
begin
  _ShowVersionDialog:=true;
  _SelectedRows:=GetSelectedRows(stgFiles);
  if length(_SelectedRows)=0 then exit;
  _Files:=TStringList.create;
  try
    for i:=0 to length(_SelectedRows)-1 do begin
      _ProjectName:=stgFiles.cells[1, _SelectedRows[i]];
       _filename:=AbsoluteFilename(DMMain.BPGPath,_ProjectName);
      _Files.add(_filename);
    end;
    DMMain.SetProjectVersion(_Files,_ShowVersionDialog);
  finally
    _Files.Free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: actSelectAllExecute
  Author:    sam
  Date:      08-Aug-2010
  Arguments: Sender: TObject
  Result:    None
  Description: ctrl+a to select all projects.
-----------------------------------------------------------------------------}
procedure TFrmMain.actSelectAllExecute(Sender: TObject);
begin
//todo
end;

{-----------------------------------------------------------------------------
  Procedure: DoWriteTrace
  Author:    sam
  Date:      28-Okt-2010
  Arguments: _level: byte; _msg: String;_params: array of Const
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.DoWriteTrace(_level: byte; _msg: String;_params: array of Const);
begin
  if FFullTrace or (_level<=3) then mmoTrace.Lines.insert(0,datetimetostr(now)+': '+format(_msg,_params))
end;

{*-----------------------------------------------------------------------------
  Procedure: DoPlatformChangeEvent
  Author:    sam
  Date:      02-Sep-2011
  Arguments: Sender: TObject;const _Platforms: string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.DoPlatformChangeEvent(Sender: TObject;const _Platforms: string);
begin
  SetPlatformCheckListBox(_Platforms);
  if DMMain.ApplicationState=tas_init then edtPackageBPLDirectory.Text:=GetDelphiPackageDir(DMMain.CurrentDelphiVersion);
end;

{*-----------------------------------------------------------------------------
  Procedure: SetPlatformCheckListBox
  Author:    sam
  Date:      02-Sep-2011
  Arguments: const _Platforms: string
  Result:    None
  Description: update the platform checklistbox.
-----------------------------------------------------------------------------}
procedure TFrmMain.SetPlatformCheckListBox(const _Platforms: string);
var
  _i:Integer;
  _itemIndex:Integer;
begin
  DMMain.CurrentBPGPlatformList.CommaText := _Platforms;

  clbPlatform.Items.Assign(DMMain.BPGPlatformList);

  for _i := 0 to clbPlatform.Items.Count-1 do begin
    // check/uncheck check boxes
    _itemIndex:=DMMain.CurrentBPGPlatformList.IndexOf(clbPlatform.Items[_i]);
    clbPlatform.Checked[_i] := (_itemIndex>=0);
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: SetConfigCheckListBox
  Author:    muem
  Date:      29-Oct-2012
  Arguments: const _Config: string
  Result:    None
  Description: update the config checklistbox.
-----------------------------------------------------------------------------}
procedure TFrmMain.SetConfigCheckListBox(const _Configs: string);
var
  _i:Integer;
  _itemIndex:Integer;
begin
  DMMain.CurrentBPGConfigList.CommaText := _Configs;
  clbConfig.Items.Assign(DMMain.BPGConfigList);

  for _i := 0 to clbConfig.Items.Count-1 do begin
    // check/uncheck check boxes
    _itemIndex:=DMMain.CurrentBPGConfigList.IndexOf(clbConfig.Items[_i]);
    clbConfig.checked[_i] := (_itemIndex>=0);
  end;
end;


procedure TFrmMain.cbxStopOnFailureExit(Sender: TObject);
begin
  DMMain.ApplicationSettings.SetBoolean('Application/StopOnFailure', 6, cbxStopOnFailure.checked);
end;

procedure TFrmMain.clbConfigClickCheck(Sender: TObject);
var
  _i:Integer;
begin
  DMMain.CurrentBPGConfigList.Clear;
  for _i := 0 to clbConfig.Count-1 do begin
    if clbConfig.ItemEnabled[_i] and clbConfig.Checked[_i] then begin
      DMMain.CurrentBPGConfigList.Add(clbConfig.Items[_i]);
    end;
  end;
end;

procedure TFrmMain.clbPlatformClickCheck(Sender: TObject);
var
  _i:Integer;
begin
  DMMain.CurrentBPGPlatformList.Clear;
  for _i := 0 to clbPlatform.Count-1 do begin
    if clbPlatform.ItemEnabled[_i] and clbPlatform.Checked[_i] then begin
      DMMain.CurrentBPGPlatformList.Add(clbPlatform.Items[_i]);
    end;
  end;
end;

procedure TFrmMain.cbxSilentModeExit(Sender: TObject);
begin
  DMMain.ApplicationSettings.SetBoolean('Application/SilentMode', 5, cbxSilentMode.checked);
end;

procedure TFrmMain.cbxStartDelphiExit(Sender: TObject);
begin
  DMMain.ApplicationSettings.SetBoolean('Application/StartDelphiOnClose', 7, cbxStartDelphi.checked);
end;

procedure TFrmMain.edtPackageBPGFileChange(Sender: TObject);
begin
  DMMain.LoadBPG(edtPackageBPGFile.Text);
end;

procedure TFrmMain.mmoTraceDblClick(Sender: TObject);
begin
  mmoTrace.clear;
end;

procedure TFrmMain.AttachGUIEvents;
begin
  FWriteMsg:=DoWriteTrace;
  DMMain.OnWriteLog:=DoWriteLog;
  DMMain.OnDeleteLog:=DoDeleteLog;
  DMMain.OnPackageInstalledEvent:=DoPackageInstallEvent;
  DMMain.OnPackageUnInstalledEvent:=DoPackageUninstallEvent;
  DMMain.OnCurrentProjectCompileStateChanged:=DoCurrentProjectCompileStateChanged;
  DMMain.OnCurrentProjectChanged:=DoCurrentProjectChanged;
end;

procedure TFrmMain.DetachGUIEvents;
begin
  FWriteMsg:=nil;
  DMMain.OnWriteLog:=nil;
  DMMain.OnDeleteLog:=nil;
  DMMain.OnPackageInstalledEvent:=nil;
  DMMain.OnPackageUnInstalledEvent:=nil;
  DMMain.OnCurrentProjectCompileStateChanged:=nil;
  DMMain.OnCurrentProjectChanged:=nil;
end;

procedure TFrmMain.FormHide(Sender: TObject);
begin
  DetachGUIEvents;
end;

procedure TFrmMain.PrepareBPGEditBox(_BPGFilename: string);
var
_index:integer;
begin
  _BPGFilename:=lowercase(_BPGFilename);
  _index:=edtPackageBPGFile.Items.IndexOf(_BPGFilename);  // check if already in the list.
  if _index=-1 then begin                              // if not then add it.
    edtPackageBPGFile.Items.add(_BPGFilename);
    _index:=edtPackageBPGFile.Items.IndexOf(_BPGFilename);
  end;  
  edtPackageBPGFile.ItemIndex:=_index;
end;

{*-----------------------------------------------------------------------------
  Procedure: PrepareGUI
  Author:    muem/sam
  Date:      17-Feb-2013
  Arguments: _BPGFilename: string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.PrepareGUI(_BPGFilename: string);
begin
  PrepareBPGEditBox(_BPGFilename);
  ProjectSettingstoGUI;
  FillProjectGrid;
  DoDelphiVersionChangeEvent(nil,DMMain.CurrentDelphiVersion);
  DoApplicationStateChange(nil,DMMain.ApplicationState,DMMain.ApplicationState);
end;

end.

