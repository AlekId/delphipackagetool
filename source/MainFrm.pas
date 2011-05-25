{-----------------------------------------------------------------------------
 Unit Name: MainFrm
 Author:    Samuel Herzog
 Purpose:
 History:

1.9.0.154  ( 25.05.2011 )
- SH: some tweaks to avoid emtpy path entries in search-path and environment variable.

1.9.0.153  ( 10.05.2011 )
- SH: if tags <release> and <debug> are not found in the dproj-file, then do not insert
      any path tags.

1.9.0.152  ( 06.05.2011 )
- SH: made changes for <DCC_HppOutput>. It should not be written into .dproj files older
      than D2009.

1.9.0.151  ( 23.04.2011 )
- SH: changes to write search path and bpl,dcp-output path into the dproj-file of Delphi XE.
      At the moment this settings will only be written into the "Release" configuration. (not debug).
      I have not decided yet how to handle this new possiblities.

1.9.0.150  ( 18.04.2011 )
- SH: improved error handling in case the global environment path can not be written
      because of access rights issues.

1.9.0.149  ( 10.04.2011 )
- SH: Version mix up because of D2008.
- SH: fix for updating Lib-Suffix in Version >D7

1.9.0.148  ( 10.04.2011 )
- SH: added CompilerVersion to Info-Hint. e.g. VER220 for Delphi XE

1.9.0.147  ( 10.01.2011 )
- SH: automatic correct vcljpg/vclimg in .dpk-file.
- SH: added AppLocation in About-Dialog.

1.9.0.146 ( 30.12.2010 )
- SH: fix for detection of Delphi XE.

1.9.0.145 ( 29.12.2010 )
- SH: if silentMode is <on>, then don't ask the user if he want's to review the changes
      with the Diff tool.

1.9.0.144 ( 26.11.2010 )
- SH: recent addition of tracing-stuff slows down the application.
      So I have added a setting to turn it on/off. default=off.

1.9.0.143 ( 22.11.2010 )
- SH: delete log before starting to re-comile all projects. 

1.9.0.142 ( 28.10.2010 )
- SH: started work to make the tool also runing with Delphi XE.
- SH: added a trace memo to display what DPT is doing. 

1.9.0.142 ( 05.10.2010 )
- SH: new function to write DPT-Path settings into the project files.

1.9.0.141 ( 18.09.2010 )
- SH: some minor GUI tweaks.

1.9.0.140 ( 10.08.2010 )
- SH: better error handling in case SetVersion.exe is not available.

1.9.0.139 ( 08.08.2010 )
- SH: it's now possible to set the version of one/many/all packages/projects due to integration
      of the application SetVersion.exe from Jason Penny.
 
1.9.0.138 ( 04.08.2010 )
- SH: started to add functions to set the projects VersionNumber.

1.9.0.137 ( 26.05.2010 )
- SH: changes to method <CleanUpPackagesByBPLPath> about return value.

1.9.0.136 ( 10.05.2010 )
- SH: added feature to Undo the last changes.

1.9.0.135 ( 06.05.2010 )
- SH: re-work of functions which change dpk,dproj,bdsproj,cfg,dof files.
- SH: added feature to setup/integrate an external diff-tool.

1.9.0.134 ( 27.04.2010 )
- SH: improvements for LIBSuffix.

1.9.0.133 ( 17.04.2010 )
- SH: fix in method <RelativePaths> about removing of double entries.

1.9.0.132 ( 15.04.2010 )
- applied patch from M.Mueller about Binary Output Path (-E).

1.9.0.131 ( 11.04.2010 )
- applied patch from M.Mueller about WinExecAndWait.

1.9.0.130 ( 07.04.2010 )
- add's -N /-NO to the batch file. Used to define the .dcu output path.

1.9.0.129 ( 06.04.2010 )
- applied patch from M.Mueller about reading of dproj-file for D2009,D2010.

1.9.0.128 ( 31.03.2010 )
- fix for Command Line Parameter -D (Conditional Define).
- improvements about information on batchfile creation.
- removed unused variable from CreateGroupProj.

1.9.0.127 ( 30.03.2010 )
- fix for Search Path.
- fix in GetSystemPath.
- fix for Project Search Path and DPT Search Path. 

1.9.0.125 ( 27.03.2010 )
- fix in CleanUpPackagesByRegistery. Expand placeholders like BDS before compare the path names.

1.9.0.124 ( 27.03.2010 )
- some changes about the config-filename. from D1-D8 it was .cfg, from D2005-D2006 it was .bdsproj, from D2007- its .dproj
- work inside SaveBackup to make it work again. DPT now creates a <backup.bat> and tries to execute it.
  The user of DPT may edit the file <backup_template.bat> to use another archiver.
- sourcecode clean-up.

1.9.0.122 ( 11.03.2010 )
- added patch from M.Mueller. DPT can now be built with different delphi versions.
- SH: fixed AV when using DPT with command-line parameters.

1.9.0.121 ( 10.03.2010 )
- added patch from M.Mueller. DPT now uses MSXML to read data from dproj,bdsgroup,groupproj files.
- changed name of BPGTemplatefile.txt to ProjectGroupTemplate.bpg.

1.9.0.120 ( 04.03.2010 )
- integrate changes from M.Mueller. Donated code to correctly read BDSCommenDir and BDSProjectsDir.

1.9.0.119 ( 24.02.2010 )
- re-factored to make this project ready to be hosted a sourceforge.net.
- zip-function has been removed. So no additional components must be installed to compile this project.
- tracefile has been replaced by OutputDebugString.

1.9.0.116 ( 17.02.2010 )
- when checking the path environments for double entries, then take into account
  that for example "c:\temp" and "c:\temp\" are the same.

1.9.0.115 ( 09.01.2010 )
- replaced all MessageDlg calls with MessageBox calls because language issues.

1.9.0.114 ( 02.01.2010 )
- moved some message strings to resourcestring section. (for translation)
- implemented <WriteDProjFile> to write LibSuffix into the dproj file.
- fix for missing zip-dll in relation with auto-backup.
- fixed typo reported by S.Besso.
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
  ComCtrls;

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
    edtDCUPath: TEdit;
    btnSelectDcuPath: TButton;
    lblDcuPath: TLabel;
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
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    mmoLogFile: TMemo;
    mmoTrace: TMemo;
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
    procedure edtPackageBPLDirectoryExit(Sender: TObject);
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
  private
    FExternalEditorFilename:string;
    FExternalEditorLineNo:Integer;
    procedure LoadBPG(_filename:string); // load a BPG-File.
    procedure OpenBPGFileInEditor;
    procedure SetCurrentPackage(const _ProjectName:string);
    procedure PrepareGrid;
    procedure ResetGrid;
    procedure WriteLog(_msg: string;_params:array of const);
    procedure ApplicationSettingstoGUI;  // copy settings into GUI fields.
    procedure GUItoApplicationSettings;  // copy GUI fields into settings.
    procedure ProjectSettingstoGUI;  // copy settings into GUI fields.
    procedure GUItoProjectSettings;  // copy GUI fields into settings.
    procedure SearchFileSelected;
    procedure SearchFile(_filename:string;_lineno:integer;_compilerOutput:string);
    procedure PrepareRecentFiles;
    procedure SetDelphiVersionCombobox(const _DelphiVersion:integer);
    procedure PrepareHint;
    procedure FillProjectGrid;
    function  ExtractFilenameFromLog:string;    
    procedure DoWriteLog(Sender:TObject;const _msg:string);
    procedure DoDeleteLog(Sender: TObject);
    procedure DoDelphiVersionChangeEvent(Sender:TObject;const _DelphiVersion:integer);
    procedure DoProjectGroupOpen(Sender:TObject);
    procedure DoProjectGroupClose(Sender:TObject);
    procedure DoApplicationStateChange(Sender:TObject;const _OldState,_NewState:TApplicationState);
    procedure DoPackageInstallEvent(Sender:TObject;const _PackageName:string;const _Message:string;const _ProjectNumber:integer);
    procedure DoPackageUnInstallEvent(Sender:TObject;const _PackageName:string;const _Message:string;const _ProjectNumber:integer);
    procedure DoCurrentProjectCompileStateChanged(Sender:TObject;const _ProjectName:string;const _CompileState:string;const _CompileDateTime:string;const _ProjectVersion:string;const _ProjectSize:string;const _ProjectNumber:integer;const _Description:string);
    procedure DoCurrentProjectChanged(Sender:TObject;const _ProjectName:string;const _ProjectNumber:integer);
    function  DoWriteTrace(_level:byte;_msg:String;_params:Array of Const):boolean;
  public
    NVBAppExecExternalCommand: TNVBAppExec;
    procedure SetLastUsedFile(_filename: string);
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses
  OptionsFrm,
  uDPTMisc,
  uDPTStringGridExt,
  AboutFrm,
  PathSelectionFrm,
  FileCtrl,
  ShellAPI,
  BPGEditorFrm,
  StartUpInfoFrm,
  BPLSearchFrm,
  MainDM,
  VersionFrm;

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
  LoadBPG(OpenDialog1.FileName);
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
var
  _showagain:boolean;
begin
  caption:='Package Group Rebuilder/Installer '+Getversion;
  left                              :=DMMain.ApplicationSettings.IntegerValue('Application/Position/Left',23);
  top                               :=DMMain.ApplicationSettings.IntegerValue('Application/Position/Top',24);
  width                             :=DMMain.ApplicationSettings.IntegerValue('Application/Position/Width',25);
  height                            :=DMMain.ApplicationSettings.IntegerValue('Application/Position/Height',26);
  ApplicationSettingstoGUI;
  PrepareGrid;
  Application.ProcessMessages;
  _showagain:=DMMain.ApplicationSettings.BoolValue('Application/ShowStartUpWarning', 10);
  if _showagain then begin
    ShowStartUpDlg(_showagain);
    DMMain.ApplicationSettings.SetBoolean('Application/ShowStartUpWarning', 10,_showagain);
  end;
  if DMMain.BPGFilename<>'' then LoadBPG(DMMain.BPGFilename)
  else begin
    if DMMain.ApplicationSettings.FileValue('Application/ProjectGroupFile', 3)<>'' then begin
      LoadBPG(DMMain.ApplicationSettings.FileValue('Application/ProjectGroupFile', 3));
    end;
  end;
  if assigned(DMMain.CommandLineAction) then begin
    DMMain.CommandLineAction.Execute;
    if exitcode=0 then close;
  end;  
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
  if DMMain.ProjectSettings.BoolValue('Application/Trace',13) then FWriteMsg:=DoWriteTrace
                                                              else FWriteMsg:=nil;
  NVBAppExecExternalCommand := TNVBAppExec.Create(Self);
  NVBAppExecExternalCommand.Name := 'NVBAppExecExternalCommand';
  NVBAppExecExternalCommand.Wait := True;
  NVBAppExecExternalCommand.WindowState := wsNormal;
  NVBAppExecExternalCommand.Priority := ppNormal;
  NVBAppExecExternalCommand.CloseRunningProcess := False;
  DMMain.OnWriteLog:=DoWriteLog;
  DMMain.OnDeleteLog:=DoDeleteLog;
  DMMain.OnDelphiVersionChange:=DoDelphiVersionChangeEvent;
  DMMain.OnBPGOpen:=DoProjectGroupOpen;
  DMMain.OnBPGClose:=DoProjectGroupClose;
  DMMain.OnApplicationStateChange:=DoApplicationStateChange;
  DMMain.OnPackageInstalledEvent:=DoPackageInstallEvent;
  DMMain.OnPackageUnInstalledEvent:=DoPackageUninstallEvent;
  DMMain.OnCurrentProjectCompileStateChanged:=DoCurrentProjectCompileStateChanged;
  DMMain.OnCurrentProjectChanged:=DoCurrentProjectChanged;
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
    if DMMain.ProjectSettings.BoolValue('Application/Trace',13) then FWriteMsg:=DoWriteTrace
                                                                else FWriteMsg:=nil;
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
  if not DirectoryExists(DMMain.BPGPath) then exit;
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
cColNo='No.';
cColProject='Project/Package';
cColState='State';
cColLastCompileDate='Last Compile Date';
cColIDEInstall='IDE Install';
cColFileVersion='File Version';
cColFileSize='File Size (MByte)';
cColDescription='Description';
begin
  stgFiles.FixedRows:=1;
  stgFiles.RowCount:=2;
  stgFiles.Cells[0, 0] := cColNo;
  stgFiles.Cells[1, 0] := cColProject;
  stgFiles.Cells[2, 0] := cColState;
  stgFiles.Cells[3, 0] := cColLastCompileDate;
  stgFiles.Cells[4, 0] := cColIDEInstall;
  stgFiles.Cells[5, 0] := cColFileVersion;
  stgFiles.Cells[6, 0] := cColFileSize;
  stgFiles.Cells[7, 0] := cColDescription;
end;

procedure TFrmMain.actShowTraceFileExecute(Sender: TObject);
begin
  {$ifdef withTrace}
  DMMain.NVBTraceFile.ShowFile;
  {$endif}
end;

procedure TFrmMain.ClearLog1Click(Sender: TObject);
begin
  mmoLogFile.Clear;
  mmoTrace.Clear;
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
  ShowSelectPathDialog(DMMain.ApplicationSettings.StringValue('Application/LastUsedSearchPath',15),'',true);
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
var
_PackagePath:string;
_iDelphiVersion:integer;
_fDelphiVersion:extended;
begin
  _PackagePath:=IncludeTrailingPathDelimiter(edtPackageBPLDirectory.Text);
  StringToFloat(cbxDelphiVersions.Text,_fDelphiVersion);
  _iDelphiVersion:=trunc(_fDelphiVersion);
  DMMain.CurrentDelphiVersion:=_iDelphiVersion;
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
  Procedure: actCompileSelectedProjectsExecute
  Author:    sam
  Date:      03-Jun-2004
  Arguments: Sender: TObject
  Result:    None
  Description: compile the selected projects.
-----------------------------------------------------------------------------}
procedure TFrmMain.actCompileSelectedProjectsExecute(Sender: TObject);
resourcestring
cAbortedByUser='Aborted by User.';
var
i:integer;
_SelectedRows:TNVBRowArray;
_success:boolean;
_AbortCompile:boolean;
begin
  _AbortCompile:=false;
  _SelectedRows:=GetSelectedRows(stgFiles);
  for i:=0 to length(_SelectedRows)-1 do begin
    SetCurrentPackage(stgFiles.cells[1, _SelectedRows[i]]);
    DMMain.actUninstallPackage.Execute;
    _success:=DMMain.CompilePackage(false);
    if ((not _success) and (cbxStopOnFailure.checked)) then break;
    if _AbortCompile then begin
      writelog(cAbortedByUser,[]);
      break;
    end;
    DMMain.actInstallPackage.Execute;
    Application.ProcessMessages;
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
  if not SelectDirectory(cSelectBPLFolder,'',_Dir) then exit;
  edtPackageBPLDirectory.Text:=RelativePath(DMMain.BPGPath,_Dir,DMMain.CurrentDelphiVersion);
  SetDelphiPackageDir(DMMain.CurrentDelphiVersion,_Dir,DMMain.ApplicationSettings.BoolValue('Application/SilentMode', 5));
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
  DMMain.SetCurrentProject(_ProjectName);
  PrepareHint;
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

procedure TFrmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if key=chr(vk_return) then DMMain.actExecuteApp.execute;
  if key=chr(vk_space)  then actShowFile.Execute;
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
  cbxDelphiVersions.Items.Assign(DMMain.InstalledDelphiVersions);
  _sDelphiVersion := inttostr(_DelphiVersion)+'.0';
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
  edtPackageBPLDirectory.Text       := DMMain.ApplicationSettings.PathValue('Application/PackageOutputPath', 4);
  cbxSilentMode.checked             := DMMain.ApplicationSettings.BoolValue('Application/SilentMode', 5);
  cbxStopOnFailure.checked          := DMMain.ApplicationSettings.BoolValue('Application/StopOnFailure', 6);
  cbxStartDelphi.checked            := DMMain.ApplicationSettings.BoolValue('Application/StartDelphiOnClose', 7);
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
  DMMain.ProjectSettings.SetPath('Application/PackageOutputPath',6,edtPackageBPLDirectory.Text);
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
  if DMMain.ProjectSettings.PathValue('Application/PackageOutputPath', 6)<>'' then edtPackageBPLDirectory.Text:= DMMain.ProjectSettings.PathValue('Application/PackageOutputPath', 6);
  if DMMain.ProjectSettings.PathValue('Application/DCUOutputPath', 7)<>''     then edtDcuPath.Text:= DMMain.ProjectSettings.PathValue('Application/DCUOutputPath', 7);
  SetDelphiVersionCombobox(DMMain.ProjectSettings.IntegerValue('Application/DelphiVersion',5));
end;

{-----------------------------------------------------------------------------
  Procedure: LoadBPG
  Author:    sam
  Date:      09-Feb-2005
  Arguments: _filename: string
  Result:    None
  Description: load a bpg-file
-----------------------------------------------------------------------------}
procedure TFrmMain.LoadBPG(_filename: string);
begin
  if not fileexists(_filename) then begin
    trace(1,'Problem in TFrmMain.LoadBPG: Could not find file <%s>.',[_filename]);
    exit;
  end;
  ClearLog1.Click;
  DMMain.CloseBPG;
  DMMain.OpenBPG(AbsoluteFilename(ExtractFilePath(Application.ExeName),_filename));
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
  LoadBPG(_bpgFilename);
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
  LoadBPG(_bpgFilename);
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
  Procedure: edtPackageBPLDirectoryExit
  Author:    sam
  Date:      17-Mrz-2005
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.edtPackageBPLDirectoryExit(Sender: TObject);
begin
  SetDelphiPackageDir(DMMain.CurrentDelphiVersion,TEdit(Sender).Text,DMMain.ApplicationSettings.BoolValue('Application/SilentMode', 5));
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
    LoadBPG(DMMain.BPGFilename);  // reload the file.
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
     (Pos(cFileNotFoundTagEnglish,_compilerOutput)>0) then ShowSelectPathDialog(DMMain.ApplicationSettings.StringValue('Application/LastUsedSearchPath',15),_filename,true) else
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
  LoadBPG(_filename);
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

{-----------------------------------------------------------------------------
  Procedure: SetLastUsedFile
  Author:    sam
  Date:      24-Jul-2005
  Arguments: _filename: string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.SetLastUsedFile(_filename: string);
var
i:integer;
_tmp:string;
_settingID:Integer;
begin
  DMMain.ApplicationSettings.SetString('Application/LastUsedInputFile', 19, _filename);
  _settingID:=DMMain.ApplicationSettings.FindStringIndex('Application/FileHistory/',_filename);
  if _settingID=-1 then begin   // check if the file is not already in the history list.
    for i:=10 downto 2 do begin
      _tmp:=DMMain.ApplicationSettings.StringValue(format('Application/FileHistory/Item%d',[i-1]),50+i-1);
      DMMain.ApplicationSettings.SetString(format('Application/FileHistory/Item%d',[i]),50+i,_tmp);
    end;
  end
  else begin // the file is already in the recent file list.
    DMMain.ApplicationSettings.SetString('',_settingID,'');
    for i:=_settingID-50 downto 2 do begin
      _tmp:=DMMain.ApplicationSettings.StringValue(format('Application/FileHistory/Item%d',[i-1]),50+i-1);
      DMMain.ApplicationSettings.SetString(format('Application/FileHistory/Item%d',[i]),50+i,_tmp);
    end;
  end;
  DMMain.ApplicationSettings.SetString('Application/FileHistory/Item1',51,_filename);
  PrepareRecentFiles;
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
  LoadBPG(OpenDialog1.filename);
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
cDeletedWrongKeys='Removed some wrong Registry-Keys. See the Trace-File CTRL+T if you need more information.';
cRegistryIsOk='Registry-Entries are ok. Nothing has been changed.';
begin
  if VerifyRegistry(DMMain.CurrentDelphiVersion) then Application.MessageBox(pchar(cDeletedWrongKeys),pchar(cInformation),MB_ICONINFORMATION or MB_OK)
                                                 else Application.MessageBox(pchar(cRegistryIsOk),pchar(cInformation),MB_ICONINFORMATION or MB_OK);
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
  if not SelectDirectory(cSelectDCUPath,'',_Dir) then exit;
  edtDcuPath.Text:=RelativePath(DMMain.BPGPath,_Dir,DMMain.CurrentDelphiVersion);
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
  Procedure: PrepareHint
  Author:    sam
  Date:      09-Jun-2007
  Arguments: None
  Result:    None
  Description: prepare hint for the current porject.
-----------------------------------------------------------------------------}
procedure TFrmMain.PrepareHint;
begin
  stgFiles.Hint:='Output Filename    :='+extractfilename(DMMain.CurrentProjectOutputFilename)+#10+#13+
                 'Project Output Path:='+DMMain.CurrentProjectOutputPath+#10+#13+
                 'BPL Output Path    :='+DMMain.CurrentBPLOutputPath+#10+#13+
                 'DCU Output Path    :='+DMMain.CurrentDCUOutputPath+#10+#13+
                 'Project Search Path:='+DMMain.CurrentSearchPath+#10+#13+
                 'DPT Search Path    :='+DMMain.DPTSearchPath;
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
  actShowDOFFile.Visible:=(_DelphiVersion<8)
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
  edtPackageBPGFile.ItemIndex:=0;
  edtPackageBPLDirectory.Text:='';
  edtDcuPath.Text:='';
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
var
_index:integer;
begin
  FCreateBatchFile:=DMMain.ProjectSettings.BoolValue('Application/CreateInstallBatch',4);
  SetLastUsedFile(DMMain.BPGFilename);
  _index:=edtPackageBPGFile.Items.IndexOf(DMMain.BPGFilename);
  if _index=-1 then begin
    edtPackageBPGFile.Items.add(DMMain.BPGFilename);
    _index:=edtPackageBPGFile.Items.IndexOf(DMMain.BPGFilename);
  end;
  edtPackageBPGFile.ItemIndex:=_index;
  ProjectSettingstoGUI;
  FillProjectGrid;
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
    tas_init:   stgFiles.Enabled:=false;
    tas_working:stgFiles.Enabled:=false;
    tas_open   :stgFiles.Enabled:=true;
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
  i:integer;
begin
  stgFiles.RowCount:=2;
  stgFiles.FixedRows:=1;
  if DMMain.ProjectList.Count=0 then exit;
  for i:=1 to DMMain.ProjectList.Count do begin
    stgFiles.cells[0,i]:=inttostr(i);
    stgFiles.cells[1,i]:=DMMain.ProjectList.Strings[i-1];
    stgFiles.cells[2,i]:='';
    stgFiles.cells[3,i]:='';
    stgFiles.cells[4,i]:='';
    stgFiles.cells[5,i]:='';
    stgFiles.cells[6,i]:='';
    stgFiles.cells[7,i]:='';
  end;
  stgFiles.RowCount:=DMMain.ProjectList.Count+1;
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
procedure TFrmMain.DoCurrentProjectCompileStateChanged(Sender:TObject;const _ProjectName:string;const _CompileState:string;const _CompileDateTime:string;const _ProjectVersion:string;const _ProjectSize:string;const _ProjectNumber:integer;const _Description:string);
begin
  stgFiles.Cells[2, _ProjectNumber+1] := _CompileState;
  stgFiles.Cells[3, _ProjectNumber+1] := _CompileDateTime;
  stgFiles.Cells[5, _ProjectNumber+1] := _ProjectVersion;
  stgFiles.Cells[6, _ProjectNumber+1] := _ProjectSize;
  stgFiles.Cells[7, _ProjectNumber+1] := _Description;
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

procedure TFrmMain.cbxStopOnFailureExit(Sender: TObject);
begin
  DMMain.ApplicationSettings.SetBoolean('Application/StopOnFailure', 6, cbxStopOnFailure.checked);
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
  LoadBPG(edtPackageBPGFile.Text);
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
resourcestring
cAbortedByUser='Aborted by User.';
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
function TFrmMain.DoWriteTrace(_level: byte; _msg: String;_params: array of Const): boolean;
begin
  mmoTrace.Lines.add(datetimetostr(now)+': '+format(_msg,_params));
  result:=true;
end;

end.

