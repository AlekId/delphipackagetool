unit MainDM;

interface

uses
  SysUtils,
  Classes,
  uDPTDelphiPackage,
  uDPTSettings,
  uDPTAppExec,
{$ifdef withTrace}
  cNVBTraceFile,
{$endif}
  uDPTDefinitions,
{$ifndef NoZipSupport}
  Zip,
{$endif}
  ActnList;

type

  TOnWriteLogEvent=procedure(Sender:TObject;const _msg:string) of object;
  TOnDelphiVersionChangeEvent=procedure(Sender:TObject;const _DelphiVersion:integer) of object;
  TOnApplicationStateChangeEvent=procedure(Sender:TObject;const _OldState,_NewState:TApplicationState) of object;
  TOnProcessStateChangeEvent=procedure(Sender:TObject;const _ProcessStates:TProcessStates) of object;
  TOnPackageInstallEvent=procedure(Sender:TObject;const _PackageName:string;const _Message:string;const _ProjectNumber:integer) of object;
  TOnCurrentProjectChanged=procedure(Sender:TObject;const _ProjectName:string;const _ProjectNumber:integer) of object;
  TOnCurrentProjectCompileStateChanged=procedure(Sender:TObject;const _ProjectName:string;const _CompileState:string;const _CompileDateTime:string;const _ProjectVersion:string;const _ProjectSize:string;const _ProjectNumber:integer;const _Description:string) of object;

  TDMMain = class(TDataModule)
    ActionList: TActionList;
    actCleanUpProjectBPLDir: TAction;
    actCleanUpAll: TAction;
    actReCompile: TAction;
    actDeleteBPL: TAction;
    actCompilePackage: TAction;
    actInstallPackage: TAction;
    actUninstallPackage: TAction;
    actResetDelphi: TAction;
    actInstallAllPackages: TAction;
    actCompileAllPackages: TAction;
    actUninstallAllPackages: TAction;
    actShutDownDelphi: TAction;
    actStartUpDelphi: TAction;
    actExecuteApp: TAction;
    actFindDCPandBPL: TAction;
    actDeleteFiles: TAction;
    actRecompileAllPackages: TAction;
    actRevertChanges: TAction;
    actWriteDPTPathsToProject: TAction;
    procedure DataModuleCreate(Sender: TObject);
    procedure ProjectSettingsError(Sender: TObject; ErrorMsg: String;Id: Integer);
    procedure ApplicationSettingsError(Sender: TObject; ErrorMsg: String;Id: Integer);
    procedure DataModuleDestroy(Sender: TObject);
    procedure actCleanUpProjectBPLDirExecute(Sender: TObject);
    procedure actCleanUpAllExecute(Sender: TObject);
    procedure actReCompileExecute(Sender: TObject);
    procedure actDeleteBPLExecute(Sender: TObject);
    procedure actCompilePackageExecute(Sender: TObject);
    procedure actInstallPackageExecute(Sender: TObject);
    procedure actUninstallPackageExecute(Sender: TObject);
    procedure actResetDelphiExecute(Sender: TObject);
    procedure actInstallAllPackagesExecute(Sender: TObject);
    procedure actCompileAllPackagesExecute(Sender: TObject);
    procedure actUninstallAllPackagesExecute(Sender: TObject);
    procedure actShutDownDelphiExecute(Sender: TObject);
    procedure actStartUpDelphiExecute(Sender: TObject);
    procedure actExecuteAppExecute(Sender: TObject);
    procedure actFindDCPandBPLExecute(Sender: TObject);
    procedure actDeleteFilesExecute(Sender: TObject);
    procedure actRecompileAllPackagesExecute(Sender: TObject);
    procedure actRevertChangesExecute(Sender: TObject);
    procedure actWriteDPTPathsToProjectExecute(Sender: TObject);
  private
    FSuccess: boolean;
    FDelphiWasStartedOnApplicationStart: Boolean;
    FApplicationState:TApplicationState;
    FProcessStates:TProcessStates;
    FCurrentProjectType:TProjectType;  // holds the type of the current project.
    FCurrentProjectFilename: string;  // real path and filename of the dpk or dpr file.
    FCurrentProjectOutputFilename: string;  // the filename only of the output file. e.g. .exe,.dll,.bpl
    FCurrentProjectNo:integer;        // the position in the project list.
    FCurrentProjectOutputPath:string;  // real path where the output file (.bpl,.dcp or .exe or .dll) file will be placed.
    FCurrentConfigFilename:string; //  real path and filename of the config-file (delphi 1..8 it was .cfg, 2005,2006 it was .bdsproj, later .dproj
    FCurrentPackageDescription:string;
    FCurrentBPLFilename:string; // if the current project is a package then this contains the full .bpl filename and path.
    FCurrentBPLOutputPath:string;  // output path for the package file bpl.
    FCurrentDCUOutputPath:string;  // output path for the dcu-files.
    FCurrentConditions:string;  // the conditions of the current project.
    FCurrentSearchPath:string;  // search path of the current project.
    FDPTSearchPath: string;     // search path defined in DPT Options Dialog.
    FDelphiRootDirectory:string;  // e.g. <C:\Program files\Borland\Delphi7>
    FDelphiCompilerFile:string;   // e.g. <C:\Program files\Borland\Delphi7\bin\dcc32.exe>
    FBPGPath:string; // path to the package group file .bpg
    FBPGFilename: string;  // the full path and filename of the .bpg file.
    FAbortCompile:boolean;  // set to true if you want to interupt
    FProjectList:TStrings; // list which contains the projects
    FInstalledDelphiVersions:TStrings; // list of installed delphi versions on the computer.
    FZipFilename:string;
    FOnWriteLog: TOnWriteLogEvent;
    FOnBPGOpen:TNotifyEvent;
    FOnBPGClose:TNotifyEvent;
    FCurrentDelphiVersion: Integer;
    FApplicationIniFilename:string;
    FOnDelphiVersionChangeEvent: TOnDelphiVersionChangeEvent;
    FOnApplicationStateEvent: TOnApplicationStateChangeEvent;
    FOnProcessStateChangeEvent:TOnProcessStateChangeEvent;
    FOnPackageInstalledEvent: TOnPackageInstallEvent;
    FOnPackageUnInstalledEvent: TOnPackageInstallEvent;
    FOnCurrentProjectChanged: TOnCurrentProjectChanged;
    FOnCurrentProjectCompileStateChanged: TOnCurrentProjectCompileStateChanged;
    FDelphiLibraryPath:TDelphiLibraryPath;
    FCurrentPackageSuffix: string;
    FOnDeleteLog: TNotifyEvent; // information read from the registery.
    procedure ReadCurrentProjectType(const _DelphiVersion:integer);
    function  GetLibSuffix(_ProjectType:TProjectType;_LibSuffix:string): string;
    procedure WriteLog(_msg: string;const _params:array of const);
    procedure DeleteLog;
    procedure FireDelphiVersionChange(const _version:integer);
    procedure FireCurrentProjectChanged;
    procedure SetApplicationState(const _newState:TApplicationState);
    procedure SetDelphiVersion(const Value: Integer);
    function  InitializeAppSettings(_filename:string):boolean;
    procedure SearchFileCompilerOutput(_compilerOutput:string);
    procedure SearchFile(_filename:string;_compilerOutput:string);
    procedure SetBPGFilename(const Value: string);
    procedure CheckDelphiRunning;
    procedure ExecuteApp;
    function  PrepareEXEParams(_filename:string;_lineNo:integer;_SourceCodeEditorParams:string):string;
    procedure AdaptSearchPath; // replace $(DELPHI) with $(BDS) when the user switches delphi version
    function  SetProjectVersionOfFile(_filename:string;Major,Minor,Release,Build:integer):boolean;
    function  GetProjectVersionOfFile(_filename:string;var Major,Minor,Release,Build:integer):boolean;
    function  OldFilesExist(_ChangedFiles:string):boolean;
  public
{$ifdef withTrace}
    NVBTraceFile: TNVBTraceFile;
{$endif}
    ApplicationSettings: TNVBSettings;
    ProjectSettings: TNVBSettings;
    NVBAppExec1: TNVBAppExec;
    NVBAppExecExternalCommand: TNVBAppExec;
    CommandLineAction: TAction;
    function GetCurrentPackageVersion:string;
    procedure ConfirmChanges(_ChangedFiles:string;const _Revert:Boolean;const _ShowQuestions:boolean=false); // present the changed files in the diff-tool and ask the user if he want to save the changes.
    procedure RevertChange(_filename:string);  // looks if a _old-file exists
    procedure UpdateProjectFiles(const _ForceWrite:boolean=false);
    function  RemoveProjectFromProjectGroup:boolean;
    function  ReCompileAndInstallAll:boolean;
    procedure AutoSaveBackup(_Lines:TStrings);
    procedure ShowProjectDir; // open the file explorer and show the current project directory.
    procedure ShowOutputDir;
    function  CompareFiles(_filename1,_filename2:string):boolean; // start the external diff-tool
    procedure SetCurrentProject(const _ProjectName:string);
    procedure ShowFile(_filename:string;_lineno:integer);
    function  CompilePackage(const _updateCursor: boolean):boolean;
    function  OpenBPG(const _filename: string):boolean;
    procedure CloseBPG;
    function  GetGlobalSearchPath(const _absolutePaths:boolean=true): string;
    procedure AbortCompile;
    procedure SaveBackup(_backupfilename:string;_Lines:TStrings);
    function  SetProjectVersion(_filenames:TStringList;var ShowVersionDialog:boolean):boolean;
    function  IncreaseProjectBuildNo(const _filename: string): boolean;
    property  Compiler:string read FDelphiCompilerFile;
    property  CurrentProjectType:TProjectType read FCurrentProjectType;
    property  CurrentProjectFilename: string  read FCurrentProjectFilename;
    property  CurrentProjectOutputFilename:string read FCurrentProjectOutputFilename;
    property  CurrentProjectOutputPath:string read FCurrentProjectOutputPath;
    property  CurrentConfigFilename:string read FCurrentConfigFilename;
    property  CurrentBPLFilename:string read FCurrentBPLFilename;
    property  CurrentBPLOutputPath:string read FCurrentBPLOutputPath;
    property  CurrentDCUOutputPath:string read FCurrentDCUOutputPath;
    property  CurrentSearchPath:string read FCurrentSearchPath;
    property  CurrentPackageDescription:string read FCurrentPackageDescription;
    property  CurrentPackageSuffix:string read FCurrentPackageSuffix;
    property  CurrentConditions:string read     FCurrentConditions;
    property  CurrentDelphiVersion:Integer read FCurrentDelphiVersion write SetDelphiVersion; // currently selected delphi version.
    property  BPGPath:string read FBPGPath;
    property  BPGFilename:string read FBPGFilename write SetBPGFilename;
    property  DPTSearchPath:string read FDPTSearchPath write FDPTSearchPath;
    property  ProjectList:TStrings read FProjectList;
    property  ZipFilename:string read FZipFilename;
    property  InstalledDelphiVersions:TStrings read FInstalledDelphiVersions;
    property  ApplicationState:TApplicationState read FApplicationState write SetApplicationState;
    property  OnWriteLog:TOnWriteLogEvent read FOnWriteLog write FOnWriteLog;
    property  OnDeleteLog:TNotifyEvent read FOnDeleteLog write FOnDeleteLog;
    property  OnDelphiVersionChange:TOnDelphiVersionChangeEvent read FOnDelphiVersionChangeEvent write FOnDelphiVersionChangeEvent;
    property  OnBPGOpen:TNotifyEvent  read FOnBPGOpen write FOnBPGOpen;
    property  OnBPGClose:TNotifyEvent read FOnBPGClose write FOnBPGClose;
    property  OnApplicationStateChange:TOnApplicationStateChangeEvent read FOnApplicationStateEvent write FOnApplicationStateEvent;
    property  OnProcessStateChange:TOnProcessStateChangeEvent read FOnProcessStateChangeEvent write FOnProcessStateChangeEvent;
    property  OnPackageInstalledEvent:TOnPackageInstallEvent read FOnPackageInstalledEvent write FOnPackageInstalledEvent;
    property  OnPackageUnInstalledEvent:TOnPackageInstallEvent read FOnPackageUnInstalledEvent write FOnPackageUnInstalledEvent;
    property  OnCurrentProjectChanged:TOnCurrentProjectChanged read FOnCurrentProjectChanged write FOnCurrentProjectChanged;
    property  OnCurrentProjectCompileStateChanged:TOnCurrentProjectCompileStateChanged read FOnCurrentProjectCompileStateChanged write FOnCurrentProjectCompileStateChanged;
  end;

var
  DMMain: TDMMain;

implementation

uses
  Forms,
  dialogs,
  Windows,
  Controls,
  RemovePackagesQuestionFrm,
  BPLSearchFrm,
  PathSelectionFrm,
  uDPTEnvironmentPath,
  uDPTMisc,
  VersionFrm;

{$R *.dfm}

const
cModifiedFileExtentions='.cfg_old;.dof_old;.dproj_old;.bdsproj_old;.dpk_old;';

{-----------------------------------------------------------------------------
  Procedure: ReadCurrentProjectType
  Author:    sam
  Date:      15-Jul-2004
  Arguments: None
  Result:    None
  Description: find out the type of the current project.
  if it is a package or a dll or a exe.
-----------------------------------------------------------------------------}
procedure TDMMain.ReadCurrentProjectType(const _DelphiVersion:integer);
begin
  FCurrentProjectType:=tp_unkown;
  if not fileexists(FCurrentProjectFilename) then begin
    trace(1,'Problem in TDMMain.ReadCurrentProjectType: Could not find file <%s>.',[FCurrentProjectFilename]);
    exit;
  end;
  FCurrentProjectType:=DetermProjectType(FCurrentProjectFilename,FBPGFilename,_DelphiVersion);
  if FCurrentProjectType=tp_unkown then trace(1,'Problem in TDMMain.ReadCurrentProjectType: Could not find out the project type of <%s>.',[FCurrentProjectFilename]);
end;

{-----------------------------------------------------------------------------
  Procedure: TDMMain.ShowFile
  Author:    Samuel Herzog
  Date:      24-Dez-2003
  Arguments: _filename:string
  Result:    None
  Description: start external editor and display the file.
-----------------------------------------------------------------------------}
procedure TDMMain.ExecuteApp;
begin
  if FCurrentProjectType<>tp_exe then exit;
  NVBAppExec1.ExePath:=FCurrentProjectOutputPath;
  NVBAppExec1.ExeName:=changeFileExt(extractFileName(FCurrentProjectFilename),'.exe');
  NVBAppExec1.Execute;
end;


{-----------------------------------------------------------------------------
  Procedure: CompareFiles
  Author:    
  Date:      06-Mai-2010
  Arguments: _filename1,_filename2:string
  Result:    None
  Description: start external diff-tool and display <filename1> and <filename2>.
-----------------------------------------------------------------------------}
function TDMMain.CompareFiles(_filename1,_filename2:string):boolean;
resourcestring
cSetupDiffTool='Please setup a Diff-Tool to be opened when DPT modifies a file. Open the <Settings><Options> Dialog to '+#13+#10+'setup.';
cCouldNotFindDiffTool='The Diff-Tool <%s> you have setup could not be '+#13+#10+'found. Please check the settings.';
var
_DiffToolEXEName:string;
_DiffToolParams:string;
begin
  result:=false;
  if not fileexists(_filename1) then begin
    trace(1,'Problem in TDMMain.CompareFiles: Could not find file <%s>.',[_filename1]);
    exit;
  end;
  if not fileexists(_filename2) then begin
    trace(1,'Problem in TDMMain.CompareFiles: Could not find file <%s>.',[_filename2]);
    exit;
  end;
  _DiffToolEXEName:=ApplicationSettings.StringValue('Application/DiffTool',29);
  if not ApplicationSettings.BoolValue('Application/SilentMode',5) then begin
    if _DiffToolEXEName='' then begin
      Application.MessageBox(pChar(cSetupDiffTool),pchar(cInformation),MB_ICONWARNING or MB_OK);
      exit;
    end;
    if not fileexists(_DiffToolEXEName) then begin
      Application.MessageBox(pchar(format(cCouldNotFindDiffTool,[_DiffToolEXEName])),pchar(cWarning),MB_ICONWARNING or MB_OK);
      exit;
    end;
  end;
  _DiffToolParams:='"'+_filename1+'" "'+_filename2+'"';
  NVBAppExec1.Wait:=true;
  NVBAppExec1.ExeName  :=_DiffToolEXEName;
  NVBAppExec1.ExeParams:=_DiffToolParams;
  if not NVBAppExec1.Execute then exit;
  result:=true;
end;


{*-----------------------------------------------------------------------------
  Procedure: ShowFile
  Author:    sam
  Date:      02-Feb-2008
  Arguments: const _filename:string
  Result:    None
  Description: open an external editor and display the file <_filename>.
               Try to set the cursor to line <_lineNo>.
-----------------------------------------------------------------------------}
procedure TDMMain.ShowFile(_filename:string;_lineno:integer);
resourcestring
cCouldnotFindTheFile='Could not find the file <%s>.';
cSetupSourceCodeEditor='You can setup a Sourcecode Editor to be opened when '+#13+#10+'doubleclicking a file. Open the <Settings><Options> Dialog to '+#13+#10+'setup.';
cCouldNotFindSourceEditor='The Sourcecode Editor <%s> you have setup could not be '+#13+#10+'found. Please check the settings.';
cFileIsReadOnly='The file <%s> is marked as read-only. Do you want to change it anyway ?';
var
_lstFiles:TStrings;
_AbortSearch:boolean;
_SourceCodeEditorEXEName:string;
_SourceCodeEditorParams:string;
_ExeParams:string;
begin
  if not fileexists(_filename) then begin
    _AbortSearch:=false;
    _lstFiles:=TStringList.create;
    try
      AllFilesOfDrive(FBPGPath, ExtractFilename(_filename), _lstFiles, _AbortSearch);
      if _lstFiles.count=0 then begin
        Application.MessageBox(pchar(format(cCouldnotFindTheFile,[_filename])),pchar(cWarning),MB_ICONWARNING or MB_OK);
        exit;
      end;
      if _lstFiles.count=1 then _filename:=_lstFiles[0]
      else _filename:=ShowSelectPathDialog(FBPGPath,ExtractFileName(_filename),false);
    finally
      _lstFiles.free;
    end;
  end;
  _SourceCodeEditorEXEName:=ApplicationSettings.StringValue('Application/SourceCodeEditor',9);
  if _SourceCodeEditorEXEName='' then begin
    Application.MessageBox(pChar(cSetupSourceCodeEditor),pchar(cInformation),MB_ICONWARNING or MB_OK);
    exit;
  end;
  if not fileexists(_SourceCodeEditorEXEName) then begin
    Application.MessageBox(pchar(format(cCouldNotFindSourceEditor,[_SourceCodeEditorEXEName])),pchar(cWarning),MB_ICONWARNING or MB_OK);
    exit;
  end;
  if FileIsReadOnly(_filename) then begin
    if Application.MessageBox(pchar(format(cFileIsReadOnly,[_filename])),pchar(cConfirm),MB_ICONQUESTION or MB_YesNo)=IdNo then exit;
    if not RemoveReadOnlyFlag(_filename,true) then exit;
  end;
  _SourceCodeEditorParams:=ApplicationSettings.StringValue('Application/SourceCodeEditorParams',28);
  _ExeParams:=PrepareEXEParams(_filename,_lineNo,_SourceCodeEditorParams);

  NVBAppExec1.ExeName:=_SourceCodeEditorEXEName;
  NVBAppExec1.ExeParams:=_ExeParams;
  NVBAppExec1.Execute;
end;

{*-----------------------------------------------------------------------------
  Procedure: GetConfigFilename
  Author:    sam
  Date:      16-Mrz-2010
  Arguments: const _ProjectFilename:string;const _DelphiVersion:integer
  Result:    string
  Description:
-----------------------------------------------------------------------------}
function GetConfigFilename(const _ProjectFilename:string;const _DelphiVersion:integer):string;
begin
  result:='';
  case _DelphiVersion of
    1..8: result:=changefileext(_ProjectFilename,'.cfg');   // delphi 1..8
    9..10:result:=changefileext(_ProjectFilename,'.bdsproj'); //delphi 2005,2006
    else result:=changefileext(_ProjectFilename,'.dproj');
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: SetCurrentProject
  Author:    sam
  Date:      14-Mrz-2008
  Arguments: const _ProjectName: string;
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.SetCurrentProject(const _ProjectName: string);
resourcestring
cAskToChangeFiles='DelphiPackageTool will change your files. Ok ?';
cCouldNotFindProjectFile='Could not find Project File <%s>. Please check if the file <%s> is still correct.';
cCouldNotFindDCUOutputPath='Could not find the DCU Output Path <%s>. Do you want to edit this path ?';
cCouldNotFindBPLOutputPath='Could not find the BPL Output Path <%s>. Do you want to edit this path ?';
begin
  FCurrentConditions:='';
  FCurrentSearchPath:='';
  FCurrentBPLFilename:='';
  FCurrentConfigFilename:='';
  if _ProjectName='' then exit;
  FCurrentProjectFilename:=AbsoluteFilename(FBPGPath,_ProjectName);
  if not fileexists(FCurrentProjectFilename) then begin
    Application.MessageBox(pchar(format(cCouldNotFindProjectFile,[FCurrentProjectFilename,FBPGFilename])),pchar(cWarning),MB_ICONWARNING or MB_OK);
    trace(1,'Problem in SetCurrentProject: The Project Filename <%s> is not valid. Could not find the file.Please check the file <%s> and path names.',[FCurrentProjectFilename,FBPGFilename]);
  end;

  FCurrentProjectNo:=FProjectList.IndexOf(_ProjectName);
  FCurrentConfigFilename:=GetConfigFilename(FCurrentProjectFilename,FCurrentDelphiVersion);
  ReadCurrentProjectType(FCurrentDelphiVersion); // check if it is a package or not
  actRevertChanges.Enabled:=OldFilesExist(cModifiedFileExtentions);
  case FCurrentProjectType of
    tp_bpl:begin
             actInstallPackage.Enabled:=true;
             actUninstallPackage.Enabled:=true;
             actExecuteApp.Enabled:=false;
             actFindDCPandBPL.Enabled:=true;
             actDeleteBPL.Enabled:=true;
           end;
    tp_exe:begin
             actInstallPackage.Enabled:=false;
             actUninstallPackage.Enabled:=false;
             actDeleteBPL.Enabled:=false;
             actExecuteApp.Enabled:=true;
             actFindDCPandBPL.Enabled:=false;
           end;
    tp_dll:begin
             actInstallPackage.Enabled:=false;
             actUninstallPackage.Enabled:=false;
             actExecuteApp.Enabled:=false;
             actDeleteBPL.Enabled:=false;
             actFindDCPandBPL.Enabled:=false;
           end;
  end;
// read configuration file.
  uDPTDelphiPackage.ReadConfigurationSettings(FCurrentProjectFilename,FCurrentConditions,FCurrentSearchPath,FCurrentProjectOutputPath,FCurrentBPLOutputPath,FCurrentDCUOutputPath);
  uDPTDelphiPackage.ReadPackageInfo(FCurrentProjectFilename,FCurrentPackageDescription,FCurrentPackageSuffix);

// setup the bpl output-path
  if ProjectSettings.PathValue('Application/PackageOutputPath',6)<>'' then FCurrentBPLOutputPath:=AbsolutePath(FBPGPath,ProjectSettings.PathValue('Application/PackageOutputPath',6),FCurrentDelphiVersion)  // then take it from the dpt
                                                                      else FCurrentBPLOutputPath:=AbsolutePath(ExtractFilePath(FCurrentProjectFilename),FCurrentBPLOutputPath,FCurrentDelphiVersion); // otherwise take the path from the cfg-file.

  FCurrentBPLOutputPath:=IncludeTrailingPathDelimiter(ReplaceTag(FCurrentBPLOutputPath,FCurrentDelphiVersion));
  if not ApplicationSettings.BoolValue('Application/SilentMode',5) then begin
    if (FCurrentBPLOutputPath<>'') and (not DirectoryExists(FCurrentBPLOutputPath)) then begin
      if Application.MessageBox(pchar(format(cCouldNotFindBPLOutputPath,[FCurrentBPLOutputPath])),pchar(cConfirm),MB_ICONQUESTION or MB_YesNo)=IdYes then begin
        ShowFile(FCurrentConfigFilename,0);
      end;
    end;
    CheckDirectory(FCurrentBPLOutputPath);
  end;
  trace(5,'CurrentBPLOutputPath=%s',[FCurrentBPLOutputPath]);

// setup the dcu output-path
  if ProjectSettings.PathValue('Application/DCUOutputPath',7)<>'' then FCurrentDCUOutputPath:=AbsolutePath(FBPGPath,ProjectSettings.PathValue('Application/DCUOutputPath',7),FCurrentDelphiVersion)// then take it from the dpt
                                                                  else FCurrentDCUOutputPath:=AbsolutePath(ExtractFilePath(FCurrentProjectFilename),FCurrentDCUOutputPath,FCurrentDelphiVersion);

  FCurrentDCUOutputPath:=IncludeTrailingPathDelimiter(ReplaceTag(FCurrentDCUOutputPath,FCurrentDelphiVersion));
  if not ApplicationSettings.BoolValue('Application/SilentMode',5) then begin
    if (FCurrentDCUOutputPath<>'') and (not DirectoryExists(FCurrentDCUOutputPath)) then begin
      if Application.MessageBox(pchar(format(cCouldNotFindDCUOutputPath,[FCurrentDCUOutputPath])),pchar(cConfirm),MB_ICONQUESTION or MB_YesNo)=IdYes then begin
        ShowFile(changeFileExt(FCurrentProjectFilename,'.cfg'),0);
        exit;
      end;
    end;
    CheckDirectory(FCurrentDCUOutputPath);
  end;
  trace(5,'CurrentDCUOutputPath=%s',[FCurrentDCUOutputPath]);

  if FCurrentProjectOutputPath='' then FCurrentProjectOutputPath:=ExtractFilePath(FCurrentProjectFilename);
  FCurrentProjectOutputPath:=IncludeTrailingPathDelimiter(FCurrentProjectOutputPath);
  FCurrentProjectOutputPath:=AbsoluteFilename(ExtractFilePath(FCurrentProjectFilename),FCurrentProjectOutputPath);
  FCurrentPackageSuffix    :=GetLibSuffix(FCurrentProjectType,FCurrentPackageSuffix);
  FCurrentProjectOutputFilename:=OutputFilename(FCurrentProjectFilename,FCurrentProjectType,FCurrentPackageSuffix);
  if FCurrentProjectType=tp_bpl then begin
    FCurrentBPLFilename      :=FCurrentBPLOutputPath+FCurrentProjectOutputFilename;
    FCurrentProjectOutputPath:=FCurrentBPLOutputPath;
  end;
  trace(5,'CurrentProjectFileName=%s',[FCurrentProjectFilename]);
  trace(5,'CurrentConditions=%s',[FCurrentConditions]);
  trace(5,'CurrentProjectOutputPath=%s',[FCurrentProjectOutputPath]);
  trace(5,'CurrentBPLFilename=%s',[FCurrentBPLFilename]);
  UpdateProjectFiles;
  FireCurrentProjectChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TDMMain.GetSearchPath
  Author:
  Date:      27-Sep-2002
  Arguments: None
  Result:    string
  Purpose:   load the path information defined in DPT Options-Dialog. if <_absolutePaths> is set to
             true, then the returned string contains absolute path names.
  History:
-----------------------------------------------------------------------------}
function TDMMain.GetGlobalSearchPath(const _absolutePaths:boolean=true): string;
var
  _SearchPath: TStrings;
  _currentPath: string;
  i: integer;
  _filename:string;
begin
  Result := '';
  _filename:=changeFileExt(FBPGFilename,'.txt');
  if not FileExists(_filename) then begin
    trace(5,'GetGlobalSearchPath: Could not find Search Path Definition file <%s>.',[_filename]);
    exit;
  end;
  _SearchPath := TStringList.create;
  try
    _SearchPath.LoadfromFile(_filename);
    for i := 0 to _SearchPath.count - 1 do
    begin
      _currentPath := trim(_SearchPath[i]);
      if LastPos(_currentPath, ';') = length(_currentPath) then Delete(_currentPath, length(_currentPath), 1);
      if LastPos(_currentPath, '\') = length(_currentPath) then Delete(_currentPath, length(_currentPath), 1);
      if _absolutePaths then begin
        _currentPath:=ReplaceTag(_currentpath,FCurrentDelphiVersion);
        _currentPath:=AbsolutePath(FBPGPath,_currentPath,FCurrentDelphiVersion);
        if (_currentPath<>'') and (DirectoryExists(_currentPath)) then begin
          Result := Result + _currentPath + ';';
          trace(5,'GetGlobalSearchPath: Added <%s> to search path.',[_currentpath]);
        end else trace(3, 'Problem in GetGlobalSearchPath: Could not find path <%s>.', [_currentPath]);
      end
      else if _currentPath<>'' then Result := Result + _currentPath + ';';
    end;
  finally
    _SearchPath.free;
  end;  
end;

{-----------------------------------------------------------------------------
  Procedure: TDMMain.GetLibSuffixFromDPTSettings
  Author:    sam
  Date:      08-Nov-2007
  Arguments: None
  Result:    string
  Description: returns the current lib-suffix.
-----------------------------------------------------------------------------}
function TDMMain.GetLibSuffix(_ProjectType:TProjectType;_libsuffix:string): string;
begin
  result:=_libsuffix;
  if _ProjectType<>tp_bpl then exit; // only suffix for bpl-files is needed.
  if not ProjectSettings.BoolValue('Application/ChangeFiles', 8) then exit;  // if changing of files is not allowed, then we can not change the suffix.
  if lowercase(ProjectSettings.StringValue('Application/LibSuffix',10))=lowercase(cLIBAutomaticTag) then result:=DelphiVersions[FCurrentDelphiVersion].ShortName
  else if lowercase(ProjectSettings.StringValue('Application/LibSuffix',10))=lowercase(cLIBNoneTag) then result:=''
  else result:=ProjectSettings.StringValue('Application/LibSuffix',10);
end;


{*-----------------------------------------------------------------------------
  Procedure: OpenBPG
  Author:    sam
  Date:      02-Feb-2008
  Arguments: const _Filename: string
  Result:    None
  Description: open the package group file.
-----------------------------------------------------------------------------}
function TDMMain.OpenBPG(const _Filename: string):boolean;
begin
  result:=false;
  if not fileexists(_Filename) then begin
    WriteLog('The file <%s> does not exist. Please check parameters.',[_Filename]);
    trace(1,'Problem in OpenBPG: The file <%s> does not exist.',[_Filename]);
    exit;
  end;
  BPGFilename := _Filename;
  ProjectSettings.Filename:=changeFileExt(FBPGFilename,'.ini');
  ProjectSettings.GetStringValue('Application/Events/OnBeforeInstallAll',1,'','The name of the batch file which will be executed when user presses "Install All".', true,false,false);
  ProjectSettings.GetStringValue('Application/Events/OnAfterInstallAll',2,'','The name of the batch file which will be executed after all projects have been compiled successfully.', true,false,false);
  ProjectSettings.GetStringValue('Application/CompilerSwitches',3,'-B -Q -W -H','This settings contains the compiler switches.',true,false,false);
  ProjectSettings.GetBoolValue('Application/CreateInstallBatch',4,false,'If this setting is on then a Install Batch-file and the Registry-Files .reg will be created.',true,false,false);
  ProjectSettings.GetIntegerValue('Application/DelphiVersion',5,7,'The compiler Version used for this project.',true,false,false);
  ProjectSettings.GetPathValue('Application/PackageOutputPath', 6, 'bpl\$(DELPHIVERSION)\', 'Path to the Delphi Projects\BPL directory.', true,false,false);
  ProjectSettings.GetPathValue('Application/DCUOutputPath', 7, 'dcu\$(DELPHIVERSION)\', 'Output Path for the dcu-files.', true,false,false);
  ProjectSettings.GetBoolValue('Application/ChangeFiles', 8, false,'If set to <True>, then DelphiPackageTool is allowed to change the *.dof and *.cfg files.', true,false,false);
  ProjectSettings.GetBoolValue('Application/ModifyEnvironmentPath', 9, true,'If set to <True>, then DelphiPackageTool tries to add the location of the bpl-files to the environment settings.', true,false,false);
  ProjectSettings.GetStringValue('Application/LibSuffix',10,cLIBNoneTag,'Defines the Lib-Suffix for the Package-Names.',true,false,false);
  ProjectSettings.GetPathValue('Application/LastUsedBackupPath',11,'','Defines last used Backup Path.',true,false,false);
  ProjectSettings.GetBoolValue('Application/AutoBackup', 12, true,'If set to <True>, then DelphiPackageTool creates a backup zip-file after succeessfull run of "Install All".', true,false,false);
  ProjectSettings.GetBoolValue('Application/Trace',13,false,'If set to <True>, then Program internals are written into the trace-memo.',true,false,false);
  if ProjectSettings.Open then trace(3,'Load project settings from file <%s>.',[ProjectSettings.FilePath+ProjectSettings.FileName])
                          else ProjectSettings.SetInteger('Application/DelphiVersion',5,CurrentDelphiVersion);
  CurrentDelphiVersion:=ProjectSettings.IntegerValue('Application/DelphiVersion',5);
  ReadPackageListfromFile(FBPGFilename,FProjectList);
  FDPTSearchPath := GetGlobalSearchPath(false);
  if FProjectList.count>0 then SetCurrentProject(FProjectList[0]);
  ApplicationState:=tas_open;
  if assigned(FOnBPGOpen) then FOnBPGOpen(self);
  result:=true;
end;

{*-----------------------------------------------------------------------------
  Procedure: CloseBPG
  Author:    sam
  Date:      02-Feb-2008
  Arguments: None
  Result:    None
  Description: close the package group file.
-----------------------------------------------------------------------------}
procedure TDMMain.CloseBPG;
begin
  if FApplicationState<>tas_open then exit;
  if ProjectSettings.FileName<>'' then begin
    trace(3,'Save project settings to file <%s>.',[ProjectSettings.FilePath+ProjectSettings.FileName]);
    ProjectSettings.SaveConfig;
  end;
  ProjectSettings.Close;
  ProjectSettings.FileName:='';
  BPGFilename:='';
  FCurrentProjectType:=tp_unkown;
  FCurrentDelphiVersion:=LatestIDEVersion;
  FCurrentProjectFilename:='';
  FCurrentProjectOutputPath:='';
  FCurrentConfigFilename:='';
  FCurrentPackageDescription:='';
  FCurrentBPLFilename:='';
  FCurrentBPLOutputPath:='';
  FCurrentDCUOutputPath:='';
  FCurrentConditions:='';
  FCurrentSearchPath:='';
  FApplicationState:=tas_init;
  DMMain.ProjectList.Clear;
  if assigned(FOnApplicationStateEvent) then FOnApplicationStateEvent(self,FApplicationState,tas_init);
  if assigned(FOnBPGClose) then FOnBPGClose(self);
end;

procedure TDMMain.ShowProjectDir;
begin
  NVBAppExec1.ExePath:=GetWindowsPath;
  NVBAppExec1.ExeName:='explorer.exe';
  NVBAppExec1.ExeParams:=ExtractFilePath(FCurrentProjectFilename);
  NVBAppExec1.Execute;
end;

procedure TDMMain.ShowOutputDir;
begin
  NVBAppExec1.ExePath:=GetWindowsPath;
  NVBAppExec1.ExeName:='explorer.exe';
  NVBAppExec1.ExeParams:=ExtractFilePath(FCurrentProjectOutputPath);
  NVBAppExec1.Execute;
end;

{*-----------------------------------------------------------------------------
  Procedure: DataModuleCreate
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.DataModuleCreate(Sender: TObject);
resourcestring
  cRegisterBPG='Do you want to register file type (*.bpg) with the Delphi Package Tool ?';
  cRegisterBDSGroup='Do you want to register file type (*.bdsgroup) with the Delphi Package Tool ?';
  cRegisterBDSGroupProj='Do you want to register file type (*.groupproj) with the Delphi Package Tool ?';
var
  _tmp: string;
  _filename: string;
  i:integer;
begin
  ApplicationState:=tas_init;
  FProcessStates:=[];
  FCurrentDelphiVersion:=LatestIDEVersion;
  FCurrentBPLOutputPath:=GetDelphiPackageDir(FCurrentDelphiVersion);
  FCurrentDCUOutputPath:='';
  FApplicationIniFilename:=changefileext(application.ExeName,'.ini');
  if not fileexists(FApplicationIniFilename) then begin  // on the first start, ask if the filetypes shall be registered.
    if MessageBox(0,pchar(cRegisterBPG),PChar(cConfirm), MB_ICONQUESTION or MB_YESNO)           = IdYes then RegisterFileType('bpg'      ,Application.ExeName);
    if MessageBox(0,pchar(cRegisterBDSGroup),pchar(cConfirm), MB_ICONQUESTION or MB_YESNO)      = IdYes then RegisterFileType('bdsgroup' ,Application.ExeName);
    if MessageBox(0,pchar(cRegisterBDSGroupProj),pchar(cConfirm), MB_ICONQUESTION or MB_YESNO)  = IdYes then RegisterFileType('groupproj',Application.ExeName);
  end;
// prepare trace file
{$ifdef withTrace}
  NVBTraceFile := TNVBTraceFile.Create(Self);
  NVBTraceFile.Name := 'NVBTraceFile';
  NVBTraceFile.FileName := 'DelphiPackageTool.trc';
  NVBTraceFile.MaxLines := 100000;
  NVBTraceFile.Level := 5;
  NVBTraceFile.UseNameExt := False;
  NVBTraceFile.Subject := 'DelphiPackageTool Trace';
  NVBTraceFile.EMailAdress := 'sam_herzog@yahoo.com';
  NVBTraceFile.FilePath:=ExtractFilePath(Application.exename);
  NVBTraceFile.OpenFile(false);
  FWriteMsg := NVBTraceFile.writeMsg;
{$endif}
  trace(3,'Started application <%s> with version <%s>.',[application.exename,GetVersion]);
// prepare project settings component.
  ProjectSettings := TNVBSettings.Create(Self);
  ProjectSettings.Name := 'ProjectSettings';
  ProjectSettings.AutoSave := False;
  ProjectSettings.FileName := 'Settings.ini';
  ProjectSettings.OnError := ProjectSettingsError;
  ProjectSettings.CryptIt := False;
// prepare component to run external  stuff.
  NVBAppExec1 := TNVBAppExec.Create(Self);
  NVBAppExec1.Name := 'NVBAppExec1';
  NVBAppExec1.Wait := False;
  NVBAppExec1.WindowState := wsNormal;
  NVBAppExec1.Priority := ppNormal;
  NVBAppExec1.CloseRunningProcess := False;

  NVBAppExecExternalCommand := TNVBAppExec.Create(Self);
  NVBAppExecExternalCommand.Name := 'NVBAppExecExternalCommand';
  NVBAppExecExternalCommand.Wait := True;
  NVBAppExecExternalCommand.WindowState := wsNormal;
  NVBAppExecExternalCommand.Priority := ppNormal;
  NVBAppExecExternalCommand.CloseRunningProcess := False;

  FInstalledDelphiVersions:=TStringList.create;
  FProjectList:=TStringList.create;
  FZipFilename:=extractfilepath(application.ExeName)+'zipdll.dll';
  GetInstalledIDEVersions(FInstalledDelphiVersions);
  InitializeAppSettings(FApplicationIniFilename); // load application settings.
  if ParamCount=1 then begin
    _tmp := lowercase(trim(Paramstr(1)));
    trace(3,'Parameter is <%s>.',[_tmp]);
    if Pos('-cleanupbpldir', _tmp) = 1 then begin
      CommandLineAction := actCleanUpProjectBPLDir; // command to be executed.
    end else
    if Pos('-cleanupall', _tmp) = 1 then begin
      CommandLineAction := actCleanUpAll; // command to be executed.
    end
    else begin
      _filename := lowercase(_tmp);
      BPGFilename:=_filename;
    end;
  end
  else begin
    for i:=1 to ParamCount do begin
      _tmp := lowercase(Paramstr(i));
      trace(2,'Parameter <%d> is <%s>.',[i,_tmp]);
      if Pos('-p', _tmp) = 1 then
      begin // project file (either bpg,dpk,dpr)
        Delete(_tmp, 1, 2); //e.g. -p"C:\Projects\Packages\Owncomponents.bpl"
        if pos('"',_tmp)=1 then delete(_tmp,1,1); // remove the leading char <">.
        if pos('"',_tmp)>0 then _tmp:=copy(_tmp,1,pos('"',_tmp)); //copy until the occurence of the char <">.
        _filename := lowercase(_tmp);
        _filename:=AbsoluteFilename(extractFilepath(application.ExeName),_filename);
        if not fileexists(_filename) then begin
          trace(1,'Warning: The file <%s> does not exists.Please check your parameters and settings.',[_filename]);
          exit;
        end;
        OpenBPG(_filename);
      end;
      if Pos('-rebuild', _tmp) = 1 then
      begin // command to be executed.
        if (Pos('.bpg', _filename) > 0) or
           (Pos('.bdsgroup',_filename)>0) or
           (Pos('.groupproj',_filename)>0) then CommandLineAction := actRecompileAllPackages;
        if (Pos('.dpk', _filename) > 0) or
           (Pos('.dpr', _filename) > 0) or
           (Pos('.dproj', _filename) > 0)then begin
          SetCurrentProject(_filename);
          CommandLineAction := actReCompile;
        end;
      end;

      if Pos('-install', _tmp) = 1 then
      begin // command to be executed.
        if (Pos('.dpk', _filename) > 0) or
           (Pos('.dproj', _filename) > 0) then CommandLineAction := actInstallPackage;
      end;

      if Pos('-uninstall', _tmp) = 1 then
      begin // command to be executed.
        if (Pos('.dpk', _filename) > 0) or
           (Pos('.dproj', _filename) > 0) then CommandLineAction := actUninstallPackage;
      end;

      if Pos('-o', _tmp) = 1 then
      begin // project file (either bpg,dpk,dpr)
        Delete(_tmp, 1, 2); //e.g. -OC:\Projects\Packages\Owncomponents.ini
        FApplicationIniFilename := AbsoluteFilename(extractFilepath(Application.exename),_tmp);
        InitializeAppSettings(FApplicationIniFilename); // load application settings.
        CommandLineAction := actRecompileAllPackages;
      end;
    end;
  end;
  FDelphiWasStartedOnApplicationStart := isDelphiStarted(FCurrentDelphiVersion);
end;

{*-----------------------------------------------------------------------------
  Procedure: SaveBackup
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: _backupfilename: string;_Lines:TStrings
  Result:    None
  Description: create a backup file by analyzing the compiler-output.
  This method creates the file <backup_template.bat> with a default content for
  7za-archiver.
  If the user want's to use another archive, he can edit this template file.
  The Placeholder's ARCHIVENAME and FILELIST will be replaced automatically.
-----------------------------------------------------------------------------}
procedure TDMMain.SaveBackup(_backupfilename: string;_Lines:TStrings);
resourcestring
cCreateBackup='Trying to create a backup file. You can modify the file <%s> if you like to change the backup behavoir.';
cOpenBackupFolder='Open the backup folder ?';
var
i:integer;
_line:string;
_BatchZipFile:TStrings;
_FileList:TStringList;
_BackupFileList:string;
_batchFilename:string;
_backupPath:string;
{$ifndef NoZipSupport}
_BackupZip :TZip;
{$endif}
begin
{$ifndef NoZipSupport}
  _BackupZip := TZip.Create(nil);
{$endif}
  try
{$ifndef NoZipSupport}
    _BackupZip.Name := 'BackupZip';
    _BackupZip.ShowProgressDialog := True;
    ProjectSettings.SetPath('Application/LastUsedBackupPath',11,extractFilePath(_BackupZip.Filename));
{$endif}
    writeLog('Searching for files to backup. Please wait...',[]);
    _FileList:=ExtractFilenamesFromDCC32Output(BPGPath,_Lines);
    try
      writeLog('Found <%d> files. Creating zip-file...',[_FileList.Count]);
      if _FileList.Count=0 then exit;
      if fileexists(_backupfilename) then BackupFile(_backupfilename);
{$ifndef NoZipSupport}
      _BackupZip.FileSpecList.Clear;
      _BackupZip.FileSpecList.Assign(_FileList);
      _BackupZip.AddOptions:=_BackupZip.AddOptions+[aoUpdate, aoWithFullPath];
      _BackupZip.Filename:=_backupfilename;
      _BackupZip.ProgressCaption:='Creating zip-file...';
      _BackupZip.add;
      writelog('Saved zip-file to <%s>.',[_BackupZip.Filename]);
{$else}
      _backupPath:=extractfilepath(_backupfilename);
      _backupFilename:=extractfilename(changefileext(_backupfilename,'.7z'));
      _BackupFileList:='FileList_to_Backup.txt';
      _FileList.SaveToFile(_backupPath+_BackupFileList);
      writelog('Saved filelist to <%s>.',[_backupPath+_BackupFileList]);
      _batchFilename:=_backupPath+'backup_template.bat';
      if not fileexists(_batchFilename) then begin
        _BatchZipFile:=TStringList.create; 
        try
          _BatchZipFile.add('7za u -r -t7z "ARCHIVENAME" @"FILELIST"');
          _BatchZipFile.add('pause');
          _BatchZipFile.SaveToFile(_batchFilename);
        finally
          _BatchZipFile.free;
        end;
      end;
      if not ApplicationSettings.BoolValue('Application/SilentMode',5) then Application.MessageBox(pchar(format(cCreateBackup,[_batchFilename])),pchar(cInformation),MB_ICONINFORMATION or MB_OK);
      _BatchZipFile:=TStringList.create;
      try
        _BatchZipFile.LoadFromFile(_batchFilename);
        for i:=0 to _BatchZipFile.Count-1 do begin
          _line:=_BatchZipFile[i];
          _line:=stringreplace(_line,'ARCHIVENAME',_backupfilename,[rfReplaceAll, rfIgnoreCase]);
          _line:=stringreplace(_line,'FILELIST',_BackupFileList,[rfReplaceAll, rfIgnoreCase]);
          _BatchZipFile[i]:=_line;
        end;
        _batchFilename:=_backupPath+'backup.bat';
        _BatchZipFile.SaveToFile(_batchFilename);
      finally
        _BatchZipFile.free;
      end;

      NVBAppExec1.ExePath:=_backupPath;
      NVBAppExec1.ExeName:=_batchFilename;
      NVBAppExec1.Execute;
{$endif}
      if not ApplicationSettings.BoolValue('Application/SilentMode',5) then begin
        if Application.MessageBox(pchar(cOpenBackupFolder),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDYes then ShowFolder(_backupPath);
      end;
    finally
      _FileList.free;
    end;
  finally
{$ifndef NoZipSupport}
    _BackupZip.free;
{$endif}
  end;
end;

procedure TDMMain.WriteLog(_msg: string;const _params: array of const);
begin
  _msg:=format(_msg,_params);
  if assigned(FOnWriteLog) then FOnWriteLog(self,_msg);
end;

procedure TDMMain.DeleteLog;
begin
  if assigned(FOnDeleteLog) then FOnDeleteLog(self);
end;


procedure TDMMain.ProjectSettingsError(Sender: TObject; ErrorMsg: String;Id: Integer);
begin
  trace(5,ErrorMsg,[]);
end;

procedure TDMMain.ApplicationSettingsError(Sender: TObject;ErrorMsg: String; Id: Integer);
begin
  trace(5,ErrorMsg,[]);
end;

procedure TDMMain.DataModuleDestroy(Sender: TObject);
begin
  if ApplicationSettings.BoolValue('Application/StartDelphiOnClose',7) or
    ((FDelphiWasStartedOnApplicationStart) and
    (not ApplicationSettings.BoolValue('Application/SilentMode',5))) then actStartUpDelphiexecute(nil);

  ApplicationSettings.SaveConfig;
  ApplicationSettings.Close;
  FWriteMsg:=nil;
{$ifdef withTrace}
  NVBTraceFile.CloseFile;
{$endif}
  FInstalledDelphiVersions.free;
  FProjectList.free;
end;

procedure TDMMain.SetDelphiVersion(const Value: Integer);
resourcestring
cAskForIDE='The IDE <%s> you used last time for this project is not installed on this computer! Do you want to open this project with IDE <%s>?';
begin
  if not isIDEInstalled(Value) then begin
    if Application.MessageBox(pchar(format(cAskForIDE,[VersionNoToIDEName(Value),VersionNoToIDEName(LatestIDEVersion)])),pchar(cConfirm),MB_ICONQUESTION or MB_YesNo)<>IdYes then Application.terminate;
    FCurrentDelphiVersion:=LatestIDEVersion;
  end
  else FCurrentDelphiVersion := Value;
  ApplicationSettings.SetString('Application/PathNameFile', 8, 'DelphiPackageToolPathD' + inttostr(FCurrentDelphiVersion) + '.txt');
  trace(3,'Set current delphi version to <%d>.',[FCurrentDelphiVersion]);
  FDelphiRootDirectory:=GetDelphiRootDir(FCurrentDelphiVersion);
  trace(3,'Set the Compiler Root Directory to <%s>.',[FDelphiRootDirectory]);
  FDelphiCompilerFile :=FDelphiRootDirectory+'bin\dcc32.exe';
  trace(3,'Set the Compiler File to <%s>.',[FDelphiCompilerFile]);
  ReadLibraryPath(FCurrentDelphiVersion,FDelphiLibraryPath);
  AdaptSearchPath;
  FireDelphiVersionChange(FCurrentDelphiVersion);
end;


{*-----------------------------------------------------------------------------
  Procedure: InitializeAppSettings
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: _filename:string
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function TDMMain.InitializeAppSettings(_filename:string):boolean;
var
i:integer;
begin
  ApplicationSettings := TNVBSettings.Create(Self);
  ApplicationSettings.Name := 'ApplicationSettings';
  ApplicationSettings.AutoSave := False;
  ApplicationSettings.FileName := 'Settings.ini';
  ApplicationSettings.OnError := ApplicationSettingsError;
  ApplicationSettings.CryptIt := False;
  ApplicationSettings.FileName := _filename;
  ApplicationSettings.GetIntegerValue('Compiler/DelphiVersion', 1,LatestIDEVersion, 'Delphi Version', true,false,false);
  ApplicationSettings.GetFileValue('Compiler/DelphiCompiler', 2, '$(DELPHI)\Bin\DCC32.EXE', 'The Borland Delphi Compiler <DCC32.EXE>.', true,false,false);
  ApplicationSettings.GetFileValue('Application/ProjectGroupFile', 3, '', 'The name of Borland Package Group File', true,false,false);
  ApplicationSettings.GetPathValue('Application/PackageOutputPath', 4,GetDelphiPackageDir(LatestIDEVersion), 'Path to the Delphi Projects\BPL directory.', true,false,false);
  ApplicationSettings.GetBoolValue('Application/SilentMode', 5, False, 'If this settings is true, then no dialog boxes will be shown.', true,false,false);
  ApplicationSettings.GetBoolValue('Application/StopOnFailure', 6, false, 'If a failure occures during a batch process like <rebuild all>, then the applications stops.', true,false,false);
  ApplicationSettings.GetBoolValue('Application/StartDelphiOnClose', 7, False, 'Start Delphi when this application terminates.', true,false,false);
  ApplicationSettings.GetStringValue('Application/PathNameFile', 8, 'DelphiPackageToolPathD'+inttostr(CurrentDelphiVersion)+'.txt', 'The file containing the search path for the compiler.', true,false,false);
  ApplicationSettings.GetStringValue('Application/SourceCodeEditor',9,'','Define the Source code Editor.',true,false,false);
  ApplicationSettings.GetBoolValue('Application/ShowStartUpWarning', 10, True, 'If true then the startup information screen is shown.', true,false,false);
  ApplicationSettings.GetStringValue('Application/CompilerSwitches',11,'-B -Q -W -H','This settings contains the compiler switches.',true,false,false);
  ApplicationSettings.GetIntegerValue('Application/Tracelevel',12,3,'Select the trace level of the Log-file. 1-5.',true,false,false);
  ApplicationSettings.GetStringValue('Application/Events/OnBeforeInstallAll',13,'','The file specified here will be executed when button <Install All> is pressed.',true,false,false);
  ApplicationSettings.GetStringValue('Application/Events/OnAfterInstallAll',14,'','The file specified here will be executed when button <Install All> is pressed and all projects are compiled successfully.',true,false,false);
  ApplicationSettings.GetStringValue('Application/LastUsedSearchPath',15,'C:\','Specifies the last used search path.',true,false,false);
  ApplicationSettings.GetEnumValue('Application/Language',16,'default,english,german,french',0,'Defines the Languages.', true,false,false);
  ApplicationSettings.GetBoolValue('Application/UseSkins', 17, True, 'If true then the application will be skinned.', true,false,false);
  ApplicationSettings.GetBoolValue('Application/AutomaticSearchFiles', 18, True, 'If the compilation aborts because a file was not found and this is set to True then the search dialog opens automatically.', true,false,false);
  ApplicationSettings.GetStringValue('Application/LastUsedInputFile',19,'','Last used project name.',true,false,false);
  ApplicationSettings.GetPathValue('Application/DCUOutputPath', 20, 'dcu\$(DELPHIVERSION)\', 'Path to the DCU directory.', true,false,false);
  ApplicationSettings.GetPathValue('Application/LastLogOutputPath',21,'','Last used path to store the log file.',true,false,false);
  ApplicationSettings.GetPathValue('Application/LastZipOutputPath',22,'','Last used path to store the zip file.',true,false,false);
  ApplicationSettings.GetIntegerValue('Application/Position/Left',23,0,'Stores the last left position of the Main-Form.',true,false,false);
  ApplicationSettings.GetIntegerValue('Application/Position/Top',24,0,'Stores the last top position of the Main-Form.',true,false,false);
  ApplicationSettings.GetIntegerValue('Application/Position/Width',25,800,'Stores the last width of the Main-Form.',true,false,false);
  ApplicationSettings.GetIntegerValue('Application/Position/Height',26,600,'Stores the last height of the Main-Form.',true,false,false);
  ApplicationSettings.GetStringValue('Application/LastUsedExtnsion',27,'.bpg','Stores the last used file-type in the file-open dialog.',true,false,false);
  ApplicationSettings.GetStringValue('Application/SourceCodeEditorParams',28,'%FILENAME%','Define the Source code Editor command Line Parameters.',true,false,false);
  ApplicationSettings.GetFileValue('Application/DiffTool', 29, 'MyFavoriteDiffTool.exe', 'Define here your favorite Diff-Tool.', true,false,false);
  for i:=1 to 10 do ApplicationSettings.GetStringValue(format('Application/FileHistory/Item%d',[i]),50+i,'',format('Recently used File <%d>.',[i]), true,false,false);
  for i:=1 to 10 do ApplicationSettings.GetStringValue(format('Application/SearchPathHistory/Item%d',[i]),100+i,'',format('Recently used File <%d>.',[i]), true,false,false);
  ApplicationSettings.Open;
{$ifdef withTrace}
  NVBTraceFile.Level:=ApplicationSettings.IntegerValue('Application/Tracelevel',12);
{$endif}  
  trace(3,'Loaded application settings from file <%s>.',[_filename]);
  CurrentDelphiVersion:=ApplicationSettings.IntegerValue('Compiler/DelphiVersion',1);
  result:=true;
end;

procedure TDMMain.FireDelphiVersionChange(const _version:integer);
begin
  if assigned(FOnDelphiVersionChangeEvent) then FOnDelphiVersionChangeEvent(self,_version);
end;

{*-----------------------------------------------------------------------------
  Procedure: actCleanUpProjectBPLDirExecute
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description: remove/delete all packages in the directory (DELPHI)projects bpl
-----------------------------------------------------------------------------}
procedure TDMMain.actCleanUpProjectBPLDirExecute(Sender: TObject);
begin
  if CleanUpPackagesByBPLPath(FCurrentDelphiVersion,FCurrentBPLOutputPath,true) then ExitCode := 0
                                                                                else ExitCode := 1;
end;

{*-----------------------------------------------------------------------------
  Procedure: actCleanUpAllExecute
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description: remove/delete all packages except the files in the delphi bin directory.
-----------------------------------------------------------------------------}
procedure TDMMain.actCleanUpAllExecute(Sender: TObject);
begin
  CleanUpPackagesByRegistry(HKEY_LOCAL_MACHINE,FCurrentDelphiVersion,'Known Packages'   ,FDelphiRootDirectory+'bin',true);
  CleanUpPackagesByRegistry(HKEY_CURRENT_USER ,FCurrentDelphiVersion,'Known Packages'   ,FDelphiRootDirectory+'bin',true);
  CleanUpPackagesByRegistry(HKEY_LOCAL_MACHINE,FCurrentDelphiVersion,'Disabled Packages',FDelphiRootDirectory+'bin',true);
  CleanUpPackagesByRegistry(HKEY_CURRENT_USER ,FCurrentDelphiVersion,'Disabled Packages',FDelphiRootDirectory+'bin',true);
end;

{-----------------------------------------------------------------------------
  Procedure: ReCompileAndInstallAll:boolean;
  Author:    sam
  Date:      22-Aug-2002
  Arguments: Sender: TObject
  Result:    None
  Description: - uninstall all packages
               - delete all bpl,dcp files.
               - recompile all projects.
               - install all packages.
-----------------------------------------------------------------------------}
function TDMMain.ReCompileAndInstallAll:boolean;
resourcestring
cRebuiltAllProjects='Rebuilt all projects successfully.';
cSomeProjectsCouldNotBeRebuilt='Some projects could not be rebuilt.' + #13 + #10 + 'See the Log file and add the path in the Options Dialog if ' + #13 + #10 + 'needed.';
var
i: integer;
_CompiledProjects: Integer;
_start:cardinal;
_end:cardinal;
_batchfilename:string;
_filename:string;
begin
  result:=false;
  _CompiledProjects := 0;
  FAbortCompile:=false;
  _batchfilename:=ProjectSettings.StringValue('Application/Events/OnBeforeInstallAll',1); //take the project specific settings
  if _batchfilename='' then begin  // this code is here for backawards compatibility with older version of the package tool.
    _batchfilename:=ApplicationSettings.StringValue('Application/Events/OnBeforeInstallAll',13); // if no project specific setting is available then take the application settings.
    ProjectSettings.SetString('Application/Events/OnBeforeInstallAll',1,_batchfilename);
    ApplicationSettings.SetString('Application/Events/OnBeforeInstallAll',13,'')
  end;
  if _batchfilename<>'' then begin
    _batchfilename:=AbsoluteFilename(FBPGPath,_batchfilename);
    NVBAppExecExternalCommand.ExePath:=ExtractFilePath(_batchfilename);
    NVBAppExecExternalCommand.ExeName:=ExtractFileName(_batchfilename);
    if NVBAppExecExternalCommand.Execute then trace(2,'Executed batch file <%s>.',[_batchfilename]);
  end;
  _start:=gettickcount;
  deletelog;
  writelog('Start to compile all projects of file <%s>.',[FBPGFilename]);
  writelog('%s',[FBPGFilename]);
  InitBatchFile(FBPGPath+changeFileExt(ExtractFilename(FBPGFilename),'.bat'));
  ApplicationState:=tas_working;
  try
    for i := 0 to FProjectList.Count - 1 do begin
      SetCurrentProject(FProjectList[i]);
      if FCurrentProjectFilename = '' then continue;
      FireCurrentProjectChanged;
      actReCompileExecute(nil);
      if FSuccess then begin
        inc(_CompiledProjects);
      end
      else if ApplicationSettings.BoolValue('Application/StopOnFailure',6) then break;
      if FAbortCompile then begin
        writelog('Aborted by User.',[]);
        break;
      end;
      Application.ProcessMessages;
      writelog('%s',[FCurrentProjectFilename]);
    end;
    _filename:=SaveBatchFile;
    if _filename<>'' then begin
      writelog('Saved batch file <%s>.',[_filename]);
      if not ApplicationSettings.BoolValue('Application/SilentMode',5) then ShowFolder(extractfilepath(_filename));
    end;
  finally
    ApplicationState:=tas_open;
  end;  
  if FProjectList.Count = _CompiledProjects then
  begin
    writelog('Rebuilt all <%d> projects successfully.', [_CompiledProjects]);
    _end:=((gettickcount-_start) div 1000);
    writelog('It took <%d> seconds to compile all projects.',[_end]);
    CheckDelphiRunning;
    if not ApplicationSettings.BoolValue('Application/SilentMode',5) then Application.MessageBox(pchar(cRebuiltAllProjects),pchar(cInformation),MB_ICONINFORMATION or MB_OK);
    _batchfilename:=ProjectSettings.StringValue('Application/Events/OnAfterInstallAll',2); //take the project specific settings
    if _batchfilename='' then begin // this code is here for backawards compatibility with older version of the package tool.
      _batchfilename:=ApplicationSettings.StringValue('Application/Events/OnAfterInstallAll',14); // if no project specific setting is available then take the application settings.
      ProjectSettings.SetString('Application/Events/OnAfterInstallAll',2,_batchfilename);
      ApplicationSettings.SetString('Application/Events/OnAfterInstallAll',14,_batchfilename);
    end;
    if _batchfilename<>'' then begin
      _batchfilename:=AbsoluteFilename(FBPGPath,_batchfilename);
      NVBAppExecExternalCommand.ExePath:=ExtractFilePath(_batchfilename);
      NVBAppExecExternalCommand.ExeName:=ExtractFileName(_batchfilename);
      if NVBAppExecExternalCommand.Execute then trace(2,'Executed batch file <%s>.',[_batchfilename]);
    end;
    ExitCode := 0;
    result:=true;
  end
  else
  begin
    _end:=((gettickcount-_start) div 1000);
    writelog('It took <%d> seconds to compile the projects.',[_end]);
    WriteLog('Compiled <%d> Projects of Total <%d> Projects.', [_CompiledProjects, FProjectList.count]);
    WriteLog('Some projects could not be rebuilt. See the Log file and add the path in the Options Dialog if needed.', []);
    if not ApplicationSettings.BoolValue('Application/SilentMode',5) then Application.MessageBox(pchar(cSomeProjectsCouldNotBeRebuilt),pchar(cError),MB_ICONERROR or MB_OK);
    ExitCode := 1;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: actReCompileExecute
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description: uninstall,rebuild and install a package.
-----------------------------------------------------------------------------}
procedure TDMMain.actReCompileExecute(Sender: TObject);
begin
  CheckDelphiRunning;
  actDeleteBPLExecute(nil);
  actCompilePackageExecute(nil);
end;

{*-----------------------------------------------------------------------------
  Procedure: CheckDelphiRunning
  Author:    sam
  Date:      15-Mrz-2008
  Arguments: None
  Result:    None
  Description: if delphi IDE is running, then close it.
-----------------------------------------------------------------------------}
procedure TDMMain.CheckDelphiRunning;
resourcestring
cPleaseCloseDelphiFirst='Please close Delphi before running this application.';
begin
  if not isDelphiStarted(FCurrentDelphiVersion) then exit;
  if ApplicationSettings.BoolValue('Application/SilentMode',5) then   // if silent mode then just close it.
    ShutDownDelphi(FCurrentDelphiVersion, false)
  else
  begin
    Application.MessageBox(pchar(cPleaseCloseDelphiFirst),pchar(cInformation),MB_ICONINFORMATION or MB_OK);  // if not silent mode, then ask the user to close delphi IDE.
    actShutDownDelphi.execute;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: actDeleteBPLExecute
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description:  delete bpl and dcp file.
-----------------------------------------------------------------------------}
procedure TDMMain.actDeleteBPLExecute(Sender: TObject);
resourcestring
cDeleteBPL_and_DCP_Files='Do you want to delete the file <%s> and <%s>?';
var
_bplFilename:string;
_dcpFilename:string;
begin
  if FCurrentProjectType<>tp_bpl then exit;
  _bplFilename:=FCurrentBPLFilename;
  _dcpFilename:=FCurrentBPLOutputPath+ChangeFileExt(ExtractFilename(_bplFilename),'.dcp');
  if not ApplicationSettings.BoolValue('Application/SilentMode',5) then begin
    if MessageBox(0,pchar(format(cDeleteBPL_and_DCP_Files,[_bplFilename,_dcpFilename])),pchar(cConfirm), MB_ICONQUESTION or MB_YESNO)  <> IdYes then exit;
  end;
  if uDPTDelphiPackage.DeleteFile(_bplFilename) then WriteLog('Deleted file <%s>.',[_bplFilename]);
  if uDPTDelphiPackage.DeleteFile(_dcpFilename) then WriteLog('Deleted file <%s>.',[_dcpFilename]);
end;

{*-----------------------------------------------------------------------------
  Procedure: actCompilePackageExecute
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description: compile a package file.
-----------------------------------------------------------------------------}
procedure TDMMain.actCompilePackageExecute(Sender: TObject);
begin
  actUninstallPackageExecute(nil);
  CompilePackage(true);
  if FSuccess then actInstallPackageExecute(nil);
end;

{*-----------------------------------------------------------------------------
  Procedure: actInstallPackageExecute
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description: install the currently selected package.
-----------------------------------------------------------------------------}
procedure TDMMain.actInstallPackageExecute(Sender: TObject);
resourcestring
cPathIsNotInEnv='The path <%s> not yet in your Environments Path. Do you want to add it? If you do not add it, then the Delphi IDE might compilain about Packages not found when starting the IDE.';
cCouldNotAddEnvPath='Could not add the Path <%s> to the Environments Variable. Please try to add it manually be using Windows Path Editor.';
var
_message:string;
begin
  if FCurrentProjectType<>tp_bpl then exit;
  UnInstallPackage(FCurrentProjectFilename, FCurrentProjectOutputPath,FCurrentPackageSuffix,FCurrentDelphiVersion);
  _message:=InstallPackage(FCurrentProjectFilename, FCurrentProjectOutputPath,FCurrentPackageDescription,FCurrentPackageSuffix,FCurrentDelphiVersion);
  WriteLog('Installed Package <%s>.',[FCurrentProjectFilename]);
  if assigned(FOnPackageInstalledEvent) then FOnPackageInstalledEvent(self,FCurrentProjectFilename,_message,FCurrentProjectNo);
  if not ProjectSettings.BoolValue('Application/ModifyEnvironmentPath', 9) then exit;
  if IsPathInEnvironmentPath(FCurrentProjectOutputPath) then exit;
  if Application.MessageBox(pchar(format(cPathIsNotInEnv,[FCurrentProjectOutputPath])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDYes then begin
    if not AddGlobalEnvironmentPath(FCurrentProjectOutputPath) then Application.MessageBox(pchar(format(cCouldNotAddEnvPath,[FCurrentProjectOutputPath])),pchar(cInformation),MB_OK);
  end
  else ProjectSettings.SetBoolean('Application/ModifyEnvironmentPath', 9,false);
end;

{*-----------------------------------------------------------------------------
  Procedure: actUninstallPackageExecute
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description:uninstall the packages selected in the listbox.
-----------------------------------------------------------------------------}
procedure TDMMain.actUninstallPackageExecute(Sender: TObject);
begin
  if FCurrentProjectType<>tp_bpl then exit;
  if not UninstallPackage(FCurrentProjectFilename, FCurrentProjectOutputPath,FCurrentPackageSuffix,FCurrentDelphiVersion) then exit;
  if assigned(FOnPackageUnInstalledEvent) then FOnPackageUnInstalledEvent(self,FCurrentProjectFilename,'-',FCurrentProjectNo);
  WriteLog('Uninstalled Package <%s>.',[FCurrentProjectFilename]);
end;

{*-----------------------------------------------------------------------------
  Procedure: actResetDelphiExecute
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description: remove registry key
               delete .bpl file
               delete .dcp file
-----------------------------------------------------------------------------}
procedure TDMMain.actResetDelphiExecute(Sender: TObject);
resourcestring
cDeleteAllPackagesNotInBIN='This will delete ALL Packages which are NOT located in ($DELPHI)\BIN directory.'+#13+#10+'Means that all Third-Party and your own packages get''s deleted and remove from '+#13+#10+'the Delphi IDE. Do you want to continue ?';
cDeleteAllPackagesInBPL   ='This will delete ALL Packages which are located in ($DELPHI)\PROJECTS\BPL directory.'+#13+#10+'Means that all Third-Party and your own packages get''s deleted and remove from '+#13+#10+'the Delphi IDE. Do you want to continue ?';
var
_RemoveType:TPackageRemoveType;
_DeleteBplAndDCPFiles:boolean;
begin
  if ShowRemovePackagesDlg(FCurrentBPLOutputPath,_RemoveType,_DeleteBplAndDCPFiles)<>mrOk then exit;
  case _RemoveType of
    tpr_3rdparty:  begin
                      if Application.MessageBox(pchar(cDeleteAllPackagesNotInBIN),pchar(cWarning),MB_ICONWARNING or MB_YESNO)<>IDyes then exit;
                      CleanUpPackagesByRegistry(HKEY_LOCAL_MACHINE,CurrentDelphiVersion,'Known Packages',FDelphiRootDirectory+'bin',_DeleteBplAndDCPFiles);
                      CleanUpPackagesByRegistry(HKEY_CURRENT_USER ,CurrentDelphiVersion,'Known Packages',FDelphiRootDirectory+'bin',_DeleteBplAndDCPFiles);
                      CleanUpPackagesByRegistry(HKEY_LOCAL_MACHINE,CurrentDelphiVersion,'Disabled Packages',FDelphiRootDirectory+'bin',_DeleteBplAndDCPFiles);
                      CleanUpPackagesByRegistry(HKEY_CURRENT_USER ,CurrentDelphiVersion,'Disabled Packages',FDelphiRootDirectory+'bin',_DeleteBplAndDCPFiles);
                   end;
    tpr_projectsbpl:begin
                      if Application.MessageBox(pchar(cDeleteAllPackagesInBPL),pchar(cWarning),MB_ICONWARNING or MB_YESNO)<>IDyes then exit;
                      CleanUpPackagesByBPLPath(CurrentDelphiVersion,FCurrentBPLOutputPath,_DeleteBplAndDCPFiles);
                    end;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: actInstallAllPackagesExecute
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description: add needed registery keys to install all packages.
-----------------------------------------------------------------------------}
procedure TDMMain.actInstallAllPackagesExecute(Sender: TObject);
var
  i: integer;
begin
  ApplicationState:=tas_working;
  try
    for i := 0 to FProjectList.Count - 1 do begin
      SetCurrentProject(FProjectList[i]);
      if FCurrentProjectFilename = '' then continue;
      FireCurrentProjectChanged;
      actInstallPackage.Execute;
    end;
  finally
    ApplicationState:=tas_open;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: actCompileAllPackagesExecute
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description: start command line compiler for each project.
-----------------------------------------------------------------------------}
procedure TDMMain.actCompileAllPackagesExecute(Sender: TObject);
resourcestring
cRebuiltAllProjectsSuccessfully='Rebuilt all projects successfully.';
cSomeProjectsCouldNotBeRebuilt='Some projects could not be rebuilt.' + #13 + #10 + 'See the Log file and add the path in the Options Dialog if ' + #13 + #10 + 'needed.';
var
  i: integer;
  _CompiledProjects: Integer;
begin
  _CompiledProjects := 0;
  FAbortCompile:=false;
  ApplicationState:=tas_working;
  try
    for i := 0 to FProjectList.Count - 1 do begin
      SetCurrentProject(FProjectList[i]);
      if FCurrentProjectFilename = '' then continue;
      FireCurrentProjectChanged;
      actCompilePackage.Execute;
      if FSuccess then inc(_CompiledProjects)
      else if ApplicationSettings.BoolValue('Application/StopOnFailure',6) then break;
      if FAbortCompile then begin
        writelog('Aborted by User.',[]);
        break;
      end;
      Application.ProcessMessages;
    end;
  finally
    ApplicationState:=tas_open;
  end;
  if FProjectList.Count = _CompiledProjects then
  begin
    trace(2, 'Rebuilt all projects successfully.', []);
    if not ApplicationSettings.BoolValue('Application/SilentMode',5) then Application.MessageBox(pchar(cRebuiltAllProjectsSuccessfully),pchar(cInformation),MB_ICONINFORMATION or MB_OK);
    ExitCode := 0;
  end
  else
  begin
    trace(1, 'Some projects could not be rebuilt. See the Log file and add the path in the Options Dialog if needed.', []);
    if not ApplicationSettings.BoolValue('Application/SilentMode',5) then Application.MessageBox(pchar(cSomeProjectsCouldNotBeRebuilt),pchar(cError),MB_ICONERROR or MB_OK);
    ExitCode := 1;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: actUninstallAllPackagesExecute
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Not available
  Result:    Not available
  Description: uninstalls all packages.
-----------------------------------------------------------------------------}
procedure TDMMain.actUninstallAllPackagesExecute(Sender: TObject);
var
  i: integer;
begin
  ApplicationState:=tas_working;
  try
    for i := 0 to FProjectList.Count - 1 do begin
      SetCurrentProject(FProjectList[i]);
      if FCurrentProjectFilename = '' then continue;
      FireCurrentProjectChanged;
      actUninstallPackage.Execute;
    end;
    WriteLog('Uninstalled all Packages.',[])
  finally
    ApplicationState:=tas_open;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: actShutDownDelphiExecute
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.actShutDownDelphiExecute(Sender: TObject);
begin
  ShutDownDelphi(FCurrentDelphiVersion, True);
end;

{*-----------------------------------------------------------------------------
  Procedure: actStartUpDelphiExecute
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.actStartUpDelphiExecute(Sender: TObject);
begin
  if not isDelphiStarted(FCurrentDelphiVersion) then StartUpDelphi(FCurrentDelphiVersion,'');
end;


{-----------------------------------------------------------------------------
  Procedure: CompilePackage
  Author:    sam
  Date:      29-Mai-2008
  Arguments: const _updateCursor: boolean
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function TDMMain.CompilePackage(const _updateCursor: boolean):boolean;
var
_CompilerSwitches: string;
_Output: string;
_SearchPath:string;
begin
  FSuccess:=false;
  // prepare search path
  _CompilerSwitches := ProjectSettings.StringValue('Application/CompilerSwitches',3);
  if _CompilerSwitches='' then _CompilerSwitches := ApplicationSettings.StringValue('Application/CompilerSwitches',11);
  _SearchPath:='';
  if FDPTSearchPath  <> ''    then _SearchPath :=GetGlobalSearchPath; // the search path settings defined in DPT Options-Dialog.
  if FCurrentSearchPath <> '' then begin // the search path settings defined in the .cfg/.dproj file of the current project.
    _SearchPath := _SearchPath + MakeAbsolutePath(ExtractfilePath(FCurrentProjectFilename),FCurrentSearchPath,FCurrentDelphiVersion);
  end;
  if FCurrentConditions <> '' then _CompilerSwitches := _CompilerSwitches +' '+FCurrentConditions + ' ';
  if _SearchPath<>'' then begin
    _SearchPath := RemoveDoublePathEntries(_SearchPath);
    _SearchPath :='"'+_SearchPath +'"';
    _CompilerSwitches := _CompilerSwitches + ' '+'-U'+_SearchPath;
    _CompilerSwitches := _CompilerSwitches + ' '+'-O'+_SearchPath;
    _CompilerSwitches := _CompilerSwitches + ' '+'-I'+_SearchPath;
    _CompilerSwitches := _CompilerSwitches + ' '+'-R'+_SearchPath;
  end;

  trace(5,'CompilePackage: Compiler switch --> %s.',[_CompilerSwitches]);
  trace(5,'Length= %d.',[length(_CompilerSwitches)]);
  WriteLog('*************************************************************************',[]);
  WriteLog('Compiling Project <%s>. Please wait...',[FCurrentProjectFilename]);
  WriteLog('Output Target <%s>.',[FCurrentProjectOutputPath+OutputFileName(FCurrentProjectFilename,FCurrentProjectType)]);
  screen.Cursor := crHourGlass;
  if assigned(FOnCurrentProjectCompileStateChanged) then FOnCurrentProjectCompileStateChanged(self,FCurrentProjectFilename,'Compiling...',DateTimeToStr(Now),'','',FCurrentProjectNo,FCurrentPackageDescription);
  FSuccess := CompileProject(FDelphiCompilerFile,
    _CompilerSwitches,
    ReadProjectFilenameFromDProj(FCurrentProjectFilename),
    FCurrentProjectOutputPath,
    FCurrentDCUOutputPath,
    ExtractFilePath(ReadProjectFilenameFromDProj(FCurrentProjectFilename)),
    FCurrentProjectType,
    _Output,
    FCurrentDelphiVersion);

  if FSuccess then
  begin
    if assigned(FOnCurrentProjectCompileStateChanged) then FOnCurrentProjectCompileStateChanged(self,FCurrentProjectFilename,'Compiled ok!',DateTimeToStr(Now),GetCurrentPackageVersion,format('%4.3f',[GetPackageSize(FCurrentProjectFilename,FCurrentProjectOutputPath,FCurrentPackageSuffix,FCurrentProjectType) / (1024*1024)]),FCurrentProjectNo,FCurrentPackageDescription);
    trace(2,'Successfully compiled Project <%s>.',[FCurrentProjectFilename]);
  end
  else begin
    if assigned(FOnCurrentProjectCompileStateChanged) then FOnCurrentProjectCompileStateChanged(self,FCurrentProjectFilename,'Failed','','','',FCurrentProjectNo,FCurrentPackageDescription);
    if (not ApplicationSettings.BoolValue('Application/SilentMode', 5)) and
       (ApplicationSettings.BoolValue('Application/AutomaticSearchFiles', 18)) then SearchFileCompilerOutput(_Output);
  end;
  screen.Cursor := crDefault;
  WriteLog(_output,[]);
  if ((Pos('fatal', lowercase(_output)) > 0) or
     ((Pos(pchar(cWarning), lowercase(_output)) > 0))) then begin
    if (not ApplicationSettings.BoolValue('Application/SilentMode', 5)) then begin
      if length(_output)>2000 then _output:=copy(_output,length(_output)-2000,2000);
      Application.MessageBox(pchar(_output),pchar(cInformation),MB_ICONINFORMATION or MB_OK);
    end;
    trace(3,'There are problems/warnings in project <%s>. Please see log-file.',[FCurrentProjectFilename]);
  end;
  result:=FSuccess;
end;


{*-----------------------------------------------------------------------------
  Procedure: actExecuteAppExecute
  Author:    sam
  Date:      13-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.actExecuteAppExecute(Sender: TObject);
begin
  ExecuteApp;
end;

{*-----------------------------------------------------------------------------
  Procedure: actFindDCPandBPLExecute
  Author:    sam
  Date:      13-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description: search for the .dcp and .bpl file of the current package.
-----------------------------------------------------------------------------}
procedure TDMMain.actFindDCPandBPLExecute(Sender: TObject);
begin
  ShowBPLSearchDialog(ApplicationSettings.StringValue('Application/LastUsedSearchPath',15),ExtractFilenameOnly(FCurrentProjectFilename));
end;

{*-----------------------------------------------------------------------------
  Procedure: FireCurrentProjectChanged
  Author:    sam
  Date:      13-Mrz-2008
  Arguments: None
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.FireCurrentProjectChanged;
begin
 if assigned(FOnCurrentProjectChanged) then FOnCurrentProjectChanged(self,FCurrentProjectFilename,FCurrentProjectNo);
end;

{-----------------------------------------------------------------------------
  Procedure: SearchFileCompilerOutput
  Author:    sam
  Date:      17-Dez-2005
  Arguments: None
  Result:    None
  Description: try to extract the filename from the compiler output
               and then open the search dialog.
-----------------------------------------------------------------------------}
procedure TDMMain.SearchFileCompilerOutput(_compilerOutput:string);
var
_pos:integer;
_filename:string;
begin
  _filename:=_compilerOutput;
  Delete(_filename,length(_filename)-2,3);
  _pos:=LastPos(_filename,'''');
  delete(_filename,1,_pos);
  SearchFile(_filename,_compilerOutput);
end;

{-----------------------------------------------------------------------------
  Procedure: SearchFile
  Author:    sam
  Date:      17-Dez-2005
  Arguments: _filename:string
  Result:    None
  Description: shows the search dialog to add a search path.
-----------------------------------------------------------------------------}
procedure TDMMain.SearchFile(_filename:string;_compilerOutput:string);
begin
  if ExtractFileExt(_filename)='' then _filename:=_filename+'*.*';
  if (Pos('nicht gefunden',_compilerOutput)>0) or
     (Pos('not found',_compilerOutput)>0) then ShowSelectPathDialog(ApplicationSettings.StringValue('Application/LastUsedSearchPath',15),_filename,true) else
  if (Pos('.dpr',_filename)>0) or
     (Pos('.dpk',_filename)>0) or
     (Pos('.pas',_filename)>0) then begin
    if Pos('<',_filename)=1 then Delete(_filename,1,1);
    if Pos('>.',_filename)=length(_filename)-1 then Delete(_filename,length(_filename)-1,2);
    _filename:=AbsoluteFilename(FBPGPath,_filename);
    ShowFile(_filename,0);
  end;
end;


procedure TDMMain.AbortCompile;
begin
  FAbortCompile:=true;
end;

procedure TDMMain.SetApplicationState(const _newState: TApplicationState);
begin
  case _NewState of
    tas_init:begin
      actUninstallPackage.enabled := false;
      actInstallPackage.enabled := false;
      actCompilePackage.enabled := false;
      actUninstallAllPackages.enabled := false;
      actInstallAllPackages.enabled := false;
      actCompileAllPackages.enabled := false;
      actReCompile.enabled := false;
      actDeleteBPL.enabled := false;
      actRecompileAllPackages.enabled := false;
      actDeleteFiles.enabled := false;
    end;
    tas_open:begin
      actUninstallPackage.enabled := true;
      actInstallPackage.enabled := true;
      actCompilePackage.enabled := true;
      actUninstallAllPackages.enabled := true;
      actInstallAllPackages.enabled := true;
      actCompileAllPackages.enabled := true;
      actReCompile.enabled := true;
      actDeleteBPL.enabled := true;
      actRecompileAllPackages.enabled := true;
      actDeleteFiles.enabled := true;
    end;
  end;
  if assigned(FOnApplicationStateEvent) then FOnApplicationStateEvent(self,FApplicationState,_newState);
  FApplicationState:=_newState;
end;

{*-----------------------------------------------------------------------------
  Procedure: actDeleteFilesExecute
  Author:    sam
  Date:      13-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description:delete all bpl,dcp files.
-----------------------------------------------------------------------------}
procedure TDMMain.actDeleteFilesExecute(Sender: TObject);
var
i: integer;
begin
  ApplicationState:=tas_working;
  try
    for i := 0 to FProjectList.Count - 1 do begin
      SetCurrentProject(FProjectList[i]);
      if FCurrentProjectFilename = '' then continue;
      FireCurrentProjectChanged;
      actDeleteBPLExecute(nil);
    end;
  finally
    ApplicationState:=tas_open;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: SetBPGFilename
  Author:    sam
  Date:      13-Mrz-2008
  Arguments: const Value: string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.SetBPGFilename(const Value: string);
begin
  FBPGFilename :=AbsoluteFilename(extractFilepath(application.ExeName),Value);
  FBPGPath     :=ExtractFilePath(FBPGFilename);
end;

function TDMMain.RemoveProjectFromProjectGroup:boolean;
begin
  result:=uDPTDelphiPackage.RemoveProjectFromProjectGroup(FBPGFilename,FCurrentProjectFilename,FCurrentProjectType);
end;

function TDMMain.PrepareEXEParams(_filename: string; _lineNo: integer;_SourceCodeEditorParams: string): string;
begin
  result:='';
  _SourceCodeEditorParams:=StringReplace(_SourceCodeEditorParams, '%FILENAME%', '"'+_filename+'"',[]);
  _SourceCodeEditorParams:=StringReplace(_SourceCodeEditorParams, '%LINENO%', inttostr(_lineNo),[]);
  result:=_SourceCodeEditorParams;
end;

{*-----------------------------------------------------------------------------
  Procedure: AdaptSearchPath
  Author:    sam
  Date:      11-Apr-2009
  Arguments: None
  Result:    None
  Description: // replace $(DELPHI) with $(BDS) when the user switches delphi version
-----------------------------------------------------------------------------}
procedure TDMMain.AdaptSearchPath;
resourcestring
cCouldNotSaveFile='Could not save file <%s>. Please check user rights. <%s>';
var
_changed:boolean;
_filename:string;
_line:string;
_temp:string;
_file:TStrings;
begin
  _changed:=false;
  if FCurrentDelphiVersion<=7 then begin
    while pos('BDS',FCurrentSearchPath)>0 do begin
      FCurrentSearchPath:=StringReplace(FCurrentSearchPath,'BDS','DELPHI',[]);
      _changed:=true;
    end;
  end
  else begin
    while pos('DELPHI',FCurrentSearchPath)>0 do begin
      FCurrentSearchPath:=StringReplace(FCurrentSearchPath,'DELPHI','BDS',[]);
      _changed:=true;
    end;
  end;
  if not _changed then exit;
  _file:=TStringList.Create;
  try
    _temp:=FCurrentSearchPath;
    while length(_temp)>0 do begin
      _line:=trim(GetField(';',_temp));
      if _line<>'' then _file.Add(_line+';');
    end;
    _filename:=changeFileExt(FBPGFilename,'.txt');
    try
      if not BackupFile(_filename,'.txt_old') then exit;
      _file.SaveToFile(_filename);
    except
      on e:exception do begin
        Application.MessageBox(pchar(format(cCouldNotSaveFile,[_filename,e.message])),pchar(cError),MB_ICONERROR or MB_OK);
        trace(1,'Could not save file <%s>. Please check user rights. <%s>',[_filename,e.message]);
      end;
    end;
  finally
    _file.free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: AutoSaveBackup
  Author:    sam
  Date:      30-Sep-2009
  Arguments: _Lines:TStrings
  Result:    None
  Description: extract folder names from <_Lines> and create a backup zip-file.
-----------------------------------------------------------------------------}
procedure TDMMain.AutoSaveBackup(_Lines:TStrings);
resourcestring
cCouldNotCreateFolder='Could not create folder <%s>. Please try to create the folder manually.';
var
_filename:string;
_path:string;
begin
  if not ProjectSettings.BoolValue('Application/AutoBackup', 12) then exit;
  _filename:=changefileExt(ExtractFilenameOnly(BPGFilename)+'_'+BuildTimeStamp(now),'.zip');
  if ProjectSettings.PathValue('Application/LastUsedBackupPath',11)='' then _path:=extractfilepath(BPGFilename)+'backup\'
                                                                       else _path:=ProjectSettings.PathValue('Application/LastUsedBackupPath',11);
  if not CreateDirectory(_path) then begin
    Application.MessageBox(pchar(format(cCouldNotCreateFolder,[_path])),pchar(cWarning),MB_ICONWARNING or MB_OK);
    exit;
  end;
  _Lines.Insert(0,CurrentProjectFilename);
  SaveBackup(_path+_filename,_Lines);
end;

{*-----------------------------------------------------------------------------
  Procedure: actRecompileAllPackagesExecute
  Author:    sam
  Date:      30-Sep-2009
  Arguments: Sender: TObject
  Result:    None
  Description: recompile&install all projects&packages.
-----------------------------------------------------------------------------}
procedure TDMMain.actRecompileAllPackagesExecute(Sender: TObject);
begin
  ReCompileAndInstallAll;
end;

{-----------------------------------------------------------------------------
  Procedure: UpdateProjectFiles
  Author:    sam
  Date:      06-Mai-2010
  Arguments: None
  Result:
  Description: this method tries to update/correct the file dpk/cfg/dproj/bdsproj
-----------------------------------------------------------------------------}
procedure TDMMain.UpdateProjectFiles(const _ForceWrite:boolean=false);
var
_ChangedFiles:string;
begin
  if not _ForceWrite then begin
    if not ProjectSettings.BoolValue('Application/ChangeFiles', 8) then exit;    // if DPT is allowed to change the files
  end;  

// try to update cfg/dproj/bdsproj files.
  _ChangedFiles:=WriteSettingsToDelphi(FBPGPath,   // prepare the new delphi settings files (cfg,dproj,bdsproj,dof).
                           FCurrentConfigFilename,
                           FCurrentConditions,
                           GetGlobalSearchPath,
                           FCurrentProjectOutputPath,
                           FCurrentBPLOutputPath,
                           FCurrentDCUOutputPath,
                           ApplicationSettings.BoolValue('Application/SilentMode',5),
                           FCurrentProjectType,
                           FCurrentDelphiVersion);
  ConfirmChanges(_ChangedFiles,false,true);

// try to update dpk/dproj files.
  if FCurrentProjectType=tp_bpl then begin // it is a package
    _ChangedFiles:= WritePackageFile(FCurrentDelphiVersion,FCurrentProjectFilename,FCurrentPackageSuffix,ApplicationSettings.BoolValue('Application/SilentMode',5)); // then prepare a new file.
    ConfirmChanges(_ChangedFiles,false);
  end;
end;


{-----------------------------------------------------------------------------
  Procedure: ConfirmChanges
  Author:    sam
  Date:      07-Mai-2010
  Arguments: _ChangedFiles:string
  Result:    None
  Description: present the changed files in the diff-tool and ask the user
               if he want to save the changes.
-----------------------------------------------------------------------------}
procedure TDMMain.ConfirmChanges(_ChangedFiles:string;const _Revert:Boolean;const _ShowQuestions:boolean=false);
resourcestring
cSaveChanges='Save changes to file <%s> ?';
cDPTSuggestsSomeChanges='DelphiPackageTool suggest''s some changes. Do you want to review them in the Diff-Tool ?';
cDPTFoundOldVersionOfFile='DelphiPackageTool could undo the latest changes. Do you want to review them in the Diff-Tool ?';
var
_NewFilename:string;
_OldFilename:string;
_NewFileExt:string;
_msg:string;
begin
  if _ChangedFiles='' then exit;
  _NewFileExt:=GetField(';',_ChangedFiles);
  while _NewFileExt<>'' do begin
    _NewFilename:=ChangeFileExt(FCurrentProjectFilename,_NewFileExt);
    _OldFilename:=copy(_NewFilename,1,length(_NewFilename)-4);
    if fileexists(_NewFilename) and
       fileexists(_OldFilename) then begin
      if _revert then _msg:=cDPTFoundOldVersionOfFile
                 else _msg:=cDPTSuggestsSomeChanges;
      if not ApplicationSettings.BoolValue('Application/SilentMode',5) then
        if Application.MessageBox(pchar(_msg),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=ID_yes then begin
          if _Revert then CompareFiles(_NewFilename,_OldFilename)
                     else CompareFiles(_OldFilename,_NewFilename);
        end;

      if (ApplicationSettings.BoolValue('Application/SilentMode',5)) or
         ( not _ShowQuestions) or
         ((not ApplicationSettings.BoolValue('Application/SilentMode',5) and
           _ShowQuestions) and (Application.MessageBox(pchar(format(cSaveChanges,[_OldFilename])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=ID_yes)) then begin
        if not RemoveReadOnlyFlag(_OldFilename,ApplicationSettings.BoolValue('Application/SilentMode',5)) then exit;
        if not CopyFile(pchar(_NewFilename),pchar(_OldFilename),false) then WriteLog('Problem to change file <%s>.',[_OldFilename])
                                                                       else WriteLog('Changed the file ''%s''',[_OldFilename]);
      end;
    end;
    _NewFileExt:=GetField(';',_ChangedFiles);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: RevertChange
  Author:    sam
  Date:      07-Mai-2010
  Arguments: _filename: string
  Result:    None  
  Description:  look if a '_old' file exists and if so then copy back.
-----------------------------------------------------------------------------}
procedure TDMMain.RevertChange(_filename: string);
begin
  if not fileexists(_filename+'_old') then exit;
  if CopyFile(pchar(_filename+'_old'),pchar(_filename),false) then WriteLog('Reverted last changes of file <%s>.',[_filename])
                                                              else WriteLog('Could not change file <%s>.',[_filename]);
end;

{-----------------------------------------------------------------------------
  Procedure: OldFilesExist
  Author:    herzogs2
  Date:      16-Sep-2010
  Arguments: _ChangedFiles:string
  Result:    boolean
  Description: check if an old file exists.
-----------------------------------------------------------------------------}
function TDMMain.OldFilesExist(_ChangedFiles:string):boolean;
var
_FileExt:string;
_Filename:string;
begin
  result:=false;
  if _ChangedFiles='' then exit;
  _FileExt:=GetField(';',_ChangedFiles);
  while _FileExt<>'' do begin
    _Filename:=ChangeFileExt(FCurrentConfigFilename,_FileExt);
    if fileexists(_Filename) then begin
      result:=true;
      exit;
    end;
    _FileExt:=GetField(';',_ChangedFiles);
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: SetProjectVersionOfFile
  Author:    sam
  Date:      23-Jul-2010
  Arguments: const _filename: string
  Result:    boolean
  Description: set the project version.
-----------------------------------------------------------------------------}
function TDMMain.SetProjectVersionOfFile(_filename:string;Major,Minor,Release,Build:integer): boolean;
begin
  result:=false;
  if not fileexists(extractfilepath(application.exename)+cSetVersionApplicationName) then begin
    trace(2,'Problem in TDMMain.SetProjectVersionOfFile: File <%s> not found.',[extractfilepath(application.exename)+cSetVersionApplicationName]);
    exit;
  end;
  if not fileexists(_filename) then begin
    trace(2,'Problem in TDMMain.SetProjectVersionOfFile: File <%s> not found.',[_filename]);
    exit;
  end;
  BackupFile(changefileext(_filename,'.res'),'.res_old','',false);
  NVBAppExec1.ExeName:=cSetVersionApplicationName;
  NVBAppExec1.ExePath:=extractfilepath(application.exename);
  NVBAppExec1.ExeParams:=format('-v%d.%d.%d.%d -s "%s"',[Major,Minor,Release,Build,_filename]);
  if not NVBAppExec1.Execute then exit;
  result:=true;
end;

{*-----------------------------------------------------------------------------
  Procedure: IncreaseProjectBuildNo
  Author:    sam
  Date:      04-Aug-2010
  Arguments: const _filename: string
  Result:    boolean
  Description: increase the build no.
-----------------------------------------------------------------------------}
function TDMMain.IncreaseProjectBuildNo(const _filename: string): boolean;
begin
  result:=false;
  if not fileexists(extractfilepath(application.exename)+cSetVersionApplicationName) then begin
    trace(2,'Problem in TDMMain.IncreaseProjectBuildNo: File <%s> not found.',[extractfilepath(application.exename)+cSetVersionApplicationName]);
    exit;
  end;
  if not fileexists(_filename) then begin
    trace(2,'Problem in TDMMain.IncreaseProjectBuildNo: File <%s> not found.',[_filename]);
    exit;
  end;
  NVBAppExec1.ExeName:=cSetVersionApplicationName;
  NVBAppExec1.ExePath:=extractfilepath(application.exename);
  NVBAppExec1.ExeParams:='-i -s "'+_filename+'"';
  if not NVBAppExec1.Execute then exit;
  result:=true;
end;

{*-----------------------------------------------------------------------------
  Procedure: GetCurrentPackageVersion
  Author:    sam
  Date:      04-Aug-2010
  Arguments: None
  Result:    string
  Description: read the version number of the current project
-----------------------------------------------------------------------------}
function TDMMain.GetCurrentPackageVersion: string;
begin
  result:=GetPackageVersion(FCurrentProjectFilename,FCurrentProjectOutputPath,FCurrentPackageSuffix,FCurrentProjectType)
end;

{-----------------------------------------------------------------------------
  Procedure: SetProjectVersion
  Author:    sam
  Date:      05-Aug-2010
  Arguments: _filename:string;Major,Minor,Release,Build:integer
  Result:    None
  Description: set the version of .dpr/.dpk/.dproj file.
-----------------------------------------------------------------------------}
function TDMMain.SetProjectVersion(_filenames:TStringList;var ShowVersionDialog:boolean):boolean;
resourcestring
cNoVersionInfo='The project <%s> does not contain version information. Open the project in the Delphi IDE and edit the project options.';
cSetVersionNotFound='The file <%s> was not found. Please download and install the latest Setup of Delphi Package Tool.';
var
i:integer;
_filename:string;
_filenameSetVersion:string;
_Major,_Minor,_Release,_Build:integer;
begin
  result:=false;
  _filenameSetVersion:=extractfilepath(application.exename)+cSetVersionApplicationName;
  if not fileexists(_filenameSetVersion) then begin
    trace(2,'Problem in TDMMain.SetProjectVersion: File <%s> not found.',[_filenameSetVersion]);
    Application.MessageBox(pChar(format(cSetVersionNotFound,[_filenameSetVersion])),pchar(cInformation),MB_ICONWARNING or MB_OK);
    exit;
  end;
  _Major:=0;
  _Minor:=0;
  _Release:=0;
  _Build:=0;
  for i:=0 to _filenames.count-1 do begin
    _filename:=_filenames[i];
    if ShowVersionDialog then begin
      if not GetProjectVersionOfFile(_filename,_Major,_Minor,_Release,_Build) then begin
        if ApplicationSettings.BoolValue('Application/SilentMode',5) then exit;
        Application.MessageBox(pChar(format(cNoVersionInfo,[_filename])),pchar(cInformation),MB_ICONWARNING or MB_OK);
        exit;
      end;
      if not ShowVersionDlg(_filename,_Major,_Minor,_Release,_Build,ShowVersionDialog) then exit;
    end;
    result:=SetProjectVersionOfFile(_filename,_Major,_Minor,_Release,_Build);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: GetProjectVersionOfFile
  Author:    sam
  Date:      05-Aug-2010
  Arguments: _filename: string; var Major, Minor, Release, Build: integer
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function TDMMain.GetProjectVersionOfFile(_filename: string; var Major,Minor, Release, Build: integer): boolean;
const
cResTag='.res:';
var
_returnValue:cardinal;
_Output:string;
_Pos:integer;
_sMajor,_sMinor,_sRelease,_sBuild:string;
begin
  result:=false;
  if not fileexists(changefileext(_filename,'.res')) then begin
    trace(2,'Problem in TDMMain.GetProjectVersionOfFile: File <%s> not found.',[changefileext(_filename,'.res')]);
    exit;
  end;
  _returnValue:=WinExecAndWait(extractfilepath(application.exename)+cSetVersionApplicationName,
                     '-p -s "'+_filename+'"',
                     extractfilepath(application.exename),
                     SW_HIDE,
                     _Output);
  if _returnValue<>0 then begin
    trace(2,'Problem in TDMMain.GetProjectVersionOfFile: <%s> did not work correctly.',[extractfilepath(application.exename)+cSetVersionApplicationName]);
    exit;
  end;
  _Output:=lowercase(_Output);
  _pos:=Pos(cResTag,_Output);
  if _pos=0 then begin
    trace(2,'Problem in TDMMain.GetProjectVersionOfFile: Could not find Tag <%s> in output <%s>.',[cResTag,_Output]);
    exit;
  end;
  delete(_output,1,_pos+4);
  _Output:=trim(_output);
  _sMajor:=GetField('.',_Output);
  _sMinor:=GetField('.',_Output);
  _sRelease:=GetField('.',_Output);
  _sBuild:=GetField('.',_Output);
  if not StringToInteger(_sMajor,Major) then exit;
  if not StringToInteger(_sMinor,Minor) then exit;
  if not StringToInteger(_sRelease,Release) then exit;
  if not StringToInteger(_sBuild,Build) then exit;
  result:=true;
end;

{-----------------------------------------------------------------------------
  Procedure: actRevertChangesExecute
  Author:    sam
  Date:      16-Sep-2010
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.actRevertChangesExecute(Sender: TObject);
begin
  ConfirmChanges(cModifiedFileExtentions,true);
end;

{-----------------------------------------------------------------------------
  Procedure: actWriteDPTPathsToProjectExecute
  Author:    herzogs2
  Date:      04-Okt-2010
  Arguments: Sender: TObject
  Result:    None
  Description: write the path-settings from DPT into the current project files.
-----------------------------------------------------------------------------}
procedure TDMMain.actWriteDPTPathsToProjectExecute(Sender: TObject);
begin
  UpdateProjectFiles(true);
end;

end.
