{-----------------------------------------------------------------------------
 Unit Name: MainDM
 Author:    herzogs2
 Date:      06-Apr-2017
 Purpose:
 History:
-----------------------------------------------------------------------------}
unit MainDM;

interface

uses
  SysUtils,
  Classes,
  uDPTDelphiPackage,
  uDPTSettings,
  uDPTAppExec,
  uDPTDefinitions,
  ActnList,
  System.Actions;

type

  TOnWriteLogEvent=procedure(Sender:TObject;const _msg:string) of object;
  TOnDelphiVersionChangeEvent=procedure(Sender:TObject;const _DelphiVersion:integer) of object;
  TOnPlatformChangeEvent=procedure(Sender:TObject;const _Platforms:string) of object;
  TOnApplicationStateChangeEvent=procedure(Sender:TObject;const _OldState,_NewState:TApplicationState) of object;
  TOnPackageInstallEvent=procedure(Sender:TObject;const _PackageName:string;const _Message:string;const _ProjectNumber:integer) of object;
  TOnCurrentProjectChanged=procedure(Sender:TObject;const _ProjectName:string;const _ProjectNumber:integer) of object;
  TOnCurrentProjectCompileStateChanged=procedure(Sender:TObject;const _ProjectName:string;const _CompileState:string;const _CompileDateTime:string;const _ProjectVersion:string;const _ProjectNumber:integer;const _Description:string) of object;

  TDMMain = class(TDataModule)
    ActionList: TActionList;
    actCleanUpProjectBPLDir: TAction;
    actCleanUpAll: TAction;
    actReCompile: TAction;
    actDeleteBPL: TAction;
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
    FPlatformConfigCompiled: Boolean;
    FProjectCompiled: Boolean;
    FDelphiWasStartedOnApplicationStart: Boolean;
    FApplicationState: TApplicationState;
    FProjectType: TProjectType;        // holds the project-type of the current project.
    FProjectFilename: string;          // real path and filename of the dpk or dpr file.
    FProjectOutputPath: string;        // real path where the output file (.exe or .dll) file will be placed.
    FProjectOutputFilename: string;    // the filename only of the output file. e.g. .exe,.dll,.bpl
    FCurrentProjectNo: Integer;        // the position in the projects list.
    FConfigFilename: string;           // real path and filename of the config-file (delphi 1..8 it was .cfg, 2005,2006 it was .bdsproj, later .dproj
    FPackageDescription: string;       // Description of the project/package. --> Reads $DESCRIPTION
    FBPLFilename: string;              // if the current project is a package then this contains the full .bpl filename and path.
    FBPLOutputPath: string;            // output path for the package files (bpl = borland package library).
    FDCPOutputPath: string;            // output path for the package files (dcp = delphi component package).
    FDCUOutputPath: string;            // output path for the dcu-files.
    FNameSpaces: string;               // namespaces introduced in Delphi 2010 (or before?)
    FUseMSBuild:boolean;               // if true, then msbuild will be used to compile.
    FCompilerSwitches: string;         // compiler switches of the current project.
    FConditions: string;               // the conditions of the current project.
    FProjectSearchPath: string;               // search path of the current project.
    FDPTSearchPath: string;                   // search path defined in DPT Options Dialog.
    FDelphiRootDirectory: string;             // e.g. <C:\Program files\Borland\Delphi7>
    FDelphiCompilerFile: string;              // e.g. <C:\Program files\Borland\Delphi7\bin\dcc32.exe>
    FIDEEnvironmentPath: string;              // the Environment Path-Variable configured for the IDE. content of regitsty-value "HKEY_CURRENT_USER\Software\Embarcadero\BDS\18.0\Environment Variables"
    FBPGPath: string;                         // path to the package group file .bpg
    FBPGFilename: string;                     // the full path and filename of the .bpg file.
    FAbortCompileUser: Boolean;               // set to true if you want to abort compilation
    FAbortCompileFailure: Boolean;            // set to true if compilation of a project failed.
    FBPGProjectList: TStringList;             // list which contains the projects of the project group
    FBPGPlatformList: TStringList;            // list with platforms supported by projects in FBPGProjectList
    FBPGConfigList: TStringList;              // list with configs supported by all projects in FBPGProjectList
    FInstalledDelphiVersionList: TStringList; // list of installed delphi versions on the computer.
    FZipFilename: string;
    FBuildMode:string;                        // build-mode for this project group. e.g. release or debug
    FCommandLineSilent:boolean;               // is set to true if the dpt is started from command line with parameter -s (Silent)
    FOnWriteLog: TOnWriteLogEvent;
    FOnBPGOpen: TNotifyEvent;
    FOnBPGClose: TNotifyEvent;
    FDelphiVersion: Integer;
    FApplicationIniFilename: string;
    FOnDelphiVersionChangeEvent: TOnDelphiVersionChangeEvent;
    FOnApplicationStateEvent: TOnApplicationStateChangeEvent;
    FOnPackageInstalledEvent: TOnPackageInstallEvent;
    FOnPackageUnInstalledEvent: TOnPackageInstallEvent;
    FOnCurrentProjectChanged: TOnCurrentProjectChanged;
    FOnCurrentProjectCompileStateChanged: TOnCurrentProjectCompileStateChanged;
    FDelphiLibraryPath: TDelphiLibraryPath;
    FPackageSuffix: string;
    FOnDeleteLog: TNotifyEvent;
    FPlatformToCompile: string;                // information read from the registery.
    FCurrentBPGPlatformList: TStringList;
    FCurrentBPGConfigList: TStringList;
    FPlatformsToCompileList: TStringList;
    FConfigsToCompileList: TStringList;
    function  GetLibSuffix: string;
    procedure WriteLog(_msg: string;const _params:array of const);
    procedure DeleteLog;
    procedure FireDelphiVersionChanged;
    procedure FireCurrentProjectChanged;
    procedure SetApplicationState(const _newState:TApplicationState);
    procedure SetDelphiVersion(const _Value: Integer);
    function  InitializeAppSettings:boolean;
    procedure SearchFileCompilerOutput(_compilerOutput:string);
    procedure SearchFile(_filename:string;_compilerOutput:string);
    procedure CheckDelphiRunning;
    procedure ExecuteApp;
    function  PrepareEXEParams(_filename:string;_lineNo:integer;_SourceCodeEditorParams:string):string;
    procedure AdaptSearchPath; // replace $(DELPHI) with $(BDS) when the user switches delphi version
    function  SetProjectVersionOfFile(_filename:string;_Major,_Minor,_Release,_Build:integer):boolean;
    function  GetProjectVersionOfFile(_filename:string;out Major,Minor,Release,Build:integer):boolean;
    function  OldFilesExist(_ChangedFiles:string):boolean;
    procedure GetAllPlatformsAndConfigsOfBPG;
    function  CompileCurrentPackageWithDcc: Boolean;
    function  CompileCurrentPackageWithMsBuild: Boolean;
    function  CompileAndInstallCurrentPackage:boolean;
    function  ReplaceTag(_filename: string): string;
    procedure DeleteBPLAndDCPFiles;
    procedure UninstallCurrentPackage;
    function InstallCurrentPackage:boolean;
    procedure InitProjectSettings;
    procedure SetDPTSearchPath(const Value: string);
    procedure LoadCurrentProject;
    function  IsDiffToolAvailable:boolean; // returns true if a external Diff-Tool is setup
    function PrepareConditions:string; // takes the conitions from the project-file and overrides the build mode
  public
    ApplicationSettings: TNVBSettings;
    ProjectSettings: TNVBSettings;
    NVBAppExec1: TNVBAppExec;
    NVBAppExecExternalCommand: TNVBAppExec;
    CommandLineAction: TAction;
    function  GetCurrentPackageVersion:string;
    procedure ConfirmChanges(_ChangedFile:string;const _Revert:Boolean;const _ForceWrite:Boolean); // present the changed files in the diff-tool and ask the user if he want to save the changes.
    procedure RevertChange(_filename:string);  // looks if a _old-file exists
    procedure UpdateProjectFiles(const _ForceWrite: Boolean = False);
    function  RemoveProjectFromProjectGroup:boolean;
    function  ReCompileAndInstallAll:boolean;
    procedure AutoSaveBackup(_Lines:TStrings);
    procedure ShowProjectDir; // open the file explorer and show the current project directory.
    procedure ShowOutputDir;
    function  CompareFiles(_filename1,_filename2:string):boolean; // start the external diff-tool
    procedure SetCurrentProject(const _ProjectName: string);
    procedure ShowFile(_filename:string;_lineno:integer);
    function  CompileCurrentPackage: boolean;
    function  OpenBPG(const _filename: string):boolean;
    procedure CloseBPG;
    function  GetGlobalSearchPath(const _absolutePaths:boolean=true): string;
    procedure AbortCompile;
    procedure SaveBackup(_backupfilename:string;_Lines:TStrings);
    function  SetProjectVersion(_filenames:TStringList;var ShowVersionDialog:boolean):boolean;
    function  IncreaseProjectBuildNo(const _filename: string): boolean;
    procedure ElaboratePlatformsAndConfigsToCompileList;
    procedure InitProjectDataForHint;
    function  CompileAndInstallProjects(ProjectsList: TStringList): Integer;
    function  SearchPath:string;
    procedure SetLastUsedBPGFile(_BPGfilename: string);
    function  IsSilentMode:boolean;
    function  SaveLogToFile(_Log:TStrings):boolean;
    property  Compiler:string read FDelphiCompilerFile;
    property  CurrentProjectFilename: string  read FProjectFilename;
    property  CurrentBPLOutputPath:string read FBPLOutputPath;
    property  DelphiVersion:Integer read FDelphiVersion write SetDelphiVersion; // currently selected delphi version.
    property  PackageSuffix:string read FPackageSuffix;
    property  BPGPath:string read FBPGPath;
    property  BPGFilename:string read FBPGFilename;
    property  DPTSearchPath:string read FDPTSearchPath write SetDPTSearchPath;
    property  BPGProjectList:TStringList read FBPGProjectList;
    property  BPGPlatformList: TStringList read FBPGPlatformList;
    property  BPGConfigList: TStringList read FBPGConfigList;
    property  PlatformToCompile: string read FPlatformToCompile;                // information read from the registry.
    property  BuildMode:String read FBuildMode;
    property  CurrentBPGPlatformList: TStringList read FCurrentBPGPlatformList; // selected platforms for the current project group
    property  CurrentBPGConfigList: TStringList read FCurrentBPGConfigList;     // selected configs for the current project group
    property  ZipFilename:string read FZipFilename;
    property  InstalledDelphiVersionList:TStringList read FInstalledDelphiVersionList;
    property  ApplicationState:TApplicationState read FApplicationState write SetApplicationState;
    property  OnWriteLog:TOnWriteLogEvent read FOnWriteLog write FOnWriteLog;
    property  OnDeleteLog:TNotifyEvent read FOnDeleteLog write FOnDeleteLog;
    property  OnDelphiVersionChange:TOnDelphiVersionChangeEvent read FOnDelphiVersionChangeEvent write FOnDelphiVersionChangeEvent;
    property  OnBPGOpen:TNotifyEvent  read FOnBPGOpen write FOnBPGOpen;
    property  OnBPGClose:TNotifyEvent read FOnBPGClose write FOnBPGClose;
    property  OnApplicationStateChange:TOnApplicationStateChangeEvent read FOnApplicationStateEvent write FOnApplicationStateEvent;
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
  Windows,
  Dialogs,
  Controls,
  StrUtils,
  uDPTJclFuncs,
  uDPTMisc,
  uDTPProjectData,
  uDPTPathFilenameConvert,
  RemovePackagesQuestionFrm,
  BPLSearchFrm,
  PathSelectionFrm,
  VersionFrm;

{$R *.dfm}

const
cModifiedFileExtentions='.cfg_old;.dof_old;.dproj_old;.bdsproj_old;.dpk_old;';

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
  if FProjectType<>tp_exe then exit;
  NVBAppExec1.ExePath:=FProjectOutputPath;
  NVBAppExec1.ExeName:=changeFileExt(extractFileName(FProjectFilename),'.exe');
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
  _DiffToolEXEName:=AbsoluteFilename(extractfilepath(application.exename),ApplicationSettings.StringValue('Application/DiffTool'));
  if _DiffToolEXEName='' then begin
    if not IsSilentMode then Application.MessageBox(pChar(cSetupDiffTool),pchar(cInformation),MB_ICONWARNING or MB_OK);
    exit;
  end;
  if not fileexists(_DiffToolEXEName) then begin
    if not IsSilentMode then Application.MessageBox(pchar(format(cCouldNotFindDiffTool,[_DiffToolEXEName])),pchar(cWarning),MB_ICONWARNING or MB_OK);
    exit;
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
                           else _filename:=ShowSelectPathDialog(SearchPath,ExtractFileName(_filename),false);
    finally
      _lstFiles.free;
    end;
  end;
  _SourceCodeEditorEXEName:=AbsoluteFilename(extractfilepath(application.exename), ApplicationSettings.StringValue('Application/SourceCodeEditor'));
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
  _SourceCodeEditorParams:=ApplicationSettings.StringValue('Application/SourceCodeEditorParams');
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
  Description: returns the configuration filename according to the
               delphi version.
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
  Procedure: LoadCurrentProject
  Author:    muem
  Date:      27-Nov-2012
  Arguments: const _ProjectName: string;
  Result:    None
  Description: 1) read configuration settings from dof,dproj,cfg
                  file.


-----------------------------------------------------------------------------}
procedure TDMMain.LoadCurrentProject;
resourcestring
  cCouldNotFindDCUOutputPath = 'Could not find the DCU Output Path <%s>. Do you want to edit this path ?';
  cCouldNotFindBPLOutputPath = 'Could not find the BPL Output Path <%s>. Do you want to edit this path ?';
begin
  FConfigFilename  :=GetConfigFilename(FProjectFilename, FDelphiVersion);
  uDPTDelphiPackage.ReadConfigurationSettings(FConfigFilename,              // read configuration file.
                                              FBuildMode,
                                              FPlatformToCompile,
                                              FCompilerSwitches,
                                              FConditions,
                                              FProjectSearchPath,
                                              FProjectOutputPath,
                                              FBPLOutputPath,
                                              FDCUOutputPath,
                                              FDCPOutputPath,
                                              FNameSpaces,
                                              FUseMSBuild);
  uDPTDelphiPackage.ReadPackageInfo(FProjectFilename, FPackageDescription, FPackageSuffix);
  FBuildMode:=Uppercase(ProjectSettings.StringValue('Application/BuildMode'));
  FConditions:=PrepareConditions;
// setup the dcu output-path
  if ProjectSettings.PathValue('Application/DCUOutputPath') <> '' then FDCUOutputPath := AbsolutePath(FBPGPath,ProjectSettings.PathValue('Application/DCUOutputPath'), FDelphiVersion, FPlatformToCompile, FBuildMode) // then take it from the dpt
                                                                  else FDCUOutputPath := AbsolutePath(ExtractFilePath(FProjectFilename), FDCUOutputPath, FDelphiVersion, FPlatformToCompile, FBuildMode); // otherwise take the path from the cfg-file.
  FDCUOutputPath := IncludeTrailingPathDelimiter(FDCUOutputPath);
  if not IsSilentMode then begin
    if (FDCUOutputPath <> '') and (not DirectoryExists(FDCUOutputPath)) then begin
      if not IsSilentMode then begin
        if Application.MessageBox(pchar(Format(cCouldNotFindDCUOutputPath, [FDCUOutputPath])), pchar(cConfirm), MB_ICONQUESTION or MB_YesNo) = IdYes then ShowFile(FConfigFilename, 0);
      end;
    end;
    CheckDirectory(FDCUOutputPath,IsSilentMode);
  end;
  trace(5, 'DCUOutputPath=%s', [FDCUOutputPath]);

  FProjectType:=DetermProjectType(FProjectFilename,FBPGFilename,FDelphiVersion);
  case FProjectType of
    tp_exe,tp_dll: begin // setup the exe/dll output-path
        if ProjectSettings.PathValue('Application/OutputPath')<>'' then FProjectOutputPath := AbsolutePath(FBPGPath,ProjectSettings.PathValue('Application/OutputPath'), FDelphiVersion, FPlatformToCompile, FBuildMode)
                                                                   else FProjectOutputPath := AbsolutePath(ExtractFilePath(FProjectFilename), FProjectOutputPath, FDelphiVersion, FPlatformToCompile, FBuildMode); // otherwise take the path from the cfg-file.

    end;

    tp_bpl:begin  // setup the bpl output-path
      if ProjectSettings.PathValue('Application/PackageOutputPath') <> '' then FBPLOutputPath := AbsolutePath(FBPGPath, ProjectSettings.PathValue('Application/PackageOutputPath'), FDelphiVersion, FPlatformToCompile, FBuildMode) // then take it from the dpt
                                                                          else FBPLOutputPath := AbsolutePath(ExtractFilePath(FProjectFilename), FBPLOutputPath, FDelphiVersion, FPlatformToCompile, FBuildMode); // otherwise take the path from the cfg-file.
      FBPLOutputPath := IncludeTrailingPathDelimiter(FBPLOutputPath);
      if not IsSilentMode then begin
        if (FBPLOutputPath <> '') and (not DirectoryExists(FBPLOutputPath)) then begin
          if not IsSilentMode then begin
            if Application.MessageBox(pchar(Format(cCouldNotFindBPLOutputPath, [FBPLOutputPath])), pchar(cConfirm), MB_ICONQUESTION or MB_YesNo) = IdYes then ShowFile(FConfigFilename, 0);
          end;
        end;
        CheckDirectory(FBPLOutputPath,IsSilentMode);
      end;
      trace(5, 'BPLOutputPath=%s', [FBPLOutputPath]);

  // setup the dcp output-path
      FDCPOutputPath:=FBPLOutputPath;
      trace(5, 'DCPOutputPath=%s', [FDCPOutputPath]);
      FProjectOutputPath:=FBPLOutputPath;
    end;
  end;

  FProjectOutputPath := IncludeTrailingPathDelimiter(FProjectOutputPath);
  FPackageSuffix := GetLibSuffix;
  FProjectOutputFilename := OutputFilename(FProjectFilename, FProjectType, FPackageSuffix);
  if FProjectType = tp_bpl then FBPLFilename := FBPLOutputPath + FProjectOutputFilename;
  trace(1, 'ProjectFileName=%s', [FProjectFilename]);
  trace(5, 'Conditions=%s', [FConditions]);
  trace(5, 'ProjectOutputPath=%s', [FProjectOutputPath]);
  trace(5, 'BPLFilename=%s', [FBPLFilename]);
  UpdateProjectFiles;
end;

{-----------------------------------------------------------------------------
  Procedure: TDMMain.GetGlobalSearchPath
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
i: integer;
_SearchPath: TStrings;
_currentPath: string;
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
    for i := 0 to _SearchPath.count - 1 do begin
      _currentPath := trim(_SearchPath[i]);
      if LastPos(_currentPath, ';') = length(_currentPath) then Delete(_currentPath, length(_currentPath), 1);
      if LastPos(_currentPath, '\') = length(_currentPath) then Delete(_currentPath, length(_currentPath), 1);
      if _absolutePaths then begin
        _currentPath:=AbsolutePath(FBPGPath,_currentPath,FDelphiVersion, FPlatformToCompile, FBuildMode);
        if _currentPath='' then continue;
        result := Result + _currentPath + ';';
        trace(6,'GetGlobalSearchPath: Added <%s> to search path.',[_currentpath]);
        if not DirectoryExists(_currentPath) then trace(3, 'Possible problem in GetGlobalSearchPath: Could not find path <%s>.', [_currentPath]);
      end
      else if _currentPath<>'' then result := result + _currentPath + ';';
    end;
  finally
    _SearchPath.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TDMMain.GetLibSuffix
  Author:    sam
  Date:      08-Nov-2007
  Arguments: None
  Result:    string
  Description: returns the current lib-suffix.
-----------------------------------------------------------------------------}
function TDMMain.GetLibSuffix: string;
begin
  result:='';
  if FProjectType<>tp_bpl then exit; // only suffix for bpl-files is needed.
  if lowercase(ProjectSettings.StringValue('Application/LibSuffix'))=lowercase(cLIBAutomaticTag) then result:=DelphiVersions[FDelphiVersion].PackageVersion else
  if lowercase(ProjectSettings.StringValue('Application/LibSuffix'))=lowercase(cLIBNoneTag) then result:=''
  else result:=ProjectSettings.StringValue('Application/LibSuffix');
end;

{*-----------------------------------------------------------------------------
  Procedure: InitProjectSettings
  Author:    sam
  Date:      16-Mai-2015
  Arguments: None
  Result:    None
  Description: init project settings.
-----------------------------------------------------------------------------}
procedure TDMMain.InitProjectSettings;
begin
  ProjectSettings.GetBoolValue('Application/ChangeFiles',true,'If set to true, the DelphiPackageTool does change your files.',true,false,false);
  ProjectSettings.GetStringValue('Application/Events/OnBeforeInstallAll','','The name of the batch file which will be executed when user presses "Install All".', true,false,false);
  ProjectSettings.GetStringValue('Application/Events/OnAfterInstallAll','','The name of the batch file which will be executed after all projects have been compiled successfully.', true,false,false);
  ProjectSettings.GetStringValue('Application/CompilerSwitches','-B -Q -W -H','This settings contains the compiler switches.',true,false,false);
  ProjectSettings.GetBoolValue('Application/CreateInstallBatch',false,'If enabled, then a compile&install batch file will be created.', true,false,false);
  ProjectSettings.GetIntegerValue('Application/DelphiVersion',7,'The compiler Version used for this project.',true,false,false);
  ProjectSettings.GetPathValue('Application/PackageOutputPath', 'bpl\$(DELPHIVERSION)\', 'Path to the output directory for bpl/dcp files.', true,false,false);
  ProjectSettings.GetPathValue('Application/OutputPath', 'bin\$(DELPHIVERSION)\', 'Path to the output directory for exe/dll files.', true,false,false);
  ProjectSettings.GetPathValue('Application/DCUOutputPath', 'dcu\$(DELPHIVERSION)\', 'Output Path for the dcu-files.', true,false,false);
  ProjectSettings.GetStringValue('Application/LibSuffix',cLIBNoneTag,'Defines the Lib-Suffix for the Package-Names.',true,false,false);
  ProjectSettings.GetPathValue('Application/LastUsedBackupPath','','Defines last used Backup Path.',true,false,false);
  ProjectSettings.GetStringValue('Application/Platform','','The platform used. e.g Win32,Win64.',true,false,false);
  ProjectSettings.GetStringValue('Application/AvailableBuildModes','DEBUG,RELEASE','The available build-modes used. e.g debug,release.',true,false,false);
  ProjectSettings.GetStringValue('Application/BuildMode','DEBUG','The build-modes used for this project group.',true,false,false);
  ProjectSettings.GetStringValue('Application/DebugCompilerSwitches','','Compiler switches used for debug config, if there is no *.cfg or *.dproj',true,false,false);
  ProjectSettings.GetStringValue('Application/ReleaseCompilerSwitches','','Compiler switches used for release config, if there is no *.cfg or *.dproj',true,false,false);
  ProjectSettings.GetBoolValue('Application/AutoBackup',false,'If set to true, the DelphiPackageTool will create backup zip-file after compiling all projects.',true,false,false);
end;

{*-----------------------------------------------------------------------------
  Procedure: OpenBPG
  Author:    sam
  Date:      02-Feb-2008
  Arguments: const _Filename: string
  Result:    None
  Description: open the package group file.
-----------------------------------------------------------------------------}
function TDMMain.OpenBPG(const _Filename: string): Boolean;
resourcestring
cCouldNotFindFile=' Could not find the file <%s>. Please check filename.';
var
_NoOfRemovedEnvironmentPaths:integer;
begin
  result := false;
  CloseBPG;
  if _Filename='' then exit;
  FBPGFilename:=AbsoluteFilename(extractFilepath(application.ExeName),_Filename);
  if not FileExists(FBPGFilename) then begin
    Application.MessageBox(pchar(format(cCouldNotFindFile,[FBPGFilename])),pchar(cError),MB_ICONERROR or MB_OK);
    WriteLog('The file <%s> does not exist. Please check parameters.', [FBPGFilename]);
    trace(1, 'Problem in OpenBPG: The file <%s> does not exist.', [FBPGFilename]);
    FBPGFilename:='';
    exit;
  end;
  FBPGPath:=ExtractFilePath(FBPGFilename);
  ProjectSettings.Filename := ChangeFileExt(FBPGFilename, '.ini');
  InitProjectSettings;
  if ProjectSettings.Open then trace(3,'Load project settings from file <%s>.',[ProjectSettings.FilePath+ProjectSettings.FileName]);
  DelphiVersion := ProjectSettings.IntegerValue('Application/DelphiVersion');
  CurrentBPGPlatformList.CommaText := ProjectSettings.StringValue('Application/Platform');
  CurrentBPGConfigList.CommaText := ProjectSettings.StringValue('Application/AvailableBuildModes');
  FBuildMode:=Uppercase(ProjectSettings.StringValue('Application/BuildMode'));

  gCreateBatchFile:=DMMain.ProjectSettings.BoolValue('Application/CreateInstallBatch');
  ReadPackageListfromFile(FBPGFilename, FBPGProjectList);
  DPTSearchPath := GetGlobalSearchPath;
  GetAllPlatformsAndConfigsOfBPG;
  SetLastUsedBPGFile(BPGFilename);
  VerifyIDEEnvrionmentsPath(FDelphiVersion,IsSilentMode,_NoOfRemovedEnvironmentPaths);
  ApplicationState := tas_open;

  InitProjectDataForHint;
  SetCurrentProject(FBPGProjectList[0]);
  if assigned(FOnBPGOpen) then FOnBPGOpen(Self);
  result := true;
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
var
  i: Integer;
begin
  if FApplicationState <> tas_open then Exit;
  if (ProjectSettings.FileName <> '') and
     (not assigned(CommandLineAction)) then begin
    trace(3, 'Save project settings to file <%s>.', [ProjectSettings.FilePath + ProjectSettings.FileName]);
    ProjectSettings.SaveConfig;
  end;
  ProjectSettings.Close;
  ProjectSettings.FileName := '';
  FBPGPlatformList.Clear;
  FBPGConfigList.Clear;
  FProjectType := tp_unkown;
  FProjectFilename := '';
  FProjectOutputPath := '';
  FConfigFilename := '';
  FPackageDescription := '';
  FBPLFilename := '';
  FBPLOutputPath := '';
  FDCPOutputPath := '';
  FDCUOutputPath := '';
  FConditions := '';
  FProjectSearchPath := '';
  FNameSpaces := '';
  FCompilerSwitches := '';
  FCurrentBPGPlatformList.Clear;
  FCurrentBPGConfigList.Clear;
  for i := FBPGProjectList.Count-1 downto 0 do begin
    if Assigned(FBPGProjectList.Objects[i]) then FBPGProjectList.Objects[i].Free;
  end;
  FBPGProjectList.Clear;
  ApplicationState := tas_init;
  if assigned(FOnBPGClose) then FOnBPGClose(self);
end;

procedure TDMMain.ShowProjectDir;
begin
  NVBAppExec1.ExePath:=GetWindowsPath;
  NVBAppExec1.ExeName:='explorer.exe';
  NVBAppExec1.ExeParams:=ExtractFilePath(FProjectFilename);
  NVBAppExec1.Execute;
end;

procedure TDMMain.ShowOutputDir;
begin
  NVBAppExec1.ExePath:=GetWindowsPath;
  NVBAppExec1.ExeName:='explorer.exe';
  NVBAppExec1.ExeParams:=ExtractFilePath(FProjectOutputPath);
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
const
  // Commandline parameters
  cCleanupBplDir = '-cleanupbpldir';
  cCleanupAll    = '-cleanupall';
  cProject       = '-p';           //e.g. -p"C:\Projects\MyComponents\BuildAllProjects&PackagesD7.bpg" -Rebuild
  cRebuild       = '-rebuild';
  cConfig        = '-n';           //e.g. -nRelease
  cPlatform      = '-a';           //e.g. -aWin32
  cInstall       = '-install';
  cUninstall     = '-uninstall';
  cProjIniFile   = '-o';           //e.g. –o”myComponents.ini” –rebuild
  cSilent        = '-silent';
resourcestring
  cRegisterBPG = 'Do you want to register file type (*.bpg) with the Delphi Package Tool ?';
  cRegisterBDSGroup = 'Do you want to register file type (*.bdsgroup) with the Delphi Package Tool ?';
  cRegisterBDSGroupProj = 'Do you want to register file type (*.groupproj) with the Delphi Package Tool ?';

var
i: integer;
_ParamStr: string;
_filename: string;
_Config: string;
_Platform: string;
begin
  _Config := '';
  _Platform := '';
  FCommandLineSilent:=(pos(cSilent,lowercase(GetCommandLine))>0);
  ApplicationState := tas_init;
  FDelphiVersion := LatestIDEVersion;
  FBPLOutputPath := '';
  FDCUOutputPath := '';
  FApplicationIniFilename := changefileext(application.ExeName, '.ini');
  FPlatformToCompile := '';
  FUseMSBuild:=false;
  FBuildMode:='DEBUG';
// prepare trace file
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

  FInstalledDelphiVersionList := TStringList.Create;

  FBPGProjectList:=TStringList.Create;
  FBPGPlatformList:=TStringList.Create;
  FBPGPlatformList.Sorted := True;
  FBPGPlatformList.Duplicates := dupIgnore;
  FBPGConfigList:=TStringList.Create;
  FBPGConfigList.Sorted := True;
  FBPGConfigList.Duplicates := dupIgnore;
  FCurrentBPGPlatformList := TStringList.Create;
  FCurrentBPGConfigList := TStringList.Create;
  FPlatformsToCompileList := TStringList.Create;
  FConfigsToCompileList := TStringList.Create;
  FZipFilename := extractfilepath(application.ExeName)+'zipdll.dll';
  GetInstalledIDEVersions(FInstalledDelphiVersionList);
  InitializeAppSettings; // load application settings.
  FDelphiWasStartedOnApplicationStart := isDelphiStarted(FDelphiVersion);

  case ParamCount of
    0: OpenBPG(ApplicationSettings.FileValue('Application/ProjectGroupFile'));
    else begin
      for i := 1 to ParamCount do begin
        _ParamStr := lowercase(Paramstr(i));
        trace(2,'Parameter <%d> is <%s>.',[i,_ParamStr]);
        if Pos(cProject, _ParamStr) = 1 then begin // project file (either bpg,dpk,dpr)
          Delete(_ParamStr, 1, Length(cProject)); //e.g. -p"C:\Projects\Packages\Owncomponents.bpl"
          if pos('"',_ParamStr)=1 then delete(_ParamStr,1,1); // remove the leading char <">.
          if pos('"',_ParamStr)>0 then _ParamStr:=copy(_ParamStr,1,pos('"',_ParamStr)); //copy until the occurence of the char <">.
          _filename := lowercase(_ParamStr);
          _filename := AbsoluteFilename(extractFilepath(application.ExeName),_filename);
          OpenBPG(_filename);
          break;
        end else
        if Pos(cRebuild, _ParamStr) = 1 then begin
          if (Pos('.bpg', _filename) > 0) or
             (Pos('.bdsgroup',_filename)>0) or
             (Pos('.groupproj',_filename)>0) then CommandLineAction := actRecompileAllPackages;
          if (Pos('.dpk', _filename) > 0) or
             (Pos('.dpr', _filename) > 0) or
             (Pos('.dproj', _filename) > 0)then begin
            SetCurrentProject(_filename);
            CommandLineAction := actReCompile;
          end;
          break;
        end else
        if Pos(cCleanupBplDir, _ParamStr) = 1 then begin
          CommandLineAction := actCleanUpProjectBPLDir;
          break;
        end else
        if Pos(cCleanupAll, _ParamStr) = 1    then begin
          CommandLineAction := actCleanUpAll; // command to be executed.
          break;
        end else begin
          OpenBPG(_ParamStr);
          break;
        end;
      end;
    end;

    if Pos(cConfig, _ParamStr) = 1 then begin // configuration to build (e.g. debug,release)
      Delete(_ParamStr, 1, Length(cConfig)); //e.g. -nRelease
      _Config := _ParamStr;
    end;

    if Pos(cPlatform, _ParamStr) = 1 then begin // platform to build (e.g. Win32,Win64)
      Delete(_ParamStr, 1, Length(cPlatform)); //e.g. -aWin32
      _Platform := _ParamStr;
    end;

    if Pos(cInstall, _ParamStr) = 1 then begin // command to be executed.
      if (Pos('.dpk', _filename) > 0) or
         (Pos('.dproj', _filename) > 0) then CommandLineAction := actInstallPackage;
    end;

    if Pos(cUninstall, _ParamStr) = 1 then begin // command to be executed.
      if (Pos('.dpk', _filename) > 0) or
         (Pos('.dproj', _filename) > 0) then CommandLineAction := actUninstallPackage;
    end;

    if Pos(cProjIniFile, _ParamStr) = 1 then begin // project ini-file
      Delete(_ParamStr, 1, Length(cProjIniFile)); //e.g. -OC:\Projects\Packages\Owncomponents.ini
      FApplicationIniFilename := AbsoluteFilename(extractFilepath(Application.exename),_ParamStr);
      InitializeAppSettings; // load application settings.
      CommandLineAction := actRecompileAllPackages;
    end;
  end;

  if _Config <> '' then CurrentBPGConfigList.CommaText := _Config;
  if _Platform <> '' then CurrentBPGPlatformList.CommaText := _Platform;

  if (not fileexists(FApplicationIniFilename)) and
     (not IsSilentMode) then begin  // on the first start, ask if the filetypes shall be registered.
    if MessageBox(0,pchar(cRegisterBPG),PChar(cConfirm), MB_ICONQUESTION or MB_YESNO)           = IdYes then RegisterFileType('bpg'      ,Application.ExeName,'DelphiPackageTool File-Type BPG');
    if MessageBox(0,pchar(cRegisterBDSGroup),pchar(cConfirm), MB_ICONQUESTION or MB_YESNO)      = IdYes then RegisterFileType('bdsgroup' ,Application.ExeName,'DelphiPackageTool File-Type BDSGroup');
    if MessageBox(0,pchar(cRegisterBDSGroupProj),pchar(cConfirm), MB_ICONQUESTION or MB_YESNO)  = IdYes then RegisterFileType('groupproj',Application.ExeName,'DelphiPackageTool File-Type GROUPPROJ');
  end;
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
cNo7zipFound='Could not find 7-zip. You must install 7-zip v9.25 or higher to use the backup feature.';
var
i:integer;
_line:string;
_filename:string;
_BatchZipFile:TStrings;
_FileList:TStringList;
_BackupFileList:string;
_batchFilename:string;
_backupPath:string;
begin
  if Get7zAppName='' then begin
    Application.MessageBox(pchar(cNo7zipFound),pchar(cWarning),MB_OK);
    exit;
  end;
  ProjectSettings.SetPath('Application/LastUsedBackupPath',extractFilePath(_backupfilename));
  writeLog('Searching for files to backup. Please wait...',[]);
  _FileList:=ExtractFilenamesFromDCC32Output(BPGPath,_Lines,ApplicationSettings.BoolValue('Application/BackupSourceOnly'));
  for i:=0 to FBPGProjectList.count-1 do begin
    _filename:=lowercase(trim(FBPGProjectList[i]));
    _filename:=AbsoluteFilename(FBPGPath,_filename);
    if _FileList.IndexOf(_filename)=-1 then begin
      if fileexists(_filename) then _FileList.add(_filename);
      _filename:=changefileext(_filename,'.res');
      if fileexists(_filename) then _FileList.add(_filename);
      _filename:=changefileext(_filename,'.cfg');
      if fileexists(_filename) then _FileList.add(_filename);
      _filename:=changefileext(_filename,'.dof');
      if fileexists(_filename) then _FileList.add(_filename);
    end;
  end;

  try
    writeLog('Found <%d> files. Creating zip-file...',[_FileList.Count]);
    if _FileList.Count=0 then exit;
    if fileexists(_backupfilename) then BackupFile(_backupfilename);
    _backupPath:=extractfilepath(_backupfilename);
    _backupFilename:=extractfilename(changefileext(_backupfilename,'.7z'));
    _BackupFileList:='FileList_to_Backup.txt';
    _FileList.SaveToFile(_backupPath+_BackupFileList);
    writelog('Saved filelist to <%s>.',[_backupPath+_BackupFileList]);
    _batchFilename:=_backupPath+'backup_template.bat';
    _BatchZipFile:=TStringList.create;
    try
      _BatchZipFile.add(Get7zAppName+' a -t7z "ARCHIVENAME" @"FILELIST" -spf');  // needs 7zip 9.25 or higher.
      _BatchZipFile.add('pause');
      _BatchZipFile.SaveToFile(_batchFilename);
    finally
      _BatchZipFile.free;
    end;
    if not IsSilentMode then Application.MessageBox(pchar(format(cCreateBackup,[_batchFilename])),pchar(cInformation),MB_ICONINFORMATION or MB_OK);
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
    if not IsSilentMode then begin
      if Application.MessageBox(pchar(cOpenBackupFolder),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDYes then ShowFolder(_backupPath);
    end;
  finally
    _FileList.free;
  end;
end;

function TDMMain.SaveLogToFile(_Log: TStrings): boolean;
var
_filename:string;
begin
  result:=false;
  if not assigned(_Log) then exit;
  if FBPGFilename='' then exit;
  if not ApplicationSettings.BoolValue('Application/SaveLogToFile') then exit;
  if not SysUtils.DirectoryExists(FBPGPath) then exit;
  _filename:=ChangeFileExt(FBPGFilename, '.log');
  try
    _Log.SaveToFile(_filename);
    trace(5,'Saved log to file <%s>.',[_filename]);
    result:=true;
  except
    on e:exception do trace(1,'Error in TDMMain.SaveLogToFile: Could not write Logfile <%s>. <%s>.',[_filename,e.message]);
  end;
end;

procedure TDMMain.WriteLog(_msg: string;const _params: array of const);
begin
  if not assigned(FOnWriteLog) then exit;
  if sizeof(_params)>0 then _msg:=format(_msg,_params);
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
var
  i: Integer;
begin
  if (ApplicationSettings.BoolValue('Application/StartDelphiOnClose') and
     (not assigned(CommandLineAction))) or
    ((FDelphiWasStartedOnApplicationStart) and
    (not IsSilentMode)) then actStartUpDelphiExecute(nil);

  if not assigned(CommandLineAction) then ApplicationSettings.SaveConfig;
  ApplicationSettings.Close;
  FWriteMsg:=nil;
  FInstalledDelphiVersionList.free;
  FPlatformsToCompileList.Free;
  FConfigsToCompileList.Free;
  FCurrentBPGConfigList.Free;
  FCurrentBPGPlatformList.Free;
  FBPGConfigList.Free;
  FBPGPlatformList.Free;
  for i := FBPGProjectList.Count-1 downto 0 do begin
    if not Assigned(FBPGProjectList.Objects[i]) then continue;
    FBPGProjectList.Objects[i].Free;
  end;
  FBPGProjectList.Free;
end;

{-----------------------------------------------------------------------------
  Procedure: SetDelphiVersion
  Author:    sam
  Date:
  Arguments: const Value: Integer
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.SetDelphiVersion(const _Value: Integer);
resourcestring
cAskForIDE='The IDE <%s> you used last time for this project is not installed on this computer! Do you want to open this project with IDE <%s>?';
begin
  if not IsIDEInstalled(_Value) then begin
    if not IsSilentMode then begin
      if Application.MessageBox(pchar(format(cAskForIDE,[VersionNoToIDEName(_Value),VersionNoToIDEName(LatestIDEVersion)])),pchar(cConfirm),MB_ICONQUESTION or MB_YesNo)<>IdYes then Application.terminate;
    end;
    FDelphiVersion:=LatestIDEVersion;
  end
  else FDelphiVersion := _Value;
  ApplicationSettings.SetString('Application/PathNameFile', 'DelphiPackageToolPathD' + inttostr(FDelphiVersion) + '.txt');
  trace(3,'Set current delphi version to <%d>.',[FDelphiVersion]);
  FDelphiRootDirectory:=GetDelphiRootDir(FDelphiVersion);
  trace(3,'Set the Compiler Root Directory to <%s>.',[FDelphiRootDirectory]);
  FIDEEnvironmentPath:=GetIDEEnvironmentPath(FDelphiVersion);
  trace(3,'Read "Environments PATH" for IDE: <%s>.',[FIDEEnvironmentPath]);
  ReadLibraryPath(FDelphiVersion,FDelphiLibraryPath);
  DPTSearchPath := GetGlobalSearchPath;
  GetAllPlatformsAndConfigsOfBPG;
  AdaptSearchPath;
  InitProjectDataForHint;
  FireDelphiVersionChanged;
end;

{*-----------------------------------------------------------------------------
  Procedure: InitializeAppSettings
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: none
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function TDMMain.InitializeAppSettings:boolean;
var
i:integer;
begin
  ApplicationSettings := TNVBSettings.Create(Self);
  ApplicationSettings.Name := 'ApplicationSettings';
  ApplicationSettings.AutoSave := False;
  ApplicationSettings.OnError := ApplicationSettingsError;
  ApplicationSettings.CryptIt := False;
  ApplicationSettings.FileName := FApplicationIniFilename;
  ApplicationSettings.GetIntegerValue('Compiler/DelphiVersion',LatestIDEVersion, 'Delphi Version', true,false,false);
  ApplicationSettings.GetFileValue('Application/ProjectGroupFile', '', 'The name of Borland Package Group File', true,false,false);
  ApplicationSettings.GetBoolValue('Application/SilentMode', False, 'If this settings is true, then no dialog boxes will be shown.', true,false,false);
  ApplicationSettings.GetBoolValue('Application/StopOnFailure', true, 'If a failure occures during a batch process like <rebuild all>, then the applications stops.', true,false,false);
  ApplicationSettings.GetBoolValue('Application/StartDelphiOnClose', False, 'Start Delphi when this application terminates.', true,false,false);
  ApplicationSettings.GetStringValue('Application/PathNameFile', 'DelphiPackageToolPathD'+inttostr(FDelphiVersion)+'.txt', 'The file containing the search path for the compiler.', true,false,false);
  ApplicationSettings.GetStringValue('Application/SourceCodeEditor','..\Notepad++\notepad++.exe','Define the Source code Editor (e.g. Notepad++).',true,false,false);
  ApplicationSettings.GetBoolValue('Application/ShowStartUpWarning', true, 'If true then the startup information screen is shown.', true,false,false);
  ApplicationSettings.GetStringValue('Application/CompilerSwitches','-B -Q -W -H','This settings contains the compiler switches.',true,false,false);
  ApplicationSettings.GetIntegerValue('Application/Tracelevel',3,'Select the trace level of the Log-file. 1-5.',true,false,false);
  ApplicationSettings.GetBoolValue('Application/DisplayFilesInDiffTool',true,'If set to true, the DelphiPackageTool does show the changed files in the external Diff-Tool.',true,false,false);
  ApplicationSettings.GetStringValue('Application/LastUsedSearchPath','C:\','Specifies the last used search path.',true,false,false);
  ApplicationSettings.GetBoolValue('Application/AutomaticSearchFiles', true, 'If the compilation aborts because a file was not found and this is set to True then the search dialog opens automatically.', true,false,false);
  ApplicationSettings.GetStringValue('Application/LastUsedInputFile','','Last used project name.',true,false,false);
  ApplicationSettings.GetBoolValue('Application/SaveLogToFile',false,'If set to true, the log output will be save to a file.',true,false,false);
  ApplicationSettings.GetBoolValue('Application/trace',false,'The application will trace all steps.',true,false,false);
  ApplicationSettings.GetPathValue('Application/LastLogOutputPath','','Last used path to store the log file.',true,false,false);
  ApplicationSettings.GetPathValue('Application/LastZipOutputPath','','Last used path to store the zip file.',true,false,false);
  ApplicationSettings.GetIntegerValue('Application/Position/Left',0,'Stores the last left position of the Main-Form.',true,false,false);
  ApplicationSettings.GetIntegerValue('Application/Position/Top',0,'Stores the last top position of the Main-Form.',true,false,false);
  ApplicationSettings.GetIntegerValue('Application/Position/Width',1200,'Stores the last width of the Main-Form.',true,false,false);
  ApplicationSettings.GetIntegerValue('Application/Position/Height',600,'Stores the last height of the Main-Form.',true,false,false);
  ApplicationSettings.GetStringValue('Application/LastUsedExtnsion','.bpg','Stores the last used file-type in the file-open dialog.',true,false,false);
  ApplicationSettings.GetStringValue('Application/SourceCodeEditorParams','%FILENAME%','Define the Source code Editor command Line Parameters.',true,false,false);
  ApplicationSettings.GetFileValue('Application/DiffTool', '..\Beyond Compare 2\BC2.exe', 'Define here your favorite Diff-Tool (e.g. BeyondCompare).', true,false,false);
  ApplicationSettings.GetBoolValue('Application/BackupSourceOnly',false,'Only sourcefiles will be taken into the zip-file.',true,false,false);
  for i:=1 to 10 do ApplicationSettings.GetStringValue(format('Application/FileHistory/Item%d',[i]),'',format('Recently used File <%d>.',[i]), true,false,false);
  for i:=1 to 10 do ApplicationSettings.GetStringValue(format('Application/SearchPathHistory/Item%d',[i]),'',format('Recently used File <%d>.',[i]), true,false,false);
  if ApplicationSettings.Open then trace(3,'Loaded application settings from file <%s>.',[FApplicationIniFilename])
                              else trace(3,'Starting application with default settings.',[]);
  DelphiVersion:=ApplicationSettings.IntegerValue('Compiler/DelphiVersion');
  result:=true;
end;

{*-----------------------------------------------------------------------------
  Procedure: FireDelphiVersionChanged
  Author:    muem
  Date:      09-Nov-2012
  Arguments: None
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.FireDelphiVersionChanged;
begin
  if assigned(FOnDelphiVersionChangeEvent) then FOnDelphiVersionChangeEvent(self,FDelphiVersion);
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
  if CleanUpPackagesByPath(FDelphiVersion,FBPLOutputPath,FDCPOutputPath,true, FPlatformToCompile, FBuildMode) then
    ExitCode := 0
  else
    ExitCode := 1;
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
  CleanUpPackagesByRegistry(HKEY_LOCAL_MACHINE,FDelphiVersion,'Known Packages'   ,true, FPlatformToCompile, FBuildMode);
  CleanUpPackagesByRegistry(HKEY_CURRENT_USER ,FDelphiVersion,'Known Packages'   ,true, FPlatformToCompile, FBuildMode);
  CleanUpPackagesByRegistry(HKEY_LOCAL_MACHINE,FDelphiVersion,'Disabled Packages',true, FPlatformToCompile, FBuildMode);
  CleanUpPackagesByRegistry(HKEY_CURRENT_USER ,FDelphiVersion,'Disabled Packages',true, FPlatformToCompile, FBuildMode);
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
  cRebuiltAllProjects = 'Rebuilt all projects successfully.';
  cSomeProjectsCouldNotBeRebuilt = 'Some projects could not be rebuilt.' + #13 +
    #10 + 'See the Log file and add the path in the Options Dialog if ' + #13 +
    #10 + 'needed.';
var
  _CompiledProjects: integer;
  _start: cardinal;
  _end: cardinal;
  _batchFilename: string;
  _filename: string;
begin
  result := false;
  FAbortCompileUser := false;
  FAbortCompileFailure:=false;
  _batchFilename := ProjectSettings.StringValue('Application/Events/OnBeforeInstallAll');
  // take the project specific settings
  if _batchFilename <> '' then begin
    _batchFilename := AbsoluteFilename(FBPGPath, _batchFilename);
    NVBAppExecExternalCommand.ExePath := ExtractFilePath(_batchFilename);
    NVBAppExecExternalCommand.ExeName := extractFileName(_batchFilename);
    if NVBAppExecExternalCommand.Execute then trace(2, 'Executed batch file <%s>.', [_batchFilename]);
  end;
  _start := gettickcount;
  DeleteLog;
  WriteLog('Start to compile all projects of file <%s>.', [FBPGFilename]);
  InitBatchFile(FBPGPath + 'batch\'+changeFileExt(extractFileName(FBPGFilename), '.bat'));
  ApplicationState := tas_working;
  try
    _CompiledProjects := CompileAndInstallProjects(FBPGProjectList);
    _filename := SaveBatchFile(IsSilentMode);
    if _filename <> '' then WriteLog('Saved batch file <%s>.', [_filename]);
  finally
    ApplicationState := tas_open;
  end;
  if FBPGProjectList.Count = _CompiledProjects then begin
    WriteLog('Rebuilt all <%d> projects successfully.', [_CompiledProjects]);
    _end := ((gettickcount - _start) div 1000);
    WriteLog('It took <%d> seconds to compile all projects.', [_end]);
    CheckDelphiRunning;
    if not IsSilentMode then Application.MessageBox(pChar(cRebuiltAllProjects), pChar(cInformation), MB_ICONINFORMATION or MB_OK);
    // take the project specific settings
    _batchFilename := ProjectSettings.StringValue('Application/Events/OnAfterInstallAll');
    if _batchFilename <> '' then begin
      _batchFilename := AbsoluteFilename(FBPGPath, _batchFilename);
      NVBAppExecExternalCommand.ExePath := ExtractFilePath(_batchFilename);
      NVBAppExecExternalCommand.ExeName := extractFileName(_batchFilename);
      if NVBAppExecExternalCommand.Execute then trace(2, 'Executed batch file <%s>.', [_batchFilename]);
    end;
    ExitCode := 0;
    result := true;
  end
  else begin
    _end := ((gettickcount - _start) div 1000);
    WriteLog('It took <%d> seconds to compile the projects.', [_end]);
    WriteLog('Compiled <%d> Projects of Total <%d> Projects.', [_CompiledProjects, FBPGProjectList.count]);
    WriteLog('Some projects could not be rebuilt. See the Log file and add the path in the Options Dialog if needed.', []);
    if not IsSilentMode then Application.MessageBox(pChar(cSomeProjectsCouldNotBeRebuilt), pChar(cError), MB_ICONERROR or MB_OK);
    ExitCode := 1;
  end;
end;

{ *-----------------------------------------------------------------------------
  Procedure: actReCompileExecute
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description: uninstall,rebuild and install a package.
-----------------------------------------------------------------------------}
procedure TDMMain.actReCompileExecute(Sender: TObject);
var
  _ProjectsToCompile: TStringList;
begin
  if FProjectFilename = '' then exit;
  _ProjectsToCompile := TStringList.Create;
  try
    _ProjectsToCompile.Add(RelativeFilename(FBPGPath, FProjectFilename, FDelphiVersion));
    CompileAndInstallProjects(_ProjectsToCompile);
  finally
    _ProjectsToCompile.Free;
  end;
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
  if not isDelphiStarted(FDelphiVersion) then exit;
  if IsSilentMode then   // if silent mode then just close it.
    ShutDownDelphi(FDelphiVersion)
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
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.actDeleteBPLExecute(Sender: TObject);
begin
  DeleteBPLAndDCPFiles;
end;
{-----------------------------------------------------------------------------
  Procedure: DeleteBPLAndDCPFiles
  Author:    sam
  Date:      10-Dez-2012
  Arguments: None
  Result:    None delete bpl and dcp file.
-----------------------------------------------------------------------------}
procedure TDMMain.DeleteBPLAndDCPFiles;
resourcestring
  cDelete_Files='Do you want to delete the file <%s>?';
var
  _bplFilename:string;
  _dcpFilename:string;
  _shallDelete:boolean;
begin
  if FProjectType<>tp_bpl then exit;
  _bplFilename:=FBPLFilename;
  _dcpFilename:=FDCPOutputPath+ChangeFileExt(ExtractFilename(_bplFilename),'.dcp');

  _shallDelete := FileExists(_bplFilename);
  if _shallDelete then begin
    if not IsSilentMode then begin
      _shallDelete:=(MessageBox(0,pchar(format(cDelete_Files,[_bplFilename])),pchar(cConfirm), MB_ICONQUESTION or MB_YESNO) = IdYes);
    end;
    if _shallDelete and uDPTDelphiPackage.DeleteFile(_bplFilename) then
      WriteLog('Deleted file <%s>.',[_bplFilename]);
  end;
  _shallDelete := FileExists(_dcpFilename);
  if _shallDelete then begin
    if not IsSilentMode then begin
      _shallDelete:=(MessageBox(0,pchar(format(cDelete_Files,[_dcpFilename])),pchar(cConfirm), MB_ICONQUESTION or MB_YESNO) = IdYes);
    end;
    if _shallDelete and uDPTDelphiPackage.DeleteFile(_dcpFilename) then
      WriteLog('Deleted file <%s>.',[_dcpFilename]);
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: CompileAndInstallCurrentPackage
  Author:    muem
  Date:      04-Dec-2012
  Arguments: None
  Result:    None
  Description: uninstall, compile and install the current package file.
-----------------------------------------------------------------------------}
function TDMMain.CompileAndInstallCurrentPackage:boolean;
begin
  result:=false;
  CheckDelphiRunning;
  UninstallCurrentPackage;
  DeleteBPLAndDCPFiles;
  if not CompileCurrentPackage then exit;
  if not FPlatformConfigCompiled then exit;
  if not InstallCurrentPackage then exit;
  WriteLog('*************************************************************************', []);
  result:=true;
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
begin
  InstallCurrentPackage;
end;

{-----------------------------------------------------------------------------
  Procedure: InstallCurrentPackage
  Author:    sam
  Date:      17-Dez-2012
  Arguments: None
  Result:    None
  Description: - uninstall package
               - then install the package again.
               - adapt delphi's environment path if needed.
-----------------------------------------------------------------------------}
function TDMMain.InstallCurrentPackage:boolean;
resourcestring
  cPathIsNotInEnv='The path <%s> not yet in your IDE Environments Path. Do you want to add it? If you do not add it, then the Delphi IDE might compilain about Packages not found when starting the IDE.';
  cCouldNotAddEnvPath='Could not add the Path <%s> to the Environments Variable. Please try to add it manually be using Windows Path Editor.';
var
  _message:string;
begin
  result:=false;
  if FProjectType<>tp_bpl then begin  // if it is not a package, then nothing to do here.
    result:=true;
    exit;
  end;
  UnInstallPackage(FProjectFilename, FProjectOutputPath,FPackageSuffix,FDelphiVersion);
  InstallPackage(FProjectFilename, FProjectOutputPath,FPackageDescription,FPackageSuffix,FDelphiVersion,IsSilentMode,_message);
  WriteLog('Installed Package <%s>.',[FProjectFilename]);
  TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).IDEInstall:=_message;
  if assigned(FOnPackageInstalledEvent) then FOnPackageInstalledEvent(self,FProjectFilename,_message,FCurrentProjectNo);
  if IsPathInIDEEnvironmentPath(FDelphiVersion,FProjectOutputPath) then exit;

  if not IsSilentMode then begin
    if Application.MessageBox(pchar(format(cPathIsNotInEnv,[FProjectOutputPath])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)<>IDYes then exit;
  end;
  if not AddIDEEnvironmentPath(FDelphiVersion,FProjectOutputPath,IsSilentMode) then begin
    if not IsSilentMode then begin
      Application.MessageBox(pchar(format(cCouldNotAddEnvPath,[FProjectOutputPath])),pchar(cInformation),MB_OK);
    end;
    exit;
  end;
  result:=true;
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
  UninstallCurrentPackage;
end;

{-----------------------------------------------------------------------------
  Procedure: UninstallCurrentPackage
  Author:    sam
  Date:      17-Dez-2012
  Arguments: None
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.UninstallCurrentPackage;
begin
  if FProjectType<>tp_bpl then exit;
  if not UninstallPackage(FProjectFilename, FProjectOutputPath,FPackageSuffix,FDelphiVersion) then exit;
  if assigned(FOnPackageUnInstalledEvent) then FOnPackageUnInstalledEvent(self,FProjectFilename,'-',FCurrentProjectNo);
  WriteLog('Uninstalled Package <%s>.',[FProjectFilename]);
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
  if ShowRemovePackagesDlg(FBPLOutputPath,_RemoveType,_DeleteBplAndDCPFiles)<>mrOk then exit;
  case _RemoveType of
    tpr_3rdparty:  begin
                      if Application.MessageBox(pchar(cDeleteAllPackagesNotInBIN),pchar(cWarning),MB_ICONWARNING or MB_YESNO)<>IDyes then exit;
                      CleanUpPackagesByRegistry(HKEY_LOCAL_MACHINE,FDelphiVersion,'Known Packages',_DeleteBplAndDCPFiles, FPlatformToCompile,FBuildMode);
                      CleanUpPackagesByRegistry(HKEY_CURRENT_USER ,FDelphiVersion,'Known Packages',_DeleteBplAndDCPFiles, FPlatformToCompile, FBuildMode);
                      CleanUpPackagesByRegistry(HKEY_LOCAL_MACHINE,FDelphiVersion,'Disabled Packages',_DeleteBplAndDCPFiles, FPlatformToCompile, FBuildMode);
                      CleanUpPackagesByRegistry(HKEY_CURRENT_USER ,FDelphiVersion,'Disabled Packages',_DeleteBplAndDCPFiles, FPlatformToCompile, FBuildMode);
                   end;
    tpr_projectsbpl:begin
                      if Application.MessageBox(pchar(cDeleteAllPackagesInBPL),pchar(cWarning),MB_ICONWARNING or MB_YESNO)<>IDyes then exit;
                      CleanUpPackagesByPath(FDelphiVersion,FBPLOutputPath,FDCPOutputPath,_DeleteBplAndDCPFiles, FPlatformToCompile, FBuildMode);
                    end;
  end;
  CleanUpPackageByEnvPaths(FDelphiVersion,IsSilentMode);
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
    for i := 0 to FBPGProjectList.Count - 1 do begin
      SetCurrentProject(FBPGProjectList[i]);
      if FProjectFilename = '' then continue;
      FireCurrentProjectChanged;
      InstallCurrentPackage;
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
begin
  ReCompileAndInstallAll;
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
    for i := 0 to FBPGProjectList.Count - 1 do begin
      SetCurrentProject(FBPGProjectList[i]);
      if FProjectFilename = '' then continue;
      FireCurrentProjectChanged;
      UninstallCurrentPackage;
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
  ShutDownDelphi(FDelphiVersion);
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
  if not isDelphiStarted(FDelphiVersion) then StartUpDelphi(FDelphiVersion,'');
end;

{-----------------------------------------------------------------------------
  Procedure: CompileCurrentPackageWithDcc
  Author:    sam
  Date:      09-Jan-2013
  Arguments: -
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function TDMMain.CompileCurrentPackageWithDcc: Boolean;
var
  _CompilerSwitches: string;
  _Output: string;
  _SearchPath:string;
  _Conditionals:string;
begin
  if SameText(FPlatformToCompile, sWin32) then
    FDelphiCompilerFile := FDelphiRootDirectory + sWin32Compiler
  else if SameText(FPlatformToCompile, sWin64) then
    FDelphiCompilerFile := FDelphiRootDirectory + sWin64Compiler
  else
    FDelphiCompilerFile := FDelphiRootDirectory + sWin32Compiler;
  trace(3, 'Set the Compiler File to <%s>.', [FDelphiCompilerFile]);

  FPlatformConfigCompiled := False;
  // prepare search path
  _CompilerSwitches := ProjectSettings.StringValue('Application/CompilerSwitches');
  if _CompilerSwitches = '' then _CompilerSwitches := ApplicationSettings.StringValue('Application/CompilerSwitches');
  if FCompilerSwitches <> '' then begin
    _CompilerSwitches := _CompilerSwitches + ' ' + FCompilerSwitches;
  end;
  _SearchPath := '';
  if FDPTSearchPath  <> ''    then _SearchPath := GetGlobalSearchPath; // the search path settings defined in DPT Options-Dialog.
  if FProjectSearchPath <> '' then begin // the search path settings defined in the .cfg/.dproj file of the current project.
    _SearchPath := _SearchPath + MakeAbsolutePath(ExtractfilePath(FProjectFilename), FProjectSearchPath, FDelphiVersion, FPlatformToCompile, FBuildMode);
  end;
  if FConditions <> '' then _CompilerSwitches := _CompilerSwitches + ' ' + FConditions + ' ';

  if FDelphiVersion >= 12 then begin   // Delphi 2009
    if ((lowercase(ExtractFileExt(FProjectFilename))='.dpk') or
        (lowercase(ExtractFileExt(FProjectFilename))='.dpr')) then begin
      if SameText(FBuildMode, sRelease) then _Conditionals:=ProjectSettings.StringValue('Application/ReleaseCompilerSwitches') else
      if SameText (FBuildMode, sDebug) then _Conditionals:=ProjectSettings.StringValue('Application/DebugCompilerSwitches')
      else _Conditionals:='';
      if _Conditionals <> '' then _CompilerSwitches := _CompilerSwitches + ' ' + _Conditionals;
    end;
  end;

  if _SearchPath <> '' then begin
    _SearchPath := RemoveDoublePathEntries(_SearchPath);
    _SearchPath := ReplaceTag(_SearchPath);
    _SearchPath := '"' + _SearchPath + '"';
    _CompilerSwitches := _CompilerSwitches + ' ' + '-U' + _SearchPath;
    _CompilerSwitches := _CompilerSwitches + ' ' + '-O' + _SearchPath;
    _CompilerSwitches := _CompilerSwitches + ' ' + '-I' + _SearchPath;
    _CompilerSwitches := _CompilerSwitches + ' ' + '-R' + _SearchPath;
  end;

  if (FDelphiVersion >= 11) and (FNameSpaces = '')  then begin  // Delphi 2007
    FNameSpaces := 'Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win';
  end;

  trace(5, 'CompilePackage: Compiler switch --> %s.', [_CompilerSwitches]);
  trace(5, 'Length= %d.', [length(_CompilerSwitches)]);
  WriteLog('Compiling Project <%s>. Please wait...', [FProjectFilename]);
  WriteLog('  Platform: %s / Config: %s', [FPlatformToCompile, FBuildMode]);
  WriteLog('  Output Target <%s>.',[FProjectOutputPath + OutputFileName(FProjectFilename, FProjectType)]);
  Screen.Cursor := crHourGlass;
  FPlatformConfigCompiled := CompileProject(FDelphiCompilerFile,
                                            _CompilerSwitches,
                                            ReadProjectFilenameFromDProj(FProjectFilename),
                                            ReplaceTag(FProjectOutputPath),
                                            ReplaceTag(FDCUOutputPath),
                                            ReplaceTag(FDCPOutputPath),
                                            ExtractFilePath(ReadProjectFilenameFromDProj(FProjectFilename)),
                                            FNameSpaces,
                                            FProjectType,
                                            IsSilentMode,
                                            _Output,
                                            FDelphiVersion);
  FProjectCompiled := FProjectCompiled and FPlatformConfigCompiled;
  if FPlatformConfigCompiled then begin
    TProjectData(FBPGProjectList.Objects[FCurrentProjectNo]).CompileResultsList.Add(FPlatformToCompile + '/' + FBuildMode + ProjectDataDelimiter + 'Compiled ok!');
    TProjectData(FBPGProjectList.Objects[FCurrentProjectNo]).VersionsList.Add(FPlatformToCompile + '/' + FBuildMode + ProjectDataDelimiter + GetCurrentPackageVersion);
    trace(2,'Successfully compiled Project <%s>.', [FProjectFilename]);
  end
  else begin
    TProjectData(FBPGProjectList.Objects[FCurrentProjectNo]).CompileResultsList.Add(FPlatformToCompile + '/' + FBuildMode + ProjectDataDelimiter + 'Failed!');
    TProjectData(FBPGProjectList.Objects[FCurrentProjectNo]).VersionsList.Add(FPlatformToCompile + '/' + FBuildMode + ProjectDataDelimiter + 'Failed');
    if (not IsSilentMode) and
       (ApplicationSettings.BoolValue('Application/AutomaticSearchFiles')) then SearchFileCompilerOutput(_Output);
  end;
  Screen.Cursor := crDefault;
  WriteLog(_Output,[]);
  if ((Pos('fatal', LowerCase(_Output)) > 0) or
     ((Pos(pchar(cWarning), LowerCase(_Output)) > 0))) then begin
    if not IsSilentMode then begin
      if Length(_Output) > 2000 then _Output := Copy(_Output, Length(_Output) - 2000, 2000);
      Application.MessageBox(pchar(_Output), pchar(cInformation), MB_ICONINFORMATION or MB_OK);
    end;
    trace(3,'There are problems/warnings in project <%s>. Please see log-file.', [FProjectFilename]);
  end;
  Result := FPlatformConfigCompiled;
end;

{-----------------------------------------------------------------------------
  Procedure: CompileCurrentPackageWithMsBuild
  Author:    muem/sam
  Date:      10-Jan-2013
  Arguments: -
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function TDMMain.CompileCurrentPackageWithMsBuild: Boolean;
var
  _envVariables: TStringList;
  _delphiEnvVariables: TStringList;
  i: Integer;
  _index, _offset: Integer;
  _tmpString: string;
  _frameworkDir: string;
  _environmentBlock: string;
  _commandLine: string;
  _returnValue: Cardinal;
  _Output: string;
  _SearchPath: string;
  _OutputPath: string;
begin
  FPlatformConfigCompiled := False;

  //Update environment block with RAD Studio variables
  _envVariables := TStringList.Create;
  _delphiEnvVariables := TStringList.Create;
  try
    GetEnvironmentVars(_envVariables);
    _delphiEnvVariables.LoadFromFile(FDelphiRootDirectory + sRadStudioVars);
    for i := _delphiEnvVariables.Count - 1 downto 0 do begin
      if Pos('@', _delphiEnvVariables[i]) = 1 then begin
        //Echo off found
        _tmpString := _delphiEnvVariables[i];
        Delete(_tmpString, 1, 1);
        _delphiEnvVariables[i] := TrimLeft(_tmpString);
      end;
      if (Pos('SET ', _delphiEnvVariables[i]) = 1) then begin
        //Environment variable found
        _tmpString := _delphiEnvVariables[i];
        Delete(_tmpString, 1, 4);
        _delphiEnvVariables[i] := TrimLeft(_tmpString);
      end
      else begin
        _delphiEnvVariables.Delete(i);
      end;
    end;
    _frameworkDir := _delphiEnvVariables.Values['FrameworkDir'];
    for i := 0 to _delphiEnvVariables.Count-1 do begin //Expand if possible
      _index := Pos('%', _delphiEnvVariables.ValueFromIndex[i]);
      while _index > 0 do begin
        _offset := PosEx('%', _delphiEnvVariables.ValueFromIndex[i], _index + 1);
        _tmpString := Copy(_delphiEnvVariables.ValueFromIndex[i], _index + 1, _offset - _index - 1);
        _delphiEnvVariables[i] := StringReplace(_delphiEnvVariables[i], '%'+_tmpString+'%', _envVariables.Values[_tmpString], [rfReplaceAll, rfIgnoreCase]);
        _index := Pos('%', _delphiEnvVariables.ValueFromIndex[i]);
      end;

      _index := _envVariables.IndexOfName(_delphiEnvVariables.Names[i]);
      if _index < 0 then _envVariables.Add(_delphiEnvVariables[i]) //Variable not found, add variable
                    else _envVariables.ValueFromIndex[_index] := _delphiEnvVariables.ValueFromIndex[i]; //Variable found, overwrite value
    end;
    _envVariables.Sort;
    _environmentBlock := '';
    for i := 0 to _envVariables.Count - 1 do begin
      if _environmentBlock = '' then begin
        _environmentBlock := _envVariables[i];
      end
      else begin
        _environmentBlock := _environmentBlock + #0 + _envVariables[i];
      end;
    end;
    _environmentBlock := _environmentBlock + #0#0;
  finally
    _delphiEnvVariables.Free;
    _envVariables.Free;
  end;
  FDelphiCompilerFile := IncludeTrailingPathDelimiter(_frameworkDir) + sMSBuild;
  trace(3, 'Set the Compiler File to <%s>.', [FDelphiCompilerFile]);

  _SearchPath := '';
  _OutputPath := '';
  if FDPTSearchPath  <> '' then _SearchPath := GetGlobalSearchPath; // the search path settings defined in DPT Options-Dialog.
  if _SearchPath <> '' then begin // global search path is defined. It will override project specific search paths, so add project specific search path
    if FProjectSearchPath <> '' then  _SearchPath := _SearchPath + MakeAbsolutePath(ExtractfilePath(FProjectFilename), FProjectSearchPath, FDelphiVersion, FPlatformToCompile, FBuildMode); // the search path settings defined in the .cfg/.dproj file of the current project.
    _SearchPath := RemoveDoublePathEntries(_SearchPath);
    _SearchPath := ReplaceTag(_SearchPath);
    _SearchPath := '"' + _SearchPath + '"';
  end;

  if FProjectOutputPath <> '' then begin // then take it from the dpt
    _OutputPath := _OutputPath + ';DCC_ExeOutput=' + ReplaceTag(FProjectOutputPath) +
                                 ';DCC_BplOutput=' + ReplaceTag(FProjectOutputPath);
  end;
  if FDCPOutputPath <> '' then _OutputPath := _OutputPath + ';DCC_DcpOutput=' + ReplaceTag(FDCPOutputPath); // then take it from the dpt
  if FDCUOutputPath <> '' then _OutputPath := _OutputPath + ';DCC_DcuOutput=' + ReplaceTag(FDCUOutputPath); // then take it from the dpt

  WriteLog('Compiling Project <%s>. Please wait...', [FProjectFilename]);
  WriteLog('  Platform: %s / Config: %s', [FPlatformToCompile, FBuildMode]);
  WriteLog('  Output Target <%s>.',[FProjectOutputPath + OutputFileName(FProjectFilename, FProjectType)]);


  _commandLine := '"' + FProjectFilename + '" /t:build' +
                                           ' /p:config="' + FBuildMode +'"'+
                                           ' /p:platform="' + FPlatformToCompile+'"';
   if SameText(FBuildMode,'release') then _commandLine :=_commandLine+' /p:target="Deploy"';

  if _SearchPath <> '' then begin
    _commandLine := _commandLine + ' /p:UnitSearchPath=' + _SearchPath +
                                   ' /p:ObjectPath=' + _SearchPath +
                                   ' /p:IncludePath=' + _SearchPath +
                                   ' /p:ResourcePath=' + _SearchPath;
  end;

  if _OutputPath <> '' then begin
    if Pos(';', _OutputPath) = 1 then Delete(_OutputPath, 1, 1);
    _commandLine := _commandLine + ' "/p:' + _OutputPath + '"';
  end;

  Screen.Cursor := crHourGlass;
  try
    trace(3, 'Command line is %s.', [_commandLine]);
    _returnValue := WinExecAndWait(FDelphiCompilerFile,
                                   _commandLine,
                                   ExtractFilePath(FProjectFilename),
                                   _environmentBlock,
                                   SW_HIDE,
                                   _Output);

  finally
    Screen.Cursor := crDefault;
  end;

  if _returnValue = 0 then begin
    trace(5, 'CompileProject: Successfully build Project file <%s>.', [ReadProjectFilenameFromDProj(FProjectFilename)]);
    FPlatformConfigCompiled := True;
  end;

  FProjectCompiled := FProjectCompiled and FPlatformConfigCompiled;
  if FPlatformConfigCompiled then begin
    TProjectData(FBPGProjectList.Objects[FCurrentProjectNo]).CompileResultsList.Add(FPlatformToCompile + '/' + FBuildMode + ProjectDataDelimiter + 'Compiled ok!');
    TProjectData(FBPGProjectList.Objects[FCurrentProjectNo]).VersionsList.Add(FPlatformToCompile + '/' + FBuildMode + ProjectDataDelimiter + GetCurrentPackageVersion);
    trace(2,'Successfully compiled Project <%s>.', [FProjectFilename]);
  end
  else begin
    TProjectData(FBPGProjectList.Objects[FCurrentProjectNo]).CompileResultsList.Add(FPlatformToCompile + '/' + FBuildMode + ProjectDataDelimiter + 'Failed!');
    TProjectData(FBPGProjectList.Objects[FCurrentProjectNo]).VersionsList.Add(FPlatformToCompile + '/' + FBuildMode + ProjectDataDelimiter + 'Failed');
    if (not IsSilentMode) and (ApplicationSettings.BoolValue('Application/AutomaticSearchFiles')) then SearchFileCompilerOutput(_Output);
  end;

  WriteLog(_Output,[]);
  if ((Pos('fatal', LowerCase(_Output)) > 0) or
     ((Pos(pchar(cWarning), LowerCase(_Output)) > 0))) then begin
    if not IsSilentMode then begin
      if Length(_Output) > 2000 then _Output := Copy(_Output, Length(_Output) - 2000, 2000);
      Application.MessageBox(pchar(_Output), pchar(cInformation), MB_ICONINFORMATION or MB_OK);
    end;
    trace(3,'There are problems/warnings in project <%s>. Please see log-file.', [FProjectFilename]);
  end;
  Result := FPlatformConfigCompiled;
end;

{-----------------------------------------------------------------------------
  Procedure: CompileCurrentPackage
  Author:    sam
  Date:      29-Mai-2008
  Arguments: -
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function TDMMain.CompileCurrentPackage: Boolean;
begin
  if FUseMSBuild then result:=CompileCurrentPackageWithMsBuild
                 else result:=CompileCurrentPackageWithDcc;
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
  ShowBPLSearchDialog(ApplicationSettings.StringValue('Application/LastUsedSearchPath'),ExtractFilenameOnly(FProjectFilename));
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
  if assigned(FOnCurrentProjectChanged) then FOnCurrentProjectChanged(self,FProjectFilename,FCurrentProjectNo);
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
     (Pos('not found',_compilerOutput)>0) then ShowSelectPathDialog(SearchPath,_filename,true) else
  if (Pos('.dpr',_filename)>0) or
     (Pos('.dpk',_filename)>0) or
     (Pos('.inc',_filename)>0) or
     (Pos('.pas',_filename)>0) then begin
    if Pos('<',_filename)=1 then Delete(_filename,1,1);
    if Pos('>.',_filename)=length(_filename)-1 then Delete(_filename,length(_filename)-1,2);
    _filename:=AbsoluteFilename(FBPGPath,_filename);
    ShowFile(_filename,0);
  end;
end;


procedure TDMMain.AbortCompile;
begin
  FAbortCompileUser:=true;
end;

procedure TDMMain.SetApplicationState(const _newState: TApplicationState);
begin
  case _NewState of
    tas_init:begin
      actUninstallPackage.Enabled := False;
      actInstallPackage.Enabled := False;
      actUninstallAllPackages.Enabled := False;
      actInstallAllPackages.Enabled := False;
      actCompileAllPackages.Enabled := False;
      actReCompile.Enabled := False;
      actDeleteBPL.Enabled := False;
      actRecompileAllPackages.Enabled := False;
      actDeleteFiles.Enabled := False;
    end;
    tas_open:begin
      actUninstallPackage.Enabled := True;
      actInstallPackage.Enabled := True;
      actUninstallAllPackages.Enabled := True;
      actInstallAllPackages.Enabled := True;
      actCompileAllPackages.Enabled := True;
      actReCompile.Enabled := True;
      actDeleteBPL.Enabled := True;
      actRecompileAllPackages.Enabled := True;
      actDeleteFiles.Enabled := True;
    end;
  end;
  if Assigned(FOnApplicationStateEvent) then FOnApplicationStateEvent(Self, FApplicationState, _newState);
  FApplicationState := _newState;
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
    for i := 0 to FBPGProjectList.Count - 1 do begin
      SetCurrentProject(FBPGProjectList[i]);
      if FProjectFilename = '' then continue;
      FireCurrentProjectChanged;
      actDeleteBPLExecute(nil);
    end;
  finally
    ApplicationState:=tas_open;
  end;
end;

function TDMMain.RemoveProjectFromProjectGroup:boolean;
begin
  result:=uDPTDelphiPackage.RemoveProjectFromProjectGroup(FBPGFilename,FProjectFilename,FProjectType);
end;

{-----------------------------------------------------------------------------
  Procedure: ReplaceTag
  Author:    Muem
  Date:      06-Dec-2012
  Arguments: _filename: string
  Result:    string
  Description: replaces the Tag <$(DELPHI)> with the real delphi path.
               replaces the Tag <$(BDS)> with the real delphi path.
               replaces the Tag <$(PROGRAMFILES)> with the real program files path.
               replaces  the Tag <$(DELPHIVERSION)> with the real delphi version.
               replaces  the Tag <$(BDSCOMMONDIR)> with the real bds common path.
               replaces  the Tag <$(BDSPROJECTSDIR)> with the real bds projects path.
               replaces  the Tag <$(PLATFORM)> with the current platform.
               replaces  the Tag <$(CONFIG)> with the current config.
----------------------------------------------------------------------------}
function TDMMain.ReplaceTag(_filename: string): string;
begin
  result := uDPTPathFilenameConvert.ReplaceTag(_filename, FDelphiVersion,FPlatformToCompile, FBuildMode);
end;

{*-----------------------------------------------------------------------------
  Procedure: PrepareEXEParams
  Author:    sam
  Date:
  Arguments: _filename: string; _lineNo: integer;_SourceCodeEditorParams: string
  Result:    string
  Description:
-----------------------------------------------------------------------------}
function TDMMain.PrepareEXEParams(_filename: string; _lineNo: integer;_SourceCodeEditorParams: string): string;
begin
  result:='';
  _SourceCodeEditorParams:=StringReplace(_SourceCodeEditorParams, '%FILENAME%', '"'+_filename+'"',[]);
  _SourceCodeEditorParams:=StringReplace(_SourceCodeEditorParams, '%LINENO%', inttostr(_lineNo),[]);
  result:=_SourceCodeEditorParams;
end;

{-----------------------------------------------------------------------------
  Procedure: PrepareConditions
  Author:    herzogs2
  Date:      13-Nov-2017
  Arguments: None
  Result:    string
  Description: this method gets the conditional defines from the projects
               cfg/dproj-file and will add/replace the word's DEBUG/RELEASE
               depending on the setting of field "buildmode" in dpt User-Interface.
-----------------------------------------------------------------------------}
function TDMMain.PrepareConditions:string;
var
_pos:integer;
//_BuildMode:string;   // build mode as defined from dpt.
_ProjectConditions:string; // conditional defines as read from cfg/dproj
_Condition:string;
_Conditions:TStringList;
begin
  result:='';
  _Conditions:=TStringList.Create;
  _Conditions.Delimiter:=';';
  try
    _ProjectConditions:=uppercase(FConditions);
    _pos:=Pos('-D',_ProjectConditions);
    if _pos>0 then Delete(_ProjectConditions,1,2);
    while _ProjectConditions<>'' do begin
      _Condition:=trim(GetField(';',_ProjectConditions));
      if _Condition='' then continue;
      if _Condition=FBuildMode then continue;
      if (FBuildMode='DEBUG') and     // they exclude each other
         (_Condition='RELEASE') then continue;
      if (FBuildMode='RELEASE') and   // they exclude each other
         (_Condition='DEBUG') then continue;
      if _Conditions.IndexOf(_Condition)=-1 then _Conditions.Add(_Condition);
    end;
    result:='-D'+FBuildMode+';'+_Conditions.DelimitedText;
  finally
    _Conditions.Free;
  end;
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
  if FDelphiVersion<=7 then begin
    while pos('BDS',FProjectSearchPath)>0 do begin
      FProjectSearchPath:=StringReplace(FProjectSearchPath,'BDS','DELPHI',[]);
      _changed:=true;
    end;
  end
  else begin
    while pos('DELPHI',FProjectSearchPath)>0 do begin
      FProjectSearchPath:=StringReplace(FProjectSearchPath,'DELPHI','BDS',[]);
      _changed:=true;
    end;
  end;
  if not _changed then exit;
  _file:=TStringList.Create;
  try
    _temp:=FProjectSearchPath;
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
  if not ProjectSettings.BoolValue('Application/AutoBackup') then exit;
  _filename:=changefileExt(ExtractFilenameOnly(BPGFilename)+'_'+BuildTimeStamp(now),'.zip');
  if ProjectSettings.PathValue('Application/LastUsedBackupPath')='' then _path:=extractfilepath(BPGFilename)+'backup\'
                                                                    else _path:=ProjectSettings.PathValue('Application/LastUsedBackupPath');
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
procedure TDMMain.UpdateProjectFiles(const _ForceWrite: Boolean = False);
var
i:integer;
_ChangedFiles:TStringList;
_NewFilename:string;
_DProjFilename:string;
begin
  if assigned(CommandLineAction) then exit;    // the app runs with command-line params, then do not change files.
  if not _ForceWrite then begin
    if not ProjectSettings.BoolValue('Application/ChangeFiles') then exit;    // if DPT is allowed to change the files
  end;

// try to update cfg/dproj/bdsproj files.
  WriteSettingsToDelphi(FBPGPath,   // prepare the new delphi settings files (cfg,dproj,bdsproj,dof).
                               FConfigFilename,
                               FConditions,
                               GetGlobalSearchPath,
                               FProjectOutputPath,
                               FBPLOutputPath,
                               FDCUOutputPath,
                               FDCPOutputPath,
                               IsSilentMode,
                               FProjectType,
                               FDelphiVersion,
                               FPlatformToCompile,
                               FBuildMode,
                               _ChangedFiles);

  for i:=0 to _ChangedFiles.count-1 do ConfirmChanges(_ChangedFiles[i], False,_ForceWrite);

// try to update dpk/dproj files.
  if FProjectType = tp_bpl then begin // it is a package
    if lowercase(ExtractFileExt(FProjectFilename))='.dpk' then begin
      WriteDPKFile(FDelphiVersion,
                   FProjectFilename,
                   FPackageSuffix,
                   IsSilentMode,
                   _NewFilename); // then prepare a new file.
      ConfirmChanges(_NewFilename,False,_ForceWrite);
    end;


    if FDelphiVersion>=11 then begin
      _DProjFilename:=ChangeFileExt(FProjectFilename,'.dproj');
      if fileexists(_DProjFilename) then WriteDprojFile(_DProjFilename,FPackageSuffix,IsSilentMode,_NewFilename);
      ConfirmChanges(_NewFilename,False,_ForceWrite);
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: ConfirmChanges
  Author:    sam
  Date:      07-Mai-2010
  Arguments: _ChangedFiles:string
  Result:    None
  Description: present the changed file in the diff-tool and ask the user
               if he want's to save the changes.
               If <_ForceWrite> is set to <true>, then ignore the setting <'Application/ChangeFiles'>.
-----------------------------------------------------------------------------}
procedure TDMMain.ConfirmChanges(_ChangedFile:string;const _Revert:Boolean;const _ForceWrite:boolean);
resourcestring
cSaveChanges='Save changes to file <%s> ?';
cDPTSuggestsSomeChanges='DelphiPackageTool suggest''s some changes. Do you want to review them in the Diff-Tool ?';
cDPTFoundOldVersionOfFile='DelphiPackageTool could undo the latest changes. Do you want to review them in the Diff-Tool ?';
var
_OldFilename:string;
_msg:string;
_answer:integer;
begin
  _answer:=0;
  if _ChangedFile='' then exit;
  if not fileexists(_ChangedFile) then exit;
  if not _ForceWrite then begin
    if not ProjectSettings.BoolValue('Application/ChangeFiles') then exit;
  end;
  if assigned(CommandLineAction) then exit;    // the app runs with command-line params, then do not change files.
  _OldFilename:=copy(_ChangedFile,1,length(_ChangedFile)-4);

  if _revert then _msg:=cDPTFoundOldVersionOfFile
             else _msg:=cDPTSuggestsSomeChanges;

  if not IsSilentMode and
     ApplicationSettings.BoolValue('Application/DisplayFilesInDiffTool') and
     IsDiffToolAvailable  then begin
    _answer:=Application.MessageBox(pchar(_msg),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO);

    if _answer=ID_Yes then begin
      if _Revert then CompareFiles(_ChangedFile,_OldFilename)
                 else CompareFiles(_OldFilename,_ChangedFile);
    end;
    if Application.MessageBox(pchar(format(cSaveChanges,[_OldFilename])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)<>IDYes then exit;
  end;
  if not RemoveReadOnlyFlag(_OldFilename,IsSilentMode) then exit;
  if not CopyFile(pchar(_ChangedFile),pchar(_OldFilename),false) then WriteLog('Problem to change file <%s>.',[_OldFilename])
                                                                 else WriteLog('Changed the file ''%s''',[_OldFilename]);
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
  Author:    sam
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
    _Filename:=ChangeFileExt(FConfigFilename,_FileExt);
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
function TDMMain.SetProjectVersionOfFile(_filename:string;_Major,_Minor,_Release,_Build:integer): boolean;
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
  NVBAppExec1.ExeParams:=format('-v%d.%d.%d.%d -s "%s"',[_Major,_Minor,_Release,_Build,_filename]);
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
  Procedure:   ElaboratePlatformsAndConfigsToCompileList
  Author:      muem
  Date:        20-Nov-2012
  Arguments:   -
  Result:      -
  Description: Elaborate FPlatformsToCompileList and FConfigsToCompileList based
               on the selected platforms and configs
-----------------------------------------------------------------------------}
procedure TDMMain.ElaboratePlatformsAndConfigsToCompileList;
begin
  FPlatformsToCompileList.Clear;
  FConfigsToCompileList.Clear;

  if FCurrentBPGPlatformList.Count = 0 then FPlatformsToCompileList.Add('') // use default platform
                                       else FPlatformsToCompileList.AddStrings(FCurrentBPGPlatformList);

  if FCurrentBPGConfigList.Count = 0 then FConfigsToCompileList.Add('') // use default platform
                                     else FConfigsToCompileList.AddStrings(FCurrentBPGConfigList);
end;

{*-----------------------------------------------------------------------------
  Procedure:   CompileAndInstallProjects
  Author:      muem
  Date:        20-Nov-2012
  Arguments:   ProjectList: TStringList
  Result:      Integer
  Description: compile and install the selected platforms and configs
-----------------------------------------------------------------------------}
function TDMMain.CompileAndInstallProjects(ProjectsList: TStringList): Integer;
resourcestring
cCompiledOK='Compiled ok!';
cCompileNOTOK='Failed';
var
  _ProjectIndex: Integer;
  _PlatformIndex: Integer;
//  _ConfigIndex: Integer;
  _TmpStr: string;
  _DelimiterPos: Integer;
begin
  Result := 0;
  for _ProjectIndex := 0 to ProjectsList.Count - 1 do begin
    SetCurrentProject(ProjectsList[_ProjectIndex]);
    ElaboratePlatformsAndConfigsToCompileList;
    TProjectData(FBPGProjectList.Objects[FCurrentProjectNo]).CompileResultsList.Clear;
    TProjectData(FBPGProjectList.Objects[FCurrentProjectNo]).VersionsList.Clear;
    if assigned(FOnCurrentProjectCompileStateChanged) then FOnCurrentProjectCompileStateChanged(self, FProjectFilename, 'Compiling...', DateTimeToStr(Now), '', FCurrentProjectNo, FPackageDescription);

    FProjectCompiled := true;
    for _PlatformIndex := 0 to FPlatformsToCompileList.Count - 1 do begin
      FPlatformToCompile :=FPlatformsToCompileList[_PlatformIndex];
      FBPLOutputPath     :=GetDelphiPackageDir(FDelphiVersion,FPlatformToCompile);
      FireCurrentProjectChanged;
      CompileAndInstallCurrentPackage;

      if (not FPlatformConfigCompiled) and
         ApplicationSettings.BoolValue('Application/StopOnFailure') then begin
        WriteLog('Aborted by Program because of Compile Problem and setting "Stop on Failure" is <ON>.', []);
        FAbortCompileFailure:=true;
        Break;
      end;

      if FAbortCompileUser then begin
        WriteLog('Aborted by User.', []);
        Break;
      end;

      Application.ProcessMessages;
      if Application.Terminated then break;
    end;

    if FProjectCompiled then begin
      Inc(Result);
      if TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).VersionsList.Count = 1 then begin
        _TmpStr := TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).VersionsList[0];
        _DelimiterPos := Pos(ProjectDataDelimiter, _TmpStr);
        _TmpStr := Copy(_TmpStr, _DelimiterPos + Length(ProjectDataDelimiter), Length(_TmpStr));
      end
      else _TmpStr := 'See Hint';
      TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).CompileDate:=DateTimeToStr(Now);
      TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).CompileState:=cCompiledOK;
      TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).Version:=_TmpStr;
      TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).Description:=FPackageDescription;
    end
    else begin
      TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).CompileDate:='';
      TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).CompileState:=cCompileNOTOK;
      TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).Version:='';
      TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).Description:=FPackageDescription;
    end;
    if Assigned(FOnCurrentProjectCompileStateChanged) then
      FOnCurrentProjectCompileStateChanged(Self,
                                           FProjectFilename,
                                           TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).CompileState,
                                           TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).CompileDate,
                                           TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).Version,
                                           FCurrentProjectNo,
                                           TProjectData(BPGProjectList.Objects[FCurrentProjectNo]).Description);
    Application.ProcessMessages;
    if Application.Terminated then break;
    if FAbortCompileFailure or FAbortCompileUser then break;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: InitProjectDataForHint
  Author:    muem
  Date:      29-Nov-2012
  Arguments: -
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.InitProjectDataForHint;
var
  i: Integer;
begin
  for i := FBPGProjectList.Count - 1 downto 0 do begin
    SetCurrentProject(FBPGProjectList[i]);
    TProjectData(FBPGProjectList.Objects[i]).OutputFilename := FProjectOutputFilename;
    TProjectData(FBPGProjectList.Objects[i]).OutputPath     := FProjectOutputPath;
    TProjectData(FBPGProjectList.Objects[i]).BPLOutputPath  := FBPLOutputPath;
    TProjectData(FBPGProjectList.Objects[i]).DCPOutputPath  := FDCPOutputPath;
    TProjectData(FBPGProjectList.Objects[i]).DCUOutputPath  := FDCUOutputPath;
    TProjectData(FBPGProjectList.Objects[i]).ProjectSearchPath := FProjectSearchPath;
    TProjectData(FBPGProjectList.Objects[i]).Description    := FPackageDescription;
    TProjectData(FBPGProjectList.Objects[i]).DPTSearchPath  := FDPTSearchPath;
    TProjectData(FBPGProjectList.Objects[i]).ProjectType    := FProjectType;
    TProjectData(FBPGProjectList.Objects[i]).UseMSBuild     := FUseMSBuild;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: SetCurrentProject
  Author:    muem
  Date:      27-Nov-2012
  Arguments: const _ProjectName: string;
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.SetCurrentProject(const _ProjectName: string);
resourcestring
  cCouldNotFindProjectFile = 'Could not find Project File <%s>. Please check if the file <%s> is still correct.';
begin
  FProjectFilename:='';
  FConfigFilename := '';
  FBuildMode:='';
  FPlatformToCompile := '';
  FCompilerSwitches := '';
  FConditions := '';
  FProjectSearchPath := '';
  FBPLFilename := '';
  FProjectOutputPath := '';
  FBPLOutputPath := '';
  FDCUOutputPath := '';
  FDCPOutputPath := '';
  FNameSpaces := '';
  FUseMSBuild:=false;
  FCurrentProjectNo:=0;

  if _ProjectName = '' then exit;
  FProjectFilename := AbsoluteFilename(FBPGPath, _ProjectName);
  if not FileExists(FProjectFilename) then begin
    Application.MessageBox(pchar(Format(cCouldNotFindProjectFile, [FProjectFilename, FBPGFilename])), pchar(cWarning), MB_ICONWARNING or MB_OK);
    trace(1, 'Problem in TDMMain.SetCurrentProject: The Project Filename <%s> is not valid. Could not find the file. Please check the file <%s> and path names.', [FProjectFilename, FBPGFilename]);
    exit;
  end;
  FCurrentProjectNo:=FBPGProjectList.IndexOf(_ProjectName);
  // check if it is a package or not

  LoadCurrentProject;

  actRevertChanges.Enabled := OldFilesExist(cModifiedFileExtentions);
  case FProjectType of
    tp_bpl: begin
        actInstallPackage.Enabled := True;
        actUninstallPackage.Enabled := True;
        actExecuteApp.Enabled := False;
        actFindDCPandBPL.Enabled := True;
        actDeleteBPL.Enabled := True;
    end;
    tp_exe: begin
        actInstallPackage.Enabled := False;
        actUninstallPackage.Enabled := False;
        actDeleteBPL.Enabled := False;
        actExecuteApp.Enabled := True;
        actFindDCPandBPL.Enabled := False;
    end;
    tp_dll: begin
        actInstallPackage.Enabled := False;
        actUninstallPackage.Enabled := False;
        actExecuteApp.Enabled := False;
        actDeleteBPL.Enabled := False;
        actFindDCPandBPL.Enabled := False;
    end;
  end;
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
  result:=GetPackageVersion(FProjectFilename,ReplaceTag(FProjectOutputPath),FPackageSuffix,FProjectType);
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
        if IsSilentMode then exit;
        Application.MessageBox(pChar(format(cNoVersionInfo,[_filename])),pchar(cInformation),MB_ICONWARNING or MB_OK);
        exit;
      end;
      if not ShowVersionDlg(_filename,_Major,_Minor,_Release,_Build,ShowVersionDialog) then exit;
    end;
    result:=SetProjectVersionOfFile(_filename,_Major,_Minor,_Release,_Build);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: GetAllPlatformsAndConfigsOfBPG
  Author:    muem
  Date:      24-Oct-2012
  Arguments: none
  Result:    none
  Description: Gets all platforms and configs of a project group
-----------------------------------------------------------------------------}
procedure TDMMain.GetAllPlatformsAndConfigsOfBPG;
var
  i: Integer;
  _TmpPlatformList: TStringList;
  _TmpConfigList: TStringList;
begin
  FBPGPlatformList.Clear;
  FBPGConfigList.Clear;
  _TmpPlatformList := TStringList.Create;
  _TmpConfigList := TStringList.Create;
  try
    for i := 0 to FBPGProjectList.Count - 1 do begin
      if FDelphiVersion > 12 then begin  // Delphi 2009
        ReadSupportedConfigsOfProject(AbsoluteFilename(FBPGPath,FBPGProjectList[i]), _TmpConfigList);
        FBPGConfigList.AddStrings(_TmpConfigList);
      end;
      if FDelphiVersion >= 16 then begin // Delphi XE2 and higher
        ReadSupportedPlatformsOfProject(AbsoluteFilename(FBPGPath,FBPGProjectList[i]), _TmpPlatformList);
        FBPGPlatformList.AddStrings(_TmpPlatformList);
      end;
    end;
  finally
    _TmpConfigList.Free;
    _TmpPlatformList.Free;
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
function TDMMain.GetProjectVersionOfFile(_filename: string; out Major,Minor, Release, Build: integer): boolean;
const
cResTag='.res:';
var
_returnValue:cardinal;
_Output:string;
_Pos:integer;
_sMajor,_sMinor,_sRelease,_sBuild:string;
begin
  result:=false;
  Major:=0;
  Minor:=0;
  Release:=0;
  Build:=0;

  if not fileexists(changefileext(_filename,'.res')) then begin
    trace(2,'Problem in TDMMain.GetProjectVersionOfFile: File <%s> not found.',[changefileext(_filename,'.res')]);
    exit;
  end;
  _returnValue:=WinExecAndWait(extractfilepath(application.exename)+cSetVersionApplicationName,
                     '-p -s "'+_filename+'"',
                     extractfilepath(application.exename),
                     '',
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
  ConfirmChanges(cModifiedFileExtentions,true,true);
end;

{-----------------------------------------------------------------------------
  Procedure: actWriteDPTPathsToProjectExecute
  Author:    sam
  Date:      04-Okt-2010
  Arguments: Sender: TObject
  Result:    None
  Description: write the path-settings from DPT into the current project files.
-----------------------------------------------------------------------------}
procedure TDMMain.actWriteDPTPathsToProjectExecute(Sender: TObject);
begin
  UpdateProjectFiles(true);
end;

{-----------------------------------------------------------------------------
  Procedure: SearchPath
  Author:    sam
  Date:      03-Dez-2012
  Arguments: None
  Result:    string
-----------------------------------------------------------------------------}
function TDMMain.SearchPath: string;
var
_SearchPath:string;
begin
  result:=FBPGPath;
  _SearchPath:=ApplicationSettings.StringValue('Application/LastUsedSearchPath');
  if (_SearchPath<>'') and DirectoryExists(_SearchPath) then result:= _SearchPath;
end;

{-----------------------------------------------------------------------------
  Procedure: SetLastUsedBPGFile
  Author:    sam
  Date:      17-Dez-2012
  Arguments: _BPGfilename: string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TDMMain.SetLastUsedBPGFile(_BPGfilename: string);
var
i:integer;
_tmp:string;
_settingName:string;
begin
  ApplicationSettings.SetString('Application/LastUsedInputFile', _BPGfilename);
  _settingName:=ApplicationSettings.FindStringValueName('Application/FileHistory/',_BPGfilename);
  if _settingName='' then begin   // check if the file is not already in the history list.
    for i:=10 downto 2 do begin
      _tmp:=ApplicationSettings.StringValue(format('Application/FileHistory/Item%d',[i-1]));
      ApplicationSettings.SetString(format('Application/FileHistory/Item%d',[i]),_tmp);
    end;
  end
  else begin // the file is already in the recent file list.
    ApplicationSettings.SetString('','');
    for i:=50 downto 2 do begin
      _tmp:=ApplicationSettings.StringValue(format('Application/FileHistory/Item%d',[i-1]));
      ApplicationSettings.SetString(format('Application/FileHistory/Item%d',[i]),_tmp);
    end;
  end;
  ApplicationSettings.SetString('Application/FileHistory/Item1',_BPGfilename);
end;


function TDMMain.IsSilentMode: boolean;
begin
  result:=(ApplicationSettings.BoolValue('Application/SilentMode') or FCommandLineSilent);
end;

procedure TDMMain.SetDPTSearchPath(const Value: string);
begin
  FDPTSearchPath := Value;
  InitProjectDataForHint;
end;

{-----------------------------------------------------------------------------
  Procedure: IsDiffToolAvailable
  Author:    sam
  Date:      18-Apr-2017
  Arguments: None
  Result:    boolean
  Description: returns true if a external Diff-Tool is setup
               and the file exists.
-----------------------------------------------------------------------------}
function TDMMain.IsDiffToolAvailable: boolean;
var
_DiffToolEXEName:string;
begin
  _DiffToolEXEName:=AbsoluteFilename(extractfilepath(application.exename),ApplicationSettings.StringValue('Application/DiffTool'));
  result:=fileexists(_DiffToolEXEName);
end;

end.
