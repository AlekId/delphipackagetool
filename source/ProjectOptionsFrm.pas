{-----------------------------------------------------------------------------
 Unit Name: OptionsFrm
 Author:    Samuel Herzog
 Purpose:  - set library path and external source code editor.
 History:  06.09.2003 SH -added setting to define an sourcecode editor.
           23.09.2004 SH -added feature <OnBeforeInstallAll>.
           12.05.2005 SH -enabled scrollbars in the path list.
           11.01.2006 SH -check edit field content of the file exits when leaving.
           17.01.2006 SH - settings for the batch-files before/after install are now
                           stored project-specific to file <PROJECTNAME.ini>.
           18.01.2006 SH - fix for OnBeforeInstall/OnAfterInstall.
           28.01.2006 SH - filepath of OnBeforeInstall/OnAfterInstall is now also
                           converted to relative path. (relative to the bpg-file)
                         - added new setting to allow the user to decide if the
                           install batch-file and the install .reg-files shall be
                           created or not.
           31.01.2008 SH - removed dependency from FrmMain.
-----------------------------------------------------------------------------}
unit ProjectOptionsFrm;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  FileCtrl,
  ExtCtrls,
  uDPTSettings;

type
  TFrmProjectOptions = class(TForm)
    mmoSearchPath: TMemo;
    btnAddDefaultPath: TBitBtn;
    btnVerifyDirectories: TBitBtn;
    pnlBottom: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    pnlTop: TPanel;
    lblSearchPath: TLabel;
    OpenDialog1: TOpenDialog;
    edtCompilerSwitches: TEdit;
    lblCompilerSwitches: TLabel;
    btnAddpath: TBitBtn;
    lblBeforeInstallAll: TLabel;
    edtBeforeInstallAll: TEdit;
    btnSelectOnBeforeInstallAll: TSpeedButton;
    lblAfterInstallAll: TLabel;
    edtAfterInstallAll: TEdit;
    btnOnAfterInstallAll: TSpeedButton;
    edtLibSuffix: TEdit;
    lblLibSuffix: TLabel;
    lblDebugCompilerSwitches: TLabel;
    edtDebugCompilerSwitches: TEdit;
    lblReleaseCompilerSwitches: TLabel;
    edtReleaseCompilerSwitches: TEdit;
    cbxAutoBackup: TCheckBox;
    cbxCreateInstallBatch: TCheckBox;
    procedure btnAddPathClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnVerifyDirectoriesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddDefaultPathClick(Sender: TObject);
    procedure btnSelectOnBeforeInstallAllClick(Sender: TObject);
    procedure btnOnAfterInstallAllClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbxAutoBackupExit(Sender: TObject);
  private
    FSearchPaths:TStrings;
    function VerifySettings:boolean;
    procedure GUIToSettings;
    procedure SettingsToGUI;
    function LoadSearchPathData:boolean;
    function VerifyDirectories(const _delphiVersion:integer;const _CurrentPlatform, _CurrentConfig: string): boolean;
  public
    function SaveSearchPathData:boolean;
    function AddPath(_path:string):boolean;
  end;


implementation

uses
  uDPTDefinitions,
  uDPTMisc,
  uDPTDelphiPackage,
  uDPTPathFilenameConvert,
  Windows,
  MainDM;

{$R *.dfm}

{-----------------------------------------------------------------------------
  Procedure: TFrmOptions.btnAddpathClick
  Author:    Not available
  Date:      03-Jul-2003
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmProjectOptions.btnAddPathClick(Sender: TObject);
resourcestring
cSelectPath='Select Path';
var
  _path: string;
begin
  _path := '';
  if not SelectDirectory(cSelectPath,'',_path) then exit;
  _path := lowercase(_path);
  _path := RelativePath(DMMain.BPGPath,_path,DMMain.CurrentDelphiVersion);
  if trim(_path)='' then exit;
  mmoSearchPath.Lines.add(_path);
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmOptions.btnOkClick
  Author:    Not available
  Date:      03-Jul-2003
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmProjectOptions.btnOkClick(Sender: TObject);
begin
  FSearchPaths.Assign(mmoSearchPath.Lines);
  SaveSearchPathData;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmOptions.FormShow
  Author:    Not available
  Date:      03-Jul-2003
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmProjectOptions.FormShow(Sender: TObject);
var
_DelphiPlaceHolder:string;
begin
  SettingsToGUI;
  _DelphiPlaceHolder:=GetDelphiPathTag(DMMain.CurrentDelphiVersion);
  with mmoSearchPath.Lines do begin
    clear;
    insert(0,_DelphiPlaceHolder+'\lib;');
    insert(1,_DelphiPlaceHolder+'\projects\bpl;');
    insert(3,_DelphiPlaceHolder+'\bin;');
    insert(4,_DelphiPlaceHolder+'\servers\ocx;');
    insert(5,DMMain.CurrentBPLOutputPath+';');
    mmoSearchPath.Lines.Assign(FSearchPaths);
  end;
  VerifyDirectories(DMMain.CurrentDelphiVersion,DMMain.PlatformToCompile, DMMain.ConfigToCompile);
  VerifySettings;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmOptions.VerifyDirectories
  Author:    Not available
  Date:      16-Apr-2003
  Arguments: None
  Result:    boolean
  Description: check if the directories really exist.
-----------------------------------------------------------------------------}
function TFrmProjectOptions.VerifyDirectories(const _DelphiVersion:integer;const _CurrentPlatform, _CurrentConfig: string): boolean;
resourcestring
cDirectoryDoesNotExist='The directory <%s> does not exits. Remove this entry ?';
var
  i,j: Integer;
  _path,_curpath: string;
begin
  result := true;
  with mmoSearchPath.Lines do begin
    i:=0;
    while i<=count do begin
      _path:=trim(lowercase(mmoSearchPath.Lines[i]));
      if _path='' then begin
        mmoSearchPath.Lines.delete(i);
        inc(i);
        continue;
      end;
      _path:=RemoveTrailingSemikolon(_path);
      if _path='' then begin
        mmoSearchPath.Lines.delete(i);
        inc(i);
        continue;
      end;
      _path:=IncludeTrailingPathDelimiter(_path);
      for j:=i+1 to mmoSearchPath.Lines.Count-1 do begin
        _curpath:=trim(lowercase(mmoSearchPath.Lines[j]));
        _curpath:=RemoveTrailingSemikolon(_curpath);
        _curpath:=IncludeTrailingPathDelimiter(_curpath);
        if _curpath=_path then begin
          mmoSearchPath.Lines.delete(i);
          trace(5,'Removed double entry <%s> from search path.',[_path]);
          break;
        end;
      end;
      inc(i);
    end;
    i:=0;
    while i<=count do begin
      _path := ReplaceTag(trim(mmoSearchPath.Lines[i]),_delphiVersion,_CurrentPlatform, _CurrentConfig);
      if _path = '' then begin
        inc(i);
        continue;
      end;
      _path:=RemoveTrailingSemikolon(_Path);
      _path:=AbsolutePath(DMMain.BPGPath,_path,_DelphiVersion,DMMain.PlatformToCompile, DMMain.ConfigToCompile);
      if not SysUtils.DirectoryExists(_path) then begin
        if Application.MessageBox(pchar(format(cDirectoryDoesNotExist, [_Path])),pchar(cWarning),MB_ICONWARNING or MB_YESNO)=IDYes then begin
          mmoSearchPath.Lines.delete(i);
        end
        else inc(i);
        Result := false;
      end
      else inc(i);
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmOptions.btnVerifyDirectoriesClick
  Author:    Not available
  Date:      03-Jul-2003
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmProjectOptions.btnVerifyDirectoriesClick(Sender: TObject);
begin
  if not VerifyDirectories(DMMain.CurrentDelphiVersion,DMMain.PlatformToCompile, DMMain.ConfigToCompile) then exit;
  ShowMessage('Path settings seems to be ok.');
end;

{-----------------------------------------------------------------------------
  Procedure: FormClose
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: Sender: TObject; var Action: TCloseAction
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmProjectOptions.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult<>mrOk then exit;
  GUIToSettings;
end;

{-----------------------------------------------------------------------------
  Procedure: LoadSearchPathData
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: None
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function TFrmProjectOptions.LoadSearchPathData: boolean;
var
_filename:string;
begin
  _filename:=changeFileExt(DMMain.BPGFilename,'.txt');
  if fileexists(_filename) then FSearchPaths.LoadfromFile(_filename);
  result:=true;
end;

{-----------------------------------------------------------------------------
  Procedure: FormCreate
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmProjectOptions.FormCreate(Sender: TObject);
begin
  FSearchPaths:=TStringList.Create;
  LoadSearchPathData;
end;

{-----------------------------------------------------------------------------
  Procedure: SaveData
  Author:    sam
  Date:      15-Mrz-2005
  Arguments: None
  Result:    boolean
  Description: save search path settings.
-----------------------------------------------------------------------------}
function TFrmProjectOptions.SaveSearchPathData: boolean;
resourcestring
cFileMarkedAsReadOnly='The file <%s> is marked as read-only. Do you want to change it anyway ?';
var
  _filename: string;
begin
  result:=false;
  if DMMain.BPGFilename='' then exit;
  _filename:=changeFileExt(DMMain.BPGFilename,'.txt');
  if FileExists(_filename) then begin
    if IsFileReadOnly(_filename) then begin
      if Application.MessageBox(pchar(format(cFileMarkedAsReadOnly,[_filename])),pchar(cWarning),MB_ICONWARNING or MB_YESNO)=IDNo then exit;
      if not RemoveReadOnlyFlag(_filename,true) then exit;
    end;
  end;
  FSearchPaths.SaveToFile(_filename);
  result:=true;
end;

{-----------------------------------------------------------------------------
  Procedure: FormDestroy
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmProjectOptions.FormDestroy(Sender: TObject);
begin
  FSearchPaths.Free;
  DMMain.DPTSearchPath := DMMain.GetGlobalSearchPath(false);
end;

{-----------------------------------------------------------------------------
  Procedure: AddPath
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: _path: string
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function TFrmProjectOptions.AddPath(_path: string): boolean;
begin
  result:=false;
  _path:=RemoveTrailingSemikolon(lowercase(_path));
  if trim(_path)='' then exit;
  _path:=_path+';';
  if FSearchPaths.IndexOf(_path)<>-1 then begin
    trace(5,'TFrmOptions.AddPath: The path <%s> is already in the list.',[_path]);
    exit;
  end;
  FSearchPaths.Add(_path);
  trace(3,'TFrmOptions.AddPath:Added path <%s>.',[_path]);
  result:=true;
end;

{-----------------------------------------------------------------------------
  Procedure: btnAddDefaultPathClick
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmProjectOptions.btnAddDefaultPathClick(Sender: TObject);
var
_DelphiPlaceHolder:string;
begin
  _DelphiPlaceHolder:=GetDelphiPathTag(DMMain.CurrentDelphiVersion);
  if mmoSearchPath.Lines.IndexOf(DMMain.CurrentBPLOutputPath+';')=-1     then mmoSearchPath.Lines.insert(0,DMMain.CurrentBPLOutputPath+';');
  if mmoSearchPath.Lines.IndexOf(_DelphiPlaceHolder+'\lib;')=-1          then mmoSearchPath.Lines.insert(0,_DelphiPlaceHolder+'\lib;');
  if mmoSearchPath.Lines.IndexOf(_DelphiPlaceHolder+'\projects\bpl;')=-1 then mmoSearchPath.Lines.insert(0,_DelphiPlaceHolder+'\projects\bpl;');
  if mmoSearchPath.Lines.IndexOf(_DelphiPlaceHolder+'\bin;')=-1          then mmoSearchPath.Lines.insert(0,_DelphiPlaceHolder+'\bin;');
end;

{-----------------------------------------------------------------------------
  Procedure: btnSelectOnBeforeInstallAllClick
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmProjectOptions.btnSelectOnBeforeInstallAllClick(Sender: TObject);
begin
  OpenDialog1.Filter:='Executeable Files|*.exe;*.bat;*.com';
  if not OpenDialog1.execute then exit;
  edtBeforeInstallAll.Text:=RelativePath(DMMain.BPGPath,OpenDialog1.FileName,DMMain.CurrentDelphiVersion);
end;

{-----------------------------------------------------------------------------
  Procedure: btnOnAfterInstallAllClick
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmProjectOptions.btnOnAfterInstallAllClick(Sender: TObject);
begin
  OpenDialog1.Filter:='Executeable Files|*.exe;*.bat;*.com';
  if not OpenDialog1.execute then exit;
  edtAfterInstallAll.Text:=RelativePath(DMMain.BPGPath,OpenDialog1.FileName,DMMain.CurrentDelphiVersion);
end;

{-----------------------------------------------------------------------------
  Procedure: VerifySettings
  Author:    sam
  Date:      11-Jan-2006
  Arguments: None
  Result:    None
  Description: check the settings if they are still valid.
-----------------------------------------------------------------------------}
function TFrmProjectOptions.VerifySettings:boolean;
var
_filename:string;
begin
  result:=false;
  _filename:=AbsoluteFilename(DMMain.BPGPath,edtBeforeInstallAll.Text);
  if (trim(edtBeforeInstallAll.Text)<>'') and
     (not FileExists(_filename)) then begin
    ShowMessage(format('Please check if the File <%s> really exists.', [_filename]));
    edtBeforeInstallAll.SetFocus;
    exit;
  end;
  _filename:=AbsoluteFilename(DMMain.BPGPath,edtAfterInstallAll.Text);
  if (trim(edtAfterInstallAll.Text)<>'') and
     (not FileExists(_filename)) then begin
    ShowMessage(format('Please check if the File <%s> really exists.', [_filename]));
    edtAfterInstallAll.SetFocus;
    exit;
  end;

  result:=true;
end;

{-----------------------------------------------------------------------------
  Procedure: GUIToSettings
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: None
  Result:    None
  Description: write GUI to settings.
-----------------------------------------------------------------------------}
procedure TFrmProjectOptions.GUIToSettings;
begin
  DMMain.ProjectSettings.SetString('Application/Events/OnBeforeInstallAll',1,edtBeforeInstallAll.Text);
  DMMain.ProjectSettings.SetString('Application/Events/OnAfterInstallAll',2,edtAfterInstallAll.Text);
  DMMain.ProjectSettings.SetString('Application/CompilerSwitches',3,edtCompilerSwitches.Text);
  DMMain.ProjectSettings.SetBoolean('Application/CreateInstallBatch',4,cbxCreateInstallBatch.checked);
  DMMain.ProjectSettings.SetString('Application/LibSuffix',10,edtLibSuffix.Text);
  DMMain.ProjectSettings.SetString('Application/DebugCompilerSwitches',18,edtDebugCompilerSwitches.Text);
  DMMain.ProjectSettings.SetString('Application/ReleaseCompilerSwitches',19,edtReleaseCompilerSwitches.Text);
  DMMain.ProjectSettings.SetBoolean('Application/AutoBackup',20,cbxAutoBackup.Checked);
end;

{-----------------------------------------------------------------------------
  Procedure: SettingsToGUI
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: None
  Result:    None
  Description:  prepare the GUI.
-----------------------------------------------------------------------------}
procedure TFrmProjectOptions.SettingsToGUI;
var
_batchfilename:string;
_compilerSwitches:string;
begin
  _compilerSwitches:=DMMain.ProjectSettings.StringValue('Application/CompilerSwitches',3);
  if _compilerSwitches='' then DMMain.ApplicationSettings.StringValue('Application/CompilerSwitches',11);
  edtCompilerSwitches.Text:=_compilerSwitches;
  edtLibSuffix.Text:=DMMain.ProjectSettings.StringValue('Application/LibSuffix',10);
  _batchfilename:=DMMain.ProjectSettings.StringValue('Application/Events/OnBeforeInstallAll',1);
  edtBeforeInstallAll.Text:=_batchfilename;
  _batchfilename:=DMMain.ProjectSettings.StringValue('Application/Events/OnAfterInstallAll',2);
  edtAfterInstallAll.Text :=_batchfilename;
  edtDebugCompilerSwitches.Text:=DMMain.ProjectSettings.StringValue('Project/DebugCompilerSwitches',18);
  edtReleaseCompilerSwitches.Text:=DMMain.ProjectSettings.StringValue('Project/ReleaseCompilerSwitches',19);
  cbxAutoBackup.Checked:=DMMain.ProjectSettings.BoolValue('Application/AutoBackup',20);
  cbxCreateInstallBatch.checked:=DMMain.ProjectSettings.BoolValue('Application/CreateInstallBatch',4);
end;

{-----------------------------------------------------------------------------
  Procedure: FormCloseQuery
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: Sender: TObject;var CanClose: Boolean
  Result:    None
  Description: check if the settings are ok. If they are ok then allow to close,
               otherwise not.
-----------------------------------------------------------------------------}
procedure TFrmProjectOptions.FormCloseQuery(Sender: TObject;var CanClose: Boolean);
begin
  if ModalResult<>mrOk then exit;
  CanClose:=VerifySettings;
end;

{*-----------------------------------------------------------------------------
  Procedure: cbxAutoBackupExit
  Author:    sam
  Date:      13-Mai-2015
  Arguments: Sender: TObject
  Result:    None
  Description: if we want to make a backup, then the compiler switch -Q must be removed
               -Q (quiet) determs how much output is generated by the compiler.
-----------------------------------------------------------------------------}
procedure TFrmProjectOptions.cbxAutoBackupExit(Sender: TObject);
begin
  if not cbxAutoBackup.checked then exit;
  edtCompilerSwitches.Text       :=StringReplace(edtCompilerSwitches.Text,'-Q','',[]);
  edtDebugCompilerSwitches.Text  :=StringReplace(edtDebugCompilerSwitches.Text,'-Q','',[]);
  edtReleaseCompilerSwitches.Text:=StringReplace(edtReleaseCompilerSwitches.Text,'-Q','',[]);
end;

end.

