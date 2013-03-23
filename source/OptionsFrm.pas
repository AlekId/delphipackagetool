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
unit OptionsFrm;

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
  TFrmOptions = class(TForm)
    pnlBottom: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    pnlTop: TPanel;
    edtCodeEditor: TEdit;
    btnSelectCodeEditor: TSpeedButton;
    lblSourceCodeEditor: TLabel;
    OpenDialog1: TOpenDialog;
    edtCompilerSwitches: TEdit;
    lblCompilerSwitches: TLabel;
    cbxAutomaticShowAddPathDialog: TCheckBox;
    cbxCreateBatchFile: TCheckBox;
    cbxChangeFiles: TCheckBox;
    cbxModifyEnvironment: TCheckBox;
    edtSourceEditorParams: TEdit;
    lblSourceEditorParams: TLabel;
    cbxAutoBackup: TCheckBox;
    lblDiffTool: TLabel;
    edtDiffTool: TEdit;
    btnDiffTool: TSpeedButton;
    cbxTrace: TCheckBox;
    cbxBackupSourceOnly: TCheckBox;
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSelectCodeEditorClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSelectOnBeforeInstallAllClick(Sender: TObject);
    procedure btnOnAfterInstallAllClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbxAutoBackupExit(Sender: TObject);
    procedure btnDiffToolClick(Sender: TObject);
  private
    function VerifySettings:boolean;
    procedure GUIToSettings;
    procedure SettingsToGUI;
  public
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
  Procedure: TFrmOptions.btnOkClick
  Author:    Not available
  Date:      03-Jul-2003
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmOptions.btnOkClick(Sender: TObject);
begin
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmOptions.FormShow
  Author:    Not available
  Date:      03-Jul-2003
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmOptions.FormShow(Sender: TObject);
var
_DelphiPlaceHolder:string;
begin
  SettingsToGUI;
  _DelphiPlaceHolder:=GetDelphiPathTag(DMMain.CurrentDelphiVersion);
  VerifySettings;
end;

{-----------------------------------------------------------------------------
  Procedure: btnSelectCodeEditorClick
  Author:    sam
  Date:      13-Jul-2004
  Arguments: Sender: TObject
  Result:    None
  Description: let the user define the codeeditor.
-----------------------------------------------------------------------------}
procedure TFrmOptions.btnSelectCodeEditorClick(Sender: TObject);
begin
  OpenDialog1.DefaultExt:='.exe';
  if not OpenDialog1.execute then exit;
  edtCodeEditor.Text:=OpenDialog1.FileName;
end;

{-----------------------------------------------------------------------------
  Procedure: FormClose
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: Sender: TObject; var Action: TCloseAction
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmOptions.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult<>mrOk then exit;
  GUIToSettings;
end;

{-----------------------------------------------------------------------------
  Procedure: FormCreate
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmOptions.FormCreate(Sender: TObject);
begin
end;

{-----------------------------------------------------------------------------
  Procedure: FormDestroy
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmOptions.FormDestroy(Sender: TObject);
begin
  DMMain.DPTSearchPath := DMMain.GetGlobalSearchPath(false);
end;

{-----------------------------------------------------------------------------
  Procedure: btnSelectOnBeforeInstallAllClick
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmOptions.btnSelectOnBeforeInstallAllClick(Sender: TObject);
begin
  OpenDialog1.Filter:='Executeable Files|*.exe;*.bat;*.com';
  if not OpenDialog1.execute then exit;
end;

{-----------------------------------------------------------------------------
  Procedure: btnOnAfterInstallAllClick
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmOptions.btnOnAfterInstallAllClick(Sender: TObject);
begin
  OpenDialog1.Filter:='Executeable Files|*.exe;*.bat;*.com';
  if not OpenDialog1.execute then exit;
end;

{-----------------------------------------------------------------------------
  Procedure: VerifySettings
  Author:    sam
  Date:      11-Jan-2006
  Arguments: None
  Result:    None
  Description: check the settings if they are still valid.
-----------------------------------------------------------------------------}
function TFrmOptions.VerifySettings:boolean;
begin
  result:=false;
  if (trim(edtCodeEditor.Text)<>'') and
     (not FileExists(edtCodeEditor.Text)) then begin
    ShowMessage(format('Please check if the File <%s> really exists.', [edtCodeEditor.Text]));
    edtCodeEditor.SetFocus;
    exit;
  end;

  if (trim(edtDiffTool.Text)<>'') and
     (not FileExists(edtDiffTool.Text)) then begin
    ShowMessage(format('Please check if the File <%s> really exists.', [edtDiffTool.Text]));
    edtDiffTool.SetFocus;
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
procedure TFrmOptions.GUIToSettings;
begin
  DMMain.ApplicationSettings.SetString('Application/SourceCodeEditor',9,edtCodeEditor.Text);
  DMMain.ApplicationSettings.SetString('Application/SourceCodeEditorParams',28,edtSourceEditorParams.Text);
  DMMain.ApplicationSettings.SetBoolean('Application/AutomaticSearchFiles', 18,cbxAutomaticShowAddPathDialog.Checked);
  DMMain.ApplicationSettings.SetFile('Application/DiffTool', 29,edtDiffTool.Text);
  DMMain.ProjectSettings.SetString('Application/CompilerSwitches',3,edtCompilerSwitches.Text);
  DMMain.ProjectSettings.SetBoolean('Application/CreateInstallBatch',4,cbxCreateBatchFile.Checked);
  DMMain.ProjectSettings.SetBoolean('Application/ChangeFiles', 8,cbxChangeFiles.Checked);
  DMMain.ProjectSettings.SetBoolean('Application/ModifyEnvironmentPath', 9,cbxModifyEnvironment.Checked);
  DMMain.ProjectSettings.SetBoolean('Application/AutoBackup', 12,cbxAutoBackup.checked);
  DMMain.ProjectSettings.SetBoolean('Application/Trace',13,cbxTrace.checked);
  DMMain.ProjectSettings.SetBoolean('Application/BackupSourceOnly',15,cbxBackupSourceOnly.Checked);
end;

{-----------------------------------------------------------------------------
  Procedure: SettingsToGUI
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: None
  Result:    None
  Description:  prepare the GUI.
-----------------------------------------------------------------------------}
procedure TFrmOptions.SettingsToGUI;
var
_compilerSwitches:string;
begin
  edtCodeEditor.Text:=DMMain.ApplicationSettings.StringValue('Application/SourceCodeEditor',9);
  edtSourceEditorParams.Text:=DMMain.ApplicationSettings.StringValue('Application/SourceCodeEditorParams',28);  
  _compilerSwitches:=DMMain.ProjectSettings.StringValue('Application/CompilerSwitches',3);
  if _compilerSwitches='' then DMMain.ApplicationSettings.StringValue('Application/CompilerSwitches',11);
  edtCompilerSwitches.Text:=_compilerSwitches;
  edtDiffTool.Text:=     DMMain.ApplicationSettings.FileValue('Application/DiffTool', 29);
  cbxAutomaticShowAddPathDialog.Checked:=DMMain.ApplicationSettings.BoolValue('Application/AutomaticSearchFiles', 18);
  cbxCreateBatchFile.Checked:=DMMain.ProjectSettings.BoolValue('Application/CreateInstallBatch',4);
  cbxChangeFiles.Checked:=DMMain.ProjectSettings.BoolValue('Application/ChangeFiles', 8);
  cbxModifyEnvironment.Checked:=DMMain.ProjectSettings.BoolValue('Application/ModifyEnvironmentPath', 9);
  cbxAutoBackup.checked:=DMMain.ProjectSettings.BoolValue('Application/AutoBackup', 12);
  cbxTrace.checked:=DMMain.ProjectSettings.BoolValue('Application/Trace',13);
  cbxBackupSourceOnly.Checked:=DMMain.ProjectSettings.BoolValue('Application/BackupSourceOnly',15);
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
procedure TFrmOptions.FormCloseQuery(Sender: TObject;var CanClose: Boolean);
begin
  if ModalResult<>mrOk then exit;
  CanClose:=VerifySettings;
end;

{-----------------------------------------------------------------------------
  Procedure: cbxAutoBackupExit
  Author:    herzogs2
  Date:      05-Mai-2010
  Arguments: Sender: TObject
  Result:    None
  Description: if we want to make a backup, then the compiler switch -Q must be removed
               -Q (quiet) determs how much output is generated by the compiler.
-----------------------------------------------------------------------------}
procedure TFrmOptions.cbxAutoBackupExit(Sender: TObject);
begin
  if not cbxAutoBackup.checked then exit;
  edtCompilerSwitches.Text:=StringReplace(edtCompilerSwitches.Text,'-Q','',[]);
end;

{-----------------------------------------------------------------------------
  Procedure: btnDiffToolClick
  Author:    herzogs2
  Date:      06-Mai-2010
  Arguments: Sender: TObject
  Result:    None
  Description: show file selection dialog to let the user choose his favorite
               diff-tool.
-----------------------------------------------------------------------------}
procedure TFrmOptions.btnDiffToolClick(Sender: TObject);
begin
  OpenDialog1.DefaultExt:='.exe';
  if not OpenDialog1.execute then exit;
  edtDiffTool.Text:=OpenDialog1.FileName;
end;

end.

