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
    lblDiffTool: TLabel;
    edtDiffTool: TEdit;
    btnDiffTool: TSpeedButton;
    cbxTrace: TCheckBox;
    cbxBackupSourceOnly: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure btnSelectCodeEditorClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure btnSelectOnBeforeInstallAllClick(Sender: TObject);
    procedure btnOnAfterInstallAllClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnDiffToolClick(Sender: TObject);
  private
    function VerifySettings:boolean;
    procedure GUIToSettings;
    procedure SettingsToGUI;
  public
  end;


implementation

uses
  uDPTDelphiPackage,
  MainDM;

{$R *.dfm}

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
  DMMain.ApplicationSettings.SetString('Application/CompilerSwitches',11,edtCompilerSwitches.Text);
  DMMain.ApplicationSettings.SetBoolean('Application/CreateInstallBatch',4,cbxCreateBatchFile.Checked);
  DMMain.ApplicationSettings.SetBoolean('Application/ChangeFiles', 13,cbxChangeFiles.Checked);
  DMMain.ApplicationSettings.SetBoolean('Application/ModifyEnvironmentPath', 14,cbxModifyEnvironment.Checked);
  DMMain.ApplicationSettings.SetBoolean('Application/Trace',20,cbxTrace.checked);
  DMMain.ApplicationSettings.SetBoolean('Application/BackupSourceOnly',30,cbxBackupSourceOnly.Checked);
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
begin
  edtCodeEditor.Text:=DMMain.ApplicationSettings.StringValue('Application/SourceCodeEditor',9);
  edtSourceEditorParams.Text:=DMMain.ApplicationSettings.StringValue('Application/SourceCodeEditorParams',28);
  edtCompilerSwitches.Text:=DMMain.ApplicationSettings.StringValue('Application/CompilerSwitches',11);
  edtDiffTool.Text:=DMMain.ApplicationSettings.FileValue('Application/DiffTool', 29);
  cbxAutomaticShowAddPathDialog.Checked:=DMMain.ApplicationSettings.BoolValue('Application/AutomaticSearchFiles', 18);
  cbxCreateBatchFile.Checked:=DMMain.ApplicationSettings.BoolValue('Application/CreateInstallBatch',4);
  cbxChangeFiles.Checked:=DMMain.ApplicationSettings.BoolValue('Application/ChangeFiles', 13);
  cbxModifyEnvironment.Checked:=DMMain.ApplicationSettings.BoolValue('Application/ModifyEnvironmentPath', 14);
  cbxTrace.checked:=DMMain.ApplicationSettings.BoolValue('Application/Trace',20);
  cbxBackupSourceOnly.Checked:=DMMain.ApplicationSettings.BoolValue('Application/BackupSourceOnly',30);
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

