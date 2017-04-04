{-----------------------------------------------------------------------------
 Unit Name: OptionsFrm
 Author:    Samuel Herzog
 Purpose:  - application options dialog.
History:
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
    cbxChangeFiles: TCheckBox;
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
  Author:    sam
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
  Author:    sam
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
  Author:    sam
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
  Author:    sam
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
  Author:    sam
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
  DMMain.ApplicationSettings.SetBoolean('Application/ChangeFiles', 13,cbxChangeFiles.Checked);
  DMMain.ApplicationSettings.SetBoolean('Application/Trace',20,cbxTrace.checked);
  DMMain.ApplicationSettings.SetBoolean('Application/BackupSourceOnly',30,cbxBackupSourceOnly.Checked);
end;

{-----------------------------------------------------------------------------
  Procedure: SettingsToGUI
  Author:    sam
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
  cbxChangeFiles.Checked:=DMMain.ApplicationSettings.BoolValue('Application/ChangeFiles', 13);
  cbxTrace.checked:=DMMain.ApplicationSettings.BoolValue('Application/Trace',20);
  cbxBackupSourceOnly.Checked:=DMMain.ApplicationSettings.BoolValue('Application/BackupSourceOnly',30);
end;

{-----------------------------------------------------------------------------
  Procedure: FormCloseQuery
  Author:    sam
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
  Author:    sam
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

