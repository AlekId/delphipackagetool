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
    edtSourceEditorParams: TEdit;
    lblSourceEditorParams: TLabel;
    lblDiffTool: TLabel;
    edtDiffTool: TEdit;
    btnDiffTool: TSpeedButton;
    cbxTrace: TCheckBox;
    cbxBackupSourceOnly: TCheckBox;
    cbxShowChangedFileInDiffTool: TCheckBox;
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

function ShowOptionsDialog:boolean;

implementation

uses
  uDPTDelphiPackage,
  uDPTPathFilenameConvert,
  MainDM;

{$R *.dfm}

{*-----------------------------------------------------------------------------
  Procedure: ShowOptionsDialog
  Author:    sam
  Date:      14-Apr-2017
  Arguments: None
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function ShowOptionsDialog:boolean;
var
_FrmOptions: TFrmOptions;
begin
  result:=false;
  _FrmOptions := TFrmOptions.create(nil);
  try
    if _FrmOptions.showmodal<>mrOk then exit;
    result:=true;
  finally
    _FrmOptions.free;
  end;
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
  _DelphiPlaceHolder:=GetDelphiPathTag(DMMain.DelphiVersion);
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
  edtCodeEditor.Text:=RelativeFilename(extractfilepath(application.exename),OpenDialog1.FileName,DMMain.DelphiVersion);
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
var
_filename:string;
begin
  result:=false;
  _filename:=AbsoluteFilename(extractfilepath(application.exename),edtCodeEditor.Text);
  if (trim(edtCodeEditor.Text)<>'') and
     (not FileExists(_filename)) then begin
    ShowMessage(format('Please check if the File <%s> really exists.', [_filename]));
    edtCodeEditor.SetFocus;
    exit;
  end;
  _filename:=AbsoluteFilename(extractfilepath(application.exename),edtDiffTool.Text);
  if (trim(edtDiffTool.Text)<>'') and
     (not FileExists(_filename)) then begin
    ShowMessage(format('Please check if the File <%s> really exists.', [_filename]));
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
  DMMain.ApplicationSettings.SetString('Application/SourceCodeEditor',edtCodeEditor.Text);
  DMMain.ApplicationSettings.SetString('Application/SourceCodeEditorParams',edtSourceEditorParams.Text);
  DMMain.ApplicationSettings.SetBoolean('Application/AutomaticSearchFiles',cbxAutomaticShowAddPathDialog.Checked);
  DMMain.ApplicationSettings.SetFile('Application/DiffTool',edtDiffTool.Text);
  DMMain.ApplicationSettings.SetString('Application/CompilerSwitches',edtCompilerSwitches.Text);
  DMMain.ApplicationSettings.SetBoolean('Application/DisplayFilesInDiffTool',cbxShowChangedFileInDiffTool.checked);
  DMMain.ApplicationSettings.SetBoolean('Application/Trace',cbxTrace.checked);
  DMMain.ApplicationSettings.SetBoolean('Application/BackupSourceOnly',cbxBackupSourceOnly.Checked);
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
  edtCodeEditor.Text:=DMMain.ApplicationSettings.StringValue('Application/SourceCodeEditor');
  edtSourceEditorParams.Text:=DMMain.ApplicationSettings.StringValue('Application/SourceCodeEditorParams');
  edtCompilerSwitches.Text:=DMMain.ApplicationSettings.StringValue('Application/CompilerSwitches');
  edtDiffTool.Text:=DMMain.ApplicationSettings.FileValue('Application/DiffTool');
  cbxAutomaticShowAddPathDialog.Checked:=DMMain.ApplicationSettings.BoolValue('Application/AutomaticSearchFiles');
  cbxShowChangedFileInDiffTool.checked:=DMMain.ApplicationSettings.BoolValue('Application/DisplayFilesInDiffTool');
  cbxTrace.checked:=DMMain.ApplicationSettings.BoolValue('Application/Trace');
  cbxBackupSourceOnly.Checked:=DMMain.ApplicationSettings.BoolValue('Application/BackupSourceOnly');
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
  edtDiffTool.Text:=RelativeFilename(extractfilepath(application.exename),OpenDialog1.FileName,DMMain.DelphiVersion);
end;

end.

