{-----------------------------------------------------------------------------
 Unit Name: PathSelectionFrm
 Author:    Samuel Herzog
 Purpose:   search the drive for certain files
 History:
-----------------------------------------------------------------------------}
unit BPLandDCPSearchFrm;

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
  IvDictio,
  IvMulti;

type
  TFrmPathSelection = class(TForm)
    pnlBottom: TPanel;
    pnlTop: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    lstFiles: TListBox;
    btnAbort: TBitBtn;
    edtSearchLocation: TEdit;
    btnFind: TBitBtn;
    lblSearchLocation: TLabel;
    lblSearchmask: TLabel;
    edtSearchMask: TEdit;
    btnAddPath: TBitBtn;
    btnSelectPath: TSpeedButton;
    IvTranslator1: TIvTranslator;
    procedure FormCreate(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lstFilesDblClick(Sender: TObject);
    procedure btnAddPathClick(Sender: TObject);
    procedure btnSelectPathClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FAbortScan: boolean;
  public
    SearchLocation: string;
    SearchCriteria: string;
  end;

function ShowBPLPathDialog(_searchLocation, _searchcriteria: string): string;


implementation

uses uNVBMisc,
  MainFrm,
  IvMlUtil,
  FileCtrl,
  OptionsFrm;

{$R *.dfm}

{-----------------------------------------------------------------------------
  Procedure: ShowSelectPathDialog
  Author:    Not available
  Date:      24-Dez-2003
  Arguments: _searchLocation, _searchcriteria: string
  Result:    string
  Description: Main function of this file. Display the dialog.
-----------------------------------------------------------------------------}
function ShowSelectPathDialog(_searchLocation, _searchcriteria: string): string;
var
  _FrmPathSelection: TFrmPathSelection;
begin
  result := '';
  _FrmPathSelection := TFrmPathSelection.create(nil);
  with _FrmPathSelection do begin
    SearchLocation := _searchLocation;
    SearchCriteria := _searchcriteria;
    edtSearchLocation.Text := _searchLocation;
    edtSearchMask.Text := _searchcriteria;
    btnFind.Caption:=FrmMain.translate({ivde}'Search <%s>',[_searchcriteria]);
    try
      showmodal;
      if lstFiles.itemindex > -1 then result := lstFiles.Items[lstFiles.itemindex];
    finally
      if assigned(_FrmPathSelection) then FreeAndNil(_FrmPathSelection);
    end;
  end;
end;


{-----------------------------------------------------------------------------
  Procedure: TFrmPathSelection.btnFindClick
  Author:    Not available
  Date:      24-Dez-2003
  Arguments: Sender: TObject
  Result:    None
  Description: search the harddisk for the files specified by the <_mask>.
-----------------------------------------------------------------------------}
procedure TFrmPathSelection.btnFindClick(Sender: TObject);
var
  _fileext: string;
  _mask: string;
begin
  if edtSearchMask.Text='' then begin
    MessageDlg(FrmMain.Translate({ivde}'Please enter a search criteria.',[]), mtInformation, [mbOK], 0);
    edtSearchMask.SetFocus;
    exit;
  end;
  FAbortScan := false;
  btnOk.Enabled := false;
  btnAbort.Enabled:=true;
  Screen.cursor := crHourGlass;
  lstFiles.Clear;
  _mask := edtSearchMask.Text;
  _fileext := lowercase(ExtractFileExt(_Mask));
  if _fileext = '.dcu' then begin
    _Mask := ChangeFileExt(_Mask, '.pas');
    AllFilesOfDrive(edtSearchLocation.Text, _Mask, lstFiles.Items, FAbortScan);
  end;
  AllFilesOfDrive(edtSearchLocation.Text, edtSearchMask.Text, lstFiles.Items, FAbortScan);
  Screen.cursor := crDefault;
  btnOk.enabled := true;
  btnAbort.Enabled:=false;
  btnAddPath.Enabled:=(lstFiles.Count>0);
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmPathSelection.FormCreate
  Author:    Not available
  Date:      24-Dez-2003
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmPathSelection.FormCreate(Sender: TObject);
begin
  FAbortScan := false;
  btnAbort.Enabled:=false;
  btnAddPath.Enabled:=false;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmPathSelection.btnAbortClick
  Author:    Not available
  Date:      24-Dez-2003
  Arguments: Sender: TObject
  Result:    None
  Description: interrupt the searching of the harddisk
-----------------------------------------------------------------------------}
procedure TFrmPathSelection.btnAbortClick(Sender: TObject);
begin
  FAbortScan := true;
  Screen.cursor := crDefault;
  btnOk.enabled := true;
  btnAbort.Enabled:=false;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmPathSelection.btnCancelClick
  Author:    Not available
  Date:      24-Dez-2003
  Arguments: Sender: TObject
  Result:    None
  Description: close the dialog.
-----------------------------------------------------------------------------}
procedure TFrmPathSelection.btnCancelClick(Sender: TObject);
begin
  FAbortScan := true;
  Screen.cursor := crDefault;
  btnOk.Enabled := true;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmPathSelection.FormCloseQuery
  Author:    Not available
  Date:      24-Dez-2003
  Arguments: Sender: TObject; var CanClose: Boolean
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmPathSelection.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FAbortScan := true;
  Screen.cursor := crDefault;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmPathSelection.lstFilesDblClick
  Author:    Not available
  Date:      24-Dez-2003
  Arguments: Sender: TObject
  Result:    None
  Description: start external editor and show the file.
-----------------------------------------------------------------------------}
procedure TFrmPathSelection.lstFilesDblClick(Sender: TObject);
begin
  FrmMain.ShowFile(lstFiles.Items[lstFiles.ItemIndex]);
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmPathSelection.btnAddPathClick
  Author:    Not available
  Date:      24-Dez-2003
  Arguments: Sender: TObject
  Result:    None
  Description: add the currently selected path to the search path of the project.
-----------------------------------------------------------------------------}
procedure TFrmPathSelection.btnAddPathClick(Sender: TObject);
var
  _FrmOptions: TFrmOptions;
  _path: string;
begin
  if lstFiles.ItemIndex<0 then exit;
  _FrmOptions := TFrmOptions.create(self);
  try
    _path := lowercase(ExtractFilePath(lstFiles.Items[lstFiles.ItemIndex]));
    _path := ExtractRelativePath(FrmMain.BPGPath,_path);
    _FrmOptions.addPath(_path);
    _FrmOptions.savedata;
  finally
    _FrmOptions.free;
  end;
  if not FrmMain.cbxSilentMode.checked then MessageDlg(FrmMain.Translate({ivde}'Added directory <%s> to the search path.',[_path]), mtInformation, [mbOK], 0);
end;

{-----------------------------------------------------------------------------
  Procedure: btnSelectPathClick
  Author:    sam
  Date:      19-Apr-2004
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}
procedure TFrmPathSelection.btnSelectPathClick(Sender: TObject);
var
  _Dir: string;
begin
  _Dir := '';
  if ivSelectDirectory(_Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0,FrmMain.IvTextDictionary1) then begin
    if LastPos(_Dir,'\')<>length(_Dir) then _Dir:=_Dir+'\';
    edtSearchLocation.Text:=_Dir;
    FrmMain.ApplicationSettings.SetString('Application/LastUsedSearchPath',15,_Dir);
  end;
end;

procedure TFrmPathSelection.FormShow(Sender: TObject);
begin
  IvTranslator1.Open(self);
end;

end.

