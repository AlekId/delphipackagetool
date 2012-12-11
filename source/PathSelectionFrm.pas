{-----------------------------------------------------------------------------
 Unit Name: PathSelectionFrm
 Author:    Samuel Herzog
 Purpose:   search the drive for certain files
 History:
-----------------------------------------------------------------------------}
unit PathSelectionFrm;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  Buttons,
  ExtCtrls,
  ActnList,
  Menus;

type
  TFrmPathSelection = class(TForm)
    pnlBottom: TPanel;
    pnlTop: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    lstFiles: TListBox;
    btnAbort: TBitBtn;
    btnFind: TBitBtn;
    lblSearchLocation: TLabel;
    lblSearchmask: TLabel;
    edtSearchMask: TEdit;
    btnAddPath: TBitBtn;
    btnSelectPath: TSpeedButton;
    PopupMenu: TPopupMenu;
    ActionList: TActionList;
    actShowFile: TAction;
    ShowFile1: TMenuItem;
    actAddPath: TAction;
    AddPathtoProject1: TMenuItem;
    cbxSearchLocation: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lstFilesDblClick(Sender: TObject);
    procedure btnSelectPathClick(Sender: TObject);
    procedure edtSearchMaskExit(Sender: TObject);
    procedure actShowFileExecute(Sender: TObject);
    procedure actAddPathExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AddDirToCombobox(_dir:string);
    procedure cbxSearchLocationExit(Sender: TObject);
  private
    AbortScan: boolean;
    SearchCriteria: string;
    SearchLocation: string;
  public
  end;

function ShowSelectPathDialog(_searchLocation, _searchcriteria: string;const _showButtonAddToProject:boolean): string;


implementation

uses
  uDPTDefinitions,
  uDPTMisc,
  uDPTPathFilenameConvert,
  MainDM,
  FileCtrl,
  Windows,
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
function ShowSelectPathDialog(_searchLocation, _searchcriteria: string;const _showButtonAddToProject:boolean): string;
var
  _FrmPathSelection: TFrmPathSelection;
  _index:integer;
begin
  result := '';
  _FrmPathSelection := TFrmPathSelection.create(nil);
  try
    _FrmPathSelection.btnAddPath.Visible:=_showButtonAddToProject;
    _FrmPathSelection.SearchLocation := lowercase(_searchLocation);
    _FrmPathSelection.SearchCriteria := _searchcriteria;
    _index:=_FrmPathSelection.cbxSearchLocation.Items.IndexOf(_FrmPathSelection.SearchLocation);
    if _index=-1 then begin  // its not already in the list.
      _FrmPathSelection.cbxSearchLocation.Items.Insert(0,_FrmPathSelection.SearchLocation);
      _FrmPathSelection.cbxSearchLocation.ItemIndex:=0;
    end
    else _FrmPathSelection.cbxSearchLocation.ItemIndex:=_index; // this entry is already in the list.

    _FrmPathSelection.edtSearchMask.Text := _searchcriteria;
    _FrmPathSelection.btnFind.Caption:=format('Search <%s>',[_searchcriteria]);
    _FrmPathSelection.showmodal;
    if _FrmPathSelection.lstFiles.itemindex > -1 then result := _FrmPathSelection.lstFiles.Items[_FrmPathSelection.lstFiles.itemindex];
  finally
    _FrmPathSelection.free;
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
resourcestring
cPleaseEnterSearch='Please enter a search criteria.';
var
  _fileext: string;
  _mask: string;
  _path:string;
begin
  if edtSearchMask.Text='' then begin
    Application.MessageBox(pchar(cPleaseEnterSearch),pchar(cInformation),MB_ICONINFORMATION or MB_OK);
    edtSearchMask.SetFocus;
    exit;
  end;
  AbortScan := false;
  btnOk.Enabled := false;
  btnAbort.Enabled:=true;
  Screen.cursor := crHourGlass;
  try
    lstFiles.Clear;
    _mask := edtSearchMask.Text;
    _fileext := lowercase(ExtractFileExt(_Mask));
    _path:=IncludeTrailingPathDelimiter(cbxSearchLocation.Text);
    if _fileext = '.dcu' then begin
      _Mask := ChangeFileExt(_Mask, '.pas');
      AllFilesOfDrive(_path, _Mask, lstFiles.Items, AbortScan);
    end;
    AllFilesOfDrive(_path, edtSearchMask.Text, lstFiles.Items, AbortScan);
    if lstFiles.Count>0 then lstFiles.ItemIndex:=0;
  finally
    Screen.cursor := crDefault;
    btnOk.enabled := true;
    btnAbort.Enabled:=false;
    btnAddPath.Enabled:=(lstFiles.Count>0);
  end;
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
  AbortScan := false;
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
  AbortScan := true;
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
  AbortScan := true;
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
  AbortScan := true;
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
  actAddPathExecute(nil);
  btnOk.Click;
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
  _Dir := cbxSearchLocation.text;
  if (_dir<>'') and (not SysUtils.DirectoryExists(_dir)) then _dir:='';
  if not SelectDirectory('Select Search-Path','',_Dir) then exit;
  _Dir:=lowercase(IncludeTrailingPathDelimiter(_Dir));
  AddDirToCombobox(_Dir);
end;

procedure TFrmPathSelection.AddDirToCombobox(_dir:string);
var
i:integer;
_index:integer;
begin
  _index:=cbxSearchLocation.Items.IndexOf(_Dir);
  if _index=-1 then begin  // its not already in the list.
    cbxSearchLocation.Items.Insert(0,_Dir);
    cbxSearchLocation.ItemIndex:=0;
    for i:=1 to cbxSearchLocation.Items.count do DMMain.ApplicationSettings.SetString(format('Application/SearchPathHistory/Item%d',[i]),100+i,cbxSearchLocation.Items[i-1]);
  end
  else cbxSearchLocation.ItemIndex:=_index; // this entry is already in the list.
  DMMain.ApplicationSettings.SetString('Application/LastUsedSearchPath',15,cbxSearchLocation.text);
end;

procedure TFrmPathSelection.edtSearchMaskExit(Sender: TObject);
begin
  btnFind.Caption:=format('Search <%s>',[edtSearchMask.text]);
end;

procedure TFrmPathSelection.actShowFileExecute(Sender: TObject);
begin
  DMMain.ShowFile(lstFiles.Items[lstFiles.ItemIndex],0);
end;

procedure TFrmPathSelection.actAddPathExecute(Sender: TObject);
resourcestring
cAddedDirectoryToSearchPath='Added directory <%s> to the search path.';
var
  _FrmOptions: TFrmOptions;
  _path: string;
begin
  if lstFiles.ItemIndex<0 then exit;
  _FrmOptions := TFrmOptions.create(nil);
  try
    _path := lowercase(ExtractFilePath(lstFiles.Items[lstFiles.ItemIndex]));
    _path := RelativePath(DMMain.BPGPath,_path,DMMain.CurrentDelphiVersion);
    if trim(_path)='' then exit;
    if not _FrmOptions.AddPath(_path) then exit;
    _FrmOptions.SaveSearchPathData;
    if not DMMain.ApplicationSettings.BoolValue('Application/SilentMode',5) then Application.MessageBox(pchar(format(cAddedDirectoryToSearchPath,[_path])),pchar(cInformation),MB_ICONINFORMATION or MB_OK);
  finally
    _FrmOptions.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: FormShow
  Author:    herzogs2
  Date:      11-Jul-2008
  Arguments: Sender: TObject
  Result:    None
  Description: Fill history list.
-----------------------------------------------------------------------------}
procedure TFrmPathSelection.FormShow(Sender: TObject);
var
i:integer;
_item:string;
begin
  for i:=1 to 10 do begin
    _item:=DMMain.ApplicationSettings.StringValue(format('Application/SearchPathHistory/Item%d',[i]),100+i);
    if (_item<>'') and
       (cbxSearchLocation.Items.IndexOf(_item)=-1) then cbxSearchLocation.Items.add(_item);
  end;  
end;

procedure TFrmPathSelection.cbxSearchLocationExit(Sender: TObject);
var
_Dir:string;
begin
  _Dir:=lowercase(IncludeTrailingPathDelimiter(cbxSearchLocation.text));
  AddDirToCombobox(_Dir);
end;

end.

