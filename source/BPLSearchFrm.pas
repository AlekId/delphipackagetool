{-----------------------------------------------------------------------------
 Unit Name: PathSelectionFrm
 Author:    Samuel Herzog
 Purpose:   search the drive for certain files
 History:
-----------------------------------------------------------------------------}
unit BPLSearchFrm;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  Buttons,
  ExtCtrls;

type
  TFrmBPLSearch = class(TForm)
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
    btnDeleteFile: TBitBtn;
    btnSelectPath: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lstFilesDblClick(Sender: TObject);
    procedure btnDeleteFileClick(Sender: TObject);
    procedure btnSelectPathClick(Sender: TObject);
  private
    FAbortScan: boolean;
    FSearchCriteria: string;
    FSearchLocation: string;
  public

  end;

procedure ShowBPLSearchDialog(_searchLocation, _searchcriteria: string);  // main method of this unit.


implementation

uses
  uDPTDefinitions,
  uDPTMisc,
  FileCtrl,
  MainDM,
  Windows;

{$R *.dfm}

{-----------------------------------------------------------------------------
  Procedure: ShowBPLSearchDialog
  Author:    Not available
  Date:      24-Dez-2003
  Arguments: _searchLocation, _searchcriteria: string
  Result:    string
  Description: Main function of this file. Display the dialog.
-----------------------------------------------------------------------------}
procedure ShowBPLSearchDialog(_searchLocation, _searchcriteria: string);
resourcestring
cSearchPackageFiles='Search .dcp and .bpl files for Package <%s>.';
cSearchFor='Search <%s>';
var
  _FrmBPLSearch: TFrmBPLSearch;
begin
  _FrmBPLSearch := TFrmBPLSearch.create(nil);
  try
    _FrmBPLSearch.caption:=format(cSearchPackageFiles,[_searchcriteria]);
    _FrmBPLSearch.FSearchLocation := _searchLocation;
    _FrmBPLSearch.FSearchCriteria := _searchcriteria;
    _FrmBPLSearch.edtSearchLocation.Text := _searchLocation;
    _FrmBPLSearch.edtSearchMask.Text := ExtractFilenameOnly(_searchcriteria) ;
    _FrmBPLSearch.btnFind.Caption:=format(cSearchFor,[_searchcriteria]);
    _FrmBPLSearch.showmodal;
  finally
    _FrmBPLSearch.free;
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
procedure TFrmBPLSearch.btnFindClick(Sender: TObject);
begin
  FAbortScan := false;
  btnOk.Enabled := false;
  btnAbort.Enabled:=true;
  Screen.cursor := crHourGlass;
  lstFiles.Clear;
  AllFilesOfDrive(edtSearchLocation.Text, edtSearchMask.Text+'.dcp', lstFiles.Items, FAbortScan);
  AllFilesOfDrive(edtSearchLocation.Text, edtSearchMask.Text+'.bpl', lstFiles.Items, FAbortScan);
  Screen.cursor := crDefault;
  btnOk.enabled := true;
  btnAbort.Enabled:=false;
  btnDeleteFile.Enabled:=(lstFiles.Count>0);
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmPathSelection.FormCreate
  Author:    Not available
  Date:      24-Dez-2003
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmBPLSearch.FormCreate(Sender: TObject);
begin
  FAbortScan := false;
  btnAbort.Enabled:=false;
  btnDeleteFile.Enabled:=false;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmPathSelection.btnAbortClick
  Author:    Not available
  Date:      24-Dez-2003
  Arguments: Sender: TObject
  Result:    None
  Description: interrupt the searching of the harddisk
-----------------------------------------------------------------------------}
procedure TFrmBPLSearch.btnAbortClick(Sender: TObject);
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
procedure TFrmBPLSearch.btnCancelClick(Sender: TObject);
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
procedure TFrmBPLSearch.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
procedure TFrmBPLSearch.lstFilesDblClick(Sender: TObject);
begin
  DMMain.ShowFile(lstFiles.Items[lstFiles.ItemIndex],0);
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmPathSelection.btnDeleteFileClick
  Author:    Not available
  Date:      24-Dez-2003
  Arguments: Sender: TObject
  Result:    None
  Description: add the currently selected path to the search path of the project.
-----------------------------------------------------------------------------}
procedure TFrmBPLSearch.btnDeleteFileClick(Sender: TObject);
resourcestring
cDeletedFile='Deleted the file <%s>.';
cFileCouldnotBeDeleted='The file <%s> could not be deleted. It may be in use or you have not enough User-Rights to delete the file.';
var
i:integer;
_filename: string;
begin
  i:=0;
  while i<lstFiles.Count do begin
    inc(i);
    if not lstFiles.Selected[i-1] then continue;
    _filename:=lstFiles.Items[i-1];
    if not fileexists(_filename) then continue;
    if not SysUtils.DeleteFile(_filename) then begin
      Application.MessageBox(pchar(format(cFileCouldnotBeDeleted,[_filename])),pchar(cInformation),MB_ICONINFORMATION or MB_OK);
      continue;
    end;
    lstFiles.Selected[i-1]:=false;
    lstFiles.Items.Delete(i-1);
    dec(i);
    if not DMMain.IsSilentMode then Application.MessageBox(pchar(format(cDeletedFile,[_filename])),pchar(cInformation),MB_ICONINFORMATION or MB_OK);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: btnSelectPathClick
  Author:    sam
  Date:      19-Apr-2004
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}
procedure TFrmBPLSearch.btnSelectPathClick(Sender: TObject);
resourcestring
cSelectPath='Select Path to search for BPL files.';
var
  _Dir: string;
begin
  _Dir := '';
  if not SelectDirectory(cSelectPath,'',_Dir) then exit;
  _Dir:=IncludeTrailingPathDelimiter(_Dir);
  edtSearchLocation.Text:=_Dir;
  DMMain.ApplicationSettings.SetString('Application/LastUsedSearchPath',_Dir);
end;

end.

