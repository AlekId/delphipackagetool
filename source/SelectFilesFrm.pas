{*-----------------------------------------------------------------------------
 Unit Name: SelectFilesFrm
 Author:    sam
 Date:      18-Mai-2016
 Purpose:   display a list of files and let the user select some of them.
 History:
-----------------------------------------------------------------------------}
unit SelectFilesFrm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  CheckLst,
  ExtCtrls,
  Buttons;

type
  TFrmSelectFiles = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    lbxFiles: TCheckListBox;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
  private
  public
  end;

function SelectFilesDlg(const _FilesToDisplay:TStringList;out SelectedFiles:TStringList):boolean;

implementation

{$R *.dfm}

function SelectFilesDlg(const _FilesToDisplay:TStringList;out SelectedFiles:TStringList):boolean;
var
i:integer;
_FrmSelectFiles: TFrmSelectFiles;
begin
  result:=false;
  _FrmSelectFiles:=TFrmSelectFiles.create(nil);
  SelectedFiles:=TStringList.create;
  try
    _FrmSelectFiles.lbxFiles.Items.Assign(_FilesToDisplay);
    if _FrmSelectFiles.ShowModal<>mrOk then exit;
    for i:=0 to _FrmSelectFiles.lbxFiles.count-1 do begin
      if not _FrmSelectFiles.lbxFiles.Checked[i] then continue;
      SelectedFiles.Add(_FrmSelectFiles.lbxFiles.Items[i]);
    end;
    result:=true;
  finally
    _FrmSelectFiles.free;
  end;
end;

end.

