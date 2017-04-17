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
  SysUtils,
  Classes,
  Controls,
  Forms,
  CheckLst,
  ExtCtrls,
  Buttons,
  Menus,
  StdCtrls;

type
  TFrmSelectFiles = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    lbxFiles: TCheckListBox;
    btnDoAction: TBitBtn;
    btnCancel: TBitBtn;
    pmnMain: TPopupMenu;
    itmSelectFolder: TMenuItem;
    itmSelectFile: TMenuItem;
    itmDeSelectFolder: TMenuItem;
    procedure itmSelectFolderClick(Sender: TObject);
    procedure itmSelectFileClick(Sender: TObject);
    procedure itmDeSelectFolderClick(Sender: TObject);
    procedure lbxFilesClick(Sender: TObject);
    procedure btnDoActionClick(Sender: TObject);
  private
    procedure MarkItems(_SetChecked:boolean);
    procedure UpdateActionButton;
    function GetCheckedCount:integer;
  public
    Silent:Boolean;
  end;

function SelectFilesDlg(const _DialogCaption,_ButtonCaption:string;const _FilesToDisplay:TStringList;const _Silent:boolean;out SelectedFiles:TStringList):boolean;

implementation
uses
  uDPTDefinitions;

{$R *.dfm}

{-----------------------------------------------------------------------------
  Procedure: SelectFilesDlg
  Author:    sam
  Date:      19-Mai-2016
  Arguments: const _DialogCaption,_ButtonCaption:string;const _FilesToDisplay:TStringList;out SelectedFiles:TStringList
  Result:    boolean
  Description: show this generic items-selection dialog.
-----------------------------------------------------------------------------}
function SelectFilesDlg(const _DialogCaption,_ButtonCaption:string;const _FilesToDisplay:TStringList;const _Silent:boolean;out SelectedFiles:TStringList):boolean;
var
i:integer;
_FrmSelectFiles: TFrmSelectFiles;
begin
  result:=false;
  _FrmSelectFiles:=TFrmSelectFiles.create(nil);
  SelectedFiles:=TStringList.create;
  try
    _FrmSelectFiles.Caption:=_DialogCaption;
    _FrmSelectFiles.Silent:=_Silent;
    _FrmSelectFiles.btnDoAction.caption:=_ButtonCaption;
    _FrmSelectFiles.btnDoAction.Width:=length(_ButtonCaption)*10;
    _FrmSelectFiles.btnDoAction.Left:=_FrmSelectFiles.btnCancel.left-16-_FrmSelectFiles.btnDoAction.Width;
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

{-----------------------------------------------------------------------------
  Procedure: itmSelectFolderClick
  Author:    sam
  Date:      19-Mai-2016
  Arguments: Sender: TObject
  Result:    None
  Description: select all files in this folder
-----------------------------------------------------------------------------}
procedure TFrmSelectFiles.itmSelectFolderClick(Sender: TObject);
begin
  MarkItems(true);
end;

{-----------------------------------------------------------------------------
  Procedure: itmDeSelectFolderClick
  Author:    sam
  Date:      19-Mai-2016
  Arguments: Sender: TObject
  Result:    None
  Description: un-mark all files in this folder.
-----------------------------------------------------------------------------}
procedure TFrmSelectFiles.itmDeSelectFolderClick(Sender: TObject);
begin
  MarkItems(false);
end;

{-----------------------------------------------------------------------------
  Procedure: itmSelectFileClick
  Author:    sam
  Date:      19-Mai-2016
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmSelectFiles.itmSelectFileClick(Sender: TObject);
begin
  lbxFiles.Checked[lbxFiles.ItemIndex]:=true;
  UpdateActionButton;
end;

{-----------------------------------------------------------------------------
  Procedure: MarkItems
  Author:    sam
  Date:      19-Mai-2016
  Arguments: _SetChecked:boolean
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmSelectFiles.MarkItems(_SetChecked:boolean);
var
i:integer;
_filename:string;
_path:string;
begin
  if lbxFiles.ItemIndex=-1 then exit;
  _filename:=lbxFiles.items[lbxFiles.ItemIndex];
  _path:=extractFilePath(_filename);
  for i:=0 to lbxFiles.count-1 do begin
    if extractFilePath(lbxFiles.items[i])<>_path then continue;
    lbxFiles.Checked[i]:=_SetChecked;
  end;
  UpdateActionButton;
end;

{-----------------------------------------------------------------------------
  Procedure: GetCheckedCount
  Author:    sam
  Date:      19-Mai-2016
  Arguments: None
  Result:    integer
  Description: get the number of checked items.
-----------------------------------------------------------------------------}
function TFrmSelectFiles.GetCheckedCount:integer;
var
i:integer;
begin
  result:=0;
  for i:=0 to lbxFiles.count-1 do begin
    if lbxFiles.Checked[i]then inc(result);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: UpdateActionButton
  Author:    sam
  Date:      19-Mai-2016
  Arguments: None
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmSelectFiles.UpdateActionButton;
begin
  btnDoAction.Enabled:=(GetCheckedCount>0);
end;

procedure TFrmSelectFiles.lbxFilesClick(Sender: TObject);
begin
   UpdateActionButton;
end;

{-----------------------------------------------------------------------------
  Procedure: btnDoActionClick
  Author:    sam
  Date:      19-Mai-2016
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmSelectFiles.btnDoActionClick(Sender: TObject);
resourcestring
cAskToDeleteFiles='This will delete the files. Continue?';
begin
  if not Silent then begin            // if we are not in silent-mode, then ask the user before deleting the file.
    if Application.MessageBox(pchar(cAskToDeleteFiles),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDNo then begin
      ModalResult:=mrNo;
      exit;
    end;
  end;
  ModalResult:=mrOK;
end;

end.

