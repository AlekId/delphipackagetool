{-----------------------------------------------------------------------------
 Unit Name: BPGEditorFrm
 Author:    sam
 Date:      12-Mrz-2005
 Purpose:
 History:   15.04.2006 - better setup of savedialog component.
            16.12.2005 - avoid doubled items.
            28.06.2005 - fix in save as method. The search path file <.txt> was not
                        copied.

-----------------------------------------------------------------------------}
unit BPGEditorFrm;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  uDPTDblList,
  ExtCtrls,
  uDPTSplitter,
  Buttons;

type

  TFrmBPGEditor = class(TForm)
    pnlLeft: TPanel;
    pnlBottom: TPanel;
    pnlRight: TPanel;
    lstAllFiles: TListBox;
    lstBPGFiles: TListBox;
    btnSave: TBitBtn;
    btnClose: TBitBtn;
    btnFind: TBitBtn;
    btnAbort: TBitBtn;
    Label1: TLabel;
    btnSaveAs: TBitBtn;
    pnlAllFiles: TPanel;
    edtFilter: TEdit;
    lblAllFiles: TLabel;
    lblFilter: TLabel;
    SaveDialog1: TSaveDialog;
    procedure btnFindClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    function NVBDblList1CheckItem(Sender: TObject; const _ItemText: String;const _Operation: TNVBOperationType): Boolean;
    procedure FormCreate(Sender: TObject);
  private
    FItems:TStrings;
    FAbortScan: boolean;
    FProjectGroupFilename: string;
    procedure FilterAvailableFileList(_filter:string);
  public
    NVBSplitter1: TNVBSplitter;
    NVBDblList1: TNVBDblList;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  end;

function ShowBPGEditor(const _ProjectGroupFilename: string):string;

implementation

uses FileCtrl,
     uDPTDefinitions,
     uDPTDelphiPackage,
     uDPTCreateProjectGroup,
     uDPTMisc,
     MainDM;
{$R *.dfm}

{-----------------------------------------------------------------------------
  Procedure: ShowBPGEditor
  Author:    sam
  Date:      12-Mrz-2005
  Arguments: _BPGFilename: string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
function ShowBPGEditor(const _ProjectGroupFilename: string):string;
resourcestring
cEditFile='Edit file <%s>.';
var
  _FrmBPGEditor: TFrmBPGEditor;
begin
  result:='';
  _FrmBPGEditor := TFrmBPGEditor.create(nil);
  try
    _FrmBPGEditor.FProjectGroupFilename := _ProjectGroupFilename;
    _FrmBPGEditor.caption := format(cEditFile, [_ProjectGroupFilename]);
    ReadPackageListfromFile(_ProjectGroupFilename, _FrmBPGEditor.lstBPGFiles);
    _FrmBPGEditor.showmodal;
    result:=_FrmBPGEditor.FProjectGroupFilename;
  finally
    _FrmBPGEditor.free
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: btnFindClick
  Author:    sam
  Date:      12-Mrz-2005
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmBPGEditor.btnFindClick(Sender: TObject);
var
  _Dir: string;
begin
  _Dir := DMMain.ApplicationSettings.StringValue('Application/LastUsedSearchPath');
  if not SysUtils.DirectoryExists(_Dir) then _Dir:='';
  FAbortScan := false;
  if not SelectDirectory('Select Search-Path','',_Dir) then exit;
  DMMain.ApplicationSettings.SetString('Application/LastUsedSearchPath',_Dir);
  lstAllFiles.Clear;
  FItems.Clear;
  btnAbort.Enabled := true;
  btnFind.Enabled := false;
  screen.Cursor := crHourGlass;
  try
    AllFilesOfDrive(_dir + '\', '*.dpk', FItems, FAbortScan);
    AllFilesOfDrive(_dir + '\', '*.dpr', FItems, FAbortScan);    
    AllFilesOfDrive(_dir + '\', '*.dproj', FItems, FAbortScan);
    AllFilesOfDrive(_dir + '\', '*.bdsproj', FItems, FAbortScan);    
    FilterAvailableFileList(edtFilter.Text);
  finally
    NVBDblList1.SetButtons;
    btnAbort.Enabled := false;
    btnFind.Enabled := true;
    screen.Cursor := crDefault;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: btnSaveClick
  Author:    sam
  Date:      12-Mrz-2005
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmBPGEditor.btnSaveClick(Sender: TObject);
begin
  CreateProjectGroupFile(lstBPGFiles, FProjectGroupFilename,DMMain.DelphiVersion);
  DMMain.SetLastUsedBPGFile(FProjectGroupFilename);
end;

{-----------------------------------------------------------------------------
  Procedure: btnAbortClick
  Author:    sam
  Date:      12-Mrz-2005
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmBPGEditor.btnAbortClick(Sender: TObject);
begin
  FAbortScan := true;
  btnFind.Enabled:=true;
  btnAbort.Enabled:=false;
end;

{-----------------------------------------------------------------------------
  Procedure: btnSaveAsClick
  Author:    sam
  Date:      12-Mrz-2005
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmBPGEditor.btnSaveAsClick(Sender: TObject);
resourcestring
  cPleaseDefineFilename='Please define a filename for the Package Group file <%s>.';
  cDelphiPackageGroupBPG='Delphi Package Group(%s)|*%s';
  cNewPackageGroup='NewPackageGroup';
  cTheFileAlreadyExistsOverwrite='The file <%s> already exists. Do you want to overwrite it ?';
  cProblemToCopyFile='Problem to copy file <%s> to <%s>. Please check if the file is not in use and you have enough access rights. <%s>.';
var
_currentPathFilename:string;
_newPathFilename:string;
_ext:string;
begin
  _ext:=ExtractFileExt(FProjectGroupFilename);
  SaveDialog1.Title:=format(cPleaseDefineFilename,[_ext]);
  SaveDialog1.InitialDir:=DMMain.ApplicationSettings.FileValue('Application/ProjectGroupFile');
  SaveDialog1.DefaultExt := _ext;
  SaveDialog1.Filter     := format(cDelphiPackageGroupBPG,[_ext,_ext]);
  SaveDialog1.FileName   := cNewPackageGroup;
  SaveDialog1.FilterIndex := 0;
  if not SaveDialog1.Execute then exit;
  _currentPathFilename:=changefileext(FProjectGroupFilename,'.txt');
  _newPathFilename:= changefileext(SaveDialog1.FileName,'.txt');
  if fileexists(_currentPathFilename) then  begin
    if fileexists(_newPathFilename) then begin
      if Application.MessageBox(PChar(format(cTheFileAlreadyExistsOverwrite,[_newPathFilename])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDNo then exit;
      DeleteFile(_newPathFilename);
    end;
    try
      copyFile(PChar(_currentPathFilename),PChar(_newPathFilename),true);
    except
      on e:exception do Application.MessageBox(PChar(format(cProblemToCopyFile,[_currentPathFilename,_newPathFilename,E.Message])),'',MB_ICONERROR or MB_OK);
    end;
  end;
  FProjectGroupFilename:= SaveDialog1.FileName;
  CreateProjectGroupFile(lstBPGFiles, FProjectGroupFilename,DMMain.DelphiVersion);
  DMMain.SetLastUsedBPGFile(FProjectGroupFilename);
end;

{-----------------------------------------------------------------------------
  Procedure: edtFilterChange
  Author:    sam
  Date:      11-Jul-2007
  Arguments: Sender: TObject
  Result:    None
  Description: only show files which match the current filter statement.
-----------------------------------------------------------------------------}
procedure TFrmBPGEditor.edtFilterChange(Sender: TObject);
begin
  FilterAvailableFileList(edtFilter.Text);
end;

constructor TFrmBPGEditor.Create(AOwner: TComponent);
begin
  inherited;
  FItems:=TStringList.create;
end;

{-----------------------------------------------------------------------------
  Procedure: FilterAvailableFileList
  Author:    sam
  Date:      11-Jul-2007
  Arguments: _filter: string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmBPGEditor.FilterAvailableFileList(_filter: string);
var
i:integer;
begin
  _filter:=trim(lowercase(_filter));
  lstAllFiles.Clear;
  for i:=0 to FItems.count-1 do begin
    if (_filter='') or (pos(_filter,lowercase(ExtractFilename(FItems[i])))>0) then  lstAllFiles.Items.add(FItems[i]);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: destroy
  Author:    sam
  Date:      11-Jul-2007
  Arguments: None
  Result:    None
  Description:
-----------------------------------------------------------------------------}
destructor TFrmBPGEditor.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TFrmBPGEditor.NVBDblList1CheckItem(Sender: TObject; const _ItemText: String; const _Operation: TNVBOperationType): Boolean;
var
_relativeName:string;
begin
  _relativeName:=ExtractRelativePath(ExtractFilePath(FProjectGroupFilename),_ItemText);
  result:=(lstBPGFiles.Items.IndexOf(_relativeName)=-1);
end;

{-----------------------------------------------------------------------------
  Procedure: FormCreate
  Author:    S.Herzog
  Date:      24-Feb-2010
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}
procedure TFrmBPGEditor.FormCreate(Sender: TObject);
begin
  NVBSplitter1 := TNVBSplitter.Create(Self);
  NVBDblList1 := TNVBDblList.Create(Self);
  with NVBSplitter1 do
  begin
    Name := 'NVBSplitter1';
    Parent := Self;
    Left := 425;
    Top := 0;
    Width := 55;
    Height := 424;
    Cursor := crHSplit;
  end;
  with NVBDblList1 do
  begin
    Name := 'NVBDblList1';
    Parent := NVBSplitter1;
    Left := 0;
    Top := 72;
    Width := 56;
    Height := 160;
    TabOrder := 0;
    Caption:='';
    OnCheckItem := NVBDblList1CheckItem;
    SourceList := lstAllFiles;
    SourceOptions := [];
    TargetList := lstBPGFiles;
    TargetOptions := [AllowAdd, AllowRemove];
  end;
end;

end.

