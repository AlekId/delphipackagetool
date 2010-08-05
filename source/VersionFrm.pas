unit VersionFrm;

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
  Buttons,
  ExtCtrls,
  Spin;

type
  TFrmVersion = class(TForm)
    pnlClient: TPanel;
    pnlBottom: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    edtMajor: TSpinEdit;
    edtMinor: TSpinEdit;
    edtRelease: TSpinEdit;
    edtBuild: TSpinEdit;
    lblMajor: TLabel;
    lblMinor: TLabel;
    lblRelease: TLabel;
    lblBuild: TLabel;
  private
  public
  end;

function ShowVersionDlg(_filename:string;var Major,Minor,Release,Build:integer):boolean;

implementation

{$R *.dfm}

function ShowVersionDlg(_filename:string;var Major,Minor,Release,Build:integer):boolean;
resourcestring
cSetVersion='Set Version of File <%s>.';
var
_FrmVersion: TFrmVersion;
begin
  _FrmVersion:=TFrmVersion.create(nil);
  try
    _FrmVersion.caption:=format(cSetVersion,[extractfilename(_filename)]);
    _FrmVersion.edtMajor.Value:=Major;
    _FrmVersion.edtMinor.value:=Minor;
    _FrmVersion.edtRelease.value:=Release;
    _FrmVersion.edtBuild.value:=Build;
    result:=(_FrmVersion.showmodal=mrOk);
    if not result then exit;
    Major:=_FrmVersion.edtMajor.Value;
    Minor:=_FrmVersion.edtMinor.value;
    Release:=_FrmVersion.edtRelease.value;
    Build:=_FrmVersion.edtBuild.value;
  finally
    _FrmVersion.free;
  end;
end;


end.
