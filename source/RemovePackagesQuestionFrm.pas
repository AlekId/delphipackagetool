{-----------------------------------------------------------------------------
 Unit Name: RemovePackagesQuestionFrm
 Author:    sam
 Date:      04-Jul-2006
 Purpose:   Dialog to ask the user which packages shall be removed.
 History:
-----------------------------------------------------------------------------}

unit RemovePackagesQuestionFrm;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  Buttons,
  uDPTDefinitions,
  ExtCtrls;

type


  TFrmRemovePackage = class(TForm)
    pnlBottom: TPanel;
    pnlClient: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    rbtAllThirdParty: TRadioButton;
    rbtAllProjectsBPL: TRadioButton;
    cbxDeleteBPLAndDCPFiles: TCheckBox;
  private
  public
  end;

function ShowRemovePackagesDlg(const _BPLPath:string;var RemoveType:TPackageRemoveType;var DeleteBplAndDCPFiles:boolean) :TModalResult;

implementation

{$R *.dfm}

{-----------------------------------------------------------------------------
  Procedure: ShowRemovePackagesDlg
  Author:    sam
  Date:      05-Jul-2006
  Arguments: var RemoveType:TPackageRemoveTypevar DeleteBplAndDCPFiles:boolean
  Result:    TModalResult
  Description:
-----------------------------------------------------------------------------}
function ShowRemovePackagesDlg(const _BPLPath:string;var RemoveType:TPackageRemoveType;var DeleteBplAndDCPFiles:boolean) :TModalResult;
var
_FrmRemovePackage: TFrmRemovePackage;
begin
  RemoveType:=tpr_none;
  _FrmRemovePackage:= TFrmRemovePackage.create(nil);
  try
    _FrmRemovePackage.cbxDeleteBPLAndDCPFiles.Checked:=DeleteBplAndDCPFiles;
    RemoveType:=tpr_none;
    _FrmRemovePackage.rbtAllProjectsBPL.Caption:=format('Delete all Packages located in %s',[_BPLPath]);
    _FrmRemovePackage.rbtAllProjectsBPL.Hint:=_BPLPath;
    result:=_FrmRemovePackage.showmodal;
    DeleteBplAndDCPFiles:=_FrmRemovePackage.cbxDeleteBPLAndDCPFiles.Checked;
    if _FrmRemovePackage.rbtAllThirdParty.Checked then RemoveType:=tpr_3rdparty;
    if _FrmRemovePackage.rbtAllProjectsBPL.Checked then RemoveType:=tpr_projectsbpl;
  finally
    _FrmRemovePackage.free;
  end;
end;


end.

