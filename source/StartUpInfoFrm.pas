{*-----------------------------------------------------------------------------
 Unit Name: StartUpInfoFrm
 Author:    sam
 Date:      14-Apr-2017
 Purpose:   show some info/read-me text on the first start-up of this tool
 History:
-----------------------------------------------------------------------------}
unit StartUpInfoFrm;

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
  TFrmStartUpInfo = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    mmoInfoText: TMemo;
    btnOk: TBitBtn;
    cbxDontShow: TCheckBox;
  private
  public
  end;

procedure ShowStartUpDlg(var _shownexttimeagain: boolean);

implementation
{$R *.dfm}

const
  cReadmeFile = 'Readme.txt';
  cHistoryFile = 'Readme.txt';

procedure ShowStartUpDlg(var _shownexttimeagain: boolean);
var
  _FrmStartUpInfo: TFrmStartUpInfo;
begin
  if not fileexists(ExtractFilePath(Application.exename) + cReadmeFile) then exit;
  _FrmStartUpInfo := TFrmStartUpInfo.create(nil);
  try
    _FrmStartUpInfo.cbxDontShow.checked := true;
    _FrmStartUpInfo.mmoInfoText.Lines.LoadFromFile(ExtractFilePath(Application.exename) +cReadmeFile);
    _FrmStartUpInfo.ShowModal;
    _shownexttimeagain := not _FrmStartUpInfo.cbxDontShow.checked;
  finally
    _FrmStartUpInfo.Free;
  end;
end;

end.

