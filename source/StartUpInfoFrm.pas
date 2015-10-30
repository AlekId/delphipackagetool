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
    with _FrmStartUpInfo do begin
      cbxDontShow.checked := not _shownexttimeagain;
      mmoInfoText.Lines.LoadFromFile(ExtractFilePath(Application.exename) +cReadmeFile);
      ShowModal;
      _shownexttimeagain := not cbxDontShow.checked;
    end;
  finally
    _FrmStartUpInfo.Free;
  end;
end;

end.

