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
    btnHistory: TBitBtn;
    procedure btnHistoryClick(Sender: TObject);
  private
  public
  end;

procedure ShowStartUpDlg(var _shownexttimeagain: boolean);

implementation
{$R *.dfm}

const
  cReadmeFile = 'Readme.txt';
  cHistoryFile = 'History.txt';

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
      btnHistory.enabled := FileExists(cHistoryFile);
      ShowModal;
      _shownexttimeagain := not cbxDontShow.checked;
    end;
  finally
    _FrmStartUpInfo.Free;
  end;
end;

procedure TFrmStartUpInfo.btnHistoryClick(Sender: TObject);
begin
  if not fileexists(ExtractFilePath(Application.exename)+cHistoryFile) then exit;
  mmoInfoText.Lines.LoadFromFile(ExtractFilePath(Application.exename)+cHistoryFile);
end;

end.

