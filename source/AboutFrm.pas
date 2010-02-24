{-----------------------------------------------------------------------------
 Unit Name: AboutFrm
 Author:    Samuel Herzog
 Purpose: 
 History:  - SH 31.10.2002 -added application date.
           - added homepage support
           - SH 01.12.2002 - the homepage link is now underlined and the mouse cursor
                             shows a hand.
           - SH 06.06.2003 - changed params of showmodal.
-----------------------------------------------------------------------------}
unit AboutFrm;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  ShellApi,
  Forms,
  StdCtrls,
  Buttons,
  ExtCtrls;

type
  TFrmAbout = class(TForm)
    Panel1: TPanel;
    pnlBottom: TPanel;
    btnClose: TBitBtn;
    lblApplicationName: TLabel;
    lblVersion: TLabel;
    lblCompany: TLabel;
    lblVersionValue: TLabel;
    lblApplicationValue: TLabel;
    lblCompanyValue: TLabel;
    lblBuildDate: TLabel;
    lblBuildValue: TLabel;
    Image1: TImage;
    mmoCredits: TMemo;
    lblHomepage: TLabel;
    lblHomepageValue: TLabel;
    procedure lblHomepageValueClick(Sender: TObject);
  private
  public    
    class procedure ShowDialog(_Caption,_Company,_Homepage:string);
  end;

implementation
uses
  uDPTMisc;

{$R *.dfm}

{ TfrmAbout }

{-----------------------------------------------------------------------------
  Procedure: TFrmAbout.ShowModal
  Author:    Not available
  Date:      15-Sep-2002
  Arguments: _Caption,_ApplicationName: string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
class procedure TFrmAbout.ShowDialog(_Caption,_Company,_Homepage: string);
var
_FileDateTime:TDateTime;
_Form:TFrmAbout;
begin
  _Form:=TFrmAbout.create(nil);
  try
    with _Form do begin
      Caption:=_Caption;
      lblApplicationValue.Caption:=Application.Title;
      lblVersionValue.Caption:=GetVersion;
      if GetFileDateTime(application.ExeName,_FileDateTime) then lblBuildValue.Caption:=DateToStr(_FileDateTime);
      lblHomepageValue.Caption:=_Homepage;
      lblCompanyValue.Caption:=_Company;
      Image1.Picture.Icon.Assign(Application.Icon);
    end;
    _Form.ShowModal;
  finally
    _Form.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmAbout.lblHomepageValueClick
  Author:    Not available
  Date:      31-Okt-2002
  Arguments: Sender: TObject
  Result:    None
  Description: opens web browser
-----------------------------------------------------------------------------}
procedure TFrmAbout.lblHomepageValueClick(Sender: TObject);
begin
  if not (Sender is TLabel) then exit;
  if TLabel(Sender).Caption='-' then exit;
  with (Sender as Tlabel) do begin
    ShellExecute(Application.Handle,
           PChar('open'),
           PChar(Caption),
           PChar(0),
           nil,
           SW_NORMAL);
  end;
end;

end.
