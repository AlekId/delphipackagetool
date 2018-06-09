unit MainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,IOUtils;

type
  TFrmMain = class(TForm)
    pnlTop: TPanel;
    mmoLog: TMemo;
    pgcMain: TPageControl;
    tabAbsoluteFilename: TTabSheet;
    edtBasePath_abpf: TLabeledEdit;
    edtRelativeFilename_abpf: TLabeledEdit;
    btnAbsoluteFilename: TButton;
    edtOutput_abpf: TLabeledEdit;
    tabAbsolutePath: TTabSheet;
    edtBasePath_abp: TLabeledEdit;
    edtRelativePath_abp: TLabeledEdit;
    btnAbsolutePath: TButton;
    edtOutput_abp: TLabeledEdit;
    procedure btnAbsoluteFilenameClick(Sender: TObject);
    procedure btnAbsolutePathClick(Sender: TObject);
  private
  public
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses uDPTPathFilenameConvert;


procedure TFrmMain.btnAbsoluteFilenameClick(Sender: TObject);
begin
  edtOutput_abpf.Text:=AbsoluteFilename(edtBasePath_abpf.Text,edtRelativeFilename_abpf.Text);
end;

procedure TFrmMain.btnAbsolutePathClick(Sender: TObject);
begin
  edtOutput_abp.Text:=AbsolutePath(edtBasePath_abp.Text,edtRelativePath_abp.Text,23,'win32','release');
end;

end.
