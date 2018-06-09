program Path_And_Filename;

uses
  Vcl.Forms,
  MainFrm in 'MainFrm.pas' {FrmMain},
  uDPTPathFilenameConvert in '..\..\..\uDPTPathFilenameConvert.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
