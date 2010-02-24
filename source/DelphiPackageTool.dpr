program DelphiPackageTool;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {FrmMain},
  OptionsFrm in 'OptionsFrm.pas' {FrmOptions},
  StartUpInfoFrm in 'StartUpInfoFrm.pas' {FrmStartUpInfo},
  BPGEditorFrm in 'BPGEditorFrm.pas' {FrmBPGEditor},
  BPLSearchFrm in 'BPLSearchFrm.pas' {FrmBPLSearch},
  PathSelectionFrm in 'PathSelectionFrm.pas' {FrmPathSelection},
  RemovePackagesQuestionFrm in 'RemovePackagesQuestionFrm.pas' {FrmRemovePackage},
  MainDM in 'MainDM.pas' {DMMain: TDataModule},
  uDPTDefinitions in 'uDPTDefinitions.pas',
  AboutFrm in 'AboutFrm.pas' {frmAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Delphi Package Tool';
  Application.HintHidePause:=4000;
  Application.CreateForm(TDMMain, DMMain);
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
