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
  AboutFrm in 'AboutFrm.pas' {frmAbout},
  uDPTJclFuncs in 'uDPTJclFuncs.pas',
  uDPTSettings in 'uDPTSettings.pas',
  uDPTMisc in 'uDPTMisc.pas',
  VersionFrm in 'VersionFrm.pas' {FrmVersion},
  uDPTCreateProjectGroup in 'uDPTCreateProjectGroup.pas',
  uDPTPathFilenameConvert in 'uDPTPathFilenameConvert.pas',
  uDTPProjectData in 'uDTPProjectData.pas',
  ProjectOptionsFrm in 'ProjectOptionsFrm.pas' {FrmProjectOptions},
  uDPTDblList in 'uDPTDblList.pas',
  uDPTDelphiPackage in 'uDPTDelphiPackage.pas';

{$R *.res}

var
  _showagain:boolean;
  _CreateMainForm:boolean;
begin
  _CreateMainForm:=true;
  Application.Initialize;
  Application.Title := 'Delphi Package Tool';
  Application.HintHidePause:=10000;
  Application.CreateForm(TDMMain, DMMain);
  if not DMMain.IsSilentMode then begin
    // show startup dialog
    _showagain:=DMMain.ApplicationSettings.BoolValue('Application/ShowStartUpWarning', 10);
    if _showagain then begin
      ShowStartUpDlg(_showagain);
      DMMain.ApplicationSettings.SetBoolean('Application/ShowStartUpWarning', 10,_showagain);
    end;
  end;
// load the bpg file
  ActivateApplication;

// if dpt is used with command line
  if assigned(DMMain.CommandLineAction) then begin
    DMMain.CommandLineAction.Execute;
    _CreateMainForm:=exitcode<>0;
  end;
// show main gui.
  if _CreateMainForm then begin
    if DMMain.BPGFilename='' then DMMain.BPGFilename:=DMMain.ApplicationSettings.FileValue('Application/ProjectGroupFile', 3);
    DMMain.LoadBPG(DMMain.BPGFilename);
    Application.CreateForm(TFrmMain, FrmMain);
    FrmMain.PrepareGUI(DMMain.BPGFilename);
  end;
  
  Application.Run;
end.
