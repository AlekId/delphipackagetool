{-----------------------------------------------------------------------------
 Unit Name: uDTPProjectData
 Author:    muem
 Purpose:   class to store project data
 History:
-----------------------------------------------------------------------------}
unit uDTPProjectData;

interface

uses
  classes,
  uDPTDefinitions;

const
  ProjectDataDelimiter = ': ';

type
  TProjectData = class
  public
    OutputFilename: string;
    OutputPath: string;
    BPLOutputPath: string;
    DCPOutputPath: string;
    DCUOutputPath: string;
    ProjectSearchPath: string;
    DPTSearchPath: string;
    CompileDate:string;
    CompileState:string;
    IDEInstall:string;
    Description:string;
    Version:string;
    ProjectType:TProjectType;
    CompileResultsList: TStringList;
    VersionsList: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ TProjectData }

constructor TProjectData.Create;
begin
  CompileResultsList := TStringList.Create;
  VersionsList := TStringList.Create;
end;

destructor TProjectData.Destroy;
begin
  VersionsList.Free;
  CompileResultsList.Free;
  inherited;
end;

end.
