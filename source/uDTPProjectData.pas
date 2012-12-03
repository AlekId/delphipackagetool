{-----------------------------------------------------------------------------
 Unit Name: uDTPProjectData
 Author:    muem
 Purpose:   class to store project data
 History:
-----------------------------------------------------------------------------}
unit uDTPProjectData;

interface

uses
  classes;

const
  ProjectDataDelimiter = ': ';

type
  TProjectData = class
  private
  public
    OutputFilename: string;
    OutputPath: string;
    BPLOutputPath: string;
    DCPOutputPath: string;
    DCUOutputPath: string;
    SearchPath: string;
    DPTSearchPath: string;
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
