{*-----------------------------------------------------------------------------
 Unit Name: uDPTDefinitions
 Author:    sam
 Date:      23-Feb-2010
 Purpose:   contains delphi package tool specific definitions.
 History:
-----------------------------------------------------------------------------}

unit uDPTDefinitions;

interface

type
  TApplicationState=(tas_init,
                     tas_open,
                     tas_working);

  TProcessState=(tps_new,
                 tps_changed,
                 tps_unchanged);

  TProcessStates=set of TProcessState;

  TPackageRemoveType=(tpr_none,          // no packages will be remove.
                      tpr_3rdparty,      // all packages except the borland packages in the directory ($DELPHI)\bin
                      tpr_projectsbpl);  // all packages which are in the directory ($DELPHI)\Projects\bpl

  TDelphiVersionInfo = record
    Name: string;
    VersionStr: string;
    IDEVersion: Integer;
    IDEVersionStr: string;
    CoreIdeVersion: string;
    ShortName:string;
    CompilerVersion:double;
    CompilerVersionStr:string;
    Supported: Boolean;
  end;


const
  cDelphiKey='SOFTWARE\BORLAND\DELPHI';
  cBorlandBDSKey='SOFTWARE\BORLAND\BDS';
  cCodeGearBDSKey='SOFTWARE\CODEGEAR\BDS';
  cEmbarcaderoBDSKey='SOFTWARE\EMBARCADERO\BDS\';
  cDelphiTag='$(DELPHI)';
  cBDSTag='$(BDS)';
  cBDSCommonDirTag='$(BDSCOMMONDIR)';
  cBDSProjectsDirTag='$(BDSPROJECTSDIR)';
  cBDSUserDirTag='$(BDSUSERDIR)';
  cProgramFilesTag='$(PROGRAMFILES)';
  cDelphiVersionTag='$(DELPHIVERSION)';
  cRADStudioDirName='RAD Studio';
  cDelphiName = 'Delphi';
  cBCBName    = 'C++Builder';
  cCSharpName = 'C#Builder';
  cBDSName    = 'Borland Developer Studio';
  cRSName     = 'RAD Studio';


  cProjectGroupExtensions='<.bpg/.bdsgroup/.groupproj>';
  cProjectGroupFilter='*.bpg;*.groupproj;*.bdsgroup';

  DelphiVersions: array [1..15] of TDelphiVersionInfo = (
    (
      Name: cDelphiName;           //1
      VersionStr: '1.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '10';
      ShortName: 'D1';
      CompilerVersion:8;
      CompilerVersionStr: 'VER80';
      Supported: True),
    (
      Name: cDelphiName;          //2
      VersionStr: '2.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '20';
      ShortName: 'D2';
      CompilerVersion:9;
      CompilerVersionStr: 'VER90';
      Supported: True),
    (
      Name: cDelphiName;          //3
      VersionStr: '3.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '30';
      ShortName: 'D3';
      CompilerVersion:10;
      CompilerVersionStr: 'VER100';
      Supported: True),
    (
      Name: cDelphiName;          //4
      VersionStr: '4.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '40';
      ShortName: 'D4';
      CompilerVersion:12;
      CompilerVersionStr: 'VER120';
      Supported: True),
    (
      Name: cDelphiName;          //5
      VersionStr: '5.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '50';
      ShortName: 'D5';
      CompilerVersion:13;
      CompilerVersionStr: 'VER130';
      Supported: True),
    (
      Name: cDelphiName;          //6
      VersionStr: '6.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '60';
      ShortName: 'D6';
      CompilerVersion:14;
      CompilerVersionStr: 'VER140';
      Supported: True),
    (
      Name: cDelphiName;          //7
      VersionStr: '7.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '70';
      ShortName: 'D7';
      CompilerVersion:15;
      CompilerVersionStr:'VER150';
      Supported: True),
    (
      Name: cDelphiName;          //8
      VersionStr: '8';
      IDEVersion: 2;
      IDEVersionStr: '2.0';
      CoreIdeVersion: '71';
      ShortName: 'D8';
      CompilerVersion:16;
      CompilerVersionStr: 'VER160';
      Supported: True),
    (
      Name: cDelphiName;          //9
      VersionStr: '2005';
      IDEVersion: 3;
      IDEVersionStr: '3.0';
      CoreIdeVersion: '90';
      ShortName: 'D2005';
      CompilerVersion:17;
      CompilerVersionStr:'VER170';
      Supported: True),
    (
      Name: cBDSName;             //10
      VersionStr: '2006';
      IDEVersion: 4;
      IDEVersionStr: '4.0';
      CoreIdeVersion: '100';
      ShortName: 'D2006';
      CompilerVersion:18;
      CompilerVersionStr:'VER180';
      Supported: True),
    (
      Name: cRSName;              //11
      VersionStr: '2007';
      IDEVersion: 5;
      IDEVersionStr: '5.0';
      CoreIdeVersion: '100';
      ShortName: 'D2007';
      CompilerVersion:18.5;
      CompilerVersionStr:'VER185';
      Supported: True),
    (
      Name: cRSName;              //12
      VersionStr: '2008';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '0';
      ShortName: 'n/a';
      Supported: False),
    (
      Name: cRSName;              //13
      VersionStr: '2009';
      IDEVersion: 6;
      IDEVersionStr: '6.0';
      CoreIdeVersion: '120';
      ShortName: 'D2009';
      CompilerVersion:20;
      CompilerVersionStr:'VER200';
      Supported: True),
    (
      Name: cRSName;              //14
      VersionStr: '2010';
      IDEVersion: 7;
      IDEVersionStr: '7.0';
      CoreIdeVersion: '140';
      ShortName: 'D2010';
      CompilerVersion:21;
      CompilerVersionStr:'Ver210';
      Supported: True),
    (
      Name: cRSName;              //15
      VersionStr: 'XE';
      IDEVersion: 8;
      IDEVersionStr: '8.0';
      CoreIdeVersion: '150';
      ShortName: 'D2011';
      CompilerVersion:22;
      CompilerVersionStr:'VER220';
      Supported: True)
  );


implementation

end.
