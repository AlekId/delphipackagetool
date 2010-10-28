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
      Name: cDelphiName;
      VersionStr: '1.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '10';
      ShortName: 'D1';
      Supported: True),
    (
      Name: cDelphiName;
      VersionStr: '2.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '20';
      ShortName: 'D2';
      Supported: True),
    (
      Name: cDelphiName;
      VersionStr: '3.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '30';
      ShortName: 'D3';
      Supported: True),
    (
      Name: cDelphiName;
      VersionStr: '4.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '40';
      ShortName: 'D4';
      Supported: True),
    (
      Name: cDelphiName;
      VersionStr: '5.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '50';
      ShortName: 'D5';
      Supported: True),
    (
      Name: cDelphiName;
      VersionStr: '6.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '60';
      ShortName: 'D6';
      Supported: True),
    (
      Name: cDelphiName;
      VersionStr: '7.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '70';
      ShortName: 'D7';
      Supported: True),
    (
      Name: cDelphiName;
      VersionStr: '8';
      IDEVersion: 2;
      IDEVersionStr: '2.0';
      CoreIdeVersion: '71';
      ShortName: 'D8';
      Supported: True),
    (
      Name: cDelphiName;
      VersionStr: '2005';
      IDEVersion: 3;
      IDEVersionStr: '3.0';
      CoreIdeVersion: '90';
      ShortName: 'D2005';
      Supported: True),
    (
      Name: cBDSName;
      VersionStr: '2006';
      IDEVersion: 4;
      IDEVersionStr: '4.0';
      CoreIdeVersion: '100';
      ShortName: 'D2006';
      Supported: True),
    (
      Name: cRSName;
      VersionStr: '2007';
      IDEVersion: 5;
      IDEVersionStr: '5.0';
      CoreIdeVersion: '100';
      ShortName: 'D2007';
      Supported: True),
    (
      Name: cRSName;
      VersionStr: '2009';
      IDEVersion: 6;
      IDEVersionStr: '6.0';
      CoreIdeVersion: '120';
      ShortName: 'D2009';
      Supported: True),
    (
      Name: cRSName;
      VersionStr: '';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '0';
      ShortName: 'n/a';
      Supported: False),
    (
      Name: cRSName;
      VersionStr: '2010';
      IDEVersion: 7;
      IDEVersionStr: '7.0';
      CoreIdeVersion: '140';
      ShortName: 'D2010';
      Supported: True),
    (
      Name: cRSName;
      VersionStr: 'XE';
      IDEVersion: 8;
      IDEVersionStr: '8.0';
      CoreIdeVersion: '150';
      ShortName: 'D2011';
      Supported: True)
  );


implementation

end.
