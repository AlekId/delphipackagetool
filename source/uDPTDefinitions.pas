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

  TDelphiPlatform=(tdp_win32,
                   tdp_win64,
                   tdp_OSX);

  TDelphiPlatforms= set of TDelphiPlatform;

  TDelphiConfig=(tdc_debug,
                 tdc_test,
                 tdc_release);

  TDelphiConfigs= set of TDelphiConfig;

  TDelphiNameType=(tdn_short,tdn_long);

  TDelphiVersionInfo = record
    Name: string;
    VersionStr: string;
    IDEVersion: Integer;
    IDEVersionStr: string;
    CoreIdeVersion: string;
    ShortName:string;
    LongName:string;
    CompilerVersion:double;
    CompilerVersionStr:string;
    Supported: Boolean;
    Platforms:TDelphiPlatforms;
    Configs:TDelphiConfigs;
  end;

  TProjectType=(tp_unkown,  // defines compiler output
              tp_dll,     // project is a dll
              tp_exe,     // project is a executable
              tp_bpl);    // project is a package


// these settings are stored in the registry in SOFTWARE\Borland\Delphi\x.x\Library
// or SOFTWARE\Borland\BDS\x.x\library.
  TDelphiLibraryPath=record
    DCPpath:string;
    BPLpath:string;
    Searchpath:string;
    DebugDCUpath:string;
    BrowsingPath:string;
    PackagePath:string
  end;


resourcestring
cConfirm='Confirm';
cWarning='Warning';
cError='Error';
cInformation='Information';

const
  cDelphiKey='SOFTWARE\BORLAND\DELPHI';
  cBorlandBDSKey='SOFTWARE\BORLAND\BDS';
  cCodeGearBDSKey='SOFTWARE\CODEGEAR\BDS';
  cEmbarcaderoBDSKey='SOFTWARE\EMBARCADERO\BDS\';
  cDelphiTag='$(DELPHI)';
  cBDSTag='$(BDS)';
  cBDSBINTag='$(BDSBIN)';
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

  cLIBAutomaticTag='<Auto>';     
  cLIBNoneTag='<None>';


  cProjectGroupExtensions='<.bpg/.bdsgroup/.groupproj>';
  cProjectGroupFilter='*.bpg;*.groupproj;*.bdsgroup';

  DelphiVersions: array [1..17] of TDelphiVersionInfo = (
    (
      Name: cDelphiName;           //1
      VersionStr: '1.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '10';
      ShortName: 'D1';
      LongName: 'Borland Delphi 1.0';
      CompilerVersion:8;
      CompilerVersionStr: 'VER80';
      Supported: True;
      Platforms:[tdp_win32];
      Configs:[tdc_release]),  //???
    (
      Name: cDelphiName;          //2
      VersionStr: '2.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '20';
      ShortName: 'D2';
      LongName: 'Borland Delphi 2.0';
      CompilerVersion:9;
      CompilerVersionStr: 'VER90';
      Supported: True;
      Platforms:[tdp_win32];
      Configs:[tdc_release]), //???
    (
      Name: cDelphiName;          //3
      VersionStr: '3.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '30';
      ShortName: 'D3';
      LongName: 'Borland Delphi 3.0';
      CompilerVersion:10;
      CompilerVersionStr: 'VER100';
      Supported: True;
      Platforms:[tdp_win32];
      Configs:[tdc_release]),
    (
      Name: cDelphiName;          //4
      VersionStr: '4.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '40';
      ShortName: 'D4';
      LongName: 'Borland Delphi 4.0';
      CompilerVersion:12;
      CompilerVersionStr: 'VER120';
      Supported: True;
      Platforms:[tdp_win32];
      Configs:[tdc_release]),
    (
      Name: cDelphiName;          //5
      VersionStr: '5.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '50';
      ShortName: 'D5';
      LongName: 'Borland Delphi 5.0';
      CompilerVersion:13;
      CompilerVersionStr: 'VER130';
      Supported: True;
      Platforms:[tdp_win32];
      Configs:[tdc_release]),
    (
      Name: cDelphiName;          //6
      VersionStr: '6.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '60';
      ShortName: 'D6';
      LongName: 'Borland Delphi 6.0';
      CompilerVersion:14;
      CompilerVersionStr: 'VER140';
      Supported: True;
      Platforms:[tdp_win32];
      Configs:[tdc_release]),
    (
      Name: cDelphiName;          //7
      VersionStr: '7.0';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '70';
      ShortName: 'D7';
      LongName: 'Borland Delphi 7.0';
      CompilerVersion:15;
      CompilerVersionStr:'VER150';
      Supported: True;
      Platforms:[tdp_win32];
      Configs:[tdc_release]),
    (
      Name: cDelphiName;          //8
      VersionStr: '8';
      IDEVersion: 2;
      IDEVersionStr: '2.0';
      CoreIdeVersion: '71';
      ShortName: 'D8';
      LongName: 'Borland Delphi 8 .NET';
      CompilerVersion:16;
      CompilerVersionStr: 'VER160';
      Supported: True;
      Platforms:[tdp_win32];
      Configs:[tdc_release]),
    (
      Name: cDelphiName;          //9
      VersionStr: '2005';
      IDEVersion: 3;
      IDEVersionStr: '3.0';
      CoreIdeVersion: '90';
      ShortName: 'D2005';
      LongName: 'Borland Delphi 2005';
      CompilerVersion:17;
      CompilerVersionStr:'VER170';
      Supported: True;
      Platforms:[tdp_win32];
      Configs:[tdc_release]),
    (
      Name: cBDSName;             //10
      VersionStr: '2006';
      IDEVersion: 4;
      IDEVersionStr: '4.0';
      CoreIdeVersion: '100';
      ShortName: 'D2006';
      LongName: 'Borland Developer Studio 2006/Turbo Delphi';
      CompilerVersion:18;
      CompilerVersionStr:'VER180';
      Supported: True;
      Platforms:[tdp_win32];
      Configs:[tdc_release]),
    (
      Name: cRSName;              //11
      VersionStr: '2007';
      IDEVersion: 5;
      IDEVersionStr: '5.0';
      CoreIdeVersion: '100';
      ShortName: 'D2007';
      LongName: 'CodeGear Developer Studio 2007/CodeGear Delphi 2007 for Win32';
      CompilerVersion:18.5;
      CompilerVersionStr:'VER185';
      Supported: True;
      Platforms:[tdp_win32];
      Configs:[tdc_release]),
    (
      Name: cRSName;              //12
      VersionStr: '2008';
      IDEVersion: 0;
      IDEVersionStr: '';
      CoreIdeVersion: '0';
      ShortName: 'n/a';
      Supported: False;
      Platforms:[tdp_win32];
      Configs:[]),
    (
      Name: cRSName;              //13
      VersionStr: '2009';
      IDEVersion: 6;
      IDEVersionStr: '6.0';
      CoreIdeVersion: '120';
      ShortName: 'D2009';
      LongName: 'CodeGear Delphi 2009';
      CompilerVersion:20;
      CompilerVersionStr:'VER200';
      Supported: True;
      Platforms:[tdp_win32];
      Configs:[tdc_release]),
    (
      Name: cRSName;              //14
      VersionStr: '2010';
      IDEVersion: 7;
      IDEVersionStr: '7.0';
      CoreIdeVersion: '140';
      ShortName: 'D2010';
      LongName: 'Embarcadero RAD Studio 2010';
      CompilerVersion:21;
      CompilerVersionStr:'Ver210';
      Supported: True;
      Platforms:[tdp_win32];
      Configs:[tdc_release]),
    (
      Name: cRSName;              //15
      VersionStr: 'XE';
      IDEVersion: 8;
      IDEVersionStr: '8.0';
      CoreIdeVersion: '150';
      ShortName: 'XE';
      LongName: 'Embarcadero RAD Studio XE';
      CompilerVersion:22;
      CompilerVersionStr:'VER220';
      Supported: True;
      Platforms:[tdp_win32];
      Configs:[tdc_release,tdc_test,tdc_debug]),
    (
      Name: cRSName;              //16
      VersionStr: 'XE2';
      IDEVersion: 9;
      IDEVersionStr: '9.0';
      CoreIdeVersion: '160';
      ShortName: 'XE2';
      LongName: 'Embarcadero RAD Studio XE2';
      CompilerVersion:23;
      CompilerVersionStr:'VER230';
      Supported: True;
      Platforms:[tdp_win32,tdp_win64,tdp_OSX];
      Configs:[tdc_release,tdc_test,tdc_debug]),
    (
      Name: cRSName;              //17
      VersionStr: 'XE3';
      IDEVersion: 10;
      IDEVersionStr: '10.0';
      CoreIdeVersion: '170';
      ShortName: 'XE3';
      LongName: 'Embarcadero RAD Studio XE3';
      CompilerVersion:24;
      CompilerVersionStr:'VER240';
      Supported: True;
      Platforms:[tdp_win32,tdp_win64,tdp_OSX];
      Configs:[tdc_release,tdc_test,tdc_debug])
  );


implementation

end.
