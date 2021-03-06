{*-----------------------------------------------------------------------------
 Unit Name: uDPTDefinitions
 Author:    sam
 Date:      23-Feb-2010
 Purpose:   contains delphi package tool specific definitions.
 History:
-----------------------------------------------------------------------------}

unit uDPTDefinitions;

interface

const
  //Platform names
  sWin32 = 'Win32';
  sWin64 = 'Win64';

  //Platform compilers
  sWin32Compiler = 'bin\dcc32.exe';
  sWin64Compiler = 'bin\dcc64.exe';

  //Config names
  sNoConfig = 'NoConfig';
  sDebug    = 'Debug';
  sRelease  = 'Release';

  sMSBuild       = 'msbuild.exe';
  sRadStudioVars = 'bin\rsvars.bat';


type
  TApplicationState=(tas_init,
                     tas_open,
                     tas_working);

  TPackageRemoveType=(tpr_none,          // no packages will be remove.
                      tpr_3rdparty,      // all packages except the borland packages in the directory ($DELPHI)\bin
                      tpr_projectsbpl);  // all packages which are in the directory ($DELPHI)\Projects\bpl

  TDelphiNameType=(tdn_short,tdn_long);

  TPlatformTypes=(tpl_win16,
                  tpl_win32,
                  tpl_win64);
                 
  TSupportedPlatforms=set of TPlatformTypes;

  TDelphiVersionInfo = record
    Name: string;
    VersionStr: string;      // the version short name e.g. XE10.1
    IDEVersion: Integer;
    IDEVersionStr: string;    // the IDE version string as used in the registery e.g. "7.0" --> \Software\Borland\Delphi\7.0 or "18.0" --> Software\Embarcadero\BDS\18.0\ 
    PackageVersion: string;
    ShortName:string;
    LongName:string;
    CompilerVersion:double;
    CompilerVersionStr:string;
    SupportedPlatforms:TSupportedPlatforms;
    Supported: Boolean;
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
  cPackageVersionTag='$(PACKAGEVERSION)';
  cPlatformTag='$(PLATFORM)';
  cConfigTag='$(CONFIG)';
  cUsersPublicTag='$(PUBLIC)';
  cRADStudioDirName='RAD Studio';
  cEmbaStudioDirName='Embarcadero\Studio';
  cDelphiName = 'Delphi';
  cBCBName    = 'C++Builder';
  cBDSName    = 'Borland Developer Studio';
  cRSName     = 'RAD Studio';

  cLIBAutomaticTag='<Auto>';     
  cLIBNoneTag='<None>';


  cProjectGroupExtensions='<.bpg/.bdsgroup/.groupproj>';
  cProjectGroupFilter='*.bpg;*.groupproj;*.bdsgroup';

  DelphiVersions: array [1..27] of TDelphiVersionInfo = (
    (
      Name: cDelphiName;           //1
      VersionStr: '1.0';
      IDEVersion: 0;
      IDEVersionStr: '1.0';
      PackageVersion: '10';
      ShortName: 'D1';
      LongName: 'Borland Delphi 1.0';
      CompilerVersion:8;
      CompilerVersionStr: 'VER80';
      Supported: True),
    (
      Name: cDelphiName;          //2
      VersionStr: '2.0';
      IDEVersion: 0;
      IDEVersionStr: '2.0';
      PackageVersion: '20';
      ShortName: 'D2';
      LongName: 'Borland Delphi 2.0';
      CompilerVersion:9;
      CompilerVersionStr: 'VER90';
      Supported: True),
    (
      Name: cDelphiName;          //3
      VersionStr: '3.0';
      IDEVersion: 0;
      IDEVersionStr: '3.0';
      PackageVersion: '30';
      ShortName: 'D3';
      LongName: 'Borland Delphi 3.0';
      CompilerVersion:10;
      CompilerVersionStr: 'VER100';
      Supported: True),
    (
      Name: cDelphiName;          //4
      VersionStr: '4.0';
      IDEVersion: 0;
      IDEVersionStr: '4.0';
      PackageVersion: '40';
      ShortName: 'D4';
      LongName: 'Borland Delphi 4.0';
      CompilerVersion:12;
      CompilerVersionStr: 'VER120';
      Supported: True),
    (
      Name: cDelphiName;          //5
      VersionStr: '5.0';
      IDEVersion: 0;
      IDEVersionStr: '5.0';
      PackageVersion: '50';
      ShortName: 'D5';
      LongName: 'Borland Delphi 5.0';
      CompilerVersion:13;
      CompilerVersionStr: 'VER130';
      Supported: True),
    (
      Name: cDelphiName;          //6
      VersionStr: '6.0';
      IDEVersion: 0;
      IDEVersionStr: '6.0';
      PackageVersion: '60';
      ShortName: 'D6';
      LongName: 'Borland Delphi 6.0';
      CompilerVersion:14;
      CompilerVersionStr: 'VER140';
      Supported: True),
    (
      Name: cDelphiName;          //7
      VersionStr: '7.0';
      IDEVersion: 0;
      IDEVersionStr: '7.0';
      PackageVersion: '70';
      ShortName: 'D7';
      LongName: 'Borland Delphi 7.0';
      CompilerVersion:15;
      CompilerVersionStr:'VER150';
      Supported: True),
    (
      Name: cDelphiName;          //8
      VersionStr: '8';
      IDEVersion: 2;
      IDEVersionStr: '2.0';
      PackageVersion: '80';
      ShortName: 'D8';
      LongName: 'Borland Delphi 8 .NET';
      CompilerVersion:16;
      CompilerVersionStr: 'VER160';
      Supported: True),
    (
      Name: cDelphiName;          //9
      VersionStr: '2005';
      IDEVersion: 3;
      IDEVersionStr: '3.0';
      PackageVersion: '90';
      ShortName: 'D2005';
      LongName: 'Borland Delphi 2005';
      CompilerVersion:17;
      CompilerVersionStr:'VER170';
      Supported: True),
    (
      Name: cBDSName;             //10
      VersionStr: '2006';
      IDEVersion: 4;
      IDEVersionStr: '4.0';
      PackageVersion: '100';
      ShortName: 'D2006';
      LongName: 'Borland Developer Studio 2006/Turbo Delphi';
      CompilerVersion:18;
      CompilerVersionStr:'VER180';
      Supported: True),
    (
      Name: cRSName;              //11
      VersionStr: '2007';
      IDEVersion: 5;
      IDEVersionStr: '5.0';
      PackageVersion: '110';
      ShortName: 'D2007';
      LongName: 'CodeGear RAD Studio 2007/CodeGear Delphi 2007 for Win32';
      CompilerVersion:18.5;
      CompilerVersionStr:'VER185';
      Supported: True),
    (
      Name: cRSName;              //12
      VersionStr: '2009';
      IDEVersion: 6;
      IDEVersionStr: '6.0';
      PackageVersion: '120';
      ShortName: 'D2009';
      LongName: 'CodeGear RAD Studio 2009/CodeGear Delphi 2009 for Win32';
      CompilerVersion:20;
      CompilerVersionStr:'VER200';
      Supported: True),
    (
      Name: cRSName;              //13
      VersionStr: 'n/a';
      IDEVersion: 0;
      IDEVersionStr: 'n/a';
      PackageVersion: '0';
      ShortName: 'n/a';
      Supported: False),
    (
      Name: cRSName;              //14
      VersionStr: '2010';
      IDEVersion: 7;
      IDEVersionStr: '7.0';
      PackageVersion: '140';
      ShortName: 'D2010';
      LongName: 'Embarcadero RAD Studio 2010';
      CompilerVersion:21;
      CompilerVersionStr:'VER210';
      Supported: True),
    (
      Name: cRSName;              //15
      VersionStr: 'XE';
      IDEVersion: 8;
      IDEVersionStr: '8.0';
      PackageVersion: '150';
      ShortName: 'XE';
      LongName: 'Embarcadero RAD Studio XE';
      CompilerVersion:22;
      CompilerVersionStr:'VER220';
      Supported: True),
    (
      Name: cRSName;              //16
      VersionStr: 'XE2';
      IDEVersion: 9;
      IDEVersionStr: '9.0';
      PackageVersion: '160';
      ShortName: 'XE2';
      LongName: 'Embarcadero RAD Studio XE2';
      CompilerVersion:23;
      CompilerVersionStr:'VER230';
      Supported: True),
    (
      Name: cRSName;              //17
      VersionStr: 'XE3';
      IDEVersion: 10;
      IDEVersionStr: '10.0';
      PackageVersion: '170';
      ShortName: 'XE3';
      LongName: 'Embarcadero RAD Studio XE3';
      CompilerVersion:24;
      CompilerVersionStr:'VER240';
      Supported: True),
    (
      Name: cRSName;              //18
      VersionStr: 'XE4';
      IDEVersion: 11;
      IDEVersionStr: '11.0';
      PackageVersion: '180';
      ShortName: 'XE4';
      LongName: 'Embarcadero RAD Studio XE4';
      CompilerVersion:25;
      CompilerVersionStr:'VER250';
      Supported: True),
    (
      Name: cRSName;              //19
      VersionStr: 'XE5';
      IDEVersion: 12;
      IDEVersionStr: '12.0';
      PackageVersion: '190';
      ShortName: 'XE5';
      LongName: 'Embarcadero RAD Studio XE5';
      CompilerVersion:26;
      CompilerVersionStr:'VER260';
      Supported: True),
    (
      Name: cRSName;              //20
      VersionStr: 'AppMethod';
      IDEVersion: 13;
      IDEVersionStr: '???';
      PackageVersion: '???';
      ShortName: '???';
      LongName: 'AppMethod';
      CompilerVersion:26.5;
      CompilerVersionStr:'VER265';
      Supported: False),
    (
      Name: cRSName;              //21
      VersionStr: 'XE6';
      IDEVersion: 14;
      IDEVersionStr: '14.0';
      PackageVersion: '200';
      ShortName: 'XE6';
      LongName: 'Embarcadero RAD Studio XE6';
      CompilerVersion:27;
      CompilerVersionStr:'VER270';
      Supported: True),
    (
      Name: cRSName;              //22
      VersionStr: 'XE7';
      IDEVersion: 15;
      IDEVersionStr: '15.0';
      PackageVersion: '210';
      ShortName: 'XE7';
      LongName: 'Embarcadero RAD Studio XE7';
      CompilerVersion:28;
      CompilerVersionStr:'VER280';
      Supported: True),
   (
      Name: cRSName;              //23
      VersionStr: 'XE8';
      IDEVersion: 16;
      IDEVersionStr: '16.0';
      PackageVersion: '220';
      ShortName: 'XE8';
      LongName: 'Embarcadero RAD Studio XE8';
      CompilerVersion:29;
      CompilerVersionStr:'VER290';
      Supported: True),
   (
      Name: cRSName;              //24
      VersionStr: 'XE10';
      IDEVersion: 17;
      IDEVersionStr: '17.0';
      PackageVersion: '230';
      ShortName: 'XE10';
      LongName: 'Embarcadero RAD Studio 10 Seattle';
      CompilerVersion:30;
      CompilerVersionStr:'VER300';
      Supported: True),
   (
      Name: cRSName;              //25
      VersionStr: 'XE10.1';
      IDEVersion: 18;
      IDEVersionStr: '18.0';
      PackageVersion: '240';
      ShortName: 'XE10.1';
      LongName: 'Embarcadero RAD Studio 10.1 Berlin';
      CompilerVersion:31;
      CompilerVersionStr:'VER310';
      Supported: True),
   (
      Name: cRSName;              //26
      VersionStr: 'XE10.2';
      IDEVersion: 19;
      IDEVersionStr: '19.0';
      PackageVersion: '250';
      ShortName: 'XE10.2';
      LongName: 'Embarcadero RAD Studio 10.2 Tokyo';
      CompilerVersion:32;
      CompilerVersionStr:'VER320';
      Supported: True),
   (
      Name: cRSName;              //27
      VersionStr: 'XE10.3';
      IDEVersion: 20;
      IDEVersionStr: '20.0';
      PackageVersion: '260';
      ShortName: 'XE10.3';
      LongName: 'Embarcadero RAD Studio 10.3 Rio';
      CompilerVersion:33;
      CompilerVersionStr:'VER330';
      Supported: True)
  );


implementation

end.
