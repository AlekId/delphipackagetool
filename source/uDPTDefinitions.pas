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

const
  cDelphiKey='SOFTWARE\BORLAND\DELPHI';
  cBorlandBDSKey='SOFTWARE\BORLAND\BDS';
  cCodeGearBDSKey='SOFTWARE\CODEGEAR\BDS';
  cDelphiTag='$(DELPHI)';
  cBDSTag='$(BDS)';
  cBDSCommonDirTag='$(BDSCOMMONDIR)';
  cProgramFilesTag='$(PROGRAMFILES)';
  cDelphiVersionTag='$(DELPHIVERSION)';


implementation

end.
