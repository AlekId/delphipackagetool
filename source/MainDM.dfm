object DMMain: TDMMain
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 239
  Width = 261
  object ActionList: TActionList
    Left = 32
    Top = 40
    object actCleanUpProjectBPLDir: TAction
      Caption = 'actCleanUpProjectBPLDir'
      OnExecute = actCleanUpProjectBPLDirExecute
    end
    object actCleanUpAll: TAction
      Caption = 'actCleanUpAll'
      OnExecute = actCleanUpAllExecute
    end
    object actReCompile: TAction
      Caption = 'Re-Install Project'
      Hint = 
        'Remove Registry Entry, delete the BPL-File, Recompile and Instal' +
        'l into Registry'
      OnExecute = actReCompileExecute
    end
    object actDeleteBPL: TAction
      Caption = 'Delete Package Files (.dcp/.bpl)'
      OnExecute = actDeleteBPLExecute
    end
    object actInstallPackage: TAction
      Caption = 'Install Package'
      Hint = 'Add Package to Delphi IDE'
      OnExecute = actInstallPackageExecute
    end
    object actUninstallPackage: TAction
      Caption = 'Uninstall Package'
      Hint = 'Removes Package from Delphi IDE.'
      OnExecute = actUninstallPackageExecute
    end
    object actResetDelphi: TAction
      Caption = 'Clean up Delphi'
      Hint = 'Remove Packages'
      OnExecute = actResetDelphiExecute
    end
    object actInstallAllPackages: TAction
      Caption = 'Install All Packages'
      Hint = 
        'Add registry entry for all Packages in the current Project Group' +
        '.'
      OnExecute = actInstallAllPackagesExecute
    end
    object actCompileAllPackages: TAction
      Caption = 'Compile All Projects'
      Hint = 'Compile all Packages/Projects of the current Project Group.'
      ShortCut = 16504
      OnExecute = actCompileAllPackagesExecute
    end
    object actUninstallAllPackages: TAction
      Caption = 'Uninstall All Packages'
      OnExecute = actUninstallAllPackagesExecute
    end
    object actShutDownDelphi: TAction
      Caption = 'Close Delphi'
      OnExecute = actShutDownDelphiExecute
    end
    object actStartUpDelphi: TAction
      Caption = 'Start Delphi'
      OnExecute = actStartUpDelphiExecute
    end
    object actExecuteApp: TAction
      Caption = 'Execute Application'
      OnExecute = actExecuteAppExecute
    end
    object actFindDCPandBPL: TAction
      Caption = 'actFindDCPandBPL'
      OnExecute = actFindDCPandBPLExecute
    end
    object actDeleteFiles: TAction
      Caption = 'Delete all files'
      Hint = 'Delete all bpl,dcp files.'
      OnExecute = actDeleteFilesExecute
    end
    object actRecompileAllPackages: TAction
      Caption = 'Install All'
      OnExecute = actRecompileAllPackagesExecute
    end
    object actRevertChanges: TAction
      Caption = 'Revert Changes'
      OnExecute = actRevertChangesExecute
    end
    object actWriteDPTPathsToProject: TAction
      Caption = 'Write DPT-Path'#39's to project'
      Hint = 
        'This will write the Path-Settings from Delphi Package Tool into ' +
        'the project files.'
      OnExecute = actWriteDPTPathsToProjectExecute
    end
  end
end
