Delphi Package Tool (DPT)
*************************

Who can use this Tool ?
***********************
1) Everyone who has taken a backup of his sources!
2) You can try to compile your projects/packages with different compiler versions, but this is only possible if
   you have the full source code of all your components and packges! 

Why shall I use this Tool ?
***************************

The DelphiPackageTool supports you when working with Delphi Package/Project Group Files (.bpg-Files):
If you have serveral Packages and Projects and you want to recompile/install in one go, then
this Tool may save you a lot of time.

Two disadvantages of the Delphi IDE can be avoided when using DelphiPackgeTool:

1. Bug in the Delphi IDE (Delphi7).
If you open a big project (.bpg) and Press <Rebuild All Projects> in the Delphi7 IDE, then for some reason
not all memory is released by the IDE. This means you have to close Delphi7 and start again.

2. The Delphi IDE has no function to install all packages automatically.
DelphiPackageTool compiles and installes all packages listed in the bpg-file.


Features:
- compile all .dpr,.dpk projects of a Delphi Package Group.(bpg/bdsgroup)
- installs Designtime Packages automatically into the Delphi IDE. 
- can also be used as command line tool. ( for automatic rebuild of your projects).
- can rebuild projects from several different delphi versions.

Licence:
The DelphiPackageTool is Freeware!
You use this Tool at your own Responsibility!
SOURCECODE is available at sourceforge. http://sourceforge.net/projects/delphipackageto/
To compile the project, open delphipackagetool.dpr with your favorite delphi IDE. ( there is no need to install addtional components )

Author:
Samuel Herzog, sam_herzog@yahoo.com


Command Line Examples:
**********************
The DelphiPackageTool return value is 0 if every thing compiled ok.
If an error happens, the return value will be 1.
Additionally also a log-file is created which contains the compiler output messages.

Example 1:
The following line recompiles all projects and packages and installs the packages into the IDE.

DelphiPackageTool.exe –o”myComponents.ini” –rebuild

The following steps are done by the DelphiPackageTool.

1. Delete Registry Entries. (This removes the Components from the IDE)
2. bpl/dcp will be deleted.
3. bpl/dcp will be recompiled.
4. Write Registry Entries. (Components will be installed)

Example 2:
The following line demonstrates how to compile and install <Package1.dpk>

DelphiPackageTool.exe -p"C:\Projects\Packages\OwnComp\Package1.dpk" -install


*************************************************************************************************************
IMPORTANT: Packages can only be re-compiled and re-installed if the Delphi IDE is CLOSED!
*************************************************************************************************************


History:

********
1.9.2.72  ( 12.10.2018 )
-SH: Set default build-mode when creating the ini-file to '' <empty> instead of <debug>.
-SH: If no checkbox is marked in group "platform", then it will take the platform from the .dproj file.

1.9.2.71  ( 18.03.2018 )
-SH: improvement in "BackupAll".

1.9.2.70  ( 17.01.2018 )
-SH: do not show gui if command line parameter "-silent" is set.

1.9.2.69  ( 12.01.2018 )
-SH: fixed regression from 1.9.2.68
     clean-up.
	 
1.9.2.68  ( 12.01.2018 )
-SH: changes for command-line parameter "-silent".

1.9.2.67  ( 24.12.2017 )
-SH: reduced complexity. Only configurations "debug" and "release" will be supported by DPT.
     added two new project-settings "DebugCompilerSwitches" and "ReleaseCompilerSwitches".
     re-named var "FConfigToCompile" into "FBuildMode".
     re-named var "FSearchPath" into "FProjectSearchPath".
     Some changes for "SilentMode". Don't ask to create folders if SilentMode is "On".

1.9.2.63  ( 10.11.2017 )
-SH: The dcp-output-path will now be set to the same location as the dcp-files.
     This means that the setting "Application/DCPOutputPath" from the project-ini-file
     will be ignored. 

1.9.2.62  ( 07.11.2017 )
-SH: Until now, DPT took always msbuild if the fileextension was <.dproj>.
     I have changed this behavoir. Now it will read "DCC_UseMSBuildExternally"
     from .dproj and only use msbuild it this setting is set to <true>.	 

1.9.2.61  ( 24.10.2017 )
-SH: Changes to make the featrue "Auto-Backup" work again. 


1.9.2.60  ( 16.10.2017 )
-SH: On the very first start to this application, the user shall be asked if he want's to assign the fileextensions .bpg,.grouproj,.bdsgroup
     to this tool. (this feature did not work anymore ->regression). Fixed now.

1.9.2.59  ( 12.10.2017 )
-SH: fix for package LibSuffix.
-SH: change for filesearch. The file search-dialog did not find file with multible '.' in the filename.

1.9.2.58  ( 25.09.2017 )
-SH: if the app runs with command-line params, then do not change the files. (event if settings "ChangeFiles"is set to true).

1.9.2.57  ( 14.09.2017 )
-SH: Moved setting "Allow DPT to change files" from ApplicationSettings to ProjectSettings.

1.9.2.56  ( 20.05.2017 )
-SH: if a .dproj file exists and user chooses "Open in IDE', then use the .dproj file.

1.9.2.55  ( 17.05.2017 )
-SH: make multi-select work again.
-SH: write DllSuffix into .dproj file if available.

1.9.2.54  ( 12.05.2017 )
-SH: remove "{$R *.otares}" from dpk-file (if exists) when delphi version is lower than XE2.

1.9.2.53  ( 10.05.2017 )
-SH: removed ifdef NoZipSupport.
-SH: removed some old trace stuff.
-SH: DPT is now built/compiled with Delphi 10.2 Tokyo.

1.9.2.52  ( 03.05.2017 )
-SH: write to all config files found.
-SH: clean-up.    

1.9.2.50  ( 26.04.2017 )
-SH: create EnvironmentVar in Registry if not exists.
-SH: don't show some message-boxes if SilentMode is selected.

1.9.2.48  ( 25.04.2017 )
-SH: changes for log-file.
-SH: changes for silentmode and display of diff-tool.
-SH: DPT is now built/compiled with Delphi 10.1 Berlin.

1.9.2.43  ( 17.04.2017 )
-SH: Improvements for external Source-Editor and Diff-Tool Setup.
-SH: Instead of deleting files, move them to recycle bin.
-SH: started to add options "Yes to all" and "No to all" to some Dialog-Boxes.

1.9.2.42  ( 14.04.2017 )
-SH: Show Options-Dialog on first Startup to let the user setup the Source-Editor and the Diff-Tool.

1.9.2.41  ( 10.04.2017 )
-SH: Breaking-Changes: The edit fields on the main form are now used to define the output-path for exe,dll and bpl. The location for dcp-files can not be modified anymore
                       and will always be the same folder as the bpl-files.
-SH: fixes,re-factoring clean-up.

1.9.2.32  ( 18.03.2017 )
-SH: instead of modifying the global environments-path, the delphi IDE Environment-Path is now used. (this was a  mis-conecption for a long time which is now corrected).
-SH: Removed setting "Allow modify Environment Path".

1.9.2.31  ( 13.03.2017 )
-SH: fix in relation with package-suffix and "Allow to change files". 
-SH: method GetLibSuffix returns now the PackageVersion instead of the ShortName. If you have set Lib-Suffix to <Auto>, the suffix will now be <240> instead of <XE10.1>

1.9.2.30  ( 10.08.2016 )
-SH: added new PlaceHolder $(PackageVersion) which will be replaced by definition from http://docwiki.embarcadero.com/RADStudio/Berlin/de/Compiler-Versionen

1.9.2.27  ( 19.05.2016 )
-SH: added new dialog to function <Tool><Clean up Delphi>.
     The new dialog will search all folder's defined in the windows environment path
     for *.bpl/*.dcp-files and display them. The user can now select the files to 
     delete.

1.9.2.26  ( 21.04.2016 )
-SH: added definitions for Delphi 10.1 Berlin.

1.9.2.25  ( 13.03.2016 )
-SH: moved setting createInstallBatch from application-settings to project-settings.

1.9.2.24  ( 15.10.2015 )
-SH: applied patch received from Andreas Heim about BDSBin-Tag for Delphi.

1.9.2.23   ( 12.10.2015 )
-SH: added definitions for Delphi 10 Seattle.

1.9.2.22   ( 25.06.2015 )
-SH: changes to make writing to the registry working again.

1.9.2.21   ( 20.06.2015 )
-CS: received patch from Christoph Schneider to make versions for XEn work (Platform and Config). Thank you.

1.9.2.20   ( 11.06.2015 )
-SH: fix: when switching between different delphi versions, the displayed hint on a package/project was not updated.

1.9.2.19   ( 19.05.2015 )
-SH: fixed regression in method "AddPackageToRegistry".

1.9.2.18   ( 17.05.2015 )
-SH: moved setting "AutoBackup" from Application-Settings to Project-Settings.
-SH: small change for reading registry.
-SH: added XE8. (sorry, could not test if it works because I have no XE8).

1.9.2.17   ( 14.09.2014 )
-SH: fixed possible problem when using strtofloat and decimalseparator does not meet the country-settings.

1.9.2.16   ( 10.09.2014 )
-SH: changes to correctly detect versions XE6 and XE7.

1.9.2.15   ( 26.08.2014 )
-MM: patch to support search-path's starting with '\'.

1.9.2.14   ( 22.08.2014 )
-SH: removed OnBeforeInstallAll and OnAfterInstallAll from Application.Settings. They are now in Project.Settings.
-MM: patch to support selection of compile-types e.g. debug/release from command line. e.g. -nRelease, e.g. -aWin32

1.9.2.13   ( 11.05.2014 )
-SH: added XE6. ( not tested yet because I do not have it)

1.9.2.12   ( 29.01.2014 )
-SH: clean-up of settings.

1.9.2.11   ( 05.12.2013 )
-SH: active app. In some cases the message boxes were displayed in the background.

1.9.2.10   ( 04.12.2013 )
-SH: fix for recent file list.
-SH: fix to load last used project group file.

1.9.2.9   ( 04.11.2013 )
-another patch from M.Mueller. For XE5 and some fixes.

1.9.2.8   ( 17.09.2013 )
-SH: improvement for source-backup. Added all files from project-group. 

1.9.2.7   ( 28.08.2013 )
-SH: don't show gui if command line parameter "-cleanupbpldir" is used.

1.9.2.6   ( 05.08.2013 )
-SH: don't load last project when starting app and silent mode is set.

1.9.2.5   ( 11.06.2013 )
-SH: added command-line parameter "-silent".

1.9.2.4   ( 20.05.2013 )
-SH: added Definitions of XE4.

1.9.2.3   ( 08.05.2013 )
-SH: performance improvement for backup function.

1.9.2.2   ( 01.05.2013 )
-SH: delete memo-log when button "Install All" is clicked.
-SH: if backup "source-only" is set, then do not backup .dll,.tlb,.ocx files.

1.9.2.1   ( 24.03.2013 )
-SH: explicittop and explicitwidth make trouble when compiling with D7.

1.9.2.0   ( 23.03.2013 )
-MM: patch to clean up project and application settings.
-MM: patch to support conditional defines.

1.9.1.11   ( 12.03.2013 )
-MM: don't save application settings when dpt is called as command line tool.
-SH: don't save project settings when dpt is called as command line tool.

1.9.1.10   ( 17.02.2013 )
-SH: fixes minor bug on updating the main-form.

1.9.1.9    ( 30.01.2013 )
- fixed bug in RelativePaths.
 
1.9.1.8    ( 22.01.2013 )
- huge rework received from M.Mueller to use
  msbuild instead of dcc32 for delphi 2007 and newer.

1.9.1.7    ( 27.12.2012 )
-SH: code-cleanup. removed/commented un-used code.
-SH: change about dcp-path to be backwards compatible with existing projects.

1.9.1.6    ( 21.12.2012 )
-patch from M.Mueller. Right-Click and Compile.

1.9.1.5    ( 19.12.2012 )
-SH: some code-cleanup and re-factoring.
-SH: Logic and GUI are now separated.
-SH: ESC is working again to abort "Compile All".

1.9.1.4    ( 14.12.2012 )
-another patch from M.Mueller. Code clean-up.
-Replace-Tag's right before feeding the command-line.

1.9.1.3    ( 11.12.2012 ) 
-SH: some code-cleanup and re-factoring. 

1.9.1.2    ( 07.12.2012 )
-another patch from M.Mueller. Code clean-up.

1.9.1.1    ( 04.12.2012 )
-another patch from M.Mueller to make F9,Shift-F9 work again.

1.9.1.0    ( 30.11.2012 )
- huge rework received from M.Mueller to support platforms and build modes of newer delphi versions.

***********************************************
1.9.0.173  ( 19.11.2012 )
-SH: some code-cleanup and re-factoring.
-SH: fix in CleanupByRegistry.

1.9.0.172  ( 10.10.2012 )
-SH: some code-cleanup and re-factoring.

1.9.0.172  ( 10.10.2012 )
-SH: some code-cleanup and re-factoring.

1.9.0.171  ( 08.10.2012 )
-SH: applied patch received from M.Mueller for detecting Delphi 2010
     and resolved some warnings in XE2.

1.9.0.170  ( 15.09.2012 )
-SH: add definitions for Delphi XE3. (not tested because I do not own XE3).

1.9.0.168  ( 15.02.2012 )
-SH: applied patch received from Andreas Heim about BDSBin-Tag for Delphi XE2.

1.9.0.167  ( 31.01.2012 )
-SH: added setting to determ if non-source-files (e.g. dcu,bpl,dcp) shall be added to backup-zip or not.

1.9.0.166  ( 27.01.2012 )
-SH: improvement in function <Get7zAppName> to find 7z.exe which is used for backup-task.

1.9.0.165  ( 01.11.2011 )
-SH: more changes when reading/writting registry settings. Try first <HKEY_LOCAL_MACHINE> and then <HKEY_CURRENT_USER>.
-SH: more cleanup of trace messages.

1.9.0.164  ( 28.10.2011 )
-SH: changes when reading registry settings. Try first <HKEY_LOCAL_MACHINE> and then <HKEY_CURRENT_USER>.
-SH: cleanup of trace messages.


1.9.0.164  ( 28.10.2011 )
-SH: changes when reading registry settings. Try first <HKEY_LOCAL_MACHINE> and then <HKEY_CURRENT_USER>.
     cleanup of trace messages.

1.9.0.163  ( 23.10.2011 )
-SH: fix in DetermProjectTypeDelphi incase when the file does not exist.
     RemoveProjectFromBPG did not work in this special case.

1.9.0.162  ( 18.10.2011 )
-SH: fixes for registry access.

1.9.0.161  ( 17.10.2011 )
-SH: changes in set/get package dir.

1.9.0.159  ( 04.10.2011 )
- SH: changes to make backup-zip feature work. Needs a installed 7zip V9.25 or higher.

1.9.0.158  ( 03.09.2011 )
- SH: started work to support platforms.
      This is needed because in delphi xe2 the path's defined in the registry
      are on different locations for the different platforms. Note! this feature
      is not finished yet and un-tested.

1.9.0.157  ( 02.09.2011 )
- SH: add definitions for Delphi XE2.

1.9.0.156  ( 14.07.2011 )
- SH: improving the backup-function.

1.9.0.155  ( 17.06.2011 )
- SH: re-compiled aftter improvement in method <RegisterFileType>. Could give an AV
      when not possible to write because of non-admin rights on Win7.

1.9.0.154  ( 25.05.2011 )
- SH: some tweaks to avoid emtpy path entries in search-path and environment variable.

1.9.0.153  ( 10.05.2011 )
- SH: if tags <release> and <debug> are not found in the dproj-file, then do not insert
      any path tags.

1.9.0.152  ( 06.05.2011 )
- SH: made changes for <DCC_HppOutput>. It should not be written into .dproj files older
      than D2009.

1.9.0.151  ( 23.04.2011 )
- SH: changes to write search path and bpl,dcp-output path into the dproj-file of Delphi XE.
      At the moment this settings will only be written into the "Release" configuration. (not debug).
      I have not decided yet how to handle this new possiblities.

1.9.0.150  ( 18.04.2011 )
- SH: improved error handling in case the global environment path can not be written
      because of access rights issues.

1.9.0.149  ( 10.04.2011 )
- SH: Version mix up because of D2008.
- SH: fix for updating Lib-Suffix in Version >D7

1.9.0.148  ( 10.04.2011 )
- SH: added CompilerVersion to Info-Hint. e.g. VER220 for Delphi XE

1.9.0.147  ( 10.01.2011 )
- SH: automatic correct vcljpg/vclimg in .dpk-file.
- SH: added AppLocation in About-Dialog.

1.9.0.146 ( 30.12.2010 )
- SH: fix for detection of Delphi XE.

1.9.0.145 ( 29.12.2010 )
- SH: if silentMode is <on>, then don't ask the user if he want's to review the changes
      with the Diff tool.

1.9.0.144 ( 26.11.2010 )
- SH: recent addition of tracing-stuff slows down the application.
      So I have added a setting to turn it on/off. default=off.

1.9.0.143 ( 22.11.2010 )
- SH: delete log before starting to re-compile all projects. 

1.9.0.142 ( 05.10.2010 )
- SH: new function to write DPT-Path settings into the project files.
- SH: added memo to display log.

1.9.0.141 ( 18.09.2010 )
- SH: some minor GUI tweaks.

1.9.0.140 ( 10.08.2010 )
- SH: better error handling in case SetVersion.exe is not available.

1.9.0.139 ( 08.08.2010 )
- SH: it's now possible to set the version of one/many/all packages/projects due to integration
      of the application SetVersion.exe from Jason Penny.
 
1.9.0.138 ( 04.08.2010 )
- SH: started to add functions to set the projects VersionNumber.

1.9.0.137 ( 26.05.2010 )
- SH: changes to method <CleanUpPackagesByBPLPath> about return value.

1.9.0.136 ( 10.05.2010 )
- SH: added feature to Revert/Undo the last changes.

1.9.0.135 ( 06.05.2010 )
- SH: re-work of functions which change dpk,dproj,bdsproj,cfg,dof files.
- SH added feature to setup/integrate an external diff-tool. 

1.9.0.134 ( 27.04.2010 )
- SH: improvements for LIBSuffix.

1.9.0.133 ( 17.04.2010 )
- SH: fix in method <RelativePaths> about removing of double entries.

1.9.0.132 ( 15.04.2010 )
- applied patch from M.Mueller about Binary Output Path (-E).

1.9.0.131 ( 11.04.2010 )
- applied patch from M.Mueller about WinExecAndWait.

1.9.0.130 ( 07.04.2010 )
- add's -N /-NO to the batch file. Used to define the .dcu output path.

1.9.0.129 ( 06.04.2010 )
- applied patch from M.Mueller about reading of dproj-file for D2009,D2010.

1.9.0.128 ( 31.03.2010 )
- fix for Command Line Parameter -D (Conditional Define).
- improvements about information on batchfile creation.
- removed unused variable from CreateGroupProj.

1.9.0.127 ( 30.03.2010 )
- fix for Search Path.
- fix in GetSystemPath.
- fix for Project Search Path and DPT Search Path. 

1.9.0.125 ( 27.03.2010 )
- fix in CleanUpPackagesByRegistery. Expand placeholders like BDS before compare the path names.

1.9.0.124 ( 27.03.2010 )
- some changes about the config-filename. from D1-D8 it was .cfg, from D2005-D2006 it was .bdsproj, from D2007- its .dproj
- work inside SaveBackup to make it work again. DPT now creates a <backup.bat> and tries to execute it.
  The user of DPT may edit the file <backup_template.bat> to use another archiver.
- sourcecode clean-up.

1.9.0.122 ( 11.03.2010 )
- added patch from M.Mueller. DPT can now be built with different delphi versions.
- SH: fixed AV when using DPT with command-line parameters.

1.9.0.121 ( 10.03.2010 )
- added patch from M.Mueller. DPT now uses MSXML to read data from dproj,bdsgroup,groupproj files.
- changed name of BPGTemplatefile.txt to ProjectGroupTemplate.bpg.

1.9.0.120 ( 04.03.2010 )
- integrate changes from M.Mueller. Donated code to correctly read BDSCommenDir and BDSProjectsDir.

1.9.0.119 ( 24.02.2010 )
- re-factored to make this project ready to be hosted a sourceforge.net.
- zip-function has been removed. So no additional components must be installed to compile this project.
- tracefile has been replaced by OutputDebugString.

1.9.0.116 ( 17.02.2010 )
- when checking the path environments for double entries, then take into account
  that for example "c:\temp" and "c:\temp\" are the same.

1.9.0.115 ( 09.01.2010 )
- replaced all MessageDlg calls with MessageBox calls because language issues.

1.9.0.114 ( 02.01.2010 )
- moved some message strings to resourcestring section. (for translation)
- implemented <WriteDProjFile> to write LibSuffix into the dproj file.
- fix for missing zip-dll in relation with auto-backup.
- fixed typo reported by S.Besso.

1.9.0.113 ( 24.12.2009 )
- fix for packages with LibSuffix.

1.9.0.112 ( 22.12.2009 )
- fix for command line to make it work with .groupproj files.

1.9.0.111 ( 02.12.2009 )
- added routine to create/handle .groupproj files of D2007.
- added .obj and .zobj files to backup-files.
- fixed memory leak in savebackup.

1.9.0.108 ( 23.11.2009 )
- added my e-mail address to send the log-file.

1.9.0.107 ( 21.11.2009 )
- changes to read dproj-files.

1.9.0.106 ( 12.10.2009 )
- changed startup message when the last used IDE is not installed on the computer.

1.9.0.104 ( 30.09.2009 )
- remove switch -Q when Auto Backup is enabled.

1.9.0.102 ( 29.09.2009 )
- new feature "Auto Backup". After a successfull "Install All" a zip-file of the sources will be created.

1.9.0.101 ( 29.09.2009 )
- button <Add Delphi Default Directories> in Options-Dialog now adds the BPL-Folder according to the settings on the main-form.

1.9.0.100 ( 22.09.2009 )
- fixed bug about writing libsuffix into .dproj file intead of .dpk file.

1.9.0.99 ( 12.09.2009 )
- if the application is started for the first time, then the latest IDE will be set as current IDE.
- the content of edit box "Output Path for Package Directory (bpl+dcp files)" is now read from the registry.
- if the path for the bpl-files is changed then the registry is updated as well.

1.9.0.98 ( 08.09.2009 )
- remove possible double entries when creating a .bpg or .bdsgroup file.
- fix about package suffix.

1.9.0.97 ( 05.09.2009 )
- added date&time stamp to zip-filename when doing a backup.
- had to do some changes for Delphi 2010 because it's not version no. 13 as expected. It's version 14.
- fix for <NewPackageGroup>. It suggested to create a bdsgroup-file even there was Delphi 7 selected.

1.9.0.96 ( 29.08.2009 )
- added support for Delphi 2010.
- fix in CleanupRegistery. Create full pathname. Did not work with Tags e.g. (BDS)\bin up to now.
- fix in UninstallPackage. Entries from registry <Disabled Packages> were not removed.

1.9.0.92 ( 20.04.2009 )
- fix for strtofloat and strings containing ',' instead of '.'

1.9.0.91 ( 16.04.2009 )
- fix in CreateBPGFile.

1.9.0.90 ( 15.04.2009 )
- fix in Options-Dialog/VerifyDirectories. If the user pressed "no" he was stuck in an endless loop.

1.9.0.89 ( 10.04.2009 )
- fix for bpg-combobox.
- adapt search path when delphi version is changes. (replace bds with delphi and opposite depending on delphi version.)

1.9.0.88 ( 09.04.2009 )
- fix for Tags <Auto> and <None>.
- fix for searchpath in Search-Dialog when trailing '\' is missing.
- remove double entries from search-path list.
- replaced BPG-Edit Field with a combobox.

1.9.0.86 ( 04.04.2009 )
- fix about package suffix.
- fix in edit package group.

1.9.0.84 ( 03.04.2009 )
- added Placeholder $(DELPHIVERSION). e.g. bpl-output path you can set now to \bpl\$(DELPHIVERION)\ which will be to converted to \bpl\d7\ for Delphi 7 or \bpl\d2007\ for Delphi 2007.
- several changes to make it possible to compile a package group with different compiler versions.

1.9.0.83 ( 20.03.2009 )
- remove section [Excluded Packages] from .dof file because it always makes troubles.

1.9.0.82 ( 13.03.2009 )
- new parameter in Options-Dialog to define the command line parameters for the external editor. Valid placeholders
  are %FILENAME% and %LINENO%.

1.9.0.81 ( 11.03.2009 )
- change in search dialog for manual entered search path.
- on the first start of DPT after installation, it detects now the lowest installed IDE Version.

1.9.0.80 ( 04.03.2009 )
- fix for cleanup dialog. The cleanup by bpl-path did only work for D7 until now.

1.9.0.79 ( 21.01.2009 )
- fix for bdsgroup-files.
- Message about missing Environment-Path for the packages was supressed when "Silent-Mode" was enabled.
  This was not a good idea. Now the question will always show up if a package is located in a path which is
  unknown for the IDE.

1.9.0.73 ( 31.12.2008 )
- fix for history combobox in find-dialog.

1.9.0.72 ( 03.12.2008 )
- creating a new package group file was broken. fixed now.

1.9.0.71 ( 15.11.2008 )
- method ReplaceTag is now also able to replace the $(BDSCOMMONDIR) statement.

1.9.0.70 ( 14.11.2008 )
- fixed problem with version-property in the bpg-editor.
- disabled files grid when application starts a no bpg-file is loaded.        

1.9.0.69 ( 11.11.2008 )
- improvements in path selection dialog.

1.9.0.65 ( 06.11.2008 )
- fix in reading package suffix.

1.9.0.37 ( 05.06.2008 )
- fix in Verify-Registry.
- fix for stoponfailure.

1.9.0.37 ( 05.06.2008 )
- fix in Verify-Registry.
- fix for stoponfailure.

1.9.0.26 ( 02.06.2008 )
- a lot of work was done to support bdsproj,dproj files.

1.9.0.34 ( 04.06.2008 )
- fix in verify directories.
- use now combobox in search dialog.
- fix about stoponfailure.

1.9.0.26 ( 02.06.2008 )
- a lot of work was done to support bdsproj,dproj files.

1.3.0.23 ( 22.05.2008 )
- started work to read bdsproj and dproj files.

1.3.0.22 ( 21.05.2008 )
- added support for D2007 (aka bds 5.0)

1.3.0.21 ( 09.05.2008)
- recompiled after changes in AllDirectoriesOfPath,AllFilesOfDrive,AllFilesOfPath.

1.3.0.20 ( 21.03.2008)
- fix for project settings.

1.3.0.18 ( 17.03.2008 )
- complete internal re-design.

1.3.0.13 ( 11.03.2008 )
- changes to the command line support for pathnames like this ".\make\mygroup.bpg".
- cleanup for the trace-levels.

1.3.0.12 ( 18.02.2008 )
- recompiled after change in tracefile component.

1.3.0.11 ( 13.02.2008 )
- removed the VCLSkin Component for two reasons:
    a) it gave troubles.
    b) this shall become an open source project and
       therefore the closed-source stuff must be removed.
       
1.3.0.10 ( 08.02.2008 )
- internal re-design to make it open-source one day.
- removed old-style multilanguage stuff. Moved strings to resourcestring section.
- changed the command SelectDirectory. (suggested by R.Kaiser).
- the application remembers now the position and size of the main form. (suggested by R.Kaiser).

1.3.0.9 ( 12.12.2007 )
- fix in GetDelphiApplication.

1.3.0.8 ( 12.12.2007 )
- fix for problem "4.0" is not a valid floatingpoint value.
- cleanup of trace-messages.

1.3.0.4 ( 05.11.2007 )
- some fixes for writing path to dof-files.

1.3.0.0 ( 17.10.2007 )
- added function to create a backup zip-file of the compiled sources.
  See DelphiPackgeTool.pdf.

1.2.0.73 ( 27.09.2007 )
- fix for conditional defines. Did not work until now.
- changed way how the createProcess is used to call dcc32.exe.
  This has somehow influence on the working path.
- added setting to switch on/off the feature about modifying the environment path.
- improved compiler output display in the bottom memo.  

1.2.0.63 ( 15.06.2007 )
- new feature to replace all "absolute" Path's with "relative" Path's in .cfg and .dof file.
- more use of PlaceHolders like $(DELPHI) or $(PROGRAMFILES).
- several small fixes.

1.2.0.34 ( 06.03.2007 )
- fix for command-line parameters and relative path.
- small fix in search dialog about the caption of the find-button.

1.2.0.33 ( 17.11.2006 )
- fix for the setting <Create Install-Batch>. The change in
  the Options-Dialog was only getting active when reloading the project.

1.2.0.32 ( 16.11.2006 )
- fix for command line.

1.2.0.31 ( 28.10.2006 )
- bugfix for the events <OnBeforeInstallAll> and <OnAfterInstallAll>.
  Settings have moved from the application ini-file to the project ini-file.

1.2.0.30 ( 26.10.2006 )
- small bugfix when changing delphi version.
- default setting for "createinstallbatch" is turned off.
- save package bpl output path also as relative path.

1.2.0.28 ( 23.10.2006 )
- added method Verify Registry. This will lookup then "known packages" section
  in the registry and check each entry if the referenced file exists.
  If the refereced file does not exist, then the register entry gets deleted.
- the described check is also done if you open the delphi ide from inside this tool.
  This will prevent Delphi to ask questions when you start the IDE.

1.2.0.27 ( 10.10.2006 )
- decided to use the following Version names: D8 =8.0,D2005=9.0,D2006=10.0

1.2.0.26 ( 07.10.2006 )
- I got now my Delphi 2006. So work started to make the package tool
  also available for Delphi 2006 users.

1.2.0.25 ( 25.08.2006 )
- set the default directory before showing the Open-Dialog.
- small fix in the "Recent Files" List. 

1.2.0.24 ( 15.07.2006 )
- recompiled because of changes in the settings component.

1.2.0.23 ( 08.07.2006 )
- fixes in the new methods <CleanUpPackagesByBPLPath>
  and <CleanUpPackagesByRegistery>.

1.2.0.22 ( 07.07.2006 )
- added dialog to uninstall/delete packages.

1.2.0.21 ( 25.04.2006 )
- bug fix in method WriteLog.
- added Menu-Item "Recent Files".

1.2.0.20 ( 22.04.2006 )
- fixes to delete .bpl,.dcp file when package uses suffix.

1.2.0.19 ( 21.04.2006 )
- added support for package suffix.

1.2.0.18 ( 15.04.2006 )
- enabled col-sizing for the main grid.
- tweak in save project group dialog.

1.2.0.18 ( 15.04.2006 )
- enabled col-sizing for the main grid.
- tweak in save project group dialog.

1.2.0.17 ( 21.03.2006 )
- fix about bug of .bpl output path.

1.2.0.16 ( 18.03.2006 )
- begin of work to support also delphi 8,delphi 2005, delphi 2006 etc.
- added dialog to search/delete .bpl and .dcp file of a package.
- better check for installed delphi IDE's.

1.2.0.15 ( 28.01.2006 )
- filepath of OnBeforeInstall/OnAfterInstall is now also
  converted to relative path. (relative to the bpg-file)
- added new setting to allow the user to decide if the
  install batch-file and the install .reg-files shall be
  created or not.

1.2.0.14 ( 18.01.2006 )
- fix for OnBeforeInstall/OnAfterInstall.
- CompilerSwitches are now also in the project-specific ini-file which is created in
  the same directory as the .bpg-file.

1.2.0.12 ( 17.01.2006 )
- settings for the batch-files before/after install are now
  stored project-specific to file <PROJECTNAME.ini>. To be backwards compatible to versions older than 1.2.0.12
  the setting also remains in the file <delphipackagetool.ini>

1.2.0.11 ( 11.01.2006 )
- check if the files setup in OnBeforeInstall,OnAfterInstall,SourceCode Editor really exists.
- The function "Open Project in Delphi IDE" did not work when the path name contained "space" chars.
- fix to open file when doubleclicking of a .bpg file in the explorer.

1.2.0.10 ( 17.12.2005 )
- avoid doubled items in the project/packages editor.
- added feature to automatically open the "Add Path Dialog" if a compilation
  stops because a file was not found.

1.2.0.9 ( 11.11.2005 )
- fix for download mechanism if the NVBUpdater needs to be loaded.

1.2.0.8 ( 08.11.2005 )
- if the key <Delete> is pressed, then the package will be removed from the .bpg-file.
- worked on translation.
- if the application nvbupdater.exe is not found, then ask the user to download it.

1.2.0.7 ( 20.09.2005 )
-the install path of the NVBUpdater is <..\NVBUpdater\> and not <..\NVBUpdate\>. 

1.2.0.6 ( 28.06.2005 )
- fix in save as method. The search path file <.txt> was not copied.

1.2.0.5 ( 22.06.2005 )
- small fix for BPL output path. The trailing '\' was missing when displaying it.
- if the application will be skinned or not can now be setup in the ini-file.
- fix to clean the grid when new bpg-file is loaded. The filesize column was not cleaned up.
- enabled scrollbars in the path list.

1.2.0.4 ( 07.05.2005 )
- now also show the filesize of a package/project.
- fix:do not write a <.log> file if filename is empty.

1.2.0.3 ( 04.05.2005 )
- removed application update and put into seperate executable file <NVBUpdater.exe>.
- fix for command line parameter -o
- fix in Application Update Component.
- changes to setup for the execution path of the application.

1.2.0.1  ( 24.03.2005 )
- fix for tracefile file path.

1.2.0.0  ( 23.03.2005 )
- new functions to create a new or edit a package group file.
- now edit a BPG file and rearrange the projects by drag and drop.
- new function to show the cfg file.
- new function to jump to the file location and open the explorer.
- all path settings should be relative now and become corrected if neccessary.
- added multilanguage support (english/german)

1.1.8.8  ( 09.02.2005 )
- the bpl-output path is not taken anymore from the cfg-file. Now the path is taken from edit field <Package Directory> on the main form.
- made changes to avoid problems with spaces inside a path name when writing to batch file.
- load bpg-file when configuration file is loaded. (bugfix).

1.1.8.7  ( 02.02.2005 )
- bugfix when using DelphiPackageTool with batch mode.
- added load/save Configuration method.

1.1.8.6  ( 25.01.2005 )
- some cosmetic changes to show/delete the version number column.
- some documentation enhancements.

1.1.8.5  ( 29.12.2004 )
- improvments to move projects from one computer to another.
  Problems showed up when I tried to move a project from my Labtop to the Desktop computer.
  On the labtop the program files directory is <C:\Programme\> and on the Desktop computer
  it is <C:\Program Files\>. Now in the Path settings the new Tag $(PROGRAMFILES) is available
  to overcome this situation.
- if F9 (Compile) is pressed on a package then the package now gets also installed and not only 
  compiled.
- in the Options Dialog changed button caption "Abbrechen" to "Cancel".
- the edit fields "Delphi Compiler" and "Package Directory" can now contain the Tag $(DELPHI).

1.1.8.4  ( 15.11.2004 )
- bugfix in GetInstalledDelphiVerions.
  On some Delphi Installations the Registry Entry in HKEY_CURRENT_USER was
  not set. Therefore the DelphiPackageTool could not find out the installed
  Delphi Versions. Now the information are read from HKEY_LOCAL_MACHINE.

1.1.8.3 ( 01.11.2004 )
- ready now to read webupdate from homepage.
- compiled with new version 2.70 of VCLSkin.
- bugfix in web update component.

1.1.8.2 ( 23.09.2004 )
- added batchfile feature <OnBeforeInstallAll> and <OnAfterInstallAll>.

1.1.8.1 ( 09.09.2004 )
- compiled with new version 2.68 of VCLSkin

1.1.8.0 ( 05.08.2004 )
- convert relative output path to absolut outputpath.
- key <space> opens the current project file in the external editor.
- key <enter> tries to execute the current project.
- key <esc> stops a batch compile.

1.1.7.9 ( 03.08.2004 )
- cleaned up the tracefile messages.
- added method to run the exe-file.

1.1.7.8 ( 31.07.2004 )
- seperated project outputpath and package outputpath.
- compiled with new version 2.66 of VCLSkin

1.1.7.7 ( 21.07.2004 )
- minor bugfixes for relative package output path.
- added Install/Uninstall to the popup menu.

1.1.7.6 ( 20.07.2004 )
- source cleanup. minor speed tweak.

1.1.7.5 ( 19.07.2004 )
- fixed problems with relative path's.
- corrected the register/unregister .reg files.


1.1.7.4 ( 13.07.2004 )
- added empty line to the reg-file at the end.
- added edit field to define the compiler switches in the options dialog.
- added column to display the new version number of the package.

1.1.7.3 ( 06.07.2004 ) 
- added web-update feature.
- re-compiled with VCLSkin 2.65

1.1.7.2 ( 03.06.2004 )
- new action to compile the selected projects. So it is now possible to select
  a range of packages to compile.
- new button to select the delphi bpl directory.

1.1.7.1 ( 24.05.2004 )
- re-compiled with VCLSkin 2.62
- added process-messages between compile of project.


1.1.7.0 ( 08.05.2004 )
- some improvements in search.
- added splitter component.

1.1.6.9 ( 06.05.2004 )
- New popup menuitem to open project in delphi IDE.
- write total compile time to log window.

1.1.6.8 ( 22.04.2004)
- find dialog can be opened manually.
- added VCLSkin component.
                         

1.1.6.7 ( 03.02.2004)
- some minor bugfixes.
- If the compiler reports an error in a file then doubleclick onto the filename and
  it will open the file in the external editor.

1.1.6.6 ( 10.01.2004)
- Added function to search files.
  If you press <Install All> and the Compiler throws some Error messages because a 
  file was not found, then Doubleclick onto the filename.
  A file search dialog will show up and let you add the path to your project.

1.1.5.35 ( 15.09.2003 )
- Initial Release
