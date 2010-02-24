{-----------------------------------------------------------------------------
 Unit Name: MainFrm
 Author:    Samuel Herzog
 Purpose:
 History:

1.9.0.119 ( 24.02.2010 )
- re-factored to make this project ready to be hosted a sourceforge.net.
- zip-function has been removed. So no additional components must be installed to compile this project.
- tracefile has been replaced by OuputDebugString.

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
- had to do some changes for Delphi 2010 because it's not version no. 13 as expected. Surprise! It's version 14.
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

1.3.0.0 ( 17.10.2007 )
- added function to create a backup zip-file of the compiled sources.
  See DelphiPackgeTool.pdf.

1.2.0.73 ( 27.09.2007 )
- fix for conditional defines. Did not work until now.
- changed way how the createProcess is used to call dcc32.exe. This has somehow influence
  on the working path.
- added setting to switch on/off the feature about modifying the environment path.
- improved compiler output display in the bottom memo.  

1.2.0.38 ( 08.03.2007 )
- added two more command line options:  -CleanUpBPLDir and -CleanUpAll
- more changes to support bds/turbo stuff.
- after deleting the bpl-files from the ($DELPHI)\Projects\BPL directory the registry is verified again.
- the path settings setup in the delphi package tool are now written into the projects .cfg-file.

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
- adde dialog to uninstall/delete packages.

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

1.2.0.17 ( 21.03.2006 )
- fix about bug of .bpl output path

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
  the setting also remains in the file <delphipackagetool.ini>.

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
- the install path of the NVBUpdater is <..\NVBUpdater\> and not <..\NVBUpdate\>.

1.2.0.6 ( 28.06.2005 )
- fix in save as method. The search path file <.txt> was not copied.

1.2.0.5 ( 22.06.2005 )
- small fix for BPL output path. The trailing '\' was missing when displaying it.
- if the application will be skinned or not can now be setup in the ini-file.
- fix to clean the grid when new bpg-file is loaded. The filesize column was not cleaned up.
- enabled scrollbars in the path list.

1.2.0.4 ( 07.05.2005 )
- now also show the filesize of a package/project.
- fix: do not write a <.log> file if filename is empty.

1.2.0.3 ( 03.05.2005 )
- removed application update and put into seperate executeable file.
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

1.1.8.9  ( 23.02.2005 )
- save the last used search path to ini file.
- sorry again changes to the bpl output path:
  First the package output path is read from the cfg-file.
  If a package output path is setup in the main dialog (edit field) then
  all packages will be placed in that directory.
  If no package output path is setup in the main dialog, then the packages
  will be placed in the directories specified in the cfg-file.

1.1.8.8  ( 09.02.2005 )
- the bpl-output path is not taken anymore from the cfg-file. Now the path is taken from edit field <Package Directory> on the main form.
- made changes to avoid problems with spaces inside a path name when writing to batch file.
- load bpg-file when configuration file is loaded. (bugfix).

1.1.8.7  ( 02.02.2005 )
- bugfix when using DelphiPackageTool with batch mode.
- added load/save Configuration method.

1.1.8.6  ( 25.01.2005 )
- some cosmetic changes to show/delete the version number column.

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


Version 1.1.8.4
           15.11.2004 SH - fix in GetInstalledDelphiVersions.

Version 1.1.8.3
           01.11.2004 SH - ready now to read webupdate from homepage.
                         - compiled with new version 2.70 of VCLSkin.
                         - bugfix in web update component.
Version 1.1.8.2
           23.09.2004 SH - added batchfile feature <OnBeforeInstallAll> and <OnAfterInstallAll>.

Version 1.1.8.1
           09.09.2004 SH - compiled with new version 2.68 of VCLSkin

Version 1.1.8.0
           05.08.2004 SH - convert relative output path to absolut outputpath.
                         - key <space> opens the current project file in the external editor.
                         - key <enter> tries to execute the current project.
                         - key <esc> stops a batch compile.
Version 1.1.7.9
           03.08.2004 SH - cleaned up the tracefile messages.
                         - added method to run the exe-file.
Version 1.1.7.8
           31.07.2004 SH - seperated project outputpath and package outputpath.
                         - compiled with new version 2.66 of VCLSkin

Version 1.1.7.7
           21.07.2004 SH - minor bugfixes for relative package output path.
                         - added Install/Uninstall to the popup menu.

Version 1.1.7.6
           20.07.2004 SH - source cleanup. minor speed tweak.

Version 1.1.7.5
           19.07.2004 SH - fixed problems with relative path's.
                         - corrected the register/unregister .reg files.

Version 1.1.7.4
           13.07.2004 SH - added empty line to the reg-file at the end.
                         - added edit field to define the compiler switches in the options dialog.
                         - added column to display the new version number of the package.
Version 1.1.7.3
           06.07.2004 SH - added web-update feature.
                         - re-compiled with VCLSkin 2.65

Version 1.1.7.2
           03.06.2004 SH - new action to compile the selected projects
                         - new button to select the delphi bpl directory.

Version 1.1.7.1
           24.05.2004 SH - re-compiled with VCLSkin 2.62
                         - added process-messages between compile of project.
Version 1.1.7.0
           08.05.2004 SH - some improvements in search.
                         - added splitter component.
Version 1.1.6.9
           06.05.2004 SH - open project in delphi IDE
                         - write total compile time to log window.
Version 1.1.6.8
           22.04.2004 SH - find dialog can be opened manually.
                         - added VCLSkin component.

Version 1.1.6.7
           03.02.2004 SH - some minor bugfixes. Read delphi info
           from registry key HKEY_LOCAL_MACHINE instead of HKEY_CURRENT_USER.
           - if the compiler reports error in a file then doubleclick onto
           the filename to display the file in the external editor.
           
Version 1.1.6
           24.12.2003 SH - added dialog to find missing files.
Version 1.1.5
           06.09.2003 SH - bugfix for resource files.
Version 1.1.4
           05.06.2003 SH - bugfix in WinExecAndWait32V2
-----------------------------------------------------------------------------}

unit MainFrm;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  ActnList,
  StdCtrls,
  Buttons,
  Menus,
  Grids,
  uDPTSettings,
{$ifdef withTrace}
  cNVBTraceFile,
{$endif}  
  uDPTAppExec,
  ExtCtrls,
  uDPTDelphiPackage,
  uDPTDefinitions;

type

  TFrmMain = class(TForm)
    ActionList1: TActionList;
    actGetPackageListFromRegistry: TAction;
    actOpenProject: TAction;
    actSelectDelphiCompiler: TAction;
    MainMenu1: TMainMenu;
    B1: TMenuItem;
    P1: TMenuItem;
    U1: TMenuItem;
    D1: TMenuItem;
    C1: TMenuItem;
    I1: TMenuItem;
    UninstallAllPackages1: TMenuItem;
    Deleteallfiles1: TMenuItem;
    CompileAllPackages1: TMenuItem;
    InstallAllPackages1: TMenuItem;
    T1: TMenuItem;
    F1: TMenuItem;
    mitOpenFile: TMenuItem;
    ReInstallAllPackages1: TMenuItem;
    mmoLogFile: TMemo;
    OpenDialog1: TOpenDialog;
    actShowOptions: TAction;
    O1: TMenuItem;
    Options1: TMenuItem;
    CloseDelphi1: TMenuItem;
    StartDelphi1: TMenuItem;
    actCheckDelphiRunning: TAction;
    stgFiles: TStringGrid;
    actShowTraceFile: TAction;
    actShowTraceFile1: TMenuItem;
    PopupMenu1: TPopupMenu;
    ClearLog1: TMenuItem;
    About1: TMenuItem;
    actShowAbout: TAction;
    LoadSkin1: TMenuItem;
    OpenSkinDialog: TOpenDialog;
    actFindFilePath: TAction;
    Find1: TMenuItem;
    pnlTop: TPanel;
    btnStart: TBitBtn;
    cbxStartDelphi: TCheckBox;
    cbxSilentMode: TCheckBox;
    cbxStopOnFailure: TCheckBox;
    cbxDelphiVersions: TComboBox;
    lblDelphiVersion: TLabel;
    lblPackageDirectory: TLabel;
    edtPackageBPLDirectory: TEdit;
    lblPackageGroupFile: TLabel;
    btnLoadFile: TButton;
    Find2: TMenuItem;
    actOpenProjectWithDelphi: TAction;
    ppmFilesGrid: TPopupMenu;
    OpenProjectinIDE1: TMenuItem;
    actShowFile: TAction;
    ShowFile1: TMenuItem;
    Splitter1: TSplitter;
    actCompileSelectedProjects: TAction;
    actCompileSelectedProjects1: TMenuItem;
    btnSetPackagePath: TButton;
    actSelectPackageBPLPath: TAction;
    actApplicationUpdate: TAction;
    WebUpdate1: TMenuItem;
    VersionHistory1: TMenuItem;
    InstallPackageinIDE1: TMenuItem;
    UninstallPackagefromIDE1: TMenuItem;
    ExecuteApplication1: TMenuItem;
    SaveDialog1: TSaveDialog;
    actShowBPGEditor: TAction;
    BPGEditor1: TMenuItem;
    PackageGroupEditor1: TMenuItem;
    actNewBPGFile: TAction;
    NewPackageGroup1: TMenuItem;
    actShowCFGFile: TAction;
    Showcfgfile1: TMenuItem;
    actShowProjectDir: TAction;
    FileExplorer1: TMenuItem;
    actRemoveProject: TAction;
    RemoveProjectPackage1: TMenuItem;
    actCloseProject: TAction;
    actCloseProject1: TMenuItem;
    actExit: TAction;
    N2: TMenuItem;
    Exit1: TMenuItem;
    actFindDCPandBPL: TAction;
    actFindDCPandBPL1: TMenuItem;
    mitRecentFiles: TMenuItem;
    actResetDelphi: TAction;
    actCleanUpDelphi1: TMenuItem;
    actVerifyRegistry: TAction;
    actVerifyRegistry1: TMenuItem;
    edtDCUPath: TEdit;
    btnSelectDcuPath: TButton;
    lblDcuPath: TLabel;
    actSelectDcuPath: TAction;
    actShowDOFFile: TAction;
    actShowDOFFile1: TMenuItem;
    actSaveLog: TAction;
    actSaveLog1: TMenuItem;
    actBackupAll: TAction;
    actBackupAll1: TMenuItem;
    actShowOutputDir: TAction;
    ShowOutputDirectory1: TMenuItem;
    actResetDelphi1: TMenuItem;
    edtPackageBPGFile: TComboBox;
    ShowProjectGroup1: TMenuItem;
    actAutoBackup: TAction;
    actRecompileAll: TAction;
    procedure FormShow(Sender: TObject);
    procedure actOpenProjectExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actShowOptionsExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actShowTraceFileExecute(Sender: TObject);
    procedure ClearLog1Click(Sender: TObject);
    procedure actShowAboutExecute(Sender: TObject);
    procedure stgFilesDblClick(Sender: TObject);
    procedure actFindFilePathExecute(Sender: TObject);
    procedure mmoLogFileDblClick(Sender: TObject);
    procedure cbxDelphiVersionsChange(Sender: TObject);
    procedure actOpenProjectWithDelphiExecute(Sender: TObject);
    procedure actShowFileExecute(Sender: TObject);
    procedure actCompileSelectedProjectsExecute(Sender: TObject);
    procedure actSelectPackageBPLPathExecute(Sender: TObject);
    procedure actApplicationUpdateExecute(Sender: TObject);
    procedure VersionHistory1Click(Sender: TObject);
    procedure stgFilesClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure actShowBPGEditorExecute(Sender: TObject);
    procedure actNewBPGFileExecute(Sender: TObject);
    procedure actShowCFGFileExecute(Sender: TObject);
    procedure actShowProjectDirExecute(Sender: TObject);
    procedure edtPackageBPLDirectoryExit(Sender: TObject);
    procedure actRemoveProjectExecute(Sender: TObject);
    procedure ProjectSettingsError(Sender: TObject; ErrorMsg: String;Id: Integer);
    procedure actCloseProjectExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actFindDCPandBPLExecute(Sender: TObject);
    procedure mitRecentFilesClick(Sender: TObject);
    procedure RecentFilesClick(Sender: TObject);
    procedure actVerifyRegistryExecute(Sender: TObject);
    procedure actSelectDcuPathExecute(Sender: TObject);
    procedure actShowDOFFileExecute(Sender: TObject);
    procedure actSaveLogExecute(Sender: TObject);
    procedure actBackupAllExecute(Sender: TObject);
    procedure actShowOutputDirExecute(Sender: TObject);
    procedure cbxStopOnFailureExit(Sender: TObject);
    procedure cbxSilentModeExit(Sender: TObject);
    procedure cbxStartDelphiExit(Sender: TObject);
    procedure edtPackageBPGFileChange(Sender: TObject);
    procedure ShowProjectGroup1Click(Sender: TObject);
    procedure actRecompileAllExecute(Sender: TObject);
  private
    FExternalEditorFilename:string;
    FExternalEditorLineNo:Integer;
    procedure LoadBPG(_filename:string); // load a BPG-File.
    procedure OpenBPGFileInEditor;
    procedure SetCurrentPackage(const _ProjectName:string);
    procedure PrepareGrid;
    procedure ResetGrid;
    procedure WriteLog(_msg: string;_params:array of const);
    procedure ApplicationSettingstoGUI;  // copy settings into GUI fields.
    procedure GUItoApplicationSettings;  // copy GUI fields into settings.
    procedure ProjectSettingstoGUI;  // copy settings into GUI fields.
    procedure GUItoProjectSettings;  // copy GUI fields into settings.
    procedure SearchFileSelected;
    procedure SearchFile(_filename:string;_lineno:integer;_compilerOutput:string);
    procedure PrepareRecentFiles;
    procedure SetDelphiVersionCombobox(const _DelphiVersion:integer);
    procedure PrepareHint;
    procedure FillProjectGrid;
    procedure DoWriteLog(Sender:TObject;const _msg:string);
    procedure DoDelphiVersionChangeEvent(Sender:TObject;const _DelphiVersion:integer);
    procedure DoProjectGroupOpen(Sender:TObject);
    procedure DoProjectGroupClose(Sender:TObject);
    procedure DoApplicationStateChange(Sender:TObject;const _OldState,_NewState:TApplicationState);
    procedure DoPackageInstallEvent(Sender:TObject;const _PackageName:string;const _Message:string;const _ProjectNumber:integer);
    procedure DoPackageUnInstallEvent(Sender:TObject;const _PackageName:string;const _Message:string;const _ProjectNumber:integer);
    procedure DoCurrentProjectCompileStateChanged(Sender:TObject;const _ProjectName:string;const _CompileState:string;const _CompileDateTime:string;const _ProjectVersion:string;const _ProjectSize:string;const _ProjectNumber:integer;const _Description:string);
    procedure DoCurrentProjectChanged(Sender:TObject;const _ProjectName:string;const _ProjectNumber:integer);
  public
    NVBAppExecExternalCommand: TNVBAppExec;
    procedure SetLastUsedFile(_filename: string);
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses
  OptionsFrm,
  uDPTMisc,
  uDPTStringGridExt,
  AboutFrm,
  PathSelectionFrm,
  FileCtrl,
  ShellAPI,
  BPGEditorFrm,
  StartUpInfoFrm,
  BPLSearchFrm,
  MainDM;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.actSelectBPGFileExecute
  Author:    herzogs2
  Date:      22-Aug-2002
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actOpenProjectExecute(Sender: TObject);
resourcestring
cPleaseSelectProjectGroup='Please select a Package Group File <.bpg/.bdsgroup>.';
cFilter='Delphi Group Files|*.bpg;*.groupproj;*.bdsgroup';
begin
  OpenDialog1.InitialDir:=ExtractFilePath(DMMain.ApplicationSettings.StringValue('Application/LastUsedInputFile', 19));
  OpenDialog1.Title := cPleaseSelectProjectGroup;
  OpenDialog1.DefaultExt := DMMain.ApplicationSettings.StringValue('Application/LastUsedExtension',27);
  OpenDialog1.Filter := cFilter;
  OpenDialog1.FileName := '';
  OpenDialog1.FilterIndex := 3;
  if not OpenDialog1.Execute then exit;
  DMMain.ApplicationSettings.SetString('Application/LastUsedExtension',27,OpenDialog1.DefaultExt);
  LoadBPG(OpenDialog1.FileName);
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.FormShow
  Author:    herzogs2
  Date:      05-Dez-2002
  Arguments: Sender: TObject
  Result:    None
  Purpose:
  History: 05.12.2002 SH - check if the package group file exits. If not, show the Opendialog.
                         - check if the compiler file exits. If not, show the OpenDialog.
-----------------------------------------------------------------------------}
procedure TFrmMain.FormShow(Sender: TObject);
var
  _showagain:boolean;
begin
  caption:='Package Group Rebuilder/Installer '+Getversion;
  left                              :=DMMain.ApplicationSettings.IntegerValue('Application/Position/Left',23);
  top                               :=DMMain.ApplicationSettings.IntegerValue('Application/Position/Top',24);
  width                             :=DMMain.ApplicationSettings.IntegerValue('Application/Position/Width',25);
  height                            :=DMMain.ApplicationSettings.IntegerValue('Application/Position/Height',26);
  ApplicationSettingstoGUI;
  PrepareGrid;
  Application.ProcessMessages;
  _showagain:=DMMain.ApplicationSettings.BoolValue('Application/ShowStartUpWarning', 10);
  if _showagain then begin
    ShowStartUpDlg(_showagain);
    DMMain.ApplicationSettings.SetBoolean('Application/ShowStartUpWarning', 10,_showagain);
  end;
  if DMMain.BPGFilename<>'' then LoadBPG(DMMain.BPGFilename)
  else begin
    if DMMain.ApplicationSettings.FileValue('Application/ProjectGroupFile', 3)<>'' then begin
      LoadBPG(DMMain.ApplicationSettings.FileValue('Application/ProjectGroupFile', 3));
    end;
  end;
  if assigned(DMMain.CommandLineAction) then begin
    DMMain.CommandLineAction.Execute;
    if exitcode=0 then close;
  end;  
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.FormCreate
  Author:    herzogs2
  Date:      29-Aug-2002
  Arguments: Sender: TObject
  Result:    None
  Description: analyze commandline parameters if exists.
-----------------------------------------------------------------------------}
procedure TFrmMain.FormCreate(Sender: TObject);
begin
  NVBAppExecExternalCommand := TNVBAppExec.Create(Self);
  NVBAppExecExternalCommand.Name := 'NVBAppExecExternalCommand';
  NVBAppExecExternalCommand.Wait := True;
  NVBAppExecExternalCommand.WindowState := wsNormal;
  NVBAppExecExternalCommand.Priority := ppNormal;
  NVBAppExecExternalCommand.CloseRunningProcess := False;
  DMMain.OnWriteLog:=DoWriteLog;
  DMMain.OnDelphiVersionChange:=DoDelphiVersionChangeEvent;
  DMMain.OnBPGOpen:=DoProjectGroupOpen;
  DMMain.OnBPGClose:=DoProjectGroupClose;
  DMMain.OnApplicationStateChange:=DoApplicationStateChange;
  DMMain.OnPackageInstalledEvent:=DoPackageInstallEvent;
  DMMain.OnPackageUnInstalledEvent:=DoPackageUninstallEvent;
  DMMain.OnCurrentProjectCompileStateChanged:=DoCurrentProjectCompileStateChanged;
  DMMain.OnCurrentProjectChanged:=DoCurrentProjectChanged;
  actBackupAll.Enabled:=fileexists(DMMain.ZipFilename);
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.actShowOptionsExecute
  Author:    Samuel Herzog
  Date:
  Arguments: Sender: TObject
  Result:    None
  Description: show the options dialog.
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowOptionsExecute(Sender: TObject);
var
_FrmOptions: TFrmOptions;
begin
  _FrmOptions := TFrmOptions.create(nil);
  try
    _FrmOptions.showmodal;
    FCreateBatchFile:=DMMain.ProjectSettings.BoolValue('Application/CreateInstallBatch',4);
  finally
    _FrmOptions.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.FormClose
  Author:    herzogs2
  Date:      05-Dez-2002
  Arguments: Sender: TObject; var Action: TCloseAction
  Result:    None
  Purpose:
  History: 05.12.2002 -check if the directory exits before trying to save the file.
           06.05.2005 - do not write a <.log> file if filename is empty.
-----------------------------------------------------------------------------}
procedure TFrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
_filename:string;
begin
  GUItoProjectSettings;
  GUItoApplicationSettings;
  DMMain.CloseBPG;
  DMMain.OnWriteLog:=nil;
  DMMain.OnDelphiVersionChange:=nil;
  if not DirectoryExists(DMMain.BPGPath) then exit;
  _filename:=ChangeFileExt(DMMain.BPGFilename, '.log');
  try
    if ExtractFilenameOnly(DMMain.BPGFilename)='' then exit;
    mmoLogFile.Lines.SaveToFile(_filename);
    trace(5,'Wrote Logfile <%s>.',[_filename]);
  except
    trace(1,'Problem in FormClose: Could not write Logfile <%s>.',[_filename]);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.PrepareGrid
  Author:    Sam
  Date:      06-Sep-2003
  Arguments: None
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.PrepareGrid;
resourcestring
cColNo='No.';
cColProject='Project/Package';
cColState='State';
cColLastCompileDate='Last Compile Date';
cColIDEInstall='IDE Install';
cColFileVersion='File Version';
cColFileSize='File Size (MByte)';
cColDescription='Description';
begin
  stgFiles.FixedRows:=1;
  stgFiles.RowCount:=2;
  stgFiles.Cells[0, 0] := cColNo;
  stgFiles.Cells[1, 0] := cColProject;
  stgFiles.Cells[2, 0] := cColState;
  stgFiles.Cells[3, 0] := cColLastCompileDate;
  stgFiles.Cells[4, 0] := cColIDEInstall;
  stgFiles.Cells[5, 0] := cColFileVersion;
  stgFiles.Cells[6, 0] := cColFileSize;
  stgFiles.Cells[7, 0] := cColDescription;
end;

procedure TFrmMain.actShowTraceFileExecute(Sender: TObject);
begin
  {$ifdef withTrace}
  DMMain.NVBTraceFile.ShowFile;
  {$endif}
end;

procedure TFrmMain.ClearLog1Click(Sender: TObject);
begin
  mmoLogFile.Clear;
end;

procedure TFrmMain.WriteLog(_msg: string;_params:array of const);
begin
  if length(_params)<>0 then _msg:=format(_msg,_params);
  mmoLogFile.Lines.add(_msg);
  trace(5, _msg, []);
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.actShowAboutExecute
  Author:    Sam
  Date:      06-Sep-2003
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowAboutExecute(Sender: TObject);
begin
  TFrmAbout.ShowDialog('About Dialog','','');
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.stgFilesDblClick
  Author:    Sam
  Date:      05-Sep-2003
  Arguments: Sender: TObject
  Result:    None
  Description: start external editor.
-----------------------------------------------------------------------------}
procedure TFrmMain.stgFilesDblClick(Sender: TObject);
begin
  actShowFile.execute;
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.ResetGrid
  Author:    Sam
  Date:      07-Sep-2003
  Arguments: None
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.ResetGrid;
var
i:integer;
begin
  stgfiles.RowCount:=2;
  for i:=0 to stgfiles.colcount-1 do stgfiles.Cells[i,1]:='';
end;


{-----------------------------------------------------------------------------
  Procedure: actFindFilePathExecute
  Author:    sam
  Date:      15-Feb-2005
  Arguments: Sender: TObject
  Result:    None
  Description: show search path selection dialog
-----------------------------------------------------------------------------}
procedure TFrmMain.actFindFilePathExecute(Sender: TObject);
begin
  ShowSelectPathDialog(DMMain.ApplicationSettings.StringValue('Application/LastUsedSearchPath',15),'',true);
end;

{-----------------------------------------------------------------------------
  Procedure: TFrmMain.mmoLogFileDblClick
  Author:    Samuel Herzog
  Date:      24-Dez-2003
  Arguments: Sender: TObject
  Result:    None
  Description: if a file was not found then try to find it
-----------------------------------------------------------------------------}
procedure TFrmMain.mmoLogFileDblClick(Sender: TObject);
begin
  SearchFileSelected;
end;

{-----------------------------------------------------------------------------
  Procedure: cbxDelphiVersionsChange
  Author:    sam
  Date:      15-Jul-2004
  Arguments: Sender: TObject
  Result:    None
  Description: if the user changes the delphi version.
-----------------------------------------------------------------------------}
procedure TFrmMain.cbxDelphiVersionsChange(Sender: TObject);
var
_PackagePath:string;
_iDelphiVersion:integer;
_fDelphiVersion:extended;
begin
  _PackagePath:=IncludeTrailingPathDelimiter(edtPackageBPLDirectory.Text);
  StringToFloat(cbxDelphiVersions.Text,_fDelphiVersion);
  _iDelphiVersion:=trunc(_fDelphiVersion);
  DMMain.CurrentDelphiVersion:=_iDelphiVersion;
  DMMain.ApplicationSettings.SetString('Application/PathNameFile', 8, 'DelphiPackageToolPathD' + inttostr(DMMain.CurrentDelphiVersion) + '.txt');
end;

{-----------------------------------------------------------------------------
  Procedure: actOpenProjectWithDelphiExecute
  Author:    sam
  Date:      04-Mai-2004
  Arguments: Sender: TObject
  Result:    None
  Description: open a project in the delphi IDE.
-----------------------------------------------------------------------------}
procedure TFrmMain.actOpenProjectWithDelphiExecute(Sender: TObject);
resourcestring
cIDEIsRunning='Delphi is already running. Do you want to open a second Delphi IDE ?';
cDidNotFindFile='Did not find the file <%s>.';
begin
  if not fileexists(DMMain.CurrentProjectFilename) then begin
    WriteLog(cDidNotFindFile,[DMMain.CurrentProjectFilename]);
    exit;
  end;

  if isDelphiStarted(DMMain.CurrentDelphiVersion) then begin
    if Application.MessageBox(pchar(cIDEIsRunning),'Confirma',MB_ICONQUESTION or MB_YESNO)=IDNo then exit;
  end;
  StartUpDelphi(DMMain.CurrentDelphiVersion, DMMain.CurrentProjectFilename);
end;

{-----------------------------------------------------------------------------
  Procedure: actShowFileExecute
  Author:    sam
  Date:      06-Mai-2004
  Arguments: Sender: TObject
  Result:    None
  Description: show a file in the external editor.
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowFileExecute(Sender: TObject);
begin
  DMMain.ShowFile(DMMain.CurrentProjectFilename,0);
end;

{-----------------------------------------------------------------------------
  Procedure: actCompileSelectedProjectsExecute
  Author:    sam
  Date:      03-Jun-2004
  Arguments: Sender: TObject
  Result:    None
  Description: compile the selected projects.
-----------------------------------------------------------------------------}
procedure TFrmMain.actCompileSelectedProjectsExecute(Sender: TObject);
resourcestring
cAbortedByUser='Aborted by User.';
var
i:integer;
_SelectedRows:TNVBRowArray;
_success:boolean;
_AbortCompile:boolean;
begin
  _AbortCompile:=false;
  _SelectedRows:=GetSelectedRows(stgFiles);
  for i:=0 to length(_SelectedRows)-1 do begin
    SetCurrentPackage(stgFiles.cells[1, _SelectedRows[i]]);
    DMMain.actUninstallPackage.Execute;
    _success:=DMMain.CompilePackage(false);
    if ((not _success) and (cbxStopOnFailure.checked)) then break;
    if _AbortCompile then begin
      writelog(cAbortedByUser,[]);
      break;
    end;
    DMMain.actInstallPackage.Execute;
    Application.ProcessMessages;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: actSelectPackageBPLPathExecute
  Author:    sam
  Date:      03-Jun-2004
  Arguments: Sender: TObject
  Result:    None
  Description: set the delphi bpl-path. Default is Projects\BPL
-----------------------------------------------------------------------------}
procedure TFrmMain.actSelectPackageBPLPathExecute(Sender: TObject);
resourcestring
cSelectBPLFolder='Select BPL-Directory';
var
  _Dir: string;
begin
  _Dir := '';
  if not SelectDirectory(cSelectBPLFolder,'',_Dir) then exit;
  edtPackageBPLDirectory.Text:=RelativePath(DMMain.BPGPath,_Dir,DMMain.CurrentDelphiVersion);
  SetDelphiPackageDir(DMMain.CurrentDelphiVersion,_Dir,DMMain.ApplicationSettings.BoolValue('Application/SilentMode', 5));
  SetCurrentPackage(stgFiles.cells[1, stgFiles.row]);
end;

{-----------------------------------------------------------------------------
  Procedure: actApplicationUpdateExecute
  Author:    sam
  Date:      06-Jul-2004
  Arguments: Sender: TObject
  Result:    None
  Description: 20.09.2005 -SH the install path of the NVBUpdater is <..\NVBUpdater\>.
-----------------------------------------------------------------------------}
procedure TFrmMain.actApplicationUpdateExecute(Sender: TObject);
resourcestring
cCouldNotFindUpdater='Could not find Application <NVBUpdater.exe>. Can not check for a new Version.'+#13+#10+'Do you want to download the Update Application?';
var
_currentVersion:string;
_applicationName:string;
_path1,_path2,_path3:string;
begin
  _currentVersion:=GetVersion;
  _applicationName:=Application.ExeName;
  NVBAppExecExternalCommand.ExeName:='NVBUpdater.exe';
  NVBAppExecExternalCommand.ExePath:='';
  NVBAppExecExternalCommand.Wait:=false;
  NVBAppExecExternalCommand.ExeParams:=format('"-d" -h"www.novabit.ch" -p"/downloads/delphipackagetool/update/" -f"%s;%s" -l"%s"',[_applicationName,'history.txt',_currentVersion]);
  _path1:='..\NVBUpdater\';
  _path2:=extractFilePath(Application.ExeName);
  _path3:=GetSystemPath(ProgFiles)+'Novabit Software\NVBUpdater\';
  if FileExists(_path1+'NVBUpdater.exe') then NVBAppExecExternalCommand.ExePath:=_path1 else
  if FileExists(_path2+'NVBUpdater.exe') then NVBAppExecExternalCommand.ExePath:=_path2 else
  if FileExists(_path3+'NVBUpdater.exe') then NVBAppExecExternalCommand.ExePath:=_path3;

  if NVBAppExecExternalCommand.ExePath<>'' then begin
    NVBAppExecExternalCommand.Execute;
    close;
  end
  else begin
    if Application.MessageBox(pchar(cCouldNotFindUpdater),pchar(cInformation),MB_ICONQUESTION or MB_YESNO)=IDYes then begin
       ShellExecute(Application.Handle,
               PChar('open'),
               PChar('www.novabit.ch/downloads/nvbupdater/setup.exe'),
               PChar(0),
               nil,
               SW_NORMAL);
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: SetCurrentPackage
  Author:    sam
  Date:      15-Jul-2004
  Arguments: _ProjectName: string
  Result:    None
  Description:  gets a project name and converts it to a real name
                and stores the result into FCurrentProjectFilename.

  For instance the bpg file is
              c:\projects\myprojects.bpg

  Examples:   c:\temp\project1.dpr      --->  c:\temp\project1.dpr
              ..\package1.dpk           --->  c:\package1.dpk
              \components\package1.dpk  --->  c:\projects\components\package1.dpk
-----------------------------------------------------------------------------}
procedure TFrmMain.SetCurrentPackage(const _ProjectName: string);
begin
  GUItoApplicationSettings;
  GUItoProjectSettings;
  DMMain.SetCurrentProject(_ProjectName);
  PrepareHint;
end;


procedure TFrmMain.VersionHistory1Click(Sender: TObject);
var
_showagain:boolean;
begin
  ShowStartUpDlg(_showagain);
end;

procedure TFrmMain.stgFilesClick(Sender: TObject);
begin
  if DMMain.ApplicationState<>tas_working then SetCurrentPackage(stgFiles.cells[1, stgFiles.row]);
end;

procedure TFrmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if key=chr(vk_return) then DMMain.actExecuteApp.execute;
  if key=chr(vk_space)  then actShowFile.Execute;
  if key=chr(vk_escape) then DMMain.AbortCompile;
end;

{*-----------------------------------------------------------------------------
  Procedure: SetDelphiVersionCombobox
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: const _DelphiVersion:integer
  Result:    None
  Description: set the correct delphi version in the combobox.
-----------------------------------------------------------------------------}
procedure TFrmMain.SetDelphiVersionCombobox(const _DelphiVersion:integer);
var
_sDelphiVersion:string;
_ItemIndex:integer;
begin
  cbxDelphiVersions.OnChange:=nil;
  cbxDelphiVersions.Items.Assign(DMMain.InstalledDelphiVersions);
  _sDelphiVersion := inttostr(_DelphiVersion)+'.0';
  _itemIndex:=cbxDelphiVersions.Items.IndexOf(_sDelphiVersion);
  if _ItemIndex>-1 then begin
    cbxDelphiVersions.ItemIndex:=_ItemIndex;
    cbxDelphiVersions.Hint:=VersionNoToIDEName(_DelphiVersion);
  end
  else begin
    if cbxDelphiVersions.Items.Count>0 then begin
      cbxDelphiVersions.ItemIndex:=0;
      cbxDelphiVersions.Hint:='';
    end;
  end;
  cbxDelphiVersions.OnChange:=cbxDelphiVersionsChange;
end;

{-----------------------------------------------------------------------------
  Procedure: DMMain.ApplicationSettingstoGUI
  Author:    sam
  Date:      02-Feb-2005
  Arguments: None
  Result:    None
  Description: copy Application settings to GUI Fields
-----------------------------------------------------------------------------}
procedure TFrmMain.ApplicationSettingstoGUI;
begin
  SetDelphiVersionCombobox(DMMain.ApplicationSettings.IntegerValue('Compiler/DelphiVersion', 1));
  edtPackageBPLDirectory.Text       := DMMain.ApplicationSettings.PathValue('Application/PackageOutputPath', 4);
  cbxSilentMode.checked             := DMMain.ApplicationSettings.BoolValue('Application/SilentMode', 5);
  cbxStopOnFailure.checked          := DMMain.ApplicationSettings.BoolValue('Application/StopOnFailure', 6);
  cbxStartDelphi.checked            := DMMain.ApplicationSettings.BoolValue('Application/StartDelphiOnClose', 7);
  PrepareRecentFiles;
end;

{-----------------------------------------------------------------------------
  Procedure:  GUItoApplicationSettings
  Author:    sam
  Date:      02-Feb-2005
  Arguments: None
  Result:    None
  Description: copy GUI fields to Application settings.
-----------------------------------------------------------------------------}
procedure TFrmMain.GUItoApplicationSettings;
begin
  DMMain.ApplicationSettings.SetInteger('Compiler/DelphiVersion', 1, DMMain.CurrentDelphiVersion);
  DMMain.ApplicationSettings.SetFile('Application/ProjectGroupFile', 3, edtPackageBPGFile.Text);
  DMMain.ApplicationSettings.SetPath('Application/PackageOutputPath', 4, edtPackageBPLDirectory.Text);
  DMMain.ApplicationSettings.SetBoolean('Application/SilentMode', 5, cbxSilentMode.checked);
  DMMain.ApplicationSettings.SetBoolean('Application/StopOnFailure', 6, cbxStopOnFailure.checked);
  DMMain.ApplicationSettings.SetBoolean('Application/StartDelphiOnClose', 7, cbxStartDelphi.checked);
{$ifdef withTrace}
  DMMain.ApplicationSettings.SetInteger('Application/Tracelevel',12,DMMain.NVBTraceFile.Level);
{$endif}  
  DMMain.ApplicationSettings.SetPath('Application/DCUOutputPath', 20,edtDcuPath.Text);
  DMMain.ApplicationSettings.SetInteger('Application/Position/Left',23,left);
  DMMain.ApplicationSettings.SetInteger('Application/Position/Top',24,top);
  DMMain.ApplicationSettings.SetInteger('Application/Position/Width',25,width);
  DMMain.ApplicationSettings.SetInteger('Application/Position/Height',26,height);
end;

{-----------------------------------------------------------------------------
  Procedure: GUItoProjectSettings
  Author:    sam
  Date:      03-Mrz-2006
  Arguments: None
  Result:    None
  Description: copy the settings from the gui to the project settings
-----------------------------------------------------------------------------}
procedure TFrmMain.GUItoProjectSettings;
begin
  if not DMMain.ProjectSettings.isLoaded then exit;
  DMMain.ProjectSettings.SetInteger('Application/DelphiVersion',5, DMMain.CurrentDelphiVersion);
  DMMain.ProjectSettings.SetPath('Application/PackageOutputPath',6,edtPackageBPLDirectory.Text);
  DMMain.ProjectSettings.SetPath('Application/DCUOutputPath',7,edtDcuPath.Text);
end;

{-----------------------------------------------------------------------------
  Procedure: ProjectSettingstoGUI
  Author:    sam
  Date:      03-Mrz-2006
  Arguments: None
  Result:    None
  Description: copy the project setting to the gui components.
-----------------------------------------------------------------------------}
procedure TFrmMain.ProjectSettingstoGUI;
begin
  if DMMain.ProjectSettings.PathValue('Application/PackageOutputPath', 6)<>'' then edtPackageBPLDirectory.Text:= DMMain.ProjectSettings.PathValue('Application/PackageOutputPath', 6);
  if DMMain.ProjectSettings.PathValue('Application/DCUOutputPath', 7)<>''     then edtDcuPath.Text:= DMMain.ProjectSettings.PathValue('Application/DCUOutputPath', 7);
  SetDelphiVersionCombobox(DMMain.ProjectSettings.IntegerValue('Application/DelphiVersion',5));
end;

{-----------------------------------------------------------------------------
  Procedure: LoadBPG
  Author:    sam
  Date:      09-Feb-2005
  Arguments: _filename: string
  Result:    None
  Description: load a bpg-file
-----------------------------------------------------------------------------}
procedure TFrmMain.LoadBPG(_filename: string);
begin
  if not fileexists(_filename) then begin
    trace(1,'Problem in TFrmMain.LoadBPG: Could not find file <%s>.',[_filename]);
    exit;
  end;
  ClearLog1.Click;
  DMMain.CloseBPG;
  DMMain.OpenBPG(AbsoluteFilename(ExtractFilePath(Application.ExeName),_filename));
end;

{-----------------------------------------------------------------------------
  Procedure: OpenBPGFileInEditor
  Author:    sam
  Date:      10-Mrz-2005
  Arguments: None
  Result:    None
  Description: show the bpg-file in the editor.
-----------------------------------------------------------------------------}
procedure TFrmMain.OpenBPGFileInEditor;
begin
  DMMain.ShowFile(DMMain.BPGFilename,0);
end;

{-----------------------------------------------------------------------------
  Procedure: actShowBPGEditorExecute
  Author:    sam
  Date:      14-Mrz-2005
  Arguments: Sender: TObject
  Result:    None
  Description: display the bpg-editor.
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowBPGEditorExecute(Sender: TObject);
var
_bpgFilename:string;
begin
  GUItoProjectSettings;
  _bpgFilename:=DMMain.BPGFilename;
  DMMain.CloseBPG;
  _bpgFilename:=ShowBPGEditor(_bpgFilename);
  LoadBPG(_bpgFilename);
end;

{-----------------------------------------------------------------------------
  Procedure: actNewBPGFileExecute
  Author:    sam
  Date:      14-Mrz-2005
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actNewBPGFileExecute(Sender: TObject);
resourcestring
cPleaseDefineFilename='Please define a filename for the Package Group file <.bpg/.groupproj/.bdsgroup>.';
var
_bpgFilename:string;
begin
  SaveDialog1.Title:=cPleaseDefineFilename;
  SaveDialog1.InitialDir  := DMMain.ApplicationSettings.FileValue('Application/ProjectGroupFile', 3);
  SaveDialog1.FileName    := 'NewPackageGroup';
  case DMMain.CurrentDelphiVersion of
    1,2,3,4,5,6,7:begin
             SaveDialog1.Filter      := '*.bpg|*.bpg';
             SaveDialog1.DefaultExt  := '.bpg';
           end;
    8,9,10:begin
             SaveDialog1.Filter      := '*.bdsgroup|*.bdsgroup';
             SaveDialog1.DefaultExt  := '.bdsgroup';
           end;
    else   begin
             SaveDialog1.Filter      := '*.groupproj|*.groupproj';
             SaveDialog1.DefaultExt  := '.groupproj';
           end;
  end;
  SaveDialog1.FilterIndex:=0;
  if not SaveDialog1.Execute then exit;
  _bpgFilename:=ShowBPGEditor(SaveDialog1.FileName);
  LoadBPG(_bpgFilename);
end;

{-----------------------------------------------------------------------------
  Procedure: actShowCFGFileExecute
  Author:    sam
  Date:      16-Mrz-2005
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowCFGFileExecute(Sender: TObject);
begin
  case DMMain.CurrentDelphiVersion of
    5,6,7,8:DMMain.ShowFile(changeFileExt(DMMain.CurrentProjectFilename,'.cfg'),0);
    else begin
      if fileexists(changeFileExt(DMMain.CurrentProjectFilename,'.bdsproj')) then DMMain.ShowFile(changeFileExt(DMMain.CurrentProjectFilename,'.bdsproj'),0)
      else  DMMain.ShowFile(changeFileExt(DMMain.CurrentProjectFilename,'.cfg'),0);
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: actShowFileExplorerExecute
  Author:    sam
  Date:      16-Mrz-2005
  Arguments: Sender: TObject
  Result:    None
  Description: open the file explorer and display the current project
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowProjectDirExecute(Sender: TObject);
begin
  DMMain.ShowProjectDir;
end;

{-----------------------------------------------------------------------------
  Procedure: edtPackageBPLDirectoryExit
  Author:    sam
  Date:      17-Mrz-2005
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.edtPackageBPLDirectoryExit(Sender: TObject);
begin
  SetDelphiPackageDir(DMMain.CurrentDelphiVersion,TEdit(Sender).Text,DMMain.ApplicationSettings.BoolValue('Application/SilentMode', 5));
  SetCurrentPackage(stgFiles.cells[1, stgFiles.row]);
end;

{-----------------------------------------------------------------------------
  Procedure: actRemoveProjectExecute
  Author:    sam
  Date:      05-Nov-2005
  Arguments: Sender: TObject
  Result:    None
  Description: remove the currently marked project/package from the .bpg-file.
-----------------------------------------------------------------------------}
procedure TFrmMain.actRemoveProjectExecute(Sender: TObject);
resourcestring
cDoYouReallyWanttoRemove='Do you really want to remove the Project/Package <%s>?';
cReadOnlyFile='The file <%s> is marked as read-only. Do you want to change it anyway ?';
cProblemToRemoveProject='Problem to remove the Project <%s>. See trace file for more info.';
var
_currentRow:integer;
begin
  if not fileexists(DMMain.BPGFilename) then exit;
  if Application.MessageBox(pchar(format(cDoYouReallyWanttoRemove,[DMMain.CurrentProjectFilename])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDNo then exit;
  if IsFileReadOnly(DMMain.BPGFilename) then begin
    if Application.MessageBox(pchar(format(cReadOnlyFile,[DMMain.BPGFilename])),pchar(cWarning),MB_ICONQUESTION or MB_YESNO)=IDNo then exit;
    if not RemoveReadOnlyFlag(DMMain.BPGFilename,true) then exit;
  end;
  _currentRow:=stgFiles.row;
  if DMMain.RemoveProjectFromProjectGroup then begin
    LoadBPG(DMMain.BPGFilename);  // reload the file.
    if _currentRow<stgFiles.RowCount then stgFiles.Row:=_currentRow
                                     else stgFiles.Row:=stgFiles.RowCount-1;
  end else Application.MessageBox(pchar(format(cProblemToRemoveProject,[DMMain.CurrentProjectFilename])),pchar(cError),MB_ICONERROR or MB_OK);
end;

{-----------------------------------------------------------------------------
  Procedure: SearchFileSelected
  Author:    sam
  Date:      17-Dez-2005
  Arguments: None
  Result:    None
  Description: the user selects the filename and doubleclicks
-----------------------------------------------------------------------------}
procedure TFrmMain.SearchFileSelected;
var
_Seltext:string;
_pos:integer;
_fileExt:string;
_LineNo:string;
begin
  FExternalEditorFilename:='';
  FExternalEditorLineNo:=0;
  _Seltext:=mmoLogFile.SelText;
  if not IsFilenameValid(_SelText) then begin
    _pos:=Pos('''',_SelText);
    if _pos>0 then begin
      while _pos>0 do begin
        delete(_SelText,_pos,1);
        _pos:=Pos('''',_SelText);
      end;
    end else begin
      _pos:=Pos('(',_SelText);
      if _pos>0 then begin
        _SelText:=Copy(_SelText,1,_pos-1);
        _pos:=Pos(''+#$D,_SelText);
        while _pos>0 do begin
          delete(_SelText,_pos,1);
          _pos:=Pos(''+#$D,_SelText);
        end;
      end;
    end;
  end;
  FExternalEditorFilename:=trim(_SelText);
  _fileExt:=ExtractFileExt(FExternalEditorFilename);
  _pos:=pos('(',_FileExt);
  if _pos>0 then begin
    _FileExt:=copy(_FileExt,1,_pos-1);
    FExternalEditorFilename:=ExtractFilenameOnly(FExternalEditorFilename)+_FileExt;
    _fileExt:=ExtractFileExt(trim(_SelText));
    _lineNo:=copy(_FileExt,_pos+1,length(_FileExt)-_pos-1);
    StringToInteger(_lineNo,FExternalEditorLineNo);
    trace(5,'TFrmMain.SearchFileSelected: Extracted LineNo <%d>.',[FExternalEditorLineNo]);
  end;
  trace(5,'TFrmMain.SearchFileSelected: Extracted Filename <%s>.',[FExternalEditorFilename]);
  SearchFile(FExternalEditorFilename,FExternalEditorLineNo,mmoLogFile.Text);
end;

{-----------------------------------------------------------------------------
  Procedure: SearchFile
  Author:    sam
  Date:      17-Dez-2005
  Arguments: _filename:string
  Result:    None
  Description: shows the search dialog to add a search path.
-----------------------------------------------------------------------------}
procedure TFrmMain.SearchFile(_filename:string;_lineno:integer;_compilerOutput:string);
begin
  _filename:=lowercase(_filename);
  if ExtractFileExt(_filename)='' then _filename:=_filename+'*.*';
  if (Pos('nicht gefunden',_compilerOutput)>0) or  //TODO we need a better way to find out if the compilation was successfull.
     (Pos('not found',_compilerOutput)>0) then ShowSelectPathDialog(DMMain.ApplicationSettings.StringValue('Application/LastUsedSearchPath',15),_filename,true) else
  if (Pos('.dpr',_filename)>0) or
     (Pos('.dpk',_filename)>0) or
     (Pos('.pas',_filename)>0) then begin
    if Pos('<',_filename)=1 then Delete(_filename,1,1);
    if Pos('>.',_filename)=length(_filename)-1 then Delete(_filename,length(_filename)-1,2);
    _filename:=AbsoluteFilename(DMMain.BPGPath,_filename);
    DMMain.ShowFile(_filename,_lineNo);
  end;
end;


procedure TFrmMain.ProjectSettingsError(Sender: TObject; ErrorMsg: String;Id: Integer);
begin
  trace(5,'%s',[ErrorMsg]);
end;

procedure TFrmMain.actCloseProjectExecute(Sender: TObject);
begin
  DMMain.CloseBPG;
end;

procedure TFrmMain.actExitExecute(Sender: TObject);
begin
  close;
end;

{-----------------------------------------------------------------------------
  Procedure: actFindDCPandBPLExecute
  Author:    sam
  Date:      18-Mrz-2006
  Arguments: Sender: TObject
  Result:    None
  Description: search for the .dcp and .bpl file of the current package.
-----------------------------------------------------------------------------}
procedure TFrmMain.actFindDCPandBPLExecute(Sender: TObject);
begin
  ShowBPLSearchDialog(DMMain.ApplicationSettings.StringValue('Application/LastUsedSearchPath',15),ExtractFilenameOnly(DMMain.CurrentProjectFilename));
end;

{-----------------------------------------------------------------------------
  Procedure: RecentFilesClick
  Author:    sam
  Date:      24-Jul-2005
  Arguments: Sender: TObject
  Result:    None
  Description: handler if a recent file is choosen in the open dialog.
-----------------------------------------------------------------------------}
procedure TFrmMain.RecentFilesClick(Sender: TObject);
var
_filename:string;
_pos:integer;
begin
  _filename:=TMenuItem(Sender).Caption;
  if not fileexists(_filename) then begin
    _pos:=Pos('&',_filename);
    if _pos<>0 then Delete(_filename,_pos,1);
  end;  
  LoadBPG(_filename);
end;

{-----------------------------------------------------------------------------
  Procedure: PrepareRecentFiles
  Author:    sam
  Date:      24-Jul-2005
  Arguments: None
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.PrepareRecentFiles;
var
i:integer;
_filename:string;
_newMenuitem:TMenuItem;
_existingMenuitem:TMenuItem;
begin
// emtpy the current items.
  while mitOpenFile.Count>1 do begin
    _existingMenuitem:=mitOpenFile.Items[1];
    _existingMenuitem.Free;
  end;
// empty the file combobox.
  edtPackageBPGFile.Clear;
  edtPackageBPGFile.Items.add('');
// reload the new items.
  for i:=1 to 10 do begin // load recent used file history
    _filename:=DMMain.ApplicationSettings.StringValue(format('Application/FileHistory/Item%d',[i]),50+i);
    if _filename='' then continue;
    if not FileExists(_filename) then continue;
    _newMenuitem:=TMenuItem.Create(nil);
    _newMenuitem.Caption:=_filename;
    _newMenuitem.OnClick:=RecentFilesClick;
    mitOpenFile.Add(_newMenuitem);
    edtPackageBPGFile.Items.Add(_filename);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: SetLastUsedFile
  Author:    sam
  Date:      24-Jul-2005
  Arguments: _filename: string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.SetLastUsedFile(_filename: string);
var
i:integer;
_tmp:string;
_settingID:Integer;
begin
  DMMain.ApplicationSettings.SetString('Application/LastUsedInputFile', 19, _filename);
  _settingID:=DMMain.ApplicationSettings.FindStringIndex('Application/FileHistory/',_filename);
  if _settingID=-1 then begin   // check if the file is not already in the history list.
    for i:=10 downto 2 do begin
      _tmp:=DMMain.ApplicationSettings.StringValue(format('Application/FileHistory/Item%d',[i-1]),50+i-1);
      DMMain.ApplicationSettings.SetString(format('Application/FileHistory/Item%d',[i]),50+i,_tmp);
    end;
  end
  else begin // the file is already in the recent file list.
    DMMain.ApplicationSettings.SetString('',_settingID,'');
    for i:=_settingID-50 downto 2 do begin
      _tmp:=DMMain.ApplicationSettings.StringValue(format('Application/FileHistory/Item%d',[i-1]),50+i-1);
      DMMain.ApplicationSettings.SetString(format('Application/FileHistory/Item%d',[i]),50+i,_tmp);
    end;
  end;
  DMMain.ApplicationSettings.SetString('Application/FileHistory/Item1',51,_filename);
  PrepareRecentFiles;
end;

procedure TFrmMain.mitRecentFilesClick(Sender: TObject);
begin
  OpenDialog1.InitialDir:=ExtractFilePath(DMMain.ApplicationSettings.StringValue('Application/LastUsedInputFile',19));
  if not OpenDialog1.Execute then exit;
  LoadBPG(OpenDialog1.filename);
end;


{-----------------------------------------------------------------------------
  Procedure: actVerifyRegistryExecute
  Author:    sam
  Date:      23-Okt-2006
  Arguments: Sender: TObject
  Result:    None
  Description: check the registry "known packages" if the refrenced file
               really exists. If not then delete the registry key.
-----------------------------------------------------------------------------}
procedure TFrmMain.actVerifyRegistryExecute(Sender: TObject);
resourcestring
cDeletedWrongKeys='Removed some wrong Registry-Keys. See the Trace-File CTRL+T if you need more information.';
cRegistryIsOk='Registry-Entries are ok. Nothing has been changed.';
begin
  if VerifyRegistry(DMMain.CurrentDelphiVersion) then Application.MessageBox(pchar(cDeletedWrongKeys),pchar(cInformation),MB_ICONINFORMATION or MB_OK)
                                                 else Application.MessageBox(pchar(cRegistryIsOk),pchar(cInformation),MB_ICONINFORMATION or MB_OK);
end;

{-----------------------------------------------------------------------------
  Procedure: actSelectDcuPathExecute
  Author:    sam
  Date:      13-Mrz-2007
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actSelectDcuPathExecute(Sender: TObject);
resourcestring
cSelectDCUPath='Select DCU-Path';
var
  _Dir: string;
begin
  _Dir := '';
  if not SelectDirectory(cSelectDCUPath,'',_Dir) then exit;
  edtDcuPath.Text:=RelativePath(DMMain.BPGPath,_Dir,DMMain.CurrentDelphiVersion);
  SetCurrentPackage(stgFiles.cells[1, stgFiles.row]);
end;

{-----------------------------------------------------------------------------
  Procedure: actShowDOFFileExecute
  Author:    HerzogS2
  Date:      06-Jun-2007
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowDOFFileExecute(Sender: TObject);
begin
  DMMain.ShowFile(changeFileExt(DMMain.CurrentProjectFilename,'.dof'),0);
end;

{-----------------------------------------------------------------------------
  Procedure: PrepareHint
  Author:    sam
  Date:      09-Jun-2007
  Arguments: None
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.PrepareHint;
begin
  stgFiles.Hint:='Output Filename    :='+extractfilename(DMMain.CurrentProjectOutputFilename)+#10+#13+
                 'Project Output Path:='+DMMain.CurrentProjectOutputPath+#10+#13+
                 'BPL Output Path    :='+DMMain.CurrentBPLOutputPath+#10+#13+
                 'DCU Output Path    :='+DMMain.CurrentDCUOutputPath;
end;

{-----------------------------------------------------------------------------
  Procedure: actSaveLogExecute
  Author:    sam
  Date:      16-Okt-2007
  Arguments: Sender: TObject
  Result:    None
  Description: save the log-output.
-----------------------------------------------------------------------------}
procedure TFrmMain.actSaveLogExecute(Sender: TObject);
resourcestring
cChooseFileSaveLog='Please choose a filename to save the log-output <.txt>.';
begin
  SaveDialog1.Title:=cChooseFileSaveLog;
  SaveDialog1.InitialDir:=DMMain.ApplicationSettings.PathValue('Application/LastLogOutputPath',21);
  SaveDialog1.DefaultExt := '.txt';
  SaveDialog1.Filter := 'Text-File(.txt)|*.txt';
  SaveDialog1.FileName := DMMain.BPGFilename+'_log.txt';
  SaveDialog1.FilterIndex := 0;
  if not SaveDialog1.Execute then exit;
  mmoLogFile.Lines.SaveToFile(SaveDialog1.filename);
  DMMain.ApplicationSettings.SetPath('Application/LastLogOutputPath',21,extractFilePath(SaveDialog1.filename));
end;

{-----------------------------------------------------------------------------
  Procedure: actBackupAllExecute
  Author:    herzogs2
  Date:      16-Okt-2007
  Arguments: Sender: TObject
  Result:    None
  Description: extracts all filenames mentioned in the log-file and creates a
               zip-file.
-----------------------------------------------------------------------------}
procedure TFrmMain.actBackupAllExecute(Sender: TObject);
resourcestring
cChooseZipFilename='Please choose a filename to save the backup-file <.zip>.';
var
_filename:string;
_path:string;
begin
  _filename:=changefileExt(ExtractFilenameOnly(DMMain.BPGFilename)+'_'+BuildTimeStamp(now),'.zip');
  SaveDialog1.Title:=cChooseZipFilename;
  _path:=extractfilepath(DMMain.BPGFilename)+'backup\';
  if not CreateDirectory(_path) then _path:=DMMain.ProjectSettings.PathValue('Application/LastUsedBackupPath',11);
  SaveDialog1.InitialDir:=_path;
  SaveDialog1.DefaultExt := '.zip';
  SaveDialog1.Filter := 'Zip-File(.zip)|*.zip';
  SaveDialog1.FileName := _filename;
  SaveDialog1.FilterIndex := 0;
  if not SaveDialog1.Execute then exit;
  mmoLogFile.lines.Insert(0,DMMain.CurrentProjectFilename);
  DMMain.SaveBackup(SaveDialog1.FileName,mmoLogFile.lines);
end;

{-----------------------------------------------------------------------------
  Procedure: actShowOutputDirExecute
  Author:    sam
  Date:      08-Nov-2007
  Arguments: Sender: TObject
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.actShowOutputDirExecute(Sender: TObject);
begin
  DMMain.ShowOutputDir;
end;

{*-----------------------------------------------------------------------------
  Procedure: DoWriteLog
  Author:    sam
  Date:      09-Feb-2008
  Arguments: Sender: TObject; const _msg: string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.DoWriteLog(Sender: TObject; const _msg: string);
begin
  WriteLog(_msg,[]);
end;

{*-----------------------------------------------------------------------------
  Procedure: DoDelphiVersionChangeEvent
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject;const _DelphiVersion: integer
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.DoDelphiVersionChangeEvent(Sender: TObject;const _DelphiVersion: integer);
begin
  SetDelphiVersionCombobox(_DelphiVersion);
end;

{*-----------------------------------------------------------------------------
  Procedure: DoProjectGroupClose
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description: this event is fired after the project group has been closed.
-----------------------------------------------------------------------------}
procedure TFrmMain.DoProjectGroupClose(Sender: TObject);
begin
  edtPackageBPGFile.ItemIndex:=0;
  edtPackageBPLDirectory.Text:='';
  edtDcuPath.Text:='';
  ResetGrid;
end;

{*-----------------------------------------------------------------------------
  Procedure: DoProjectGroupOpen
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject
  Result:    None
  Description: this event is fired when the project group is loaded.
-----------------------------------------------------------------------------}
procedure TFrmMain.DoProjectGroupOpen(Sender: TObject);
var
_index:integer;
begin
  FCreateBatchFile:=DMMain.ProjectSettings.BoolValue('Application/CreateInstallBatch',4);
  SetLastUsedFile(DMMain.BPGFilename);
  _index:=edtPackageBPGFile.Items.IndexOf(DMMain.BPGFilename);
  if _index=-1 then begin
    edtPackageBPGFile.Items.add(DMMain.BPGFilename);
    _index:=edtPackageBPGFile.Items.IndexOf(DMMain.BPGFilename);
  end;
  edtPackageBPGFile.ItemIndex:=_index;
  ProjectSettingstoGUI;
  FillProjectGrid;
end;

{*-----------------------------------------------------------------------------
  Procedure: DoApplicationStateChange
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: Sender: TObject;const _OldState, _NewState: TApplicationState
  Result:    None
  Description: is fired when the application state changes.
-----------------------------------------------------------------------------}
procedure TFrmMain.DoApplicationStateChange(Sender: TObject;const _OldState, _NewState: TApplicationState);
begin
  case _NewState of
    tas_init:   stgFiles.Enabled:=false;
    tas_working:stgFiles.Enabled:=false;
    tas_open   :stgFiles.Enabled:=true;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: FillProjectGrid
  Author:    sam
  Date:      12-Mrz-2008
  Arguments: None
  Result:    None
  Description: fill projects into the grid.
-----------------------------------------------------------------------------}
procedure TFrmMain.FillProjectGrid;
var
  i:integer;
begin
  stgFiles.RowCount:=2;
  stgFiles.FixedRows:=1;
  if DMMain.ProjectList.Count=0 then exit;
  for i:=1 to DMMain.ProjectList.Count do begin
    stgFiles.cells[0,i]:=inttostr(i);
    stgFiles.cells[1,i]:=DMMain.ProjectList.Strings[i-1];
    stgFiles.cells[2,i]:='';
    stgFiles.cells[3,i]:='';
    stgFiles.cells[4,i]:='';
    stgFiles.cells[5,i]:='';
    stgFiles.cells[6,i]:='';
    stgFiles.cells[7,i]:='';
  end;
  stgFiles.RowCount:=DMMain.ProjectList.Count+1;
end;

{*-----------------------------------------------------------------------------
  Procedure: DoPackageInstallEvent
  Author:    sam
  Date:      14-Mrz-2008
  Arguments: Sender: TObject;const _PackageName, _Message: string;const _ProjectNumber:integer
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.DoPackageInstallEvent(Sender: TObject;const _PackageName, _Message: string;const _ProjectNumber:integer);
begin
  stgFiles.cells[4,_ProjectNumber+1]:=_Message;
end;

{*-----------------------------------------------------------------------------
  Procedure: DoPackageUnInstallEvent
  Author:    sam
  Date:      14-Mrz-2008
  Arguments: Sender: TObject;const _PackageName, _Message: string;const _ProjectNumber:integer
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.DoPackageUnInstallEvent(Sender: TObject;const _PackageName, _Message: string;const _ProjectNumber:integer);
begin
  stgFiles.cells[4,_ProjectNumber+1]:=_Message;
end;

{*-----------------------------------------------------------------------------
  Procedure: DoCurrentProjectCompileStateChanged
  Author:    sam
  Date:      14-Mrz-2008
  Arguments: Sender:TObject;const _ProjectName:string;const _CompileState:string;const _CompileDateTime:string;const _ProjectVersion:string;const _ProjectSize:string;const _ProjectNumber:integer
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.DoCurrentProjectCompileStateChanged(Sender:TObject;const _ProjectName:string;const _CompileState:string;const _CompileDateTime:string;const _ProjectVersion:string;const _ProjectSize:string;const _ProjectNumber:integer;const _Description:string);
begin
  stgFiles.Cells[2, _ProjectNumber+1] := _CompileState;
  stgFiles.Cells[3, _ProjectNumber+1] := _CompileDateTime;
  stgFiles.Cells[5, _ProjectNumber+1] := _ProjectVersion;
  stgFiles.Cells[6, _ProjectNumber+1] := _ProjectSize;
  stgFiles.Cells[7, _ProjectNumber+1] := _Description;
end;

{*-----------------------------------------------------------------------------
  Procedure: DoCurrentProjectChanged
  Author:    sam
  Date:      13-Mrz-2008
  Arguments: Sender: TObject;const _ProjectName: string; const _ProjectNumber: integer
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TFrmMain.DoCurrentProjectChanged(Sender: TObject;const _ProjectName: string; const _ProjectNumber: integer);
begin
  stgFiles.Row:=_ProjectNumber+1;
end;

procedure TFrmMain.cbxStopOnFailureExit(Sender: TObject);
begin
  DMMain.ApplicationSettings.SetBoolean('Application/StopOnFailure', 6, cbxStopOnFailure.checked);
end;

procedure TFrmMain.cbxSilentModeExit(Sender: TObject);
begin
  DMMain.ApplicationSettings.SetBoolean('Application/SilentMode', 5, cbxSilentMode.checked);
end;

procedure TFrmMain.cbxStartDelphiExit(Sender: TObject);
begin
  DMMain.ApplicationSettings.SetBoolean('Application/StartDelphiOnClose', 7, cbxStartDelphi.checked);
end;

procedure TFrmMain.edtPackageBPGFileChange(Sender: TObject);
begin
  LoadBPG(edtPackageBPGFile.Text);
end;

procedure TFrmMain.ShowProjectGroup1Click(Sender: TObject);
begin
  OpenBPGFileInEditor;;
end;

{-----------------------------------------------------------------------------
  Procedure: actRecompileAllExecute
  Author:    s.herzog
  Date:      24-Feb-2010
  Arguments: Sender: TObject
  Result:    None
  Description: recompile all.
-----------------------------------------------------------------------------}
procedure TFrmMain.actRecompileAllExecute(Sender: TObject);
begin
  if not DMMain.ReCompileAndInstallAll then exit;
  DMMain.AutoSaveBackup(mmoLogFile.lines);
end;

end.

