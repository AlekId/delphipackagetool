; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppVer GetFileVersion("..\Bin\DelphiPackageTool.exe")

[Setup]
AppName=Delphi Package Tool
AppVerName=Delphi Package Tool {#MyAppVer}
DefaultDirName={pf}\Delphi Package Tool
DefaultGroupName=Delphi Package Tool
InfoBeforeFile=..\Bin\readme.txt
InfoAfterFile=
Compression=lzma
SolidCompression=yes
OutputBaseFilename=setup

[Tasks]
; NOTE: The following entry contains English phrases ("Create a desktop icon" and "Additional icons"). You are free to translate them into another language if required.
Name: desktopicon; Description: Create a &desktop icon; GroupDescription: Additional icons:; Flags: unchecked

[Files]
Source: ..\Bin\DelphiPackageTool.exe; DestDir: {app}; Flags: ignoreversion
Source: ..\Bin\SetVersion.exe; DestDir: {app}; Flags: ignoreversion
Source: ..\Bin\ReadMe.txt; DestDir: {app}; Flags: ignoreversion
Source: ..\Bin\Constributors.txt; DestDir: {app}; Flags: ignoreversion
Source: ..\Bin\TheDelphiPackageTool.pdf; DestDir: {app}; Flags: ignoreversion
Source: ..\Bin\Lies_mich.txt; DestDir: {app}; Flags: ignoreversion
Source: ..\doc\create_new_project.swf; DestDir: {app}\doc\video; Flags: ignoreversion
Source: ..\doc\open_in_IDE.swf; DestDir: {app}\doc\video; Flags: ignoreversion
Source: ..\doc\setup_search_path.swf; DestDir: {app}\doc\video; Flags: ignoreversion
Source: ..\Bin\ProjectGroupTemplate.bpg; DestDir: {app}; Flags: ignoreversion
Source: ..\Bin\ProjectGroupTemplate.bdsgroup; DestDir: {app}; Flags: ignoreversion
Source: ..\Bin\ProjectGroupTemplate.groupproj; DestDir: {app}; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: {group}\Delphi Package Tool; Filename: {app}\DelphiPackageTool.exe; WorkingDir: {app}; IconIndex: 0
Name: {group}\Documentation; Filename: {app}\TheDelphiPackageTool.pdf
Name: {group}\Video\First Steps; Filename: {app}\doc\video\create_new_project.swf
Name: {group}\Video\Search Path; Filename: {app}\doc\video\setup_search_path.swf
Name: {group}\Video\Open IDE; Filename: {app}\doc\video\open_in_IDE.swf

; NOTE: The following entry contains an English phrase ("Uninstall"). You are free to translate it into another language if required.
Name: {group}\Uninstall Delphi Package Tool; Filename: {uninstallexe}
Name: {userdesktop}\Delphi Package Tool; Filename: {app}\DelphiPackageTool.exe; Tasks: desktopicon; WorkingDir: {app}; IconIndex: 0


[Run]
; NOTE: The following entry contains an English phrase ("Launch"). You are free to translate it into another language if required.
Filename: {app}\DelphiPackageTool.exe; Description: Launch Delphi Package Tool; Flags: nowait postinstall skipifsilent

