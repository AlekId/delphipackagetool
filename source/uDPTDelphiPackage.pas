{-----------------------------------------------------------------------------
 Unit Name: uDPTDelphiPackage
 Author:    s.herzog
 Purpose:   cool functions to install/uninstall and recompile delphi projects.
 History:
-----------------------------------------------------------------------------}
//TODO: some re-factoring of this unit is needed.


unit uDPTDelphiPackage;

{$I jedi.inc}

interface
uses
  Classes,
  Windows,
  StdCtrls,
  IniFiles,
  Registry,
  uDPTDefinitions;

function  GetDelphiPackageDir(const _DelphiVersion:integer;const _PlatformToCompile: string):string; // get the delphi project\bpl path for Delphi Version <_DelphiVersion>.
function  SetDelphiPackageDir(const _DelphiVersion:integer;_PackageDir:string;const _silent:boolean;const _PlatformToCompile: string):boolean; // write the package dir (bpl-folder) <_PackageDir> for Delphi Version <_DelphiVersion>.
function  InstallPackage(_PackageName, _PackageDirectory, _PackageDescription, _PackageLibSuffix: string; _DelphiVersion: Integer; out msg: string): Boolean; // add package into the regitstry.
function  UninstallPackage(_PackageName, _PackageDirectory, _PackageLibSuffix: string; _DelphiVersion: Integer): Boolean;  // remove package from regeistry.
function  CompileProject(_Compiler, _CompilerSwitches, _ProjectName, _TargetPath, _DCUPath, _DCPPath, _WorkPath, _NameSpaces: string; _ProjectType: TProjectType; var Output: string; const _DelphiVersion: Integer): Boolean; // compile the package
function  VerifyRegistry(const _DelphiVersion:integer;out NoOfRemovedKeys:integer; const _CurrentPlatform,_CurrentConfig: string):boolean; // scan through the registry items of "Known Packages" and "Disabled Packages" and check if the referenced files really exists. If not then remove the registry key.
procedure ReadPackageListfromFile(_filename:string;var lst:TListBox);overload;  //read packages&projects from the goup-file <_filename> (.bpg or .bdsgroup or .groupproj) into the listbox <lst>.
procedure ReadPackageListfromFile(_filename:string;var lst:TStringList);overload;  //read packages&projects from the goup-file <_filename> (.bpg or .bdsgroup or .groupproj) into the stringlist <lst>.
function  ReadPackageInfo(const _PackageName:string;out Description:string;out LibSuffix:string):boolean; // get the information from the dpk file.
function  WinExecAndWait(FileName,CommandLine,WorkPath,Environment: string; Visibility: Integer;out Output:String): LongWord;
function  IsDelphiStarted(const _DelphiVersion:Integer): Boolean;
procedure ShutDownDelphi(const _DelphiVersion:Integer);
procedure StartUpDelphi(const _DelphiVersion:Integer;_ProjectName:string);
function  ReadProjectFilenameFromDProj(const _Filename:String):string; // the real project filename is now hidden in the dproj-file.
function  ReadSupportedConfigsOfProject(const _filename: string; var _Configs: TStringList): Boolean;
function  ReadSupportedPlatformsOfProject(const _filename: string; var _Platforms: TStringList): Boolean;
function  ReadAllPlatformsOfProject(const _filename: string; var _Platforms: TStringList): Boolean;
function  ReadConfigurationSettings(const _filename:string;out Config:string;out dPlatform:string;out CompilerSwitches: string;out Conditions:string;out SearchPath:string;out ProjectOutputPath:string;out BPLOutputPath:string;out DCUOutputPath:string;out DCPOutputPath:string;out NameSpaces:string):boolean;
function  WriteSettingsToDelphi(_bpgPath,_Filename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath,_DCPOutputPath:string;const _silent:boolean;const _ProjectType:TProjectType;const _DelphiVersion:integer;const _CurrentPlatform, _CurrentConfig: string;out ChangedFiles:TStringList):boolean;
function  GetDelphiRootDir(const _DelphiVersion:integer):string;  // returns delphi root directory e.g. C:\Program files\Borland\Delphi7
function  GetDelphiStdPackagesDir(const _DelphiVersion: integer; const _PlatformToCompile: string): string;  // returns directory with platform dependent standard packages included in Delphi e.g. C:\Program files\Embarcadero\RAD Studio\9.0\<bin|bin64>
function  GetInstalledIDEVersions(_list:TStrings):boolean; // returns delphi and bds versions.
procedure InitBatchFile(const _filename:string); // reset batch file
function  SaveBatchFile:string; // save the batch file.
function  GetPackageVersion(const _PackageName,_PackageOutputPath,_PackageLibSuffix:string;const _ProjectType:TProjectType):string;
function  DetermProjectType(_projectfilename:string;const _projectGroupfilename:string;const _DelphiVersion:integer):TProjectType; // find out if the source file contains a application or library or package.
function  OutputFilename(const _filename:string;const _ProjectType:TProjectType;const _libsuffix:string=''):string; // input is a source code filename, output is the name of the compiled target.
function  GetPackageSize(_PackageName,_PackageOutputPath,_PackageLibSuffix:string;const _ProjectType:TProjectType):Int64; // read the filesize of the package.
function  GetDelphiPathTag(const _version:integer):string; // returns $(DELPHI) or $(BDS) according to the version number
function  VersionNoToIDEName(const _version:integer;const _NameType:TDelphiNameType=tdn_long):string; // turns a ide version no 1-9 into 6.0,7.0,BDS 1.0,BDS 2.0
function  IDENameToVersionNo(_version:string):integer; // turns the ide name 6.0 into 6 or bds 4.0 into 10.
function  CleanUpPackagesByRegistry(const _ROOTKEY:DWORD;const _DelphiVersion:integer;const _DelphiSubKey:string;const _deletefiles:boolean; const _CurrentPlatform,_CurrentConfig: string):boolean; // this method delete's the key HKEY_LOCAL_MACHINE/Software/Borland/Delphi/%VERSIONNO%/Known Packages and the same for HKEY_CURRENT_USER
function  CleanUpPackagesByPath(const _DelphiVersion:integer;_BPLPath:string;_DCPPath:string;const _deletefiles:boolean; const _CurrentPlatform,_CurrentConfig: string):boolean; // this method delete's the packages located in ($DELPHI)\Projects\Bpl and removes the key's from the registery.
function  CleanupByRegistry(const _ROOTKEY:DWORD;const _DelphiSubKey:string;const _DelphiVersion:integer;var NoOfRemovedKeys:integer; const _CurrentPlatform,_CurrentConfig: string):boolean; // find registry-entries without the packages
function  CleanUpPackageByEnvPaths(const _DelphiVersion:integer;const _silent:boolean):boolean;
function  ReadLibraryPath(const _DelphiVersion:integer;out DelphiLibraryPath:TDelphiLibraryPath):boolean; //read the library setting from the registry.
function  ExtractFilenamesFromDCC32Output(const _BasePath:string;const _CompilerOutput:TStrings;_SourceCodeOnly:boolean):THashedStringList; // extract filenames from the dcc32.exe output.
function  WritePackageFile(const _DelphiVersion:integer;const _filename:string;const _LibSuffix:string;const _silent:boolean;out NewFilename:string):boolean;
function  WriteDPKFile(const _DelphiVersion:integer;_filename:string;const _LibSuffix:string;const _silent:boolean;out NewFilename:string):boolean;  // write libsuffix into the dpk-file.
function  WriteDprojFile(_filename:string;const _LibSuffix:string;const _silent:boolean;out NewFilename:string):boolean; // write libsuffix into the dproj-file.
function  DeleteFile(const _Filename:String;const _toTrashBin:boolean=true):boolean;  // delete the file <_filename>.
function  RemoveProjectFromProjectGroup(const _GroupFilename,_ProjectFilename:string;const _ProjectType:TProjectType):boolean;
function  ReadBDSCommonDir(const _DelphiVersion:integer):string; // reads the path to the BDSCOMMONDIR.
function  ReadBDSProjectsDir(const _DelphiVersion:integer):string; // reads the path to the BDSPROJECTSDIR.
function  ReadBDSUserDir(const _DelphiVersion:integer):string; // reads the path to the BDSUSERDIR.
function  isIDEInstalled(const _Version:Integer):boolean; // find out if the ide version e.g. 11.0 is installed.
function  OldestIDEVersion:integer; // returns the VersionNo of the oldest IDE installed.
function  LatestIDEVersion:integer; // returns the VersionNo of the newest IDE installed.
function  GetIDERootKey(const _version:integer;out RootKey:string):boolean;
function  RemoveDoublePathEntries(_Path:string):string; // verify the string <_Path> and remove double entries.
function  RemoveTrailingSemikolon(const _path:string):string;
function  AddTag(_filename: string;_DelphiVersion:integer): string;
function  GetIDEEnvironmentPath(const _DelphiVersion:integer): string; // read the environments-path for delphi version <_DelphiVersion>.
function  SetIDEEnvironmentPath(const _DelphiVersion:integer;_IDEEnvironmentPath:string;const _silent:boolean):boolean; //set IDE Environment Path <_IDEEnvironmentPath> for version <_DelphiVersion>.
function  IsPathInIDEEnvironmentPath(const _DelphiVersion:integer;const _path: string): boolean; // checks if the path <_path> is already in the IDE environment variable.
function  AddIDEEnvironmentPath(const _DelphiVersion:integer;const _path: string): boolean; // add the path <_path> to the IDE Environment Variable.
function  VerifyIDEEnvrionmentsPath(const _DelphiVersion:integer;const _silent:boolean;out DeletedPathEntries:integer):boolean; // remove in-existent folders from the IDE Environments Path. Returns the number of deleted entries in <DeletedPathEntries>.
function  GetIDEEnvironmentPathList(const _DelphiVersion:integer): TStrings;


var
  gCreateBatchFile:boolean;

implementation

uses
  uDPTMisc,
  SysUtils,
  StrUtils,
  TlHelp32,
  Forms,
  XMLDoc,
  XMLIntf,
  Controls,
  Dialogs,
  ShellApi,
  uDPTPathFilenameConvert,
  uDPTXMLReader,
  uDPTJclFuncs,
  uDTPProjectData,
  SelectFilesFrm,
  MSXML_TLB;

var
  FBatchFile:TStrings;
  FBatchFilename:string;


{-----------------------------------------------------------------------------
  Procedure: RemoveDoublePathEntries
  Author:    sam
  Date:      30-Mrz-2010
  Arguments: _Path:string
  Result:    string
  Description: verify the string <_Path> and remove double entries.
-----------------------------------------------------------------------------}
function  RemoveDoublePathEntries(_Path:string):string;
var
i:integer;
_item:string;
_list:TStrings;
begin
  result:='';
  _list:=TStringList.create;
  try
    while _path<>'' do begin
      _item:=ExcludeTrailingPathDelimiter(lowercase(GetField(';',_path)));
      if _list.IndexOf(_item)=-1 then _list.add(_item)
      else trace(5,'The path <%s> is already in the list. Removed!',[_item]);
    end;
    for i:=0 to _list.count-1 do result:=result+_list[i]+';'
  finally
    _list.free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: OldestIDEVersion
  Author:    sam
  Date:      11-Mrz-2009
  Arguments: None
  Result:    integer
  Description: returns the VersionNo of the oldest IDE installed.
-----------------------------------------------------------------------------}
function  OldestIDEVersion:integer;
var
i:integer;
_installedVersion:integer;
_InstalledIDEVersions:TStrings;
begin
  result:=High(integer);
  _InstalledIDEVersions:=TStringList.create;
  try
    GetInstalledIDEVersions(_InstalledIDEVersions);
    for i:=0 to _InstalledIDEVersions.count-1 do begin
      _installedVersion:=IDENameToVersionNo(_InstalledIDEVersions[i]);
      if _installedVersion<result then result:=_installedVersion;
    end;
  finally
    _InstalledIDEVersions.free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: LatestIDEVersion
  Author:    sam
  Date:      12-Sep-2009
  Arguments: None
  Result:    integer
  Description: returns the newst ide version installed on this computer.
-----------------------------------------------------------------------------}
function  LatestIDEVersion:integer;
var
i:integer;
_installedVersion:integer;
_InstalledIDEVersions:TStrings;
begin
  result:=0;
  _InstalledIDEVersions:=TStringList.create;
  try
    GetInstalledIDEVersions(_InstalledIDEVersions);
    for i:=0 to _InstalledIDEVersions.count-1 do begin
      _installedVersion:=IDENameToVersionNo(_InstalledIDEVersions[i]);
      if _installedVersion>result then result:=_installedVersion;
    end;
  finally
    _InstalledIDEVersions.free;
  end;
end;


{*-----------------------------------------------------------------------------
  Procedure: isIDEInstalled
  Author:    sam
  Date:      11-Mrz-2009
  Arguments: const _Version:Integer
  Result:    boolean
  Description: returns true if the version <_Version> is installed. e.g. 12
-----------------------------------------------------------------------------}
function isIDEInstalled(const _Version:Integer):boolean;
var
i:integer;
_installedVersion:integer;
_InstalledIDEVersions:TStrings;
begin
  result:=false;
  _InstalledIDEVersions:=TStringList.create;
  try
    GetInstalledIDEVersions(_InstalledIDEVersions);
    for i:=0 to _InstalledIDEVersions.count-1 do begin
      _installedVersion:=IDENameToVersionNo(_InstalledIDEVersions[i]);
      if _installedVersion=_Version then begin
        result:=true;
        exit;
      end;
    end;
  finally
    _InstalledIDEVersions.free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadBDSCommomDir
  Author:    muem
  Date:      26-Feb-2010
  Arguments: _DelphiVersion:integer
  Result:    string
  Description:  reads the path to the BDSCOMMONDIR.
-----------------------------------------------------------------------------}
function ReadBDSCommonDir(const _DelphiVersion:integer):string;
begin
  Result := '';
  if _DelphiVersion > 11 then begin
    if _DelphiVersion < 15  then
      Result := GetSystemPath(spCommonDocs) + cRADStudioDirName + PathDelim + DelphiVersions[_DelphiVersion].IDEVersionStr //RAD Studio 2009, 2010
    else
      Result := GetSystemPath(spCommonDocs) + cEmbaStudioDirName + PathDelim + DelphiVersions[_DelphiVersion].IDEVersionStr
  end
  else Result := GetEnvironmentVariable(Copy(cBDSCommonDirTag, 3, length(cBDSCommonDirTag)-3));
  trace(5,'ReadBDSCommonDir: BDSCOMMONDIR is set to <%s>.',[result]);
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadBDSProjectsDir
  Author:    sam
  Date:      02-Mar-2010
  Arguments: _DelphiVersion:integer
  Result:    string
  Description:  reads the path to the BDSPROJECTSDIR.
-----------------------------------------------------------------------------}
function ReadBDSProjectsDir(const _DelphiVersion:integer):string;
var
  LocStr: WideStringArray;
begin
  Result:='';
  if _DelphiVersion <= 7 then exit;  // delphi 1-7 do nothing, because bdsprojectdir was introduced later.

  //Delphi 8 .. RAD Studio 2010
  LocStr := LoadResStrings(GetDelphiRootDir(_DelphiVersion) + 'Bin\coreide' + DelphiVersions[_DelphiVersion].PackageVersion + '.',['Borland Studio Projects', 'RAD Studio', 'Projects', 'Embarcadero', 'Studio']);

  if DelphiVersions[_DelphiVersion].IDEVersion < 5 then begin
    Result := LocStr[0];
  end
  else if DelphiVersions[_DelphiVersion].IDEVersion < 15 then begin
    Result := LocStr[1] + PathDelim + LocStr[2];
  end else
    Result := LocStr[3] + PathDelim + LocStr[4] + PathDelim + LocStr[2];

  Result := IncludeTrailingPathDelimiter(GetSystemPath(spPersonal)) + Result;
  trace(5,'ReadBDSProjectsDir: BDSPROJECTSDIR is set to <%s>.',[result]);
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadBDSUserDir
  Author:    sam
  Date:      15-Nov-2008
  Arguments: _DelphiVersion:integer
  Result:    string
  Description:  reads the path to the BDSUSERDIR.
-----------------------------------------------------------------------------}
function ReadBDSUserDir(const _DelphiVersion:integer):string;
begin
  result:='';
{ TODO : Impementation of BDSUSERDIR }
  Showmessage('function ReadBDSUserDir: TODO: We are looking for a nice guy who would like to implement this method ;-)');
  trace(5,'ReadBDSUserDir: BDSUSERDIR is set to <%s>.',[result]);
end;

{-----------------------------------------------------------------------------
  Procedure: ReadNodeIndex
  Author:    sam
  Date:      25-Mrz-2010
  Arguments: const _ParentNode:IXMLNode;const _NodeName,_AttributeName:string
  Result:    integer
  Description:
-----------------------------------------------------------------------------}
function ReadNodeIndex(const _ParentNode:IXMLNode;const _NodeName,_AttributeName:string):integer;
var
i:integer;
_Node: IXMLNode;
begin
  result:=-1;
  i:=0;
  _Node := _ParentNode.ChildNodes.First;
  while Assigned(_Node) do begin
    if _Node.nodeName = _NodeName then begin
      if lowercase(_Node.Attributes['Name'])=lowercase(_AttributeName) then begin
        result:=i;
        break
      end;
    end;
    _Node := _Node.nextSibling;
    inc(i);
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: RemoveProjectFromBDSGroup
  Author:    sam
  Date:      09-Jun-2008
  Arguments: const _bdsgroupfilename,_projectfilename:string;const _filetype:TProjectType
  Result:    boolean
  Description: remove the project <_projectfilename> with filetype <_filetype> from the project-group file <_bdsgroupfilename>.
-----------------------------------------------------------------------------}
function  RemoveProjectFromBDSGroup(const _bdsgroupfilename,_projectfilename:string;const _filetype:TProjectType):boolean;
var
i:integer;
_XMLFile:xmlIntf.IXMLDocument;
_BorlandProject:xmlIntf.IXMLNode;
_DelphiPersonality:xmlIntf.IXMLNode;
_DefaultPersonality:xmlIntf.IXMLNode;
_Projects:xmlIntf.IXMLNode;
_outputfilename:string;
_index:integer;
_targets:string;
begin
  Result:=false;
  _index:=-1;
  if not fileExists(_bdsgroupfilename) then begin
    trace(5,'RemoveProjectFromBDSGroup: Could not find the file <%s>.',[_bdsgroupfilename]);
    exit;
  end;
  _OutputFilename:=OutputFilename(_projectfilename,_filetype);
  _XMLFile:=xmldoc.newXMLDocument;
  try
    _XMLFile.LoadFromFile(_bdsgroupfilename);
    _XMLFile.active:=true;
    _BorlandProject := _XMLFile.DocumentElement;
    _DefaultPersonality := _BorlandProject.childNodes['Default.Personality'];
    if assigned(_DefaultPersonality) then begin
      _Projects:=_DefaultPersonality.childNodes['Projects'];
      _index:=ReadNodeIndex(_Projects,'Projects',_OutputFilename);
    end;
    if _index=-1 then begin
      _DelphiPersonality  := _BorlandProject.childNodes['Delphi.Personality'];
      if assigned(_DelphiPersonality) then begin
        _Projects:=_DelphiPersonality.childNodes['Projects'];
        _index:=ReadNodeIndex(_Projects,'Projects',_OutputFilename);
      end;
    end;
    if _index=-1 then exit;
    try
      _Projects.ChildNodes.Delete(_index);
      _targets:='';
      for i:=0 to _Projects.ChildNodes.Count-2 do begin
        _targets:=_targets+' '+_Projects.ChildNodes[i].Attributes['Name'];
      end;
      _Projects.ChildNodes.Last.Text:=_targets;
      _XMLFile.SaveToFile(_bdsgroupfilename);
      trace(3,'Removed project <%s> from file <%s>.',[_projectfilename,_bdsgroupfilename]);
      result:=true;
    except
      on e:exception do trace(1,'Error in RemoveProjectFromBDSGroup: <%s>.',[e.message]);
    end;
  finally
    _XMLFile.active:=false;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: RemoveProjectFromBPG
  Author:    sam
  Date:      05-Nov-2005
  Arguments: None
  Result:    boolean
  Description: remove the project <_projectfilename> with filetype <_filetype> from the project-group file <_bpgfilename>.
-----------------------------------------------------------------------------}
function RemoveProjectFromBPG(const _bpgfilename,_projectfilename:string;const _filetype:TProjectType):boolean;
var
i:integer;
_bpgFile:TStrings;
_OutputFilename:string;
_pos:integer;
_line:string;
begin
  result:=false;
  _OutputFilename:=OutputFilename(_projectfilename,_filetype);
  _bpgFile:=TStringList.Create;
  try
    _bpgFile.LoadFromFile(_bpgfilename);
    for i:=0 to _bpgFile.Count-1 do begin
      _line:=_bpgFile[i];
      _pos:=Pos(_OutputFilename,_line);
      if _pos>0 then begin
        if Pos(_OutputFilename+':',_line)=1 then begin
          _bpgFile.Delete(i);
          _bpgFile.Delete(i);
          _bpgFile.Delete(i);
          break;
        end
        else begin
          Delete(_line,_pos,length(_OutputFilename)+1);
          _bpgFile[i]:=_line;
        end;
      end;
    end;
    try
      _bpgFile.SaveToFile(_bpgfilename);
      result:=true;
    except
      on e:exception do begin
        trace(1,'Error in RemoveProjectFromBPG: Could not write file <%s>.<%s>.',[_bpgfilename,E.Message]);
      end;
    end;
  finally
    _bpgFile.free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: RemoveProjectFromGroupProj
  Author:    sam
  Date:      02-Jan-2010
  Arguments: const _bpgfilename,_projectfilename:string;const _filetype:TProjectType
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function RemoveProjectFromGroupProj(const _bpgfilename,_projectfilename:string;const _filetype:TProjectType):boolean;
begin
  result:=false;
  showmessage('TODO: Deletion of a project from a .groupproj file is not implemented yet. You can get the sourcecode from sourceforge.net and implement this feaure.');
end;

{*-----------------------------------------------------------------------------
  Procedure: RemoveProjectFromProjectGroup
  Author:    sam
  Date:      09-Jun-2008
  Arguments: None
  Result:    boolean
  Description: remove the project <_projectname> from the project group file <_GroupFilename>.
-----------------------------------------------------------------------------}
function RemoveProjectFromProjectGroup(const _GroupFilename,_ProjectFilename:string;const _ProjectType:TProjectType):boolean;
var
_FileExt:string;
begin
  result:=false;
  _FileExt:=lowercase(extractfileext(_GroupFilename));
  if _FileExt='.bpg'       then result:=RemoveProjectFromBPG(_GroupFilename,_ProjectFilename,_ProjectType) else
  if _FileExt='.bdsgroup'  then result:=RemoveProjectFromBDSGroup(_GroupFilename,_ProjectFilename,_ProjectType) else
  if _FileExt='.groupproj' then result:=RemoveProjectFromGroupProj(_GroupFilename,_ProjectFilename,_ProjectType);
end;

{*-----------------------------------------------------------------------------
  Procedure: VerifyRegistry
  Author:    sam
  Date:      05-Jun-2008
  Arguments: const _DelphiVersion:integer
  Result:    boolean
  Description: this method returns true if an invalid key from the registry gets deleted.
-----------------------------------------------------------------------------}
function  VerifyRegistry(const _DelphiVersion:integer;out NoOfRemovedKeys:integer; const _CurrentPlatform,_CurrentConfig: string):boolean; // scan through the registry items of "Known Packages" and "Disabled Packages" and check if the referenced files really exists. If not then remove the registry key.
begin
  result:=true;
  NoOfRemovedKeys:=0;
  if not CleanupByRegistry(HKEY_CURRENT_USER, 'Known Packages'   ,_DelphiVersion,NoOfRemovedKeys,_CurrentPlatform,_CurrentConfig) then result:=false;
  if not CleanupByRegistry(HKEY_LOCAL_MACHINE,'Known Packages'   ,_DelphiVersion,NoOfRemovedKeys,_CurrentPlatform,_CurrentConfig) then result:=false;
  if not CleanupByRegistry(HKEY_CURRENT_USER, 'Disabled Packages',_DelphiVersion,NoOfRemovedKeys,_CurrentPlatform,_CurrentConfig) then result:=false;
  if not CleanupByRegistry(HKEY_LOCAL_MACHINE,'Disabled Packages',_DelphiVersion,NoOfRemovedKeys,_CurrentPlatform,_CurrentConfig) then result:=false;
end;

{-----------------------------------------------------------------------------
  Procedure: FindLine
  Author:    sam
  Date:      25-Mrz-2010
  Arguments: var content:TStrings;_Tag:string;var removedText:string
  Result:    integer
  Description:
-----------------------------------------------------------------------------}
function FindLine(var content:TStrings;_Tag:string;var removedText:string):integer;
var
i:integer;
_Text:String;
begin
  result:=-1;
  removedText:='';
  for i:=0 to content.Count-1 do begin
    _text:=trim(content[i]);
    if pos(_Tag,_text)<>1 then continue;
    removedText:=_text;
    result:=i;
    break;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: WritePackageFile
  Author:    sam
  Date:      22-Sep-2009
  Arguments: const _filename:string;const _LibSuffix:string;const _silent:boolean
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function  WritePackageFile(const _DelphiVersion:integer;const _filename:string;const _LibSuffix:string;const _silent:boolean;out NewFilename:string):boolean;
var
_DProjFilename:string;
begin
  result:=false;
  if lowercase(ExtractFileExt(_filename))='.dpk'   then result:=WriteDPKFile(_DelphiVersion,_filename,_libsuffix,_silent,NewFilename);
  if _DelphiVersion>=11 then begin
    _DProjFilename:=ChangeFileExt(_filename,'.dproj');
    if fileexists(_DProjFilename) then result:=WriteDprojFile(_DProjFilename,_libsuffix,_silent,NewFilename);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: WriteDPKFile
  Author:    sam
  Date:      08-Nov-2007
  Arguments: const _filename:string;const _LibSuffix:string
  Result:    boolean
  Description: write libsuffix into dpk-file.
-----------------------------------------------------------------------------}
function  WriteDPKFile(const _DelphiVersion:integer;_filename:string;const _LibSuffix:string;const _silent:boolean;out NewFilename:string):boolean;
resourcestring
cAskToReplaceLibSuffix='Do you want to replace <%s> with <%s>?';
var
_File:TStrings;
_index:integer;
_OldText:string;
_NewText:string;
_FileChanged:boolean;

  procedure UpdateLibSuffix;
  begin
    _index:=FindLine(_File,'{$LIBSUFFIX',_OldText);
    if _LibSuffix='' then begin
      if _index>-1 then begin // no libsuffix to be set but there is one already in the file.
        _File.Delete(_index); // delete it.
        _FileChanged:=true;
      end;
    end
    else begin
      if _index=-1 then begin
        _index:=_File.IndexOf('requires');
        if _index>-1 then _index:=_index-2
                     else _index:=2;
      end;
      _NewText:=format('{$LIBSUFFIX ''%s''}',[_LibSuffix]);
      if _NewText<>_OldText then begin
        if not _silent then begin
          if Application.MessageBox(pchar(format(cAskToReplaceLibSuffix,[_OldText,_NewText])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDNo then exit;
        end;
        _File.Delete(_index);
        _File.Insert(_index,_NewText);
        _FileChanged:=true;
      end;
    end;
  end;

  procedure UpdateImageLib;
  begin
    _index:=FindLine(_File,'vcljpg',_OldText);
    if (_index>-1) and
       (_DelphiVersion>7) then begin    // replace "vcljpg" with "vclimg".
      _NewText:=_File[_index];
      _newText:=stringreplace(_NewText,'vcljpg','vclimg',[rfIgnoreCase]);
      _File[_index]:=_newText;
      _FileChanged:=true;
      exit;
    end;

    _index:=FindLine(_File,'vclimg',_OldText);
    if (_index>-1) and
       (_DelphiVersion<=7) then begin    // replace "vclimg" with "vcljpg".
      _NewText:=_File[_index];
      _newText:=stringreplace(_NewText,'vclimg','vcljpg',[rfIgnoreCase]);
      _File[_index]:=_newText;
      _FileChanged:=true;
      exit;
    end;
  end;

  procedure UpdateOtares;
  begin
    _index:=FindLine(_File,'{$R *.otares}',_OldText);
    if (_index>-1) and
       (_DelphiVersion<=15) then begin    // in versions before XE2 this statement shall no be in the .dpk-file.
      _NewText:=_File[_index];
      _newText:=stringreplace(_NewText,'{$R *.otares}','',[rfIgnoreCase]);
      _File[_index]:=_newText;
      _FileChanged:=true;
      exit;
    end;
  end;
begin
  result:=false;
  NewFilename:='';
  _FileChanged:=false;
  if not fileexists(_filename) then begin
    trace(1,'Problem in WriteDPKFile: Could not find the file <%s>. Nothing to do.',[_filename]);
    exit;
  end;
  _File:=TStringList.create;
  try
    _File.LoadFromFile(_filename);
    UpdateLibSuffix;
    UpdateImageLib;
    UpdateOtares;
    if not _FileChanged then exit;
    if not BackupFile(_filename,'.dpk_old','',false) then exit;
    _filename:=changefileext(_filename,'.dpk_new');
    try
      _File.SaveToFile(_filename);
      trace(5,'WriteDPKFile: Saved changes to file <%s>.',[_filename]);
      NewFilename:=_filename;
      result:=true;
    except
      on e:exception do trace(1,'Error in WriteDPKFile: Could not save file <%s>. Check User-Rights. <%s>.',[_filename,e.message]);
    end;
  finally
    _File.free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: WriteDProjFile
  Author:    sam
  Date:      22-Sep-2009
  Arguments: const _filename:string;const _LibSuffix:string;const _silent:boolean
  Result:    boolean
  Description: write libsuffix into .dproj file.
-----------------------------------------------------------------------------}
function  WriteDProjFile(_filename:string;const _LibSuffix:string;const _silent:boolean;out NewFilename:string):boolean;
resourcestring
cAskToReplaceLibSuffix='Do you want to replace <%s> with <%s>?';
var
_File:TStrings;
_FileChanged:boolean;
_LibSuffixAlreadyInFile:boolean;

  procedure UpdateDllSuffix;
  var
   i:integer;
   _OldText:string;
   _NewText:string;
   _SuffixInFile:string;
   _pos:integer;
  begin
    for i := 0 to _File.Count-1 do begin
      _OldText:=_File[i];
      _pos:=pos('<DllSuffix>',_OldText);
      if _pos>0 then begin
        _NewText:=copy(_OldText,1,_pos-1);
        _NewText:=_NewText+format('<DllSuffix>%s</DllSuffix>',[_LibSuffix]);
        if trim(_NewText)<>trim(_OldText) then begin
          if not _silent then begin
            if Application.MessageBox(pchar(format(cAskToReplaceLibSuffix,[_OldText,_NewText])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDNo then exit;
          end;
          _File.Delete(i);
          _File.Insert(i,_NewText);
          _FileChanged:=true;
          trace(3,'Succsessfully written <%s> into file <%s>.',[_NewText,_filename]);
        end;
      end;
    end;
 end;
//    _index:=FindLine(_File,'<Package_Options Name="LibSuffix">',_OldText);
//    if _index=-1 then begin
//      _index:=FindLine(_File,'</Package_Options>',_OldText);
//      if _index=-1 then begin
//        trace(1,'Error in WriteDProjFile: Could not find section <Package_Options> in file <%s>.',[_filename]);
//        exit;
//      end;
//    end else _LibSuffixAlreadyInFile:=true;
//    _NewText:=format('       <Package_Options Name="LibSuffix">%s</Package_Options>',[_LibSuffix]);
//    if trim(_NewText)<>trim(_OldText) then begin
//      if not _silent then begin
//        if Application.MessageBox(pchar(format(cAskToReplaceLibSuffix,[_OldText,_NewText])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDNo then exit;
//      end;
//      if _LibSuffixAlreadyInFile then _File.Delete(_index);
//      _File.Insert(_index,_NewText);
//      _FileChanged:=true;
//      trace(3,'Succsessfully written <%s> into file <%s>.',[_NewText,_filename]);
//    end;
//  end;

begin
  result:=false;
  NewFilename:='';
  _FileChanged:=false;
  _LibSuffixAlreadyInFile:=false;
  if not fileexists(_filename) then begin
    trace(1,'Problem in WriteDprojFile: Could not find the file <%s>. Nothing to do.',[_filename]);
    exit;
  end;
  _File:=TStringList.create;
  try
    _File.LoadFromFile(_filename);
    UpdateDllSuffix;
    if not _FileChanged then exit;
    if not BackupFile(_filename,'.dproj_old','',false) then exit;
    _filename:=changefileext(_filename,'.dproj_new');
    try
      _File.SaveToFile(_filename);
      trace(5,'WriteDProjFile: Saved changes to file <%s>.',[_filename]);
      NewFilename:=_filename;
      result:=true;
    except
      on e:exception do trace(1,'Error in WriteDProjFile: Could not save file <%s>. Check User-Rights. <%s>.',[_filename,e.message]);
    end;
  finally
    _File.free;
  end;
end;


{-----------------------------------------------------------------------------
  Procedure: ExtractFilenamesFromDCC32Output
  Author:    sam
  Date:      16-Okt-2007
  Arguments: const _BasePath:string;const _CompilerOutput:TStringList
  Result:    TStringList
  Description: extract filenames from the dcc32.exe output.
-----------------------------------------------------------------------------}
function  ExtractFilenamesFromDCC32Output(const _BasePath:string;const _CompilerOutput:TStrings;_SourceCodeOnly:boolean):THashedStringList;
var
i,k,l:integer;
_ExtensionsOfInterest:TStringList;
_TempFilesList:THashedStringList;
_FoldersToScan:THashedStringList;
_line:string;
_ext:string;
_filename:string;
_filepath:string;
_pos:integer;
_workPath:string;
begin
  result:=THashedStringList.create;
  result.Sorted:=true;
  _TempFilesList:=THashedStringList.create;
  _TempFilesList.Sorted:=true;
  _ExtensionsOfInterest:=TStringList.create;
  _ExtensionsOfInterest.Sorted:=true;
  _FoldersToScan:=THashedStringList.create;
  _FoldersToScan.Sorted:=true;

  Screen.cursor:=crHourGlass;
  try
    _ExtensionsOfInterest.add('.pas');
    _ExtensionsOfInterest.add('.dfm');
    _ExtensionsOfInterest.add('.dpk');
    _ExtensionsOfInterest.add('.inc');
    _ExtensionsOfInterest.add('.dcr');
    _ExtensionsOfInterest.add('.drc');
    _ExtensionsOfInterest.add('.res');
    _ExtensionsOfInterest.add('.dpr');
    _ExtensionsOfInterest.add('.bpg');
    _ExtensionsOfInterest.add('.txt');
    _ExtensionsOfInterest.add('.ini');
    _ExtensionsOfInterest.add('.cfg');
    _ExtensionsOfInterest.add('.dof');
    _ExtensionsOfInterest.add('.xml');
    if not _SourceCodeOnly then begin
      _ExtensionsOfInterest.add('.dcu');
      _ExtensionsOfInterest.add('.obj');
      _ExtensionsOfInterest.add('.bpl');
      _ExtensionsOfInterest.add('.dcp');
      _ExtensionsOfInterest.add('.bcc_obj');
      _ExtensionsOfInterest.add('.obj');
      _ExtensionsOfInterest.add('.zobj');
      _ExtensionsOfInterest.add('.tlb');
      _ExtensionsOfInterest.add('.ocx');
      _ExtensionsOfInterest.add('.dll');
    end;
    _ExtensionsOfInterest.add('.rc');
    _ExtensionsOfInterest.add('.ico');
    _ExtensionsOfInterest.add('.dproj');
    _ExtensionsOfInterest.add('.bdsproj');
    _ExtensionsOfInterest.add('.bdsgroup');
    _ExtensionsOfInterest.add('.groupproj');
    for i:=0 to _CompilerOutput.Count-1 do begin   // iterate through all lines of the compiler output
      _line:=lowercase(_CompilerOutput[i]);
      _ext:=ExtractFileExt(_line);
      if length(_ext)<4 then continue;
      _ext:=GetField('(',_ext);
      if _ext='' then continue;   // if the line does not contain a fileextension, then continue
      if _ExtensionsOfInterest.IndexOf(_ext)=-1 then continue; // if the fileextension is not in the list of interesting fileextensions then continue.
      _pos:=pos('(',_line);
      if _pos>0 then _filename:=copy(_line,1,_pos-1)
                else _filename:=_line;
      _filename:=AbsoluteFilename(_BasePath,_filename);  // create an absolute filename
      if not fileexists(_filename) then continue;        // check if the file exists
      if result.indexof(_filename)=-1 then begin         // only add the file to the list, if it is not already in the list.
        result.add(_filename);
        trace(5,'Added file <%s> to backup list.',[_filename]);
        _filepath:=lowercase(extractfilepath(_filename));
        if _FoldersToScan.IndexOf(_filepath)=-1 then _FoldersToScan.Add(_filepath);
      end;
    end;

    for i:=0 to _FoldersToScan.count-1 do begin
      _TempFilesList.Clear;
      _workPath:=_FoldersToScan[i];
      for k:=0 to _ExtensionsOfInterest.count-1 do begin  // now check all other fileextensions
        trace(5,'Searching for files in path <%s> for files <%s>.',[_workPath,'*'+_ExtensionsOfInterest[k]]);
        AllFilesOfPath(_workPath,'*'+_ExtensionsOfInterest[k],_TempFilesList);
        Application.ProcessMessages;
        for l:=0 to _TempFilesList.count-1 do begin
          _filename:=_workPath+_TempFilesList[l];
          if result.indexof(_filename)<>-1 then continue;
          result.add(_filename);
          trace(5,'Added file <%s> to backup list.',[_filename]);
        end;
      end;
    end;
  finally
    _ExtensionsOfInterest.free;
    _TempFilesList.free;
    _FoldersToScan.free;
    Screen.cursor:=crDefault;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: WriteDOFFile
  Author:    sam
  Date:      15-Jun-2007
  Arguments: _bpgPath,_dofFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath:string;const _silent:boolean
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function  WriteDOFFile(_bpgPath,_dofFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath:string;const _silent:boolean;const _DelphiVersion:integer;out NewFilename:string):boolean; // write info to the dof-file.
var
_DOFFile:TStrings;
_index:integer;
_FileChanged:boolean;
_searchText:string;

  function ReplaceItem(const _searchStr:string;const _NewText:string;var content:TStrings):boolean;
  var
    _index:integer;
    _indexDirectories:integer;
    _OldText:string;
  begin
    result:=false;
    _indexDirectories:=_DOFFile.IndexOf('[Directories]');
    _index:=FindLine(content,_searchStr,_OldText);
    if _SearchPath='' then exit;
    if _OldText=_NewText then exit;
    if _index>-1 then begin
      content.Delete(_index);
      content.Insert(_index,_NewText);
      result:=true;
    end
    else begin
      content.Insert(_indexDirectories+1,_NewText);
      result:=true;
    end;
  end;

begin
  result:=false;
  NewFilename:='';
  _FileChanged:=false;
  if not fileExists(_dofFilename) then begin
    trace(2,'Problem in WriteDOFFile: Could not find the file <%s>.',[_dofFilename]);
    exit;
  end;

// make all paths relative
  _searchPath       :=RelativePaths(ExtractFilePath(_dofFilename),_searchPath,_DelphiVersion,'$PLATFOPRM is not support','$CONFIG is not support');
  _ProjectOutputPath:=RelativePath(ExtractFilePath(_dofFilename),_ProjectOutputPath,_DelphiVersion);
  _BPLOutputPath    :=RelativePath(ExtractFilePath(_dofFilename),_BPLOutputPath,_DelphiVersion);
  _DCUOutputPath    :=RelativePath(ExtractFilePath(_dofFilename),_DCUOutputPath,_DelphiVersion);

  _DOFFile:=TStringList.Create;
  try
    _DOFFile.LoadFromFile(_dofFilename);

    if _SearchPath<>'' then begin
      if ReplaceItem('SearchPath=','SearchPath='+_SearchPath,_DOFFile) then _FileChanged:=true;
    end;
    if _ProjectOutputPath<>'' then begin
      if ReplaceItem('OutputDir=','OutputDir='+_ProjectOutputPath,_DOFFile) then _FileChanged:=true;
    end;
    if _DCUOutputPath<>'' then begin
      if ReplaceItem('UnitOutputDir=','UnitOutputDir='+_DCUOutputPath,_DOFFile) then _FileChanged:=true;
    end;
// remove section [Excluded Packages] It gives only troubles.
    _index:=FindLine(_DOFFile,'[Excluded Packages]',_searchText);
    if _index>-1 then begin
      _DOFFile.delete(_index);
      while (_index<_DofFile.Count) and
            (pos('[',trim(_DofFile[_index]))=0) do _DOFFile.delete(_index);
       _FileChanged:=true;
    end;

    if not _FileChanged then exit; // if nothing changed then exit;
    if not BackupFile(_dofFilename,'.dof_old','',false) then exit;  // if no backup could be made.
    _dofFilename:=changefileext(_dofFilename,'.dof_new');
    try
      _DOFFile.SaveToFile(_dofFilename);
      trace(5,'Saved changes to file <%s>.',[_dofFilename]);
      NewFilename:=_dofFilename;
      result:=true;
    except
     on e:exception do trace(1,'Error in WriteDOFFile: Could not save file <%s>. Check User-Rights. <%s>.',[_dofFilename,e.message]);
    end;
  finally
    _DOFFile.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: GetIDERootKey
  Author:    sam
  Date:      25-Feb-2006
  Arguments: const _version:integer
  Result:    string
  Description: returns the registry root key according to the passed version number.
-----------------------------------------------------------------------------}
function GetIDERootKey(const _version:integer;out RootKey:string):boolean;
var
_sDelphiVersion:string;
begin
  RootKey:='';
//  trace(5,'Enter GetIDERootKey with value <%d>.',[_version]);
  if _version<=7 then begin // for delphi version 1-7
    _sDelphiVersion:=inttostr(_version)+'.0\';
    RootKey:=cDelphiKey+'\'+_sDelphiVersion;
    result:=true;
  end else
  if (_version>7) and
     (_version<12) then begin // for delphi version 8-.. aka delphi 2005,delphi 2006 aka bds3.0 bds4.0
    _sDelphiVersion:=inttostr(_version-6)+'.0\';
    RootKey:=cBorlandBDSKey+'\'+_sDelphiVersion;
    result:=true;
  end else
  if (_version>11) and
     (_version<15) then begin // for delphi 2008 (aka bds 6.0) til D2010
    if _version<13 then _sDelphiVersion:=inttostr(_version-6)+'.0\'
                   else _sDelphiVersion:=inttostr(_version-7)+'.0\';
    RootKey:=cCodeGearBDSKey+'\'+_sDelphiVersion;
    result:=true;
  end
  else begin  // for Delphi 2011 (aka XE) and later.
 //   if _version>19 then _sDelphiVersion:=inttostr(_version-6)+'.0\'  // they skiped 13
    _sDelphiVersion:=inttostr(_version-7)+'.0\';
    RootKey:=cEmbarcaderoBDSKey+_sDelphiVersion;
    result:=true;
  end;
//  trace(5,'Leave GetIDERootKey with value <%s>.',[RootKey]);
end;

{-----------------------------------------------------------------------------
  Procedure: ReadLibraryPath
  Author:    sam
  Date:      09-Mrz-2007
  Arguments: _DelphiVersion:integer;var DelphiLibraryPath:TDelphiLibraryPath
  Result:    boolean
  Description: read the library setting from the registry.
-----------------------------------------------------------------------------}
function  ReadLibraryPath(const _DelphiVersion:integer;out DelphiLibraryPath:TDelphiLibraryPath):boolean;
var
_Key:string;

  function _ReadLibraryPath(_RootKey:HKEY):boolean;
  var
  _Reg:TRegistry;
  begin
    result:=false;
    _Reg := TRegistry.Create(KEY_READ or KEY_WOW64_32KEY);
    try
      _Reg.RootKey := _RootKey;
      if not _Reg.OpenKeyReadOnly(_Key) then begin
        trace(5,'Warning in _ReadLibraryPath: The Key <%s,%s> could not be opened in the registry.',[HKEYToStr(_RootKey),_Key]);
        exit;
      end;
      try
        DelphiLibraryPath.BrowsingPath:=_Reg.ReadString('Browsing Path');
        DelphiLibraryPath.DebugDCUpath:=_Reg.ReadString('Debug DCU Path');
        DelphiLibraryPath.DCPpath     :=_Reg.ReadString('Package DCP Output');
        DelphiLibraryPath.BPLpath     :=_Reg.ReadString('Package DPL Output');
        DelphiLibraryPath.PackagePath :=_Reg.ReadString('Package Search Path');
        DelphiLibraryPath.Searchpath  :=_Reg.ReadString('Search Path');
        result:=true;
      except
        on e:exception do trace(1,'Warning in _ReadLibraryPath: Could not read Library-Settings for delphi version <%s>.You need to have Admin rights for this computer. <%s>',[_DelphiVersion,e.message]);
      end;
      _Reg.CloseKey;
    finally
      _Reg.free;
    end;
  end;
begin
  result:=false;
  if not GetIDERootKey(_DelphiVersion,_Key) then begin
    trace(3,'Problem in ReadLibraryPath: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;
  _Key:=_Key+'library\';
  result:=_ReadLibraryPath(HKEY_LOCAL_MACHINE);
  if not result then result:=_ReadLibraryPath(HKEY_CURRENT_USER);
end;

{-----------------------------------------------------------------------------
  Procedure: GetDelphiPathTag
  Author:    sam
  Date:      08-Mrz-2007
  Arguments: const _version:integer
  Result:    string
  Description: returns $(DELPHI) or $(BDS) according to the version number
-----------------------------------------------------------------------------}
function  GetDelphiPathTag(const _version:integer):string;
begin
  if _version<=7 then result:=cDelphiTag
                 else result:=cBDSTag;
  result:=lowercase(result);               
end;

{-----------------------------------------------------------------------------
  Procedure: CleanupByRegistry
  Author:    sam
  Date:      23-Okt-2006
  Arguments: const _DelphiVersion:integer
  Result:    boolean
  Description: iterate through the known packages and check if the referenced
               file really exists. if not then delete the registry entry.
               This method returns true if an invalid key get's deleted from the registry.
-----------------------------------------------------------------------------}
function  CleanupByRegistry(const _ROOTKEY:DWORD;const _DelphiSubKey:string;const _DelphiVersion:integer;var NoOfRemovedKeys:integer; const _CurrentPlatform,_CurrentConfig: string):boolean; // find registry-entries without the packages
var
i:integer;
_DelphiRootDirKey:string;
_Reg: TRegistry;
_ValueNames:TStrings;
_packageName:string;
begin
  result:=true;
  if not GetIDERootKey(_DelphiVersion,_DelphiRootDirKey) then begin
    trace(3,'Problem in CleanupByRegistry: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;
  _DelphiRootDirKey:=_DelphiRootDirKey+_DelphiSubKey;
  _Reg := TRegistry.Create(KEY_READ or KEY_WRITE or KEY_WOW64_32KEY);
  _ValueNames:=TStringList.create;
  try
    _Reg.RootKey := _ROOTKEY;
    if not _Reg.OpenKey(_DelphiRootDirKey,false) then begin
      trace(1,'CleanupByRegistry: The Key <%s/%s> was not found in the registry or no access rights.',[HKEYToStr(_RootKey),_DelphiRootDirKey]);
      exit;
    end;
    _Reg.GetValueNames(_ValueNames);
    try
      for i:=0 to _ValueNames.count-1 do begin
        _packageName:=lowercase(ReplaceTag(_ValueNames[i],_DelphiVersion,_CurrentPlatform,_CurrentConfig));
        if fileexists(_packageName) then continue;
        if not _Reg.DeleteValue(_ValueNames[i]) then begin
          trace(3,'Problem in CleanupByRegistry: Could not remove value <%s> from key <%s,%s> from registry for delphi <%d>.',[_ValueNames[i],HKEYToStr(_RootKey),_DelphiRootDirKey,_DelphiVersion]);
          result:=false;
          continue;
        end;
        trace(3,'Removed value <%s> from registry key <%s,%s> because the referenced file <%s> does not exist.',[_ValueNames[i],HKEYToStr(_RootKey),_DelphiRootDirKey,_packageName]);
        inc(NoOfRemovedKeys);
      end;
    except
      on e:exception do trace(1,'Problem in CleanupByRegistry: Could not remove value <%s> from registry key <%s,%s>.<%s>.',[_packageName,HKEYToStr(_RootKey),_DelphiRootDirKey,e.Message]);
    end;
  finally
    _Reg.CloseKey;
    _reg.free;
    _ValueNames.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: RemoveValueFromRegistry
  Author:    sam
  Date:      30-Aug-2002
  Arguments: _RootKey:HKey;_Key,_PackageName:string
  Result:    None
  Description: remove the value <_PackageName> from the registry
-----------------------------------------------------------------------------}
function RemoveValueFromRegistry(_RootKey:HKey;_Key,_PackageName:string):boolean;
var
  _Reg: TRegistry;
begin
  result:=false;
  _PackageName:=lowercase(_PackageName);
  _Reg := TRegistry.Create(KEY_READ or KEY_WRITE or KEY_WOW64_32KEY);
  try
    _Reg.RootKey := _RootKey;
    if not _Reg.OpenKey(_Key,false) then begin
      trace(5,'RemoveValueFromRegistry: The package <%s> was not found in the registry Key <%s,%s> or no access rights.',[_PackageName,HKEYToStr(_RootKey),_Key]);
      exit;
    end;
    if not _Reg.ValueExists(_PackageName) then begin
      trace(5,'RemoveValueFromRegistry: Could not find Key Value <%s> in <%s,%s>.',[_PackageName,HKEYToStr(_RootKey),_key]);
      exit;
    end;
    if not _Reg.DeleteValue(_PackageName) then begin
      trace(5,'RemoveValueFromRegistry: Could not delete Key Value <%s> in <%s,%s>.',[_PackageName,HKEYToStr(_RootKey),_key]);
      exit;
    end;
    result:=true;
    trace(5,'RemoveValueFromRegistry: Successfully deleted Key Value <%s> from <%s,%s>.',[_PackageName,HKEYToStr(_RootKey),_Key]);
  finally
    _Reg.CloseKey;
    _Reg.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: CleanUpPackagesByPath
  Author:    sam
  Date:      05-Jul-2006
  Arguments: const _DelphiVersion:integerconst _Path:stringconst _deletefiles:boolean
  Result:    boolean
  Description: find all files .dcp/.bpl files in the folder <_Path> and delete them. remove the registry entries also.
-----------------------------------------------------------------------------}
function CleanUpPackagesByPath(const _DelphiVersion:integer;_BPLPath:string;_DCPPath:string;const _deletefiles:boolean; const _CurrentPlatform,_CurrentConfig: string):boolean; // this method delete's the packages located in ($DELPHI)\Projects\Bpl and removes the key's from the registery.
var
i:integer;
_fileList:TStrings;
_PackageKey:string;
_filename:string;
_NoOfDeletedKeys:integer;
_NoOfBPLFilesFound:integer;
_NoOfDCPFilesFound:integer;
begin
  result:=false;
  _BPLPath:=IncludeTrailingPathDelimiter(_BPLPath);
  if not GetIDERootKey(_DelphiVersion,_PackageKey) then begin
    trace(3,'Problem in CleanUpPackagesByBPLPath: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;
  _fileList:=TStringList.create;
  try
    AllFilesOfPath(_BPLPath,'*.bpl',_fileList,true);
    _NoOfBPLFilesFound:=_filelist.count;
    for i:=0 to _NoOfBPLFilesFound-1 do begin
      _filename:=_filelist[i];
      RemoveValueFromRegistry(HKEY_CURRENT_USER ,_PackageKey+'Known Packages',_BPLPath+_filename);
      RemoveValueFromRegistry(HKEY_LOCAL_MACHINE,_PackageKey+'Known Packages',_BPLPath+_filename);
      RemoveValueFromRegistry(HKEY_CURRENT_USER ,_PackageKey+'Disabled Packages',_BPLPath+_filename);
      RemoveValueFromRegistry(HKEY_LOCAL_MACHINE,_PackageKey+'Disabled Packages',_BPLPath+_filename);
      if _deletefiles then begin
        if uDPTDelphiPackage.DeleteFile(_BPLPath+_filename) then result:=true;
        _filename:=changefileext(_filename,'.dcp');
        if uDPTDelphiPackage.DeleteFile(_DCPPath+_filename) then result:=true;
      end;
    end;
    _fileList.clear;
    AllFilesOfPath(_BPLPath,'*.dcp',_fileList,true);
    _NoOfDCPFilesFound:=_filelist.count;
    for i:=0 to _filelist.count-1 do begin
      _filename:=_filelist[i];
      if _deletefiles then begin
        if uDPTDelphiPackage.DeleteFile(_DCPPath+_filename) then result:=true;
        _filename:=changefileext(_filename,'.bpl');
        if uDPTDelphiPackage.DeleteFile(_BPLPath+_filename) then result:=true;
      end;
    end;
    if (_NoOfDCPFilesFound=0) and
       (_NoOfBPLFilesFound=0) then result:=true;  // nothing to delete.
  finally
    _fileList.free;
  end;
  VerifyRegistry(_DelphiVersion,_NoOfDeletedKeys,_CurrentPlatform,_CurrentConfig);
end;


{-----------------------------------------------------------------------------
  Procedure: CleanUpPackagesByRegistry
  Author:    sam
  Date:      28-Jun-2006
  Arguments: const _DelphiVersion:integer
  Result:    boolean
  Description:
  this method delete's the key HKEY_LOCAL_MACHINE/Software/Borland/Delphi/%VERSIONNO%/Known Packages and
  HKEY_CURRENT_USER/Software/Borland/Delphi/%VERSIONNO%/Known Packages except
  the packages located in the path <_DelphiBINPath>.
  if <_deletefiles> is set to true, then the corresponding dcp and bpl files will be deleted.
-----------------------------------------------------------------------------}
function CleanUpPackagesByRegistry(const _ROOTKEY:DWORD;const _DelphiVersion:integer;const _DelphiSubKey:string;const _deletefiles:boolean; const _CurrentPlatform,_CurrentConfig: string):boolean; //
var
  i:integer;
  _DelphiRootDirKey:string;
  _DelphiStdPackagesWin32Dir: string;
  _DelphiStdPackagesWin64Dir: string;
  _DelphiStdPackagesCurPlatformDir: string;
  _Reg: TRegistry;
  _ValueNames:TStrings;
  _packageName:string;
begin
  result:=false;
  if not GetIDERootKey(_DelphiVersion,_DelphiRootDirKey) then begin
    trace(3,'Problem in CleanUpPackagesByRegistry: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;

  _DelphiStdPackagesWin32Dir       := GetDelphiStdPackagesDir(_DelphiVersion, sWin32);
  _DelphiStdPackagesWin64Dir       := GetDelphiStdPackagesDir(_DelphiVersion, sWin64);
  _DelphiStdPackagesCurPlatformDir := GetDelphiStdPackagesDir(_DelphiVersion, _CurrentPlatform);

  if (_DelphiStdPackagesCurPlatformDir = '') and (_DelphiStdPackagesWin32Dir = '') and (_DelphiStdPackagesWin64Dir = '') then
  begin
    trace(3,'Problem in CleanUpPackagesByRegistry: Directory of platform dependent standard packages not found. Delphi Version <%d>, platform <%s>',[_DelphiVersion, _CurrentPlatform]);
    exit;
  end;

  _Reg := TRegistry.Create(KEY_READ or KEY_WRITE or KEY_WOW64_32KEY);
  _ValueNames:=TStringList.create;
  try
    _Reg.RootKey := _ROOTKEY;
    _DelphiRootDirKey:=_DelphiRootDirKey+_DelphiSubKey;
    if not _Reg.OpenKey(_DelphiRootDirKey,false) then begin
      trace(3,'Warning in CleanUpPackagesByRegistry: The Key <%s,%s> was not found in the registry.',[HKEYToStr(_ROOTKEY),_DelphiRootDirKey]);
      exit;
    end;
    _Reg.GetValueNames(_ValueNames);
    try
      for i:=0 to _ValueNames.count-1 do begin
        if _CurrentPlatform <> '' then
        begin
          _packageName:=_ValueNames[i];
          _packageName:=ReplaceTag(_packageName,_DelphiVersion, _CurrentPlatform,_CurrentConfig);
          if pos(lowercase(_DelphiStdPackagesCurPlatformDir),lowercase(_packageName))<>0 then continue;
        end
        else
        begin
          _packageName:=_ValueNames[i];
          _packageName:=ReplaceTag(_packageName,_DelphiVersion, sWin32,_CurrentConfig);
          if pos(lowercase(_DelphiStdPackagesWin32Dir),lowercase(_packageName))<>0 then continue;

          _packageName:=_ValueNames[i];
          _packageName:=ReplaceTag(_packageName,_DelphiVersion, sWin64,_CurrentConfig);
          if pos(lowercase(_DelphiStdPackagesWin64Dir),lowercase(_packageName))<>0 then continue;
        end;

        if not _Reg.DeleteValue(_ValueNames[i]) then begin
          trace(3,'Problem in CleanUpPackagesByRegistry: Could not delete package <%s> for delphi <%d>.',[_ValueNames[i],_DelphiVersion]);
          continue;
        end;

        trace(5,'Deleted Package <%s> for delphi version <%d> from registry.',[_packageName,_DelphiVersion]);

        if _deletefiles then begin
          if uDPTDelphiPackage.DeleteFile(_packageName) then trace(5,'CleanUpPackagesByRegistry: Deleted File <%s> for delphi version <%d>.',[_packageName,_DelphiVersion]);
          _packageName:=ChangeFileExt(_packageName,'.dcp');
          if uDPTDelphiPackage.DeleteFile(_packageName) then trace(5,'CleanUpPackagesByRegistry: Deleted File <%s> for delphi version <%d>.',[_packageName,_DelphiVersion]);
        end;
      end;
      result:=true;
    except
      on e:exception do trace(1,'Warning in CleanUpPackagesByRegistry: Could not delete the key <%s> for delphi version <%s>.You need to have Admin rights for this computer.%s',[_DelphiRootDirKey,_DelphiVersion,e.message]);
    end;
  finally
    _ValueNames.free;
    _Reg.CloseKey;
    _Reg.Free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: CleanUpPackageByEnvPaths
  Author:    sam
  Date:      18-Mai-2016
  Arguments: None
  Result:    boolean
  Description: 1.) get all paths from environment variable.
               2.) search all  bpl and dcp files in this paths.
               3.) display a dialog to the user where he can select the
                   files to be deleted.
-----------------------------------------------------------------------------}
function CleanUpPackageByEnvPaths(const _DelphiVersion:integer;const _silent:boolean):boolean;
var
i,j:integer;
_EnvPaths:TStrings;
_FilesOfPath:TStringList;
_FilesToDisplay:TStringList;
_FilesToDelete:TStringList;
_path:string;
_file:string;
begin
  result:=false;
  _FilesOfPath:=TStringList.Create;
  _FilesToDisplay:=TStringList.Create;
  _EnvPaths:=GetIDEEnvironmentPathList(_DelphiVersion);
  try
    for i:=0 to _EnvPaths.Count-1 do begin
      _path:=_EnvPaths[i]+'\';
      _FilesOfPath.Clear;
      AllFilesOfPath(_path,'*.bpl',_FilesOfPath);
      for j:=0 to _FilesOfPath.Count-1 do _FilesToDisplay.add(_path+_FilesOfPath[j]);
      _FilesOfPath.Clear;
      AllFilesOfPath(_path,'*.dcp',_FilesOfPath);
      for j:=0 to _FilesOfPath.Count-1 do _FilesToDisplay.add(_path+_FilesOfPath[j]);
    end;
    if _FilesToDisplay.count=0 then exit; // no files found, no need to show the dialog.
    if not SelectFilesDlg('Please select files to be deleted.','Delete selected files',_FilesToDisplay,_Silent,_FilesToDelete) then exit;
    if _FilesToDelete.Count=0 then exit; // now file marked for deletion
    for i:=0 to _FilesToDelete.Count-1 do begin
      _file:=_FilesToDelete[i];
      uDPTDelphiPackage.DeleteFile(_File);
    end;
  finally
    _FilesToDisplay.free;
    _EnvPaths.free;
    _FilesToDelete.free;
    _FilesOfPath.Free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: VersionNoToIDEName
  Author:    sam
  Date:      02-Jun-2008
  Arguments: _version:integer
  Result:    string
  Description:   information taken from http://delphi.wikia.com/wiki/Delphi_Release_Dates
-----------------------------------------------------------------------------}
function VersionNoToIDEName(const _version:integer;const _NameType:TDelphiNameType=tdn_long):string;
begin
  result:='unknown version';
  if (_version<1) or (_version>length(DelphiVersions)) then exit;
  case _NameType of
    tdn_long :result:=DelphiVersions[_version].LongName;
    tdn_short:result:=DelphiVersions[_version].ShortName;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: IDENameToVersionNo
  Author:    sam
  Date:      17-Mrz-2006
  Arguments: _version:string
  Result:    integer
  Description: method to convert a application version into a version number
-----------------------------------------------------------------------------}
function IDENameToVersionNo(_version:string):integer; // turns the ide name 6.0 into 6 or bds 4.0 into 10.
var
i:integer;
begin
  result:=0;
  _version:=uppercase(trim(_version));
  for i:=1 to length(DelphiVersions) do begin
    if (_version<>uppercase(DelphiVersions[i].LongName)) and
       (_version<>uppercase(DelphiVersions[i].ShortName)) and
       (_version<>uppercase(DelphiVersions[i].VersionStr)) and
       (_version<>uppercase(DelphiVersions[i].IDEVersionStr)) then continue;
    result:=i;
    exit;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: OutputFilename
  Author:    sam
  Date:      12-Mrz-2005
  Arguments: _filename:string
  Result:    string
  Description: returns the output filename of the project file <_filename>.
  For example : Package1.dpk will be converted into Package1.bpl
                Application1.dpr will be converted into Application1.exe
                library1.dpr will be converted into library1.dll

-----------------------------------------------------------------------------}
function  OutputFilename(const _filename:string;const _ProjectType:TProjectType;const _libsuffix:string=''):string; // input is a source code filename, output is the name of the compiled target.
begin
  result:='unkownfile';
  case _ProjectType of
    tp_dll:result:=changeFileExt(extractFilename(_filename),'.dll');      // project is a dll
    tp_exe:result:=changeFileExt(extractFilename(_filename),'.exe');      // project is a executable
    tp_bpl:result:=extractFilenameOnly(_filename)+_libsuffix+'.bpl';      // project is a package.
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: DetermProjectTypeDelphi
  Author:    sam
  Date:      12-Mrz-2005
  Arguments: _projectfilename:string
  Result:    TProjectType
  Description: find out if the project is a package an exe or a dll.
               This method is used for Delphi 3-7
-----------------------------------------------------------------------------}
function  DetermProjectTypeDelphi(_projectfilename:string):TProjectType;
var
_ProjectFile:TStrings;
_sourcetext:string;
_ProjectName:string;
begin
  result:=tp_unkown;
  if not fileexists(_projectfilename) then begin
    if lowercase(ExtractFileExt(_projectfilename))='.dpk' then result:=tp_bpl
                                                          else result:=tp_exe;
    exit;
  end;
  _projectfilename:=ReadProjectFilenameFromDProj(_projectfilename);
  if not fileexists(_projectfilename) then exit;
  _ProjectFile:=TStringList.Create;
  try
    _ProjectFile.LoadFromFile(_projectfilename);
    _ProjectName:=lowercase(ExtractFilenameOnly(_projectfilename));
    if _ProjectFile.Count>0 then begin
      _sourcetext:=lowercase(_ProjectFile.Text);
      if Pos('program '+_ProjectName+';',_sourcetext)>0 then result:=tp_exe
      else if Pos('library '+_ProjectName+';',_sourcetext)>0 then result:=tp_dll
      else if Pos('package '+_ProjectName+';',_sourcetext)>0 then result:=tp_bpl;
    end;
  finally
    _ProjectFile.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: DetermProjectTypeBDS
  Author:    sam
  Date:      26-Mai-2008
  Arguments: const _projectfilename:string;const _projectGroupfilename
  Result:    TProjectType
  Description: find out if the project is a package an exe or a dll.
               This method is used for BDS 1.0-....
-----------------------------------------------------------------------------}
function  DetermProjectTypeBDS(const _projectfilename:string;const _projectGroupfilename:string):TProjectType;
var
_XMLFile:xmlIntf.IXMLDocument;
_Projects:String;
_count:integer;
_filenameOnly:string;
_pos:integer;
_fileext:string;
begin
  result:=tp_unkown;
  if not fileExists(_projectGroupfilename) then begin
    trace(5,'DetermProjectTypeBDS: Could not find the file <%s>.',[_projectGroupfilename]);
    exit;
  end;
  _filenameOnly:=lowercase(ExtractFilenameOnly(_projectfilename));
  _XMLFile:=newXMLDocument;
  try
    _XMLFile.LoadFromFile(_projectGroupfilename);
    _XMLFile.active:=true;
    _count:=_XMLFile.DocumentElement.ChildNodes['Default.Personality'].ChildNodes['Projects'].ChildNodes.Count;
    if _count=0 then exit;
    _Projects:=_XMLFile.DocumentElement.ChildNodes['Default.Personality'].ChildNodes['Projects'].ChildNodes[_count-1].text;
    trace(5,'DetermProjectTypeBDS: Read projects <%s> from file <%s>.',[_Projects,_projectGroupfilename]);
    _pos:=Pos(_filenameOnly+'.',lowercase(_Projects));
    if _pos=0 then begin
      trace(1,'Warning in DetermProjectTypeBDS: The project <%s> does not seem to be in the project group file <%s>.',[_filenameOnly,_projectGroupfilename]);
      exit;
    end;
    delete(_Projects,1,_pos+length(_filenameOnly));
    _fileext:=lowercase(GetField(' ',_Projects));
    if _fileext='exe' then begin
      trace(5,'DetermProjectTypeBDS: its an exe-file.',[]);
      result:=tp_exe;
    end else
    if _fileext='dll' then begin
      trace(5,'DetermProjectTypeBDS: its a dll-file.',[]);
      result:=tp_dll;
    end else
    if _fileext='bpl' then begin
      trace(5,'DetermProjectTypeBDS: its a bpl-file.',[]);
      result:=tp_bpl;
    end;
  finally
    _XMLFile.active:=false;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: DetermProjectTypeGroupProj
  Author:    sam
  Date:      15-Mrz-2010
  Arguments: const _projectfilename:string;const _projectGroupfilename:string
  Result:    TProjectType
  Description:
-----------------------------------------------------------------------------}
function  DetermProjectTypeGroupProj(const _projectfilename:string;const _projectGroupfilename:string):TProjectType;
begin
  result:=tp_unkown;
  showmessage('TODO: Reading of project type from a .groupproj file is not implemented yet.');
end;

{*-----------------------------------------------------------------------------
  Procedure: DetermProjectType
  Author:    sam
  Date:      29-Aug-2009
  Arguments: _projectfilename:string;const _projectGroupfilename:string;const _DelphiVersion:integer
  Result:    TProjectType
  Description: find out if the project <_projectfilename> is a .exe,.dll or a .bpl.
-----------------------------------------------------------------------------}
function  DetermProjectType(_projectfilename:string;const _projectGroupfilename:string;const _DelphiVersion:integer):TProjectType;
begin
  result:=tp_unkown;
  _projectfilename:=ReadProjectFilenameFromDProj(_projectfilename);
  if (lowercase(ExtractFileext(_projectfilename))='.dpk') or
     (lowercase(ExtractFileext(_projectfilename))='.dpr') or
     (lowercase(ExtractFileext(_projectGroupfilename))='.bpg') then result:=DetermProjectTypeDelphi(_projectfilename)
  else
  if (lowercase(ExtractFileext(_projectfilename))='.bdsgoup')  then result:=DetermProjectTypeBDS(_projectfilename,_projectGroupfilename)
  else
  if (lowercase(ExtractFileext(_projectfilename))='.groupproj')then result:=DetermProjectTypeGroupProj(_projectfilename,_projectGroupfilename);
end;

{-----------------------------------------------------------------------------
  Procedure: PrepapreRegistryPath
  Author:    sam
  Date:      15-Jun-2007
  Arguments: _filename:string
  Result:    string
  Description:
-----------------------------------------------------------------------------}
function PrepapreRegistryPath(_filename:string):string;
var
i:integer;
begin
  result:='';
  for i:=1 to length(_filename) do begin
    result:=result+_filename[i];
    if _filename[i]='\' then result:=result+'\';
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: GetPackageVersion
  Author:    sam
  Date:      13-Jul-2004
  Arguments: _PackageName:string
  Result:    string
  Description: read the version number of the package
-----------------------------------------------------------------------------}
function GetPackageVersion(const _PackageName,_PackageOutputPath,_PackageLibSuffix:string;const _ProjectType:TProjectType):string;
var
_filename:string;
_path:string;
begin
  _filename:=OutputFilename(_PackageName,_ProjectType,_PackageLibSuffix);
  _path:=IncludeTrailingPathDelimiter(_PackageOutputPath);
  trace(5,'Try to read version of file <%s>.',[_path+_filename]);
  result:=uDPTMisc.GetFileVersion(_path+_filename);
end;

{-----------------------------------------------------------------------------
  Procedure: GetPackageSize
  Author:    sam
  Date:      06-Mai-2005
  Arguments: _PackageName:string;_PackageOutputPath:string
  Result:    Int64
  Description: read the filesize of the package.
-----------------------------------------------------------------------------}
function GetPackageSize(_PackageName,_PackageOutputPath,_PackageLibSuffix:string;const _ProjectType:TProjectType):Int64;
var
_filename:string;
_path:string;
begin
  _filename:=OutputFilename(_PackageName,_ProjectType,_PackageLibSuffix);
  _path:=IncludeTrailingPathDelimiter(_PackageOutputPath);
  trace(5,'Try to read size of file <%s>.',[_path+_filename]);
  uDPTMisc.GetFileSize(_path+_filename,result);
end;

{-----------------------------------------------------------------------------
  Procedure: InitBatchFile
  Author:    sam
  Date:      06-Jul-2004
  Arguments: _RegFilePath:string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure InitBatchFile(const _filename:string); // reset batch file an define the location where the reg-files will be placed.
begin
  FBatchFile.Clear;
  FBatchFilename:=_filename;
end;

{-----------------------------------------------------------------------------
  Procedure: SaveBatchFile
  Author:    sam
  Date:      06-Jul-2004
  Arguments: _filename:string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
function SaveBatchFile:string; // save the batch file to _filename
begin
  result:='';
  if not gCreateBatchFile then exit;
  try
    CheckDirectory(extractfilepath(FBatchFilename));
    FBatchFile.SaveToFile(FBatchFilename);
    result:=FBatchFilename;
    ShowFolder(extractfilepath(FBatchFilename));
  except
    on e:exception do trace(1,'Error in SaveBatchFile: Could not save file <%s>. <%s>.',[FBatchFilename,e.message]);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: GetDelphiApplication
  Author:    Samuel Herzog
  Date:      10-Jan-2004
  Arguments: const _DelphiVersion:integer
  Result:    string
  Description: 25.02.2006 - changes to also support BDS.
-----------------------------------------------------------------------------}
function  GetDelphiApplication(const _DelphiVersion:integer):string; // returns the name and full path of the delphi32.exe file.
var
_DelphiRootDirKey:string;

  function _GetDelphiApplication(_RootKey:HKEY;_Key:string):string;
  var
  _Reg: TRegistry;
  begin
    result:='';
    _Reg := TRegistry.Create(KEY_READ or KEY_WOW64_32KEY);
    try
      _Reg.RootKey := _RootKey;
      if not _Reg.OpenKeyReadOnly(_Key) then begin
        trace(5,'Warning in _GetDelphiApplication: The Key <%s,%s> was not found in the registry.',[HKEYToStr(_RootKey),_Key]);
        exit;
      end;
      try
        result:=_Reg.ReadString('App');
        if result<>'' then begin
          trace(5,'_GetDelphiApplication: Found Delphi <%s> for delphi version <%d>.',[result,_DelphiVersion]);
        end else trace(5,'Problem in _GetDelphiApplication: Could not find root directory for delphi <%d>.',[_DelphiVersion]);
      except
        trace(1,'Warning in _GetDelphiApplication: Could not read the delphi root directory for delphi version <%s>.You need to have Admin rights for this computer.',[_DelphiVersion]);
      end;
      _Reg.CloseKey;
    finally
      _Reg.Free;
    end;
  end;

begin
  result:='';
  if not GetIDERootKey(_DelphiVersion,_DelphiRootDirKey) then begin
    trace(5,'Warning in GetDelphiApplication: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;
  result:=_GetDelphiApplication(HKEY_LOCAL_MACHINE,_DelphiRootDirKey);
  if result='' then result:=_GetDelphiApplication(HKEY_CURRENT_USER,_DelphiRootDirKey);
end;

{-----------------------------------------------------------------------------
  Procedure: GetInstalledDelphiVersions
  Author:    sam
  Date:      25-Feb-2006
  Arguments: _list:TStrings
  Result:    boolean
  Description: read the registry to try to find the installed delphi/bds versions.
-----------------------------------------------------------------------------}
function GetInstalledDelphiVersions(_basekey:string;_list:TStrings):boolean;
var
i:integer;

  procedure GetSubKeys(_RootKey:HKey);
  var
  _reg: TRegistry;
  begin
    _reg := TRegistry.Create(KEY_READ or KEY_WOW64_32KEY);
    try
      try
        _reg.RootKey := _RootKey;
        if not _reg.OpenKeyReadOnly(_basekey) then exit;
        if _reg.HasSubKeys then _reg.GetKeyNames(_list);
        _reg.CloseKey;
      except
        on e:exception do trace(1,'Error in GetInstalledDelphiVersions: <%s>.',[e.Message]);
      end;
    finally
      _reg.free;
    end;
  end;

begin
  result:=false;
  if not assigned(_list) then exit;
  GetSubKeys(HKEY_LOCAL_MACHINE);    // first look at local machine.
  if _list.Count=0 then GetSubKeys(HKEY_CURRENT_USER); // if nothing is found then lookup the current user.
  for i:=0 to _list.count-1 do trace(5,'GetInstalledDelphiVersions: Found <%s> for <%s>.',[_list[i],_basekey]);
  result:=(_list.count>0);
end;

{-----------------------------------------------------------------------------
  Procedure: GetInstalledIDEVersions
  Author:    sam
  Date:      25-Feb-2006
  Arguments: _list:TStrings
  Result:    boolean
  Description: return a list with the installed IDE's.
-----------------------------------------------------------------------------}
function GetInstalledIDEVersions(_list:TStrings):boolean;
var
i:integer;
_tmp:TStrings;
_version:integer;
_delphiExeFilename:string;
_fVersion:extended;
_sVersion:string;
begin
  _list.Clear;
  GetInstalledDelphiVersions(cDelphiKey,_list);
  _tmp:=TStringList.create;
  try
    GetInstalledDelphiVersions(cBorlandBDSKey,_tmp);
    for i:=0 to _tmp.Count-1 do begin
      _sVersion:=_tmp[i];
      if not StringToFloat(_sversion,_fVersion) then continue;
      _list.Add(inttostr(trunc(_fVersion)+6));
    end;
    _tmp.Clear;
    GetInstalledDelphiVersions(cCodeGearBDSKey,_tmp);
    for i:=0 to _tmp.Count-1 do begin
      _sVersion:=_tmp[i];
      if not StringToFloat(_sversion,_fVersion) then continue;
      if _fVersion<7 then _list.Add(inttostr(trunc(_fVersion)+6))
                     else _list.Add(inttostr(trunc(_fVersion)+7))
    end;
    _tmp.Clear;
    GetInstalledDelphiVersions(cEmbarcaderoBDSKey,_tmp);
    for i:=0 to _tmp.Count-1 do begin
      _sVersion:=_tmp[i];
      if not StringToFloat(_sversion,_fVersion) then continue;
      if _fVersion<7 then _list.Add(inttostr(trunc(_fVersion)+6))
                     else _list.Add(inttostr(trunc(_fVersion)+7))
    end;
  finally
    _tmp.free;
  end;
  i:=0;
  while i<_list.count do begin     // found n entries in the registry. Now check if referenced compiler file is really available.
    StringToFloat(_list.Strings[i],_fVersion);
    _version:=trunc(_fVersion);
    _delphiExeFilename:=GetDelphiApplication(_version);
    if _delphiExeFilename='' then _list.Delete(i)
    else begin
      if fileexists(_delphiExeFilename) then begin
        trace(3,'Found IDE <%s> for Version <%s>. <%s>. ',[_delphiExeFilename,_list.Strings[i],VersionNoToIDEName(_version)]);
        inc(i);
      end
      else begin
        trace(3,'Found the Registry entry for IDE <%s> but the file <%s> is not present.',[_list.Strings[i],_delphiExeFilename]);
        _list.Delete(i);  // remove item.
      end;
    end;
  end;
  i:=0;
  while i<_list.count do begin
    StringToFloat(_list.Strings[i],_fVersion);
    _version:=trunc(_fVersion);
    _list[i]:=VersionNoToIDEName(_version,tdn_long);
    inc(i);
  end;
  result:=(_list.count>0);
end;

{-----------------------------------------------------------------------------
  function: GetDelphiRootDir
  Author:    Not available
  Date:      03-Jul-2003
  Arguments: const _Version:integer
  Result:    string
  Description: returns delphi root directory e.g. <C:\Program files\Borland\Delphi7>
-----------------------------------------------------------------------------}
function GetDelphiRootDir(const _DelphiVersion:integer):string;
var
_DelphiRootDirKey:string;
_DelphiRootPath:string;


function _GetDelphiRootDir(_RootKey:HKEY):string;
var
_Reg: TRegistry;
begin
  result:='';
  _Reg := TRegistry.Create(KEY_READ or KEY_WOW64_32KEY);
  try
    _Reg.RootKey := _RootKey;
    if not _Reg.OpenKeyReadOnly(_DelphiRootDirKey) then begin
      trace(5,'Problem in _GetDelphiRootDir: The Key <%s> was not found in the registry.',[_DelphiRootDirKey]);
      exit;
    end;
    try
      _DelphiRootPath:=_Reg.ReadString('RootDir');
      if _DelphiRootPath='' then begin
        trace(5,'Problem in _GetDelphiRootDir: Could not get root directory for delphi <%d>.',[_DelphiVersion]);
        exit;
      end;
      result:=IncludeTrailingPathDelimiter(_DelphiRootPath);
      trace(5,'_GetDelphiRootDir: Delphi root directory <%s> for delphi version <%d>.',[_DelphiRootPath,_DelphiVersion]);
    except
      on e:exception do trace(1,'Warning in _GetDelphiRootDir: Could not read the delphi root directory for delphi version <%s>.You need to have Admin rights for this computer. <%s>.',[_DelphiVersion,e.message]);
    end;
    _Reg.CloseKey;
  finally
    _Reg.Free;
  end;
end;

begin
  result:='';
  if not GetIDERootKey(_DelphiVersion,_DelphiRootDirKey) then begin
    trace(3,'Problem in GetDelphiRootDir: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;
  result:=_GetDelphiRootDir(HKEY_LOCAL_MACHINE);
  if result='' then result:=_GetDelphiRootDir(HKEY_CURRENT_USER);
end;

{*-----------------------------------------------------------------------------
  Procedure: GetDelphiStdPackagesDir
  Author:    Andreas Heim
  Date:      13-Oct-2015
  Arguments: const _DelphiVersion:integer
             const _Platform: integer
  Result:    string
  Description: returns directory with target platform dependent standard packages
               included in Delphi
               e.g. C:\Program files\Embarcadero\RAD Studio\9.0\bin (Win32-Platform)
                    C:\Program files\Embarcadero\RAD Studio\9.0\bin64 (Win64-Platform)
-----------------------------------------------------------------------------}
function GetDelphiStdPackagesDir(const _DelphiVersion: integer; const _PlatformToCompile: string): string;
var
  sRet: string;

begin
  Result := '';

  sRet := GetDelphiRootDir(_DelphiVersion);
  sRet := IncludeTrailingPathDelimiter(sRet) + 'bin';

  if SameText(_PlatformToCompile, sWin64) then
    sRet := sRet + '64';

  if not DirectoryExists(sRet) then begin
    trace(3, 'Problem in GetDelphiStdPackagesDir: Could not get directory of platform dependent standard packages for Delphi version <%d>, target platform <%s>.',[_DelphiVersion, _PlatformToCompile]);
    exit;
  end;

  trace(5, 'GetDelphiStdPackagesDir: Directory of platform dependent standard packages for Delphi version <%d>, target platform <%s>: <%s>',[_DelphiVersion, _PlatformToCompile, sRet]);

  Result := IncludeTrailingPathDelimiter(sRet);
end;


function DelphiPackageDirKey(_DelphiVersion:integer; const _PlatformToCompile: string): string;
var
  _DelphiRootDirKey:string;
begin
  result:='';
  if not GetIDERootKey(_DelphiVersion,_DelphiRootDirKey) then begin
    trace(3,'Problem in GetDelphiPackageDir: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;
  result:=_DelphiRootDirKey+'Library' + PathDelim;
  if _DelphiVersion>=15 then
    result:=result + _PlatformToCompile + PathDelim;
end;

{*-----------------------------------------------------------------------------
  Procedure: GetDelphiPackageDir
  Author:    sam
  Date:      11-Sep-2009
  Arguments: const _DelphiVersion:integer
  Result:    string
  Description: get the bpl-folder from the registry.
-----------------------------------------------------------------------------}
function GetDelphiPackageDir(const _DelphiVersion:integer; const _PlatformToCompile: string):string;
var
_DelphiPackagePath:string;
  function _GetDelphiPackageDir(_RootKey:HKEY):string;
  var
  _Reg: TRegistry;
  _DelphiPackageDirKey: string;
  begin
    _Reg := TRegistry.Create(KEY_READ or KEY_WOW64_32KEY);
    try
      _Reg.RootKey := _RootKey;
      _DelphiPackageDirKey:=DelphiPackageDirKey(_DelphiVersion, _PlatformToCompile);
      if not _Reg.OpenKeyReadOnly(_DelphiPackageDirKey) then begin
         trace(5,'Problem in _GetDelphiPackageDir: The Key <%s> was not found in the registry.',[_DelphiPackageDirKey]);
        exit;
      end;
      try
        _DelphiPackagePath:=_Reg.ReadString('Package DPL Output');
        if _DelphiPackagePath='' then begin
          trace(5,'Problem in _GetDelphiPackageDir: Could not get root directory for delphi <%d>.',[_DelphiVersion]);
          exit;
        end;
        result:=IncludeTrailingPathDelimiter(_DelphiPackagePath);
        trace(5,'_GetDelphiPackageDir: Delphi root directory <%s> for delphi version <%d>.',[_DelphiPackagePath,_DelphiVersion]);
      except
        on e:exception do trace(1,'Error in _GetDelphiPackageDir: Could not read the delphi package directory for delphi version <%s>.You need to have Admin rights for this computer. <%s>.',[_DelphiVersion,e.message]);
      end;
      _Reg.CloseKey;
    finally
      _Reg.Free;
    end;
  end;

begin
  result:=_GetDelphiPackageDir(HKEY_LOCAL_MACHINE);
  if result='' then result:=_GetDelphiPackageDir(HKEY_CURRENT_USER);
end;

{*-----------------------------------------------------------------------------
  Procedure: SetDelphiPackageDir
  Author:    sam
  Date:      29-Sep-2009
  Arguments: const _DelphiVersion:integer;_PackageDir:string;const _silent:boolean
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function SetDelphiPackageDir(const _DelphiVersion:integer;_PackageDir:string;const _silent:boolean;const _PlatformToCompile: string):boolean;
resourcestring
cAskToChangePackageOutputPath='Do you want to change the Delphi''s Package Output Path in the registry to <%s>?';
var
_DelphiRootDirKey:string;

function _SetDelphiPackageDir(_RootKey:HKEY):boolean;
var
_Reg: TRegistry;
_DelphiPackageDirKey: string;
begin
  result:=false;
  _Reg := TRegistry.Create(KEY_READ or KEY_WRITE or KEY_WOW64_32KEY);
  try
    _Reg.RootKey := _RootKey;
    _DelphiPackageDirKey := DelphiPackageDirKey(_DelphiVersion, _PlatformToCompile);
    if not _Reg.OpenKey(_DelphiPackageDirKey,false) then begin
      trace(1,'Problem in _SetDelphiPackageDir: The Key <%s,%s> was not found in the registry.',[HKEYToStr(_RootKey),_DelphiPackageDirKey]);
      exit;
    end;
    try
      _Reg.WriteString('Package DPL Output',_PackageDir);
       trace(3,'Successfully set <Pakcage DPL Output> to <%s>',[_PackageDir]);
      _Reg.WriteString('Package DCP Output',_PackageDir);
       trace(3,'Successfully set <Package DCP Output> to <%s>',[_PackageDir]);
      result:=true;
    except
      on e:exception do trace(1,'Warning in _SetDelphiPackageDir: Could not write the delphi package directory for delphi version <%s>.You need to have Admin rights for this computer. <%s>.',
        [DelphiVersions[_DelphiVersion].LongName,e.message]);
    end;
    _Reg.CloseKey;
  finally
    _Reg.Free;
  end;
end;

begin
  result:=false;
  if SameText(GetDelphiPackageDir(_DelphiVersion,_PlatformToCompile), IncludeTrailingPathDelimiter(_PackageDir)) then begin
    result:=true;
    exit; // is already set to this value
  end;

  if not GetIDERootKey(_DelphiVersion,_DelphiRootDirKey) then begin
    trace(3,'Problem in SetDelphiPackageDir: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;
  if not _Silent then begin
    if Application.MessageBox(pchar(format(cAskToChangePackageOutputPath,[_PackageDir])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDNo then exit;
  end;
  result:=_SetDelphiPackageDir(HKEY_LOCAL_MACHINE) or
          _SetDelphiPackageDir(HKEY_CURRENT_USER);

end;

{-----------------------------------------------------------------------------
  Procedure: ReadDPKfromDProj
  Author:    sam
  Date:      21-Nov-2009
  Arguments: const _filename:string
  Result:    string
  Description: the new-style xml-files contain a node "MainSource" which points to the .dpk file.
-----------------------------------------------------------------------------}
function ReadDPKfromDProj(const _filename:string):string;
var
_msg:string;
_stmt:string;
begin
  result:='n/a';
  _stmt:='//PropertyGroup/MainSource';
  if not ReadNodeText(_filename,_stmt,result,_msg) then begin
    trace(1,'Problem in ReadDPKfromDProj: Could not read Packagename with statement <%s> from file <%s>.',[_stmt,_filename]);
    exit;
  end;
  trace(5,'ReadDPKfromDProj: Extracted Packagename <%s> from file <%s>.',[result,_filename]);
end;

{-----------------------------------------------------------------------------
  Procedure: ReadCFGSettings
  Author:    sam
  Date:      27-Sep-2002
  Arguments: _cfgFilename:String;var _Conditions:String
  Result:    boolean
  Purpose:   get information from the cfg-file.
  History:
-----------------------------------------------------------------------------}
function ReadCFGSettings(const _cfgFilename:String;out Conditions:string;out SearchPath:String;out ProjectOutputPath:string;out BPLOutputPath:string;out DCUOutputPath:string):boolean; // get informations from the cfg-file.
var
  _CFGFile:TStrings;
  _Pos:Integer;
  _Text:String;
begin
  Result:=false;
  ProjectOutputPath:='';
  SearchPath:='';
  BPLOutputPath:='';
  DCUOutputPath:='';
  Conditions:='';
  if not fileExists(_cfgFilename) then begin
    trace(5,'Warning in ReadCFGSettings: Could not find the file <%s>.',[_cfgFilename]);
    exit;
  end;
  _CFGFile:=TStringList.Create;
  try
    _CFGFile.LoadFromFile(_cfgFilename);
    _Text:=_CFGFile.Text;
    _Pos:=Pos('-D',_Text);
    if _pos>0 then begin
      Delete(_text,1,_pos+1);
      _pos:=Pos(#$D+#$A,_Text);
      Conditions:=Copy(_Text,1,_Pos-1);
      trace(5,'ReadCFGSettings: Conditions are <%s>.',[Conditions]);
    end;

    _Text:=_CFGFile.Text;
    _Pos:=Pos('-U"',_Text);
    if _pos>0 then begin
      Delete(_text,1,_pos+2);
      _pos:=Pos('"',_Text);
      SearchPath:=SearchPath+Copy(_Text,1,_Pos-1);
      trace(5,'ReadCFGSettings: SearchPath is <%s>.',[SearchPath]);
    end;

    _Text:=_CFGFile.Text;
    _Pos:=Pos('-E"',_Text);
    if _pos>0 then begin
      Delete(_text,1,_pos+2);
      _pos:=Pos('"',_Text);
      ProjectOutputPath:=Copy(_Text,1,_Pos-1);
      if ProjectOutputPath<>'' then ProjectOutputPath:=IncludeTrailingPathDelimiter(ProjectOutputPath);
      trace(5,'ReadCFGSettings: Project Output Path is <%s>.',[ProjectOutputPath]);
    end;

    _Text:=_CFGFile.Text;
    _Pos:=Pos('-N"',_Text);
    if _pos>0 then begin
      Delete(_text,1,_pos+2);
      _pos:=Pos('"',_Text);
      DCUOutputPath:=Copy(_Text,1,_Pos-1);
      if DCUOutputPath<>'' then DCUOutputPath:=IncludeTrailingPathDelimiter(DCUOutputPath);
      trace(5,'ReadCFGSettings: DCU Output Path is <%s>.',[DCUOutputPath]);
    end;

    _Text:=_CFGFile.Text;
    _Pos:=Pos('-LE"',_Text);
    if _pos>0 then begin
      Delete(_text,1,_pos+3);
      _pos:=Pos('"',_Text);
      BPLOutputPath:=Copy(_Text,1,_Pos-1);
      if BPLOutputPath<>'' then BPLOutputPath:=IncludeTrailingPathDelimiter(BPLOutputPath);
      trace(5,'ReadCFGSettings: BPL Output Path is <%s>.',[BPLOutputPath]);
    end;
  finally
    _CFGFile.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: ReadBDSProjSettings
  Author:    sam
  Date:      27-Mai-2008
  Arguments: const _bdsprojFilename:String;out ProjectName:string;out Conditions:string;out SearchPath:String;out ProjectOutputPath:string;out BPLOutputPath:string;out DCUOutputPath:string
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function ReadBDSProjSettings(const _bdsprojFilename:String;out ProjectName:string;out Conditions:string;out SearchPath:String;out ProjectOutputPath:string;out BPLOutputPath:string;out DCUOutputPath:string):boolean; // get informations from the cfg-file.
var
_msg:string;
_stmt:string;
begin
  Result:=false;
  ProjectOutputPath:='';
  SearchPath:='';
  BPLOutputPath:='';
  DCUOutputPath:='';
  Conditions:='';
  ProjectName:='';
  if not fileExists(_bdsprojFilename) then begin
    trace(5,'ReadBDSProjSettings: Could not find the file <%s>.',[_bdsprojFilename]);
    exit;
  end;
  _stmt:='//Delphi.Personality/Source/Source[@Name="MainSource"]';
  if not ReadNodeText(_bdsprojFilename,_stmt,ProjectName,_msg) then begin
    trace(1,'ReadBDSProjSettings: Could not Project-Name from file <%s> with statement <%s>. <%s>',[_bdsprojFilename,_stmt,_msg]);
    exit;
  end;

  ReadNodeText(_bdsprojFilename,'//Delphi.Personality/Directories/Directories[@Name="Conditionals"]',Conditions,_msg);
  trace(5,'ReadBDSProjSettings: Conditions are <%s>.',[Conditions]);

  ReadNodeText(_bdsprojFilename,'//Delphi.Personality/Directories/Directories[@Name="SearchPath"]',SearchPath,_msg);
  trace(5,'ReadBDSProjSettings: SearchPath is <%s>.',[SearchPath]);

  ReadNodeText(_bdsprojFilename,'//Delphi.Personality/Directories/Directories[@Name="OutputDir"]',ProjectOutputPath,_msg);
  trace(5,'ReadBDSProjSettings: Project Output Path is <%s>.',[ProjectOutputPath]);

  ReadNodeText(_bdsprojFilename,'//Delphi.Personality/Directories/Directories[@Name="UnitOutputDir"]',DCUOutputPath,_msg);
  trace(5,'ReadBDSProjSettings: DCU Output Path is <%s>.',[DCUOutputPath]);

  ReadNodeText(_bdsprojFilename,'//Delphi.Personality/Directories/Directories[@Name="PackageDLLOutputDir"]',BPLOutputPath,_msg);
  trace(5,'ReadBDSProjSettings: BPL Output Path is <%s>.',[BPLOutputPath]);
  result:=true;
end;

{-----------------------------------------------------------------------------
  Procedure: ReadDPROJSettingsD2005_D2007
  Author:    sam
  Date:      03-Mrz-2010
  Arguments: const _dprojFilename: string    path + name of project file
             var _Config:string              out: found configuration  ('' = not found)
             var _Conditions: string
             var _SearchPath: string
             var _ProjectOutputPath: string
             var _BPLOutputPath: string
             var _DCUOutputPath: string
             var _DCPOutputPath: string
  Result:    boolean
  Description: read dproj settings from a file of version D2005-D2007.
-----------------------------------------------------------------------------}
function ReadDPROJSettingsD2005_D2007(const _dprojFilename: string;
                                      out Config: string;
                                      out Conditions: string;
                                      out SearchPath: string;
                                      out ProjectOutputPath: string;
                                      out BPLOutputPath: string;
                                      out DCUOutputPath: string;
                                      out DCPOutputPath: string):boolean; // get informations from the cfg-file.
var
  _msg:string;
  _configuration:string;
  _platform:string;
  _ProjectVersion:string;
begin
  Result:=false;
  Config := '';
  Conditions := '';
  SearchPath := '';
  ProjectOutputPath := '';
  BPLOutputPath := '';
  DCUOutputPath := '';
  DCPOutputPath := '';
  _ProjectVersion := '';
  if not fileExists(_dprojFilename) then begin
    trace(5,'ReadDPROJSettingsD2005_D2007: Could not find the file <%s>.',[_dprojFilename]);
    exit;
  end;
  if not ReadNodeText(_dprojFilename,'//PropertyGroup/ProjectVersion',_ProjectVersion,_msg) then begin
    trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not read condition. <%s>.',[_msg]);
  end;
  if _ProjectVersion='' then begin
      // read default configuration
    if not ReadNodeText(_dprojFilename,'//PropertyGroup/Configuration[@Condition="''$(Configuration)'' == ''''"]',_configuration,_msg) then begin
      trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not read condition. <%s>.',[_msg]);
    end;
    trace(5,'ReadDPROJSettingsD2005_D2007: Configuration = %s.',[_configuration]);
    Config := _configuration;

    if not ReadNodeText(_dprojFilename,'//PropertyGroup/Platform[@Condition="''$(Platform)'' == ''''"]',_platform,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not read platform. <%s>.',[_msg]);
    trace(5,'ReadDPROJSettingsD2005_D2007: Platform = %s.',[_platform]);
    if _configuration='' then begin
       _configuration:='Base';
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$('+_configuration+')''!=''''"]/DCC_UnitSearchPath',SearchPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find SearchPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: SearchPath is <%s>.',[SearchPath]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$('+_configuration+')''!=''''"]/DCC_DcuOutput',DCUOutputPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find DCUOutputPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: DCU Output Path is <%s>.',[DCUOutputPath]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$('+_configuration+')''!=''''"]/DCC_DcpOutput',DCPOutputPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find DCPOutputPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: DCP Output Path is <%s>.',[DCPOutputPath]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$('+_configuration+')''!=''''"]/DCC_BplOutput',BPLOutputPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find BPLOutputPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: BPL Output Path is <%s>.',[BPLOutputPath]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$('+_configuration+')''!=''''"]/DCC_Define',Conditions,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find Conditions. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: Conditions are <%s>.',[Conditions]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$('+_configuration+')''!=''''"]/DCC_ExeOutput',ProjectOutputPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find ProjectOutputPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: Project Output Path is <%s>.',[ProjectOutputPath]);
    end
    else begin
      if _platform<>'' then _configuration:=_configuration+'|'+_platform;
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$(Configuration)|$(Platform)'' == '''+_configuration+'''"]/DCC_UnitSearchPath',SearchPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find SearchPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: SearchPath is <%s>.',[SearchPath]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$(Configuration)|$(Platform)'' == '''+_configuration+'''"]/DCC_DcuOutput',DCUOutputPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find DCUOutputPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: DCU Output Path is <%s>.',[DCUOutputPath]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$(Configuration)|$(Platform)'' == '''+_configuration+'''"]/DCC_DcpOutput',DCPOutputPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find DCPOutputPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: DCP Output Path is <%s>.',[DCPOutputPath]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$(Configuration)|$(Platform)'' == '''+_configuration+'''"]/DCC_BplOutput',BPLOutputPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find BPLOutputPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: BPL Output Path is <%s>.',[BPLOutputPath]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$(Configuration)|$(Platform)'' == '''+_configuration+'''"]/DCC_Define',Conditions,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find Conditions. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: Conditions are <%s>.',[Conditions]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$(Configuration)|$(Platform)'' == '''+_configuration+'''"]/DCC_ExeOutput',ProjectOutputPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find ProjectOutputPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: Project Output Path is <%s>.',[ProjectOutputPath]);
    end;
  end;
  result:=true;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadInheritedAttribute
  Author:    muem
  Date:      26-Oct-2012
  Arguments: _xmlDOMfile: IXMLDOMDocument;  xmlDOM file of dproj
             _statement: string;            XPath statement (e.g. '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute')
                                            兌nheritance will be replaced by _InheritanceList[i]
                                            你ttribute will be replaced by _Attribute
             _InheritanceList:TStringList;
             _Attribute: string;            name of the requested attribute (e.g. 'DCC_Namespace')
  Result:    string
  Description: gets the inhertied attribute from a dproj file used by D2009 or newer.
-----------------------------------------------------------------------------}
function ReadInheritedAttribute(_xmlDOMfile: IXMLDOMDocument;
                               _statement: string;
                               _InheritanceList: TStringList;
                               _Attribute: string): string;
var
  i: Integer;
  _Temp: string;
  _msg: string;
  _XPath: string;
begin
  Result := '';
  for i := 0 to _InheritanceList.Count - 1 do begin
    _Temp := '';
    _XPath := StringReplace(_statement, '兌nheritance', _InheritanceList[i], [rfIgnoreCase, rfReplaceAll]);
    _XPath := StringReplace(_XPath, '你ttribute', _Attribute, [rfIgnoreCase, rfReplaceAll]);
    ReadNodeDocument(_xmlDOMfile, _XPath, _Temp, _msg);
    if _Temp <> '' then Result := StringReplace(_Temp, '$('+_Attribute+')', Result, [rfIgnoreCase, rfReplaceAll]);
  end;
  i := Length(Result);
  if (i > 0) and (Result[i] = ';') then begin
    Delete(Result, i, 1);
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadDPROJSettingsD2009_D2010
  Author:    sam
  Date:      06-Mrz-2010
  Arguments: const _dprojFilename:string      path + name of project file
             var Config:string               in: desired configuration ('' = use default configuration)
                                              out: found configuration  ('' = not found)
             out CompilerSwitches: string;
             out Defines:string
             out SearchPath:string
             out ProjectOutputPath:string
             out BPLOutputPath:string
             out DCUOutputPath:string
             out DCPOutputPath:string
  Result:    boolean
  Description: read path information from a dproj file used by D2009 or D2010.
-----------------------------------------------------------------------------}
function ReadDPROJSettingsD2009_D2010(const _dprojFilename: string;
                                      var Config: string;
                                      out CompilerSwitches: string;
                                      out Defines: string;
                                      out SearchPath: string;
                                      out ProjectOutputPath: string;
                                      out BPLOutputPath: string;
                                      out DCUOutputPath: string;
                                      out DCPOutputPath: string):boolean;
var
  _xmlDOMfile: IXMLDOMDocument;
  _msg: string;
  _desiredConfig: string;
  _BuildConfiguration: string;
  _Configs: TStringList;
  _InheritanceList: TStringList;
  _Tmp: string;
begin
  Result := false;
  _desiredConfig := Config;
  Defines := '';
  SearchPath := '';
  ProjectOutputPath := '';
  BPLOutputPath := '';
  DCUOutputPath := '';
  DCPOutputPath := '';
  CompilerSwitches := '';
  _InheritanceList := TStringList.Create;
  _xmlDOMfile := CoDOMDocument.Create;
  try
    if not _xmlDOMfile.load(_dprojFilename) then begin
      trace(5,'ReadDPROJSettingsD2009_D2010: Could not find the file <%s>.',[_dprojFilename]);
      exit;
    end;

    if Config = '' then begin // read default configuration
      if not ReadNodeDocument(_xmlDOMfile,'/Project/PropertyGroup/Config',Config,_msg) then trace(3,'Warning in ReadDPROJSettingsD2009_D2010: Could not find Config. <%s>.',[_msg]);
    end;
    trace(5,'ReadDPROJSettingsD2009_D2010: Config is <%s>.',[Config]);

    // check for support of configuration
    _Configs := TStringList.Create;
    try
      ReadSupportedConfigsOfProject(_dprojFilename, _Configs);
      if _Configs.IndexOf(Config) < 0 then begin // configuration not supported
        Config := '';
        trace(3,'ReadDPROJSettingsD2009_D2010: Warning config "%s" is not supported.',[_desiredConfig]);
      end;
    finally
      _Configs.Free;
    end;

    if (Config <> '') then begin
      // read properties for desired configuration and platform
      if not ReadNodeDocument(_xmlDOMfile,'/Project/ItemGroup/BuildConfiguration[@Include = "'+Config+'"]/Key',_BuildConfiguration,_msg) then trace(3,'Warning in ReadDPROJSettingsD2009_D2010: Could not find BuildConfiguration. <%s>',[_msg]);

      // Inheritance list for attributes
      _InheritanceList.Clear;
      _InheritanceList.Add('Base');
      _InheritanceList.Add(_BuildConfiguration);

      // Read DCC_Define
      Defines := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_Define');
      trace(5,'ReadDPROJSettingsD2009_D2010: DCC_Define is <%s>.',[Defines]);

      // Read DCC_UnitSearchPath
      SearchPath := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_UnitSearchPath');
      trace(5,'ReadDPROJSettingsD2009_D2010: SearchPath is <%s>.',[SearchPath]);

      // Read DCC_DcuOutput
      DCUOutputPath := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_DcuOutput');
      trace(5,'ReadDPROJSettingsD2009_D2010: DCU Output Path is <%s>.',[DCUOutputPath]);

      // Read DCC_DcpOutput
      DCPOutputPath := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_DcpOutput');
      trace(5,'ReadDPROJSettingsD2009_D2010: DCP Output Path is <%s>.',[DCPOutputPath]);

      //Read DCC_BplOutput
      BPLOutputPath := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_BplOutput');
      trace(5,'ReadDPROJSettingsD2009_D2010: BPL Output Path is <%s>.',[BPLOutputPath]);

      //Read DCC_ExeOutput
      ProjectOutputPath := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_ExeOutput');
      trace(5,'ReadDPROJSettingsD2009_D2010: ProjectOutputPath is <%s>.',[ProjectOutputPath]);

      // Read some compiler switches
      // Local debug symbols
      _Tmp := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_LocalDebugSymbols');
      trace(5, 'ReadDPROJSettingsD2009_D2010: DCC_LocalDebugSymbols is <%s>.', [_Tmp]);
      if SameText(_Tmp, 'False') then CompilerSwitches := CompilerSwitches + '-$L- '; // No local debug symbols

      // Debug information
      _Tmp := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_DebugInformation');
      trace(5, 'ReadDPROJSettingsD2009_D2010: DCC_DebugInformation is <%s>.', [_Tmp]);
      if SameText(_Tmp, 'False') then CompilerSwitches := CompilerSwitches + '-$D- '; // No local debug symbols

      // Symbol reference info
      _Tmp := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_SymbolReferenceInfo');
      trace(5, 'ReadDPROJSettingsD2009_D2010: DCC_SymbolReferenceInfo is <%s>.', [_Tmp]);
      if SameText(_Tmp, '0') then CompilerSwitches := CompilerSwitches + '-$Y- '; // No local debug symbols

      // Optimize
      _Tmp := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_Optimize');
      trace(5, 'ReadDPROJSettingsD2009_D2010: DCC_Optimize is <%s>.', [_Tmp]);
      if SameText(_Tmp, 'False') then CompilerSwitches := CompilerSwitches + '-$O- '; // Do not No local debug symbols

      // Generate stack frames
      _Tmp := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_GenerateStackFrames');
      trace(5, 'ReadDPROJSettingsD2009_D2010: DCC_GenerateStackFrames is <%s>.', [_Tmp]);
      if SameText(_Tmp, 'True') then CompilerSwitches := CompilerSwitches + '-$W+ '; // Do not generate stack frames

      // Image base
      _Tmp := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_ImageBase');
      trace(5,'ReadDPROJSettingsD2009_D2010: DCC_ImageBase is <%s>.', [_Tmp]);
      if _Tmp <> '' then CompilerSwitches := CompilerSwitches + '-$K' + _TMP + ' ';

      Result:=true;
    end;
  finally
    _xmlDOMfile := nil;
    _InheritanceList.Free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadDPROJSettingsDXE_and_Newer
  Author:    muem/sam
  Date:      26-Oct-2012
  Arguments: const _dprojFilename:string      path + name of project file
             var Config:string               in: desired configuration ('' = use default configuration)
                                              out: found configuration  ('' = not found)
             var dPlatform:string             in: desired platform ('' = use default platform)
                                              out: found platform  ('' = not found)
             out CompilerSwitches: string;
             out Defines:string
             out SearchPath:string
             out ProjectOutputPath:string
             out BPLOutputPath:string
             out DCUOutputPath:string
             out DCPOutputPath:string
             out NameSpaces:string
  Result:    boolean
  Description: read path information from a dproj file used by Delphi XE or newer.
-----------------------------------------------------------------------------}
function ReadDPROJSettingsDXE_and_Newer(const _dprojFilename: string;
                                        var Config: string;
                                        var dPlatform: string;
                                        out CompilerSwitches: string;
                                        out Defines: string;
                                        out SearchPath: string;
                                        out ProjectOutputPath: string;
                                        out BPLOutputPath: string;
                                        out DCUOutputPath: string;
                                        out DCPOutputPath: string;
                                        out NameSpaces: string):boolean;
var
  _xmlDOMfile: IXMLDOMDocument;
  _msg:string;
  _desiredConfig: string;
  _desiredPlatform: string;
  _BuildConfiguration:string;
  _Platforms: TStringList;
  _Configs: TStringList;
  _InheritanceList: TStringList;
  _Tmp: string;
begin
  Result := false;
  _desiredConfig := Config;
  _desiredPlatform := dPlatform;
  Defines := '';
  SearchPath := '';
  ProjectOutputPath := '';
  BPLOutputPath := '';
  DCUOutputPath := '';
  DCPOutputPath := '';
  NameSpaces := '';
  CompilerSwitches := '';
  _InheritanceList := TStringList.Create;
  _xmlDOMfile := CoDOMDocument.Create;
  try
    if not _xmlDOMfile.load(_dprojFilename) then begin
      trace(5, 'ReadDPROJSettingsDXE_and_Newer: Could not find the file <%s>.', [_dprojFilename]);
      Exit;
    end;

    if Config = '' then begin  // read default configuration
      if not ReadNodeDocument(_xmlDOMfile, '/Project/PropertyGroup/Config', Config, _msg) then trace(3, 'Warning in ReadDPROJSettingsDXE_and_Newer: Could not find Config. <%s>.', [_msg]);
    end;
    trace(5, 'ReadDPROJSettingsDXE_and_Newer: Config is <%s>.', [Config]);

    if dPlatform = '' then begin // read default platform
      if not ReadNodeDocument(_xmlDOMfile, '/Project/PropertyGroup/Platform', dPlatform, _msg) then trace(3, 'Warning in ReadDPROJSettingsDXE_and_Newer: Could not find Platform. <%s>.', [_msg]);
    end;
    trace(5, 'ReadDPROJSettingsDXE_and_Newer: Platform is <%s>.', [dPlatform]);

    // check for support of configuration and platform
    _Platforms := TStringList.Create;
    _Configs := TStringList.Create;
    try
      ReadSupportedConfigsOfProject(_dprojFilename, _Configs);
      ReadSupportedPlatformsOfProject(_dprojFilename, _Platforms);
      if _Platforms.IndexOf(dPlatform) < 0 then begin
        // platform not supported
        dPlatform := '';
        trace(3, 'ReadDPROJSettingsDXE_and_Newer: Warning platform "%s" is not supported.', [_desiredPlatform]);
      end;
      if _Configs.IndexOf(Config) < 0 then begin
        // configuration not supported
        Config := '';
        trace(3, 'ReadDPROJSettingsDXE_and_Newer: Warning configuration "%s" is not supported.', [_desiredConfig]);
      end;
    finally
      _Configs.Free;
      _Platforms.Free;
    end;

    if (Config <> '') and (dPlatform <> '') then begin
      // read properties for desired configuration and platform
      if not ReadNodeDocument(_xmlDOMfile, '/Project/ItemGroup/BuildConfiguration[@Include = "' + Config + '"]/Key', _BuildConfiguration, _msg) then begin
        trace(3, 'Warning in ReadDPROJSettingsDXE_and_Newer: Could not find BuildConfiguration. <%s>', [_msg])
      end;

      // Inheritance list for attributes
      _InheritanceList.Clear;
      _InheritanceList.Add('Base');
      _InheritanceList.Add('Base_' + dPlatform);
      _InheritanceList.Add(_BuildConfiguration);
      _InheritanceList.Add(_BuildConfiguration+'_' + dPlatform);

      // Read DCC_Define
      Defines := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_Define');
      trace(5, 'ReadDPROJSettingsDXE_and_Newer: DCC_Define is <%s>.', [Defines]);

      // Read DCC_UnitSearchPath
      SearchPath := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_UnitSearchPath');
      trace(5, 'ReadDPROJSettingsDXE_and_Newer: SearchPath is <%s>.', [SearchPath]);

      // Read DCC_DcuOutput
      DCUOutputPath := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_DcuOutput');
      trace(5, 'ReadDPROJSettingsDXE_and_Newer: DCU Output Path is <%s>.', [DCUOutputPath]);

      // Read DCC_DcpOutput
      DCPOutputPath := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_DcpOutput');
      trace(5, 'ReadDPROJSettingsDXE_and_Newer: DCP Output Path is <%s>.', [DCPOutputPath]);

      // Read DCC_BplOutput
      BPLOutputPath := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_BplOutput');
      trace(5, 'ReadDPROJSettingsDXE_and_Newer: BPL Output Path is <%s>.', [BPLOutputPath]);

      // Read DCC_ExeOutput
      ProjectOutputPath := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_ExeOutput');
      trace(5, 'ReadDPROJSettingsDXE_and_Newer: ProjectOutputPath is <%s>.', [ProjectOutputPath]);

      // Read NameSpaces
      NameSpaces := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_Namespace');
      trace(5, 'ReadDPROJSettingsDXE_and_Newer: NameSpaces is <%s>.', [NameSpaces]);

      // Read some compiler switches
      // Local debug symbols
      _Tmp := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_LocalDebugSymbols');
      trace(5, 'ReadDPROJSettingsDXE_and_Newer: DCC_LocalDebugSymbols is <%s>.', [_Tmp]);
      if SameText(_Tmp, 'False') then CompilerSwitches := CompilerSwitches + '-$L- ';// No local debug symbols

      // Debug information
      _Tmp := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_DebugInformation');
      trace(5, 'ReadDPROJSettingsDXE_and_Newer: DCC_DebugInformation is <%s>.', [_Tmp]);
      if SameText(_Tmp, 'False') then CompilerSwitches := CompilerSwitches + '-$D- ';// No local debug symbols

      // Symbol reference info
      _Tmp := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_SymbolReferenceInfo');
      trace(5, 'ReadDPROJSettingsDXE_and_Newer: DCC_SymbolReferenceInfo is <%s>.', [_Tmp]);
      if SameText(_Tmp, '0') then CompilerSwitches := CompilerSwitches + '-$Y- '; // No local debug symbols

      // Optimize
      _Tmp := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_Optimize');
      trace(5, 'ReadDPROJSettingsDXE_and_Newer: DCC_Optimize is <%s>.', [_Tmp]);
      if SameText(_Tmp, 'False') then CompilerSwitches := CompilerSwitches + '-$O- ';  // Do not No local debug symbols

      // Generate stack frames
      _Tmp := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_GenerateStackFrames');
      trace(5, 'ReadDPROJSettingsDXE_and_Newer: DCC_GenerateStackFrames is <%s>.', [_Tmp]);
      if SameText(_Tmp, 'True') then CompilerSwitches := CompilerSwitches + '-$W+ ';  // Do not generate stack frames

      // Image base
      _Tmp := ReadInheritedAttribute(_xmlDOMfile, '//PropertyGroup[@Condition="''$(兌nheritance)''!=''''"]/你ttribute', _InheritanceList, 'DCC_ImageBase');
      trace(5,'ReadDPROJSettingsDXE_and_Newer: DCC_ImageBase is <%s>.', [_Tmp]);
      if _Tmp <> '' then CompilerSwitches := CompilerSwitches + '-$K' + _TMP + ' ';

      Result:=true;
    end;
  finally
    _xmlDOMfile := nil;
    _InheritanceList.Free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadDPROJSettings
  Author:    sam
  Date:      22-Mai-2008
  Arguments: const _dprojFilename: string      path + name of project file
             var _Config: string               in: desired configuration ('' = use default configuration)
                                               out: found configuration  ('' = not found)
             var _Platform: string             in: desired platform ('' = use default platform)
                                               out: found platform  ('' = not found)
             out CompilerSwitches: string;
             out Conditions: string
             var _SearchPath: string
             var _ProjectOutputPath: string
             var _BPLOutputPath: string
             var _DCUOutputPath: string
             var _NameSpaces: string
  Result:    boolean
  Description: read settings from a dproj-file.
-----------------------------------------------------------------------------}
function ReadDPROJSettings(const _dprojFilename: string;
                           var Config: string;
                           var dPlatform: string;
                           out CompilerSwitches: string;
                           out Conditions: string;
                           out SearchPath: string;
                           out ProjectOutputPath: string;
                           out BPLOutputPath: string;
                           out DCUOutputPath: string;
                           out DCPOutputPath: string;
                           out NameSpaces: string):boolean; // get informations from the dproj-file.
var
  _msg: string;
  _ProjectVersion: string;
begin
  Result := False;
  Conditions := '';
  SearchPath := '';
  NameSpaces := '';
  ProjectOutputPath := '';
  BPLOutputPath := '';
  DCUOutputPath := '';
  DCPOutputPath := '';
  _ProjectVersion := '';
  if not fileExists(_dprojFilename) then begin
    trace(5, 'ReadDPROJSettings: Could not find the file <%s>.', [_dprojFilename]);
    exit;
  end;
  if not ReadNodeText(_dprojFilename, '//PropertyGroup/ProjectVersion', _ProjectVersion, _msg) then
    trace(3, 'Warning in ReadDPROJSettings: Could not read condition. <%s>.', [_msg]);
  if _ProjectVersion = '' then  begin
    Result := ReadDPROJSettingsD2005_D2007(_dprojFilename,
                                           Config,
                                           Conditions,
                                           SearchPath,
                                           ProjectOutputPath,
                                           BPLOutputPath,
                                           DCUOutputPath,
                                           DCPOutputPath);
    dPlatform := sWin32;
    CompilerSwitches := '';
  end
  else if _ProjectVersion = '12.0' then begin
    Result := ReadDPROJSettingsD2009_D2010(_dprojFilename,
                                           Config,
                                           CompilerSwitches,
                                           Conditions,
                                           SearchPath,
                                           ProjectOutputPath,
                                           BPLOutputPath,
                                           DCUOutputPath,
                                           DCPOutputPath);
    dPlatform := sWin32;
  end
  else begin
    Result := ReadDPROJSettingsDXE_and_Newer(_dprojFilename,
                                             Config,
                                             dPlatform,
                                             CompilerSwitches,
                                             Conditions,
                                             SearchPath,
                                             ProjectOutputPath,
                                             BPLOutputPath,
                                             DCUOutputPath,
                                             DCPOutputPath,
                                             NameSpaces);
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadSupportedConfigsOfProject
  Author:    muem
  Date:      24-Oct-2012
  Arguments: const _filename:string;var _Configs: TStringList
  Result:    boolean
  Description: Gets all configs supported by the project
-----------------------------------------------------------------------------}
function  ReadSupportedConfigsOfProject(const _filename: string; var _Configs: TStringList): Boolean;
var
  _msg: string;
  _fileext: string;
begin
  Result := False;
  _Configs.Clear;
  _fileext:=LowerCase(ExtractFileExt(_filename));
  trace(5,'ReadSupportedConfigsOfProject: filename <%s>.',[_filename]);
  if (_fileext='.dpk') or
     (_fileext='.dpr') then begin
    _Configs.Add(sDebug);
    _Configs.Add(sRelease);
  end;
  if _fileext='.dproj' then result := ReadNodesText(_filename,'//ItemGroup/BuildConfiguration[@Include != "Base"]/@Include',_Configs, _msg);   // get supported configs
  trace(5,'ReadSupportedConfigsOfProject: configs <%s>.',[_Configs.CommaText]);
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadSupportedPlatformsOfProject
  Author:    muem
  Date:      24-Oct-2012
  Arguments: const _filename:string;var _Platforms: TStringList
  Result:    boolean
  Description: Gets all platforms supported by the project
-----------------------------------------------------------------------------}
function  ReadSupportedPlatformsOfProject(const _filename: string; var _Platforms: TStringList): Boolean;
var
  _msg: string;
  _fileext: string;
begin
  Result := False;
  _Platforms.Clear;
  _fileext:=LowerCase(ExtractFileExt(_filename));
  trace(5,'ReadSupportedPlatformsOfProject: filename <%s>.',[_filename]);
  if (_fileext='.dpk') or
     (_fileext='.dpr') then begin
    _Platforms.Add(sWin32);
  end;
  if _fileext='.dproj' then result := ReadNodesText(_filename,'//Platforms/Platform[. = "True"]/@value',_Platforms, _msg);  // get supported platforms
  trace(5,'ReadSupportedPlatformsOfProject: platforms <%s>.',[_Platforms.CommaText]);
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadAllPlatformsOfProject
  Author:    muem
  Date:      24-Oct-2012
  Arguments: const _filename:string;var _Platforms:TStringList
  Result:    boolean
  Description: Gets all platforms of the project
-----------------------------------------------------------------------------}
function  ReadAllPlatformsOfProject(const _filename: string; var _Platforms: TStringList): Boolean;
var
  _msg:string;
  _fileext:string;
begin
  result := false;
  _Platforms.Clear;
  _fileext:=lowercase(ExtractFileExt(_filename));
  trace(5,'ReadAllPlatformsOfProject: filename <%s>.',[_filename]);
  if _fileext='.dproj' then begin
    // get all platforms
    result := ReadNodesText(_filename,'//Platforms/Platform/@value',_Platforms, _msg);

  end;
  trace(5,'ReadAllPlatformsOfProject: platforms <%s>.',[_Platforms.CommaText]);
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadConfigurationSettings
  Author:    sam
  Date:      02-Jun-2008
  Arguments: const _filename: string        path + name of project file
             out Config: string            in: desired configuration ('' = use default configuration)
                                           out: found configuration  ('' = not found)
             out dPlatform: string          in: desired platform ('' = use default platform, 'Win32', 'Win64')
                                           out: found platform  ('' = not found)
             out CompilerSwitches: string
             out Conditions: string
             out SearchPath: string
             out ProjectOutputPath: string
             out BPLOutputPath: string
             out DCUOutputPath: string
             out DCPOutputPath: string
             out NameSpaces: string
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function ReadConfigurationSettings(const _filename: string;
                                   out Config: string;
                                   out dPlatform: string;
                                   out CompilerSwitches: string;
                                   out Conditions: string;
                                   out SearchPath: string;
                                   out ProjectOutputPath: string;
                                   out BPLOutputPath: string;
                                   out DCUOutputPath: string;
                                   out DCPOutputPath: string;
                                   out NameSpaces: string): Boolean;
var
_fileext: string;
_ProjectName: string;
begin
  result := False;
  NameSpaces:='';
  _fileext := LowerCase(ExtractFileExt(_filename));
  trace(5, 'ReadConfigurationSettings: filename <%s>.', [_filename]);
  if _fileext = '.cfg' then begin
    result := ReadCFGSettings(_filename, Conditions, SearchPath, ProjectOutputPath, BPLOutputPath, DCUOutputPath);
    dPlatform := sWin32;
    if Config = '' then Config := sNoConfig;
    CompilerSwitches := '';
    DCPOutputPath := BPLOutputPath;
  end
  else if _fileext = '.dproj' then begin
    result := ReadDProjSettings(_filename, Config, dPlatform, CompilerSwitches, Conditions, SearchPath, ProjectOutputPath, BPLOutputPath, DCUOutputPath, DCPOutputPath, NameSpaces);
    if DCPOutputPath = '' then DCPOutputPath:=BPLOutputPath;
  end
  else if _fileext = '.bdsproj' then begin
    result := ReadBDSProjSettings(_filename, _ProjectName ,Conditions, SearchPath, ProjectOutputPath, BPLOutputPath, DCUOutputPath);
    dPlatform := sWin32;
    if Config = '' then Config := sNoConfig;
    CompilerSwitches := '';
    DCPOutputPath := BPLOutputPath;
  end;
  if Conditions <> '' then Conditions := '-D'+Conditions;
end;

{-----------------------------------------------------------------------------
  Procedure: ReadProjectFilenameFromDProj
  Author:    sam
  Date:      29-Mai-2008
  Arguments: const _Filename:String
  Result:    string
  Description: the real project filename is hidden in the dproj-file.
-----------------------------------------------------------------------------}
function ReadProjectFilenameFromDProj(const _Filename:String):string;
var
_msg:string;
_stmt:string;
begin
  Result:='';
  if (lowercase(ExtractFileExt(_Filename))='.dpk') or
     (lowercase(ExtractFileExt(_Filename))='.dpr')  then begin  // if already a dpr or dpk file is passed to this method, then return the same name.
    result:=_Filename;
    exit;
  end;
  if not fileExists(_Filename) then begin   // if it is an dproj file then read the real filename from the xml.
    trace(5,'ReadProjectFilenameFromDProj: Could not find the file <%s>.',[_Filename]);
    exit;
  end;
  _stmt:='//PropertyGroup/MainSource';
  ReadNodeText(_Filename,_stmt,result,_msg);
  if result='' then begin
    _stmt:='//Delphi.Personality/Source/Source[@Name="MainSource"]';
    ReadNodeText(_Filename,_stmt,result,_msg);
  end;
  if result<>'' then result:=extractfilepath(_Filename)+result;
  trace(5,'ReadProjectFilenameFromDProj: Read project name <%s> from file <%s>.',[result,_Filename]);
end;

{-----------------------------------------------------------------------------
  Procedure: AddTag
  Author:    sam
  Date:      15-Jun-2007
  Arguments: _filename: string;_DelphiVersion:integer
  Result:    string
  Description: try to replace "C:\Program Files\Borland\Delphi 7\" with $(DELPHI)
               try to replace "C:\Program Files\" with $(PROGRAMFILES)"
               try to replace "C:\Program Files\Borland\BDS\3.0" with $(BDS).
-----------------------------------------------------------------------------}
function AddTag(_filename: string;_DelphiVersion:integer): string;
var
_temp:string;
begin
  _filename:=lowercase(_filename);
  result:=_filename;
  _temp:=lowercase(GetDelphiRootDir(_DelphiVersion));
  if Pos(_temp,_filename)=1 then begin
    delete(_filename,1,length(_temp));
    result:=GetDelphiPathTag(_DelphiVersion)+'\'+_filename;
    exit;
  end;
  _temp:=lowercase(GetSystemPath(spProgFiles));
  if Pos(_temp,_filename)=1 then begin
    delete(_filename,1,length(_temp));
    result:=cProgramFilesTag+'\'+_filename;
    exit;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: WriteCFGSettings
  Author:    sam
  Date:      07-Mrz-2007
  Arguments: _cfgFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath:string
  Result:    boolean
  Description: write the path-settings from the delphipackagetool into the .cfg-file.
  -returns true a new file has been prepared.
  - the out-param <NewFilename> contains filename of the new prepared file.
-----------------------------------------------------------------------------}
function  WriteCFGSettings(const _bpgPath:string;
                                 _cfgFilename:String;
                                 _Conditions:string;
                                 _SearchPath:String;
                                 _ProjectOutputPath:string;
                                 _BPLOutputPath,
                                 _DCUOutputPath:string;
                                 const _silent:boolean;
                                 const _ProjectType:TProjectType;
                                 const _DelphiVersion:integer;
                                 out NewFilename:string):boolean;
var
_CFGFile:TStrings;
_FileChanged:boolean;

  function GetField(_BeginMark,_EndMark:string):string;
  var
  _Text:String;
  _begin,_End:Integer;
  begin
    result:='';
    _Text:=_CFGFile.Text;
    _begin:=Pos(_BeginMark,_Text);
    if _begin=0 then exit;
    _end:=PosEx(_EndMark,_Text,_begin+length(_BeginMark));
    result:=copy(_text,_begin+length(_BeginMark),_end-_begin-length(_BeginMark));
  end;

  function ReplaceItem(const _OptionMark:string;const _value:string):boolean;
  var
  _OldValue:string;
  _text:string;
  begin
    result:=false;
    _Text:=_CFGFile.Text;
    _OldValue:=GetField(_OptionMark,'"');
    if _OldValue=_value then exit;
    if _OldValue<>'' then begin
      _text:=StringReplace(_text,_OptionMark+_OldValue,_OptionMark+_value,[rfIgnoreCase]);
      _CFGFile.Text:=_text;
    end
    else _CFGFile.Add(_OptionMark+_value+'"');
    trace(5,'WriteCFGSettings: Set Option <%s> to <%s>.',[_OptionMark,_value]);
    result:=true;
  end;

  function DeleteItem(const _OptionMark:string):boolean;
  var
  i:integer;
  begin
    result:=false;
    for i:=0 to _CFGFile.Count-1 do begin
      if pos(_OptionMark,_CFGFile[i])=0 then continue;
      _CFGFile.Delete(i);
      result:=true;
      exit;
    end;
  end;

begin
  result:=false;
  NewFilename:='';
  _FileChanged:=false;
  if not fileExists(_cfgFilename) then begin
    trace(2,'Problem in WriteCFGSettings: Could not find the file <%s>.',[_cfgFilename]);
    exit;
  end;
// make all paths relative
  _searchPath       :=RelativePaths(ExtractFilePath(_cfgFilename),_searchPath,_DelphiVersion,'$PLATFOPRM is not support','$CONFIG is not support');

  _ProjectOutputPath:=AddTag(_ProjectOutputPath,_DelphiVersion);
  _BPLOutputPath    :=AddTag(_BPLOutputPath,_DelphiVersion);
  _DCUOutputPath    :=AddTag(_DCUOutputPath,_DelphiVersion);

  _ProjectOutputPath:=RelativePath(ExtractFilePath(_cfgFilename),_ProjectOutputPath,_DelphiVersion,false);
  _BPLOutputPath    :=RelativePath(ExtractFilePath(_cfgFilename),_BPLOutputPath,_DelphiVersion,false);
  _DCUOutputPath    :=RelativePath(ExtractFilePath(_cfgFilename),_DCUOutputPath,_DelphiVersion,false);

// then write them into the cfg-file.
  _CFGFile:=TStringList.Create;
  try
    _CFGFile.LoadFromFile(_cfgFilename);
    if ReplaceItem('-U"',_SearchPath) then _FileChanged:=true;
    if ReplaceItem('-O"',_SearchPath) then _FileChanged:=true;
    if ReplaceItem('-I"',_SearchPath) then _FileChanged:=true;
    if ReplaceItem('-R"',_SearchPath) then _FileChanged:=true;
    if ReplaceItem('-N"',_DCUOutputPath) then _FileChanged:=true;
    case _ProjectType of
      tp_dll,tp_exe: begin
                       if ReplaceItem('-E"',_ProjectOutputPath) then _FileChanged:=true;
                       if DeleteItem('-LE"') then _FileChanged:=true;
                       if DeleteItem('-LN"') then _FileChanged:=true;
                     end;
      tp_bpl:        begin
                       if ReplaceItem('-LE"',_BPLOutputPath) then _FileChanged:=true;
                       if ReplaceItem('-LN"',_BPLOutputPath) then _FileChanged:=true;
                       if DeleteItem('-E"') then _FileChanged:=true;
                     end;
    end;
    if not _FileChanged then exit;  // if nothing has changed, then leave here.
    if not BackupFile(_cfgFilename,'.cfg_old','',false) then exit;
    _cfgFilename:=changefileext(_cfgFilename,'.cfg_new');
    try
      _CFGFile.SaveToFile(_cfgFilename);
      trace(5,'Saved changes to file <%s>.',[_cfgFilename]);
      NewFilename:=_cfgFilename;
      result:=true;
    except
      on e:exception do trace(1,'Error in WriteCFGSettings: Could not save file <%s>. Check User-Rights. <%s>.',[_cfgFilename,e.message]);
    end;
  finally
    _CFGFile.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: WriteBDSProjSettings
  Author:    sam
  Date:      08-Jul-2008
  Arguments: const _bpgPath,_bdsprojFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath:string;const _silent:boolean;const _DelphiVersion:integer
  Result:    boolean
  Description: write the path-settings from the delphipackagetool into the .bdsproj-file.
-----------------------------------------------------------------------------}
function  WriteBDSProjSettings(const _bpgPath:string;
                               _bdsprojFilename:String;
                               _Conditions:string;
                               _SearchPath:String;
                               _ProjectOutputPath:string;
                               _BPLOutputPath,
                               _DCUOutputPath:string;const _silent:boolean;
                               const _DelphiVersion:integer;
                               out NewFilename:string):boolean;
var
_BDSProjFile:TStrings;
_index:integer;
_FileChanged:boolean;
_temp:string;
_pos:integer;
  function FindText(const _s:string;var position:integer):integer;
  var
  i:integer;
  begin
    result:=-1;
    position:=0;
    for i:=0 to _BDSProjFile.Count-1 do begin
      position:=pos(_s,_BDSProjFile[i]);
      if position>0 then begin
        result:=i;
        exit;
      end;
    end;
  end;

begin
  result:=false;
  NewFilename:='';
  _FileChanged:=false;
  if not fileExists(_bdsprojFilename) then exit;

// make all paths relative
  _searchPath       :=RelativePaths(ExtractFilePath(_bdsprojFilename),_searchPath,_DelphiVersion,'$PLATFOPRM is not support','$CONFIG is not support');
  _ProjectOutputPath:=RelativePath(ExtractFilePath(_bdsprojFilename),_ProjectOutputPath,_DelphiVersion,false);
  _BPLOutputPath    :=RelativePath(ExtractFilePath(_bdsprojFilename),_BPLOutputPath,_DelphiVersion,false);
  _DCUOutputPath    :=RelativePath(ExtractFilePath(_bdsprojFilename),_DCUOutputPath,_DelphiVersion,false);

// then write them into the bdsproj-file.
  _BDSProjFile:=TStringList.Create;
  try
    _BDSProjFile.LoadFromFile(_bdsprojFilename);
    _index:=FindText('<Directories Name="SearchPath">',_pos);
    if _index>-1 then begin
      _temp:=copy(_BDSProjFile[_index],1,_pos-1);
      if _BDSProjFile[_index]<>_temp+'<Directories Name="SearchPath">'+_searchPath+'</Directories>' then begin
        _BDSProjFile[_index]:=_temp+'<Directories Name="SearchPath">'+_searchPath+'</Directories>';
        trace(4,'WriteBDSProjSettings: Write search path <%s> to file <%s>.',[_searchPath,_bdsprojFilename]);
        _FileChanged:=true;
      end;
    end;

    _index:=FindText('<Directories Name="OutputDir">',_pos);
    if _index>-1 then begin
      _temp:=copy(_BDSProjFile[_index],1,_pos-1);
      if _BDSProjFile[_index]<>_temp+'<Directories Name="OutputDir">'+_ProjectOutputPath+'</Directories>' then begin
        _BDSProjFile[_index]:=_temp+'<Directories Name="OutputDir">'+_ProjectOutputPath+'</Directories>';
        trace(4,'WriteBDSProjSettings: Write output path <%s> to file <%s>.',[_ProjectOutputPath,_bdsprojFilename]);
        _FileChanged:=true;
      end;
    end;

    _index:=FindText('<Directories Name="PackageDLLOutputDir">',_pos);
    if _index>-1 then begin
      _temp:=copy(_BDSProjFile[_index],1,_pos-1);
      if _BDSProjFile[_index]<>_temp+'<Directories Name="PackageDLLOutputDir">'+_ProjectOutputPath+'</Directories>' then begin
        _BDSProjFile[_index]:=_temp+'<Directories Name="PackageDLLOutputDir">'+_ProjectOutputPath+'</Directories>';
        trace(4,'WriteBDSProjSettings: Write dll output path <%s> to file <%s>.',[_ProjectOutputPath,_bdsprojFilename]);
        _FileChanged:=true;
      end;
    end;

    _index:=FindText('<Directories Name="UnitOutputDir">',_pos);
    if _index>-1 then begin
      _temp:=copy(_BDSProjFile[_index],1,_pos-1);
      if _BDSProjFile[_index]<>_temp+'<Directories Name="UnitOutputDir">'+_DCUOutputPath+'</Directories>' then begin
        _BDSProjFile[_index]:=_temp+'<Directories Name="UnitOutputDir">'+_DCUOutputPath+'</Directories>';
        trace(4,'WriteBDSProjSettings: Write unit output path <%s> to file <%s>.',[_DCUOutputPath,_bdsprojFilename]);
        _FileChanged:=true;
      end;
    end;
    _index:=FindText('<Directories Name="PackageDCPOutputDir">',_pos);
    if _index>-1 then begin
      _temp:=copy(_BDSProjFile[_index],1,_pos-1);
      if _BDSProjFile[_index]<>_temp+'<Directories Name="PackageDCPOutputDir">'+_BPLOutputPath+'</Directories>' then begin
        _BDSProjFile[_index]:=_temp+'<Directories Name="PackageDCPOutputDir">'+_BPLOutputPath+'</Directories>';
        trace(4,'WriteBDSProjSettings: Write package output path <%s> to file <%s>.',[_BPLOutputPath,_bdsprojFilename]);
        _FileChanged:=true;
      end;
    end;
    if not _FileChanged then exit;
    if not RemoveReadOnlyFlag(_bdsprojFilename,_silent) then exit;
    if not BackupFile(_bdsprojFilename,'.bdsproj_old','',false) then exit;
    _bdsprojFilename:=changefileext(_bdsprojFilename,'.bdsproj_new');
    try
      _BDSProjFile.SaveToFile(_bdsprojFilename);
      trace(3,'WriteBDSProjSettings: Write changes to file <%s>.',[_bdsprojFilename]);
      NewFilename:=_bdsprojFilename;
      result:=true;
    except
      on e:exception do trace(1,'Error in WriteBDSProjSettings: Could not save file <%s>. Check User-Rights. <%s>.',[_bdsprojFilename,e.message]);
    end;
  finally
    _BDSProjFile.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: WriteDProjSettings
  Author:    sam
  Date:      08-Jul-2008
  Arguments: const _bpgPath,_bdsprojFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath,_DCPOutputPath:string;const _silent:boolean;const _DelphiVersion:integer
  Result:    boolean
  Description: write the path-settings from the delphipackagetool into the .dproj-file.
-----------------------------------------------------------------------------}
function  WriteDProjSettings(const _bpgPath:string;
                                   _dprojFilename:String;
                                   _Conditions:string;
                                   _SearchPath:String;
                                   _ProjectOutputPath:string;
                                   _BPLOutputPath,
                                   _DCUOutputPath,
                                   _DCPOutputPath:string;
                                   const _silent:boolean;
                                   const _DelphiVersion:integer;
                                   const _CurrentPlatform,
                                   _CurrentConfig: string;
                                   out NewFilename:string):boolean;
const
cTagConfig1='<PropertyGroup Condition="''$(Cfg_1)';
cTagConfig2='<PropertyGroup Condition="''$(Cfg_2)';
var
_DProjFile:TStrings;
_FileChanged:boolean;
_IndexConfig1:integer;
_posConfig1:integer;
_IndexConfig2:integer;
_posConfig2:integer;
  function FindText(const _s:string;var position:integer):integer;
  var
  i:integer;
  begin
    result:=-1;
    position:=0;
    for i:=0 to _DProjFile.Count-1 do begin
      position:=pos(_s,_DProjFile[i]);
      if position<=0 then continue;
      result:=i;
      exit;
    end;
  end;

  procedure ChangeSetting(_tag:string;_value:string);
  var
  i:integer;
  _index:integer;
  _temp:string;
  _closeTag:string;
  _pos:integer;
  begin
    _index:=FindText(_tag,_pos);
    _closeTag:=_tag;
    delete(_CloseTag,1,1);
    _CloseTag:='</'+_CloseTag;
    if _index>-1 then begin // if tag exists, then modify it
      _temp:=copy(_DProjFile[_index],1,_pos-1);
      if _DProjFile[_index]<>_temp+_tag+_value+_CloseTag then begin
        _DProjFile[_index]:=_temp +_tag+_value+_CloseTag;
        trace(4,'WriteDProjSettings: Changed Setting <%s> to <%s> to file <%s>.',[_tag,_value,_dprojFilename]);
        _FileChanged:=true;
      end;
    end
    else begin // if it does not exist, then insert it
      if _searchPath<>'' then begin
        _temp:='';
        for i:=1 to _posConfig1 do _temp:=_temp+#9;
        _DProjFile.insert(_IndexConfig1+2,_temp+_tag+_value+_CloseTag);
        trace(4,'WriteDProjSettings: Changed Setting <%s> to <%s> to file <%s>.',[_tag,_value,_dprojFilename]);
        _FileChanged:=true;
      end;
    end;
  end;

begin
  result:=false;
  NewFilename:='';
  _FileChanged:=false;
  if not fileExists(_DProjFilename) then begin
    trace(2,'Problem in WriteDProjSettings: Could not find the file <%s>.',[_dprojFilename]);
    exit;
  end;

// make all paths relative
  _searchPath       :=RelativePaths(ExtractFilePath(_dprojFilename),_searchPath,_DelphiVersion,_CurrentPlatform,_CurrentConfig);
  _ProjectOutputPath:=RelativePath(ExtractFilePath(_dprojFilename),_ProjectOutputPath,_DelphiVersion,false);
  _BPLOutputPath    :=RelativePath(ExtractFilePath(_dprojFilename),_BPLOutputPath,_DelphiVersion,false);
  _DCUOutputPath    :=RelativePath(ExtractFilePath(_dprojFilename),_DCUOutputPath,_DelphiVersion,false);
  _DCPOutputPath    :=RelativePath(ExtractFilePath(_dprojFilename),_DCPOutputPath,_DelphiVersion,false);
  _ProjectOutputPath:=RelativePath(ExtractFilePath(_dprojFilename),_ProjectOutputPath,_DelphiVersion,false);

// then write them into the dproj-file.
  _DProjFile:=TStringList.Create;
  try
    _DProjFile.LoadFromFile(_dprojFilename);

    _IndexConfig1:=FindText(cTagConfig1,_posConfig1);
    _IndexConfig2:=FindText(cTagConfig2,_posConfig2);
    if (_IndexConfig1=-1) and (_IndexConfig2=-1) then exit; // if the tag was not found, then leave.
    ChangeSetting('<DCC_UnitSearchPath>',_searchPath);
    ChangeSetting('<DCC_ResourcePath>',_ProjectOutputPath);
    ChangeSetting('<DCC_ObjPath>',_DCUOutputPath);
    ChangeSetting('<DCC_ExeOutput>',_ProjectOutputPath);
    ChangeSetting('<DCC_BplOutput>',_BPLOutputPath);
    ChangeSetting('<DCC_DcuOutput>',_DCUOutputPath);
    ChangeSetting('<DCC_DcpOutput>',_DCPOutputPath);
    ChangeSetting('<DCC_ObjOutput>',_DCUOutputPath);
    ChangeSetting('<DCC_HppOutput>',_DCUOutputPath);
    if not _FileChanged then exit;
    if not BackupFile(_dprojFilename,'.dproj_old','',false) then exit;
    _dprojFilename:=changefileext(_dprojFilename,'.dproj_new');
    try
      _DProjFile.SaveToFile(_dprojFilename);
      trace(3,'WriteDProjSettings: Write changes to file <%s>.',[_dprojFilename]);
      NewFilename:=_dprojFilename;
      result:=true;
    except
      on e:exception do trace(1,'Error in WriteDProjSettings: Could not save file <%s>. Check User-Rights. <%s>.',[_dprojFilename,e.message]);
    end;
  finally
    _DProjFile.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: WriteSettingsToDelphi
  Author:    sam
  Date:      08-Jul-2008
  Arguments: const _bpgPath,_cfgFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath,_DCPOutputPath:string;const _silent:boolean;const _DelphiVersion:integer
  Result:    boolean
  Description: write path settings to cfg,bdsproj,dproj,.dof file.
-----------------------------------------------------------------------------}
function WriteSettingsToDelphi(_bpgPath,_Filename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath,_DCPOutputPath:string;const _silent:boolean;const _ProjectType:TProjectType;const _DelphiVersion:integer; const _CurrentPlatform, _CurrentConfig: string;out ChangedFiles:TStringList):boolean;
var
_NewFilename:string;
begin
  result:=false;
  ChangedFiles:=TStringList.create;
  if WriteCFGSettings(_bpgPath,changefileext(_Filename,'.cfg'),_Conditions,_SearchPath,_ProjectOutputPath,_BPLOutputPath,_DCUOutputPath,_silent,_ProjectType,_DelphiVersion,_NewFilename) then ChangedFiles.add(_NewFilename);
  if WriteDOFFile(_bpgPath,changefileext(_Filename,'.dof'),_Conditions,_SearchPath,_ProjectOutputPath,_BPLOutputPath,_DCUOutputPath,_silent,_DelphiVersion,_NewFilename) then ChangedFiles.add(_NewFilename);
  if _DelphiVersion>8 then begin
    if WriteBDSProjSettings(_bpgPath,changefileext(_Filename,'.bdsproj'),_Conditions,_SearchPath,_ProjectOutputPath,_BPLOutputPath,_DCUOutputPath,_silent,_DelphiVersion,_NewFilename) then ChangedFiles.add(_NewFilename);
    if WriteDProjSettings(_bpgPath,changefileext(_Filename,'.dproj'),_Conditions,_SearchPath,_ProjectOutputPath,_BPLOutputPath,_DCUOutputPath,_DCPOutputPath,_silent,_DelphiVersion,_CurrentPlatform, _CurrentConfig,_NewFilename) then ChangedFiles.add(_NewFilename);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: AddPackageToRegistry
  Author:    sam
  Date:      30-Aug-2002
  Arguments: _RootKey:HKey;_Key,_PackageName,_PackageDescription:string
  Result:    None
  Description:
               19.11.2002 - added error message in case the user has no administrator rights
-----------------------------------------------------------------------------}
function AddPackageToRegistry(_RootKey:HKey;_Key,_PackageName,_PackageDescription:string):boolean; // delete a package entry from the registery
var
  _Reg: TRegistry;
begin
  Result:=false;
  _PackageName:=lowercase(_PackageName);
  _Reg := TRegistry.Create(KEY_READ or KEY_WRITE or KEY_WOW64_32KEY);
  try
    _Reg.RootKey := _RootKey;
    if not _Reg.OpenKey(_Key,false) then begin
      trace(1,'AddPackageToRegistry: The Key <%s,%s> was not found in the registry or no access rights.',[HKEYToStr(_RootKey),_Key]);
      exit;
    end;
    if _Reg.ValueExists(_PackageName) then begin
      trace(5,'AddPackageToRegistry: The Packages <%s> is already registered.',[_PackageName]);
      result:=true;
      exit;
    end;
    try
      _Reg.WriteString(_PackageName,_PackageDescription);
      trace(3,'Successfully installed the  Packages <%s> for <%s,%s>.',[_PackageName,HKEYToStr(_RootKey),_Key]);
      Result:=true;
    except
      on e:exception do trace(1,'Warning in AddPackageToRegistry: Could not add the package <%s> for <%s,%s> to the registry.You need to have Admin rights for this computer. <%s>.',[_PackageName,HKEYToStr(_RootKey),_key,e.message]);
    end;
  finally
    _Reg.CloseKey;
    _Reg.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: CanInstallPackage
  Author:    sam
  Date:      22-Aug-2002
  Arguments: _PackageName:string
  Result:    boolean
  Description: runtime only packages can not be installed in the delphi IDE.
-----------------------------------------------------------------------------}
function CanInstallPackage(const _PackageName:string):boolean; // checks if it is a designtime package or not
var
  _DPKFile:TStrings;
begin
  Result:=False;
  if not fileExists(_PackageName) then begin
    trace(5,'CanInstallPackage: Could not find the file <%s>.',[_PackageName]);
    exit;
  end;
  _DPKFile:=TStringList.Create;
  try
    _DPKFile.LoadFromFile(_PackageName);
    result:=(Pos('$RUNONLY',_DPKFile.Text)=0);
  finally
    _DPKFile.free;
  end;
end;


{-----------------------------------------------------------------------------
  Procedure: InstallPackage
  Author:    sam
  Date:      30-Aug-2002
  Arguments: _PackageName,_PackageDirectory:String;_DelphiVersion:Integer
  Result:    None
  Description: install the delphi package <_PackageName> (.dpk) which is located in the
               directory <_PackageDirectory> for delphi version <_DelphiVersion>.
               The parameter <_PackageDirectory> points to the location of the <.bpl> file.
-----------------------------------------------------------------------------}
function InstallPackage(_PackageName, _PackageDirectory, _PackageDescription, _PackageLibSuffix: string; _DelphiVersion: Integer; out msg: string): Boolean;
var
  _PackageKey: string;
  _RegFile: TStringList;
  _RegFileName: string;
  _BplFilename: string;
begin
  Result := False;
  msg := 'Failed';
  _PackageName := ReadProjectFilenameFromDProj(_PackageName);
  if _PackageDescription = '' then _PackageDescription := '(untitled)';
  if LowerCase(ExtractFileExt(_PackageName)) <> '.dpk' then begin
    msg := '-';
    Exit;
  end;
  if not GetIDERootKey(_DelphiVersion, _PackageKey) then begin
    trace(3, 'Problem in InstallPackage: Could not find key for Delphi Version <%d>.', [_DelphiVersion]);
    Exit;
  end;

  if not CanInstallPackage(_PackageName) then begin
    trace(5, 'InstallPackage: The package <%s> is a runtime only package. It will not be installed into the IDE.', [_PackageName]);
    msg := '-';
    Exit;
  end;
  if _PackageLibSuffix <> '' then _BplFilename := LowerCase(_PackageDirectory + ExtractFilenameOnly(_PackageName) + _PackageLibSuffix+'.bpl')
                             else _BplFilename := LowerCase(_PackageDirectory + ExtractFilenameOnly(_PackageName) + '.bpl');

  if not fileexists(_BplFilename) then begin
    trace(5, 'InstallPackage: The file <%s> does not exist. DPT will not add it to the registry.', [_PackageName]);
    Exit;
  end;

  _RegFile := TStringList.Create;
  try
    _RegFile.Add('Windows Registry Editor Version 5.00');
    _RegFile.Add('');
    _PackageKey := _PackageKey + 'Known Packages';
    if AddPackageToRegistry(HKEY_CURRENT_USER, _PackageKey, _BplFilename, _PackageDescription) then begin
      trace(2, 'Installed the package <%s> into the Delphi IDE for the current user.', [ExtractFileName(_PackageName)]);
      _RegFile.Add('[HKEY_CURRENT_USER\' + _PackageKey+']');
      _RegFile.Add('"' + PrepapreRegistryPath(_BplFilename) + '"="' + _PackageDescription + '"');
      msg := 'Current User';
      Result := True;
    end;

    if AddPackageToRegistry(HKEY_LOCAL_MACHINE,_PackageKey,_BplFilename,_PackageDescription) then begin
      trace(2,'Installed the package <%s> into the Delphi IDE for the all users on this computer.',[ExtractFileName(_PackageName)]);
      _RegFile.Add('[HKEY_LOCAL_MACHINE\'+_PackageKey+']');
      _RegFile.Add('"'+PrepapreRegistryPath(_BplFilename)+'"="'+_PackageDescription+'"');
      if msg = 'Current User' then msg := msg + '/All Users'
                              else msg := 'All Users';
      Result := True;
    end;

    if _RegFile.Count > 0 then begin  // create a .reg file if needed.
      try
        _RegFile.Add('');
        _RegFileName := 'Register_Package_' + ChangeFileExt(ExtractFilename(_PackageName), '.reg');
        if gCreateBatchFile then begin
          CheckDirectory(extractfilepath(FBatchFilename));
          _RegFile.SaveToFile(ExtractFilePath(FBatchFilename) + _RegFileName);
        end;
        FBatchFile.Add(_RegFileName);
      except
        On E:Exception do Showmessage(format('Could not save file <%s> because <%s>.', [_RegFileName, E.Message]));
      end;
    end;
  finally
    _RegFile.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: UninstallPackage
  Author:    sam
  Date:      30-Aug-2002
  Arguments: _PackageName,_PackageDirectory:String;_DelphiVersion:Integer
  Result:    None
  Description: uninstall the package <_PackageName>.
               This deletes the registry keys.
-----------------------------------------------------------------------------}
function UnInstallPackage(_PackageName, _PackageDirectory, _PackageLibSuffix: string; _DelphiVersion: Integer): Boolean;
var
  _PackageBaseKey: string;
  _PackageKey: string;
  _PackageValue: string;
  _RegFile: TStrings;
  _RegFileName: string;
  _RegFilePackagePath: string;
  _BplFilename: string;
begin
  Result := False;
  _PackageName := ReadProjectFilenameFromDProj(_PackageName);
  if LowerCase(ExtractFileExt(_PackageName)) <> '.dpk' then Exit;

  if not GetIDERootKey(_DelphiVersion, _PackageBaseKey) then begin
    trace(3,'Problem in UnInstallPackage: Could not find key for Delphi Version <%d>.', [_DelphiVersion]);
    Exit;
  end;

  _RegFilePackagePath := PrepapreRegistryPath(_PackageDirectory);
  if _PackageLibSuffix <> '' then _BplFilename := LowerCase(ExtractFilenameOnly(_PackageName) + _PackageLibSuffix + '.bpl')
                             else _BplFilename := LowerCase(ExtractFilenameOnly(_PackageName) + '.bpl');

  _RegFile := TStringList.create;
  try
    _RegFile.Add('Windows Registry Editor Version 5.00');
    _RegFile.Add('');
    _PackageKey := _PackageBaseKey + 'Known Packages';
    _RegFile.Add('[HKEY_CURRENT_USER\' + _PackageKey + ']');
    _RegFile.Add('"' + _RegFilePackagePath + _BplFilename + '"=-');
    _RegFile.Add('[HKEY_LOCAL_MACHINE\' + _PackageKey + ']');
    _RegFile.Add('"' + _RegFilePackagePath + _BplFilename + '"=-');
    _PackageValue := LowerCase(_PackageDirectory + _BplFilename);
    if RemoveValueFromRegistry(HKEY_CURRENT_USER, _PackageKey, _PackageValue) then trace(3, 'Un-Installed the package <%s> from the Delphi IDE for the current user on this computer.', [_PackageDirectory + _BplFilename]);
    if RemoveValueFromRegistry(HKEY_LOCAL_MACHINE, _PackageKey, _PackageValue) then trace(3, 'Un-Installed the package <%s> from the Delphi IDE for all users of this computer.', [_PackageDirectory + _BplFilename]);
    _PackageValue := AddTag(_PackageValue, _DelphiVersion);
    if RemoveValueFromRegistry(HKEY_CURRENT_USER, _PackageKey, _PackageValue) then trace(3, 'Un-Installed the package <%s> from the Delphi IDE for the current user on this computer.', [_PackageDirectory + _BplFilename]);
    if RemoveValueFromRegistry(HKEY_LOCAL_MACHINE, _PackageKey, _PackageValue) then trace(3, 'Un-Installed the package <%s> from the Delphi IDE for all users of this computer.', [_PackageDirectory + _BplFilename]);
    _PackageKey := _PackageBaseKey + 'Disabled Packages';
    _RegFile.Add('[HKEY_CURRENT_USER\' + _BplFilename + ']');
    _RegFile.Add('"' + _RegFilePackagePath + _BplFilename + '"=-');
    _RegFile.Add('[HKEY_LOCAL_MACHINE\' + _PackageKey + ']');
    _RegFile.Add('"' + _RegFilePackagePath + _BplFilename + '"=-');
    _RegFile.Add('');
    _PackageValue := LowerCase(_PackageDirectory + _BplFilename);
    if RemoveValueFromRegistry(HKEY_CURRENT_USER, _PackageKey, _PackageValue) then trace(3, 'Un-Installed the package <%s> from the Delphi IDE for the current user on this computer.', [_PackageDirectory + _BplFilename]);
    if RemoveValueFromRegistry(HKEY_LOCAL_MACHINE, _PackageKey, _PackageValue) then trace(3, 'Un-Installed the package <%s> from the Delphi IDE for all users of this computer.', [_PackageDirectory + _BplFilename]);
    _PackageValue := AddTag(_PackageValue, _DelphiVersion);
    if RemoveValueFromRegistry(HKEY_CURRENT_USER, _PackageKey, _PackageValue) then trace(3, 'Un-Installed the package <%s> from the Delphi IDE for the current user on this computer.', [_PackageDirectory + _BplFilename]);
    if RemoveValueFromRegistry(HKEY_LOCAL_MACHINE, _PackageKey, _PackageValue) then trace(3, 'Un-Installed the package <%s> from the Delphi IDE for all users of this computer.', [_PackageDirectory + _BplFilename]);
    try
      _RegFileName := 'UnRegister_Package_' + ChangeFileExt(ExtractFilename(_PackageName), '.reg');
      if gCreateBatchFile then _RegFile.SaveToFile(ExtractFilePath(FBatchFilename) + _RegFileName);
      FBatchFile.Add(_RegFileName);
    except
      On E:Exception do Showmessage(format('Could not save file <%s> because <%s>.', [_RegFileName, E.Message]));
    end;
  finally
    if Assigned(_RegFile) then FreeAndNil(_RegFile);
  end;
  Result := True;
end;

{-----------------------------------------------------------------------------
  Procedure: ReadPackageInfoDelphi
  Author:    sam
  Date:      22-Aug-2002
  Arguments: _PackageName:string
  Result:    String
  Description: get the package description text out of the dpk-file.
               For delphi 1..7
-----------------------------------------------------------------------------}
function ReadPackageInfoDelphi(_PackageName:string;out Description:string;out LibSuffix:string):boolean; // get the description and lib suffix
var
  _DPKFile:TStrings;
  _Pos:Integer;
  _Text:String;
begin
  result:=false;
  LibSuffix:='';
  Description:='';
  if not fileExists(_PackageName) then begin
    trace(5,'ReadPackageInfo: Could not find the file <%s>.',[_PackageName]);
    exit;
  end;
  _PackageName:=ReadProjectFilenameFromDProj(_PackageName);
  if not fileExists(_PackageName) then begin
    trace(5,'ReadPackageInfo: Could not find the file <%s>.',[_PackageName]);
    exit;
  end;
  _DPKFile:=TStringList.Create;
  try
    _DPKFile.LoadFromFile(_PackageName);
    _Text:=_DPKFile.Text;
    _Pos:=Pos('$DESCRIPTION',_Text);
    if _pos>0 then begin
      Delete(_Text,1,_pos+13);
      _pos:=Pos('''',_Text);
      Description:=Copy(_Text,1,_Pos-1);
    end;

    _Text:=_DPKFile.Text;
    _Pos:=Pos('$LIBSUFFIX',_Text);
    if _pos>0 then begin
      Delete(_Text,1,_pos+11);
      _pos:=Pos('''',_Text);
      LibSuffix:=Copy(_Text,1,_Pos-1);
    end;
    result:=true;
  finally
    FreeAndNil(_DPKFile);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: ReadPackageInfo
  Author:    sam
  Date:      23-Dez-2009
  Arguments: const _PackageName:string;var Description:string
  Result:    boolean
  Description: read information from package file.
-----------------------------------------------------------------------------}
function ReadPackageInfo(const _PackageName:string;out Description:string;out LibSuffix:string):boolean; // get the description text
begin
  result:=ReadPackageInfoDelphi(_PackageName,Description,LibSuffix)
end;


{-----------------------------------------------------------------------------
  Procedure: WinExecAndWait32V2
  Author:    P.Below
             Added Output parameter by S.Herzog
  Date:      22-Aug-2002
  Arguments: FileName: string; Visibility: Integer
  Result:    DWORD
  Description:
  Changes: -SH 05.06.2003 Bugfix for deadlock of the external app.
-----------------------------------------------------------------------------}
function WinExecAndWait(FileName,CommandLine,WorkPath,Environment: string; Visibility: Integer;out Output:String): LongWord;
const
  BufSize = 1024;
var
  i:integer;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SA: TSecurityAttributes;
  StdOutPipeRead,
  StdOutPipeWrite: THandle;
  Buffer: array[1..BufSize] of Byte;
  BytesRead: cardinal;
  _prev_field,_field:string;
  _line:string;
  _lpEnvironment: Pointer;
  _creationFlags: Cardinal;
begin
   result:=0;
   Output:='';
   with SA do
   begin
     nLength := SizeOf(SA);
     bInheritHandle := True;
     lpSecurityDescriptor := nil;
   end;
   CreatePipe(StdOutPipeRead,  // read handle
              StdOutPipeWrite, // write handle
              @SA,             // security attributes
              0                // number of bytes reserved for pipe - 0 default
              );
  try
    FillChar(StartupInfo, SizeOf(StartupInfo), #0);
    StartupInfo.cb          := SizeOf(StartupInfo);
    StartupInfo.dwFlags     := STARTF_USESHOWWINDOW  or  STARTF_USESTDHANDLES;
    StartupInfo.wShowWindow := Visibility;
    StartupInfo.hStdInput   := GetStdHandle(STD_INPUT_HANDLE); // don't redirect std input
    StartupInfo.hStdOutput  := StdOutPipeWrite;
    StartupInfo.hStdError   := StdOutPipeWrite;
    if Environment = '' then begin
      _lpEnvironment := nil;
    end
    else begin
      _lpEnvironment := Pointer(Environment);
    end;

{$IFDEF SUPPORTS_UNICODE_STRING}
    _creationFlags := CREATE_NEW_CONSOLE or
                      NORMAL_PRIORITY_CLASS or
                      CREATE_UNICODE_ENVIRONMENT;
{$ELSE}
    _creationFlags := CREATE_NEW_CONSOLE or
                      NORMAL_PRIORITY_CLASS;
{$ENDIF SUPPORTS_UNICODE_STRING}

    if not CreateProcess(
      nil,    { application name }
      PChar(FileName + ' ' + CommandLine), { command line }
      nil, { pointer to process security attributes }
      nil, { pointer to thread security attributes }
      true, { handle inheritance flag }
      _creationFlags, { creation flags }
      _lpEnvironment, { pointer to new environment block }
      PChar(WorkPath), { pointer to current directory name }
      StartupInfo, { pointer to STARTUPINFO }
      ProcessInfo) { pointer to PROCESS_INF }
    then begin
      Result := DWORD(-1) { failed, GetLastError has error code }
    end
    else begin
      //Wait for completion and read pipe
      CloseHandle(ProcessInfo.hThread);
      repeat
        GetExitCodeProcess(ProcessInfo.hProcess, Result);

        PeekNamedPipe(StdOutPipeRead, @Buffer, sizeof(Buffer), @BytesRead, nil, nil);
        if BytesRead > 0 then begin
          if ReadFile(StdOutPipeRead, Buffer, sizeof(Buffer), BytesRead, nil) then begin
            while BytesRead > 0 do begin
              for i:=1 to BytesRead do Output := Output + char(Buffer[i]); // combine the buffer with the rest of the last run
              if BytesRead < sizeof(Buffer) then break;
              if not ReadFile(StdOutPipeRead, Buffer,sizeof(Buffer), BytesRead, nil) then break;
            end;
          end;
        end;
        Application.ProcessMessages;
      until (Result <> STILL_ACTIVE) or (Application.Terminated);
      CloseHandle(ProcessInfo.hProcess);
      _line:=Output;
      _prev_field:='';
      _field:='';
      Output:='';
      if length(_line)>0 then begin
        _field:=trim(GetField(#$D,_line));
        while _line<>'' do begin
          if (_field<>_prev_field) then Output:=Output+_field+#$D+#$A;
           _prev_field:=_field;
           _field:=trim(GetField(#$D,_line));
        end;
      end;
    end;
  finally
    CloseHandle(StdOutPipeWrite);
    CloseHandle(StdOutPipeRead);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: CompileProject
  Author:    sam
  Date:      29-Aug-2002
  Arguments:
  _Compiler,                 --> full name and path to bcc32.exe
  _CompilerSwitches,         --> compiler switches like -W -H -B for example
  _ProjectName,              --> the name of the project to be compiled (can be .dpr or .dpk )
  _TargetPath,               --> output target path
  _DCUPath,                  --> unit .dcu output path
  _DCPPath,                  --> unit .dcp output path
  _WorkPath,                 --> working path for the compiler
  _NameSpaces: string        --> unit namespaces for the compiler
  _ProjectType: TProjectType --> (tp_dll, tp_exe, tp_bpl)
  var Output: string         --> output text of the compiler.
  _DelphiVersion: Integer    -->
  Result:    Boolean
  Description:
-----------------------------------------------------------------------------}
function CompileProject(_Compiler, _CompilerSwitches, _ProjectName, _TargetPath, _DCUPath, _DCPPath, _WorkPath, _NameSpaces: string; _ProjectType: TProjectType; var Output: string; const _DelphiVersion: Integer): Boolean; // compile the package
var
  _commandLine: string;
  _returnValue: Cardinal;
begin
  Result := False;
  if not fileexists(_Compiler) then begin
    ShowMessage(Format('Could not find the Delphi Compiler file <%s>. Please check settings.', [_Compiler]));
    trace(1, 'Problem in CompileProject: Problem, could not find the Delphi Compiler file <%s>.', [_Compiler]);
    Exit;
  end;

  if not FileExists(_ProjectName) then begin
    ShowMessage(Format('Could not find the Project file <%s>. Please check if your BPG-File is still ok.', [_ProjectName]));
    trace(1, 'Problem in CompileProject: Problem, could not find the Project file <%s>.', [_ProjectName]);
    exit;
  end;

  if not CheckDirectory(_WorkPath) then begin
    trace(1, 'Problem in CompileProject: Problem, could not find the work path <%s>.', [_WorkPath]);
    Exit;
  end;

  if not CheckDirectory(_TargetPath) then begin
    trace(1, 'Problem in CompileProject: Problem, could not find the target path <%s>.', [_TargetPath]);
    Exit;
  end;

  if not CheckDirectory(_DCUPath) then begin
    trace(1, 'Problem in CompileProject: Problem, could not find the dcu path <%s>.', [_DCUPath]);
    Exit;
  end;

  if not CheckDirectory(_DCPPath) then begin
    trace(1, 'Problem in CompileProject: Problem, could not find the dcp path <%s>.', [_DCPPath]);
    Exit;
  end;

  if (LastPos(_TargetPath, '\') = Length(_TargetPath)) and
     (Length(_TargetPath) > 2) then Delete(_TargetPath, Length(_TargetPath), 1);

  if (LastPos(_DCUPath, '\') = Length(_DCUPath)) and
     (Length(_DCUPath) > 2) then Delete(_DCUPath, Length(_DCUPath), 1);

  if (LastPos(_DCPPath, '\') = Length(_DCPPath)) and
     (Length(_DCPPath) > 2) then Delete(_DCPPath, Length(_DCPPath), 1);

  trace(5, '*************************************************************************************', []);
  trace(5, 'Compile Project <%s>.', [_ProjectName]);
  trace(5, 'Compiler is <%s>.', [_compiler]);
  trace(5, 'Work path is <%s>.', [_WorkPath]);
  trace(5, 'Output path is <%s>.', [_TargetPath]);
  trace(5, 'DCU path is <%s>.', [_DCUPath]);
  trace(5, 'DCP path is <%s>.', [_DCPPath]);
  _commandLine := _CompilerSwitches + ' "' + _ProjectName;
  if _TargetPath <> '' then begin
    case _ProjectType of
      tp_dll,
      tp_exe: _commandLine := _commandLine + '" -E"' + _TargetPath + '"';
    else
      _commandLine := _commandLine + '" -LE"' + _TargetPath + '"';
    end;
  end;
  if _DCPPath <> '' then begin
    if _ProjectType = tp_bpl then begin
      _commandLine := _commandLine + ' -LN"' + _DCPPath + '"';
    end;
  end;
  if _DCUPath <> '' then begin
    if _DelphiVersion <= 7 then begin
      // Delphi 7
      _commandLine := _commandLine + ' -N"' + _DCUPath + '"';
    end
    else begin
      _commandLine := _commandLine + ' -N0"' + _DCUPath + '"';
    end;
  end;

  if _DelphiVersion >= 15 then begin
    // Delphi XE
    _commandLine := _commandLine + ' -NS"' + _NameSpaces + '"';
  end;
  trace(3, 'Command line is %s.', [_commandLine]);
  FBatchFile.Add('cd "' + ExtractFilePath(_ProjectName) + '"');
  FBatchFile.Add('"' + _compiler + '" ' + _commandLine);
  _returnValue := WinExecAndWait(_compiler,
                                 _commandLine,
                                 _WorkPath,
                                 '',
                                 SW_HIDE,
                                 Output);
  if _returnValue = 0 then begin
    trace(5, 'CompileProject: Successfully build Project file <%s>.', [_ProjectName]);
    Result := True;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadPackageListfromBPGFileToList
  Author:    sam
  Date:      02-Jan-2010
  Arguments: _filename:string;var lst:TListBox
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure ReadPackageListfromBPGFileToList(_filename:string;var lst:TListBox);
var
  _BPGFile:TStrings;
  i:integer;
  _line:string;
  _Projects:String;
  _Project:String;
  _idx:Integer;

function RemoveSlash(_Project:String):String;
var
_pos:Integer;
begin
  Result:=_Project;
  _pos:=Pos('\',_Project);
  if _pos>0 then begin
    System.Delete(_Project,_pos,1);
    Result:=trim(_project);
  end;
end;

begin
  if not fileExists(_filename) then begin
    trace(5,'ReadPackageListfromBPGFile: Could not find the file <%s>.',[_filename]);
    exit;
  end;
  _BPGFile:=TStringList.Create;
  try
    _BPGFile.LoadFromFile(_filename);
    _idx:=0;
    _Projects:='';
    for i:=0 to _BPGFile.Count-1 do begin
      _line:=trim(_BPGFile.Strings[i]);
      if Pos('PROJECTS',_line)=1 then _Projects:=_line
      else begin
        if _Projects<>'' then begin
          if pos('#--------------',_line)=1 then begin
            _idx:=i;
            break;
          end
          else _Projects:=_Projects+_line;
        end;
      end;
    end;
    System.Delete(_Projects,1,10);
    _Projects:=trim(_Projects);
    _Project:=GetField(' ',_Projects);
    _Project:=RemoveSlash(_Project);
    while _Project<>'' do begin
      for i:=_idx to _BPGFile.Count-1 do begin
        _line:=trim(_BPGFile.Strings[i]);
        if Pos(_Project,_line)=1 then begin
          System.Delete(_line,1,Pos(':',_line));
          lst.Items.add(trim(_line));
          break;
        end;
      end;
      _Project:=GetField(' ',_Projects);
      _Project:=RemoveSlash(_Project);
    end;
  finally
    _BPGFile.free;
  end;
end;


{*-----------------------------------------------------------------------------
  Procedure: ReadPackageListfromBDSGroupFile
  Author:    sam
  Date:      26-Mai-2008
  Arguments: _filename:string;var lst:TListBox
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure ReadPackageListfromBDSGroupFileToList(_filename:string;var lst:TListBox); // read the package names from the delphi project group file <.bdsgroup>
var
  i:integer;
  _XMLFile:xmlintf.IXMLDocument;
  _Project:String;
begin
  if not fileExists(_filename) then begin
    trace(5,'ReadPackageListfromBDSGroupFileToList: Could not find the file <%s>.',[_filename]);
    exit;
  end;
  _XMLFile:=newXMLDocument;
  try
    _XMLFile.LoadFromFile(_filename);
    _XMLFile.active:=true;
    for i:=0 to _XMLFile.DocumentElement.ChildNodes['Default.Personality'].ChildNodes['Projects'].ChildNodes.Count-2 do begin
      _Project:=_XMLFile.DocumentElement.ChildNodes['Default.Personality'].ChildNodes['Projects'].ChildNodes[i].text;
      trace(5,'ReadPackageListfromBDSGroupFileToList: Read project <%s> from file <%s>.',[_Project,_filename]);
      lst.Items.add(trim(_Project));
    end;
  finally
    _XMLFile.active:=false;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadPackageListfromGroupProjFileToStrings
  Author:    sam
  Date:      13-Jun-2008
  Arguments: _filename:string;var lst:TStringList
  Result:    None
  Description:  read packages&projects from a .groupproj-file <_filename> into the stringlist <lst>.
-----------------------------------------------------------------------------}
procedure ReadPackageListfromGroupProjFileToStrings(_filename: string; var lst: TStringList); // read the package names from the delphi project group file <.bdsgroup>
var
  i: Integer;
  _Projectname: string;
  _errormsg: string;
  _ProjectData: TProjectData;
begin
  if not Assigned(lst) then Exit;
  if not FileExists(_filename) then begin
    trace(5, 'ReadPackageListfromGroupProjFileToStrings: Could not find the file <%s>.', [_filename]);
    Exit;
  end;
  lst.Clear;
  i := 0;
  repeat
    if not ReadNodeText(_filename, Format('//Projects[%d]/@Include',[i]),_Projectname,_errormsg) then Exit;
    if  _Projectname <> '' then begin
      _ProjectData := TProjectData.Create;
      lst.AddObject(_Projectname, _ProjectData);
    end;
    Inc(i);
  until (_Projectname = '') or (i > 10000);
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadPackageListfromGroupProjFileToList
  Author:    sam
  Date:      15-Nov-2008
  Arguments: _filename:string;var lst:TListBox
  Result:    None
  Description: read the project/package list from a .groupproj file.
-----------------------------------------------------------------------------}
procedure ReadPackageListfromGroupProjFileToList(_filename:string;var lst:TListBox);
var
  i:integer;
  _Strings:TStringList;
begin
  if not fileExists(_filename) then begin
    trace(5,'ReadPackageListfromGroupProjFileToList: Could not find the file <%s>.',[_filename]);
    exit;
  end;
  _Strings:=TStringList.create;
  try
    ReadPackageListfromGroupProjFileToStrings(_filename,_Strings);
    for i:=0 to _Strings.count-1 do begin
      lst.items.add(trim(_Strings[i]));
    end;
  finally
    _Strings.free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadPackageListfromFile
  Author:    sam
  Date:      13-Jun-2008
  Arguments: _filename:string;var lst:TListBox
  Result:    None
  Description: read packages&projects from the goup-file <_filename> into the listbox <lst>.
-----------------------------------------------------------------------------}
procedure ReadPackageListfromFile(_filename:string;var lst:TListBox);overload;
begin
  if not assigned(lst) then exit;
  if lowercase(ExtractFileExt(_filename))='.bpg'       then ReadPackageListfromBPGFileToList(_filename,lst) else
  if lowercase(ExtractFileExt(_filename))='.bdsgroup'  then ReadPackageListfromBDSGroupFileToList(_filename,lst) else
  if lowercase(ExtractFileExt(_filename))='.groupproj' then ReadPackageListfromGroupProjFileToList(_filename,lst);
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadPackageListfromBPGFileToStrings
  Author:    sam
  Date:      26-Mai-2008
  Arguments: _filename:string;var lst:TStrings
  Result:    None
  Description: read the packages list from file <_filename> and put them into the list <lst>.
-----------------------------------------------------------------------------}
procedure ReadPackageListfromBPGFileToStrings(_filename: string; var lst: TStringList);
var
  _BPGFile: TStrings;
  i: Integer;
  _line: string;
  _Projects: string;
  _Project: string;
  _idx: Integer;
  _ProjectData: TProjectData;

  function RemoveSlash(_Project: string): string;
  var
    _pos: Integer;
  begin
    Result := _Project;
    _pos := Pos('\', _Project);
    if _pos > 0 then begin
      System.Delete(_Project, _pos, 1);
      Result := Trim(_Project);
    end;
  end;

begin
  if not Assigned(lst) then exit;
  if not FileExists(_filename) then begin
    trace(5, 'ReadPackageListfromBPGFileToStrings: Could not find the file <%s>.', [_filename]);
    Exit;
  end;
  _BPGFile := TStringList.Create;
  _BPGFile.LoadFromFile(_filename);
  lst.Clear;
  _idx := 0;
  _Projects := '';
  for i := 0 to _BPGFile.Count-1 do begin
    _line := Trim(_BPGFile.Strings[i]);
    if Pos('PROJECTS', _line) = 1 then _Projects := _line
    else begin
      if _Projects <> '' then begin
        if Pos('#--------------',_line) = 1 then begin
          _idx := i;
          Break;
        end
        else _Projects := _Projects + _line;
      end;
    end;
  end;
  System.Delete(_Projects, 1, 10);
  _Projects := Trim(_Projects);
  _Project := GetField(' ', _Projects);
  _Project := RemoveSlash(_Project);
  while _Project <> '' do begin
    for i := _idx to _BPGFile.Count-1 do begin
      _line := Trim(_BPGFile.Strings[i]);
      if Pos(_Project, _line) = 1 then begin
        _ProjectData := TProjectData.Create;
        System.Delete(_line, 1, Pos(':', _line));
        lst.AddObject(Trim(_line), _ProjectData);
        Break;
      end;
    end;
    _Project := GetField(' ', _Projects);
    _Project := RemoveSlash(_Project);
  end;
  _BPGFile.Free;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadPackageListfromBDSGroupFileToStrings
  Author:    sam
  Date:      13-Jun-2008
  Arguments: _filename:string;var lst:TStringList
  Result:    None
  Description: read packages&projects from a .bdsgroup-file <_filename> into the stringlist <lst>.
-----------------------------------------------------------------------------}
procedure ReadPackageListfromBDSGroupFileToStrings(_filename: string; var lst: TStringList); // read the package names from the delphi project group file <.bdsgroup>
var
  i: Integer;
  _XMLFile: xmlintf.IXMLDocument;
  _Project: string;
  _ProjectData: TProjectData;
begin
  if not Assigned(lst) then Exit;
  if not FileExists(_filename) then begin
    trace(5, 'ReadPackageListfromBDSGroupFileToStrings: Could not find the file <%s>.', [_filename]);
    Exit;
  end;
  lst.Clear;
  _XMLFile := newXMLDocument;
  try
    _XMLFile.LoadFromFile(_filename);
    _XMLFile.Active := True;
    for i := 0 to _XMLFile.DocumentElement.ChildNodes['Default.Personality'].ChildNodes['Projects'].ChildNodes.Count-2 do begin
      _Project := _XMLFile.DocumentElement.ChildNodes['Default.Personality'].ChildNodes['Projects'].ChildNodes[i].Text;
      _ProjectData := TProjectData.Create;
      lst.AddObject(Trim(_Project), _ProjectData);
    end;
  finally
    _XMLFile.active:=false;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadPackageListfromFile
  Author:    sam
  Date:      13-Jun-2008
  Arguments: _filename:string;var lst:TStringList
  Result:    None
  Description: read packages&projects from the group-file <_filename> into the stringlist <lst>.
-----------------------------------------------------------------------------}
procedure ReadPackageListfromFile(_filename: string; var lst: TStringList); overload;
begin
  if lowercase(ExtractFileExt(_filename))='.bpg'       then ReadPackageListfromBPGFileToStrings(_filename, lst) else
  if lowercase(ExtractFileExt(_filename))='.bdsgroup'  then ReadPackageListfromBDSGroupFileToStrings(_filename, lst) else
  if lowercase(ExtractFileExt(_filename))='.groupproj' then ReadPackageListfromGroupProjFileToStrings(_filename, lst);
end;

{-----------------------------------------------------------------------------
  Procedure: DeleteFile
  Author:    sam
  Date:      22-Aug-2002
  Arguments: _Filename:String
  Result:    None
  Description: delete the  file if exists.
-----------------------------------------------------------------------------}
function DeleteFile(const _Filename:String;const _toTrashBin:boolean=true):boolean;  // delete the bpl file.
resourcestring
cCouldNotDeleteFile='Could not delete file <%s>. Error <%s>.';

function DeleteFileWithUndo(sFileName: string): Boolean;
var
_returnvalue:integer;
_fos: TSHFileOpStruct;
begin
  FillChar(_fos, SizeOf(_fos), 0);
  _fos.wFunc  := FO_DELETE;
  _fos.pFrom  := PChar(sFileName+#0);
  _fos.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT;
  _returnvalue:=ShFileOperation(_fos);
  result :=(0 =_returnvalue);
  if not result then  trace(1,'Error in DeleteFileWithUndo: Deletion of file <%s> did not work. Return value <%d>.',[sFileName,_returnvalue]);
end;

begin
  result:=false;
  if _Filename='' then exit;
  if not fileexists(_Filename) then begin
    trace(3,'DeleteFile: Can not Delete the file <%s> because it does not exist.',[_Filename]);
    exit;
  end;
  try
    if _toTrashBin then begin
      if not DeleteFileWithUndo(_Filename) then begin
        trace(1,'Error when deleting file <%s> to trash. The file may be in use.',[_Filename]);
        exit;
      end;
    end
    else begin
      if not SysUtils.DeleteFile(_Filename) then begin
        trace(1,'Error when deleting file <%s>. The file may be in use.',[_Filename]);
        exit;
      end;
    end;
    FBatchFile.add('del "'+_Filename+'"');
    trace(3,'Deleted file <%s>.',[_Filename]);
    result:=true;
  except
    on e:exception do begin
      Application.MessageBox(pchar(format(cCouldNotDeleteFile,[_Filename,e.message])),pchar(cError),MB_ICONERROR or MB_OK);
      trace(1,'Error when deleting file <%s>. <%s>',[_Filename,e.message]);
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: processExists
  Author:    sam
  Date:      03-Mrz-2006
  Arguments: exeFileName: string
  Result:    Boolean
  Description: tip from swissdelphicenter.ch
-----------------------------------------------------------------------------}
function GetProcessID(exeFileName: string): DWord;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  result:=0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeFileName))) then
    begin
      Result := FProcessEntry32.th32ProcessID;
      break;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

{-----------------------------------------------------------------------------
  Procedure: IsDelphiStarted
  Author:
  Date:      30-Aug-2002
  Arguments: None
  Result:    Boolean
  Description:
-----------------------------------------------------------------------------}
function IsDelphiStarted(const _DelphiVersion:Integer): Boolean;
begin
  result := GetProcessID(GetDelphiApplication(_DelphiVersion))>0;
end;

{-----------------------------------------------------------------------------
  Procedure: ShutDownDelphi
  Author:
  Date:      30-Aug-2002
  Arguments: _DelphiVersion:Integer
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure ShutDownDelphi(const _DelphiVersion:Integer);
var
  PrID: Integer; // processidentifier
  Ph: THandle;   // processhandle
begin
  PrID := GetProcessID(GetDelphiApplication(_DelphiVersion));
  if PrID=0 then exit;
  Ph := OpenProcess(1, BOOL(0), PrID);
  TerminateProcess(Ph, 0);
  trace(5,'ShutDownDelphi:  try to close delphi.',[]);
end;

{-----------------------------------------------------------------------------
  Procedure: StartUpDelphi
  Author:
  Date:      30-Aug-2002
  Arguments: None
  Result:    None
  Description: start delphi IDE
  11.01.2006 SH - added chars " at begin and end of filename so that projects with spaces in pathname also open.
-----------------------------------------------------------------------------}
procedure StartUpDelphi(const _DelphiVersion:Integer;_ProjectName:string);
var
_FileName : String;
_NoOfDeletedKeys:integer;
begin
  _FileName := GetDelphiApplication(_DelphiVersion);
  if not FileExists(_FileName) then exit;
  VerifyRegistry(_DelphiVersion,_NoOfDeletedKeys,'','');
  trace(5,'StartUpDelphi:Try to start Delphi from <%s>.',[_FileName]);
{$IF CompilerVersion < 20.0}
  ShellExecute(0, 'open', PChar(_FileName), PChar(' /ns "'+_ProjectName+'"'), nil, SW_SHOWNORMAL);
{$ELSE}
  ShellExecute(0, 'open', PWideChar(_FileName), PWideChar(' /ns "'+_ProjectName+'"'), nil, SW_SHOWNORMAL);
{$IFEND}
end;

{*-----------------------------------------------------------------------------
  Procedure: RemoveTrailingSemikolon
  Author:    sam
  Date:      27-Apr-2010
  Arguments: const _path:string
  Result:    string
  Description: if there is a ';' at the end, then remove it.
-----------------------------------------------------------------------------}
function RemoveTrailingSemikolon(const _path:string):string;
begin
  result:=_path;
  if LastPos(result, ';') = length(result) then System.Delete(result, length(result), 1);
end;

{-----------------------------------------------------------------------------
  Procedure: GetIDEEnvironmentPath
  Author:    sam
  Date:      17-Mrz-2017
  Arguments: const _DelphiVersion:integer
  Result:    string
  Description: read the environments variable for Delphi-Version <_DelphiVersion>.
-----------------------------------------------------------------------------}
function GetIDEEnvironmentPath(const _DelphiVersion:integer): string;
const
cEnvironmentsValueName='PATH';
var
_Reg: TRegistry;
_IDERootKey:string;
begin
  result := '';
  if not GetIDERootKey(_DelphiVersion,_IDERootKey) then exit;
  _Reg := TRegistry.Create;
  try
    _Reg.rootkey := HKEY_CURRENT_USER;
    _IDERootKey:=_IDERootKey+'Environment Variables';

    if not _Reg.KeyExists(_IDERootKey) then begin   // if the key does not exist, then try to create it.
      trace(2,'Warning in GetIDEEnvironmentPath: Could not find key <%s> in <HKEY_CURRENT_USER>. Try now to create it...',[_IDERootKey]);
      try
        _Reg.CreateKey(_IDERootKey)
      except
        on e:exception do begin
          trace(1,'Error in GetIDEEnvironmentPath: Could not create key <%s> in <HKEY_CURRENT_USER>. <%s>.',[_IDERootKey,e.message]);
          exit;
        end;
      end;
    end;

    if not _Reg.OpenKeyReadOnly(_IDERootKey) then begin
      trace(1,'Error in GetIDEEnvironmentPath: Could not open <HKEY_CURRENT_USER,%s>. Check user rights.',[_IDERootKey]);
      exit;
    end;
    if not _Reg.ValueExists(cEnvironmentsValueName) then begin
      trace(2,'Warning in GetIDEEnvironmentPath: Could not find value <%s>. Try now to create it....',[cEnvironmentsValueName]);
      try
        _Reg.WriteString(cEnvironmentsValueName,'');
      except
        on e:exception do begin
           trace(1,'Error in GetIDEEnvironmentPath: Could not create value <%s>. <%s>.',[cEnvironmentsValueName,e.message]);
          exit;
        end;
      end;
    end;
    result := _Reg.ReadString(cEnvironmentsValueName);
    trace(5,'GetIDEEnvironmentPath: Read value <%s> from registry.',[result]);
  finally
    _Reg.CloseKey;
    _Reg.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: SetIDEEnvironmentPath
  Author:    sam
  Date:      17-Mrz-2017
  Arguments: const _DelphiVersion:integer;_IDEEnvironmentPath:string;const _silent:boolean
  Result:    boolean
  Description: set IDE Environment Path <_IDEEnvironmentPath> for version <_DelphiVersion>.
-----------------------------------------------------------------------------}
function SetIDEEnvironmentPath(const _DelphiVersion:integer;_IDEEnvironmentPath:string;const _silent:boolean):boolean;
resourcestring
cAskToChangeEnvrionmentPath='Do you want to change the Delphi''s Environment Path to <%s>?';
var
_IDERootKey: string;

  function _SetIDEEnvironmentPath(_RootKey:HKEY):boolean;
  const
  cEnvironmentsValueName='PATH';
  var
  _Reg: TRegistry;
  begin
    result:=false;

    _Reg := TRegistry.Create(KEY_READ or KEY_WRITE or KEY_WOW64_32KEY);
    try
      _Reg.RootKey := _RootKey;
      if not _Reg.OpenKey(_IDERootKey,false) then begin
        trace(1,'Problem in _SetIDEEnvironmentPath: The Key <%s,%s> was not found in the registry.',[HKEYToStr(_RootKey),_IDERootKey]);
        exit;
      end;
      try
        _Reg.WriteString(cEnvironmentsValueName,_IDEEnvironmentPath);
         trace(3,'Successfully set <%s> to <%s>',[cEnvironmentsValueName,_IDEEnvironmentPath]);
        result:=true;
      except
        on e:exception do trace(1,'Warning in _SetIDEEnvironmentPath: Could not write the IDE Environment Path for delphi version <%s>.You need to have Admin rights for this computer. <%s>.',
          [DelphiVersions[_DelphiVersion].LongName,e.message]);
      end;
      _Reg.CloseKey;
    finally
      _Reg.Free;
    end;
  end;

begin
  result:=false;
  if not GetIDERootKey(_DelphiVersion,_IDERootKey) then begin
    trace(3,'Problem in SetIDEEnvironmentPath: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;
  _IDERootKey:=_IDERootKey+'Environment Variables';
  if not _Silent then begin
    if Application.MessageBox(pchar(format(cAskToChangeEnvrionmentPath,[_IDEEnvironmentPath])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDNo then exit;
  end;
  result:=_SetIDEEnvironmentPath(HKEY_CURRENT_USER);
end;

{-----------------------------------------------------------------------------
  Procedure: GetIDEEnvironmentPathList
  Author:    sam
  Date:      17-Mrz-2017
  Arguments: const _DelphiVersion:integer
  Result:    TStrings
  Description:
-----------------------------------------------------------------------------}
function GetIDEEnvironmentPathList(const _DelphiVersion:integer): TStrings;
var
  _s: string;
  _path: string;
begin
  result := TStringList.create;
  _s := GetIDEEnvironmentPath(_DelphiVersion);
  while _s <> '' do begin
    _path := lowercase(trim(GetField(';', _s)));
    if _path = '' then continue;
    _path := ExcludeTrailingPathDelimiter(_path);
    if result.IndexOf(_path) = -1 then result.add(lowercase(_path));
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: IsPathInIDEEnvironmentPath
  Author:    sam
  Date:      17-Mrz-2017
  Arguments: const _DelphiVersion:integer;const _path: string
  Result:    boolean
  Description: returns <true> if the path <_path> is already in the IDEenvironment variable.
-----------------------------------------------------------------------------}
function IsPathInIDEEnvironmentPath(const _DelphiVersion:integer;const _path: string): boolean; // checks if the path <_path> is already in the IDE environment variable.
var
  _pathList: TStrings;
  _entry: string;
begin
  result := false;
  _pathList := GetIDEEnvironmentPathList(_DelphiVersion);
  try
    _entry := lowercase(_path);
    _entry := ExcludeTrailingPathDelimiter(_entry);
    if _entry = '' then exit;
    if (_pathList.IndexOf(_entry) = -1) and
      (_pathList.IndexOf(IncludeTrailingPathDelimiter(_entry)) = -1) then exit;
    result := true;
  finally
    _pathList.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: SetIDEEnvironmentPathList
  Author:    sam
  Date:      17-Mrz-2017
  Arguments: const _DelphiVersion:integer;const _list: TStrings
  Result:    boolean
  Description: set the IDE Environment Path to the value created
               by the content of <_list>.
-----------------------------------------------------------------------------}
function SetIDEEnvironmentPathList(const _DelphiVersion:integer;const _list: TStrings): boolean;
var
  i: integer;
  _path: string;
  _entry: string;
begin
  _path := '';
  for i := 0 to _list.count - 1 do begin
    _entry := _list[i];
    if IsLastChar('\', _entry) then Delete(_entry, length(_entry), 1);
    _path := _path + _entry + ';';
  end;
  result := SetIDEEnvironmentPath(_DelphiVersion,_path, false);
end;

{-----------------------------------------------------------------------------
  Procedure: AddIDEEnvironmentPath
  Author:    sam
  Date:      17-Mrz-2017
  Arguments: const _DelphiVersion:integer;const _path: string
  Result:    boolean
  Description: add the path <_path> to the IDE Envrionment Path.
-----------------------------------------------------------------------------}
function AddIDEEnvironmentPath(const _DelphiVersion:integer;const _path: string): boolean;
var
_PathList: TStrings;
begin
  result := false;
  if _path = '' then exit;
  _pathList := GetIDEEnvironmentPathList(_DelphiVersion);
  try
    if _pathList.IndexOf(lowercase(_path)) > -1 then exit;
    _pathList.Add(lowercase(_path));
    trace(1, 'Added the directory <%s> to the IDE Environments Variable.', [_path]);
    result := SetIDEEnvironmentPathList(_DelphiVersion,_pathList);
  finally
    _pathList.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: VerifyIDEEnvrionmentsPath
  Author:    sam
  Date:      17-Mrz-2017
  Arguments: const _DelphiVersion:integer
  Result:    boolean
  Description: remove in-existent folders from the IDE Environments Path.
-----------------------------------------------------------------------------}
function VerifyIDEEnvrionmentsPath(const _DelphiVersion:integer;const _silent:boolean;out DeletedPathEntries:integer):boolean;
resourcestring
cAskToCleanUPEnvrionmentPath='The folder <%s> does not exist, but the Delphi IDE Environment Path contains this entry. Shall DPT clean-up this entry?';
var
i:integer;
_PathList: TStrings;
_Path:string;
_NewPathValue:string;
begin
  DeletedPathEntries:=0;
  _PathList:=GetIDEEnvironmentPathList(_DelphiVersion);
  try
    for i:=0 to _PathList.Count-1 do begin
      _path:=ReplaceTag(_PathList[i],_DelphiVersion,'',''); // try to expand place-holders.
      if pos('%',_path)>0 then begin // if there is still a placeholder in the path-name, then don't touch it.
        _NewPathValue:=_NewPathValue+_PathList[i]+';';
        continue;
      end;
      if DirectoryExists(_path) or                 // if the path still exists
         (lowercase(_path)='$(path)') then begin    // or it is the entry $(path)
        _NewPathValue:=_NewPathValue+_PathList[i]+';';  // then keep it
        continue;
      end;
      if not _silent then begin
        if Application.MessageBox(pchar(format(cAskToCleanUPEnvrionmentPath,[_path])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDNo then begin
          _NewPathValue:=_NewPathValue+_PathList[i]+';';  // or if the user wants to keept it
          continue;
        end;
      end;
      trace(2,'VerifyIDEEnvrionmentsPath: Remove path <%s>.',[_path]);
      inc(DeletedPathEntries);
    end;
    if DeletedPathEntries=0 then begin  // nothing changed, so we can leave method here.
      result:=true;
      exit;
    end;
    result:=SetIDEEnvironmentPath(_DelphiVersion,_NewPathValue,_silent);
    if result then trace(3,'VerifyIDEEnvrionmentsPath: Changed IDE Environment Settings from <%s> to <%s>.',[_PathList.text,_NewPathValue]);
  finally
    _PathList.free;
  end;
end;

initialization
  FBatchFile:=TStringList.create;
finalization
  FBatchFile.Free;
end.












