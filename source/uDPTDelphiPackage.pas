{-----------------------------------------------------------------------------
 Unit Name: uDPTDelphiPackage
 Author:    s.herzog
 Purpose:   cool functions to install/uninstall and recompile delphi projects.
 History:
-----------------------------------------------------------------------------}
//TODO: some re-factoring of this unit is needed.


unit uDPTDelphiPackage;

interface
uses Classes,
     Windows,
     StdCtrls,
     IniFiles,
     Registry;

resourcestring
cConfirm='Confirm';
cWarning='Warning';
cError='Error';
cInformation='Information';

const
  cLIBAutomaticTag='<Auto>';     
  cLIBNoneTag='<None>';

type
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

function  GetDelphiPackageDir(const _DelphiVersion:integer):string; // get the delphi project\bpl path for Delphi Version <_DelphiVersion>.
function SetDelphiPackageDir(const _DelphiVersion:integer;_PackageDir:string;const _silent:boolean):boolean; // write the package dir (bpl-folder) <_PackageDir> for Delphi Version <_DelphiVersion>.
procedure CreateProjectGroupFile(const _lstProjectFiles:TListBox;const _projectGroupFilename:string;const _DelphiVersion:integer); // create a bpg-file or bdsproj-file.
function  InstallPackage(_PackageName,_PackageDirectory,_PackageDescription,_PackageLibSuffix:String;_DelphiVersion:Integer):string; // add package into the regitstry.
function  UninstallPackage(_PackageName,_PackageDirectory,_PackageLibSuffix:String;_DelphiVersion:Integer):boolean;  // remove package from regeistry.
function  CompileProject(_Compiler,_CompilerSwitches,_ProjectName,_TargetPath,_DCUPath,_WorkPath:string;Var Output:String;const _DelphiVersion:integer):boolean; // compile the package
function  VerifyRegistry(const _DelphiVersion:integer):boolean; // scan through the registry items of "Known Packages" and "Disabled Packages" and check if the referenced files really exists. If not then remove the registry key.
procedure ReadPackageListfromFile(_filename:string;var lst:TListBox);overload;  //read packages&projects from the goup-file <_filename> (.bpg or .bdsgroup or .groupproj) into the listbox <lst>.
procedure ReadPackageListfromFile(_filename:string;var lst:TStrings);overload;  //read packages&projects from the goup-file <_filename> (.bpg or .bdsgroup or .groupproj) into the stringlist <lst>.
function  ReadPackageInfo(const _PackageName:string;var Description:string;var LibSuffix:string):boolean; // get the information from the dpk file.
function  WinExecAndWait(FileName,CommandLine,WorkPath: string; Visibility: Integer;Var Output:String): LongWord;
function  isDelphiStarted(const _DelphiVersion:Integer): Boolean;
procedure ShutDownDelphi(const _DelphiVersion:Integer;_Blocking : Boolean);
procedure StartUpDelphi(const _DelphiVersion:Integer;_ProjectName:string);
function  ReadProjectFilenameFromDProj(const _Filename:String):string; // the real project filename is now hidden in the dproj-file.
function  ReadConfigurationSettings(const _filename:string;var Conditions:string;var SearchPath:String;var ProjectOutputPath:string;var BPLOutputPath:string;var DCUOutputPath:string):boolean;
function  WriteSettingsToDelphi(_bpgPath,_Filename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath:string;const _silent:boolean;const _DelphiVersion:integer):boolean; // get informations from the cfg-file.
function  GetDelphiRootDir(const _DelphiVersion:integer):string;  // returns delphi root directory e.g. C:\Program files\Borland\Delphi7
function  GetInstalledIDEVersions(_list:TStrings):boolean; // returns delphi and bds versions.
procedure InitBatchFile(const _filename:string); // reset batch file
function  SaveBatchFile:string; // save the batch file.
function  GetPackageVersion(const _PackageName,_PackageOutputPath,_PackageLibSuffix:string;const _ProjectType:TProjectType):string;
function  ReplaceTag(_filename:string;_DelphiVersion:integer):string;
function  DetermProjectType(_projectfilename:string;const _projectGroupfilename:string;const _DelphiVersion:integer):TProjectType; // find out if the source file contains a application or library or package.
function  AbsoluteFilename(_realpath,_filename: string):string; // converts a relative package filename into absolute filename
function  AbsolutePath(_basepath,_path:string;const _DelphiVersion:integer):string; // converts the path <_path> into an absolute pathname.
function  RelativeFilename(const _basepath,_filename:string;const _DelphiVersion:integer):string; // converts the filename <_filename> into a relative filename.
function  RelativePath(_basepath,_path:string;const _DelphiVersion:integer;const _ReplaceTags:boolean=true):string; // converts the path <_path> into a realtive pathname.
function  OutputFilename(const _filename:string;const _ProjectType:TProjectType;const _libsuffix:string=''):string; // input is a source code filename, output is the name of the compiled target.
function  GetPackageSize(_PackageName,_PackageOutputPath,_PackageLibSuffix:string;const _ProjectType:TProjectType):Int64; // read the filesize of the package.
function  GetDelphiPathTag(const _version:integer):string; // returns $(DELPHI) or $(BDS) according to the version number
function  VersionNoToIDEName(const _version:integer):string; // turns a ide version no 1-9 into 6.0,7.0,BDS 1.0,BDS 2.0
function  CleanUpPackagesByRegistery(const _ROOTKEY:DWORD;const _DelphiVersion:integer;const _DelphiSubKey:string;const _DelphiBINPath:string;const _deletefiles:boolean):boolean; // this method delete's the key HKEY_LOCAL_MACHINE/Software/Borland/Delphi/%VERSIONNO%/Known Packages and the same for HKEY_CURRENT_USER
function  CleanUpPackagesByBPLPath(const _DelphiVersion:integer;_BPLPath:string;const _deletefiles:boolean):boolean; // this method delete's the packages located in ($DELPHI)\Projects\Bpl and removes the key's from the registery.
function  CleanupByRegistry(const _ROOTKEY:DWORD;const _DelphiSubKey:string;const _DelphiVersion:integer):boolean; // find registry-entries without the packages
function  CheckDirectory(const _name:string):boolean; // check if the directory exists. if not then ask the user and create it.
function  ReadLibraryPath(const _DelphiVersion:integer;var DelphiLibraryPath:TDelphiLibraryPath):boolean; //read the library setting from the registry.
function  ExtractFilenamesFromDCC32Output(const _BasePath:string;const _CompilerOutput:TStrings):THashedStringList; // extract filenames from the dcc32.exe output.
function  WritePackageFile(const _filename:string;const _LibSuffix:string;const _silent:boolean):boolean;
function  WriteDPKFile(const _filename:string;const _LibSuffix:string;const _silent:boolean):boolean;  // write libsuffix into the dpk-file.
function  WriteDprojFile(const _filename:string;const _LibSuffix:string;const _silent:boolean):boolean;  // write libsuffix into the dproj-file.
function  DeleteFile(const _Filename:String):boolean;  // delete the file <_filename>.
function  RemoveProjectFromProjectGroup(const _GroupFilename,_ProjectFilename:string;const _ProjectType:TProjectType):boolean;
function  ReadBDSCommonDir(const _DelphiVersion:integer):string; // reads the path to the BDSCOMMONDIR.
function  ReadBDSProjectsDir(const _DelphiVersion:integer):string; // reads the path to the BDSPROJECTSDIR.
function  ReadBDSUserDir(const _DelphiVersion:integer):string; // reads the path to the BDSUSERDIR.
function  isIDEInstalled(const _Version:Integer):boolean; // find out if the ide version e.g. 11.0 is installed.
function  OldestIDEVersion:integer; // returns the VersionNo of the oldest IDE installed.
function  LatestIDEVersion:integer; // returns the VersionNo of the newest IDE installed.
function  GetIDERootKey(const _version:integer;var RootKey:string):boolean;
function  RemoveDoublePathEntries(_Path:string):string; // verify the string <_Path> and remove double entries.
function  MakeAbsolutePath(_basePath,_path:string;_DelphiVersion:integer):string; // <_path> is a semicolon seperated path-list which will be converted in to absolut path-list.

var
FCreateBatchFile:boolean;

implementation

uses uDPTMisc,
     SysUtils,
     Messages,
     StrUtils,
     TlHelp32,
     Forms,
     XMLDoc,
     XMLIntf,
     Controls,
     Dialogs,
     ShellApi,
     uDPTXMLReader,
     uDPTDefinitions,
     uDPTJclFuncs,
     MSXML_TLB;


var
FBatchFile:TStrings;
FBatchFilename:string;

{-----------------------------------------------------------------------------
  Procedure: RemoveDoublePathEntries
  Author:    herzogs2
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
      _item:=lowercase(GetField(';',_path));
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
_finstalledVersion:extended;
_InstalledIDEVersions:TStrings;
begin
  result:=High(integer);
  _InstalledIDEVersions:=TStringList.create;
  try
    GetInstalledIDEVersions(_InstalledIDEVersions);
    for i:=0 to _InstalledIDEVersions.count-1 do begin
      if not StringToFloat(_InstalledIDEVersions[i],_finstalledVersion) then continue;
      _installedVersion:=trunc(_finstalledVersion);
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
_finstalledVersion:extended;
_InstalledIDEVersions:TStrings;
begin
  result:=0;
  _InstalledIDEVersions:=TStringList.create;
  try
    GetInstalledIDEVersions(_InstalledIDEVersions);
    for i:=0 to _InstalledIDEVersions.count-1 do begin
      if not StringToFloat(_InstalledIDEVersions[i],_finstalledVersion) then continue;
      _installedVersion:=trunc(_finstalledVersion);
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
_finstalledVersion:extended;
_InstalledIDEVersions:TStrings;
begin
  result:=false;
  _InstalledIDEVersions:=TStringList.create;
  try
    GetInstalledIDEVersions(_InstalledIDEVersions);
    for i:=0 to _InstalledIDEVersions.count-1 do begin
      if not StringToFloat(_InstalledIDEVersions[i],_finstalledVersion) then continue;
      _installedVersion:=trunc(_finstalledVersion);
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
    //RAD Studio 2009, 2010
    Result := GetSystemPath(spCommonDocs) + cRADStudioDirName + PathDelim + DelphiVersions[_DelphiVersion].IDEVersionStr;
  end
  else begin
    Result := GetEnvironmentVariable(Copy(cBDSCommonDirTag, 3, length(cBDSCommonDirTag)-3));
  end;
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
  LocStr := LoadResStrings(GetDelphiRootDir(_DelphiVersion) + 'Bin\coreide' + DelphiVersions[_DelphiVersion].CoreIdeVersion + '.',['Borland Studio Projects', 'RAD Studio', 'Projects']);

  if DelphiVersions[_DelphiVersion].IDEVersion < 5 then begin
    Result := LocStr[0];
  end
  else begin
    Result := LocStr[1] + PathDelim + LocStr[2];
  end;

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
  showmessage('TODO: Deletion of a project from a .groupproj file is not implemented yet.');
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
  Description:
-----------------------------------------------------------------------------}
function  VerifyRegistry(const _DelphiVersion:integer):boolean; // scan through the registry items of "Known Packages" and "Disabled Packages" and check if the referenced files really exists. If not then remove the registry key.
begin
  result:=false;
  if CleanupByRegistry(HKEY_CURRENT_USER, 'Known Packages'   ,_DelphiVersion) then result:=true;
  if CleanupByRegistry(HKEY_LOCAL_MACHINE,'Known Packages'   ,_DelphiVersion) then result:=true;
  if CleanupByRegistry(HKEY_CURRENT_USER, 'Disabled Packages',_DelphiVersion) then result:=true;
  if CleanupByRegistry(HKEY_LOCAL_MACHINE,'Disabled Packages',_DelphiVersion) then result:=true;
end;

{-----------------------------------------------------------------------------
  Procedure: FindLine
  Author:    herzogs2
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
  removedText:='';
  result:=-1;
  for i:=0 to content.Count-1 do begin
    _text:=trim(content[i]);
    if pos(_Tag,_text)=1 then begin
      removedText:=_text;
      result:=i;
      break;
    end;
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
function  WritePackageFile(const _filename:string;const _LibSuffix:string;const _silent:boolean):boolean;
begin
  result:=false;
  if lowercase(ExtractFileExt(_filename))='.dpk'   then result:=WriteDPKFile(_filename,_libsuffix,_silent) else
  if lowercase(ExtractFileExt(_filename))='.dproj' then result:=WriteDprojFile(_filename,_libsuffix,_silent);
end;

{-----------------------------------------------------------------------------
  Procedure: WriteDPKFile
  Author:    sam
  Date:      08-Nov-2007
  Arguments: const _filename:string;const _LibSuffix:string
  Result:    boolean
  Description: write libsuffix into dpk-file.
-----------------------------------------------------------------------------}
function  WriteDPKFile(const _filename:string;const _LibSuffix:string;const _silent:boolean):boolean;
resourcestring
cAskToReplaceLibSuffix='Do you want to replace <%s> with <%s>?';
var
_File:TStrings;
_index:integer;
_OldText:string;
_NewText:string;
_FileChanged:boolean;
begin
  result:=false;
  _FileChanged:=false;
  if not fileexists(_filename) then begin
    trace(1,'Problem in WriteDPKFile: Could not find the file <%s>. Nothing to do.',[_filename]);
    exit;
  end;
  if not RemoveReadOnlyFlag(_filename,_silent) then exit;
  _File:=TStringList.create;
  try
    _File.LoadFromFile(_filename);
    _index:=FindLine(_File,'{$LIBSUFFIX',_OldText);
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
    if _FileChanged then begin
      if BackupFile(_filename,'.dof_old','',false) then _File.SaveToFile(_filename);
    end;
    result:=true;
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
function  WriteDProjFile(const _filename:string;const _LibSuffix:string;const _silent:boolean):boolean;
resourcestring
cAskToReplaceLibSuffix='Do you want to replace <%s> with <%s>?';
var
_File:TStrings;
_index:integer;
_OldText:string;
_NewText:string;
_FileChanged:boolean;
_LibSuffixAlreadyInFile:boolean;
begin
  result:=false;
  _FileChanged:=false;
  _LibSuffixAlreadyInFile:=false;
  if not fileexists(_filename) then begin
    trace(1,'Problem in WriteDprojFile: Could not find the file <%s>. Nothing to do.',[_filename]);
    exit;
  end;
  if not RemoveReadOnlyFlag(_filename,_silent) then exit;
  _File:=TStringList.create;
  try
    _File.LoadFromFile(_filename);
    _index:=FindLine(_File,'<Package_Options Name="LibSuffix">',_OldText);
    if _index=-1 then begin
      _index:=FindLine(_File,'</Package_Options>',_OldText);
      if _index=-1 then begin
        trace(1,'Error in WriteDProjFile: Could not find section <Package_Options> in file <%s>.',[_filename]);
        exit;
      end;
    end else _LibSuffixAlreadyInFile:=true;
    _NewText:=format('       <Package_Options Name="LibSuffix">%s</Package_Options>',[_LibSuffix]);
    if trim(_NewText)<>trim(_OldText) then begin
      if not _silent then begin
        if Application.MessageBox(pchar(format(cAskToReplaceLibSuffix,[_OldText,_NewText])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDNo then exit;
      end;
      if _LibSuffixAlreadyInFile then _File.Delete(_index);
      _File.Insert(_index,_NewText);
      _FileChanged:=true;
      trace(3,'Succsessfully written <%s> into file <%s>.',[_NewText,_filename]);
    end;
    if _FileChanged then begin
      if BackupFile(_filename,'.dproj_old','',false) then _File.SaveToFile(_filename);
    end;
    result:=true;
  finally
    _File.free;
  end;
end;


{-----------------------------------------------------------------------------
  Procedure: ExtractFilenamesFromDCC32Output
  Author:    herzogs2
  Date:      16-Okt-2007
  Arguments: const _BasePath:string;const _CompilerOutput:TStringList
  Result:    TStringList
  Description: extract filenames from the dcc32.exe output.
-----------------------------------------------------------------------------}
function  ExtractFilenamesFromDCC32Output(const _BasePath:string;const _CompilerOutput:TStrings):THashedStringList;
var
i,j,k,l:integer;
_ExtensionsOfInterest:TStringList;
_line:string;
_ext:string;
_filename,_lastfilename:string;
_pos:integer;
_files:THashedStringList;
_workPath:string;
begin
  result:=THashedStringList.create;
  _files:=THashedStringList.create;
  _ExtensionsOfInterest:=TStringList.create;
  Screen.cursor:=crHourGlass;
  try
    _ExtensionsOfInterest.add('.pas');
    _ExtensionsOfInterest.add('.dfm');
    _ExtensionsOfInterest.add('.dcu');
    _ExtensionsOfInterest.add('.obj');
    _ExtensionsOfInterest.add('.dpk');
    _ExtensionsOfInterest.add('.inc');
    _ExtensionsOfInterest.add('.dcr');
    _ExtensionsOfInterest.add('.drc');
    _ExtensionsOfInterest.add('.res');
    _ExtensionsOfInterest.add('.dpr');
    _ExtensionsOfInterest.add('.bpg');
    _ExtensionsOfInterest.add('.tlb');
    _ExtensionsOfInterest.add('.ocx');
    _ExtensionsOfInterest.add('.dll');
    _ExtensionsOfInterest.add('.txt');
    _ExtensionsOfInterest.add('.ini');
    _ExtensionsOfInterest.add('.cfg');
    _ExtensionsOfInterest.add('.dof');
    _ExtensionsOfInterest.add('.xml');
    _ExtensionsOfInterest.add('.bcc_obj');
    _ExtensionsOfInterest.add('.obj');
    _ExtensionsOfInterest.add('.zobj');
    _ExtensionsOfInterest.add('.rc');
    _ExtensionsOfInterest.add('.ico');
    _ExtensionsOfInterest.add('.dproj');
    _ExtensionsOfInterest.add('.bdsproj');
    _ExtensionsOfInterest.add('.bdsgroup');
    _ExtensionsOfInterest.add('.groupproj');
    for i:=0 to _CompilerOutput.Count-1 do begin
      _line:=lowercase(_CompilerOutput[i]);
      _ext:=ExtractFileExt(_line);
      if length(_ext)<4 then continue;
      _ext:=GetField('(',_ext);
      if _ext='' then continue;
      for j:=0 to _ExtensionsOfInterest.count-1 do begin
        if _ext<>_ExtensionsOfInterest[j] then continue;
         _pos:=pos('(',_line);
        _lastfilename:=_filename;
        if _pos>0 then _filename:=copy(_line,1,_pos-1)
                  else _filename:=_line;
        _filename:=AbsoluteFilename(_BasePath,_filename);
        if _filename=_lastfilename then break;
        if result.indexof(_filename)<0 then begin
          result.add(_filename);
          trace(5,'Added file <%s> to backup list.',[_filename]);
        end;
        for k:=0 to _ExtensionsOfInterest.count-1 do begin
          _files.Clear;
          _workPath:=extractFilePath(_filename);
          trace(5,'Searching for files in path <%s> for files <%s>.',[_workPath,'*'+_ExtensionsOfInterest[k]]);
          AllFilesOfPath(_workPath,'*'+_ExtensionsOfInterest[k],_files);
          Application.ProcessMessages;
          for l:=0 to _files.count-1 do begin
            _filename:=_workPath+_files[l];
            if result.indexof(_filename)<0 then begin
              result.add(_filename);
              trace(5,'Added file <%s> to backup list.',[_filename]);
            end;
          end;
        end;
        break;
      end;
    end;
  finally
    _ExtensionsOfInterest.free;
    _files.free;
    Screen.cursor:=crDefault;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: RelativePaths
  Author:    sam
  Date:      09-Jun-2007
  Arguments: _basepath,_paths:string
  Result:    string
  Description: turns a collection of path's which are seperated by ';' into relative paths.
               Inexistent Paths will be removed.
               Doubled items will be removed.
-----------------------------------------------------------------------------}
function  RelativePaths(_basepath,_paths:string;const _DelphiVersion:integer):string; // converts a list of paths to relative paths.
var
_path:string;
_absolutepath:string;
begin
  _paths:=lowercase(_paths);
  _path:=Getfield(';',_paths);
  while _path<>'' do begin
    _absolutepath:=AbsolutePath(_basepath,_path,_DelphiVersion);
    if DirectoryExists(_absolutepath) then begin
      _path:=RelativePath(_basepath,_path,_DelphiVersion);
      if (pos(_path+';',result)=0) and
         (pos(_path+'\;',result)=0) then result:=result+_path+';';
    end;
    _path:=Getfield(';',_paths);
  end;
end;


{-----------------------------------------------------------------------------
  Procedure: WriteDOFFile
  Author:    herzogs2
  Date:      15-Jun-2007
  Arguments: _bpgPath,_dofFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath:string;const _silent:boolean
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function  WriteDOFFile(_bpgPath,_dofFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath:string;const _silent:boolean;const _DelphiVersion:integer):boolean; // write info to the dof-file.
resourcestring
cAskToReplace='Do you want to replace <%s> with <%s>?';
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
    if _silent or
       (not _silent and
        (Application.MessageBox(pchar(format(cAskToReplace,[_OldText,_NewText])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDYes)) then begin
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
  end;

begin
  Result:=false;
  _FileChanged:=false;
  if not fileExists(_dofFilename) then begin
    trace(2,'Problem in WriteDOFFile: Could not find the file <%s>.',[_dofFilename]);
    exit;
  end;
  if not RemoveReadOnlyFlag(_dofFilename,_silent) then exit;

// make all paths relative
  _searchPath       :=RelativePaths(ExtractFilePath(_dofFilename),_searchPath,_DelphiVersion);
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

    if _FileChanged then begin
      if BackupFile(_dofFilename,'.dof_old','',false) then _DOFFile.SaveToFile(_dofFilename);
    end;
    result:=true;
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
function GetIDERootKey(const _version:integer;var RootKey:string):boolean;
var
_sDelphiVersion:string;
begin
  trace(5,'Enter GetIDERootKey with value <%d>.',[_version]);
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
  end else begin           // for delphi 2008 (aka bds 6.0) and later
    if _version<7 then _sDelphiVersion:=inttostr(_version-6)+'.0\'
                  else _sDelphiVersion:=inttostr(_version-7)+'.0\';
    RootKey:=cCodeGearBDSKey+'\'+_sDelphiVersion;
    result:=true;
  end;
  trace(5,'Leave GetIDERootKey with value <%s>.',[RootKey]);
end;

{-----------------------------------------------------------------------------
  Procedure: ReadLibraryPath
  Author:    HerzogS2
  Date:      09-Mrz-2007
  Arguments: _DelphiVersion:integer;var DelphiLibraryPath:TDelphiLibraryPath
  Result:    boolean
  Description: read the library setting from the registry.
-----------------------------------------------------------------------------}
function  ReadLibraryPath(const _DelphiVersion:integer;var DelphiLibraryPath:TDelphiLibraryPath):boolean;
var
_Reg:TRegistry;
_Key:string;
begin
  result:=false;
  if not GetIDERootKey(_DelphiVersion,_Key) then begin
    trace(3,'Problem in ReadLibraryPath: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;
  _Key:=_Key+'library\';
  _Reg := TRegistry.Create;
  try
    _Reg.RootKey := HKEY_LOCAL_MACHINE;
    if not _Reg.OpenKeyReadOnly(_Key) then begin
      trace(1,'Problem in ReadLibraryPath: The Key <%s> could not be opened in the registry.',[_Key]);
      exit;
    end;
    try
      DelphiLibraryPath.BrowsingPath:=_Reg.ReadString('Browsing Path');
      DelphiLibraryPath.DebugDCUpath:=_Reg.ReadString('Debug DCU Path');
      DelphiLibraryPath.DCPpath     :=_Reg.ReadString('Package DCP Output');
      DelphiLibraryPath.BPLpath     :=_Reg.ReadString('Package DPL Output');
      DelphiLibraryPath.PackagePath :=_Reg.ReadString('Package Search Path');
      DelphiLibraryPath.Searchpath  :=_Reg.ReadString('Search Path');
    except
      trace(1,'Warning in ReadLibraryPath: Could not read Library-Settings for delphi version <%s>.You need to have Admin rights for this computer.',[_DelphiVersion]);
    end;
    _Reg.CloseKey;
  finally
    _Reg.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: GetDelphiPathTag
  Author:    HerzogS2
  Date:      08-Mrz-2007
  Arguments: const _version:integer
  Result:    string
  Description: returns $(DELPHI) or $(BDS) according to the version number
-----------------------------------------------------------------------------}
function  GetDelphiPathTag(const _version:integer):string;
begin
  if _version<=7 then result:=cDelphiTag
                 else result:=cBDSTag;
end;

{-----------------------------------------------------------------------------
  Procedure: CheckDirectory
  Author:    HerzogS2
  Date:      08-Mrz-2007
  Arguments: const _name:string
  Result:    boolean
  Description: check if the directory with name <_name> exists. If not then
  it asks if the directory shall be created.
-----------------------------------------------------------------------------}
function  CheckDirectory(const _name:string):boolean;
resourcestring
cAskToCreateFolder='Could not find the directory <%s>. Create it ?';
begin
  result:=false;
  if _name='' then begin
    result:=true;
    exit;
  end;
  if DirectoryExists(_name) then begin
    result:=true;
    exit;
  end;
  if Application.MessageBox(pchar(format(cAskToCreateFolder,[_name])),pchar(cConfirm),MB_ICONQUESTION or MB_YESNO)=IDNo then exit;
  try
    if not ForceDirectories(_name) then begin
      trace(1,'Problem to create directory <%s>. Please check settings and user rights.',[_name]);
      exit;
    end;
    result:=true;
  except
    on e:exception do trace(1,'Problem to create directory <%s>. Please check settings and user rights. <%s>.',[_name,e.message]);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: CleanupByRegistry
  Author:    sam
  Date:      23-Okt-2006
  Arguments: const _DelphiVersion:integer
  Result:    boolean
  Description: iterate through the known packages and check if the referenced
               file really exists. if not then delete the registry entry.
-----------------------------------------------------------------------------}
function  CleanupByRegistry(const _ROOTKEY:DWORD;const _DelphiSubKey:string;const _DelphiVersion:integer):boolean; // find registry-entries without the packages
var
i:integer;
_DelphiRootDirKey:string;
_Reg: TRegistry;
_ValueNames:TStrings;
_packageName:string;
begin
  result:=false;
  if not GetIDERootKey(_DelphiVersion,_DelphiRootDirKey) then begin
    trace(3,'Problem in CleanupByRegistry: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;
  _DelphiRootDirKey:=_DelphiRootDirKey+_DelphiSubKey;
  _Reg := TRegistry.Create;
  _ValueNames:=TStringList.create;
  try
    _Reg.RootKey := _ROOTKEY;
    if not _Reg.OpenKey(_DelphiRootDirKey,false) then begin
      trace(5,'CleanupByRegistry: The Key <%s> was not found in the registry.',[_DelphiRootDirKey]);
      exit;
    end;
    _Reg.GetValueNames(_ValueNames);
    try
      for i:=0 to _ValueNames.count-1 do begin
        _packageName:=lowercase(ReplaceTag(_ValueNames[i],_DelphiVersion));
        if fileexists(_packageName) then continue;
        if not _Reg.DeleteValue(_ValueNames[i]) then begin
          trace(3,'Problem in CleanupByRegistry: Could not remove key <%s> from registry for delphi <%d>.',[_DelphiRootDirKey+_ValueNames[i],_DelphiVersion]);
          continue;
        end
        else begin
          result:=true;
          trace(3,'Removed key <%s> from registry because the referenced file <%s> does not exist.',[_DelphiRootDirKey+_ValueNames[i],_packageName]);
        end;
      end;
    except
      on e:exception do begin
        trace(1,'Problem in CleanupByRegistry: Could not remove registry key <%s>.<%s>.',[_packageName,e.Message]);
      end;
    end;
  finally
    _Reg.CloseKey;
    _reg.free;
    _ValueNames.free;
  end;
end;


{-----------------------------------------------------------------------------
  Procedure: RemoveValueFromRegistry
  Author:    herzogs2
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
  _Reg := TRegistry.Create;
  try
    _Reg.RootKey := _RootKey;
    _Reg.OpenKey(_Key,false);
    if not _Reg.ValueExists(_PackageName) then begin
      trace(5,'RemoveValueFromRegistry: Could not find Key Value <%s> in <%s>.',[_PackageName,_key]);
      exit;
    end;
    if not _Reg.DeleteValue(_PackageName) then begin
      trace(5,'RemoveValueFromRegistry: Could not delete Key Value <%s> in <%s>.',[_PackageName,_key]);
      exit;
    end;
    result:=true;
    trace(5,'RemoveValueFromRegistry: Successfully deleted Key Value <%s> from <%s>.',[_PackageName,_Key]);
  finally
    _Reg.CloseKey;
    _Reg.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: CleanUpPackagesByBPLPath
  Author:    sam
  Date:      05-Jul-2006
  Arguments: const _DelphiVersion:integerconst _DelphiBINPath:stringconst _deletefiles:boolean
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function CleanUpPackagesByBPLPath(const _DelphiVersion:integer;_BPLPath:string;const _deletefiles:boolean):boolean; // this method delete's the packages located in ($DELPHI)\Projects\Bpl and removes the key's from the registery.
var
i:integer;
_fileList:TStrings;
_PackageKey:string;
_filename:string;
begin
  result:=false;
  _BPLPath:=IncludeTrailingPathDelimiter(_BPLPath);
  if not GetIDERootKey(_DelphiVersion,_PackageKey) then begin
    trace(3,'Problem in CleanUpPackagesByBPLPath: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;
  _fileList:=TStringList.create;
  try
    AllFilesOfPath(_bplpath,'*.bpl',_fileList,true);
    for i:=0 to _filelist.count-1 do begin
      _filename:=_filelist[i];
      RemoveValueFromRegistry(HKEY_CURRENT_USER ,_PackageKey+'Known Packages',_bplpath+_filename);
      RemoveValueFromRegistry(HKEY_LOCAL_MACHINE,_PackageKey+'Known Packages',_bplpath+_filename);
      RemoveValueFromRegistry(HKEY_CURRENT_USER ,_PackageKey+'Disabled Packages',_bplpath+_filename);
      RemoveValueFromRegistry(HKEY_LOCAL_MACHINE,_PackageKey+'Disabled Packages',_bplpath+_filename);
      if _deletefiles then begin
        uDPTDelphiPackage.DeleteFile(_bplpath+_filename);
        _filename:=changefileext(_filename,'.dcp');
        uDPTDelphiPackage.DeleteFile(_bplpath+_filename);
      end;
    end;
    _fileList.clear;
    AllFilesOfPath(_bplpath,'*.dcp',_fileList,true);
    for i:=0 to _filelist.count-1 do begin
      _filename:=_filelist[i];
      if _deletefiles then begin
        uDPTDelphiPackage.DeleteFile(_bplpath+_filename);
        _filename:=changefileext(_filename,'.bpl');
        uDPTDelphiPackage.DeleteFile(_bplpath+_filename);
      end;
    end;
  finally
    _fileList.free;
  end;
  result:=VerifyRegistry(_DelphiVersion);
end;


{-----------------------------------------------------------------------------
  Procedure: CleanUpPackagesByRegistery
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
function CleanUpPackagesByRegistery(const _ROOTKEY:DWORD;const _DelphiVersion:integer;const _DelphiSubKey:string;const _DelphiBINPath:string;const _deletefiles:boolean):boolean; //
var
i:integer;
_DelphiRootDirKey:string;
_Reg: TRegistry;
_ValueNames:TStrings;
_packageName:string;
begin
  result:=false;
  if not GetIDERootKey(_DelphiVersion,_DelphiRootDirKey) then begin
    trace(3,'Problem in CleanUpPackagesByRegistery: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;

  _Reg := TRegistry.Create;
  _ValueNames:=TStringList.create;
  try
    _Reg.RootKey := _ROOTKEY;
    _DelphiRootDirKey:=_DelphiRootDirKey+_DelphiSubKey;
    if not _Reg.OpenKey(_DelphiRootDirKey,false) then begin
      trace(3,'Warning in CleanUpPackagesByRegistery: The Key <%s> was not found in the registry.',[_DelphiRootDirKey]);
      exit;
    end;
    _Reg.GetValueNames(_ValueNames);
    try
      for i:=0 to _ValueNames.count-1 do begin
        _packageName:=_ValueNames[i];
        _packageName:=ReplaceTag(_packageName,_DelphiVersion);
        if pos(lowercase(_DelphiBINPath),lowercase(_packageName))<>0 then continue;
        if not _Reg.DeleteValue(_packageName) then begin
          trace(3,'Problem in CleanUpPackagesByRegistery: Could not delete package <%s> for delphi <%d>.',[_packageName,_DelphiVersion]);
          continue;
        end;
        trace(5,'CleanUpPackagesByRegistery: Deleted Package <%s> for delphi version <%d> from registry.',[_packageName,_DelphiVersion]);
        if _deletefiles then begin
          if uDPTDelphiPackage.DeleteFile(_packageName) then trace(5,'CleanUpPackagesByRegistery: Deleted File <%s> for delphi version <%d>.',[_packageName,_DelphiVersion]);
          _packageName:=ChangeFileExt(_packageName,'.dcp');
          if uDPTDelphiPackage.DeleteFile(_packageName) then trace(5,'CleanUpPackagesByRegistery: Deleted File <%s> for delphi version <%d>.',[_packageName,_DelphiVersion]);
        end;
      end;
    except
      on e:exception do trace(1,'Warning in CleanUpPackagesByRegistery: Could not delete the key <%s> for delphi version <%s>.You need to have Admin rights for this computer.%s',[_DelphiRootDirKey,_DelphiVersion,e.message]);
    end;
    _Reg.CloseKey;
    result:=true;
  finally
    _ValueNames.free;
    _Reg.Free;
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
function VersionNoToIDEName(const _version:integer):string;
begin
  result:='unknown version';
  case _version of
    1,2,3,4,5,6,7:result:=format('Borland Delphi %d',[_version]);
                8:result:='Borland Delphi 8 .NET';
                9:result:='Borland Delphi 2005';
               10:result:='Borland Developer Studio 2006/Turbo Delphi';
               11:result:='CodeGear Developer Studio 2007/CodeGear Delphi 2007 for Win32';
               12:result:='Delphi 2009';
               14:result:='Embarcadero RAD Studio 2010';
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
_fVersion:extended;
begin
  result:=0;
  _version:=uppercase(trim(_version));
  if Pos('BDS',_version)=1 then begin
    delete(_version,1,3);
    _version:=trim(_version);
    if not StringToFloat(_version,_fVersion) then exit;
    result:=trunc(_fVersion);
    if result<7 then result:=result+6   //BDS1.0-BDS6.0
                else result:=result+7;  //BDS7.0....
  end
  else begin
    if Pos('D',_version)=1 then delete(_version,1,1);
    if not StringToFloat(_version,_fVersion) then exit;
    result:=trunc(_fVersion); //D1-D7
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: CreateBPGFile
  Author:    herzogs2
  Date:      04-Jun-2008
  Arguments: const _lstProjectFiles:TListBox;const _bpgFilename:string
  Result:    None
  Description: create a bpg-file with all the projects in the list <_lstProjectFiles>.
-----------------------------------------------------------------------------}
procedure CreateBPGFile(const _lstProjectFiles:TListBox;const _projectGroupFilename:string;const _DelphiVersion:integer);
resourcestring
cDoFreshInstall='Could not find the file <%s>. Download the latest <setup.exe> of Delphi Package Tool and do fresh install.';
const
  cBPGTemplateFilename='ProjectGroupTemplate.bpg'; // this file must be in the application directory. the file contains the header part of a bpg-file.
var
i:integer;
_Files:TStrings;
_line:string;
_Projects:string;
_filename:string;                                                                                
_ProjectFilename:string;
_ProjectType:TProjectType;
begin
  if not FileExists(ExtractFilePath(Application.exename)+cBPGTemplateFilename) then begin
    trace(5,'Could not find the file <%s>. Make sure it is in the application directory.',[ExtractFilePath(Application.exename)+cBPGTemplateFilename]);
    Application.MessageBox(Pchar(format(cDoFreshInstall,[ExtractFilePath(Application.exename)+cBPGTemplateFilename])),pchar(cError),MB_ICONERROR or MB_OK);
    exit;
  end;
  if not RemoveReadOnlyFlag(_projectGroupFilename,false) then exit;

  _Files:=TStringList.create;
  try
    _Files.loadfromfile(extractFilePath(Application.exename)+cBPGTemplateFilename);
    _Projects:='PROJECTS =';
    for i:=0 to _lstProjectFiles.Items.count-1 do begin
      _ProjectFilename:=AbsoluteFilename(ExtractFilePath(_projectGroupFilename),_lstProjectFiles.Items[i]);
      _ProjectType:=DetermProjectType(_ProjectFilename,_projectGroupFilename,_DelphiVersion);
      _Projects:=_Projects+OutputFilename(_ProjectFilename,_ProjectType,DelphiVersions[_DelphiVersion].ShortName);
      if (length(_Projects)-LastPos(_Projects,'\'))>80 then _Projects:=_Projects+' \'+#$D+#$A+'  '
      else _Projects:=_Projects+' ';
    end;
    _Files.Add(_Projects);
    _Files.Add('#------------------------------------------------------------------------------');
    _Files.Add('default: $(PROJECTS)                                                           ');
    _Files.Add('#------------------------------------------------------------------------------');
    _Files.Add('');
    for i:=0 to _lstProjectFiles.Items.count-1 do begin
      _ProjectFilename:=AbsoluteFilename(ExtractFilePath(_projectGroupFilename),_lstProjectFiles.Items[i]);
      _ProjectType:=DetermProjectType(_ProjectFilename,_projectGroupFilename,_DelphiVersion);

      _filename:=OutputFilename(_ProjectFilename,_ProjectType,DelphiVersions[_DelphiVersion].ShortName);
      _line:=ExtractFilename(_filename)+': '+ExtractRelativePath(ExtractFilePath(_projectGroupFilename),_lstProjectFiles.Items[i]);
      if _files.IndexOf(_line)=-1 then begin // avoid doubles.
        _Files.Add(_line);
        _Files.Add('  $(DCC)');
        _Files.Add('');
      end;
    end;
    if RenameFile(_projectGroupFilename,changeFileExt(_projectGroupFilename,'.~old')) then begin
      trace(1,'Renamed the file <%s> to <%s>.',[_projectGroupFilename,changeFileExt(_projectGroupFilename,'.~old')]);
    end;
    _Files.SaveToFile(_projectGroupFilename);
    trace(1,'Saved ProjectGroup-File <%s>.',[_projectGroupFilename]);
  finally
    _Files.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: CreateBDSGroup
  Author:    herzogs2
  Date:      04-Jun-2008
  Arguments: const _lstProjectFiles:TListBox;const _bpgFilename:string
  Result:    None
  Description: create a bdsgroup-file with all the projects in the list <_lstProjectFiles>.
-----------------------------------------------------------------------------}
procedure CreateBDSGroup(const _lstProjectFiles:TListBox;const _projectGroupFilename:string;const _DelphiVersion:integer);
resourcestring
cDoFreshInstall='Could not find the file <%s>. Download the latest <setup.exe> of Delphi Package Tool and do fresh install.';
cCouldNotCopy='Could not copy file <%s> to <%s>. Check User-Rights.';
const
  cBDSGroupTemplateFilename='ProjectGroupTemplate.bdsgroup';
var
i,j:integer;
_file:TStrings;
_line:string;
_BeginProjectSection:integer;
_filename:string;
_lineToInsert:string;
_TargetsLine:string;
_description:string;
_packagefilename:string;
_templateFilename:string;
_projectType:TProjectType;
_libsuffix:string;
begin
  if not fileexists(_projectGroupFilename) then begin // if the project file does not exists, then create it from the template.
  _templateFilename:=ExtractFilePath(Application.exename)+cBDSGroupTemplateFilename;
  if not FileExists(_templateFilename) then begin
    trace(5,'Could not find the file <%s>. Make sure it is in the application directory.',[_templateFilename]);
    Application.MessageBox(pchar(format(cDoFreshInstall,[_templateFilename])),pchar(cError),MB_ICONERROR or MB_OK);
    exit;
  end;
  if not CopyFile(pchar(_templateFilename),pchar(_projectGroupFilename),false) then begin
    trace(2,'Problem in CreateBDSGroup: Could not copy file <%s> to <%s>. Check User-Rights.',[_templateFilename,_projectGroupFilename]);
    Application.MessageBox(pchar(format(cCouldNotCopy,[_templateFilename,_projectGroupFilename])),pchar(cError),MB_ICONERROR or MB_OK);
    exit;
  end;
  if not RemoveReadOnlyFlag(_projectGroupFilename,false) then exit;
  end;
  _file:=TStringList.Create;
  try
    _file.LoadFromFile(_projectGroupFilename);
// find the begin of the project section
    _BeginProjectSection:=0;
    for i:=0 to _file.Count-1 do begin
      _line:=_file[i];
      inc(_BeginProjectSection);
      if Pos('<Projects>',_line)=0 then continue;
      break;
    end;
// replace projects in the file with the projects from the list <_listProjectFiles>.
    j:=_BeginProjectSection;
    _TargetsLine:='      <Projects Name="Targets">';
    for i:=0 to _lstProjectFiles.Count-1 do begin
      _filename:=_lstProjectFiles.Items[i];
      _packagefilename:=AbsoluteFilename(extractFilepath(_projectGroupFilename),_filename);
      _projectType:=DetermProjectType(_packagefilename,'',_DelphiVersion);
      case _projectType of
        tp_unkown,  // defines compiler output
        tp_dll:begin // project is a dll
                 _lineToInsert:=format('      <Projects Name="%s">%s</Projects>',[extractfilenameonly(_filename)+'.dll',ExtractRelativePath(ExtractFilePath(_projectGroupFilename),_lstProjectFiles.Items[i])]);
                 if _file.IndexOf(_lineToInsert)=-1 then begin
                   _file.Insert(j,_lineToInsert);
                   _TargetsLine:=_TargetsLine+extractfilenameonly(_filename)+'.dll'+' ';
                 end;
               end;
        tp_exe:begin // project is a executable
                 _lineToInsert:=format('      <Projects Name="%s">%s</Projects>',[extractfilenameonly(_filename)+'.exe',ExtractRelativePath(ExtractFilePath(_projectGroupFilename),_lstProjectFiles.Items[i])]);
                 if _file.IndexOf(_lineToInsert)=-1 then begin
                   _file.Insert(j,_lineToInsert);
                   _TargetsLine:=_TargetsLine+extractfilenameonly(_filename)+'.exe'+' ';
                 end;
               end;
        tp_bpl:begin  // project is a package
                 ReadpackageInfo(_packagefilename,_description,_libsuffix);
                 _lineToInsert:=format('      <Projects Name="%s">%s</Projects>',[extractfilenameonly(_filename)+DelphiVersions[_DelphiVersion].ShortName+'.bpl',ExtractRelativePath(ExtractFilePath(_projectGroupFilename),_lstProjectFiles.Items[i])]);
                 if _file.IndexOf(_lineToInsert)=-1 then begin
                   _file.Insert(j,_lineToInsert);
                   _TargetsLine:=_TargetsLine+extractfilenameonly(_filename)+DelphiVersions[_DelphiVersion].ShortName+'.bpl'+' ';
                 end;
               end;
      end;
      inc(j);
    end;
    _TargetsLine:=_TargetsLine+'</Projects>';
    _file.Insert(j,_TargetsLine);
    if RenameFile(_projectGroupFilename,changeFileExt(_projectGroupFilename,'.~old')) then begin
      trace(1,'Renamed the file <%s> to <%s>.',[_projectGroupFilename,changeFileExt(_projectGroupFilename,'.~old')]);
    end;
    _File.SaveToFile(_projectGroupFilename);
    trace(1,'Saved ProjectGroup-File <%s>.',[_projectGroupFilename]);
  finally
    _File.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: CreateGroupProj
  Author:    herzogs2
  Date:      21-Jan-2009
  Arguments: const _lstProjectFiles:TListBox;const _projectGroupFilename:string;const _DelphiVersion:integer
  Result:    None
  Description: create a group file as introduced in D2007.
-----------------------------------------------------------------------------}
procedure CreateGroupProj(const _lstProjectFiles:TListBox;const _projectGroupFilename:string;const _DelphiVersion:integer);
resourcestring
cDoFreshInstall='Could not find the file <%s>. Download the latest <setup.exe> of Delphi Package Tool and do fresh install.';
cCouldNotCopy='Could not copy file <%s> to <%s>. Check User-Rights.';
const
  cBDSGroupTemplateFilename='ProjectGroupTemplate.groupproj';
var
i,j:integer;
_file:TStrings;
_BeginProjectSection:integer;
_EndProjectSection:integer;
_BeginTargetSection:integer;
_EndTargetSection:integer;
_filename:string;
_lineToInsert:string;
_templateFilename:string;
_buildsection:string;
_makesection:string;
_cleansection:string;
_text:string;
begin
  if not fileexists(_projectGroupFilename) then begin // if the project file does not exists, then create it from the template.
    _templateFilename:=ExtractFilePath(Application.exename)+cBDSGroupTemplateFilename;
    if not FileExists(_templateFilename) then begin
      trace(5,'Could not find the file <%s>. Make sure it is in the application directory.',[_templateFilename]);
      Application.MessageBox(pchar(format(cDoFreshInstall,[_templateFilename])),pchar(cError),MB_ICONERROR or MB_OK);
      exit;
    end;
    if not CopyFile(pchar(_templateFilename),pchar(_projectGroupFilename),false) then begin
      trace(2,'Problem in CreateGroupProj: Could not copy file <%s> to <%s>. Check User-Rights.',[_templateFilename,_projectGroupFilename]);
      Application.MessageBox(pchar(format(cCouldNotCopy,[_templateFilename,_projectGroupFilename])),pchar(cError),MB_ICONERROR or MB_OK);
      exit;
    end;
    if not RemoveReadOnlyFlag(_projectGroupFilename,false) then exit;
  end;
  _file:=TStringList.Create;
  try
    _file.LoadFromFile(_projectGroupFilename);
// find the begin of the project section
    _BeginProjectSection:=FindLine(_file,'<ItemGroup>',_text);
    if _BeginProjectSection=-1 then begin
      trace(2,'Problem in CreateGroupProj: Could not find the begin of project section in file <%s>. Something wrong here.',[_projectGroupFilename]);
      exit;
    end;
    _EndProjectSection  :=FindLine(_file,'</ItemGroup>',_text);
    if _EndProjectSection=-1 then begin
      trace(2,'Problem in CreateGroupProj: Could not find the end of project section in file <%s>. Something wrong here.',[_projectGroupFilename]);
      exit;
    end;
// remove the existing lines.
    for j:=_BeginProjectSection+1 to  _EndProjectSection-1 do _file.delete(_BeginProjectSection+1);
// now add the projects from the list <_listProjectFiles>.
    j:=_BeginProjectSection+1;
    for i:=0 to _lstProjectFiles.Count-1 do begin
      _filename:=_lstProjectFiles.Items[i];
      _lineToInsert:=format('    <Projects Include="%s" />',[ExtractRelativePath(ExtractFilePath(_projectGroupFilename),_lstProjectFiles.Items[i])]);
      _file.insert(j,_lineToInsert);
      inc(j);
    end;
    _BeginTargetSection:=FindLine(_file,'</ProjectExtensions>',_text);
    if _BeginTargetSection=-1 then begin
      trace(2,'Problem in CreateGroupProj: Could not find the begin of target section in file <%s>. Something wrong here.',[_projectGroupFilename]);
      exit;
    end;
    _EndTargetSection:=FindLine(_file,'<Import Condition="',_text);
    if _EndTargetSection=-1 then begin
      trace(2,'Problem in CreateGroupProj: Could not find the end of target section in file <%s>. Something wrong here.',[_projectGroupFilename]);
      exit;
    end;
// remove the existing lines.
    for j:=_BeginTargetSection+1 to  _EndTargetSection-1 do _file.delete(_BeginTargetSection+1);
// recreate the target section        
    j:=_BeginTargetSection+1;
    for i:=0 to _lstProjectFiles.Count-1 do begin
      _filename:=_lstProjectFiles.Items[i];
// build
      _lineToInsert:=format('  <Target Name="%s">',[ExtractFileNameOnly(_filename)]);
      _file.insert(j,_lineToInsert);
      inc(j);
      _lineToInsert:=format('    <MSBuild Projects="%s" Targets="" />',[ExtractRelativePath(ExtractFilePath(_projectGroupFilename),_lstProjectFiles.Items[i])]);
      _file.insert(j,_lineToInsert);
      inc(j);
      _lineToInsert:=format('  </Target>',[]);
      _file.insert(j,_lineToInsert);
      inc(j);
      _buildsection:=_buildsection+ExtractFileNameOnly(_filename)+';';
// clean
      _lineToInsert:=format('  <Target Name="%s:Clean">',[ExtractFileNameOnly(_filename)]);
      _file.insert(j,_lineToInsert);
      inc(j);
      _lineToInsert:=format('    <MSBuild Projects="%s" Targets="Clean" />',[ExtractRelativePath(ExtractFilePath(_projectGroupFilename),_lstProjectFiles.Items[i])]);
      _file.insert(j,_lineToInsert);
      inc(j);
      _lineToInsert:=format('  </Target>',[]);
      _file.insert(j,_lineToInsert);
      inc(j);
      _cleansection:=_cleansection+ExtractFileNameOnly(_filename)+':Clean;';
//make
      _lineToInsert:=format('  <Target Name="%s:Make">',[ExtractFileNameOnly(_filename)]);
      _file.insert(j,_lineToInsert);
      inc(j);
      _lineToInsert:=format('    <MSBuild Projects="%s" Targets="Make" />',[ExtractRelativePath(ExtractFilePath(_projectGroupFilename),_lstProjectFiles.Items[i])]);
      _file.insert(j,_lineToInsert);
      inc(j);
      _lineToInsert:=format('  </Target>',[]);
      _file.insert(j,_lineToInsert);
      inc(j);
      _makesection:=_makesection+ExtractFileNameOnly(_filename)+':Make;';
    end;
//build
    _lineToInsert:=format('  <Target Name="Build">',[]);
    _file.insert(j,_lineToInsert);
    inc(j);
    _lineToInsert:=format('    <CallTarget Targets="%s" />',[_buildsection]);
    _file.insert(j,_lineToInsert);
    inc(j);
    _lineToInsert:=format('  </Target>',[]);
    _file.insert(j,_lineToInsert);
    inc(j);
//clean
    _lineToInsert:=format('  <Target Name="Clean">',[]);
    _file.insert(j,_lineToInsert);
    inc(j);
    _lineToInsert:=format('    <CallTarget Targets="%s" />',[_cleansection]);
    _file.insert(j,_lineToInsert);
    inc(j);
    _lineToInsert:=format('  </Target>',[]);
    _file.insert(j,_lineToInsert);
    inc(j);
//make
    _lineToInsert:=format('  <Target Name="Make">',[]);
    _file.insert(j,_lineToInsert);
    inc(j);
    _lineToInsert:=format('    <CallTarget Targets="%s" />',[_makesection]);
    _file.insert(j,_lineToInsert);
    inc(j);
    _lineToInsert:=format('  </Target>',[]);
    _file.insert(j,_lineToInsert);
//    inc(j);
    if RenameFile(_projectGroupFilename,changeFileExt(_projectGroupFilename,'.~old')) then begin
      trace(1,'Renamed the file <%s> to <%s>.',[_projectGroupFilename,changeFileExt(_projectGroupFilename,'.~old')]);
    end;
    _File.SaveToFile(_projectGroupFilename);
    trace(1,'Saved ProjectGroup-File <%s>.',[_projectGroupFilename]);
  finally
    _File.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: CreateProjectGroupFile
  Author:    herzogs2
  Date:      06-Nov-2008
  Arguments: const _lstProjectFiles:TListBox;const _projectGroupFilename:string;const _DelphiVersion:integer
  Result:    None
  Description: create a project group file (either .bpg or .bdsgroup or .groupproj).
-----------------------------------------------------------------------------}
procedure CreateProjectGroupFile(const _lstProjectFiles:TListBox;const _projectGroupFilename:string;const _DelphiVersion:integer);
var
_fileExt:string;
begin
  _fileExt:=lowercase(ExtractFileExt(_ProjectGroupFilename));
  if _fileExt='.bpg'       then CreateBPGFile(_lstProjectFiles,_projectGroupFilename,_DelphiVersion)  else
  if _fileExt='.bdsgroup'  then CreateBDSGroup(_lstProjectFiles,_projectGroupFilename,_DelphiVersion) else
  if _fileExt='.groupproj' then CreateGroupProj(_lstProjectFiles,_projectGroupFilename,_DelphiVersion);
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
  if not fileexists(_projectfilename) then exit;
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
  Author:    herzogs2
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
  Author:    herzogs2
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
  Procedure: ReplaceTag
  Author:    Sam
  Date:      07-Sep-2003
  Arguments: _filename: string
  Result:    string
  Description: replaces the Tag <$(DELPHI)> with the real delphi path.
               replaces the Tag <$(BDS)> with the real delphi path.
               replaces the Tag <$(PROGRAMFILES)> with the real program files path.
               replaces  the Tag <$(DELPHIVERSION)> with the real delphi version.
               replaces  the Tag <$(BDSCOMMONDIR)> with the real bds common path.
               replaces  the Tag <$(BDSPROJECTSDIR)> with the real bds projects path.
----------------------------------------------------------------------------}
function ReplaceTag(_filename: string;_DelphiVersion:integer): string;
var
_pos:integer;
begin
  _filename := lowercase(_filename);
  _filename := StringReplace(_filename,lowercase(cDelphiVersionTag),DelphiVersions[_DelphiVersion].ShortName,[]);

  _pos := Pos(lowercase(cDelphiTag),_filename);
  if _pos > 0 then begin
    Delete(_filename,1,_pos+length(cDelphiTag));
    result := IncludeTrailingPathDelimiter(GetDelphiRootDir(_DelphiVersion))+_filename;
    exit;
  end;

  _pos := Pos(lowercase(cBDSTag),_filename);
  if _pos > 0 then begin
    Delete(_filename,1,_pos+length(cBDSTag));
    result := IncludeTrailingPathDelimiter(GetDelphiRootDir(_DelphiVersion))+_filename;
    exit;
  end;

  _pos := Pos(lowercase(cProgramFilesTag),_filename);
  if _pos > 0 then begin
    Delete(_filename,1,_pos+length(cProgramFilesTag));
    result := IncludeTrailingPathDelimiter(GetSystemPath(spProgFiles))+_filename;
    exit;
  end;

  _pos := Pos(lowercase(cBDSCommonDirTag),_filename);
  if _pos > 0 then begin
    Delete(_filename,1,_pos+length(cBDSCommonDirTag));
    result := IncludeTrailingPathDelimiter(ReadBDSCommonDir(_DelphiVersion))+_filename;
    exit;
  end;

  _pos := Pos(lowercase(cBDSProjectsDirTag),_filename);
  if _pos > 0 then begin
    Delete(_filename,1,_pos+length(cBDSProjectsDirTag));
    result := IncludeTrailingPathDelimiter(ReadBDSProjectsDir(_DelphiVersion))+_filename;
    exit;
  end;

  _pos := Pos(lowercase(cBDSUserDirTag),_filename);
  if _pos > 0 then begin
    Delete(_filename,1,_pos+length(cBDSUserDirTag));
    result := IncludeTrailingPathDelimiter(ReadBDSUserDir(_DelphiVersion))+_filename;
    exit;
  end;

  result := _filename;
end;


{-----------------------------------------------------------------------------
  Procedure: AddTag
  Author:    herzogs2
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
  Procedure: PrepapreRegistryPath
  Author:    herzogs2
  Date:      15-Jun-2007
  Arguments: _filename:string
  Result:    string
  Description:
-----------------------------------------------------------------------------}
function PrepapreRegistryPath(_filename:string):string;
var
i:integer;
_outputstring:string;
begin
  _outputstring:='';
  for i:=1 to length(_filename) do begin
    _outputstring:=_outputstring+_filename[i];
    if _filename[i]='\' then _outputstring:=_outputstring+'\';
  end;
  result:=_outputstring;
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
  if not FCreateBatchFile then exit;
  try
    FBatchFile.SaveToFile(FBatchFilename);
    result:=FBatchFilename;
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
_Reg: TRegistry;
begin
  result:='';
  if not GetIDERootKey(_DelphiVersion,_DelphiRootDirKey) then begin
    trace(5,'Warning in GetDelphiApplication: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;

  _Reg := TRegistry.Create;
  try
    _Reg.RootKey := HKEY_LOCAL_MACHINE;
    if not _Reg.OpenKeyReadOnly(_DelphiRootDirKey) then begin
      trace(1,'Problem in GetDelphiApplication: The Key <%s> was not found in the registry.',[_DelphiRootDirKey]);
      exit;
    end;
    try
      result:=_Reg.ReadString('App');
      if result<>'' then begin
        trace(5,'GetDelphiApplication: Found Delphi <%s> for delphi version <%d>.',[result,_DelphiVersion]);
      end else trace(5,'Problem in GetDelphiApplication: Could not find root directory for delphi <%d>.',[_DelphiVersion]);
    except
      trace(1,'Warning in GetDelphiApplication: Could not read the delphi root directory for delphi version <%s>.You need to have Admin rights for this computer.',[_DelphiVersion]);
    end;
    _Reg.CloseKey;
  finally
    _Reg.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: GetInstalledDelphiVersions
  Author:    Sami
  Date:      24-Dez-2003
  Arguments: var array of integer
  Result:    None
  Description: read the registry to try to find the install delphi version 1-7
  15.11.2004 SH -first try to read from HKEY_LOCAL_MACHINE
-----------------------------------------------------------------------------}
function GetInstalledDelphiVersions(_list:TStrings):boolean;
var
i:integer;
_reg: TRegistry;
begin
  result:=false;
  if not assigned(_list) then exit;
  try
    _reg := TRegistry.Create;
    try
      _reg.CloseKey;
      _reg.RootKey := HKEY_LOCAL_MACHINE;
      if _reg.OpenKeyReadOnly(cDelphiKey) then
        if _reg.HasSubKeys then _reg.GetKeyNames(_list);
      _reg.CloseKey;
      if _list.Count=0 then begin
        _reg.CloseKey;
        _reg.RootKey := HKEY_CURRENT_USER;
        if _reg.OpenKeyReadOnly(cDelphiKey) then
          if _reg.HasSubKeys then _reg.GetKeyNames(_list);
        _reg.CloseKey;
      end;
    finally
      _reg.free;
    end;
  except
    on e:exception do trace(1,'Error in GetInstalledDelphiVersions: <%s>.',[e.Message]);
  end;
  for i:=0 to _list.count-1 do trace(5,'GetInstalledDelphiVersions: Found <%s>.',[_list[i]]);
  result:=(_list.count>0);
end;

{-----------------------------------------------------------------------------
  Procedure: GetInstalledBorlandBDSVersions
  Author:    sam
  Date:      25-Feb-2006
  Arguments: _list:TStrings
  Result:    boolean
  Description: read the registry to try to find the installed bds versions 1-6
-----------------------------------------------------------------------------}
function GetInstalledBorlandBDSVersions(_list:TStrings):boolean;
var
i:integer;
_reg: TRegistry;
begin
  result:=false;
  if not assigned(_list) then exit;
  try
    _reg := TRegistry.Create;
    try
      with _reg do begin     // first search in local machine
        CloseKey;
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKeyReadOnly(cBorlandBDSKey) then
          if HasSubKeys then GetKeyNames(_list);
        CloseKey;
      end;

      if _list.Count=0 then begin // if nothing is found then lookup the current user.
        with _reg do begin
          CloseKey;
          RootKey := HKEY_CURRENT_USER;
          if OpenKeyReadOnly(cBorlandBDSKey) then
            if HasSubKeys then GetKeyNames(_list);
          CloseKey;
        end;
      end;
    finally
      if Assigned(_reg) then FreeAndNil(_reg);
    end;
  except
    on e:exception do trace(1,'Error in GetInstalledBDSVersions: <%s>.',[e.Message]);
  end;
  for i:=0 to _list.count-1 do trace(5,'GetInstalledBDSVersions: Found <%s>.',[_list[i]]);
  result:=(_list.count>0);
end;

{-----------------------------------------------------------------------------
  Procedure: GetInstalledCodeGearBDSVersions
  Author:    herzogs2
  Date:      26-Mai-2008
  Arguments: _list:TStrings
  Result:    boolean
  Description: 
-----------------------------------------------------------------------------}
function GetInstalledCodeGearBDSVersions(_list:TStrings):boolean;
var
i:integer;
_reg: TRegistry;
begin
  result:=false;
  if not assigned(_list) then exit;
  try
    _reg := TRegistry.Create;

    try
      with _reg do begin     // first search in local machine
        CloseKey;
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKeyReadOnly(cCodeGearBDSKey) then
          if HasSubKeys then GetKeyNames(_list);
        CloseKey;
      end;

      if _list.Count=0 then begin // if nothing is found then lookup the current user.
        with _reg do begin
          CloseKey;
          RootKey := HKEY_CURRENT_USER;
          if OpenKeyReadOnly(cCodeGearBDSKey) then
            if HasSubKeys then GetKeyNames(_list);
          CloseKey;
        end;
      end;
    finally
      if Assigned(_reg) then FreeAndNil(_reg);
    end;
  except
    on e:exception do trace(1,'Error in GetInstalledCodeGearBDSVersions: <%s>.',[e.Message]);
  end;
  for i:=0 to _list.count-1 do trace(5,'GetInstalledCodeGearBDSVersions: Found <%s>.',[_list[i]]);
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
  GetInstalledDelphiVersions(_list);
  _tmp:=TStringList.create;
  try
    GetInstalledBorlandBDSVersions(_tmp);
    for i:=0 to _tmp.Count-1 do begin
      _sVersion:=_tmp[i];
      if not StringToFloat(_sversion,_fVersion) then continue;
      _list.Add(inttostr(trunc(_fVersion)+6)+'.0');
    end;
    _tmp.Clear;
    GetInstalledCodeGearBDSVersions(_tmp);
    for i:=0 to _tmp.Count-1 do begin
      _sVersion:=_tmp[i];
      if not StringToFloat(_sversion,_fVersion) then continue;
      if _fVersion<7 then _list.Add(inttostr(trunc(_fVersion)+6)+'.0')
                     else _list.Add(inttostr(trunc(_fVersion)+7)+'.0')
    end;
  finally
    _tmp.free;
  end;
  i:=0;
  while i<_list.count do begin
    _version:=IDENameToVersionNo(_list.Strings[i]);
    _delphiExeFilename:=GetDelphiApplication(_version);
    if _delphiExeFilename='' then _list.Delete(i)
    else begin
      if fileexists(_delphiExeFilename) then inc(i)
      else begin
        trace(3,'Found the Registry entry for IDE <%s> but the file <%s> is not present.',[_list.Strings[i],_delphiExeFilename]);
        _list.Delete(i);
      end;
    end;
  end;
  result:=(_list.count>0);
end;

//*****************************************************
// Method:  GetField
// Programmer: S.Herzog
// Description: get a field from a string <_s> seperated by chars <_ch>.
// Last changes: 23.04.02
//*****************************************************
function GetField(_ch:char;var _s:string):string;
var
_pos:integer;
begin
  Result:='';
  _pos:=Pos(_ch,_s);
  if _pos=0 then begin
    Result:=_s;
    _s:='';
    exit;
  end;
  Result:=Copy(_s,1,_pos-1);
  Delete(_s,1,_pos);
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
_Reg: TRegistry;
begin
  result:='';
  if not GetIDERootKey(_DelphiVersion,_DelphiRootDirKey) then begin
    trace(3,'Problem in GetDelphiRootDir: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;
  _Reg := TRegistry.Create;
  try
    _Reg.RootKey := HKEY_LOCAL_MACHINE;
    if not _Reg.OpenKeyReadOnly(_DelphiRootDirKey) then begin
      trace(1,'Problem in GetDelphiRootDir: The Key <%s> was not found in the registry.',[_DelphiRootDirKey]);
      exit;
    end;
    try
      _DelphiRootPath:=_Reg.ReadString('RootDir');
      if _DelphiRootPath='' then begin
        trace(3,'Problem in GetDelphiRootDir: Could not get root directory for delphi <%d>.',[_DelphiVersion]);
        exit;
      end;
      result:=IncludeTrailingPathDelimiter(_DelphiRootPath);
      trace(5,'GetDelphiRootDir: Delphi root directory <%s> for delphi version <%d>.',[_DelphiRootPath,_DelphiVersion]);
    except
      on e:exception do trace(1,'Warning in GetDelphiRootDir: Could not read the delphi root directory for delphi version <%s>.You need to have Admin rights for this computer. <%s>.',[_DelphiVersion,e.message]);
    end;
    _Reg.CloseKey;
  finally
    _Reg.Free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: GetDelphiPackageDir
  Author:    sam
  Date:      11-Sep-2009
  Arguments: const _DelphiVersion:integer
  Result:    string
  Description: get the bpl-folder from the registry.
-----------------------------------------------------------------------------}
function GetDelphiPackageDir(const _DelphiVersion:integer):string;
var
_DelphiRootDirKey:string;
_DelphiPackageDirKey:string;
_DelphiPackagePath:string;
_Reg: TRegistry;
begin
  result:='unknown';
  if not GetIDERootKey(_DelphiVersion,_DelphiRootDirKey) then begin
    trace(3,'Problem in GetDelphiPackageDir: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;
  _Reg := TRegistry.Create;
  try
    _Reg.RootKey := HKEY_LOCAL_MACHINE;
    _DelphiPackageDirKey:=_DelphiRootDirKey+'Library\';
    if not _Reg.OpenKeyReadOnly(_DelphiPackageDirKey) then begin
       trace(1,'Problem in GetDelphiPackageDir: The Key <%s> was not found in the registry.',[_DelphiPackageDirKey]);
      exit;
    end;
    try
      _DelphiPackagePath:=_Reg.ReadString('Package DPL Output');
      if _DelphiPackagePath='' then begin
        trace(3,'Problem in GetDelphiPackageDir: Could not get root directory for delphi <%d>.',[_DelphiVersion]);
        exit;
      end;
      result:=IncludeTrailingPathDelimiter(_DelphiPackagePath);
      trace(5,'GetDelphiPackageDir: Delphi root directory <%s> for delphi version <%d>.',[_DelphiPackagePath,_DelphiVersion]);
    except
      on e:exception do trace(1,'Warning in GetDelphiPackageDir: Could not read the delphi package directory for delphi version <%s>.You need to have Admin rights for this computer. <%s>.',[_DelphiVersion,e.message]);
    end;
    _Reg.CloseKey;
  finally
    _Reg.Free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: SetDelphiPackageDir
  Author:    sam
  Date:      29-Sep-2009
  Arguments: const _DelphiVersion:integer;_PackageDir:string;const _silent:boolean
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function SetDelphiPackageDir(const _DelphiVersion:integer;_PackageDir:string;const _silent:boolean):boolean;
resourcestring
cAskToChangePackageOutputPath='Do you want to change the Delphi''s Package Output Path in the registry to <%s>?';
var
_DelphiRootDirKey:string;
_DelphiPackageDirKey:string;
_Reg: TRegistry;
begin
  result:=false;
  if _PackageDir='' then exit;
  if GetDelphiPackageDir(_DelphiVersion)=_PackageDir then begin
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

  _Reg := TRegistry.Create;
  try
    _Reg.RootKey := HKEY_LOCAL_MACHINE;
    _DelphiPackageDirKey:=_DelphiRootDirKey+'Library\';
    if not _Reg.OpenKey(_DelphiPackageDirKey,false) then begin
      trace(1,'Problem in SetDelphiPackageDir: The Key <%s> was not found in the registry.',[_DelphiPackageDirKey]);
      exit;
    end;
    try
      _Reg.WriteString('Package DPL Output',_PackageDir);
      _Reg.WriteString('Package DCP Output',_PackageDir);
      result:=true;
    except
      on e:exception do trace(1,'Warning in SetDelphiPackageDir: Could not write the delphi package directory for delphi version <%s>.You need to have Admin rights for this computer. <%s>.',[_DelphiVersion,e.message]);
    end;
    _Reg.CloseKey;
  finally
    _Reg.Free;
  end;
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
  Author:    herzogs2
  Date:      27-Sep-2002
  Arguments: _cfgFilename:String;var _Conditions:String
  Result:    boolean
  Purpose:   get information from the cfg-file.
  History:
-----------------------------------------------------------------------------}
function ReadCFGSettings(const _cfgFilename:String;var Conditions:string;var SearchPath:String;var ProjectOutputPath:string;var BPLOutputPath:string;var DCUOutputPath:string):boolean; // get informations from the cfg-file.
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
    trace(2,'Problem in ReadCFGSettings: Could not find the file <%s>.',[_cfgFilename]);
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
  Author:    herzogs2
  Date:      27-Mai-2008
  Arguments: const _bdsprojFilename:String;var Conditions:string;var SearchPath:String;var ProjectOutputPath:string;var BPLOutputPath:string;var DCUOutputPath:string
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function ReadBDSProjSettings(const _bdsprojFilename:String;var ProjectName:string;var Conditions:string;var SearchPath:String;var ProjectOutputPath:string;var BPLOutputPath:string;var DCUOutputPath:string):boolean; // get informations from the cfg-file.
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
  Author:    herzogs2
  Date:      03-Mrz-2010
  Arguments: const _dprojFilename:String;var Conditions:string;var SearchPath:String;var ProjectOutputPath:string;var BPLOutputPath:string;var DCUOutputPath:string
  Result:    boolean
  Description: read dproj settings from a file of version D2005-D2007.
-----------------------------------------------------------------------------}
function ReadDPROJSettingsD2005_D2007(const _dprojFilename:String;var Conditions:string;var SearchPath:String;var ProjectOutputPath:string;var BPLOutputPath:string;var DCUOutputPath:string):boolean; // get informations from the cfg-file.
var
_msg:string;
_condition:string;
_platform:string;
_ProjectVersion:string;
begin
  Result:=false;
  ProjectOutputPath:='';
  SearchPath:='';
  BPLOutputPath:='';
  DCUOutputPath:='';
  Conditions:='';
  _ProjectVersion:='';
  if not fileExists(_dprojFilename) then begin
    trace(5,'ReadDPROJSettingsD2005_D2007: Could not find the file <%s>.',[_dprojFilename]);
    exit;
  end;
  if not ReadNodeText(_dprojFilename,'//PropertyGroup/ProjectVersion',_ProjectVersion,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not read condition. <%s>.',[_msg]);
  if _ProjectVersion='' then begin
    if not ReadNodeText(_dprojFilename,'//PropertyGroup/Configuration[@Condition="''$(Configuration)'' == ''''"]',_condition,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not read condition. <%s>.',[_msg]);
    trace(5,'ReadDPROJSettingsD2005_D2007: Configuration = %s.',[_condition]);
    if not ReadNodeText(_dprojFilename,'//PropertyGroup/Platform[@Condition="''$(Platform)'' == ''''"]',_platform,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not read platform. <%s>.',[_msg]);
    trace(5,'ReadDPROJSettingsD2005_D2007: Platform = %s.',[_platform]);
    if _condition='' then begin
       _condition:='Base';
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$('+_condition+')''!=''''"]/DCC_UnitSearchPath',SearchPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find SearchPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: SearchPath is <%s>.',[SearchPath]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$('+_condition+')''!=''''"]/DCC_DcuOutput',DCUOutputPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find DCUOutputPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: DCU Output Path is <%s>.',[DCUOutputPath]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$('+_condition+')''!=''''"]/DCC_BplOutput',BPLOutputPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find BPLOutputPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: BPL Output Path is <%s>.',[BPLOutputPath]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$('+_condition+')''!=''''"]/DCC_Define',Conditions,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find Conditions. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: Conditions are <%s>.',[Conditions]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$('+_condition+')''!=''''"]/DCC_ExeOutput',ProjectOutputPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find ProjectOutputPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: Project Output Path is <%s>.',[ProjectOutputPath]);
    end
    else begin
      if _platform<>'' then _condition:=_condition+'|'+_platform;
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$(Configuration)|$(Platform)'' == '''+_condition+'''"]/DCC_UnitSearchPath',SearchPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find SearchPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: SearchPath is <%s>.',[SearchPath]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$(Configuration)|$(Platform)'' == '''+_condition+'''"]/DCC_DcuOutput',DCUOutputPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find DCUOutputPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: DCU Output Path is <%s>.',[DCUOutputPath]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$(Configuration)|$(Platform)'' == '''+_condition+'''"]/DCC_BplOutput',BPLOutputPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find BPLOutputPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: BPL Output Path is <%s>.',[BPLOutputPath]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$(Configuration)|$(Platform)'' == '''+_condition+'''"]/DCC_Define',Conditions,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find Conditions. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: Conditions are <%s>.',[Conditions]);
      if not ReadNodeText(_dprojFilename,'//PropertyGroup[@Condition="''$(Configuration)|$(Platform)'' == '''+_condition+'''"]/DCC_ExeOutput',ProjectOutputPath,_msg) then trace(3,'Warning in ReadDPROJSettingsD2005_D2007: Could not find ProjectOutputPath. <%s>.',[_msg]);
      trace(5,'ReadDPROJSettingsD2005_D2007: Project Output Path is <%s>.',[ProjectOutputPath]);
    end;
  end;
  result:=true;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadDPROJSettingsD2009_and_Newer
  Author:    sam
  Date:      06-Mrz-2010
  Arguments: const _dprojFilename:String;var Conditions:string;var SearchPath:String;var ProjectOutputPath:string;var BPLOutputPath:string;var DCUOutputPath:string
  Result:    boolean
  Description: read path information from a dproj file used by D2009 or newer.
-----------------------------------------------------------------------------}
function ReadDPROJSettingsD2009_and_Newer(const _dprojFilename:String;var Conditions:string;var SearchPath:String;var ProjectOutputPath:string;var BPLOutputPath:string;var DCUOutputPath:string):boolean; // get informations from the cfg-file.
var
  _xmlDOMfile: IXMLDOMDocument;
  _msg:string;
  _Config:string;
  _Temp:string;
  i: Integer;
  _NodeList:IXMLDOMNodeList;
  _Node:IXMLDOMNode;
  _Configs: TStringList;
begin
  Result:=false;
  _Config:='';
  ProjectOutputPath:='';
  SearchPath:='';
  BPLOutputPath:='';
  DCUOutputPath:='';
  Conditions:='';
  _Configs := TStringList.Create;
  _xmlDOMfile := CoDOMDocument.Create;
  try
    if not _xmlDOMfile.load(_dprojFilename) then begin
      trace(5,'ReadDPROJSettingsD2009_and_Newer: Could not find the file <%s>.',[_dprojFilename]);
      exit;
    end;

    if not ReadNodeDocument(_xmlDOMfile,'//PropertyGroup[0]/DCC_Define',Conditions,_msg) then trace(3,'Warning in ReadDPROJSettingsD2009_and_Newer: Could not find Conditions. <%s>.',[_msg]);
    trace(5,'ReadDPROJSettingsD2009_and_Newer: Conditions are <%s>.',[Conditions]);

    if not ReadNodeDocument(_xmlDOMfile,'//PropertyGroup/Config[@Condition="''$(Config)''==''''"]',_Config,_msg) then trace(3,'Warning in ReadDPROJSettingsD2009_and_Newer: Could not find Config. <%s>.',[_msg]);
    trace(5,'ReadDPROJSettingsD2009_and_Newer: Config is <%s>.',[_Config]);

    //Search for Config
    _NodeList := _xmlDOMfile.selectNodes('//PropertyGroup/@Condition');
    _Temp := '';
    for i:=0 to _NodeList.length - 1 do begin
      if Pos(WideString('''$(Config)''=='''+ _Config +''''), _NodeList[i].text) > 0 then begin
        _Node:= _NodeList[i];
        break;
      end;
    end;

    if not Assigned(_Node) then begin
      trace(5,'ReadDPROJSettingsD2009_and_Newer: Could not find Config <%s>.',[_Config]);
      exit;
    end;

    //Config found
    _Temp := _Node.text;
    _Node := _xmlDOMfile.selectSingleNode('//PropertyGroup[@Condition="' + _Temp + '"]');
    _NodeList := _Node.childNodes;
    if _NodeList.length > 0 then begin
      _Configs.Add(_NodeList[0].nodeName);

      _Node := _xmlDOMfile.selectSingleNode('//PropertyGroup[@Condition="' + _Temp + '"]/CfgParent');
      if Assigned(_Node) then begin
        _Configs.Insert(0, _Node.text);
      end;
    end;

    //Get DCC_UnitSearchPath
    for i := 0 to _Configs.Count - 1 do begin
      if _Configs[i] <> '' then begin
        _Temp := '';
        if not ReadNodeDocument(_xmlDOMfile,'//PropertyGroup[@Condition="''$(' + _Configs[i] + ')''!=''''"]/DCC_UnitSearchPath',_Temp,_msg) then trace(3,'Warning in ReadDPROJSettingsD2009_and_Newer: Could not find %s SearchPath. <%s>.',[_Configs[i], _msg]);
        if _Temp <> '' then SearchPath := StringReplace(_Temp, '$(DCC_UnitSearchPath)', SearchPath, [rfIgnoreCase, rfReplaceAll]);
      end;
    end;
    trace(5,'ReadDPROJSettingsD2009_and_Newer: SearchPath is <%s>.',[SearchPath]);

    //Get DCC_DcuOutput
    for i := 0 to _Configs.Count - 1 do begin
      if _Configs[i] <> '' then begin
        _Temp := '';
        if not ReadNodeDocument(_xmlDOMfile,'//PropertyGroup[@Condition="''$(' + _Configs[i] + ')''!=''''"]/DCC_DcuOutput',_Temp,_msg) then trace(3,'Warning in ReadDPROJSettingsD2009_and_Newer: Could not find %s DCUOutputPath. <%s>.',[_Configs[i], _msg]);
        if _Temp <> '' then DCUOutputPath := StringReplace(_Temp, '$(DCC_DcuOutput)', DCUOutputPath, [rfIgnoreCase, rfReplaceAll]);
      end;
    end;
    trace(5,'ReadDPROJSettingsD2009_and_Newer: DCU Output Path is <%s>.',[DCUOutputPath]);

    //Get DCC_BplOutput
    for i := 0 to _Configs.Count - 1 do begin
      if _Configs[i] <> '' then begin
        _Temp := '';
        if not ReadNodeDocument(_xmlDOMfile,'//PropertyGroup[@Condition="''$(' + _Configs[i] + ')''!=''''"]/DCC_BplOutput',_Temp,_msg) then trace(3,'Warning in ReadDPROJSettingsD2009_and_Newer: Could not find %s BPLOutputPath. <%s>.',[_Configs[i], _msg]);
        if _Temp <> '' then BPLOutputPath := StringReplace(_Temp, '$(DCC_BplOutput)', BPLOutputPath, [rfIgnoreCase, rfReplaceAll]);
      end;
    end;
    trace(5,'ReadDPROJSettingsD2009_and_Newer: BPL Output Path is <%s>.',[BPLOutputPath]);

    //Get DCC_Define
    for i := 0 to _Configs.Count - 1 do begin
      if _Configs[i] <> '' then begin
        _Temp := '';
        if not ReadNodeDocument(_xmlDOMfile,'//PropertyGroup[@Condition="''$(' + _Configs[i] + ')''!=''''"]/DCC_Define',_Temp,_msg) then trace(3,'Warning in ReadDPROJSettingsD2009_and_Newer: Could not find %s Conditions. <%s>.',[_Configs[i], _msg]);
        if _Temp <> '' then Conditions := StringReplace(_Temp, '$(DCC_Define)', Conditions, [rfIgnoreCase, rfReplaceAll]);
      end;
    end;
    trace(5,'ReadDPROJSettingsD2009_and_Newer: Conditions are <%s>.',[Conditions]);

    //Get DCC_ExeOutput
    for i := 0 to _Configs.Count - 1 do begin
      if _Configs[i] <> '' then begin
        _Temp := '';
        if not ReadNodeDocument(_xmlDOMfile,'//PropertyGroup[@Condition="''$(' + _Configs[i] + ')''!=''''"]/DCC_ExeOutput',_Temp,_msg) then trace(3,'Warning in ReadDPROJSettingsD2009_and_Newer: Could not find %s ProjectOutputPath. <%s>.',[_Configs[i], _msg]);
        if _Temp <> '' then ProjectOutputPath := StringReplace(_Temp, '$(DCC_ExeOutput)', ProjectOutputPath, [rfIgnoreCase, rfReplaceAll]);
      end;
    end;
    trace(5,'ReadDPROJSettingsD2009_and_Newer: ProjectOutputPath is <%s>.',[ProjectOutputPath]);

    result:=true;
  finally
    _xmlDOMfile := nil;
    _Configs.Free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadDPROJSettings
  Author:    sam
  Date:      22-Mai-2008
  Arguments: const _dprojFilename:String;var Conditions:string;var SearchPath:String;var ProjectOutputPath:string;var BPLOutputPath:string;var DCUOutputPath:string
  Result:    boolean
  Description: read settings from a dproj-file.
-----------------------------------------------------------------------------}
function ReadDPROJSettings(const _dprojFilename:String;var Conditions:string;var SearchPath:String;var ProjectOutputPath:string;var BPLOutputPath:string;var DCUOutputPath:string):boolean; // get informations from the cfg-file.
var
_msg:string;
_ProjectVersion:string;
begin
  Result:=false;
  ProjectOutputPath:='';
  SearchPath:='';
  BPLOutputPath:='';
  DCUOutputPath:='';
  Conditions:='';
  _ProjectVersion:='';
  if not fileExists(_dprojFilename) then begin
    trace(5,'ReadDPROJSettings: Could not find the file <%s>.',[_dprojFilename]);
    exit;
  end;
  if not ReadNodeText(_dprojFilename,'//PropertyGroup/ProjectVersion',_ProjectVersion,_msg) then trace(3,'Warning in ReadDPROJSettings: Could not read condition. <%s>.',[_msg]);
  if _ProjectVersion='' then ReadDPROJSettingsD2005_D2007(_dprojFilename,Conditions,SearchPath,ProjectOutputPath,BPLOutputPath,DCUOutputPath)
                        else ReadDPROJSettingsD2009_and_Newer(_dprojFilename,Conditions,SearchPath,ProjectOutputPath,BPLOutputPath,DCUOutputPath);
  result:=true;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadConfigurationSettings
  Author:    sam
  Date:      02-Jun-2008
  Arguments: const _filename:string;var Conditions:string;var SearchPath:String;var ProjectOutputPath:string;var BPLOutputPath:string;var DCUOutputPath:string
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function ReadConfigurationSettings(const _filename:string;var Conditions:string;var SearchPath:String;var ProjectOutputPath:string;var BPLOutputPath:string;var DCUOutputPath:string):boolean;
var
_fileext:string;
_ProjectName:string;
begin
  result:=false;
  _fileext:=lowercase(ExtractFileExt(_filename));
  trace(5,'ReadConfigurationSettings: filename <%s>.',[_filename]);
  if (_fileext='.dpk') or
     (_fileext='.dpr')   then result:=ReadCFGSettings(ChangefileExt(_filename,'.cfg'),Conditions,SearchPath,ProjectOutputPath,BPLOutputPath,DCUOutputPath)   else
  if _fileext='.dproj'   then result:=ReadDProjSettings(_filename,Conditions,SearchPath,ProjectOutputPath,BPLOutputPath,DCUOutputPath) else
  if _fileext='.bdsproj' then result:=ReadBDSProjSettings(_filename,_ProjectName,Conditions,SearchPath,ProjectOutputPath,BPLOutputPath,DCUOutputPath);
  if Conditions<>'' then Conditions:='-D"'+Conditions+'"';
end;

{-----------------------------------------------------------------------------
  Procedure: ReadProjectFilenameFromDProj
  Author:    herzogs2
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
  Procedure: WriteCFGSettings
  Author:    HerzogS2
  Date:      07-Mrz-2007
  Arguments: _cfgFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath:string
  Result:    boolean
  Description: write the path-settings from the delphipackagetool into the .cfg-file.
-----------------------------------------------------------------------------}
function  WriteCFGSettings(const _bpgPath,_cfgFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath:string;const _silent:boolean;const _DelphiVersion:integer):boolean; // get informations from the cfg-file.
var
_CFGFile:TStrings;

  procedure DeleteSection(_BeginMark,_EndMark:string);
  var
  _Text:String;
  _begin,_End:Integer;
  begin
    _Text:=_CFGFile.Text;
    _begin:=Pos(_BeginMark,_Text);
    if _begin<=0 then exit;
    Delete(_text,_begin,length(_BeginMark));
    _end:=PosEx(_EndMark,_Text,_begin);
    if _end=0 then _end:=length(_text);
    if _EndMark<>#13+#10 then Delete(_Text,_begin,_end-_begin+3)
                         else Delete(_Text,_begin,_end-_begin+1);
    _CFGFile.Text:=_Text;
  end;

begin
  Result:=false;
  if not fileExists(_cfgFilename) then begin
    trace(2,'Problem in WriteCFGSettings: Could not find the file <%s>.',[_cfgFilename]);
    exit;
  end;
  if not RemoveReadOnlyFlag(_cfgFilename,_silent) then exit;
  if not BackupFile(_cfgFilename,'.cfg_old','',false) then exit;
// make all paths relative
  _searchPath       :=RelativePaths(ExtractFilePath(_cfgFilename),_searchPath,_DelphiVersion);
  _ProjectOutputPath:=RelativePath(ExtractFilePath(_cfgFilename),_ProjectOutputPath,_DelphiVersion,false);
  _BPLOutputPath    :=RelativePath(ExtractFilePath(_cfgFilename),_BPLOutputPath,_DelphiVersion,false);
  _DCUOutputPath    :=RelativePath(ExtractFilePath(_cfgFilename),_DCUOutputPath,_DelphiVersion,false);

// then write them into the cfg-file.
  _CFGFile:=TStringList.Create;
  try
    _CFGFile.LoadFromFile(_cfgFilename);
    DeleteSection('-U"','"');
    if _SearchPath<>'' then _CFGFile.Add('-U"'+_SearchPath+'"');
    DeleteSection('-O"','"');
    if _SearchPath<>'' then _CFGFile.Add('-O"'+_SearchPath+'"');
    DeleteSection('-I"','"');
    if _SearchPath<>'' then _CFGFile.Add('-I"'+_SearchPath+'"');
    DeleteSection('-R"','"');
    if _SearchPath<>'' then begin
      _CFGFile.Add('-R"'+_SearchPath+'"');
      trace(5,'WriteCFGSettings: Set Search Path to <%s>.',[_SearchPath]);
    end;  
    DeleteSection('-E"','"');
    if _ProjectOutputPath<>'' then begin
      _CFGFile.Add('-E"'+_ProjectOutputPath+'"');
      trace(5,'WriteCFGSettings: -E Set Output Path to <%s>.',[_ProjectOutputPath]);
    end;
    DeleteSection('-LE"','"');
    if _BPLOutputPath<>'' then begin
      _CFGFile.Add('-LE"'+_BPLOutputPath+'"');
      trace(5,'WriteCFGSettings: -LE BPL Output Path is <%s>.',[_BPLOutputPath]);
    end;
    DeleteSection('-LN"','"');
    if _BPLOutputPath<>'' then begin
      _CFGFile.Add('-LN"'+_BPLOutputPath+'"');
      trace(5,'WriteCFGSettings: -LN BPL Output Path is <%s>.',[_BPLOutputPath]);
    end;
    DeleteSection('-N"','"');
    if _DCUOutputPath<>'' then begin
      _CFGFile.Add('-N"'+_DCUOutputPath+'"');
      trace(5,'WriteCFGSettings: -N DCU Output Path is <%s>.',[_DCUOutputPath]);
    end;
    _CFGFile.SaveToFile(_cfgFilename);
    result:=true;
  finally
    _CFGFile.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: WriteBDSProjSettings
  Author:    herzogs2
  Date:      08-Jul-2008
  Arguments: const _bpgPath,_bdsprojFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath:string;const _silent:boolean;const _DelphiVersion:integer
  Result:    boolean
  Description: write the path-settings from the delphipackagetool into the .bdsproj-file.
-----------------------------------------------------------------------------}
function  WriteBDSProjSettings(const _bpgPath,_bdsprojFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath:string;const _silent:boolean;const _DelphiVersion:integer):boolean;
var
_BDSProjFile:TStrings;
_index:integer;
_changed:boolean;
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
  Result:=false;
  _changed:=false;
  if not fileExists(_bdsprojFilename) then begin
    trace(2,'Problem in WriteBDSProjSettings: Could not find the file <%s>.',[_bdsprojFilename]);
    exit;
  end;

// make all paths relative
  _searchPath       :=RelativePaths(ExtractFilePath(_bdsprojFilename),_searchPath,_DelphiVersion);
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
        _changed:=true;
      end;
    end;

    _index:=FindText('<Directories Name="OutputDir">',_pos);
    if _index>-1 then begin
      _temp:=copy(_BDSProjFile[_index],1,_pos-1);
      if _BDSProjFile[_index]<>_temp+'<Directories Name="OutputDir">'+_ProjectOutputPath+'</Directories>' then begin
        _BDSProjFile[_index]:=_temp+'<Directories Name="OutputDir">'+_ProjectOutputPath+'</Directories>';
        trace(4,'WriteBDSProjSettings: Write output path <%s> to file <%s>.',[_ProjectOutputPath,_bdsprojFilename]);
        _changed:=true;
      end;
    end;

    _index:=FindText('<Directories Name="PackageDLLOutputDir">',_pos);
    if _index>-1 then begin
      _temp:=copy(_BDSProjFile[_index],1,_pos-1);
      if _BDSProjFile[_index]<>_temp+'<Directories Name="PackageDLLOutputDir">'+_ProjectOutputPath+'</Directories>' then begin
        _BDSProjFile[_index]:=_temp+'<Directories Name="PackageDLLOutputDir">'+_ProjectOutputPath+'</Directories>';
        trace(4,'WriteBDSProjSettings: Write dll output path <%s> to file <%s>.',[_ProjectOutputPath,_bdsprojFilename]);
        _changed:=true;
      end;
    end;

    _index:=FindText('<Directories Name="UnitOutputDir">',_pos);
    if _index>-1 then begin
      _temp:=copy(_BDSProjFile[_index],1,_pos-1);
      if _BDSProjFile[_index]<>_temp+'<Directories Name="UnitOutputDir">'+_DCUOutputPath+'</Directories>' then begin
        _BDSProjFile[_index]:=_temp+'<Directories Name="UnitOutputDir">'+_DCUOutputPath+'</Directories>';
        trace(4,'WriteBDSProjSettings: Write unit output path <%s> to file <%s>.',[_DCUOutputPath,_bdsprojFilename]);
        _changed:=true;
      end;
    end;
    _index:=FindText('<Directories Name="PackageDCPOutputDir">',_pos);
    if _index>-1 then begin
      _temp:=copy(_BDSProjFile[_index],1,_pos-1);
      if _BDSProjFile[_index]<>_temp+'<Directories Name="PackageDCPOutputDir">'+_BPLOutputPath+'</Directories>' then begin
        _BDSProjFile[_index]:=_temp+'<Directories Name="PackageDCPOutputDir">'+_BPLOutputPath+'</Directories>';
        trace(4,'WriteBDSProjSettings: Write package output path <%s> to file <%s>.',[_BPLOutputPath,_bdsprojFilename]);
        _changed:=true;
      end;
    end;
    if _changed then  begin
      if not RemoveReadOnlyFlag(_bdsprojFilename,_silent) then exit;
      if not BackupFile(_bdsprojFilename,'.bdsproj_old','',false) then exit;
      _BDSProjFile.SaveToFile(_bdsprojFilename);
      trace(3,'WriteBDSProjSettings: Write changes to file <%s>.',[_bdsprojFilename]);
    end;
    result:=true;
  finally
    _BDSProjFile.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: WriteDProjSettings
  Author:    herzogs2
  Date:      08-Jul-2008
  Arguments: const _bpgPath,_bdsprojFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath:string;const _silent:boolean;const _DelphiVersion:integer
  Result:    boolean
  Description: write the path-settings from the delphipackagetool into the .dproj-file.
-----------------------------------------------------------------------------}
function  WriteDProjSettings(const _bpgPath,_dprojFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath:string;const _silent:boolean;const _DelphiVersion:integer):boolean;
var
_DProjFile:TStrings;
_index:integer;
_changed:boolean;
_temp:string;
_pos:integer;
  function FindText(const _s:string;var position:integer):integer;
  var
  i:integer;
  begin
    result:=-1;
    position:=0;
    for i:=0 to _DProjFile.Count-1 do begin
      position:=pos(_s,_DProjFile[i]);
      if position>0 then begin
        result:=i;
        exit;
      end;
    end;
  end;


begin
  Result:=false;
  _changed:=false;
  if not fileExists(_DProjFilename) then begin
    trace(2,'Problem in WriteDProjSettings: Could not find the file <%s>.',[_dprojFilename]);
    exit;
  end;

// make all paths relative
  _searchPath       :=RelativePaths(ExtractFilePath(_dprojFilename),_searchPath,_DelphiVersion);
  _ProjectOutputPath:=RelativePath(ExtractFilePath(_dprojFilename),_ProjectOutputPath,_DelphiVersion,false);
  _BPLOutputPath    :=RelativePath(ExtractFilePath(_dprojFilename),_BPLOutputPath,_DelphiVersion,false);
  _DCUOutputPath    :=RelativePath(ExtractFilePath(_dprojFilename),_DCUOutputPath,_DelphiVersion,false);

// then write them into the dproj-file.
  _DProjFile:=TStringList.Create;
  try
    _DProjFile.LoadFromFile(_dprojFilename);
    _index:=FindText('<DCC_UnitSearchPath>',_pos);
    if _index>-1 then begin
      _temp:=copy(_DProjFile[_index],1,_pos-1);
      if _DProjFile[_index]<>_temp+'<DCC_UnitSearchPath>'+_searchPath+'</DCC_UnitSearchPath>' then begin
        _DProjFile[_index]:=_temp+'<DCC_UnitSearchPath>'+_searchPath+'</DCC_UnitSearchPath>';
        trace(4,'WriteDProjSettings: Write search path <%s> to file <%s>.',[_searchPath,_dprojFilename]);
        _changed:=true;
      end;
    end;

    _index:=FindText('<Directories Name="OutputDir">',_pos);
    if _index>-1 then begin
      _temp:=copy(_DProjFile[_index],1,_pos-1);
      if _DProjFile[_index]<>_temp+'<Directories Name="OutputDir">'+_ProjectOutputPath+'</Directories>' then begin
        _DProjFile[_index]:=_temp+'<Directories Name="OutputDir">'+_ProjectOutputPath+'</Directories>';
        trace(4,'WriteDProjSettings: Write output path <%s> to file <%s>.',[_ProjectOutputPath,_dprojFilename]);
        _changed:=true;
      end;
    end;

    _index:=FindText('<DCC_ResourcePath>',_pos);
    if _index>-1 then begin
      _temp:=copy(_DProjFile[_index],1,_pos-1);
      if _DProjFile[_index]<>_temp+'<DCC_ResourcePath>'+_ProjectOutputPath+'</DCC_ResourcePath>' then begin
        _DProjFile[_index]:=_temp+'<DCC_ResourcePath>'+_ProjectOutputPath+'</DCC_ResourcePath>';
        trace(4,'WriteDProjSettings: Write dll output path <%s> to file <%s>.',[_ProjectOutputPath,_dprojFilename]);
        _changed:=true;
      end;
    end;

    _index:=FindText('<DCC_ObjPath>',_pos);
    if _index>-1 then begin
      _temp:=copy(_DProjFile[_index],1,_pos-1);
      if _DProjFile[_index]<>_temp+'<DCC_ObjPath>'+_DCUOutputPath+'</DCC_ObjPath>' then begin
        _DProjFile[_index]:=_temp+'<DCC_ObjPath>'+_DCUOutputPath+'</DCC_ObjPath>';
        trace(4,'WriteDProjSettings: Write unit output path <%s> to file <%s>.',[_DCUOutputPath,_dprojFilename]);
        _changed:=true;
      end;
    end;
    _index:=FindText('<DCC_IncludePath>',_pos);
    if _index>-1 then begin
      _temp:=copy(_DProjFile[_index],1,_pos-1);
      if _DProjFile[_index]<>_temp+'<DCC_IncludePath>'+_BPLOutputPath+'</DCC_IncludePath>' then begin
        _DProjFile[_index]:=_temp+'<DCC_IncludePath>'+_BPLOutputPath+'</DCC_IncludePath>';
        trace(4,'WriteDProjSettings: Write package output path <%s> to file <%s>.',[_BPLOutputPath,_dprojFilename]);
        _changed:=true;
      end;
    end;
    if _changed then  begin
      if not RemoveReadOnlyFlag(_dprojFilename,_silent) then exit;
      if not BackupFile(_dprojFilename,'.dproj_old','',false) then exit;
      _DProjFile.SaveToFile(_dprojFilename);
      trace(3,'WriteDProjSettings: Write changes to file <%s>.',[_dprojFilename]);
    end;
    result:=true;
  finally
    _DProjFile.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: WriteSettingsToDelphi
  Author:    herzogs2
  Date:      08-Jul-2008
  Arguments: const _bpgPath,_cfgFilename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath:string;const _silent:boolean;const _DelphiVersion:integer
  Result:    boolean
  Description: write path settings to cfg,bdsproj,dproj file.
-----------------------------------------------------------------------------}
function  WriteSettingsToDelphi(_bpgPath,_Filename:String;_Conditions:string;_SearchPath:String;_ProjectOutputPath:string;_BPLOutputPath,_DCUOutputPath:string;const _silent:boolean;const _DelphiVersion:integer):boolean; // get informations from the cfg-file.
begin
  result:=false;
  case _DelphiVersion of
    5,6,7,8:begin
              result:=WriteCFGSettings(_bpgPath,changefileext(_Filename,'.cfg'),_Conditions,_SearchPath,_ProjectOutputPath,_BPLOutputPath,_DCUOutputPath,_silent,_DelphiVersion);
              WriteDOFFile(_bpgPath,changefileext(_Filename,'.dof'),_Conditions,_SearchPath,_ProjectOutputPath,_BPLOutputPath,_DCUOutputPath,_silent,_DelphiVersion);
            end;
    else begin
      if fileexists(changefileext(_Filename,'.cfg'))     then  result:=WriteCFGSettings(_bpgPath,changefileext(_Filename,'.cfg'),_Conditions,_SearchPath,_ProjectOutputPath,_BPLOutputPath,_DCUOutputPath,_silent,_DelphiVersion);
      if fileexists(changefileext(_Filename,'.bdsproj')) then  result:=WriteBDSProjSettings(_bpgPath,changefileext(_Filename,'.bdsproj'),_Conditions,_SearchPath,_ProjectOutputPath,_BPLOutputPath,_DCUOutputPath,_silent,_DelphiVersion);
      if fileexists(changefileext(_Filename,'.dproj'))   then  result:=WriteDProjSettings(_bpgPath,changefileext(_Filename,'.dproj'),_Conditions,_SearchPath,_ProjectOutputPath,_BPLOutputPath,_DCUOutputPath,_silent,_DelphiVersion);
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: AddPackageToRegistry
  Author:    herzogs2
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
  _Reg := TRegistry.Create;
  try
    _Reg.RootKey := _RootKey;
    if _Reg.ValueExists(_PackageName) then begin
      trace(5,'AddPackageToRegistry: The Packages <%s> is already registered.',[_PackageName]);
      exit;
    end;
    _Reg.OpenKey(_Key,false);
    try
      _Reg.WriteString(_PackageName,_PackageDescription);
      trace(5,'AddPackageToRegistry: Successfully installed the  Packages <%s> for <%s>.',[_PackageName,_Key]);
      Result:=true;
    except
      on e:exception do trace(1,'Warning in AddPackageToRegistry: Could not add the package <%s> for <%s> to the registry.You need to have Admin rights for this computer. <%s>.',[_PackageName,_key,e.message]);
    end;
  finally
    _Reg.CloseKey;
    _Reg.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: CanInstallPackage
  Author:    herzogs2
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
  Author:    herzogs2
  Date:      30-Aug-2002
  Arguments: _PackageName,_PackageDirectory:String;_DelphiVersion:Integer
  Result:    None
  Description: install the delphi package <_PackageName> (.dpk) which is located in the
               directory <_PackageDirectory> for delphi version <_DelphiVersion>.
               The parameter <_PackageDirectory> points to the location of the <.bpl> file.
-----------------------------------------------------------------------------}
function InstallPackage(_PackageName,_PackageDirectory,_PackageDescription,_PackageLibSuffix:String;_DelphiVersion:Integer):string;
var
_PackageKey:String;
_RegFile:TStrings;
_RegFileName:string;
_BplFilename:string;
begin
  result:='Failed';
  _PackageName:=ReadProjectFilenameFromDProj(_PackageName);
  if _PackageDescription='' then _PackageDescription:='(untitled)';
  if lowercase(ExtractFileExt(_PackageName))<>'.dpk' then begin
    Result:='-';
    exit;
  end;
  if not GetIDERootKey(_DelphiVersion,_PackageKey) then begin
    trace(3,'Problem in InstallPackage: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;

  if not CanInstallPackage(_PackageName) then begin
    trace(5,'InstallPackage: The package <%s> is a runtime only package. It will not be installed into the IDE.',[_PackageName]);
    Result:='-';
    exit;
  end;
  _RegFile:=TStringList.create;
  try
    if _PackageLibSuffix<>'' then _BplFilename:=lowercase(_PackageDirectory+ExtractFilenameOnly(_PackageName)+_PackageLibSuffix+'.bpl')
                             else _BplFilename:=lowercase(_PackageDirectory+ExtractFilenameOnly(_PackageName)+'.bpl');
    _RegFile.add('Windows Registry Editor Version 5.00');
    _RegFile.add('');
    _PackageKey:=_PackageKey+'Known Packages';
    if AddPackageToRegistry(HKEY_CURRENT_USER,_PackageKey,_BplFilename,_PackageDescription) then begin
      trace(2,'Installed the package <%s> into the Delphi IDE for the current user.',[ExtractFileName(_PackageName)]);
      _RegFile.add('[HKEY_CURRENT_USER\'+_PackageKey+']');
      _RegFile.add('"'+PrepapreRegistryPath(_BplFilename)+'"="'+_PackageDescription+'"');
      result:='Current User';
    end;

    if AddPackageToRegistry(HKEY_LOCAL_MACHINE,_PackageKey,_BplFilename,_PackageDescription) then begin
      trace(2,'Installed the package <%s> into the Delphi IDE for the all users on this computer.',[ExtractFileName(_PackageName)]);
      _RegFile.add('[HKEY_LOCAL_MACHINE\'+_PackageKey+']');
      _RegFile.add('"'+PrepapreRegistryPath(_BplFilename)+'"="'+_PackageDescription+'"');
      if result='Current User' then result:=result+'/All Users'
                               else result:='All Users';
    end;

    if _RegFile.Count>0 then begin  // create a .reg file if needed.
      try
        _RegFile.add('');
        _RegFileName:='Register_Package_'+ChangeFileExt(ExtractFilename(_PackageName),'.reg');
        if FCreateBatchFile then _RegFile.SaveToFile(ExtractFilePath(FBatchFilename)+_RegFileName);
        FBatchFile.add(_RegFileName);
      except
        On E:Exception do Showmessage(format('Could not save file <%s> because <%s>.',[_RegFileName,E.Message]));
      end;
    end;
  finally
    _RegFile.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: UninstallPackage
  Author:    herzogs2
  Date:      30-Aug-2002
  Arguments: _PackageName,_PackageDirectory:String;_DelphiVersion:Integer
  Result:    None
  Description: uninstall the package <_PackageName>.
               This deletes the registry keys.
-----------------------------------------------------------------------------}
function UnInstallPackage(_PackageName,_PackageDirectory,_PackageLibSuffix:String;_DelphiVersion:Integer):boolean;
var
_sDelphiVersion:String;
_PackageBaseKey:String;
_PackageKey:string;
_PackageValue:string;
_RegFile:TStrings;
_RegFileName:string;
_RegFilePackagePath:string;
_BplFilename:string;
begin
  result:=false;
  _PackageName:=ReadProjectFilenameFromDProj(_PackageName);
  if lowercase(ExtractFileExt(_PackageName))<>'.dpk' then exit;
//  trace(1,'Removed Package <%s>,<%s> from Registry for Delphi %d.',[_PackageName,_PackageDirectory,_DelphiVersion]);
  _sDelphiVersion:=inttostr(_DelphiVersion)+'.0';
  if not GetIDERootKey(_DelphiVersion,_PackageBaseKey) then begin
    trace(3,'Problem in UnInstallPackage: Could not find key for Delphi Version <%d>.',[_DelphiVersion]);
    exit;
  end;

  _RegFilePackagePath:=PrepapreRegistryPath(_PackageDirectory);
  if _PackageLibSuffix<>'' then _BplFilename:=lowercase(ExtractFilenameOnly(_PackageName)+_PackageLibSuffix+'.bpl')
                           else _BplFilename:=lowercase(ExtractFilenameOnly(_PackageName)+'.bpl');

  _RegFile:=TStringList.create;
  try
    _RegFile.add('Windows Registry Editor Version 5.00');
    _RegFile.add('');
    _PackageKey:=_PackageBaseKey+'Known Packages';
    _RegFile.add('[HKEY_CURRENT_USER\'+_PackageKey+']');
    _RegFile.add('"'+_RegFilePackagePath+_BplFilename+'"=-');
    _RegFile.add('[HKEY_LOCAL_MACHINE\'+_PackageKey+']');
    _RegFile.add('"'+_RegFilePackagePath+_BplFilename+'"=-');
    _PackageValue:=lowercase(_PackageDirectory+_BplFilename);
    if RemoveValueFromRegistry(HKEY_CURRENT_USER, _PackageKey,_PackageValue) then trace(3,'Un-Installed the package <%s> from the Delphi IDE for the current user on this computer.',[_PackageDirectory+_BplFilename]);
    if RemoveValueFromRegistry(HKEY_LOCAL_MACHINE,_PackageKey,_PackageValue) then trace(3,'Un-Installed the package <%s> from the Delphi IDE for all users of this computer.',[_PackageDirectory+_BplFilename]);
    _PackageValue:=AddTag(_PackageValue,_DelphiVersion);
    if RemoveValueFromRegistry(HKEY_CURRENT_USER, _PackageKey,_PackageValue) then trace(3,'Un-Installed the package <%s> from the Delphi IDE for the current user on this computer.',[_PackageDirectory+_BplFilename]);
    if RemoveValueFromRegistry(HKEY_LOCAL_MACHINE,_PackageKey,_PackageValue) then trace(3,'Un-Installed the package <%s> from the Delphi IDE for all users of this computer.',[_PackageDirectory+_BplFilename]);
    _PackageKey:=_PackageBaseKey+'Disabled Packages';
    _RegFile.add('[HKEY_CURRENT_USER\'+_BplFilename+']');
    _RegFile.add('"'+_RegFilePackagePath+_BplFilename+'"=-');
    _RegFile.add('[HKEY_LOCAL_MACHINE\'+_PackageKey+']');
    _RegFile.add('"'+_RegFilePackagePath+_BplFilename+'"=-');
    _RegFile.add('');
    _PackageValue:=lowercase(_PackageDirectory+_BplFilename);
    if RemoveValueFromRegistry(HKEY_CURRENT_USER, _PackageKey,_PackageValue) then trace(3,'Un-Installed the package <%s> from the Delphi IDE for the current user on this computer.',[_PackageDirectory+_BplFilename]);
    if RemoveValueFromRegistry(HKEY_LOCAL_MACHINE,_PackageKey,_PackageValue) then trace(3,'Un-Installed the package <%s> from the Delphi IDE for all users of this computer.',[_PackageDirectory+_BplFilename]);
    _PackageValue:=AddTag(_PackageValue,_DelphiVersion);
    if RemoveValueFromRegistry(HKEY_CURRENT_USER, _PackageKey,_PackageValue) then trace(3,'Un-Installed the package <%s> from the Delphi IDE for the current user on this computer.',[_PackageDirectory+_BplFilename]);
    if RemoveValueFromRegistry(HKEY_LOCAL_MACHINE,_PackageKey,_PackageValue) then trace(3,'Un-Installed the package <%s> from the Delphi IDE for all users of this computer.',[_PackageDirectory+_BplFilename]);
    try
      _RegFileName:='UnRegister_Package_'+ChangeFileExt(ExtractFilename(_PackageName),'.reg');
      if FCreateBatchFile then _RegFile.SaveToFile(ExtractFilePath(FBatchFilename)+_RegFileName);
      FBatchFile.add(_RegFileName);
    except
      On E:Exception do Showmessage(format('Could not save file <%s> because <%s>.',[_RegFileName,E.Message]));
    end;
  finally
    if assigned(_RegFile) then FreeAndNil(_RegFile);
  end;
  result:=true;
end;

{-----------------------------------------------------------------------------
  Procedure: ReadPackageInfoDelphi
  Author:    herzogs2
  Date:      22-Aug-2002
  Arguments: _PackageName:string
  Result:    String
  Description: get the package description text out of the dpk-file.
               For delphi 1..7
-----------------------------------------------------------------------------}
function ReadPackageInfoDelphi(_PackageName:string;var Description:string;var LibSuffix:string):boolean; // get the description and lib suffix
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
  Author:    herzogs2
  Date:      23-Dez-2009
  Arguments: const _PackageName:string;var Description:string
  Result:    boolean
  Description: read information from package file.
-----------------------------------------------------------------------------}
function ReadPackageInfo(const _PackageName:string;var Description:string;var LibSuffix:string):boolean; // get the description text
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
function WinExecAndWait(FileName,CommandLine,WorkPath: string; Visibility: Integer;var Output:String): LongWord;
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
    StartupInfo.hStdOutput := StdOutPipeWrite;
    StartupInfo.hStdError  := StdOutPipeWrite;
    if not CreateProcess(
      nil,
      PChar(FileName+' '+CommandLine), // command line.
      nil,
      nil, { pointer to thread security attributes }
      true, { handle inheritance flag }
      CREATE_NEW_CONSOLE or { creation flags }
      NORMAL_PRIORITY_CLASS,
      nil, { pointer to new environment block }
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
      until (Result <> STILL_ACTIVE) and (not Application.Terminated);
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
  Author:    herzogs2
  Date:      29-Aug-2002
  Arguments:
  _Compiler   --> full name and path to bcc32.exe
  _CompilerSwitches, --> compiler switches like -W -H -B for example
  _ProjectName --> the name of the project to be compiled (can be .dpr or .dpk )
  _TargetPath,  --> output target path
  Var Output:String   --> output text of the compiler.
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function CompileProject(_Compiler,_CompilerSwitches,_ProjectName,_TargetPath,_DCUPath,_WorkPath:string;var Output:String;const _DelphiVersion:integer):boolean; // compile the package
var
_commandLine:string;
_returnValue:Cardinal;
begin
  Result:=false;
  if not fileexists(_Compiler) then begin
    showmessage(format('Could not find the Delphi Compiler file <%s>. Please check settings.',[_Compiler]));
    trace(1,'Problem in CompileProject: Problem, could not find the Delphi Compiler file <%s>.',[_Compiler]);
    exit;
  end;

  if not FileExists(_ProjectName) then begin
    showmessage(format('Could not find the Project file <%s>. Please check if your BPG-File is still ok.',[_ProjectName]));
    trace(1,'Problem in CompileProject: Problem, could not find the Project file <%s>.',[_ProjectName]);
    exit;
  end;

  if not CheckDirectory(_WorkPath) then begin
    trace(1,'Problem in CompileProject: Problem, could not find the work path <%s>.',[_WorkPath]);
    exit;
  end;

  if not CheckDirectory(_TargetPath) then begin
    trace(1,'Problem in CompileProject: Problem, could not find the target path <%s>.',[_TargetPath]);
    exit;
  end;

  if not CheckDirectory(_DCUPath) then begin
    trace(1,'Problem in CompileProject: Problem, could not find the target path <%s>.',[_DCUPath]);
    exit;
  end;

  if (LastPos(_DCUPath,'\')=length(_DCUPath)) and
     (length(_DCUPath)>2) then delete(_DCUPath,length(_DCUPath),1);

  if (LastPos(_TargetPath,'\')=length(_TargetPath)) and
     (length(_TargetPath)>2) then delete(_TargetPath,length(_TargetPath),1);
  trace(5,'*************************************************************************************',[]);
  trace(5,'Compile Project <%s>.',[_ProjectName]);
  trace(5,'Compiler is <%s>.',[_compiler]);
  trace(5,'Work path is <%s>.',[_WorkPath]);
  trace(5,'Output path is <%s>.',[_TargetPath]);
  trace(5,'DCU path is <%s>.',[_DCUPath]);
  _commandLine:=_CompilerSwitches+' "'+_ProjectName;
  if _TargetPath<>'' then _commandLine:=_commandLine+'" -LE"'+_TargetPath+'"'+' -LN"'+_TargetPath+'"';
  if _DCUPath<>'' then begin
    if _DelphiVersion<=7 then _commandLine:=_commandLine+' -N"'+_DCUPath+'"'
                         else _commandLine:=_commandLine+' -N0"'+_DCUPath+'"';
  end;
  trace(5,'Command line is %s.',[_commandLine]);
  FBatchFile.Add('cd "'+ExtractFilePath(_ProjectName)+'"');
  FBatchFile.Add('"'+_compiler+'" '+_commandLine);
  _returnValue:=WinExecAndWait(_compiler,
                     _commandLine,
                     _WorkPath,
                     SW_HIDE,
                     Output);
  if _returnValue=0 then begin
    trace(5,'CompileProject: Successfully build Project file <%s>.',[_ProjectName]);
    Result:=True;
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
  Arguments: _filename:string;var lst:TStrings
  Result:    None
  Description:  read packages&projects from a .groupproj-file <_filename> into the stringlist <lst>.
-----------------------------------------------------------------------------}
procedure ReadPackageListfromGroupProjFileToStrings(_filename:string;var lst:TStrings); // read the package names from the delphi project group file <.bdsgroup>
var
i:integer;
_Projectname:String;
_errormsg:string;
begin
  if not assigned(lst) then exit;
  if not fileExists(_filename) then begin
    trace(5,'ReadPackageListfromGroupProjFileToStrings: Could not find the file <%s>.',[_filename]);
    exit;
  end;
  lst.Clear;
  i:=0;
  repeat
    if not ReadNodeText(_filename,format('//Projects[%d]/@Include',[i]),_Projectname,_errormsg) then exit;
    if  _Projectname<>'' then lst.Add(_Projectname);
    inc(i);
  until (_Projectname='') or (i>10000);
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
_Strings:TStrings;
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
procedure ReadPackageListfromBPGFileToStrings(_filename:string;var lst:TStrings);
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
  if not assigned(lst) then exit;
  if not fileExists(_filename) then begin
    trace(5,'ReadPackageListfromBPGFileToStrings: Could not find the file <%s>.',[_filename]);
    exit;
  end;
  _BPGFile:=TStringList.Create;
  _BPGFile.LoadFromFile(_filename);
  lst.Clear;
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
        lst.add(trim(_line));
        break;
      end;
    end;
    _Project:=GetField(' ',_Projects);
    _Project:=RemoveSlash(_Project);
  end;
  _BPGFile.free;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadPackageListfromBDSGroupFileToStrings
  Author:    sam
  Date:      13-Jun-2008
  Arguments: _filename:string;var lst:TStrings
  Result:    None
  Description: read packages&projects from a .bdsgroup-file <_filename> into the stringlist <lst>.
-----------------------------------------------------------------------------}
procedure ReadPackageListfromBDSGroupFileToStrings(_filename:string;var lst:TStrings); // read the package names from the delphi project group file <.bdsgroup>
var
  i:integer;
  _XMLFile:xmlintf.IXMLDocument;
  _Project:String;
begin
  if not assigned(lst) then exit;
  if not fileExists(_filename) then begin
    trace(5,'ReadPackageListfromBDSGroupFileToStrings: Could not find the file <%s>.',[_filename]);
    exit;
  end;
  lst.Clear;
  _XMLFile:=newXMLDocument;
  try
    _XMLFile.LoadFromFile(_filename);
    _XMLFile.active:=true;
    for i:=0 to _XMLFile.DocumentElement.ChildNodes['Default.Personality'].ChildNodes['Projects'].ChildNodes.Count-2 do begin
      _Project:=_XMLFile.DocumentElement.ChildNodes['Default.Personality'].ChildNodes['Projects'].ChildNodes[i].text;
      lst.add(trim(_Project));
    end;
  finally
    _XMLFile.active:=false;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadPackageListfromFile
  Author:    sam
  Date:      13-Jun-2008
  Arguments: _filename:string;var lst:TStrings
  Result:    None
  Description: read packages&projects from the group-file <_filename> into the stringlist <lst>.
-----------------------------------------------------------------------------}
procedure ReadPackageListfromFile(_filename:string;var lst:TStrings);overload;
begin
  if lowercase(ExtractFileExt(_filename))='.bpg'       then ReadPackageListfromBPGFileToStrings(_filename,lst) else
  if lowercase(ExtractFileExt(_filename))='.bdsgroup'  then ReadPackageListfromBDSGroupFileToStrings(_filename,lst) else
  if lowercase(ExtractFileExt(_filename))='.groupproj' then ReadPackageListfromGroupProjFileToStrings(_filename,lst);
end;

{-----------------------------------------------------------------------------
  Procedure: DeleteFile
  Author:    herzogs2
  Date:      22-Aug-2002
  Arguments: _Filename:String
  Result:    None
  Description: delete the  file if exists.
-----------------------------------------------------------------------------}
function DeleteFile(const _Filename:String):boolean;  // delete the bpl file.
resourcestring
cCouldNotDeleteFile='Could not delete file <%s>. Error <%s>.';
begin
  result:=false;
  if _Filename='' then exit;
  if not fileexists(_Filename) then begin
    trace(3,'DeleteFile: Can not Delete the file <%s> because it does not exist.',[_Filename]);
    exit;
  end;
  try
    if not SysUtils.DeleteFile(_Filename) then exit;
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
  Procedure: DelphiStarted
  Author:
  Date:      30-Aug-2002
  Arguments: None
  Result:    Boolean
  Description:
-----------------------------------------------------------------------------}
function isDelphiStarted(const _DelphiVersion:Integer): Boolean;
begin
  result := GetProcessID(GetDelphiApplication(_DelphiVersion))>0;
end;

{-----------------------------------------------------------------------------
  Procedure: ShutDownDelphi
  Author:
  Date:      30-Aug-2002
  Arguments: Blocking : Boolean
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure ShutDownDelphi(const _DelphiVersion:Integer;_Blocking : Boolean);
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
begin
  _FileName := GetDelphiApplication(_DelphiVersion);
  if not FileExists(_FileName) then exit;
  VerifyRegistry(_DelphiVersion);
  trace(5,'StartUpDelphi:Try to start Delphi from <%s>.',[_FileName]);
{$IF CompilerVersion < 20.0}
  ShellExecute(0, 'open', PChar(_FileName), PChar(' /ns "'+_ProjectName+'"'), nil, SW_SHOWNORMAL);
{$ELSE}
  ShellExecute(0, 'open', PWideChar(_FileName), PWideChar(' /ns "'+_ProjectName+'"'), nil, SW_SHOWNORMAL);
{$IFEND}
end;

{-----------------------------------------------------------------------------
  Procedure: AbsoluteFilename
  Author:    Sami
  Date:      03-Feb-2004
  Arguments: _sourcedir, _filename: string
  Result:    string
  Description: make the relative filename <_filename> into an absolute filename with path <_realPath>.
-----------------------------------------------------------------------------}
function AbsoluteFilename(_realpath,_filename: string): string;
var
_pos:integer;
_len:integer;
_relativepath:string;
begin
  trace(5,'Enter method <AbsoluteFilename> with realpath <%s> and filename <%s>.',[_realpath,_filename]);
  if (pos('\\',_filename)=1) or               // looks like the filename contains already a absolute path.
     (pos(':\',_filename)>0) then begin
    result:=_filename;
    trace(5,'Leave method <AbsoluteFilename> with filename <%s>.',[result]);
    exit;
  end;

  if (pos('..\',_filename)=0) and
     (pos('.\',_filename)=0) then begin    // its not a relative path, leave here.
    _realpath:=IncludeTrailingPathDelimiter(_realpath);
    result:=_realpath+_filename;
    trace(5,'Leave method <AbsoluteFilename> with filename <%s>.',[result]);
    exit;
  end;

  _relativepath:=extractFilepath(_filename);
  _filename:=extractFilename(_filename);


  _pos:=Pos('.\',_relativepath);
  if _pos=1 then begin
    delete(_relativepath,1,2);
    result:=_realpath+_relativepath+_filename;
    trace(5,'Leave method <AbsoluteFilename> with filename <%s>.',[result]);
    exit;
  end;
  _pos:=Pos('..\',_relativepath);
  if (_pos=0) then begin
    if (pos('\\',_relativepath)>0) or
       (pos(':\',_relativepath)>0) then begin
      result:=_relativepath+_filename;
      trace(5,'Leave method <AbsoluteFilename> with filename <%s>.',[result]);
      exit;
    end else begin
      result:=_realpath+_relativepath+_filename;
      trace(5,'Leave method <AbsoluteFilename> with filename <%s>.',[result]);
      exit;
    end;
  end;

  while _pos>0 do begin
    Delete(_relativepath,1,_pos+2);
    _pos:=LastPos(_realpath,'\');
    _len:=length(_realpath);
    if _pos=_len then delete(_realpath,_pos,1);
    _pos:=LastPos(_realpath,'\');
    _len:=length(_realpath);
    if _pos>0 then delete(_realpath,_pos,_len);
    _pos:=Pos('..\',_relativepath);
  end;
  _realpath:=IncludeTrailingPathDelimiter(_realpath);
  result:=_realpath+_relativepath+_filename;
  trace(5,'Leave method <AbsoluteFilename> with filename <%s>.',[result]);
end;

{-----------------------------------------------------------------------------
  Procedure: AbsolutePath
  Author:    sam
  Date:      12-Mrz-2005
  Arguments: _basepath,_path:string
  Result:    string
  Description: converts the path <_path> into an absolute pathname.
-----------------------------------------------------------------------------}
function  AbsolutePath(_basepath,_path:string;const _DelphiVersion:integer):string;
var
_pos:integer;
_len:integer;
begin
  _path:=ReplaceTag(_path,_DelphiVersion);

  if (pos('\\',_path)=1) or               // looks like the filename contains already a absolute path.
     (pos(':\',_path)>0) then begin
    result:=IncludeTrailingPathDelimiter(_path);
    trace(5,'Leave method <AbsolutePath> with filename <%s>.',[result]);
    exit;
  end;

  if (pos('..\',_path)=0) and
     (pos('.\',_path)=0) then begin    // its not a relative path, leave here.
    if _path<>'' then begin
      _path:=IncludeTrailingPathDelimiter(_path);
      if (pos('\',_path)=1) then Delete(_path,1,1); // remove leading path delimiter
    end;
    result:=IncludeTrailingPathDelimiter(_basepath)+_path;
    trace(5,'Leave method <AbsolutePath> with filename <%s>.',[result]);
    exit;
  end;

  _pos:=Pos('..\',_path);
  if _pos=0 then begin
    if _path<>'' then begin
      _path:=IncludeTrailingPathDelimiter(_path);
      if (pos('\',_path)=1) then Delete(_path,1,1); // remove leading path delimiter
    end;
    result:=IncludeTrailingPathDelimiter(_basepath)+_path;
    trace(5,'Leave method <AbsolutePath> with filename <%s>.',[result]);
    exit;
  end;

  while _pos>0 do begin
    Delete(_path,1,_pos+2);
    _pos:=LastPos(_basepath,'\');
    if _pos=0 then begin
      result:='';
      trace(1,'Error in AbsolutePath: Relative path <%s> can not be matched to base path <%s>.',[_path,_basepath]);
      exit;
    end;
    _len:=length(_basepath);
    if _pos=_len then delete(_basepath,_pos,1);
    _pos:=LastPos(_basepath,'\');
    _len:=length(_basepath);
    if _pos>0 then delete(_basepath,_pos,_len);
    _pos:=Pos('..\',_path);
  end;
  if _path<>'' then begin
    _path:=IncludeTrailingPathDelimiter(_path);
    if (pos('\',_path)=1) then Delete(_path,1,1); // remove leading path delimiter
  end;
  result:=IncludeTrailingPathDelimiter(_basepath)+_path;
  trace(5,'Leave method <AbsolutePath> with filename <%s>.',[result]);
end;

{-----------------------------------------------------------------------------
  Procedure: MakeAbsolutePath
  Author:    herzogs2
  Date:      30-Mrz-2010
  Arguments: _basePath,_path.string;_DelphiVersion:integer
  Result:    string
  Description: convert all entries in the path-list <_path> into a path-list with absolut path-names.
  e.g. $(Delphi)\bin;$(ProgramFiles)\test;..\..\library; will be converted into absolute path names.
-----------------------------------------------------------------------------}
function MakeAbsolutePath(_basePath,_path:string;_DelphiVersion:integer):string;
var
i:integer;
_item:string;
_list:TStrings;
begin
  result:='';
  _list:=TStringList.create;
  try
    while _path<>'' do begin
      _item:=GetField(';',_path);
      _item:=lowercase(AbsolutePath(_basePath,_item,_DelphiVersion));
      if _list.IndexOf(_item)=-1 then _list.Add(_item);
    end;
    for i:=0 to _list.Count-1 do result:=result+_list[i]+';';
  finally
    _list.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: RelativePath
  Author:    sam
  Date:      12-Mrz-2005
  Arguments: _basepath,_path:string
  Result:    string
  Description: converts the path <_path> into a realtive pathname.
-----------------------------------------------------------------------------}
function  RelativePath(_basepath,_path:string;const _DelphiVersion:integer;const _ReplaceTags:boolean=true):string;
begin
  if _ReplaceTags then _path:=AddTag(_path,_DelphiVersion);
  result:=ExtractRelativePath(_basepath,_path)
end;

{-----------------------------------------------------------------------------
  Procedure: RelativeFilename
  Author:    sam
  Date:      14-Mrz-2005
  Arguments: _basepath,_filename:string
  Result:    string
  Description:
-----------------------------------------------------------------------------}
function  RelativeFilename(const _basepath,_filename:string;const _DelphiVersion:integer):string; // converts the filename <_filename> into a relative filename.
var
_path:string;
begin
  _path:=Relativepath(_basePath,extractFilePath(_filename),_DelphiVersion);
  result:=_path+ExtractFilename(_filename);
end;


initialization
  FBatchFile:=TStringList.create;
finalization
  FBatchFile.Free;
end.
