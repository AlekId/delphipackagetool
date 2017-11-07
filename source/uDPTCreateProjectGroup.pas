{*-----------------------------------------------------------------------------
 Unit Name: uDPTCreateProjectGroup
 Author:    sam
 Date:      03-Nov-2012
 Purpose:
 History:
-----------------------------------------------------------------------------}
unit uDPTCreateProjectGroup;

interface

uses
  stdctrls;

procedure CreateProjectGroupFile(const _lstProjectFiles:TListBox;const _projectGroupFilename:string;const _DelphiVersion:integer;const _LibSuffix:string);

implementation

uses
  classes,
  sysutils,
  forms,
  windows,
  uDPTDefinitions,
  uDPTMisc,
  uDPTPathFilenameConvert,
  uDPTDelphiPackage;

{-----------------------------------------------------------------------------
  Procedure: CreateBPGFile
  Author:    sam
  Date:      04-Jun-2008
  Arguments: const _lstProjectFiles:TListBox;const _bpgFilename:string
  Result:    None
  Description: create a bpg-file with all the projects in the list <_lstProjectFiles>.
-----------------------------------------------------------------------------}
procedure CreateBPGFile(const _lstProjectFiles:TListBox;const _projectGroupFilename:string;const _DelphiVersion:integer;const _LibSuffix:string);
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

  _Files:=TStringList.create;
  try
    _Files.loadfromfile(extractFilePath(Application.exename)+cBPGTemplateFilename);
    _Projects:='PROJECTS =';
    for i:=0 to _lstProjectFiles.Items.count-1 do begin
      _ProjectFilename:=AbsoluteFilename(ExtractFilePath(_projectGroupFilename),_lstProjectFiles.Items[i]);
      _ProjectType:=DetermProjectType(_ProjectFilename,_projectGroupFilename,_DelphiVersion);
      _Projects:=_Projects+OutputFilename(_ProjectFilename,_ProjectType,_LibSuffix);
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

      _filename:=OutputFilename(_ProjectFilename,_ProjectType,_LibSuffix);
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
  Author:    sam
  Date:      04-Jun-2008
  Arguments: const _lstProjectFiles:TListBox;const _bpgFilename:string
  Result:    None
  Description: create a bdsgroup-file with all the projects in the list <_lstProjectFiles>.
-----------------------------------------------------------------------------}
procedure CreateBDSGroup(const _lstProjectFiles:TListBox;const _projectGroupFilename:string;const _DelphiVersion:integer;_LibSuffix:string);
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
_LibsuffixFromPackageFile:string;
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
                 ReadpackageInfo(_packagefilename,_description,_LibsuffixFromPackageFile);
                 if _LibsuffixFromPackageFile<>'' then _Libsuffix:=_LibsuffixFromPackageFile;
                 _lineToInsert:=format('      <Projects Name="%s">%s</Projects>',[extractfilenameonly(_filename)+_Libsuffix+'.bpl',ExtractRelativePath(ExtractFilePath(_projectGroupFilename),_lstProjectFiles.Items[i])]);
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
  Author:    sam
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
      _EndTargetSection:=FindLine(_file,'<Import Project="',_text);
      if _EndTargetSection=-1 then begin
        trace(2,'Problem in CreateGroupProj: Could not find the end of target section in file <%s>. Something wrong here.',[_projectGroupFilename]);
        exit;
      end;
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
    if RenameFile(_projectGroupFilename,changeFileExt(_projectGroupFilename,'.~old')) then trace(1,'Renamed the file <%s> to <%s>.',[_projectGroupFilename,changeFileExt(_projectGroupFilename,'.~old')]);
    _File.SaveToFile(_projectGroupFilename);
    trace(1,'Saved ProjectGroup-File <%s>.',[_projectGroupFilename]);
  finally
    _File.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: CreateProjectGroupFile
  Author:    sam
  Date:      06-Nov-2008
  Arguments: const _lstProjectFiles:TListBox;const _projectGroupFilename:string;const _DelphiVersion:integer
  Result:    None
  Description: create a project group file (either .bpg or .bdsgroup or .groupproj).
-----------------------------------------------------------------------------}
procedure CreateProjectGroupFile(const _lstProjectFiles:TListBox;const _projectGroupFilename:string;const _DelphiVersion:integer;const _LibSuffix:string);
var
_fileExt:string;
begin
  _fileExt:=lowercase(ExtractFileExt(_ProjectGroupFilename));
  if _fileExt='.bpg'       then CreateBPGFile(_lstProjectFiles,_projectGroupFilename,_DelphiVersion,_LibSuffix)  else
  if _fileExt='.bdsgroup'  then CreateBDSGroup(_lstProjectFiles,_projectGroupFilename,_DelphiVersion,_LibSuffix) else
  if _fileExt='.groupproj' then CreateGroupProj(_lstProjectFiles,_projectGroupFilename,_DelphiVersion);
end;


end.
