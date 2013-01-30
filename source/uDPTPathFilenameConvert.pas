{*-----------------------------------------------------------------------------
 Unit Name: uDPTPathFilenameConvert
 Author:    sam
 Date:      03-Nov-2012
 Purpose:   convert between relative and absolute filenames/filepaths.
 History:
-----------------------------------------------------------------------------}
unit uDPTPathFilenameConvert;

interface

function AbsoluteFilename(_realpath,_filename: string): string;
function AbsolutePath(_basepath,_path:string;const _DelphiVersion:integer):string; // converts the path <_path> into an absolute pathname.
function RelativeFilename(const _basepath,_filename:string;const _DelphiVersion:integer):string; // converts the filename <_filename> into a relative filename.
function RelativePath(_basepath,_path:string;const _DelphiVersion:integer;const _ReplaceTags:boolean=true):string; // converts the path <_path> into a realtive pathname.
function RelativePaths(_basepath,_paths:string;const _DelphiVersion:integer):string; // converts a list of paths to relative paths.
function MakeAbsolutePath(_basePath,_path:string;_DelphiVersion:integer):string; // <_path> is a semicolon seperated path-list which will be converted in to absolut path-list.

implementation

uses
  uDPTMisc,
  uDPTDelphiPackage,
  SysUtils,
  Classes;

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
      if _path<>'' then result:=result+_path+';';
    end
    else trace(2,'RelativePaths: The path <%s> does not exist. Removed!',[_absolutepath]);
    _path:=Getfield(';',_paths);
  end;
  result:=RemoveDoublePathEntries(result);
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

end.
