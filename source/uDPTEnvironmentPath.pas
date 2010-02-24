{-----------------------------------------------------------------------------
 Unit Name: uDPTEnvironmentPath
 Author:    herzogs2
 Date:      18-Jun-2007
 Purpose:   methods to read/write/change/verify the environment variable of windows.
 History:
-----------------------------------------------------------------------------}

unit uDPTEnvironmentPath;

interface
uses Classes;

function GetEnvVarValue(const VarName: string): string; // get the environment path of the application
function SetEnvVarValue(const VarName: string;Value:string): boolean; // set the environment path of the application
function SetGlobalEnvironment(const VarName, Value: string;const User: Boolean = True): Boolean; // set the system environment path
function GetGlobalEnvironment(const VarName:string): String;  // get the system environment path.
function GetGlobalEnvironmentPathList:TStrings;
function SetGlobalEnvironmentPathList(const _list:TStrings):boolean;
function CleanUpGlobalEnvironment:boolean; // remove double entries and entries where the path does not exist.
function AddGlobalEnvironmentPath(const _path:string):boolean;
function RemoveDoublesFromList(var list:TStrings):boolean;
function RemoveInvalidEnvironmentPath(var list:TStrings):boolean;
function IsPathInEnvironmentPath(const _path:string):boolean; // checks if the path <_path> is already in the environment variable.

implementation

uses
     SysUtils,
     Registry,
     windows,
     Messages,
     Dialogs,
     uDPTMisc;


{-----------------------------------------------------------------------------
  Procedure: IsPathInEnvironmentPath
  Author:    herzogs2
  Date:      18-Jun-2007
  Arguments: const _path:string
  Result:    boolean
  Description: returns <true> if the path <_path> is already in the environment variable.
-----------------------------------------------------------------------------}
function IsPathInEnvironmentPath(const _path:string):boolean; // checks if the path <_path> is already in the environment variable.
var
_pathList:TStrings;
_entry:string;
begin
  result:=false;
  _pathList:=GetGlobalEnvironmentPathList;
  try
    _entry:=lowercase(_path);
    if IsLastChar('\',_entry) then Delete(_entry,length(_entry),1);
    if (_pathList.IndexOf(_entry)=-1) and
       (_pathList.IndexOf(IncludeTrailingPathDelimiter(_entry))=-1) then exit;
    result:=true;
  finally
    _pathList.free;
  end;
end;

function GetGlobalEnvironmentPathList:TStrings;
var
_s:string;
_path:string;
begin
  result:=TStringList.create;
  _s:=GetGlobalEnvironment('PATH');
  while _s<>'' do begin
    _path:=lowercase(trim(GetField(';',_s)));
    if _path<>'' then begin
      if result.IndexOf(_path)=-1 then result.add(_path);
    end;
  end;
end;

function SetGlobalEnvironmentPathList(const _list:TStrings):boolean;
var
i:integer;
_path:string;
_entry:string;
begin
  _path:='';
  for i:=0 to _list.count-1 do begin
    _entry:=_list[i];
    if IsLastChar('\',_entry) then Delete(_entry,length(_entry),1);
    _path:=_path+_entry+';';
  end;
  result:=SetGlobalEnvironment('PATH',_path,false);
end;

function RemoveDoublesFromList(var list:TStrings):boolean;
var
i:integer;
_tmp:TStringList;
_entry:string;
begin
  result:=false;
  if not assigned(list) then exit;
  _tmp:=TStringList.create;
  try
    _tmp.Assign(list);
    list.Clear;
    for i:=0 to _tmp.count-1 do begin
      _entry:=lowercase(_tmp[i]);
      if IsLastChar('\',_entry) then delete(_entry,length(_entry),1);
      if (list.IndexOf(_entry)=-1) and
         (list.IndexOf(IncludeTrailingPathDelimiter(_entry))=-1) then list.add(_entry);
    end;
    result:=true;
  finally
    _tmp.free;
  end;
end;

function RemoveInvalidEnvironmentPath(var list:TStrings):boolean;
var
i:integer;
_path:string;
begin
  i:=0;
  result:=false;
  if not assigned(list) then exit;
  while i<list.count do begin
    _path:=list[i];
    if pos('%',_path)>0 then begin
      inc(i);
      continue;
    end;
    if not directoryExists(_path) then begin
      trace(1,'The directory <%s> could not be found. Remove it from the Environments Variable.',[_path]);
      list.delete(i);
      continue;
    end;
    inc(i);
  end;
  result:=true;
end;


function AddGlobalEnvironmentPath(const _path:string):boolean;
var
_pathList:TStrings;
begin
  result:=false;
  _pathList:=GetGlobalEnvironmentPathList;
  try
    if _pathList.IndexOf(lowercase(_path))>-1 then exit;
    _pathList.Add(lowercase(_path));
    trace(1,'Added the directory <%s> to the Environments Variable.',[_path]);
    RemoveDoublesFromList(_pathList);
    result:=SetGlobalEnvironmentPathList(_pathList);
  finally
    _pathList.free;
  end;
end;


{-----------------------------------------------------------------------------
  Procedure: CleanUpGlobalEnvironment
  Author:    sam
  Date:      10-Mrz-2007
  Arguments: None
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function CleanUpGlobalEnvironment:boolean; // remove double entries and entries where the path does not exist.
var
_paths:TStrings;
begin
  _paths:=GetGlobalEnvironmentPathList;
  try
// remove doubles
   RemoveDoublesFromList(_paths);
// check if path really exists.
   RemoveInvalidEnvironmentPath(_paths);
// write the list back.
   result:=SetGlobalEnvironmentPathList(_paths);
  finally
    _paths.free;
  end;
end;


function SetGlobalEnvironment(const VarName, Value: string;const User: Boolean = True): Boolean;
resourcestring
  REG_MACHINE_LOCATION = 'System\CurrentControlSet\Control\Session Manager\Environment';
  REG_USER_LOCATION = 'Environment';
var
_Reg:TRegistry;
begin
  _Reg:=TRegistry.Create;
  try
    if User then { User Environment Variable }
      Result := _Reg.OpenKey(REG_USER_LOCATION, True)
    else { System Environment Variable }
    begin
      _Reg.RootKey := HKEY_LOCAL_MACHINE;
      Result  := _Reg.OpenKey(REG_MACHINE_LOCATION, True);
    end;
    if not Result then exit;
    _Reg.WriteString(VarName, Value); { Write Registry for Global Environment }
    result:=SetEnvironmentVariable(PChar(VarName), PChar(Value)); { Update Current Process Environment Variable }
    if not result then exit;
    SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, Integer(PChar('Environment'))); { Send Message To All Top Window for Refresh }
  finally
    _Reg.Free;
  end;
end;

function GetGlobalEnvironment(const VarName:string): String;
resourcestring
  REG_MACHINE_LOCATION = 'System\CurrentControlSet\Control\Session Manager\Environment';
  REG_USER_LOCATION = 'Environment';
var
    Reg : TRegistry;
    NotFound : Boolean;
begin

    NotFound := True;
    {Open registry key and return ValueData}
    Reg := TRegistry.Create;
    try
        Reg.rootkey :=  HKEY_LOCAL_MACHINE ;
        if Reg.OpenKey(REG_MACHINE_LOCATION ,False) then
        begin
            if Reg.ValueExists(VarName) then
            begin
                Result :=  Reg.ReadString(VarName);
                NotFound := False;
            end;
            Reg.CloseKey;
        end;
        if NotFound then
        begin
            ShowMessage('The Key'
                        +  REG_MACHINE_LOCATION + ' - '
                        + VarName
                        + ', Could not be found in the registry.');
            exit;
        end;


    finally
        Reg.CloseKey;
        Reg.Free;
    end;

end;

function GetEnvVarValue(const VarName: string): string;
var
  BufSize: Integer;  // buffer size required for value
begin
  Result := '';
  // Get required buffer size (inc. terminal #0)
  BufSize := GetEnvironmentVariable(PChar(VarName), nil, 0);
  if BufSize <= 0 then exit;
  // Read env var value into result string
  SetLength(Result, BufSize - 1);
  GetEnvironmentVariable(PChar(VarName),PChar(Result), BufSize);
end;

function SetEnvVarValue(const VarName: string;Value:string): boolean;
begin
  Result := SetEnvironmentVariable(PChar(VarName),pchar(value));
end;




end.
