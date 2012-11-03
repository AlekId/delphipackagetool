{***************************************************************
 *
 * Unit Name: uDPTMisc
 * Purpose  :  different help functions for the other units.
 * Author   : Donated by Samuel Herzog

 ****************************************************************}

unit uDPTMisc;

interface
uses typinfo,
     classes,
     Sysutils,
     windows,
     controls,
     registry;

const
  cValidChars=['0','1','2','3','4','5','6','7','8','9','-'];
  cValidFilenameChars=['a'..'z','A'..'Z','0'..'9','.','_','&',' ','#','%','(',')','%','-','+','$'];
  cNoVersion='No Version';
  cSetVersionApplicationName='SetVersion.exe';

{$H+}
type

  TSystemPath = (spDesktop, spStartMenu, spPrograms, spStartup, spPersonal, spAppData, spFonts, spSendTo,
    spRecent, spFavorites, spCache, spCookies, spHistory, spNetHood, spPrintHood, spTemplates, spLocADat,
    spWindRoot, spWindSys, spTempPath, spRootDir, spProgFiles, spComFiles, spConfigPath, spDevicePath,
    spMediaPath, spWallPaper, spCommonDocs);


  TNVBTraceProcedure=procedure(_level:byte;_msg:String;_params:Array of Const) of object;
  TNVBSendTrace     =procedure (_to:string='';_subject:string='';_attachementFilename:string='') of object;

procedure RegisterFileType(_ExtName:String;_AppName:String) ;
function GetWindowsPath:String;  // returns the windows directory name
function IsFilenameValid(_filename:string):boolean; // check if the given name <_filename> can be used as filename.
procedure Trace(const _level:byte;const _msg:String;const _params:array of const);
function BackupFile(_Filename:String;_BackupExt:String='.old';_BackupPath:String='';_DeleteOriginalFile:boolean=true):Boolean;
function ExtractFilenameOnly(_filename:String):String; // filename without path and without file extenstion.
procedure AllFilesOfPath(_Path, _Mask : string;_FileList:TStrings;_withExtension:boolean=true); // get all files with extension <_FileExt> of path <_path> into the Filelist.
function CheckPropExists(_component:TComponent;_propname:string):Boolean; // check if the component has a property with namd <_propname>.
function SetProperty(_persistent:TPersistent;var PropName:string;_Value:string):Boolean; // set a property of an component
function StringToFloat(_str:string;var value:extended):Boolean;
function StringToInteger(_str:string;var value:integer):boolean;
function GetVersion : String; // get the file version information.
function GetFileVersion(_filename: string): string;
function IsFileInUse(const fName: TFileName): Boolean;
function IsFileReadOnly(_filename:string):boolean;
function IsLastChar(_char:char;_s:string):boolean; // checks if the last char of the string is equal to <_ch>.
function GetField(_ch:char;var _s:string):string;
function BuildTimeStamp(_datetime:TDateTime):String;
function VerifySeparator(_s:String):String;
function GetFileDateTime(const _Filename:TFileName;var FileDateTime:TDateTime):Boolean; // get the date&time of the file
function LastPos(_s:string;_ch:char):integer; // find the last occurence of the char <_ch> inside string <_s>.
function GetSystemPath(SystemPath: TSystemPath): string;
function AllFilesOfDrive(_path,_mask:string;_FileList:TStrings;var AbortScan:Boolean):boolean; // search all file with mask <_mask> from Path <_path>
function GetFileSize(const _filename: string; var filesize:Int64): boolean; // read the size of a file.
function CreateDirectory(const _path:string):boolean;
function RemoveReadOnlyFlag(const _filename:string;const _silent:boolean):boolean;
function ShellExecute_AndWait(Operation, FileName, Parameter, Directory: string;Show: Word; bWait: Boolean; var ExitCode: LongWord): LongWord;
procedure ShowFolder(strFolder: string);
function Get7zAppName:string; // returns full filename&path to the 7z.exe file.
function HKEYToStr(const _Key: HKEY): string;
function FindLine(var content:TStrings;_Tag:string;var removedText:string):integer;

var
  FWriteMsg:TNVBTraceProcedure;
  FSendTrace:TNVBSendTrace;

implementation

uses Graphics,
     Forms,
     Dialogs,
     ShellAPI,
     shlObj;

{-----------------------------------------------------------------------------
  Procedure: FindLine
  Author:    herzogs2
  Date:      25-Mrz-2010
  Arguments: var content:TStrings;_Tag:string;var removedText:string
  Result:    integer
  Description: find the text <_Tag> inside the string-list <content>
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
  Procedure: HKEYToStr
  Author:    sam
  Date:      28-Okt-2011
  Arguments: const Key: HKEY
  Result:    string
  Description:
-----------------------------------------------------------------------------}
function HKEYToStr(const _Key: HKEY): string;
begin
  result:='unkown';
  case _Key of
    HKEY_CLASSES_ROOT:result:='HKEY_CLASSES_ROOT';
    HKEY_CURRENT_USER:result:='HKEY_CURRENT_USER';
    HKEY_LOCAL_MACHINE:result:='HKEY_LOCAL_MACHINE';
    HKEY_USERS:result:='HKEY_USERS';
    HKEY_PERFORMANCE_DATA:result:='HKEY_PERFORMANCE_DATA';
    HKEY_CURRENT_CONFIG:result:='HKEY_CURRENT_CONFIG';
    HKEY_DYN_DATA:result:='HKEY_DYN_DATA';
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: Get7zAppName
  Author:    sam
  Date:      04-Okt-2011
  Arguments: None
  Result:    string
  Description: returns full filename&path to the 7z.exe file.
-----------------------------------------------------------------------------}
function Get7zAppName:string;
var
_Reg: TRegistry;
begin
  result:='';
  _Reg := TRegistry.Create;
  try
    _Reg.RootKey := HKEY_CLASSES_ROOT;
    if _Reg.OpenKeyReadOnly('\Applications\7z.exe\shell\open\command') then begin
      try
        Result := _Reg.ReadString('');
        if pos('"',result)=1 then delete(result,1,1);
        result:=GetField('"',result);
      finally
        _Reg.CloseKey;
      end;
    end;
    if result='' then begin
      _Reg.RootKey :=HKEY_LOCAL_MACHINE;
      if _Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\7zFM.exe') then begin
        try
          Result := _Reg.ReadString('Path');
          if pos('"',result)=1 then delete(result,1,1);
          result:=IncludeTrailingPathDelimiter(GetField('"',result))+'7z.exe';
        finally
          _Reg.CloseKey;
        end;
      end;
    end;
    if result<>'' then result:='"'+result+'"';
  finally
    _Reg.free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: RemoveReadOnlyFlag
  Author:    sam
  Date:      08-Mrz-2007
  Arguments: const _filename:string;const _silent:string
  Result:    boolean
  Description: make sure the file is not read-only.
  Return value is True if the file is not read-only or does not exist.
  Return value is False if the read-only could not be removed.
-----------------------------------------------------------------------------}
function RemoveReadOnlyFlag(const _filename:string;const _silent:boolean):boolean;
begin
  result:=true;
  if not fileexists(_filename) then exit;
  if not isFileReadOnly(_filename) then exit;
  if not _silent then begin
    trace(5,'The file <%s> is Read-Only. Do you want to overwrite it ?',[_filename]);
    if MessageDlg(format('The file <%s> is Read-Only. Do you want to remove the Read-Only Attribute ?',[_filename]), mtWarning, [mbYes, mbNo], 0)=mrNo then begin
      result:=false;
      exit;
    end;
  end;
  try
    FileSetAttr(_filename, FileGetAttr(_filename) xor faReadOnly);
    trace(5,'Removed Read-Only Flag from file <%s>.',[_filename]);
  except
    on e:exception do begin
      result:=false;
      trace(1,'Could not change the Read-Only Flag of file <%s>. You probably have not enough user rights.',[_filename]);
    end;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: ShowFolder
  Author:    sam
  Date:      01-Mrz-2008
  Arguments: strFolder: string
  Result:    None
  Description: open file-explorer and disply the folder <strfolder>.
-----------------------------------------------------------------------------}
procedure ShowFolder(strFolder: string);
begin
  ShellExecute(Application.Handle,
    PChar('explore'),
    PChar(strFolder),
    nil,
    nil,
    SW_SHOWNORMAL);
end;

{-----------------------------------------------------------------------------
  Procedure: CreateDirectory
  Author:    sam
  Date:      08-Feb-2007
  Arguments: const _path:string
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function CreateDirectory(const _path:string):boolean;
begin
  result:=false;
  if _path='' then begin
    trace(1,'The passed path value is empty. Call support crew.',[]);
    exit;
  end;
  if directoryExists(_path) then begin
    result:=true;
    exit;
  end;
  try
    result:=ForceDirectories(_path);
    if result then trace(1,'Created directory <%s>.',[_path]);
  except
    on e:exception do Showmessage(format('Could not create Directory <%s>. Please try to create it manually or change settings. <%s>.',[_path,e.message]));
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: GetFileSize
  Author:    sam
  Date:      06-Mai-2005
  Arguments: const _filename: string; var filesize:Int64
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function GetFileSize(const _filename: string; var filesize:Int64): boolean;
begin
  result:=false;
  filesize:=0;
  if not fileExists(_filename) then begin
    trace(3,'Could not find File <%s>. Can not read FileSize.',[_filename]);
    exit;
  end;
  with TFileStream.Create(_filename, fmOpenRead or fmShareDenyNone) do
  try
    filesize := Size;
    result:=true;
  finally
    Free;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: RegisterFileType
  Author:    sam
  Date:      17-Jun-2011
  Arguments: _ExtName:String;_AppName:String
  Result:    None
  Description: try to register the file extenstion <_ExtName> with application <_AppName>.
-----------------------------------------------------------------------------}
procedure RegisterFileType(_ExtName:String;_AppName:String);
resourcestring
cRegisterFileTypeProblem='Could not register File-Extension <%s> for application <%s>. <%s>. You might need Admin-Rights for this Operation.';
var
  reg:TRegistry;
begin
  try
    reg := TRegistry.Create;
    try
      reg.RootKey:=HKEY_CLASSES_ROOT;
      if reg.OpenKey('.' + _ExtName, True) then reg.WriteString('', _ExtName + 'file') ;
    finally
      reg.CloseKey;
      reg.Free;
    end;
    reg := TRegistry.Create;
    try
      if reg.CreateKey(_ExtName + 'file') then begin
        if reg.OpenKey(_ExtName + 'file\DefaultIcon', True) then reg.WriteString('', _AppName + ',0') ;
      end;
    finally
      reg.CloseKey;
      reg.free;
    end;
    reg := TRegistry.Create;
    try
      if reg.OpenKey(_ExtName + 'file\shell\open\command', True) then reg.WriteString('',_AppName+' "%1"');
    finally
      reg.CloseKey;
      reg.free;
    end;
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil) ;
  except
    on e:exception do ShowmessageFmt(cRegisterFileTypeProblem,[_ExtName,_AppName,e.message]);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: IsFilenameValid
  Author:    HerzogS2
  Date:      19-Dez-2003
  Arguments: _filename:string
  Result:    boolean
  Description: pass only the filename to this method ( without the path !)
  This method returns true if the string <_filename> can be used as a valid filename.
   24.05.2004 SH -bugfix. The chars '-','+' are also valid in a filename.
-----------------------------------------------------------------------------}
function IsFilenameValid(_filename:string):boolean; // check if the given name <_filename> can be used as filename.
var
i:integer;
begin
  result:=false;
  if trim(_filename)='' then exit;
  for i:=1 to length(_filename) do begin
{$IF CompilerVersion < 20.0}
    if not (_filename[i] in cValidFilenameChars) then exit;
{$ELSE}
    if not CharInSet(_filename[i], cValidFilenameChars) then exit;
{$IFEND}
  end;
  result:=true;
end;

{-----------------------------------------------------------------------------
  Procedure: GetSystemPath
  Author:    HerzogS2
  Date:      10-Dez-2003
  Arguments: SystemPath: TSystemPath
  Result:    string
  Description: read system path settings
-----------------------------------------------------------------------------}
function GetSystemPath(SystemPath: TSystemPath): string;
const
  CSIDL_DESKTOP              = $0000;
  CSIDL_STARTMENU            = $000B;
  CSIDL_PROGRAMS             = $0002;
  CSIDL_STARTUP              = $0007;
  CSIDL_PERSONAL             = $0005;
  CSIDL_APPDATA              = $001A;
  CSIDL_FONTS                = $0014;
  CSIDL_SENDTO               = $0009;
  CSIDL_RECENT               = $0008;
  CSIDL_FAVORITES            = $0006;
  CSIDL_COOKIES              = $0021;
  CSIDL_HISTORY              = $0022;
  CSIDL_NETHOOD              = $0013;
  CSIDL_PRINTHOOD            = $001B;
  CSIDL_TEMPLATES            = $0015;
  CSIDL_LOCAL_APPDATA        = $001C;
  CSIDL_WINDOWS              = $0024;
  CSIDL_SYSTEM               = $0025;
  CSIDL_PROGRAM_FILES        = $0026;
  CSIDL_PROGRAM_FILES_COMMON = $002B;
  CSIDL_COMMON_DOCUMENTS     = $002E; // All Users\Documents
var
  Path: array[0..MAX_PATH] of Char;
  ph: PChar;
  CSIDL: Integer;
  Reg: TRegistry;
begin
  CSIDL := -1;
  Reg := TRegistry.Create;
  try
    case SystemPath of
      spDesktop:   CSIDL := CSIDL_DESKTOP;
      spStartMenu: CSIDL := CSIDL_STARTMENU;
      spPrograms:  CSIDL := CSIDL_PROGRAMS;
      spStartup:   CSIDL := CSIDL_STARTUP;
      spPersonal:  CSIDL := CSIDL_PERSONAL;
      spAppData:   CSIDL := CSIDL_APPDATA;
      spFonts:     CSIDL := CSIDL_FONTS;
      spSendTo:    CSIDL := CSIDL_SENDTO;
      spRecent:    CSIDL := CSIDL_RECENT;
      spFavorites: CSIDL := CSIDL_FAVORITES;
      spCache:
        begin
          Reg.RootKey := HKEY_CURRENT_USER;
          Reg.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders');
          try
            Result := Reg.ReadString('Cache');
          finally
            Reg.CloseKey;
          end;
        end;
      spCookies:   CSIDL := CSIDL_COOKIES;
      spHistory:   CSIDL := CSIDL_HISTORY;
      spNetHood:   CSIDL := CSIDL_NETHOOD;
      spPrintHood: CSIDL := CSIDL_PRINTHOOD;
      spTemplates: CSIDL := CSIDL_TEMPLATES;
      spLocADat:   CSIDL := CSIDL_LOCAL_APPDATA;
      spWindRoot:  CSIDL := CSIDL_WINDOWS;
      spWindSys:   CSIDL := CSIDL_SYSTEM;
      spTempPath:
        begin
          GetMem(ph, 255);
          GetTempPath(254, ph);
          Result := Strpas(ph);
          Freemem(ph);
        end;
      spRootDir:
        begin
          GetMem(ph, 255);
          GetSystemDirectory(ph, 254);
          Result := (Copy(Strpas(ph), 1, 2));
          Freemem(ph);
        end;
      spProgFiles: CSIDL := CSIDL_PROGRAM_FILES;
      spComFiles:  CSIDL := CSIDL_PROGRAM_FILES_COMMON;
      spConfigPath:
        begin
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows\CurrentVersion');
          try
            Result := Reg.ReadString('ConfigPath');
          finally
            Reg.CloseKey;
          end;
        end;
      spDevicePath:
        begin
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows\CurrentVersion');
          try
            Result := Reg.ReadString('DevicePath');
          finally
            Reg.CloseKey;
          end;
        end;
      spMediaPath:
        begin
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows\CurrentVersion');
          try
            Result := Reg.ReadString('MediaPath');
          finally
            Reg.CloseKey;
          end;
        end;
      spWallPaper:
        begin
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows\CurrentVersion');
          try
            Result := Reg.ReadString('WallPaperDir');
          finally
            Reg.CloseKey;
          end;
        end;
      spCommonDocs: CSIDL := CSIDL_COMMON_DOCUMENTS;
    end;
    if CSIDL > -1 then begin
      SHGetSpecialFolderPath(GetActiveWindow, Path, CSIDL, False);
      Result := Path;
    end;
  finally
    Reg.Free;
  end;
  if (Result <> '') then
    Result := IncludeTrailingPathDelimiter(Result);
end;

//*****************************************************
// Method:  IsFileInUse
// Programmer: S.Herzog
// Description: check if file is already in use.
// The parameter <_filename> needs the path+filename.
// Last changes: 03.09.01
//*****************************************************
function IsFileInUse(const fName: TFileName): Boolean;
var
  HFileRes: HFILE;
begin
  result:=false;
  if not fileexists(fName) then exit;
  HFileRes := CreateFile(PChar(fName),
                         GENERIC_READ or GENERIC_WRITE,
                         0,
                         nil,
                         OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL,
                         0);
  Result := (HFileRes = INVALID_HANDLE_VALUE);
  if not Result then CloseHandle(HFileRes);
end;


{-----------------------------------------------------------------------------
  Procedure: GetFileVersion
  Author:
  Date:      04-Jun-2003
  Arguments: _filename:string
  Result:    String
  Purpose:   read version info from a file.
  History:
-----------------------------------------------------------------------------}
function GetFileVersion(_filename: string): string;
var
  VerInfoSize: DWord;
  VerInfo: Pointer;
  VerValueSize: DWord;           
  VerValue: PVSFixedFileInfo;
  Dummy: DWord;
begin
  Result := cNoVersion;
  if not FileExists(_filename) then begin
    trace(1,'Problem in GetFileVersion: Could not find file <%s>.',[_filename]);
    exit;
  end;
  VerInfoSize := GetFileVersionInfoSize(PChar(_filename), Dummy);
  if VerInfoSize = 0 then begin
    trace(1,'Warning in GetFileVersion: The file <%s> contains not Version information.',[_filename]);
    exit;
  end;
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(_filename), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do begin
    result := IntTostr(dwFileVersionMS shr 16);
    result := result + '.' + IntTostr(dwFileVersionMS and $FFFF);
    result := result + '.' + IntTostr(dwFileVersionLS shr 16);
    result := result + '.' + IntTostr(dwFileVersionLS and $FFFF);
  end;
  FreeMem(VerInfo, VerInfoSize);
end;

{-----------------------------------------------------------------------------
  Procedure: LastPos
  Author:    S.Herzog
  Date:      26-Jan-2003
  Arguments: _s:string;_ch:char
  Result:    integer
  Description: find the last occurence of the char <_ch> inside string <_s>.
-----------------------------------------------------------------------------}
function LastPos(_s:string;_ch:char):integer;
var
i:integer;
begin
  Result:=0;
  for i:=length(_s) downto 1 do begin
    if _s[i]=_ch then begin
      Result:=i;
      exit;
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: GetFileDateTime
  Author:    Not available
  Date:      30-Okt-2002
  Arguments: const _Filename:TFileName
  Result:    Boolean  is true if the method worked ok.
  Description: get the date&time of a file.
-----------------------------------------------------------------------------}
function GetFileDateTime(const _Filename:TFileName;var FileDateTime:TDateTime):Boolean;
{$IF CompilerVersion < 20.0}
var
  _age: Integer;
{$IFEND}
begin
  Result := false;
{$IF CompilerVersion < 20.0}
  _age := FileAge(_Filename);
  if _age = -1 then exit;  // File doesn't exist
  FileDateTime := FileDateToDateTime(_age);
{$ELSE}
  if not FileAge(_Filename, FileDateTime)then exit;  // File doesn't exist
{$IFEND}
end;

{-----------------------------------------------------------------------------
  Procedure: VerifySeparator
  Author:    Not available
  Date:      04-Okt-2002
  Arguments: _s:String
  Result:    String
  Description: make sure the correct decimal seperator is used.
-----------------------------------------------------------------------------}
function VerifySeparator(_s:String):String;
var
_delim_pos:integer;
begin
  _s:=trim(_s);
  _Delim_pos:=Pos('.',_s);
  if _delim_pos=0 then _Delim_pos:=Pos(',',_s);
{$if CompilerVersion < 20.0}
  if _delim_pos>0 then _s[_Delim_pos]:=DecimalSeparator;
{$else}
  if _delim_pos>0 then _s[_Delim_pos]:=FormatSettings.DecimalSeparator;
{$ifend}
  result:=_s;
end;

{-----------------------------------------------------------------------------
  Procedure: Trace
  Author:    Not available
  Date:      28-Aug-2002
  Arguments: _msg:String;_params:array of const
  Result:    None
  Description: Write the to the debug log.
-----------------------------------------------------------------------------}
procedure Trace(const _level:byte;const _msg:String;const _params:array of const);
var
_s:string;
begin
  if assigned(FWriteMsg) then begin
     FWriteMsg(_level,_msg,_params);
     exit;
  end;
  if SizeOf(_params)>0 then _s:=format(_msg,_params)
                       else _s:=_msg;
  OutputDebugString(PChar(_s));
end;

{-----------------------------------------------------------------------------
  Procedure: BackupFile
  Author:    herzogs2
  Date:      23-Aug-2002
  Arguments: _Filename:String;_BackupExt:String='.old';_BackupPath:String=''
  Result:    Boolean  True if successful.
  Description: Renames or Copies the file <_Filename> to a file with the new
  file extension <_BackupExt>.
  Example : BackupFile('C:\work\Readme.txt');
            BackupFile('C:\work\Readme.txt','.bak','C:\Temp');
-----------------------------------------------------------------------------}
function BackupFile(_Filename:String;_BackupExt:String='.old';_BackupPath:String='';_DeleteOriginalFile:boolean=true):Boolean;
var
_TargetFilename:String;
begin
  Result:=False;
  _TargetFilename:=ChangeFileExt(_Filename,_BackupExt);
  if (_BackupPath<>'') and
     (not DirectoryExists(_BackupPath)) then ForceDirectories(_BackupPath); // create the backup directory

  if FileExists(_TargetFilename) then begin  // check if already a backup file exists
    if (not IsFileInUse(_TargetFilename)) and
       (not IsFileReadOnly(_TargetFilename)) then DeleteFile(PChar(_TargetFilename));
  end;

  if _BackupPath='' then begin
    if _DeleteOriginalFile then begin
      if RenameFile(_Filename,_TargetFilename) then begin
        Trace(2,'BackupFile: Renamed file <%s> to <%s>.',[_Filename,_TargetFilename]);
        Result:=true;
      end;
    end
    else begin
      if CopyFile(PChar(_Filename),PChar(_TargetFilename),false) then begin
        Trace(2,'BackupFile: Copied file <%s> to <%s>.',[_Filename,_TargetFilename]);
        Result:=true;
      end;  
    end;
  end else begin
    if CopyFile(PChar(_Filename),PChar(_TargetFilename),false) then begin
      if _DeleteOriginalFile then DeleteFile(PChar(_Filename));
      Trace(2,'BackupFile: Copied file <%s> to <%s>.',[_Filename,_TargetFilename]);
      Result:=true;
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: ExtractFilenameOnly
  Author:    herzogs2
  Date:      02-Mai-2002
  Arguments: _filename:String
  Result:    String
  Description: returns the filename only. Without any extension.
-----------------------------------------------------------------------------}
function ExtractFilenameOnly(_filename:String):String;
var
_pos:integer;
begin
  _Filename:=ExtractFilename(_Filename);
  _pos:=LastPos(_Filename,'.');
  if _pos>0 then _Filename:=Copy(_Filename,1,_pos-1);
  Result:=_Filename;
end;

{-----------------------------------------------------------------------------
  Procedure: GetWindowPath
  Author:    herzogs2
  Date:      09-Okt-2002
  Arguments: None
  Result:    String  e.g. C:\WINNT\ or C:\Windows\
-----------------------------------------------------------------------------}
function GetWindowsPath:String;
var
  aWinDir: array[0..MAX_PATH] of char;
begin
  try
    GetWindowsDirectory(@aWinDir,SizeOf(aWinDir));
    Result:=ExtractFilePath(string(aWinDir)+'\');
  except
    Result:='';
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: StringToFloat
  Author:    herzogs2
  Date:      23-Apr-2002
  Arguments: _str:string;var _value:Float
  Result:    Boolean
  Description: 04.10.2002 -added VerifySeperator.
-----------------------------------------------------------------------------}
function StringToFloat(_str:string;var value:Extended):Boolean;
begin
  Result:=false;
  if _str='' then exit;
  try
    _str:=VerifySeparator(_str);
    Value:=strtofloat(_str);
    Result:=true;
  except
    Value:=0;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: StringToInteger
  Author:    sam
  Date:      28-Okt-2005
  Arguments: const _str:string;var value:integer
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function StringToInteger(_str:string;var value:integer):boolean;
begin
  Result:=false;
  _str:=trim(_str);
  if _str='' then exit;
  try
    value:=strtoint(_str);
    Result:=true;
  except
    Value:=0;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: IsLastChar
  Author:    herzogs2
  Date:      06-Mrz-2002
  Arguments: _char:char;_s:string
  Result:    boolean
  Description: checks if the last char of the string <_s> is equal to <_ch>.
  Example if not isLastChar('\',FPath) then FPath:=FPath+'\';
-----------------------------------------------------------------------------}
function IsLastChar(_char:char;_s:string):boolean;
var
_len:Integer;
begin
  Result:=false;
  _len:=length(_s);
  if _len=0 then exit;
  if _s[_len]=_char then result:=true;
end;

{-----------------------------------------------------------------------------
  Procedure: IsFileReadOnly
  Author:    herzogs2
  Date:      01-Mrz-2002
  Arguments: _filename:String
  Result:    boolean
  Description: Checks if the file <_filename> is readonly or not.
-----------------------------------------------------------------------------}
function IsFileReadOnly(_filename:String):boolean;
begin
  result:=FileIsReadOnly(_filename);
end;

//*************************************************************************
//Procedure: AllFilesOfPath
//Description: get a list of all files in path <_path>. E.g. <*.pas>.
//Author: SH
//History: 22.12.2001 09:47:33
//Example: AllFilesOfPath(PackagePath,'*.pas',FFileList);
// 31.01.2004 - added optional parameter <_withExtension>. If false then the extension is cut off before adding to the list.
//*************************************************************************
procedure AllFilesOfPath(_Path, _Mask : string;_FileList:TStrings;_withExtension:boolean=true);
var
  SearchRec : TSearchRec;
  _Filename:string;
begin
  if not assigned(_FileList) then exit;
  try
    if SysUtils.FindFirst(_Path+_Mask, faAnyFile, SearchRec) <> 0 then exit;
    _Filename:=trim(SearchRec.Name);
    if not _withExtension then _Filename:=ExtractFilenameOnly(_Filename);
    if _Filename<>'' then _FileList.add(_Filename);
    while SysUtils.FindNext(SearchRec) = 0 do begin
      _Filename:=trim(SearchRec.Name);
      if not _withExtension then _Filename:=ExtractFilenameOnly(_Filename);
      if _Filename<>'' then _FileList.add(_Filename);
    end;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: AllFilesOfDrive
  Author:    Not available
  Date:      24-Dez-2003
  Arguments: _path,_mask:string;_FileList:TStrings
  Result:    true if scan was succesful
  Description: get all files with mask <_mask> from path <_path>.
-----------------------------------------------------------------------------}
function AllFilesOfDrive(_path,_mask:string;_FileList:TStrings;var AbortScan:Boolean):boolean;
var
_search: TSearchRec;
begin
  result:=false;
  if not assigned(_FileList) then exit;
  _mask:=lowercase(_mask);
  // find all files
  try
    if SysUtils.FindFirst(_path+_mask, faAnyFile, _search) = 0 then
    begin
      repeat
        // add the files to the listbox
        if trim(_search.Name)<>'' then _FileList.Add(_path + _search.Name);
      until (SysUtils.FindNext(_search) <> 0) or (AbortScan);
    end;

    Application.ProcessMessages;
    Result := not (AbortScan or Application.Terminated);
    if not Result then begin
      Exit;
    end;

    // Subdirectories/ Unterverzeichnisse
    if SysUtils.FindFirst(_path + '*.*', faDirectory, _search) = 0 then
    begin
      repeat
        if ((_search.Attr and faDirectory) = faDirectory) and
           (trim(_search.Name)<>'') and
           (_search.Name[1] <> '.') then AllFilesOfDrive(_path + _search.Name + '\',_mask,_FileList,AbortScan);
      until (SysUtils.FindNext(_search) <> 0) or (AbortScan);
    end;
    result:=true;
  finally
    SysUtils.FindClose(_search);
  end;
end;



//*************************************************************************
//Procedure: BuildTimeStamp
//Description: build a timestamp from the <datetime>. This means all seperators are
// replaced with <_>.
//Author: SH
//History: 21.11.2001 22:40:42
//*************************************************************************
function  BuildTimeStamp(_datetime:TDateTime):String;
var
i:integer;
_s:string;
_FormatSettings:TFormatSettings;
begin
  _FormatSettings.ShortDateFormat:='yyyy.mm.dd';
  _FormatSettings.LongTimeFormat:='HH:MM:SS';
  _s:=datetimetostr(_datetime,_FormatSettings);
  for i:=1 to length(_s) do begin
{$IF CompilerVersion < 20.0}
    if not (_s[i] in cValidChars) then _s[i]:='_';
{$ELSE}
    if not CharInSet(_s[i], cValidChars) then _s[i]:='_';
{$IFEND}
  end;
  result:=_s;
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

//*****************************************************
// Method:  GetVersion
// Programmer: from www.swissdelphicenter.ch
// Description: get file version information.
// Last changes: 19.09.01
// 09.08.2002 - fixed bug incase of no version info.
//*****************************************************
function GetVersion : String;
var
  VerInfoSize: DWord;
  VerInfo: Pointer;
  VerValueSize: DWord;
  VerValue: PVSFixedFileInfo;
  Dummy: DWord;
begin
  Result:=cNoVersion;
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  if VerInfoSize=0 then exit;
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do
  begin
    result := IntTostr(dwFileVersionMS shr 16);
    result := result+'.'+   IntTostr(dwFileVersionMS and $FFFF);
    result := result+'.'+   IntTostr(dwFileVersionLS shr 16);
    result := result+'.'+   IntTostr(dwFileVersionLS and $FFFF);
  end;
  FreeMem(VerInfo, VerInfoSize);
end;

//*****************************************************
// Method:  IsNumeric
// Programmer: S.Herzog
// Description:
// Last changes: 10.07.01
//*****************************************************
function IsNumeric(_s:string):Boolean;
var
i:integer;
_done:boolean;
begin
  result:=false;
  _done:=false;
  for i:=1 to length(_s) do begin
{$IF CompilerVersion < 20.0}
    if not (_s[i] in cValidChars) then exit;
{$ELSE}
    if not CharInSet(_s[i], cValidChars) then exit;
{$IFEND}
    _done:=true;
  end;
  if _done then result:=true;
end;


//============================================================
// PROCEDURE SetProperty
// discription :  set the value <_value> on property <_PropName>.
// programmed by : SH
// tested by :
// last changes on : 04.10.00
// 14.08.2001: It is now possible to set also properties of sub class.
//             For example "Font.Color".
// 06.08.2002: added tkWString to the case statement.
//============================================================
function SetProperty(_persistent:TPersistent;var PropName:string;_Value:string):Boolean;
var
_PropInfo:PPropInfo;
_pos:integer;
_ivalue:longint;
_PropName:string;
_ovalue:TObject;
begin
  result:=False;
  _ivalue:=0;
  if not assigned(_persistent) then exit;
  if PropName='' then exit;
  _pos:=Pos('.',PropName);
  if _pos>0 then _PropName:=Copy(PropName,1,_pos-1)
            else _PropName:=PropName;
  Delete(PropName,1,_pos);

  _PropInfo:=GetPropInfo(_persistent.ClassInfo,_propname);
  if not Assigned(_PropInfo) then exit;
  if not assigned(_PropInfo^.setProc) then exit;
  case _PropInfo.PropType^.Kind of
    tkLString,tkWString:begin
                try
                  SetStrProp(_persistent,_PropInfo,_value);
                  result:=True;
                except
                  _value:='';
                end;
              end;
    tkInteger,
    tkEnumeration:begin
                try
                  if _value<>'' then
                  begin
                    _value:=lowercase(_value);
                    if _value='false' then _ivalue:=0 else
                    if _value='true'  then _ivalue:=1 else
                    if IsNumeric(_value) then _ivalue:=strtoint(_value)
                                         else IdentToColor(_value,_ivalue);
                    SetOrdProp(_persistent,_PropInfo,_ivalue);
                    result:=True;
                  end;
                except
                  _iValue:=0;
                end;
              end;
     tkMethod:begin
              end;

     tkClass: begin
                _ovalue:=GetObjectProp(_persistent,_PropName);
                 if Assigned(_ovalue) then begin
                   if (_ovalue is TStrings) and
                      (_propname='SQL') then TStrings(_ovalue).Text:=_value else
                   if _ovalue is TPersistent then Result:=SetProperty(TPersistent(_ovalue),PropName,_value);
                 end;
              end;
  end;
end;


//============================================================
// PROCEDURE CheckPropExists
// discription :  check if a component has a property with the name <_PropName>.
// programmed by : SH
// tested by :
// last changes on : 04.10.00
//============================================================
function CheckPropExists(_component:TComponent;_propname:string):Boolean; // check if the component has a property with namd <_propname>.
var
_PropInfo:PPropInfo;
begin
  _PropInfo:=GetPropInfo(_component.ClassInfo,_propname);
  result:=Assigned(_PropInfo);
end;

{-----------------------------------------------------------------------------
  Procedure: ShellExecute_AndWait
  Author:
  Date:      13-Jan-2004
  Arguments: Operation, FileName, Parameter, Directory: string;Show: Word; bWait: Boolean
  Result:    Longint
  Description:
-----------------------------------------------------------------------------}
function ShellExecute_AndWait(Operation, FileName, Parameter, Directory: string;Show: Word; bWait: Boolean; var ExitCode: LongWord): LongWord;
var
  Info: TShellExecuteInfo;
{
  ****** Parameters ****** 
  Operation:

  edit  Launches an editor and opens the document for editing. 
  explore Explores the folder specified by lpFile. 
  find Initiates a search starting from the specified directory. 
  open Opens the file, folder specified by the lpFile parameter. 
  print Prints the document file specified by lpFile. 
  properties Displays the file or folder's properties. 

  FileName: 

  Specifies the name of the file or object on which 
  ShellExecuteEx will perform the action specified by the lpVerb parameter. 

  Parameter: 

  String that contains the application parameters. 
  The parameters must be separated by spaces. 

  Directory: 

  specifies the name of the working directory. 
  If this member is not specified, the current directory is used as the working directory. 

  Show: 

  Flags that specify how an application is to be shown when it is opened. 
  It can be one of the SW_ values

  bWait: 

  If true, the function waits for the process to terminate 
} 
begin 
  FillChar(Info, SizeOf(Info), Chr(0));
  Info.cbSize := SizeOf(Info);
  Info.fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_NO_UI	;
  Info.lpVerb := PChar(Operation);
  Info.lpFile := PChar(FileName);
  Info.lpParameters := PChar(Parameter);
  Info.lpDirectory  := PChar(Directory);
  Info.nShow := Show;
  Result := NO_ERROR;
  if not ShellExecuteEx(@Info) then Result := GetLastError;
  if Result <> NO_ERROR then exit;
  if not bWait then exit;
  while
    WaitForSingleObject(Info.hProcess, 100) = WAIT_TIMEOUT
    do Application.ProcessMessages;
  if not GetExitCodeProcess(Info.hProcess, DWORD(ExitCode)) then begin
    Result := GetLastError;
  end;
end;

end.


