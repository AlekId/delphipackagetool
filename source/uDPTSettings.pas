{***************************************************************
 *
 * Unit Name: uDPTSettings
 * Purpose  : A component to keep settings of an application in memory, load/save them and reinitialize the application
 * without restart.
 *
 ****************************************************************}

unit uDPTSettings;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Dialogs,
  uDPTSettingsMisc;


const
  cComponentVersion = '1.13';

type

  TNVBSettings = class(TComponent)
  private
    FFileName: string;
    FFilePath: string;
    FSettingsChanged: Boolean;
    FSettingList: TStringList;
    FIsLoaded: Boolean; // is true if LoadConfig was successfull
    FIsNew:boolean;
    FOnError: TNVBSettingErrorEvent;
    FAutoSave: Boolean;
    FCryptIt: Boolean;
    FKeyString: string;
    FVersion: string;
    function GetSetting(var SettingData: TNVBSettingData): boolean;overload;
    function GetCount: Integer;
    procedure SetFileName(const Value: string); // load configuration
    procedure VerifyFilePath;
    procedure DoError(_ErrorMsg: string; _ID: Longint);
    procedure SetFilePath(const Value: string);
    procedure SetVersion(const Value: string);
    // find the setting name for a specific value in a group of settings
    function FindSettingIdx(const _Name:string):integer;overload;   // try to find the setting with name <name>. Returns the Index of the setting.
    function FindSettingIdx(const _ID:integer):integer;overload;    // try to find the setting with ID <ID>. Returns the Index of the setting.
    function GetSetting(const _Name: string): TNVBSettingData;overload; // try to find the setting with Name <Name>. Returns setting object.
    function GetSetting(const _ID: integer): TNVBSettingData;overload; // try to find the setting with ID <ID>. Returns setting object.
    function GetSetting(const _Name:string;_ID: integer): TNVBSettingData;overload; // try to find the setting with Name or ID <ID>. Returns setting object.
  protected
    procedure Initialize;
    procedure CleanUp;
  public
    // with Delphi 4>, this could be done with method overload
    // this methods get the values and creates the list entry if not already existing.
    function GetStringValue(const _Name: string;const ID: integer;const DefaultValue: string;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): string;
    function GetIntegerValue(const _Name: string;const ID: integer;const DefaultValue: integer;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): Integer;
    function GetBoolValue(const _Name: string;const ID: integer;const DefaultValue: boolean;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): boolean;
    function GetFileValue(const _Name: string;const ID: integer;const DefaultValue: string;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): string;
    function GetPathValue(const _Name: string;const ID: integer;const DefaultValue: string;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): string;
    // this methods just gets the value without creating an list-item
    function StringValue(const _Name: string; _ID: integer=-1): string;
    function IntegerValue(const _Name: string; _ID: integer=-1): Integer;
    function BoolValue(const _Name: string; _ID: integer=-1): boolean;
    function FileValue(const _Name: string; _ID: integer=-1): string;
    function PathValue(const _Name: string; _ID: integer=-1): string;
    // this methods just set the values without create
    function SetString(const _Name: string; _ID: integer; _value: string): boolean;
    function SetFile(const _Name: string; _ID: integer; _value: string): boolean;
    function SetInteger(const _Name: string; _ID: integer; _value: Integer): boolean;
    function SetBoolean(const _Name: string; _ID: integer; _value: Boolean): boolean;
    function SetPath(const _Name: string; _ID: integer; _value: string): boolean;

    function FindStringIndex(Group: string; Value: string): integer; // returns the item index.
    function SaveConfig: Boolean; // save all the settings to the file
    function LoadConfig: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Open: boolean;
    procedure Close;
    property IsLoaded: Boolean read FIsLoaded;
  published
    property AutoSave: Boolean read FAutoSave write FAutoSave; // if set to true and method close is called, then the settings are saved automatically.
    property Count: Integer read GetCount;
    property FileName: string read FFileName write SetFileName;
    property FilePath: string read FFilepath write SetFilePath;
    property OnError: TNVBSettingErrorEvent read FOnError write FOnError;
    property CryptIt: Boolean read FCryptIt write FCryptIt;
    property KeyString: string read FKeyString write FKeyString; // used for en/descryption
    property Version: string read FVersion write SetVersion;
  end;


implementation

uses
{$if CompilerVersion >= 24.0 }
  UITypes,
{$ifend}
{$ifdef NoCryptSupport}
  uDPTMisc;
{$else}
  uDPTMisc,
  DCPrc4,
  DCPsha1;
{$endif}  

//*************************************************************************
//Procedure: TNVBSettings.SetInteger
//Description: set the integer value of a list entry.
//Author: SH
//History: 05.01.2001
//*************************************************************************
function TNVBSettings.SetInteger(const _Name: string; _ID, _value: Integer): boolean;
var
  _SettingData: TNVBSettingData;
begin
  result:=false;
  _SettingData:=GetSetting(_Name,_ID);
  if not assigned(_SettingData) then exit;
  _SettingData.StrValue := IntToStr(_value);
  _SettingData.IntValue := _value;
  FSettingsChanged:=true;
  Result := true;
end;

//*************************************************************************
//Procedure: TNVBSettings.SetString
//Description: set the string value of a list entry.
//Author: SH
//History: 05.01.2001
//*************************************************************************
function TNVBSettings.SetString(const _Name: string; _ID: integer; _value: string): boolean;
var
  _SettingData: TNVBSettingData;
begin
  result:=false;
  _SettingData:=GetSetting(_Name,_ID);
  if not assigned(_SettingData) then exit;
  _SettingData.StrValue := _value;
  FSettingsChanged:=true;
  Result := true;
end;

//*****************************************************
// Method:  TNVBSettings.SetFile
// Programmer: S.Herzog
// Description:
// Last changes: 10.12.01
//*****************************************************
function TNVBSettings.SetFile(const _Name: string; _ID: integer; _value: string): boolean;
var
  _SettingData: TNVBSettingData;
begin
  result:=false;
  _SettingData:=GetSetting(_Name,_ID);
  if not assigned(_SettingData) then exit;
  _SettingData.StrValue := _value;
  FSettingsChanged:=true;
  Result := true;
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.SetPath
  Author:    sam
  Date:
  Arguments: Name: string; ID: integer;_value: string
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function TNVBSettings.SetPath(const _Name: string; _ID: integer; _value: string): boolean;
var
  _SettingData: TNVBSettingData;
begin
   result:=false;
  _SettingData:=GetSetting(_Name,_ID);
  if not assigned(_SettingData) then exit;
  _SettingData.StrValue := _value;
  FSettingsChanged:=true;
  Result := true;
end;

//*****************************************************
// Method:  TNVBSettings.SetBoolean
// Programmer: S.Herzog
// Description:
// Last changes: 21.11.01
//*****************************************************
function TNVBSettings.SetBoolean(const _Name: string; _ID: integer; _value: Boolean): boolean;
var
  _SettingData: TNVBSettingData;
begin
  result:=false;
  _SettingData:=GetSetting(_Name,_ID);
  if not assigned(_SettingData) then exit;
  if _value then
  begin
    _SettingData.StrValue := 'YES';
    _SettingData.IntValue := 1;
  end
  else
  begin
    _SettingData.StrValue := 'NO';
    _SettingData.IntValue := 0;
  end;
  _SettingData.BoolValue := _value;
  FSettingsChanged:=true;
  Result := true;
end;

{-----------------------------------------------------------------------------
  Procedure: FindStringIndex
  Author:    sam
  Date:      31-Okt-2005
  Arguments: Group: string; Value: string
  Result:    integer
  Description: get the settings number of value <Value> in group <Group>.
-----------------------------------------------------------------------------}
function TNVBSettings.FindStringIndex(Group: string; Value: string): integer;
var
  _SettingData: TNVBSettingData;
  Index: integer;
begin
  result := -1;
  for Index := 0 to FSettingList.Count - 1 do // go through all settings
  begin
    _SettingData := TNVBSettingData(FSettingList.Objects[Index]);
    if _SettingData.SettingType <> nvbString then continue; // check if type string
    if (lowercase(_SettingData.StrValue) = lowercase(Value)) and
      (Pos(Group, _SettingData.Name) > 0) then
    begin
      result := _SettingData.ID;
      break;
    end;
  end;
end;

//============================================================
// PROCEDURE TNVBSettings.Create
// discription : create the component and create the settings list.
// programmed by : GA
// tested by :
// last changes on : 19.09.00
//============================================================
constructor TNVBSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSave := false;
  FFilename := 'Settings'+cNVBSettingsFileExtension;
  FVersion:=cComponentVersion;
  Initialize;
end;

//============================================================
// PROCEDURE TNVBSettings.Destroy
// discription : release the component.
// programmed by : GA
// tested by :
// last changes on : 19.09.00
//============================================================
destructor TNVBSettings.Destroy;
begin
  CleanUp;
  inherited Destroy;
end;

//============================================================
// PROCEDURE TNVBSettings.Open
// discription : load the settings from the file into the settings list.
// programmed by : SA
// tested by :
// last changes on : 19.09.00
//============================================================
function TNVBSettings.Open: boolean;
begin
  Result := true;
  if not FIsLoaded then Result := LoadConfig;
end;

//============================================================
// PROCEDURE TNVBSettings.LoadConfig
// discription :
// programmed by : SA
// tested by :
// last changes on : 19.09.00 - skip empty lines
//============================================================
function TNVBSettings.LoadConfig: boolean;
var
  _SettingData: TNVBSettingData;
  _ExistingData: TNVBSettingData;
  i: integer;
  _Input: TStrings;
  _LineStr: string;
{$ifndef NoCryptSupport}
  _Cipher: TDCP_rc4;
{$endif}
  _SettingCount: integer;
  _index: Integer;
  _IdStr: string;
begin
  Result := false;
  _SettingCount := 0;
  gIsVersion107orOlder:=false;
  if FIsLoaded then Close;
  VerifyFilePath;
  if not FileExists(FFilePath + FFileName) then begin // check if file exists
    DoError(format('TNVBSettings.LoadConfig: Did not find file <%s>. Start application with default values.', [FFilePath + FFileName]), 0);
    FSettingsChanged:=true;
    FIsNew:=true;
    exit;
  end;
{$ifndef NoCryptSupport}
  _Cipher := TDCP_rc4.Create(Self);
  _Cipher.InitStr(FKeyString, TDCP_sha1); // initialize the cipher with a hash of the passphrase
{$endif}
  _Input := TStringList.Create;
  try
    _Input.LoadFromFile(FFilePath + FFileName);
    try
      for i := 0 to _input.count - 1 do
      begin
        _LineStr := _input[i];
        if _LineStr = '' then continue;
        if _LineStr[1] <> '*' then
        begin
          {$ifndef NoCryptSupport}
          if gIsVersion107orOlder and
             FCryptIt then _LineStr := _Cipher.DecryptString(_LineStr); // decrypt this line of the ini-file.
          {$endif}
          _SettingData:=TNVBSettingData.create;
          {$ifdef NoCryptSupport}
             ParseTokens(_SettingData, _LineStr);
          {$else}
             ParseTokens(_SettingData, _LineStr,_Cipher);
          {$endif}
          _IdStr := CreateIDStr(_SettingData.ID);
          if FSettingList.Find(_IdStr, _Index) then begin
            _ExistingData:=TNVBSettingData(FSettingList.objects[_Index]);
            _ExistingData.Free;
            FSettingList.Delete(_Index);
          end;
          FSettingList.AddObject(_IdStr, TObject(_SettingData));
          inc(_SettingCount);
        end;
      end;
      FIsLoaded := True; // the settings are loaded now.
      FSettingsChanged := _SettingCount <> FSettingList.count;
      Result := true;
    except
      DoError(format('Error in TNVBSettings.LoadConfig: Could not load file <%s>.', [FFilePath + FFileName]), 0);
      FIsLoaded := False;
    end;
  finally
    if assigned(_Input) then FreeAndNil(_Input);
{$ifndef NoCryptSupport}
    if assigned(_Cipher) then
    begin
      _Cipher.burn;
      FreeAndNil(_Cipher);
    end;
{$endif}    
  end;
end;

//============================================================
// PROCEDURE TNVBSettings.Close
// discription : save settings to file and cleanup the settings list.
// programmed by : SA
// tested by :
// last changes on : 03.01.01 - only execute if is open.
//============================================================
procedure TNVBSettings.Close;
var
  i1: integer;
  _SettingData: TNVBSettingData;
begin
  if FAutoSave then SaveConfig; // if FAutoSave is true, then save the configuration.
  for i1 := 0 to FSettingList.Count - 1 do // free the SettingList with all its records
  begin
    _SettingData := TNVBSettingData(FSettingList.Objects[0]);
    if assigned(_SettingData) then FreeAndNil(_SettingData);
    FSettingList.Delete(0);
  end;
  FSettingList.clear;
  FIsLoaded := False;
  FIsNew    := false;
  FSettingsChanged := False;
end;

//============================================================
// PROCEDURE TNVBSettings.SaveConfig
// discription :   save the settings into the file.
// programmed by : GA
// tested by :
// last changes on : 19.09.00
// 01.03.2002 SH: if file is readonly or in use, then generate error event.
//============================================================
function TNVBSettings.SaveConfig: boolean;
var
  i1: integer;
  _SettingData: TNVBSettingData;
  _LineStr: string;
  _Output: TStrings;
{$ifndef NoCryptSupport}
  _Cipher: TDCP_rc4;
{$endif}
begin
  Result := false;
  if csDesigning in ComponentState then exit;
  if FFileName = '' then begin
    DoError('Please specify a filename. The current filename is empty.', 0);
    exit;
  end;
  VerifyFilePath;
  if FileExists(FFilePath + FFileName) then begin // if the file already exists
    if isFileReadOnly(FFilePath + FFileName) then
    begin
      DoError(format('Can not save the settings to file <%s> because it is read-only.', [FFilePath + FFileName]), 0);
      exit;
    end;
    if isFileInUse(FFilePath + FFileName) then
    begin
      DoError(format('Can not save the settings to file <%s> because the file is in use by another application.', [FFilePath + FFileName]), 0);
      exit;
    end;
    if not FSettingsChanged then exit; // if no changes have been made, then it does not make sense to overwrite the file.
  end;
  // write the configuration file now

  _Output := TStringList.Create;
{$ifndef NoCryptSupport}
  _Cipher := TDCP_rc4.Create(Self);
  _Cipher.InitStr(FKeyString, TDCP_sha1); // initialize the cipher with a hash of the passphrase
{$endif}
  try
    try
      for i1 := 0 to FSettingList.Count - 1 do begin
        _SettingData := TNVBSettingData(FSettingList.Objects[i1]);
        if FCryptIt then _SettingData.isCrypted:=true;
        {$ifdef NoCryptSupport}
        _LineStr:=CreateLine(_SettingData);
        {$else}
        _LineStr:=CreateLine(_SettingData,_Cipher);
        {$endif}
        if _LineStr='' then continue;
        _Output.Add(_LineStr)
      end;
{$ifndef NoCryptSupport}
      if assigned(_Cipher) then
      begin
        _Cipher.Burn;
        FreeAndNil(_Cipher);
      end;
{$endif}
      _Output.SaveToFile(FFilePath + FFilename);
      Result := true;
    except
      DoError(format('Could not write the settings to the file <%s>. Check if you have rights to write to this drive.', [FFilePath + FFilename]), 0);
    end;
  finally
    if assigned(_Output) then FreeAndNil(_Output);
  end;
end;

//============================================================
// PROCEDURE TNVBSettings.GetSetting
// discription : find a setting by its ID number.
// programmed by : GA
// tested by :
// last changes on : 14.02.2002 - use FreeMem to release memory allocated by New.
//                   04.02.2003 - speed optimization.
//============================================================
function TNVBSettings.GetSetting(var SettingData: TNVBSettingData): boolean;
var
  _IDStr: string;
  _ExistingData: TNVBSettingData;
  _NewSettingData: TNVBSettingData;
begin
  result := false;
  _IDStr := '';
  _ExistingData:=GetSetting(SettingData.Name,SettingData.Id);
  if assigned(_ExistingData) then begin
    SettingData.assign(_ExistingData);
    result := true;
    exit;
  end;

  // if the entry was not found, then create a new one.
  if (SettingData.Descr <> '') and // only create it if there is a description available
     (SettingData.Id > -1) then
  begin // and at least a setting ID number.
    _NewSettingData:=TNVBSettingData.create;
    _NewSettingData.assign(SettingData);
    if _IDStr = '' then _IDStr := CreateIDStr(SettingData.ID);
    FSettingList.AddObject(_IDStr, _NewSettingData);
    result:=true;
  end
  else DoError(Format('The setting <%s> needs at least an ID and Description.', [SettingData.Name]), SettingData.ID);
end;


// Value must contain the default value; if there is already an entry in the
// configuration file, Value will be overwritten with the customer setting,
// otherwise a new entry with the default value will be created in the
// configuration file
function TNVBSettings.GetStringValue(const _Name: string;const ID: integer;const DefaultValue: string;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): string;
var
  _SettingData: TNVBSettingData;
begin
  result:='';
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := _Name;
    _SettingData.ID := ID;
    _SettingData.SettingType := nvbString;
    _SettingData.StrValue := DefaultValue;
    _SettingData.IntValue := 0;
    _SettingData.FloatValue := 0.0;
    _SettingData.BoolValue := false;
    _SettingData.ColorValue := clNone;
    _SettingData.CharValue := Chr(0);
    _SettingData.isVisible := _isVisible;
    _SettingData.isCrypted := _isCrypted;
    _SettingData.isReadOnly:= _isReadOnly;
    _SettingData.Descr := _Descr;
    if not GetSetting(_SettingData) then exit;
    Result := _SettingData.StrValue;
  finally
    _SettingData.Free;
  end;
end;

function TNVBSettings.GetPathValue(const _Name: string;const ID: integer;const DefaultValue: string;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): string;
var
  _SettingData: TNVBSettingData;
begin
  result:='';
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := _Name;
    _SettingData.ID := ID;
    _SettingData.SettingType := nvbPath;
    _SettingData.StrValue := DefaultValue;
    _SettingData.IntValue := 0;
    _SettingData.FloatValue := 0.0;
    _SettingData.BoolValue := false;
    _SettingData.ColorValue := clNone;
    _SettingData.CharValue := Chr(0);
    _SettingData.isVisible := _isVisible;
    _SettingData.isCrypted := _isCrypted;
    _SettingData.isReadOnly:= _isReadOnly;
    _SettingData.Descr := _Descr;
    if not GetSetting(_SettingData) then exit;
    Result := _SettingData.StrValue;
  finally
    _SettingData.Free;
  end;
end;

function TNVBSettings.GetFileValue(const _Name: string;const ID: integer;const DefaultValue: string;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): string;
var
  _SettingData: TNVBSettingData;
begin
  result:='';
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := _Name;
    _SettingData.ID := ID;
    _SettingData.SettingType := nvbFile;
    _SettingData.StrValue := DefaultValue;
    _SettingData.IntValue := 0;
    _SettingData.FloatValue := 0.0;
    _SettingData.BoolValue := false;
    _SettingData.ColorValue := clNone;
    _SettingData.CharValue := Chr(0);
    _SettingData.isVisible := _isVisible;
    _SettingData.isCrypted := _isCrypted;
    _SettingData.isReadOnly:= _isReadOnly;
    _SettingData.Descr := _Descr;
    if not GetSetting(_SettingData) then exit;
    Result := _SettingData.StrValue;
  finally
    _SettingData.Free;
  end;
end;

// Value must contain the default value; if there is already an entry in the
// configuration file, Value will be overwritten with the customer setting,
// otherwise a new entry with the default value will be created in the
// configuration file
function TNVBSettings.GetIntegerValue(const _Name: string;const ID: integer;const DefaultValue: integer;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): Integer;
var
  _SettingData: TNVBSettingData;
begin
  result:=0;
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := _Name;
    _SettingData.ID := ID;
    _SettingData.SettingType := nvbInteger;
    _SettingData.StrValue := IntToStr(DefaultValue);
    _SettingData.IntValue := DefaultValue;
    _SettingData.FloatValue := 0.0;
    _SettingData.BoolValue := false;
    _SettingData.ColorValue := clNone;
    _SettingData.CharValue := Chr(0);
    _SettingData.isVisible := _isVisible;
    _SettingData.isCrypted := _isCrypted;
    _SettingData.isReadOnly:= _isReadOnly;
    _SettingData.Descr := _Descr;
    if not GetSetting(_SettingData) then exit;
    Result := _SettingData.IntValue;
  finally
    _SettingData.Free;
  end;
end;


// Value must contain the default value; if there is already an entry in the
// configuration file, Value will be overwritten with the customer setting,
// otherwise a new entry with the default value will be created in the
// configuration file
function TNVBSettings.GetBoolValue(const _Name: string;const ID: integer;const DefaultValue: boolean;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): boolean;
var
  _SettingData: TNVBSettingData;
begin
  result:=false;
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := _Name;
    _SettingData.ID := ID;
    _SettingData.SettingType := nvbBool;
    _SettingData.StrValue := BooleanToStr(DefaultValue);
    _SettingData.IntValue := 0;
    _SettingData.FloatValue := 0.0;
    _SettingData.BoolValue := DefaultValue;
    _SettingData.ColorValue := clNone;
    _SettingData.CharValue := Chr(0);
    _SettingData.isVisible := _isVisible;
    _SettingData.isCrypted := _isCrypted;
    _SettingData.isReadOnly:= _isReadOnly;
    _SettingData.Descr := _Descr;
    if not GetSetting(_SettingData) then exit;
    Result := _SettingData.BoolValue;
  finally
    _SettingData.Free;
  end;
end;

function TNVBSettings.StringValue(const _Name: string; _ID: integer): string;
begin
  result := GetStringValue(_Name, _Id, '', '', true,false,false);
end;

function TNVBSettings.IntegerValue(const _Name: string; _ID: integer): Integer;
begin
  result := GetIntegerValue(_Name, _Id, 0, '', true,false,false);
end;


function TNVBSettings.BoolValue(const _Name: string; _ID: integer): boolean;
begin
  result := GetBoolValue(_Name, _Id, false, '', true,false,false);
end;

function TNVBSettings.FileValue(const _Name: string; _ID: integer): string;
begin
  result := GetFileValue(_Name, _Id, '', '', true,false,false);
end;

function TNVBSettings.PathValue(const _Name: string; _ID: integer): string;
begin
  result := GetPathValue(_Name, _Id, '', '', true,false,false);
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.GetCount
  Author:    sam
  Date:      26-Feb-2002
  Arguments: None
  Result:    Integer
  Description: get method for property count.
-----------------------------------------------------------------------------}
function TNVBSettings.GetCount: Integer;
begin
  Result := FSettingList.Count;
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.SetFileName
  Author:    sam
  Date:      02-Jul-2002
  Arguments: const Value: string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TNVBSettings.SetFileName(const Value: string);
begin
  FFilePath := ExtractFilePath(Value);
  FFileName := ExtractFileName(Value);
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.Initialize
  Author:    sam
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
  Description: initialize the component.
-----------------------------------------------------------------------------}
procedure TNVBSettings.Initialize;
begin
  if assigned(FSettingList) then exit;
  FSettingList := TStringList.Create;
  FSettingList.Sorted := true;
  FIsLoaded := False;
  FIsNew:=False;
  FSettingsChanged := false;
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.CleanUp
  Author:    sam
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TNVBSettings.CleanUp;
begin
  if not assigned(FSettingList) then exit;
  Close;
  FreeAndNil(FSettingList);
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.VerifyFilePath
  Author:    sam
  Date:      02-Jul-2002
  Arguments: None
  Result:    None
  Description: if the path is empty and the component is not in desigen mode,
  then set the path to the application directory.
-----------------------------------------------------------------------------}
procedure TNVBSettings.VerifyFilePath;
begin
  if FFilePath <> '' then exit;
  if assigned(Application) and
     (not (csDesigning in ComponentState)) then FFilePath := ExtractFilePath(Application.ExeName);
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.DoError
  Author:    Samuel
  Date:      23-Jul-2002
  Arguments: _ErrorMsg: String; _ID: Integer
  Result:    None
  Description: Error Handler. If the component user has assigned his own error handler to the OnError
               event, then execute the users error handler, otherwise execute the default error message.

-----------------------------------------------------------------------------}
procedure TNVBSettings.DoError(_ErrorMsg: string; _ID: Integer);
begin
  if Assigned(FOnError) then
    FOnError(Self, _ErrorMsg, _ID)
  else
    MessageDlg(format('Error: <%s> on setting <%d>.', [_ErrorMsg, _ID]), mtError, [mbOK], 0);
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.SetFilePath
  Author:    Not available
  Date:      05-Apr-2003
  Arguments: const Value: string
  Result:    None
  Description:
-----------------------------------------------------------------------------}
procedure TNVBSettings.SetFilePath(const Value: string);
begin
  FFilepath := Value;
end;

procedure TNVBSettings.SetVersion(const Value: string);
begin
  FVersion := cComponentVersion;
end;

{-----------------------------------------------------------------------------
  Procedure: GetSetting
  Author:    sam
  Date:      27-Okt-2004
  Arguments: const _Name: string
  Result:    TNVBDataSetting
  Description: returns the setting object with Name <_name>.
-----------------------------------------------------------------------------}
function TNVBSettings.GetSetting(const _Name: string): TNVBSettingData;
var
_index:Longint;
begin
  result:=nil;
  _index:=FindSettingIdx(_Name);
  if _index=-1 then exit;
  if _index>=FSettingList.count then exit;
  result:= TNVBSettingData(FSettingList.Objects[_index]);
end;

{*-----------------------------------------------------------------------------
  Procedure: FindSettingIdx
  Author:    sam
  Date:
  Arguments: const _Name: string
  Result:    integer
  Description: return the list index of setting with Name <_Name>.
-----------------------------------------------------------------------------}
function TNVBSettings.FindSettingIdx(const _Name: string): integer;
var
i:integer;
_tmpData :TNVBSettingData;
begin
  result:=-1;
  if _Name='' then exit;
  for i := 0 to FSettingList.count - 1 do begin
    _tmpData := TNVBSettingData(FSettingList.Objects[I]);
    if not SameText(_Name,_tmpData.Name) then continue;
    result:=i;
    exit;
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: FindSettingIdx
  Author:    sam
  Date:
  Arguments: const _ID: integer
  Result:    integer
  Description: return the list index of setting with ID <_ID>.
-----------------------------------------------------------------------------}
function TNVBSettings.FindSettingIdx(const _ID: integer): integer;
var
_IDStr:string;
begin
  result:=-1;
  if _Id < 0 then exit;
  _IDStr := CreateIDStr(_ID);
  result:=FSettingList.IndexOf(_IDStr);
end;

{*-----------------------------------------------------------------------------
  Procedure: GetSetting
  Author:    sam
  Date:
  Arguments: const _ID: integer
  Result:    TNVBSettingData
  Description: try to find setting by ID <_ID>.
-----------------------------------------------------------------------------}
function TNVBSettings.GetSetting(const _ID: integer): TNVBSettingData;
var
_index:Longint;
begin
  result:=nil;
  _index:=FindSettingIdx(_ID);
  if _index=-1 then exit;
  if _index>=FSettingList.count then exit;
  result:= TNVBSettingData(FSettingList.Objects[_index]);
end;

{-----------------------------------------------------------------------------
  Procedure: GetSetting
  Author:    sam
  Date:      27-Okt-2004
  Arguments: const _Name: string;ID: integer
  Result:    TNVBSettingData
  Description: try to find the setting by ID <_ID>. If not found, then
               try to find the setting by Name <_Name>.
-----------------------------------------------------------------------------}
function TNVBSettings.GetSetting(const _Name: string;_ID: integer): TNVBSettingData;
begin
  result:=GetSetting(_ID);
  if not assigned(result) then result:=GetSetting(_Name);
end;

end.

