{***************************************************************
 *
 * Unit Name: uDPTSettings
 * Purpose  : A component to keep settings of an application in memory, load/save them and reinitialize the application
 * without restart.
 *
 * Author   : G.Amrein & S.Herzog

 * History  :
 * 19.09.2000: skip empty lines in the file.
 *             added method <FindIntegerValue>.
 * 03.01.2001: changes in method <close>. Exit if file is not open.
 * 05.01.2001: added methods SetInteger,SetString.
 * 27.03.2001: added methos FindStringValue
 * 15.11.2001: Check if the file is not read-only in method saveconfig.
 * 19.11.2001: added property <Count>. This is the number of settings stored in the list.
 * 21.11 2001: added method SetBoolean.
 * 10.12.2001: added method SetFile.
 * 08.01.2002 -SH delimiter is now an constant <cDelimiterChar>. The field delimiter has changed from <';'> to <'|'>.
 * 11.01.2002 -SH Extended method GetSetting so that a setting can also be found by its name.
 * 21.01.2002 -SH added support for tracefile.
 * 12.02.2002 -SH bugfix in GetSetting. The Freemem command was missing.
 * 21.02.2002 -SH removed the tracefile stuff. Crosslinking of components was not a good idea.
 * 26.02.2002 -SH added Methods <GroupExists>,<SettingExists>.
 * 06.03.2002 -SH changed methods open and refresh to functions
 * 21.03.2002 -SH added method <SetColor>.
 * 15.06.2002 -SH added method SetFileName to set the filename property
 * 25.06.2002 -SH this class is now implemented as singleton pattern.
 * This means, the settings are only instanciated and initialized once.
 * 02.07.2002 -SH set the path to the application directory.
 * 11.07.2002 -SH bugfix in GetSetting. Added <close> into loadconfig.
 * 22.07.2002 -SH added FontStylesToInteger/IntegerToFontStyles methods.
 * 23.07.2002 -SH reviewed Error Handling.
 *            -SH added destructor for singleton class.
 * 14.08.2002 -SH moved method Register into file <RegisterSettingComponents.pas>
 * 27.08.2002 -SH removed not needed units from the uses statement.
 * 01.10.2002 -SH added method <SetFloat>.
 * 16.10.2002 -SH Bugfix for running this component on a read-only drive.
 * 28.10.2002 -SH bugfix in GetEnumValue.
 *            -SH added method SetEnum
 * 26.11.2002 -SH added read-only property <isLoaded>.
 * 16.12.2002 -SH the setting can now also be set when only the setting name is available.
 * 17.12.2002 -SH added method <FindSetting>.
 * 06.01.2003 -SH added resource statement $R.
 * 04.02.2003 -SH speed optimization.
 *            -SH added property <AutoSave>.
 * 17.02.2003 -SH made change to in LoadConfig,SaveConfig and added Crypt function.
 * 19.02.2003 -SH renamed unit and class.
 * 05.04.2003 -SH added FilePath property.
 * 07.06.2003 -SH added <const> in function parameters.
 * 01.07.2003 -SH Flags are now used to make settings visible,hidden,readonly.
 * Version 1.00 08.09.2003 -SH added version property.
 * Version 1.01 17.10.2003 -SH small fix in SaveConfig. Do not save during design-time.
 * Version 1.02 13.03.2004 -SH version property was not initialized in constructor.
 * Version 1.03 15.10.2004 -SH fix for enum values.
 * Version 1.04 29.10.2004 -SH big re-work to make enum also running.
 * Version 1.05 14.10.2005 -SH fixed memory leak.
 * Version 1.06 31.10.2005 -SH fixed return value of FindStringValue. the first char was cut off.
*  Version 1.07 20.06.2006 -SH fixed memory leaks.
*  Version 1.08 28.06.2006 -SH re-work of flags. is now splitted in isVisible,isCrypted,isReadOnly.
*  Version 1.09 29.06.2006 -SH its now possible to crypt a single setting.
*  Version 1.10 14.08.2006 -SH fix in FindSettingIdx.
                           -SH modifications to crypt a single setting instead of the whole file.
* Version  1.11 22.11.2006 -SH changed parameterlist of method GetEnumItems to aviod memory-leaks.
* Version  1.12 23.02.2010 -SH added define "NoCryptSupport". If this define is set, then the crypt feature is not compiled into the package.
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
  cComponentVersion = '1.12';

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
  protected
    procedure Initialize;
    procedure CleanUp;
  public
    procedure DeleteSetting(const Name: string; ID: integer);
    // with Delphi 4>, this could be done with method overload
    // this methods get the values and creates the list entry if not already existing.
    function GetStringValue(const Name: string;const ID: integer;const DefaultValue: string;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): string;
    function GetIntegerValue(const Name: string;const ID: integer;const DefaultValue: integer;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): Integer;
    function GetFloatValue(const Name: string;const ID: integer;const DefaultValue: double;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): Double;
    function GetBoolValue(const Name: string;const ID: integer;const DefaultValue: boolean;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): boolean;
    function GetColorValue(const Name: string;const ID: integer;const DefaultValue: TColor;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): TColor;
    function GetEnumValue(const Name: string;const ID: integer;const EnumListStr: string;const DefaultValue: integer;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): Integer;
    function GetCharValue(const Name: string;const ID: integer;const DefaultValue: char;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): Char;
    function GetFileValue(const Name: string;const ID: integer;const DefaultValue: string;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): string;
    function GetPathValue(const Name: string;const ID: integer;const DefaultValue: string;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): string;
    // this methods just gets the value without create
    function StringValue(const Name: string; ID: integer): string;
    function IntegerValue(const Name: string; ID: integer): Integer;
    function FloatValue(const Name: string; ID: integer): Double;
    function BoolValue(const Name: string; ID: integer): boolean;
    function ColorValue(const Name: string; ID: integer): TColor;
    function EnumValue(const Name: string; ID: integer): Integer;
    function CharValue(const Name: string; ID: integer): Char;
    function FileValue(const Name: string; ID: integer): string;
    function PathValue(const Name: string; ID: integer): string;
    // this methods just set the values without create
    function SetString(const _Name: string; _ID: integer; _value: string): boolean;
    function SetFile(const _Name: string; _ID: integer; _value: string): boolean;
    function SetInteger(const _Name: string; _ID: integer; _value: Integer): boolean;
    function SetBoolean(const _Name: string; _ID: integer; _value: Boolean): boolean;
    function SetPath(const _Name: string; _ID: integer; _value: string): boolean;
    function SetColor(const _Name: string; _ID: integer; _value: TColor): boolean;
    function SetFloat(const _Name: string; _ID: integer; _value: Double): boolean;
    function SetEnum(const _Name: string; _ID: integer; _value: Integer): boolean;
    // find the setting name for a specific value in a group of settings
    function FindIntegerValue(Group: string; Value: Integer): string;
    function FindStringValue(Group: string; Value: string): string;  // returns the item name
    function FindStringIndex(Group: string; Value: string): integer; // returns the item index.
    function GroupCount(_Group: string): Integer; // number of settings in the group.
    function GroupExists(_Group: string): Boolean; // checks if the group exists in the ini-file.
    function SettingExists(Name: string): Boolean; // check if the setting exists.
    function FindSetting(const _Name: string): Integer;overload; // try to find the setting with Name <Name>. Returns the ID of the setting.
    function FindSettingIdx(const _Name:string):integer;overload;    // try to find the setting with name <name>. Returns the Index of the setting.
    function FindSettingIdx(const _ID:integer):integer;overload;    // try to find the setting with ID <ID>. Returns the Index of the setting.
    function GetSetting(const Name: string): TNVBSettingData;overload; // try to find the setting with Name <Name>. Returns setting object.
    function GetSetting(const ID: integer): TNVBSettingData;overload; // try to find the setting with ID <ID>. Returns setting object.
    function GetSetting(const _Name:string;_ID: integer): TNVBSettingData;overload; // try to find the setting with Name or ID <ID>. Returns setting object.
    function SaveConfig: Boolean; // save all the settings to the file
    function LoadConfig: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Open: boolean;
    procedure Close;
    function Refresh: boolean; // close and reload the settings from ini-file.
    function GetEnumItems(const Name: string; ID: integer;var ItemList:TStrings):integer;
    property isLoaded: Boolean read FIsLoaded;
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

  TNVBSingleTonSettings = class(TNVBSettings)
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
    class function RefCount: Integer;
    destructor Destroy; override;
  end;

function FontStylesToInteger(_FontStyle: TFontStyles): Integer;
function IntegerToFontStyles(_Value: Integer): TFontStyles;

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

var
  Instance: TNVBSingleTonSettings = nil;
  Ref_Count: Integer = 0;

  {-----------------------------------------------------------------------------
    Procedure: TNVBSettings.FontStylesToInteger
    Author:    sam
    Date:      22-July-2002
    Arguments:  _FontStyle:TFontStyles
    Result:    Integer
    Description: converts a font style property into an integer value.
  -----------------------------------------------------------------------------}
function FontStylesToInteger(_FontStyle: TFontStyles): Integer;
var
  _Value: Integer;
begin
  _Value := 0;
  if fsBold in _FontStyle then _Value := _Value + 1;
  if fsItalic in _FontStyle then _Value := _Value + 2;
  if fsUnderline in _FontStyle then _Value := _Value + 4;
  if fsStrikeOut in _FontStyle then _Value := _Value + 8;
  Result := _Value;
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.IntegerToFontStyles
  Author:    sam
  Date:      22-July-2002
  Arguments:  _Value:Integer
  Result:    Integer
  Description: converts a integer value into font style property.
-----------------------------------------------------------------------------}
function IntegerToFontStyles(_Value: Integer): TFontStyles;
var
  _FontStyles: TFontStyles;
begin
  _FontStyles := [];
  if _Value and 1 = 1 then _FontStyles := _FontStyles + [fsBold];
  if _Value and 2 = 2 then _FontStyles := _FontStyles + [fsItalic];
  if _Value and 4 = 4 then _FontStyles := _FontStyles + [fsUnderline];
  if _Value and 8 = 8 then _FontStyles := _FontStyles + [fsStrikeOut];
  Result := _FontStyles;
end;

// ****************************************************************************
// ************** Singleton Class of TNVBSettings.********************************
// ****************************************************************************
// ****************************************************************************

destructor TNVBSingleTonSettings.Destroy;
begin
  // very important, do not delete this method.
end;

procedure TNVBSingleTonSettings.FreeInstance;
begin
  Dec(Ref_Count);
  if (Ref_Count = 0) then
  begin
    if assigned(Instance) then TNVBSingleTonSettings(Instance).CleanUp;
    Instance := nil;
    inherited FreeInstance;
  end;
end;

class function TNVBSingleTonSettings.NewInstance: TObject;
begin
  if (not Assigned(Instance)) then
  begin
    Instance := TNVBSingleTonSettings(inherited NewInstance);
    TNVBSingleTonSettings(Instance).initialize;
  end;
  Result := Instance;
  Inc(Ref_Count);
end;

class function TNVBSingleTonSettings.RefCount: Integer;
begin
  Result := Ref_Count;
end;

procedure TNVBSettings.DeleteSetting(const Name: string; ID: integer);
var
  i: longint;
  _tmpData: TNVBSettingData;
  _IDStr: string;
  _Index: Integer;
begin
  _IDStr := '';
  if ID > -1 then
  begin // search by ID number
    _IDStr := CreateIDStr(ID);
    if FSettingList.Find(_IDStr, _Index) then
    begin
      _tmpData := TNVBSettingData(FSettingList.Objects[_Index]);
      if assigned(_tmpData) then FreeAndNil(_tmpData);
      FSettingList.Delete(_Index);
      exit;
    end;
  end
  else
  begin // else search by name
    for i := 0 to FSettingList.Count - 1 do // iterate through all settings
    begin
      _tmpData := TNVBSettingData(FSettingList.Objects[i]); // get a setting object
      if (assigned(_tmpData)) and (Name = _tmpData.Name) then
      begin // found the setting
        FreeAndNil(_tmpData);
        FSettingList.Delete(i);
        exit;
      end;
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.FindSetting
  Author:    sam
  Date:      17-Dez-2002
  Arguments: Name:String
  Result:    Integer
  Purpose:   find a setting by its name and return the ID.
             If the setting can not be found then return -1.
  History:
-----------------------------------------------------------------------------}
function TNVBSettings.FindSetting(const _Name: string): Integer; // try to find the setting with Name <Name>. Returns the ID of the setting.
var
  i: longint;
  _tmpData: TNVBSettingData;
begin
  result := -1;
  for i := 0 to FSettingList.Count - 1 do // iterate through all settings
  begin
    _tmpData := TNVBSettingData(FSettingList.Objects[i]); // get a setting object
    if (assigned(_tmpData)) and (_Name = _tmpData.Name) then
    begin // found the setting
      Result := _tmpData.ID;
      Break;
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.SettingExists
  Author:    sam
  Date:      26-Feb-2002
  Arguments: Name: String
  Result:    Boolean
  Description: check if a setting exists.
-----------------------------------------------------------------------------}
function TNVBSettings.SettingExists(Name: string): Boolean;
begin
  result:=(FindSetting(Name)<>-1);
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.GroupExists
  Author:    sam
  Date:      26-Feb-2002
  Arguments: Group: string
  Result:    Boolean
  Description: checks if the group <group> exists.
-----------------------------------------------------------------------------}
function TNVBSettings.GroupExists(_Group: string): Boolean;
begin
  Result := Boolean(GroupCount(_Group));
end;

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

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.SetEnum
  Author:    Not available
  Date:      28-Okt-2002
  Arguments: Name: string; ID: integer; _value:Integer
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function TNVBSettings.SetEnum(const _Name: string; _ID: integer; _value: Integer): boolean;
var
  _SettingData: TNVBSettingData;
begin
  result:=false;
  _SettingData:=GetSetting(_Name,_ID);
  if not assigned(_SettingData) then exit;
  _SettingData.StrValue := IntToStr(_value);
  _SettingData.IntValue := _value;
  FSettingsChanged := true;
  Result := true;
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.SetFloat
  Author:    sam
  Date:      03-Okt-2002
  Arguments: Name: string; ID, _value: Double
  Result:    boolean
  Purpose:
  History:
-----------------------------------------------------------------------------}
function TNVBSettings.SetFloat(const _Name: string; _ID: integer; _value: Double): boolean;
var
  _SettingData: TNVBSettingData;
begin
  result:=false;
  _SettingData:=GetSetting(_Name,_ID);
  if not assigned(_SettingData) then exit;
  _SettingData.StrValue := FloatToStr(_value);
  _SettingData.FloatValue := _value;
  FSettingsChanged := true;
  Result := true;
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBSettings.SetColor
  Author:    sam
  Date:      21-Mrz-2002
  Arguments: Name: string; ID: integer;_value: TColor
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function TNVBSettings.SetColor(const _Name: string; _ID: integer; _value: TColor): boolean;
var
  _SettingData: TNVBSettingData;
begin
  result:=false;
  _SettingData:=GetSetting(_Name,_ID);
  if not assigned(_SettingData) then exit;
  _SettingData.StrValue := ColorToString(_value);
  _SettingData.ColorValue := _value;
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

//============================================================
// PROCEDURE TNVBSettings.FindIntegerValue
// discription : find the name of setting inside a group with
// the value <value>. The parameter <group> is case sensitive !
// programmed by : SH
// tested by :
// last changes on : 19.09.00
//============================================================
function TNVBSettings.FindIntegerValue(Group: string; Value: Integer): string;
var
  _SettingData: TNVBSettingData;
  Index: integer;
  _Name: string;
begin
  FindIntegerValue := '';
  for Index := 0 to FSettingList.Count - 1 do // go through all settings
  begin
    _SettingData := TNVBSettingData(FSettingList.Objects[Index]);
    if _SettingData.SettingType <> nvbInteger then continue; // check if type integer
    if (_SettingData.IntValue = Value) and
      (Pos(Group, _SettingData.Name) > 0) then
    begin
      _Name := _SettingData.Name;
      Delete(_Name, 1, Length(group) + 1);
      FindIntegerValue := _Name;
      break;
    end;
  end;
end;

//============================================================
// PROCEDURE TNVBSettings.FindStringValue
// discription : find the name of setting inside a group with
// the value <value>. The parameter <group> is case sensitive !
// Returns the setting name.
// programmed by : SA
// tested by :
// last changes on : 19.09.00
//   31.10.2005 SH fixed return value. the first char was cut off.
//============================================================
function TNVBSettings.FindStringValue(Group: string; Value: string): string;
var
  _SettingData: TNVBSettingData;
  Index: integer;
  _Name: string;
begin
  result := '';
  for Index := 0 to FSettingList.Count - 1 do // go through all settings
  begin
    _SettingData := TNVBSettingData(FSettingList.Objects[Index]);
    if _SettingData.SettingType <> nvbString then continue; // check if type string
    if (_SettingData.StrValue = Value) and
      (Pos(Group, _SettingData.Name) > 0) then
    begin
      _Name := _SettingData.Name;
      Delete(_Name, 1, Length(group));
      result := _Name;
      break;
    end;
  end;
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
// PROCEDURE TNVBSettings.Refresh
// discription :  reload the settings from the file.
// programmed by : SH
// tested by :
// last changes on : 19.09.00
//============================================================
function TNVBSettings.Refresh: boolean;
begin
  Result := Open;
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
  else begin
    DoError(Format('The setting <%s> needs at least an ID and Description.', [SettingData.Name]), SettingData.ID);
//    if assigned(_NewSettingData) then FreeAndNil(_NewSettingData);
  end;
end;


// Value must contain the default value; if there is already an entry in the
// configuration file, Value will be overwritten with the customer setting,
// otherwise a new entry with the default value will be created in the
// configuration file
function TNVBSettings.GetStringValue(const Name: string;const ID: integer;const DefaultValue: string;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): string;
var
  _SettingData: TNVBSettingData;
begin
  result:='';
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := Name;
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

function TNVBSettings.GetPathValue(const Name: string;const ID: integer;const DefaultValue: string;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): string;
var
  _SettingData: TNVBSettingData;
begin
  result:='';
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := Name;
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

function TNVBSettings.GetFileValue(const Name: string;const ID: integer;const DefaultValue: string;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): string;
var
  _SettingData: TNVBSettingData;
begin
  result:='';
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := Name;
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
function TNVBSettings.GetIntegerValue(const Name: string;const ID: integer;const DefaultValue: integer;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): Integer;
var
  _SettingData: TNVBSettingData;
begin
  result:=0;
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := Name;
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
function TNVBSettings.GetFloatValue(const Name: string;const ID: integer;const DefaultValue: double;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): Double;
var
  _SettingData: TNVBSettingData;
begin
  result:=0.0;
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := Name;
    _SettingData.ID := ID;
    _SettingData.SettingType := nvbFloat;
    _SettingData.StrValue := FloatToStr(DefaultValue);
    _SettingData.IntValue := 0;
    _SettingData.FloatValue := DefaultValue;
    _SettingData.BoolValue := false;
    _SettingData.ColorValue := clNone;
    _SettingData.CharValue := Chr(0);
    _SettingData.isVisible := _isVisible;
    _SettingData.isCrypted := _isCrypted;
    _SettingData.isReadOnly:= _isReadOnly;
    _SettingData.Descr := _Descr;
    if not GetSetting(_SettingData) then exit;
    Result := _SettingData.FloatValue;
  finally
    _SettingData.Free;
  end;
end;

// Value must contain the default value; if there is already an entry in the
// configuration file, Value will be overwritten with the customer setting,
// otherwise a new entry with the default value will be created in the
// configuration file
function TNVBSettings.GetBoolValue(const Name: string;const ID: integer;const DefaultValue: boolean;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): boolean;
var
  _SettingData: TNVBSettingData;
begin
  result:=false;
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := Name;
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

// Value must contain the default value; if there is already an entry in the
// configuration file, Value will be overwritten with the customer setting,
// otherwise a new entry with the default value will be created in the
// configuration file
function TNVBSettings.GetColorValue(const Name: string;const ID: integer;const DefaultValue: TColor;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): TColor;
var
  _SettingData: TNVBSettingData;
begin
  result:=0;
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := Name;
    _SettingData.ID := ID;
    _SettingData.SettingType := nvbColor;
    _SettingData.StrValue := ColorToString(DefaultValue);
    _SettingData.IntValue := 0;
    _SettingData.FloatValue := 0.0;
    _SettingData.BoolValue := false;
    _SettingData.ColorValue := DefaultValue;
    _SettingData.CharValue := Chr(0);
    _SettingData.isVisible := _isVisible;
    _SettingData.isCrypted := _isCrypted;
    _SettingData.isReadOnly:= _isReadOnly;
    _SettingData.Descr := _Descr;
    if not GetSetting(_SettingData) then exit;
    Result := _SettingData.ColorValue;
  finally
    _SettingData.Free;
  end;
end;

// Value must contain the default value; if there is already an entry in the
// configuration file, Value will be overwritten with the customer setting,
// otherwise a new entry with the default value will be created in the
// configuration file

// example how to use :   settings1.GetEnumValue('Font',1,'(fsBold,fsItalic,fsUnderline,fsStrikeOut)',0,'Styles of a font property.',0);
function TNVBSettings.GetEnumValue(const Name: string;const ID: integer;const EnumListStr: string;const DefaultValue: integer;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): Integer;
var
  _SettingData: TNVBSettingData;
  _TypeStr: string;
begin
  result:=0;
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := Name;
    _SettingData.ID := ID;
    _SettingData.SettingType := nvbEnum;
    _TypeStr := 'Enum(' + EnumListStr + ')';
    FillEnumList(_TypeStr,_SettingData.EnumList);
    _SettingData.StrValue := inttostr(DefaultValue);
    _SettingData.IntValue := DefaultValue;
    _SettingData.FloatValue := 0.0;
    _SettingData.BoolValue := false;
    _SettingData.ColorValue := clNone;
    _SettingData.CharValue := Chr(0);
    _SettingData.isVisible:= _isVisible;
    _SettingData.isCrypted:= _isCrypted;
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
function TNVBSettings.GetCharValue(const Name: string;const ID: integer;const DefaultValue: char;const _Descr: string;const _isVisible: Boolean;const _isCrypted:boolean;const _isReadOnly:boolean): Char;
var
  _SettingData: TNVBSettingData;
begin
  result:='?';
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := Name;
    _SettingData.ID := ID;
    _SettingData.SettingType := nvbChar;
    _SettingData.StrValue := DefaultValue;
    _SettingData.IntValue := 0;
    _SettingData.FloatValue := 0.0;
    _SettingData.BoolValue := false;
    _SettingData.ColorValue := clNone;
    _SettingData.CharValue := DefaultValue;
    _SettingData.isVisible := _isVisible;
    _SettingData.isCrypted := _isCrypted;
    _SettingData.isReadOnly:= _isReadOnly;
    _SettingData.Descr := _Descr;
    if not GetSetting(_SettingData) then exit;
    result := _SettingData.CharValue;
  finally
    _SettingData.Free;
  end;
end;

function TNVBSettings.StringValue(const Name: string; ID: integer): string;
begin
  result := GetStringValue(Name, Id, '', '', true,false,false);
end;

function TNVBSettings.IntegerValue(const Name: string; ID: integer): Integer;
begin
  result := GetIntegerValue(Name, Id, 0, '', true,false,false);
end;

function TNVBSettings.FloatValue(const Name: string; ID: integer): Double;
begin
  result := GetFloatValue(Name, Id, 0, '', true,false,false);
end;

function TNVBSettings.BoolValue(const Name: string; ID: integer): boolean;
begin
  result := GetBoolValue(Name, Id, false, '', true,false,false);
end;

function TNVBSettings.ColorValue(const Name: string; ID: integer): TColor;
begin
  result := GetColorValue(Name, Id, 0, '', true,false,false);
end;

function TNVBSettings.EnumValue(const Name: string; ID: integer): Integer;
begin
  result := GetEnumValue(Name, Id, '',0, '', true,false,false);
end;

function TNVBSettings.CharValue(const Name: string; ID: integer): Char;
begin
  result := GetCharValue(Name, Id, '?', '', true,false,false);
end;

function TNVBSettings.FileValue(const Name: string; ID: integer): string;
begin
  result := GetFileValue(Name, Id, '', '', true,false,false);
end;

function TNVBSettings.PathValue(const Name: string; ID: integer): string;
begin
  result := GetPathValue(Name, Id, '', '', true,false,false);
end;

//*************************************************************************
//Procedure: TNVBSettings.GroupCount
//Description: find out the number of items in a group
//Author: SH
//History: 10.08.2001 23:32:50
//*************************************************************************
function TNVBSettings.GroupCount(_Group: string): Integer;
var
  i, _count: longint;
  _tmpData: TNVBSettingData;
begin
  _count := 0;
  for i := 0 to FSettingList.Count - 1 do // iterate through all settings
  begin
    _tmpData := TNVBSettingData(FSettingList.Objects[i]); // get a setting object
    if Pos(_Group, _tmpData.Name) > 0 then inc(_count); // if it belongs to the group, then increment.
  end;
  result := _count;
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
  if FFilePath = '' then
  begin
    if assigned(Application) and
      (not (csDesigning in ComponentState)) then FFilePath := ExtractFilePath(Application.ExeName);
  end;
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

function TNVBSettings.GetEnumItems(const Name: string;ID: integer;var ItemList:TStrings): integer;
var
  _SettingData: TNVBSettingData;
begin
  result:=-1;
  if not assigned(ItemList) then exit;
  _SettingData:=TNVBSettingData.create;
  try
    _SettingData.Name := Name;
    _SettingData.ID := ID;
    _SettingData.SettingType := nvbEnum;
    GetSetting(_SettingData);
    ItemList.assign(_SettingData.EnumList);
    result:=ItemList.count;
  finally
    _SettingData.free;
  end;  
end;

{-----------------------------------------------------------------------------
  Procedure: FindSetting
  Author:    sam
  Date:      27-Okt-2004
  Arguments: const Name: string
  Result:    TNVBDataSetting
  Description: returns the setting object with Name <_name>.
-----------------------------------------------------------------------------}
function TNVBSettings.GetSetting(const Name: string): TNVBSettingData;
var
_index:Longint;
begin
  result:=nil;
  _index:=FindSettingIdx(Name);
  if (_index>-1) and
     (_index<FSettingList.count) then result:= TNVBSettingData(FSettingList.Objects[_index]);
end;

function TNVBSettings.FindSettingIdx(const _Name: string): integer;
var
i:integer;
_tmpData :TNVBSettingData;
begin
  result:=-1;
  if _Name='' then exit;
  for i := 0 to FSettingList.count - 1 do
  begin
    _tmpData := TNVBSettingData(FSettingList.Objects[I]);
    if _tmpData.Name = _Name then
    begin
      result:=i;
      exit;
    end;
  end;
end;

function TNVBSettings.FindSettingIdx(const _ID: integer): integer;
var
_IDStr:string;
begin
  result:=-1;
  if _Id < 0 then exit;
  _IDStr := CreateIDStr(_ID);
  if not FSettingList.Find(_IDStr, result) then result:=-1;
end;

function TNVBSettings.GetSetting(const ID: integer): TNVBSettingData;
var
_index:Longint;
begin
  result:=nil;
  _index:=FindSettingIdx(ID);
  if (_index>-1) and
     (_index<FSettingList.count) then result:= TNVBSettingData(FSettingList.Objects[_index]);
end;

{-----------------------------------------------------------------------------
  Procedure: GetSetting
  Author:    sam
  Date:      27-Okt-2004
  Arguments: const Name: string;ID: integer
  Result:    TNVBSettingData
  Description:
-----------------------------------------------------------------------------}
function TNVBSettings.GetSetting(const _Name: string;_ID: integer): TNVBSettingData;
begin
  result:=GetSetting(_ID);
  if not assigned(result) then result:=GetSetting(_Name);
end;

end.

