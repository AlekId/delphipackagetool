{***************************************************************
 *
 * Unit Name: uDPTSettingsMisc
 * Purpose  : misc. routines used by the DPTSetting components.
 * Author   :
 * History  :
 ****************************************************************}

unit uDPTSettingsMisc;

interface
uses Graphics,
     {$ifndef NoCryptSupport}
     DCPrc4,
     DCPsha1,
     {$endif}
     Classes;


const
  cDelimiterChar = '|';
  cNVBSettingsFileExtension='.ini';
  cNVBFilename='_CustomColors'+cNVBSettingsFileExtension;
  UpDownWidth=16;

type

  TNVBSettingType = (nvbNone,
                     nvbString,
                     nvbInteger,
                     nvbFloat,
                     nvbBool,
                     nvbColor,
                     nvbEnum,
                     nvbChar,
                     nvbVariant,
                     nvbFile,
                     nvbPath);


  TNVBSettingData = class(TObject)
    Name: string;
    SettingType: TNVBSettingType;
    StrValue: string;
    IntValue: integer;
    FloatValue: double;
    BoolValue: boolean;
    ColorValue: TColor;
    CharValue: char;
    EnumList: TStrings;
    isVisible: Boolean;
    isReadOnly: Boolean;
    isCrypted:Boolean;
    Descr: string;
  public
    constructor create;
    destructor Destroy;override;
    procedure assign(_dataobject:TNVBSettingData);
  end;

  TNVBSettingErrorEvent= procedure (Sender: TObject; ErrorMsg:string ; Id:integer) of object;

  TNVBNextSettingEvent = procedure(Sender: TObject; Data: TNVBSettingData) of object;

  TNVBNextDirEvent = procedure(Sender: TObject; DirName: string) of object;

  TNVBStringChangeEvent = procedure(Sender: TObject; Value: string) of object;

  TNVBIntegerChangeEvent = procedure(Sender: TObject; Value: integer) of object;

  TNVBFloatChangeEvent = procedure(Sender: TObject; Value: double) of object;

  TNVBBoolChangeEvent = procedure(Sender: TObject; Value: boolean) of object;

  TNVBColorChangeEvent = procedure(Sender: TObject; Value: TColor) of object;

  TNVBEnumChangeEvent = procedure(Sender: TObject; Value: integer) of object;

  TNVBCharChangeEvent = procedure(Sender: TObject; Value: char) of object;

  TNVBFileChangeEvent = procedure(Sender: TObject; Value: string) of object;

  TNVBChangeEvent = function(Sender: TObject; oldValue,newValue: string):boolean of object;

  TNVBUpdateString = procedure(const _Value: string) of object;

  TNVBUpdateInteger = procedure(const _Value: integer) of object;

  TNVBUpdateFloat = procedure(const _Value: double) of object;

  TNVBUpdateBool = procedure(const _Value: boolean) of object;

  TNVBUpdateColor = procedure(const _Value: TColor) of object;

  TNVBUpdateEnum = procedure(const _Value: Integer) of object;

  TNVBUpdateChar = procedure(const _Value: char) of object;

  TNVBUpdateFile = procedure(const _Value: string) of object;

  TNVBUpdatePath = procedure(const _Value: string) of object;

{$ifdef NoCryptSupport}
procedure ParseTokens(var SettingData: TNVBSettingData; LineStr: string);overload;
function CreateLine(const _SettingData: TNVBSettingData):string;overload;
{$else}
procedure ParseTokens(var SettingData: TNVBSettingData; LineStr: string;const _Cipher: TDCP_rc4);overload;
function CreateLine(const _SettingData: TNVBSettingData;const _Cipher: TDCP_rc4):string;overload;
{$endif}
function GetToken(var Text: string;const _Delimiter: char): string;
procedure PrepSetRec(var Data: TNVBSettingData);
function TypeToStr(SettingData: TNVBSettingData): string;
function StrtoType(_TypeStr:string):TNVBSettingType;
function BooleanToStr(Value: boolean): string;
procedure FillEnumList(_EnumStr: string;var EnumList:TStrings);


implementation

uses
  SysUtils,
  uDPTMisc;

{$ifdef NoCryptSupport}
procedure ParseTokens(var SettingData: TNVBSettingData; LineStr: string);
var
_NameStr: string;
_IDStr: string;
_TypeStr: string;
_ValueStr: string;
_DescrStr: string;
_isVisibleStr: string;
_isCryptedStr: string;
_isReadOnlyStr: string;
_TmpStr: string;
begin
  // initialize the temporary variables
  _NameStr:='';
  _IDStr:='';
  _TypeStr:='';
  _ValueStr:='';
  _DescrStr:='';
  _isVisibleStr:='';
  _isCryptedStr:='';
  _isReadOnlyStr:='';
  // parse the tokens
  _NameStr:=GetToken(LineStr,':');
  while LineStr<>'' do
  begin
    _TmpStr:=GetToken(LineStr,cDelimiterChar);
    if Pos('ID=',_TmpStr)=1 then _IDStr:=_TmpStr
    else if Pos('Type=',_TmpStr)=1  then _TypeStr :=_TmpStr
    else if Pos('Value=',_TmpStr)=1 then _ValueStr:=_TmpStr
    else if Pos('Descr=',_TmpStr)=1 then _DescrStr:=_TmpStr
    else if Pos('isVisible=',_TmpStr)=1 then _isVisibleStr :=_TmpStr
    else if Pos('isCrypted=',_TmpStr)=1 then _isCryptedStr :=_TmpStr
    else if Pos('isReadOnly',_TmpStr)=1 then _isReadOnlyStr:=_TmpStr;
  end;
  // set up the setting record
  SettingData.Name:=_NameStr;
  if _IDStr<>'' then GetToken(_IDStr,'=');
  GetToken(_TypeStr,'=');
  SettingData.SettingType:=StrtoType(_TypeStr);
  if  SettingData.SettingType=nvbEnum then FillEnumList(_TypeStr,SettingData.EnumList);
  GetToken(_ValueStr,'=');

  GetToken(_DescrStr,'=');
  SettingData.Descr := _DescrStr;
  GetToken(_isVisibleStr,'=');
  SettingData.isVisible:=false;
  if _isVisibleStr='' then _isVisibleStr:='0';
  if _isVisibleStr='1' then SettingData.isVisible:=true;

  GetToken(_isCryptedStr,'=');
  SettingData.isCrypted:=false;
  if _isCryptedStr='' then _isCryptedStr:='0';
  if _isCryptedStr='1' then SettingData.isCrypted:=true;

  GetToken(_isReadOnlyStr,'=');
  SettingData.isReadOnly:=false;
  if _isReadOnlyStr='' then _isReadOnlyStr:='0';
  if _isReadOnlyStr='1' then SettingData.isReadOnly:=true;
  SettingData.StrValue:= _ValueStr;
  PrepSetRec(SettingData);
end;

function CreateLine(const _SettingData: TNVBSettingData):string;
begin
  result:='';
  if not assigned(_SettingData) then exit;
  result := _SettingData.Name + ':';
  result := result + 'ID=0' + cDelimiterChar;
  result := result + 'Type=' + TypeToStr(_SettingData) + cDelimiterChar;
  result := result + 'Value=' + _SettingData.StrValue + cDelimiterChar;
  result := result + 'Descr=' + _SettingData.Descr + cDelimiterChar;

  if _SettingData.isVisible then result := result + 'isVisible=1' + cDelimiterChar
                            else result := result + 'isVisible=0' + cDelimiterChar;
  if _SettingData.isCrypted then result := result + 'isCrypted=1' +  cDelimiterChar
                            else result := result + 'isCrypted=0' +  cDelimiterChar;
  if _SettingData.isReadOnly then result := result + 'isReadOnly=1' +  cDelimiterChar
                             else result := result + 'isReadOnly=0' +  cDelimiterChar
end;
{$else}
{-----------------------------------------------------------------------------
  Procedure: ParseTokens
  Author:    herzogs2
  Date:
  Arguments: var SettingData: TNVBSettingData; LineStr: string
  Result:    None
  Description: 15.07.2002 use Pos instead of Copy for performance reason.
-----------------------------------------------------------------------------}
procedure ParseTokens(var SettingData: TNVBSettingData; LineStr: string;const _Cipher: TDCP_rc4);
var
  _NameStr: string;
  _IDStr: string;
  _TypeStr: string;
  _ValueStr: string;
  _DescrStr: string;
  _isVisibleStr: string;
  _isCryptedStr: string;
  _isReadOnlyStr: string;
  _TmpStr: string;
begin
  // initialize the temporary variables
  _NameStr:='';
  _IDStr:='';
  _TypeStr:='';
  _ValueStr:='';
  _DescrStr:='';
  _isVisibleStr:='';
  _isCryptedStr:='';
  _isReadOnlyStr:='';
  // parse the tokens
  _NameStr:=GetToken(LineStr,':');
  while LineStr<>'' do
  begin
    _TmpStr:=GetToken(LineStr,cDelimiterChar);
    if Pos('ID=',_TmpStr)=1 then _IDStr:=_TmpStr
    else if Pos('Type=',_TmpStr)=1  then _TypeStr :=_TmpStr
    else if Pos('Value=',_TmpStr)=1 then _ValueStr:=_TmpStr
    else if Pos('Descr=',_TmpStr)=1 then _DescrStr:=_TmpStr
    else if Pos('Flags=',_TmpStr)=1 then begin    // to be backward compatible with older ini-files.
      gIsVersion107orOlder:=true;
//      if _TmpStr='Flags=0' then _isVisibleStr :='isVisible=1';
      if _TmpStr='Flags=1' then _isCryptedStr :='isCrypted=1';
      _isVisibleStr :='isVisible=1';
      _isReadOnlyStr:='isReadOnly=0';
    end
    else if Pos('isVisible=',_TmpStr)=1 then _isVisibleStr :=_TmpStr
    else if Pos('isCrypted=',_TmpStr)=1 then _isCryptedStr :=_TmpStr
    else if Pos('isReadOnly',_TmpStr)=1 then _isReadOnlyStr:=_TmpStr
  end;
  // set up the setting record
  SettingData.Name:=_NameStr;
  SettingData.ID:=0;
  if _IDStr<>'' then
  begin
    GetToken(_IDStr,'=');
    try
      SettingData.ID:=StrToInt(_IDStr);
    except
      SettingData.ID:=0;
    end;
  end;
  GetToken(_TypeStr,'=');
  SettingData.SettingType:=StrtoType(_TypeStr);
  if  SettingData.SettingType=nvbEnum then FillEnumList(_TypeStr,SettingData.EnumList);
  GetToken(_ValueStr,'=');

  GetToken(_DescrStr,'=');
  StrLCopy(@SettingData.Descr,PChar(_DescrStr),cMaxDescr);
  GetToken(_isVisibleStr,'=');
  SettingData.isVisible:=false;
  if _isVisibleStr='' then _isVisibleStr:='0';
  if _isVisibleStr='1' then SettingData.isVisible:=true;

  GetToken(_isCryptedStr,'=');
  SettingData.isCrypted:=false;
  if _isCryptedStr='' then _isCryptedStr:='0';
  if _isCryptedStr='1' then SettingData.isCrypted:=true;

  GetToken(_isReadOnlyStr,'=');
  SettingData.isReadOnly:=false;
  if _isReadOnlyStr='' then _isReadOnlyStr:='0';
  if _isReadOnlyStr='1' then SettingData.isReadOnly:=true;
  if (not gIsVersion107orOlder) and
    SettingData.isCrypted and
    (_ValueStr<>'') then _ValueStr:=_Cipher.DecryptString(_ValueStr);

  SettingData.StrValue:= _ValueStr;

  PrepSetRec(SettingData);
end;

function CreateLine(const _SettingData: TNVBSettingData;const _Cipher: TDCP_rc4):string;
begin
  result:='';
  if not assigned(_SettingData) then exit;
  result := _SettingData.Name + ':';
  if _SettingData.ID <> 0 then result := result + 'ID=' + IntToStr(_SettingData.ID) + cDelimiterChar;
  result := result + 'Type=' + TypeToStr(_SettingData) + cDelimiterChar;

  if (not gIsVersion107orOlder) and
     (_SettingData.isCrypted)   and
     (_SettingData.StrValue<>'') then _SettingData.StrValue:=_Cipher.EncryptString(_SettingData.StrValue);

  result := result + 'Value=' + _SettingData.StrValue + cDelimiterChar;
  result := result + 'Descr=' + _SettingData.Descr + cDelimiterChar;
  if gIsVersion107orOlder then begin
    if _SettingData.isVisible then result := result + 'Flags=0'+ cDelimiterChar;
    if _SettingData.isCrypted then result := result + 'Flags=1'+ cDelimiterChar;
  end
  else begin
    if _SettingData.isVisible then result := result + 'isVisible=1' + cDelimiterChar
                              else result := result + 'isVisible=0' + cDelimiterChar;
    if _SettingData.isCrypted then result := result + 'isCrypted=1' +  cDelimiterChar
                              else result := result + 'isCrypted=0' +  cDelimiterChar;
    if _SettingData.isReadOnly then result := result + 'isReadOnly=1' +  cDelimiterChar
                              else result := result + 'isReadOnly=0' +  cDelimiterChar
  end;
end;
{$endif}


function GetToken(var Text: string;const _Delimiter: char): string;
var
_TmpPos: integer;
begin
  _TmpPos:=Pos(_Delimiter,Text);
  if _TmpPos>0 then
  begin
    result:=Copy(Text,1,_TmpPos-1);
    Delete(Text,1,_TmpPos);
  end
  else
  begin
    result:=Text;
    Text:='';
  end;
end;

//============================================================
// PROCEDURE PrepSetRec
// discription : convert into correct type
// programmed by :
// tested by :
// last changes on : 18.09.00
// 05.06.2003 -SH Bugfix: The decimal seperator must be set independent from the
// the system settings.
//============================================================
procedure PrepSetRec(var Data: TNVBSettingData);
var
_value:string;
begin
  case Data.SettingType of
    nvbInteger:
      begin
        try
          Data.IntValue:=StrToInt(Data.StrValue);
        except
          Data.IntValue:=0;
        end;
      end;
    nvbFloat:
      begin
        try
          _value:=Data.StrValue;
          _value:=VerifySeparator(_value);
          Data.FloatValue:=StrToFloat(_value);
        except
          Data.FloatValue:=0.0;
        end;
      end;
    nvbBool:
      begin
        if Data.StrValue='YES' then Data.BoolValue:=true
        else Data.BoolValue:=false;
      end;
    nvbColor:
      begin
        try
          Data.ColorValue:=StringToColor(Data.StrValue);
        except
          Data.ColorValue:=clNone;
        end;
      end;
    nvbEnum:
      begin
        try
          Data.IntValue:=strtoint(Data.StrValue);
        except
          Data.EnumList.Text:='';
        end;
      end;
    nvbChar:
      begin
        if Data.StrValue='' then Data.CharValue:=Chr(0)
        else Data.CharValue:=Data.StrValue[1];
      end;
  end;
end;

//============================================================
// PROCEDURE StrtoType
// discription :  converts a string into TNVBSettingType
// programmed by : SA
// tested by :
// last changes on : 18.09.00
//============================================================
function StrtoType(_TypeStr:string):TNVBSettingType;
begin
  result:=nvbNone;
  if      _TypeStr='String' then result:=nvbString
  else if _TypeStr='Integer' then result:=nvbInteger
  else if _TypeStr='Float' then result:=nvbFloat
  else if _TypeStr='Bool' then result:=nvbBool
  else if _TypeStr='Color' then result:=nvbColor
  else if Copy(_TypeStr,1,4)='Enum' then result:=nvbEnum
  else if Copy(_TypeStr,1,4)='Char' then result:=nvbChar
  else if Copy(_TypeStr,1,4)='File' then result:=nvbFile
  else if Copy(_TypeStr,1,4)='Path' then result:=nvbPath;
end;

//============================================================
// PROCEDURE TypeToStr
// discription :
// programmed by : GA
// tested by :
// last changes on : 18.09.00
//============================================================
function TypeToStr(SettingData: TNVBSettingData): string;
var
  i1: integer;
  TmpStr: string;
begin
  result:='';
  case SettingData.SettingType of
    nvbString:  result:='String';
    nvbInteger: result:='Integer';
    nvbFloat:   result:='Float';
    nvbBool:    result:='Bool';
    nvbColor:   result:='Color';
    nvbEnum:
      begin
        if assigned(SettingData.EnumList) and (SettingData.EnumList.Count>0) then
        begin
          TmpStr:='Enum(';
          for i1:=0 to SettingData.EnumList.Count-2 do
          begin
            TmpStr:=TmpStr+SettingData.EnumList[i1]+',';
          end;
          TmpStr:=TmpStr+SettingData.EnumList[SettingData.EnumList.Count-1]+')';
        end
        else
        begin
          TmpStr:='Enum()';
        end;
        result:=TmpStr;
      end;
    nvbChar:    TypeToStr:='Char';
    nvbFile:    TypeToStr:='File';
    nvbPath:    TypeToStr:='Path';
  end;
end;


//============================================================
// PROCEDURE BooleanToStr
// discription :
// programmed by : GA
// tested by :
// last changes on : 18.09.00
//============================================================
function BooleanToStr(Value: boolean): string;
begin
  if Value then BooleanToStr:='YES' else BooleanToStr:='NO';
end;

//============================================================
// PROCEDURE CreateEnumList
// discription :
// programmed by : GA
// tested by :
// last changes on : 18.09.00
//============================================================
procedure FillEnumList(_EnumStr: string;var EnumList:TStrings);
var
  _TmpPos: integer;
  _EnumItem: string;
begin
  EnumList.clear;
  _TmpPos:=Pos('(',_EnumStr);
  Delete(_EnumStr,1,_TmpPos);
  _TmpPos:=Pos(')',_EnumStr);
  Delete(_EnumStr,_TmpPos,Length(_EnumStr));
  while _EnumStr<>'' do begin
    _EnumItem:=GetToken(_EnumStr,',');
    EnumList.Add(_EnumItem);
  end;
end;


{ TNVBSettingData }

procedure TNVBSettingData.assign(_dataobject: TNVBSettingData);
begin
  if not assigned(_dataobject) then exit;
  Name       := _dataobject.Name;
  SettingType:= _dataobject.SettingType;
  StrValue   := _dataobject.StrValue;
  IntValue   := _dataobject.IntValue;
  FloatValue := _dataobject.FloatValue;
  BoolValue  := _dataobject.BoolValue;
  ColorValue := _dataobject.ColorValue;
  CharValue  := _dataobject.CharValue;
  isVisible  := _dataobject.isVisible;
  isCrypted  := _dataobject.isCrypted;
  isReadOnly := _dataobject.isReadOnly;
  Descr      := _dataobject.Descr;
  EnumList.Assign(_dataobject.EnumList);
end;

constructor TNVBSettingData.create;
begin
  EnumList:=TStringList.create;
end;

destructor TNVBSettingData.Destroy;
begin
  if assigned(EnumList) then begin
    EnumList.Clear;
    FreeAndNil(EnumList);
  end;
  inherited;
end;

end.
