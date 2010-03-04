{*-----------------------------------------------------------------------------
 Unit Name: uDPTDefinitions
 Author:    Muem
 Date:      02-Mar-2010
 Purpose:   contains some Jcl (Jedi Code Library) functions. Thanks!
 History:
-----------------------------------------------------------------------------}

unit uDPTJclFuncs;

interface

type
  WideStringArray = array of WideString;

  TLoadResRec = record
    EnglishStr: WideStringArray;
    ResId: array of Integer;
  end;
  PLoadResRec = ^TLoadResRec;

function LoadResStrings(const BaseBinName: string;
                        const ResEn: array of WideString): WideStringArray;

implementation

uses
  Windows,
  SysUtils;

const
  BinaryExtensionPackage       = '.bpl';

function StrLICompW(const _s1,_s2: PWideChar; _len: integer): Integer;
var
i: integer;
_ps1, _ps2: PWideChar;
_c1, _c2: Word;
_ch1,_ch2: WideChar;
begin
  result := 0;
  _ps1 := _s1;
  _ps2 := _s2;
  for i:=0 to _len-1 do begin
    _ch1 := _ps1^;
    _ch2 := _ps2^;

    case _ch1 of
      'a'..'z': begin
        _c1 := Word(_ch1);
        _ch1 := WideChar(_c1 xor $0020);
      end;
    end;

    case _ch2 of
      'a'..'z': begin
        _c2 := Word(_ch2);
        _ch2 := WideChar(_c2 xor $0020);
      end;
    end;

    if (_ch1 <> _ch2) or (_ch1 = #0) then begin
      Result := Ord(_ch1) - Ord(_ch2);
      exit;
    end;
    Inc(_ps1);
    Inc(_ps2);
  end;

end;


// helper function to find strings in current string table
function LoadResCallBack(hModule: HMODULE; lpszType, lpszName: PChar;
  lParam: PLoadResRec): BOOL; stdcall;
var
  ResInfo, ResHData, ResSize, ResIndex: Cardinal;
  ResData: PWord;
  StrLength: Word;
  StrIndex, ResOffset, MatchCount, MatchLen: Integer;
begin
  Result := True;
  MatchCount := 0;

  ResInfo := FindResource(hModule, lpszName, lpszType);
  if ResInfo = 0 then exit;

  ResHData := LoadResource(hModule, ResInfo);
  if ResHData = 0 then exit;

  ResData := LockResource(ResHData);
  if not Assigned(ResData) then exit;

  ResSize := SizeofResource(hModule, ResInfo) div 2;
  ResIndex := 0;
  ResOffset := 0;
  while ResIndex < ResSize do begin
    StrLength := ResData^;
    Inc(ResData);
    Inc(ResIndex);
    // for each requested strings
    for StrIndex := Low(lParam^.EnglishStr) to High(lParam^.EnglishStr) do begin
      MatchLen := Length(lParam^.EnglishStr[StrIndex]);
      if (lParam^.ResId[StrIndex] = 0) and (StrLength = MatchLen)
         and (StrLICompW(PWideChar(lParam^.EnglishStr[StrIndex]), PWideChar(ResData), MatchLen) = 0) then begin
        // http://support.microsoft.com/kb/q196774/
        lParam^.ResId[StrIndex] := (PWord(@lpszName)^ - 1) * 16 + ResOffset;
        Inc(MatchCount);
        if MatchCount = Length(lParam^.EnglishStr) then begin
          Result := False;
          Break; // all requests were translated to ResId
        end;
      end;
    end;
    Inc(ResOffset);
    Inc(ResData, StrLength);
    Inc(ResIndex, StrLength);
  end;
end;

function LoadResStrings(const BaseBinName: string;
                        const ResEn: array of WideString): WideStringArray;
var
  H: HMODULE;
  LocaleName: array [0..4] of Char;
  FileName: string;
  Index, NbRes: Integer;
  LoadResRec: TLoadResRec;
begin
  NbRes := Length(ResEn);
  SetLength(LoadResRec.EnglishStr, NbRes);
  SetLength(LoadResRec.ResId, NbRes);
  SetLength(Result, NbRes);

  for Index := Low(ResEn) to High(ResEn) do begin
    LoadResRec.EnglishStr[Index] := ResEn[Index];
  end;

  H := LoadLibraryEx(PChar(ChangeFileExt(BaseBinName, BinaryExtensionPackage)), 0,
    LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
  if H <> 0 then begin
    try
      EnumResourceNames(H, RT_STRING, @LoadResCallBack, LPARAM(@LoadResRec));
    finally
      FreeLibrary(H);
    end;
  end;

  FileName := '';

  ZeroMemory(@LocaleName, SizeOf(LocaleName));
  GetLocaleInfo(GetThreadLocale, LOCALE_SABBREVLANGNAME, LocaleName, SizeOf(LocaleName));
  if LocaleName[0] <> #0 then begin
    FileName := BaseBinName;
    if FileExists(FileName + LocaleName) then begin
      FileName := FileName + LocaleName;
    end
    else begin
      LocaleName[2] := #0;
      if FileExists(FileName + LocaleName) then begin
        FileName := FileName + LocaleName;
      end
      else begin
        FileName := '';
      end;
    end;
  end;

  if FileName = '' then begin
    Result := LoadResRec.EnglishStr;
    exit;
  end;

  H := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
  if H = 0 then exit;

  try
    for Index := 0 to NbRes - 1 do begin
      SetLength(Result[Index], 1024);
      SetLength(Result[Index],
        LoadStringW(H, LoadResRec.ResId[Index], PWideChar(Result[Index]), Length(Result[Index]) - 1));
    end;
  finally
    FreeLibrary(H);
  end;
end;

end.
