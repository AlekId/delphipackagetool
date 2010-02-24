{*-----------------------------------------------------------------------------
 Unit Name: uDPTRelativePath
 Author:    yan lei. Copied from Delphi3000.
 Date:      14-Mrz-2008
 Purpose:
 History:
-----------------------------------------------------------------------------}
unit uDPTRelativePath;

interface

function GetabsolutePath(Source, Relative: string): string;

implementation
uses
  Classes,
  SysUtils;


function GetappearNum(sub, st: string): integer;
var
  i: integer;
  P: integer;
begin
  p := Pos(sub, st);
  I := 0;
  while p > 0 do
  begin
    inc(i);
    delete(st, 1, p + length(sub) - 1);
    p := Pos(sub, st);
  end;
  result := i;
end;

function decomposestr(sub, st: string; var tst: TStringList): Boolean;
var
  num: integer;
  P: integer;

begin
  p := Pos(sub, st);
  tst.Clear;
  while p > 0 do
  begin
    num := p + length(sub) - 1;
    tst.Add(copy(st, 1, num));
    delete(st, 1, num);
    p := Pos(sub, st);
  end;
  tst.Add(st);
  Result := True;

end;



function CopyLeftNum(sub, st: string; num: integer): string;
var
  Tst: TStringList;
  I: integer;
begin
  tst := TStringList.Create;
  try
    decomposestr(sub, st, Tst);
    if Num >= Tst.Count then
      Result := st
    else
    begin
      for i := 0 to num - 1 do
      begin
        Result := Result + Tst[i];
      end;
    end;
  finally
    Tst.Free;
  end;
end;


function CopyRightNum(sub, st: string; Num: integer): string;
var
  Tst: TStringList;
  I: integer;
begin
  Tst := TStringList.Create;
  try
    decomposestr(sub, st, Tst);
    Result := '';
    if Num < Tst.Count then
    begin
      for i := Tst.Count - Num to Tst.Count - 1 do
      begin
        Result := Result + Tst[i]
      end;
    end;
  finally
    Tst.Free;
  end;
end;


function GetabsolutePath(Source, Relative: string): string;
var
  i, Num, num1: integer;
  St: TStringList;
  s: string;
begin
  if (pos('..\',Relative)=0) and
     (pos('.\',Relative)=0) then begin    // its not a relative path, leave here.

    if (pos(':',Relative)=0) and
       (pos('\\',Relative)=0) then result:=Source+Relative
                               else result:=Relative;
    exit;
  end;

  Num := GetappearNum('..', Relative);
  St := TStringList.Create;
  try
    decomposestr('\', Source, st);
    Num1 := st.Count;

    Result := '';

    for i := 0 to num1 - num - 1 do
    begin
      Result := Result + st[i];
    end;
    s := CopyRightNum('..\', Relative, 1);
    Result := Result + s;
  finally
    st.Free;
  end;  
end;


end.

