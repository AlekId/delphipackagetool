{-----------------------------------------------------------------------------
 Unit Name: uDPTStringGridExt
 Author:    sam
 Date:      17-Mrz-2007
 Purpose:   functions to improve the stringgrid.
 History:
-----------------------------------------------------------------------------}
unit uDPTStringGridExt;

interface
uses Grids;

type
TNVBRowArray=array of integer;

function GetSelectedRows(const _StringGrid:TStringGrid):TNVBRowArray;

implementation

function IsCellSelected(const _StringGrid: TStringGrid; X, Y: Longint): Boolean;
begin
  Result := False;
  try
    if (X >= _StringGrid.Selection.Left) and (X <= _StringGrid.Selection.Right) and
       (Y >= _StringGrid.Selection.Top) and (Y <= _StringGrid.Selection.Bottom) then Result := True;
  except
  end;
end;

function GetSelectedRows(const _StringGrid:TStringGrid):TNVBRowArray;
var
i,j:integer;
begin
  j:=0;
  for i:=0 to _StringGrid.rowcount-1 do begin
    if not IsCellSelected(_StringGrid,1,i) then continue;
    SetLength(result,length(result)+1);
    result[j]:=i;
    inc(j);
  end;
end;
end.
