unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button: TButton;
    edtStmt: TEdit;
    edtValue: TEdit;
    procedure ButtonClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses
  uXMLReader;

{$R *.dfm}

procedure TForm1.ButtonClick(Sender: TObject);
var
_value:string;
_msg:string;
_stmt:string;
begin
  _stmt:=edtStmt.text;

  ReadNodeText('Project1.bdsproj',  // the name of the xml-file
                      _stmt,      // the data select statement
                      _value,        // contains the value (if found)
                      _msg); // contains an error message ( if not found)

  MessageDlg(_value+'  '+_msg, mtInformation, [mbOK], 0);
  edtValue.Text:=_value;
end;

end.

