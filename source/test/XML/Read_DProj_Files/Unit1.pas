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
  StdCtrls,
  ExtCtrls;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    mmoFile: TMemo;
    Panel1: TPanel;
    Button: TButton;
    filename: TLabel;
    cbxfilename: TComboBox;
    lblstatement: TLabel;
    lbldata: TLabel;
    edtValue: TEdit;
    cbxstatement: TComboBox;
    Splitter1: TSplitter;
    mmoStatement: TMemo;
    procedure ButtonClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

uses
  uDPTXMLReader;

{$R *.dfm}

procedure TForm1.ButtonClick(Sender: TObject);
var
  _value: string;
  _msg: string;
  _stmt: string;
  _lineNo: integer;
begin
  _stmt := cbxstatement.text;
  if not fileexists(cbxfilename.text) then begin
    showmessage(format('The file <%s> does not exist. Please select an existing file.',[cbxfilename.text]));
    cbxfilename.SetFocus;
    exit;
  end;
  mmoFile.Lines.LoadFromFile(cbxfilename.text);
  ReadNodeText(cbxfilename.text, // the name of the xml-file
    _stmt, // the data select statement
    _value, // contains the value (if found)
    _msg,
    _lineNo); // contains an error message ( if not found)
  if _msg<>'' then MessageDlg(_value + '  ' + _msg, mtInformation, [mbOK], 0);
  edtValue.Text := _value;
end;

end.

