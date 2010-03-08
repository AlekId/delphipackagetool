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
  ExtCtrls,
  ComCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    tabD2006: TTabSheet;
    tabD2007: TTabSheet;
    tabD2009: TTabSheet;
    tabD2010: TTabSheet;
    Panel1: TPanel;
    mmoFile: TMemo;
    btnExecute: TButton;
    edtValue: TEdit;
    edtStmt2010: TEdit;
    cbxFilename2010: TComboBox;
    lblFilename2010: TLabel;
    lblStmt2010: TLabel;
    lblFilename2007: TLabel;
    cbxFilename2007: TComboBox;
    lblstmt2007: TLabel;
    edtStmt2007: TEdit;
    lblFilename2006: TLabel;
    cbxFilename2006: TComboBox;
    lblStmt2006: TLabel;
    edtStmt2006: TEdit;
    lblFilename2009: TLabel;
    cbxFilename2009: TComboBox;
    lblStmt2009: TLabel;
    edtStmt2009: TEdit;
    procedure btnExecuteClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

uses
  MSXML2_TLB;

{$R *.dfm}





procedure TForm1.btnExecuteClick(Sender: TObject);
var
_filename:string;
_stmt:string;
_XMLDoc:IXMLDOMDocument;
_node:IXMLDOMNode;
//_nodeList:IXMLDOMNodeList;
begin
  case PageControl1.ActivePageIndex of
    0:begin
        _filename:=cbxFilename2006.Text;
        _stmt    :=edtStmt2006.text;
      end;
    1:begin
        _filename:=cbxFilename2007.Text;
        _stmt    :=edtStmt2007.text;
      end;
    2:begin
        _filename:=cbxFilename2009.Text;
        _stmt    :=edtStmt2009.text;
      end;
    3:begin
        _filename:=cbxFilename2010.Text;
        _stmt    :=edtStmt2010.text;
      end;
  end;
  if not fileexists(_filename) then begin
    showmessagefmt('The file <%s> does not exists.',[_filename]);
    exit;
  end;
  _XMLDoc := CoDOMDocument.Create;
  try
    _XMLDoc.load(_filename);
    _node:=_XMLDoc.selectSingleNode(_stmt);
    if not assigned(_node) then begin
      edtValue.Text:='';
      showmessage('Not found.');
      exit;
    end;
    edtValue.Text:=_node.text;
  finally
    _XMLDoc := nil;
  end;
end;

end.

