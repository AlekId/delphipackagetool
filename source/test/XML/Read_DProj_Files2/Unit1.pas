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
_XMLDoc:IXMLDOMDocument;
_node:IXMLDOMNode;
_nodeList:IXMLDOMNodeList;
begin

  case PageControl1.ActivePageIndex of
    3:_filename:=cbxFilename2010.Text;
  end;
  _XMLDoc := CoDOMDocument.Create;
  try
    _XMLDoc.load(_filename);
    _node:=_XMLDoc.selectSingleNode(edtStmt2010.text);
    if not assigned(_node) then begin
      showmessage('Not found.');
      exit;
    end;
    edtValue.Text:=_node.text;
  finally
    _XMLDoc := nil;
  end;


end;

end.

