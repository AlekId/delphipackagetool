{-----------------------------------------------------------------------------
 Unit Name: uDPTXMLReader
 Author:    herzogs2
 Date:      19-Aug-2008
 Purpose:   read from an xml file.
-----------------------------------------------------------------------------}
unit uDPTXMLReader;

interface
uses
  Classes,
  MSXML_TLB;

function ReadNodeText(const _filename:string;            // the name of the xml-file
                      _statement: string;                // the data select statement
                      var value:string;                  // contains the value (if found)
                      var errormsg:string):boolean;      // contains an error message ( if not found)

function ReadNodesText(const _filename:string;           // the name of the xml-file
                      _statement: string;                // the data select statement
                      var value:TStringList;             // contains the value (if found)
                      var errormsg:string):boolean;      // contains an error message ( if not found)

function ReadNodeDocument(var xmlfile: IXMLDOMDocument;  // the xml DOM document
                          _statement: string;            // the data select statement
                          var value:string;              // contains the value (if found)
                          var errormsg:string): boolean; // contains an error message ( if not found)

function ReadNodesDocument(var xmlfile: IXMLDOMDocument; // the xml DOM document
                           _statement: string;           // the data select statement
                           var value:TStringList;        // contains the value (if found)
                           var errormsg:string): boolean;// contains an error message ( if not found)

implementation

uses
  SysUtils,
  Dialogs;


{*-----------------------------------------------------------------------------
  Procedure: ReadNodeDocument
  Author:    muem
  Date:      10-Mrz-2010
  Arguments: var xmlfile: IXMLDocument;_statement: string;var value:string;var errormsg:string
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function ReadNodeDocument(var xmlfile: IXMLDOMDocument;_statement: string;var value:string;var errormsg:string): boolean;
var
  _node:IXMLDOMNode;
begin
  result := false;
  _node := xmlfile.selectSingleNode(_statement);
  if not assigned(_node) then begin
    errormsg := 'Not found.';
    exit;
  end;
  value := _node.text;
  result := true;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadNodesDocument
  Author:    muem
  Date:      23-Oct-2012
  Arguments: var xmlfile: IXMLDocument;_statement: string;var value:TStringList;var errormsg:string
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function ReadNodesDocument(var xmlfile: IXMLDOMDocument;_statement: string;var value:TStringList;var errormsg:string): boolean;
var
  _nodes:IXMLDOMNodeList;
  i: Integer;
begin
  result := false;
  _nodes := xmlfile.selectNodes(_statement);
  if not assigned(_nodes) then begin
    errormsg := 'Not found.';
    exit;
  end;
  for i := 0 to _nodes.length - 1 do begin
    value.Add(_nodes[i].text);
  end;
  result := true;
end;

{-----------------------------------------------------------------------------
  Procedure: ReadNodeText
  Author:    muem
  Date:      10-Mar-2010
  Arguments: const _filename:string;_statement:string;var value:string;var errormsg:string
  Result:    boolean
  Description: read data from an xml-file.
  Pass a statement in XPath notation e.g. //PropertyGroup/Config[@Condition="'$(Config)'==''"]
  to get the value.
-----------------------------------------------------------------------------}
function ReadNodeText(const _filename:string;_statement: string;var value:string;var errormsg:string): boolean;
var
  _xmlDOMfile: IXMLDOMDocument;
begin
  result := False;
  if not fileexists(_filename) then begin
    showmessagefmt('ReadNodeText: File <%s> not found.',[_filename]);
    exit;
  end;
  _xmlDOMfile := CoDOMDocument.Create;
  try
    if not _xmlDOMfile.load(_filename) then begin
      showmessagefmt('ReadNodeText: Could not load file <%s>. Supposed to be an XML-File',[_filename]);
      exit;
    end;
    result := ReadNodeDocument(_xmlDOMfile, _statement, value, errormsg);
  finally
    _xmlDOMfile := nil;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: ReadNodesText
  Author:    muem
  Date:      24-Oct-2012
  Arguments: const _filename:string;_statement: string;var value:TStringList;var errormsg:string
  Result:    boolean
  Description: read data from an xml-file.
  Pass a statement in XPath notation e.g. //PropertyGroup/Config[@Condition="'$(Config)'==''"]
  to get the value.
-----------------------------------------------------------------------------}
function ReadNodesText(const _filename:string;_statement: string;var value:TStringList;var errormsg:string):boolean;
var
  _xmlDOMfile: IXMLDOMDocument;
begin
  result := False;
  if not fileexists(_filename) then begin
    showmessagefmt('ReadNodesText: File <%s> not found.',[_filename]);
    exit;
  end;
  _xmlDOMfile := CoDOMDocument.Create;
  try
    if not _xmlDOMfile.load(_filename) then begin
      showmessagefmt('ReadNodesText: Could not load file <%s>. Supposed to be an XML-File',[_filename]);
      exit;
    end;
    result := ReadNodesDocument(_xmlDOMfile, _statement, value, errormsg);
  finally
    _xmlDOMfile := nil;
  end;
end;

end.
