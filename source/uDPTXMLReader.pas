{-----------------------------------------------------------------------------
 Unit Name: uDPTXMLReader
 Author:    herzogs2
 Date:      19-Aug-2008
 Purpose:   read from an xml file.
TODO: CLEANUP THIS MESS.
-----------------------------------------------------------------------------}
unit uDPTXMLReader;

interface
uses
  MSXML_TLB;

function ReadNodeText(const _filename:string;  // the name of the xml-file
                      _statement: string;      // the data select statement
                      var value:string;        // contains the value (if found)
                      var errormsg:string):boolean;overload; // contains an error message ( if not found)

function ReadNodeDocument(var xmlfile: IXMLDOMDocument;
                          _statement: string;
                          var value:string;
                          var errormsg:string): boolean;

implementation

uses
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

{-----------------------------------------------------------------------------
  Procedure: ReadNodeText
  Author:    muem
  Date:      10-Mar-2010
  Arguments: const _filename:string;_statement: string
  Result:    string
  Description: read data from an xml-file.
  Pass a statement in XPath notation e.g. //PropertyGroup/Config[@Condition="'$(Config)'==''"]
  to get the value.
-----------------------------------------------------------------------------}
function ReadNodeText(const _filename:string;_statement: string;var value:string;var errormsg:string): boolean;
var
  _xmlDOMfile: IXMLDOMDocument;
begin
  result := False;
  _xmlDOMfile := CoDOMDocument.Create;
  try
    if not _xmlDOMfile.load(_filename) then begin
      showmessagefmt('file <%s> not found.',[_filename]);
      exit;
    end;
    result := ReadNodeDocument(_xmlDOMfile, _statement, value, errormsg);
  finally
    _xmlDOMfile := nil;
  end;
end;

end.
