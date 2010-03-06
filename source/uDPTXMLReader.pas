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
  MSXML2_TLB,
  XMLIntf,
  XMLDoc;

function ReadNodeText(const _filename:string;  // the name of the xml-file
                      _statement: string;      // the data select statement
                      var value:string;        // contains the value (if found)
                      var errormsg:string;var lineNo:integer):boolean;overload; // contains an error message ( if not found)

function ReadNodeDocument(var xmlfile: IXMLDocument;  // an instance of an xml file.
                      _statement: string;      // the data select statement
                      var value:string;        // contains the value (if found)
                      var errormsg:string;var lineNo:integer):boolean;overload; // contains an error message ( if not found)


implementation

uses
  uDPTMisc,
  Dialogs,
  StrUtils,
  SysUtils;


{-----------------------------------------------------------------------------
  Procedure: ParseToken
  Author:    herzogs2
  Date:      19-Aug-2008
  Arguments: var _tokenname: string; var _tokenvalue: string; var _tokenindex:integer;var _statement: string
  Result:    None
  Description: example: (Test Name="Inspection").Steps.(Step Name="Init").Title

  23.02.2010: Changes to make Node-Names which contain a '.' char also work. E.g. NodeName is <Delphi.Personality> then
              the statement must look like this: (Delphi.Personality)  <- additional brakets around the node-name.
-----------------------------------------------------------------------------}
procedure ParseToken(var _tokenname: string; var _tokenvalue: string; var _tokenindex:integer;var _statement: string);

var
_pos: Integer;
_token: string;
_temp:string;
_StringStart:integer;
_StringEnd  :integer;
_BraketStart:integer;
_BraketEnd  :integer;
_EqualPos   :integer;
_DotPos     :integer;
begin
  _tokenindex:=-1;
  // get the token
  _BraketStart:=Pos('(',_statement);
  _BraketEnd  :=Pos(')',_statement);
  _DotPos     :=Pos('.',_statement);
  _EqualPos   :=Pos('=',_statement);
  if (_BraketStart>-1) and
     (_BraketEnd>_BraketStart) and
     (_DotPos>_BraketStart) then begin // yes, there are some brakets available
    if (_EqualPos>-1) then begin         // yes there is an equal sign.
      if  (_braketStart<_EqualPos) and   // is it inside the brakets ?
          (_EqualPos<_braketEnd) then begin
        _token:=GetField('=', _statement);
      end
      else begin
        _token:=GetField(')', _statement);
        delete(_statement,1,1);
      end;
    end
    else begin
      _token:=GetField(')', _statement);
      delete(_statement,1,1);
    end;
  end
  else _token := GetField('.', _statement);     // e.g. (Test Name="Inspection")
  // extract token name
  _StringStart:=pos('"',_token);           // check if a string is in the statement
  _StringEnd  :=PosEx('"',_token,_StringStart+1);
  _tokenname := GetField('=', _token);     // e.g. (Test Name
  _tokenname := RemoveChar('(',_tokenname);  // e.g. Test Name
  _pos := Pos(' ', _tokenname);
  if _pos > 0 then Delete(_tokenname, 1, _pos); // because a space is not allowed in a node name, this is removed e.g. Name
  if _StringStart>0 then _pos:=PosEx(')', _tokenname,_StringEnd)
                    else _pos := Pos(')', _tokenname);
  if _pos > 0 then begin
    _tokenname := copy(_tokenname,1,_pos-1);// incase we have a ')' at the end.
    delete(_token,1,_pos);
  end;
  if pos('[',_tokenname)>0 then begin      // e.g. Lines[3]
    _temp:=_tokenname;
    _tokenname :=GetField('[',_temp); // e.g. Lines
    _temp:=removechar('[',_temp);
    _temp:=removechar(']',_temp);
    StringToInteger(_temp,_tokenindex); // e.g 3
  end;
  _pos:=Pos(':',_tokenname);
  if _pos>0 then Delete(_tokenname,1,_pos); // remove namespace prefix if any
  _tokenname := trim(_tokenname);
  // extract token value
  _StringStart:=pos('"',_token);           // check if a string is in the statement
  _StringEnd  :=PosEx('"',_token,_StringStart+1);
  if _pos > 0 then Delete(_token, 1, _pos); // because a space is not allowed in a node name, this is removed e.g. Name
  if _StringStart>0 then _pos:=PosEx(')', _token,_StringEnd)
                    else _pos := Pos(')', _token);
  if _pos > 0 then _tokenvalue := copy(_token,1,_pos-1)// incase we have a ')' at the end.
              else _tokenvalue :='';
  _tokenvalue:=RemoveChar('"',_tokenvalue);  // e.g.  "Inspection"
end;


{-----------------------------------------------------------------------------
  Procedure: ReadNodeText
  Author:    herzogs2
  Date:      19-Aug-2008
  Arguments: const _filename:string;_statement: string
  Result:    string
  Description: read data from an xml-file.
  Pass a statement e.g. (Test Name="Inspection").Steps.(Step Name="Init").Title
  to get the value.
-----------------------------------------------------------------------------}
function ReadNodeText(const _filename:string;_statement: string;var value:string;var errormsg:string;var lineNo:integer): boolean;
var
_xmlfile: IXMLDocument;
_lineNo:integer;
begin
  result:=false;
  if not fileexists(_filename) then begin
    showmessagefmt('file <%s> not found.',[_filename]);
    exit;
  end;
  try
    _xmlfile:=newXMLDocument;
    _xmlfile.LoadFromFile(_filename);
    _xmlfile.Active := true;
    result:=ReadNodeDocument(_xmlfile,_statement,value,errormsg,_lineNo);
  finally
    _xmlfile.Active:=false;
    _xmlfile:=nil;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: ReadNodeDocumentOldStyle
  Author:    herzogs2
  Date:      21-Aug-2008
  Arguments: const _xmlfile: TXMLDocument;_statement: string;var value:string;var errormsg:string
  Result:    boolean
  Description: The same as above but, this time an instance of TXMLDocument is passed instead of
               the filename.
  TODO: change all statements of offical xpath and afterwards remove this method.
-----------------------------------------------------------------------------}
function ReadNodeDocumentOldStyle(var xmlfile: IXMLDocument;_statement: string;var value:string;var errormsg:string;var lineNo:integer): boolean;
var
i:integer;
_node:IXMLNode;
_tokenname:string;
_tokenvalue:string;
_tokenindex:integer;
_founditem:boolean;
_NodeName:string;
_pos:integer;
begin
  result:=false;
  value:='';
  errormsg:=_statement;
  if not assigned(xmlfile) then begin
    showmessagefmt('Input parameter is not correct.',[]);
    exit;
  end;
   if not xmlfile.Active then xmlfile.Active := true;
   _node:=xmlfile.DocumentElement;
   ParseToken(_tokenname, _tokenvalue,_tokenindex,_statement);
   try
     while (_tokenname<>'') and (_node<>nil) do begin
       _founditem:=false;
       for i := 0 to _node.ChildNodes.count - 1 do begin
         if _tokenvalue='' then begin  // find a child node by its name.
           if _tokenindex=-1 then begin
             _NodeName:=_node.ChildNodes[i].NodeName;
             _pos:=Pos(':',_NodeName);
             if _pos>0 then delete(_NodeName,1,_pos);
             if _NodeName=_tokenname then begin
               _node:=_node.ChildNodes[i];
               _founditem:=true;
               break;
             end;
           end
           else begin       // find a child node by its index.
             if (_tokenindex>-1) and (_tokenindex<_node.ChildNodes.count) then begin
               _node:=_node.ChildNodes[_tokenindex];
               _NodeName:=_node.NodeName;
               _pos:=Pos(':',_NodeName);
               if _pos>0 then delete(_NodeName,1,_pos);
               if _NodeName<>_tokenname then begin
                 _node:=nil;
                 _founditem:=true;
                 break;
               end;
             end
             else _node:=nil;
             _founditem:=true;
             break;
           end;
         end else
         begin     // otherwise find a child node by its attribute
           if _node.ChildNodes[i].AttributeNodes[_tokenname].Text=_tokenvalue then begin
             _node:=_node.ChildNodes[i];
             _founditem:=true;
             break;
           end;
         end;
       end;
       if _founditem then ParseToken(_tokenname, _tokenvalue,_tokenindex, _statement)
       else begin
         if _statement='' then _statement:=_tokenname;
         if pos('[',_statement)>0 then begin
           trace(5,'Problem: Could not find <%s:%s:%d>. Statement might be wrong',[_tokenname, _tokenvalue,_tokenindex]);
           errormsg:=format('Could not find item <%s:%s:%d> in statement <%s>.',[_tokenname, _tokenvalue,_tokenindex,errormsg]);
           _node:=nil;
           exit;
         end;
         if (_statement<>'') and (_node.HasAttribute(_statement)) then begin
           value:=_node.Attributes[_statement];
           errormsg:='';
           result:=true;
           exit;
         end
         else begin
        //   trace(trcTrace,'Problem: Could not find <%s:%s:%d>. Statement might be wrong',[_tokenname, _tokenvalue,_tokenindex]);
           errormsg:=format('Could not find item <%s:%s:%d> in statement <%s>.',[_tokenname, _tokenvalue,_tokenindex,errormsg]);
           _node:=nil;
         end;
       end;
     end;
   except
     on e:exception do trace(1,'Problem in ReadNodeText: Could not read <%s> from file <%s>. <%s>.',[_tokenname,xmlfile.filename,e.Message]);
   end;
   if assigned(_node) then begin
     if _node.IsTextElement then value:=_node.Text;
     errormsg:='';
     result:=true;
   end;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadNodeDocumentNewStyle
  Author:    sam
  Date:      06-Mrz-2010
  Arguments: var xmlfile: IXMLDocument;_statement: string;var value:string;var errormsg:string;var lineNo:integer
  Result:    boolean
  Description:
-----------------------------------------------------------------------------}
function ReadNodeDocumentNewStyle(var xmlfile: IXMLDocument;_statement: string;var value:string;var errormsg:string;var lineNo:integer): boolean;
var
_node:IXMLDOMNode;
begin
  result:=false;
  lineNo:=0;
  _node:=(xmlfile as  IXMLDOMDocument).selectSingleNode(_statement);
  if not assigned(_node) then begin
    errormsg:='Not found.';
    exit;
  end;
  value:=_node.text;
  result:=true;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadNodeDocument
  Author:    sam
  Date:      06-Mrz-2010
  Arguments: var xmlfile: IXMLDocument;_statement: string;var value:string;var errormsg:string;var lineNo:integer
  Result:    boolean
  Description: until we have clean-up the old-style node-selection, this supports both way's.
-----------------------------------------------------------------------------}
function ReadNodeDocument(var xmlfile: IXMLDocument;_statement: string;var value:string;var errormsg:string;var lineNo:integer): boolean;
begin
  if pos('//',_statement)=1 then result:=ReadNodeDocumentNewStyle(xmlfile,_statement,value,errormsg,lineNo)
                            else result:=ReadNodeDocumentOldStyle(xmlfile,_statement,value,errormsg,lineNo);
end;

end.
