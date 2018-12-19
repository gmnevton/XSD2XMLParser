{
  XSD to XML Parser v.15.0

  Reads XSD schema and outputs XML structure described by schema,
    contains only default or fixed values for nodes and attributes.

  Author:
    (C) 2015-2018, Grzegorz Molenda; gmnevton@o2.pl

  Documentation:
    http://www.w3schools.com/schema/default.asp
    http://www.w3.org/TR/xmlschema11-1/
    http://www.w3.org/XML/Schema --> generateDS.py -- generate bindings from XML Schema D. Kuhlman 2010-01-05

  Version history:
    v.1  - 2015.02.6  - GM - beta version
    v.2  - 2015.04.10 - GM - fix ParseComplexType, ParseRestriction, ParseExtension, ParseTypeReference, ParseReference
    v.3  - 2015.09.2  - GM - added TNodeInfo.CheckIfNodeHaveParent
    v.4  - 2015.12.21 - GM - fix MakeXSDImport, fixed path determination for external imports, if schemaLocation had no path, get it from namespace
    v.5  - 2016.01.12 - GM - added handling of compressed stream for HTTP protocol
    v.6  - 2016.06.2  - GM - prepare for JPK (eng: SAFT)
    v.7  - 2016.06.27 - GM - more JPK changes, TXSD2XMLParser.IsBuiltinAttr, TXSD2XMLParser.Add
    v.8  - 2016.09.8  - GM - more JPK changes, TNodeInfo.GetMappingInfo
    v.9  - 2016.10.13 - GM - changed FindNodeRecursive to FindNode with parameter set to search recursively, due to Xml.VerySimple changes
    v.10 - 2016.10.24 - GM - more changes to FindNode in Xml.VerySimple
    v.11 - 2016.11.7  - GM - more changes to FindNode in Xml.VerySimple
    v.12 - 2017.09.22 - GM - changes to Xml.VerySimple, ScanNodes procedure is now a function, that returns False for break in loop
    v.13 - 2017.10.18 - GM - added extended attribute 'default' to TXSD2XMLParser.IsBuiltinAttr
    v.14 - 2018.11.23 - GM - added detection for > Circular Type Reference < to TXSD2XMLParser.ParseTypeReference, ParseSimpleType, ParseComplexType
    v.15 - 2018.12.19 - GM - fix, skip comments from xml schema
}

unit uXSD2XMLParser;

interface

uses
  Classes,
  SysUtils,
  Generics.Defaults,
  Generics.Collections,
  Xml.VerySimple,
  uGMUtils;

type
  ENodeException = class(Exception);

  TXSDOptions = set of (xsdNodeAutoIndent, xsdCompact, xsdParseProcessingInstr, xsdParseImportInstr, xsdPreserveWhiteSpace,
                        xsdCaseInsensitive, xsdSearchAutoIncludeNamespacePrefix, xsdWriteBOM);

  TXSDElementType = (etUnknown,
                     etToken, // remove line feeds, carriage returns, tabs, leading and trailing spaces, and multiple spaces
                     etString,
                     etFloat,
                     etDecimal, // decimal value xxx.yy or xxx.yyyy
                     etInteger, // 32-bit integer value
                     etByte, // signed 8-bit integer
                     etInt, // signed 32-bit integer
                     etLong, // signed 64-bit integer
                     etNegativeInteger, // integer containing only negative values (..,-2,-1)
                     etNonNegativeInteger, // integer containing only non-negative values (0,1,2,..)
                     etNonPositiveInteger, // integer containing only non-positive values (..,-2,-1,0)
                     etPositiveInteger, // integer containing only positive values (1,2,..)
                     etShort, // signed 16-bit integer
                     etUnsignedLong, // unsigned 64-bit integer
                     etUnsignedInt, // unsigned 32-bit integer
                     etUnsignedShort, // unsigned 16-bit integer
                     etUnsignedByte, // unsigned 8-bit integer
                     etBoolean,
                     etDate,
                     etTime,
                     etDateTime,
                     etgDay, // defines a part of a date - the day (DD)
                     etgMonth, // defines a part of a date - the month (MM)
                     etgMonthDay, // defines a part of a date - the month and day (MM-DD)
                     etgYear, // defines a part of a date - the year (YYYY)
                     etgYearMonth, // defines a part of a date - the year and month (YYYY-MM)
                     etNormalizedString, // remove line feeds, carriage returns, and tab characters
                     etDuration,
                     etBase64Binary, // base64-encoded binary data
                     etHexBinary, // hexadecimal-encoded binary data
                     etAnyURI, // specify a URI
                     etENTITY,
                     etENTITIES,
                     etID, // string that represents the ID attribute in XML (only used with schema attributes)
                     etIDREF, // string that represents the IDREF attribute in XML (only used with schema attributes)
                     etIDREFS,
                     etLanguage, // string that contains a valid language id
                     etName, // string that contains a valid XML name
                     etNCName,
                     etNMTOKEN, // string that represents the NMTOKEN attribute in XML (only used with schema attributes)
                     etNMTOKENS,
                     etQName,
                     etNOTATION
                    );

  TXSDSimpleType = (stUnknown, stRestriction, stList, stUnion);
  TXSDComplexType = (ctUnknown, ctAll, ctChoice, ctSequence, ctSimpleContent, ctComplexContent, ctElement);

  TNodeInfo = class
  public
    class function GetNodeInfo(const Node: TXmlNode): String;
    class function GetMappingInfo(const Node: TXmlNode): String;
    class function CheckIfNodeHaveParent(const Node: TXmlNode; ParentName: String): Boolean;
  end;

  TXSD2XMLParser = class;
  TXSDImportList = class;

  TXSDImport = class(TObject)
  private
    FImport: TXmlVerySimple;
    FXSDImports: TXSDImportList;

    procedure ParseImport;
  public
    ID: String;
    Namespace: String;
    SchemaLocation: String;

    constructor Create; virtual;
    destructor Destroy; override;

    function Add(const Value: String): TXSDImport;

    property Import: TXmlVerySimple read FImport;
    property XSDImports: TXSDImportList read FXSDImports;
  end;

  TXSDImportList = class(TObjectList<TXSDImport>)
  public
    [Weak] Parent: TXSD2XMLParser;
    function Add(const ANamespace: String): TXSDImport; overload; virtual;
    function Add(const ANamespace, ASchemaLocation: String): TXSDImport; overload; virtual;
    function Find(const ANamespace: String): TXSDImport; virtual;
    procedure Delete(const ANamespace: String); overload; virtual;
    function HasImport(const ANamespace: String): Boolean; virtual;
  end;

  TXSDImportsSearchRootList = class(TObjectList<TXmlNode>);

  TXSD2XMLParser = class(TObject)
  private
    FXSD: TXmlVerySimple;
    FXSDImports: TXSDImportList;
    FXML: TXmlVerySimple;
    FLastXSDImportsSearchRoot: TXSDImportsSearchRootList;
    FSchemaPrefix: String;
    FSchemaNamespace: String;
    FSchemaTargetNamespace: String;
    FIncludePrefix: Boolean;
    FIncludePrefixString: TStringArray;
  protected
    function GetXMLString: String;
    function IsSame(const Value1, Value2: String): Boolean;
    function GetPrefix: String;

    function HasReference(const Node: TXmlNode): Boolean; virtual;

    function IsReferenced(const Node: TXmlNode; var ReferenceName: String): Boolean; virtual;
    function IsSimpleType(const Node: TXmlNode): Boolean; virtual;
    function IsComplexType(const Node: TXmlNode): Boolean; virtual;
    function IsTypeDeclared(const Node: TXmlNode; var TypeDeclared: String): Boolean; virtual;
    function IsAbstract(const Node: TXmlNode): Boolean; virtual;
    function IsOptional(const Node: TXmlNode): Boolean; virtual;
    function IsDefault(const Node: TXmlNode; var DefaultValue: String): Boolean; virtual;
    function IsFixed(const Node: TXmlNode; var FixedValue: String): Boolean; virtual;
    function IsSimpleContent(const Node: TXmlNode): Boolean; virtual; // text only child elements
    function IsMixed(const Node: TXmlNode): Boolean; virtual; // text and child elements inside
    function IsBuiltinSkipableElement(const Node: TXmlNode): Boolean; virtual;
    class function IsBuiltinAttr(const AttributeName: String; const IncludeExtended: Boolean = False): Boolean; virtual;
    function IsPascalType(const TypeName: String): Boolean;

    function GetXSDBuiltinType(const ElementTypeName: String): TXSDElementType;
    function GetSimpleType(var Node: TXmlNode): TXSDSimpleType; virtual;
    function GetComplexType(var Node: TXmlNode): TXSDComplexType; virtual;
    function GetPascalType(const TypeName: String): String; virtual;

    function Add(const Node, Parent: TXmlNode; const Name: String = ''; const CheckOptional: Boolean = True; const SetOptional: Boolean = False): TXmlNode;
  public
    Options: TXSDOptions;

    constructor Create; virtual;
    destructor Destroy; override;

    class function LoadFromFile(const FileName: String; const BufferSize: Integer = 4096): TXSD2XMLParser; virtual;

    procedure Clear; virtual;
    procedure Parse; virtual;
    procedure ParseSchemaAttributes(const Node: TXmlNode);
    procedure ParseImport(const Node: TXmlNode; var Parent: TXmlNode);
    procedure ParseElement(const Node: TXmlNode; var Parent: TXmlNode; const Recursive: Boolean = False; const Optional: Boolean = False);
    procedure ParseSimpleType(const Node: TXmlNode; var Parent: TXmlNode);
    procedure ParseComplexType(const Node: TXmlNode; var Parent: TXmlNode);
    procedure ParseRestriction(const Node: TXmlNode; var Parent: TXmlNode);
    procedure ParseExtension(const Node: TXmlNode; var Parent: TXmlNode);
    procedure ParseTypeReference(const Node: TXmlNode; var Parent: TXmlNode; const ReferenceName: String);
    procedure ParseReference(const Node: TXmlNode; var Parent: TXmlNode; const ReferenceName: String);
    procedure ParseValue(const Node: TXmlNode; var Parent: TXmlNode);
    procedure ParseAttribute(const Node: TXmlNode; var Parent: TXmlNode);
    procedure ParseEnumeration(const Node: TXmlNode; var Parent: TXmlNode);
    procedure ParseList(const Node: TXmlNode; var Parent: TXmlNode);
    procedure ParseUnion(const Node: TXmlNode; var Parent: TXmlNode);
    procedure ParseChoice(const Node: TXmlNode; var Parent: TXmlNode);
    procedure ParseGroup(const Node: TXmlNode; var Parent: TXmlNode);
    procedure ParseSequence(const Node: TXmlNode; var Parent: TXmlNode);
    procedure ParseBuiltinAttr(const Node: TXmlNode; var Parent: TXmlNode);

    property XSD: TXmlVerySimple read FXSD;
    property XML: TXmlVerySimple read FXML;
    property XMLString: String read GetXMLString;
  end;

implementation

uses
  StrUtils,
  IdHTTP,
  IdCompressorZLib;

const
  TXSDElementTypeString: Array[TXSDElementType] of String = ('--unknown--',
                                                             'token',
                                                             'string',
                                                             'float',
                                                             'decimal',
                                                             'integer',
                                                             'byte',
                                                             'int',
                                                             'long',
                                                             'negativeinteger',
                                                             'nonnegativeinteger',
                                                             'nonpositiveinteger',
                                                             'positiveinteger',
                                                             'short',
                                                             'unsignedlong',
                                                             'unsignedint',
                                                             'unsignedshort',
                                                             'unsignedbyte',
                                                             'boolean',
                                                             'date',
                                                             'time',
                                                             'datetime',
                                                             'gday',
                                                             'gmonth',
                                                             'gmonthday',
                                                             'gyear',
                                                             'gyearmonth',
                                                             'normalizedstring',
                                                             'duration',
                                                             'base64binary',
                                                             'hexbinary',
                                                             'anyuri',
                                                             'entity',
                                                             'entities',
                                                             'id',
                                                             'idref',
                                                             'idrefs',
                                                             'language',
                                                             'name',
                                                             'ncname',
                                                             'nmtoken',
                                                             'nmtokens',
                                                             'qname',
                                                             'notation'
                                                            );

type
  TXmlVerySimpleAcces = class(TXmlVerySimple);

function ExtractURLPath(const AUrl: String): String;
var
  I: Integer;
begin
  I := LastDelimiter('/', AUrl);
  Result := Copy(AUrl, 1, I);
end;

function ExtractURLFileName(const AUrl: String): String;
var
  I: Integer;
begin
  I := LastDelimiter('/', AUrl);
  Result := Copy(AUrl, I + 1, Length(AUrl) - I);
end;

function LoadXSDImport(const URI: String): String;
var
  zlib: TIdCompressorZLib;
  http: TIdHTTP;
  reader: TStreamReader;
begin
  try
    if StartsStr('http', URI) then begin
      zlib:=TIdCompressorZLib.Create(Nil);
      http:=TIdHTTP.Create(Nil);
      try
        http.HandleRedirects:=True;
        http.Compressor:=zlib;
        Result:=http.Get(URI);
      finally
        http.Compressor:=Nil;
        http.Free;
        zlib.Free;
      end;
    end
    else if (URI <> '') and FileExists(URI) then begin
      reader:=TStreamReader.Create(URI, TEncoding.UTF8, True);
      try
        Result:=reader.ReadToEnd;
      finally
        reader.Free;
      end;
    end;
  except
    Result:='';
  end;
end;

function MakeXSDImport(const Node: TXmlNode): TXSDImport; overload;
var
  Import: TXSDImport;
  path: String;
  xsd: WideString;
begin
  Import:=TXSDImport.Create;
  if Node.HasAttribute('id') then
    Import.ID:=Node.Attributes['id'];
  if Node.HasAttribute('namespace') then
    Import.Namespace:=Node.Attributes['namespace'];
  if Node.HasAttribute('schemaLocation') then
    Import.SchemaLocation:=Node.Attributes['schemaLocation'];

  try
    path:=Import.SchemaLocation;
    if ExtractURLFullPath(path) = '' then
      path:=ExtractURLFullPath(Import.Namespace) + path;

    xsd:=LoadXSDImport(path);
    if xsd <> '' then
      Import.Add(xsd)
    else
      FreeAndNil(Import);
  except
    Import.Free;
    raise;
  end;

  Result:=Import;
end;

{ TNodeInfo }

class function TNodeInfo.GetNodeInfo(const Node: TXmlNode): String;

  function NodeTypeToString(const ANodeType: TXmlNodeType): String;
  begin
    case ANodeType of
      ntElement        : Result:='Element';
      ntText           : Result:='Text';
      ntCData          : Result:='CData';
      ntProcessingInstr: Result:='Processing instruction';
      ntComment        : Result:='Comment';
      ntDocument       : Result:='Document';
      ntDocType        : Result:='Document type declaration';
      ntXmlDecl        : Result:='XML declaration';
    else
      Result:='';
    end;
  end;

var
  Child: TXmlNode;
begin
  Result:='';
  if Node <> Nil then begin
    Child:=Node.ParentNode;
    while (Child <> Nil) and (Child <> TXmlVerySimpleAcces(Child.Document).Root) do begin
      Result:=Child.Name + ifString(Child.HasAttribute('name'), '[' + Child.Attributes['name'] + ']') + '.' + Result;
      Child:=Child.ParentNode;
    end;
    Result:='"' + Result + Node.Name + '(' + Node.AttributeList.AsString + ')"';
    Result:=Result + #13#10 + Format('Level: %d, Index: %d, Type: %s', [Node.Level, Node.Index, NodeTypeToString(Node.NodeType)]);
  end;
end;

class function TNodeInfo.GetMappingInfo(const Node: TXmlNode): String;
var
  Child: TXmlNode;
  hasMaxOccurs: Boolean;
  maxOccurs: String;
begin
  Result:='';
  if Node <> Nil then begin
    Child:=Node.ParentNode;
    while (Child <> Nil) and (Child <> TXmlVerySimpleAcces(Child.Document).Root) do begin
      if (LowerCase(Child.Name) <> 'choice') and (LowerCase(Child.Name) <> 'optional') then begin
        hasMaxOccurs:=Child.HasAttribute('maxOccurs');
        if not hasMaxOccurs then
          Result:=Child.NameWithPrefix + '.' + Result
        else begin
          maxOccurs:=LowerCase(AnsiDequotedStr(Child.Attributes['maxOccurs'], '"'));
          Result:=IfThen((maxOccurs = 'unbounded') or (StrToIntDef(maxOccurs, 1) > 1), '#' + Child.NameWithPrefix, Child.NameWithPrefix) + '.' + Result;
        end;
      end;
      Child:=Child.ParentNode;
    end;
    Result:='.' + Result;
    if (LowerCase(Node.Name) <> 'choice') and (LowerCase(Node.Name) <> 'optional') then begin
      hasMaxOccurs:=Node.HasAttribute('maxOccurs');
      if not hasMaxOccurs then
        Result:=Result + Node.NameWithPrefix
      else begin
        maxOccurs:=LowerCase(AnsiDequotedStr(Node.Attributes['maxOccurs'], '"'));
        Result:=Result + IfThen((maxOccurs = 'unbounded') or (StrToIntDef(maxOccurs, 1) > 1), '#' + Node.NameWithPrefix, Node.NameWithPrefix);
      end;
    end;
  end;
end;

class function TNodeInfo.CheckIfNodeHaveParent(const Node: TXmlNode; ParentName: String): Boolean;
var
  Child: TXmlNode;
begin
  Result:=False;
  ParentName:=LowerCase(ParentName);
  if (Node <> Nil) and (ParentName <> '') then begin
    Child:=Node.ParentNode;
    while (Child <> Nil) and (Child <> TXmlVerySimpleAcces(Child.Document).Root) do begin
      Result:=(LowerCase(Child.Name) = ParentName);
      if Result then
        Break;
      Child:=Child.ParentNode;
    end;
  end;
end;

{ TXSDImport }

constructor TXSDImport.Create;
begin
  FImport := Nil;
  ID := '';
  Namespace := '';
  SchemaLocation := '';

  FXSDImports:=TXSDImportList.Create;
end;

destructor TXSDImport.Destroy;
begin
  if Assigned(FImport) then
    FImport.Free;

  FXSDImports.Free;
  inherited;
end;

procedure TXSDImport.ParseImport;
begin
  FImport.DocumentElement.ScanNodes('import',
    function (Node: TXmlNode): Boolean // import includes
    var
      Import: TXSDImport;
    begin
      Result:=True;
      Import:=MakeXSDImport(Node);
      if Import <> Nil then
        FXSDImports.Add(Import);
    end, True);
  FImport.DocumentElement.ScanNodes('include',
    function (Node: TXmlNode): Boolean // import includes
    var
      schema_Location, uri_path: String;
      xsd_include: TXmlVerySimple;
    begin
      Result:=True;
      if Node.HasAttribute('schemaLocation') then begin
        schema_Location:=Node.Attributes['schemaLocation'];
        uri_path:=ExtractURLPath(schema_Location);
        if uri_path = '' then
          uri_path:=ExtractURLPath(SchemaLocation);
        xsd_include:=TXmlVerySimple.Create;
        try
          xsd_include.Xml:=LoadXSDImport(uri_path + ExtractURLFileName(schema_Location));
          FImport.DocumentElement.AddNodes(xsd_include.DocumentElement);
        finally
          xsd_include.Free;
        end;
      end;
    end, True);
end;

function TXSDImport.Add(const Value: String): TXSDImport;
begin
  if not Assigned(FImport) then
    FImport:=TXmlVerySimple.Create;

  FImport.Xml := Value;

  ParseImport;

  Result:=Self;
end;

{ TXSDImportList }

function TXSDImportList.Add(const ANamespace: String): TXSDImport;
begin
  Result := TXSDImport.Create;
  Result.Namespace := ANamespace;
  try
    Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TXSDImportList.Add(const ANamespace, ASchemaLocation: String): TXSDImport;
begin
  Result := TXSDImport.Create;
  Result.Namespace := ANamespace;
  Result.SchemaLocation := ASchemaLocation;
  try
    Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TXSDImportList.Delete(const ANamespace: String);
var
  Import: TXSDImport;
begin
  Import := Find(ANamespace);
  if Assigned(Import) then
    Remove(Import);
end;

function TXSDImportList.Find(const ANamespace: String): TXSDImport;
var
  Import, found: TXSDImport;
begin
  Result := Nil;
  for Import in Self do
    if ((Assigned(Parent) and Parent.IsSame(Import.Namespace, ANamespace)) or // use the documents text comparison
       ((not Assigned(Parent)) and (Import.Namespace = ANamespace))) then begin // or if not assigned then compare names case sensitive
      Result := Import;
      Break;
    end;
  if Result <> Nil then
    Exit;
  // search recursive 
  for Import in Self do
    if Import.XSDImports.Count > 0 then begin
      found:=Import.XSDImports.Find(ANamespace);
      if found <> Nil then begin
        Result := found;
        Break;
      end;  
    end;
end;

function TXSDImportList.HasImport(const ANamespace: String): Boolean;
begin
  Result := Assigned(Find(ANamespace));
end;

{ TXSD2XMLParser }

constructor TXSD2XMLParser.Create;
begin
  Options := [xsdNodeAutoIndent, xsdParseImportInstr, xsdSearchAutoIncludeNamespacePrefix];
  FXSD:=TXmlVerySimple.Create;
  FXSDImports:=TXSDImportList.Create;
  FXML:=TXmlVerySimple.Create;
  FXML.Options:=[doParseProcessingInstr];
  FXML.LineBreak:='';
  FXML.NodeIndentStr:='';
  FLastXSDImportsSearchRoot:=Nil;
  FSchemaPrefix:='';
  FSchemaNamespace:='';
  FSchemaTargetNamespace:='';
  FIncludePrefix:=False;
  SetLength(FIncludePrefixString, 0);
  array_insert('', FIncludePrefixString);
end;

destructor TXSD2XMLParser.Destroy;
begin
  if Assigned(FXSD) then
    FXSD.Free;
  FXSDImports.Free;
  FXML.Free;
  if FLastXSDImportsSearchRoot <> Nil then
    FreeAndNil(FLastXSDImportsSearchRoot);
  SetLength(FIncludePrefixString, 0);
  inherited;
end;

function TXSD2XMLParser.GetXMLString: String;
begin
  Result := FXML.Xml;
end;

function TXSD2XMLParser.IsSame(const Value1, Value2: String): Boolean;
begin
  if xsdCaseInsensitive in Options then
    Result := (CompareText(Value1, Value2) = 0)
  else
    Result := (Value1 = Value2);
end;

function TXSD2XMLParser.GetPrefix: String;
begin
  Result:='';
  if FSchemaPrefix <> '' then
    Result:=ifString(xsdSearchAutoIncludeNamespacePrefix in Options, FSchemaPrefix + ':');
end;

function TXSD2XMLParser.HasReference(const Node: TXmlNode): Boolean;
var
  refName: String;
  n: TXmlNode;
begin
  Result:=False;

  if IsReferenced(Node, refName) then
    Exit;

  if Node.HasAttribute('name') then
    refName:=Node.Attributes['name']
  else begin
    raise ENodeException.CreateFmt('Node does not have a name!'#13#10'%s', [TNodeInfo.GetNodeInfo(Node)]);
  end;

  if FLastXSDImportsSearchRoot = Nil then begin
    FLastXSDImportsSearchRoot:=TXSDImportsSearchRootList.Create(False);
    FLastXSDImportsSearchRoot.Add(FXSD.DocumentElement);
  end;

  n:=FLastXSDImportsSearchRoot.Last;
  if n <> Nil then
    n:=n.FindNode('element', 'ref', refName, [ntElement], [nsRecursive, nsSearchWithoutPrefix]);
  if n <> Nil then
    Result:=True;
end;

function TXSD2XMLParser.IsReferenced(const Node: TXmlNode; var ReferenceName: String): Boolean;
begin
  ReferenceName:='';
  Result:=Node.HasAttribute('ref');
  if Result then
    ReferenceName:=Node.Attributes['ref'];
end;

function TXSD2XMLParser.IsSimpleType(const Node: TXmlNode): Boolean;
begin
  Result:=False;
  if (not Node.HasAttribute('type') and not IsComplexType(Node)) or (Node.FindNode('simpleType', [ntElement], [nsSearchWithoutPrefix]) <> Nil) then
    Result:=True;
end;

function TXSD2XMLParser.IsComplexType(const Node: TXmlNode): Boolean;
begin
  Result:=(Node.FindNode('complexType', [ntElement], [nsSearchWithoutPrefix]) <> Nil);
end;

function TXSD2XMLParser.IsTypeDeclared(const Node: TXmlNode; var TypeDeclared: String): Boolean;
begin
  TypeDeclared:='';
  Result:=Node.HasAttribute('type');
  if Result then
    TypeDeclared:=Node.Attributes['type'];
end;

function TXSD2XMLParser.IsAbstract(const Node: TXmlNode): Boolean;
begin
  Result:=False;
end;

function TXSD2XMLParser.IsOptional(const Node: TXmlNode): Boolean;
begin
  Result:=Node.HasAttribute('minOccurs');
  if Result then
    Result:=(Node.Attributes['minOccurs'] = '0');
end;

function TXSD2XMLParser.IsDefault(const Node: TXmlNode; var DefaultValue: String): Boolean;
begin
  DefaultValue:='';
  Result:=Node.HasAttribute('default');
  if Result then
    DefaultValue:=Node.Attributes['default'];
end;

function TXSD2XMLParser.IsFixed(const Node: TXmlNode; var FixedValue: String): Boolean;
begin
  FixedValue:='';
  Result:=Node.HasAttribute('fixed');
  if Result then
    FixedValue:=Node.Attributes['fixed'];
end;

function TXSD2XMLParser.IsSimpleContent(const Node: TXmlNode): Boolean;
begin
  Result:=(Node.FindNode('simpleContent', [ntElement], [nsSearchWithoutPrefix]) <> Nil);
end;

function TXSD2XMLParser.IsMixed(const Node: TXmlNode): Boolean;
begin
  Result:=(Node.HasAttribute('mixed') and (LowerCase(Node.Attributes['mixed']) = 'true'));
end;

function TXSD2XMLParser.IsBuiltinSkipableElement(const Node: TXmlNode): Boolean;
var
  ElementName: String;
begin
  Result:=False;
  if (Node.Name = '') and (Node.NodeType <> ntElement) then begin
    Result:=True;
    Exit;
  end
  else begin
    ElementName:=Node.Name;
    if ElementName = 'annotation' then
      Result:=True
    else if ElementName = 'any' then
      Result:=True
    else if ElementName = 'anyAttribute' then
      Result:=True
    else if ElementName = 'appinfo' then
      Result:=True
    else if ElementName = 'documentation' then
      Result:=True;
    ElementName:='';
  end;
end;

class function TXSD2XMLParser.IsBuiltinAttr(const AttributeName: String; const IncludeExtended: Boolean = False): Boolean;
begin
  Result:=False;
  if AttributeName = 'length' then
    Result:=True
  else if AttributeName = 'maxLength' then
    Result:=True
  else if AttributeName = 'minLength' then
    Result:=True
  else if AttributeName = 'pattern' then
    Result:=True
  else if AttributeName = 'whiteSpace' then
    Result:=True
  else if AttributeName = 'maxExclusive' then
    Result:=True
  else if AttributeName = 'maxInclusive' then
    Result:=True
  else if AttributeName = 'minExclusive' then
    Result:=True
  else if AttributeName = 'minInclusive' then
    Result:=True
  else if AttributeName = 'fractionDigits' then
    Result:=True
  else if AttributeName = 'totalDigits' then
    Result:=True;

  if IncludeExtended then begin
    if AttributeName = 'targetNamespace' then
      Result:=True
    else if AttributeName = 'elementFormDefault' then
      Result:=True
    else if AttributeName = 'type' then
      Result:=True
    else if AttributeName = 'minOccurs' then
      Result:=True
    else if AttributeName = 'maxOccurs' then
      Result:=True
    else if AttributeName = 'base' then
      Result:=True
    else if AttributeName = 'value' then
      Result:=True
    else if AttributeName = 'fixed' then
      Result:=True
    else if AttributeName = 'default' then
      Result:=True
    else if AttributeName = 'use' then
      Result:=True
    else if AttributeName = 'xpath' then
      Result:=True
    else if AttributeName = 'nillable' then
      Result:=True;
  end;
end;

function TXSD2XMLParser.IsPascalType(const TypeName: String): Boolean;
var
  et: TXSDElementType;
begin
  Result:=True;
  et:=GetXSDBuiltinType(TypeName);
  case et of
    etUnknown,
    etDuration,
    etBase64Binary,
    etHexBinary: Result:=False;
  end;
end;

function TXSD2XMLParser.GetXSDBuiltinType(const ElementTypeName: String): TXSDElementType;
var
  et: TXSDElementType;
  type_name: String;
begin
  Result:=etUnknown;
  type_name:=LowerCase(ElementTypeName);
  for et in [Low(TXSDElementType)..High(TXSDElementType)] do begin
    if type_name = TXSDElementTypeString[et] then begin
      Result:=et;
      Break;
    end;
  end;
end;

function TXSD2XMLParser.GetSimpleType(var Node: TXmlNode): TXSDSimpleType;
var
  Child: TXmlNode;
begin
  Result:=stUnknown;
  Child:=Node.ChildNodes.First;
  if Child = Nil then
    Exit;

  if LowerCase(Child.Name) = 'annotation' then
    Child:=Child.NextSibling;

  if Child <> Nil then
    Node:=Child
  else
    Exit;

  if LowerCase(Child.Name) = 'restriction' then
    Result:=stRestriction
  else if LowerCase(Child.Name) = 'list' then
    Result:=stList
  else if LowerCase(Child.Name) = 'union' then
    Result:=stUnion;
end;

function TXSD2XMLParser.GetComplexType(var Node: TXmlNode): TXSDComplexType;
var
  Child: TXmlNode;
begin
  Result:=ctUnknown;
  if Node.ChildNodes.Count = 0 then
    Exit;

  Child:=Node.ChildNodes.First;
  if Child = Nil then
    Exit;

  if LowerCase(Child.Name) = 'annotation' then
    Child:=Child.NextSibling;

  if Child <> Nil then
    Node:=Child
  else
    Exit;

  if LowerCase(Child.Name) = 'all' then
    Result:=ctAll
  else if LowerCase(Child.Name) = 'choice' then
    Result:=ctChoice
  else if LowerCase(Child.Name) = 'sequence' then
    Result:=ctSequence
  else if LowerCase(Child.Name) = 'simplecontent' then
    Result:=ctSimpleContent
  else if LowerCase(Child.Name) = 'complexcontent' then
    Result:=ctComplexContent
  else if LowerCase(Child.Name) = 'element' then begin
    Result:=ctElement;
    Node:=Child.ParentNode; // go back one level
  end;

  if Result = ctUnknown then
    Node:=Node.ParentNode;
end;

function TXSD2XMLParser.GetPascalType(const TypeName: String): String;
var
  et: TXSDElementType;
begin
  et:=GetXSDBuiltinType(TypeName);
  case et of
    etUnknown: Result:='';
    etToken: Result:='string(token)';
    etString: Result:='string';
    etFloat: Result:='float';
    etDecimal: Result:='decimal';
    etInteger: Result:='integer';
    etByte: Result:='short';
    etInt: Result:='integer';
    etLong: Result:='int64';
    etNegativeInteger: Result:='integer(negative)';
    etNonNegativeInteger: Result:='integer(non-negative)';
    etNonPositiveInteger: Result:='integer(non-positive)';
    etPositiveInteger: Result:='integer(positive)';
    etShort: Result:='word';
    etUnsignedLong: Result:='int64';
    etUnsignedInt: Result:='longint';
    etUnsignedShort: Result:='word';
    etUnsignedByte: Result:='byte';
    etBoolean: Result:='boolean';
    etDate: Result:='date';
    etTime: Result:='time';
    etDateTime: Result:='datetime';
    etgDay: Result:='date(day)';
    etgMonth: Result:='date(month)';
    etgMonthDay: Result:='date(month-day)';
    etgYear: Result:='date(year)';
    etgYearMonth: Result:='date(year-month)';
    etNormalizedString: Result:='string(normalized)';
//    etDuration: Result:='';
//    etBase64Binary: Result:='';
//    etHexBinary: Result:='';
    etAnyURI: Result:='string';
    etENTITY: Result:='string';
    etENTITIES: Result:='string';
    etID: Result:='string';
    etIDREF: Result:='string';
    etIDREFS: Result:='string';
    etLanguage: Result:='string';
    etName: Result:='string';
    etNCName: Result:='string';
    etNMTOKEN: Result:='string';
    etNMTOKENS: Result:='string';
    etQName: Result:='string';
    etNOTATION: Result:='string';
  else
    Result:='';
  end;
end;

function TXSD2XMLParser.Add(const Node, Parent: TXmlNode; const Name: String = ''; const CheckOptional: Boolean = True; const SetOptional: Boolean = False): TXmlNode;
var
  node_name: String;
  Attr: TXmlAttribute;
begin
  if Name = '' then
    node_name:=Node.Attributes['name']
  else
    node_name:=Name;

  if FIncludePrefix then
    node_name:=ifString(FIncludePrefixString[High(FIncludePrefixString)] <> '', FIncludePrefixString[High(FIncludePrefixString)] + ':' + node_name, node_name);

  if Parent = Nil then begin
    Result:=FXML.AddChild(node_name);
    if (FSchemaTargetNamespace <> '') and (FSchemaTargetNamespace <> FSchemaNamespace) then
      Result.SetAttribute('xmlns', FSchemaTargetNamespace)
    else if FSchemaNamespace <> '' then
      Result.SetAttribute('xmlns', FSchemaNamespace);
  end
  else
    Result:=Parent.AddChild(node_name);

  if (CheckOptional and IsOptional(Node)) or SetOptional then
    Result.SetAttribute('optional', 'true');

  for Attr in Node.AttributeList do
    if LowerCase(Attr.Name) <> 'name' then
      Result.SetAttribute(Attr.Name, Attr.Value);
end;

class function TXSD2XMLParser.LoadFromFile(const FileName: String; const BufferSize: Integer): TXSD2XMLParser;
begin
  Result:=TXSD2XMLParser.Create;
  Result.FXSD.LoadFromFile(FileName, BufferSize);
  Result.Parse;
end;

procedure TXSD2XMLParser.Clear;
begin
  FXML.Clear;
  if FLastXSDImportsSearchRoot <> Nil then
    FreeAndNil(FLastXSDImportsSearchRoot);
  FSchemaPrefix:='';
  FSchemaNamespace:='';
  FSchemaTargetNamespace:='';
  FIncludePrefix:=False;
  SetLength(FIncludePrefixString, 0);
  array_insert('', FIncludePrefixString);
end;

procedure TXSD2XMLParser.Parse;
var
  Parent, Node: TXmlNode;
begin
  Clear;

  Parent:=Nil; // the FXML node
  Node:=FXSD.DocumentElement;

  ParseSchemaAttributes(Node);

  while Node <> Nil do begin
    if IsSame(Node.Name, 'schema') then begin
      Node:=Node.ChildNodes.First;
    end;

    if IsSame(Node.Name, 'import') and (xsdParseImportInstr in Options) then
      ParseImport(Node, Parent)
//    else if IsSame(Node.Name, 'complexType') then
//      ParseComplexType(Node, Parent)
//    else if IsSame(Node.Name, 'simpleType') then
//      ParseSimpleType(Node, Parent)
    else if IsSame(Node.Name, 'element') then
      ParseElement(Node, Parent);

    Node:=Node.NextSibling;
  end;
end;

procedure TXSD2XMLParser.ParseSchemaAttributes(const Node: TXmlNode);
var
  name, prefix, temp: String;
  attr: TXmlAttribute;
begin
  TXmlNode.GetNameAndPrefix(Node.NameWithPrefix, temp, FSchemaPrefix);
  if Node.AttributeList.Count > 0 then begin
    for attr in Node.AttributeList do begin
      TXmlNode.GetNameAndPrefix(attr.Name, name, prefix);
      if (LowerCase(name) = 'xmlns') and (prefix = '') then
        FSchemaNamespace:=attr.Value
      else if (LowerCase(name) = 'targetnamespace') and (prefix = '') then
        FSchemaTargetNamespace:=attr.Value;
    end;
  end;
end;

procedure TXSD2XMLParser.ParseImport(const Node: TXmlNode; var Parent: TXmlNode);
var
  Import: TXSDImport;
begin
  Import:=MakeXSDImport(Node);
  if Import <> Nil then
    FXSDImports.Add(Import);
end;

procedure TXSD2XMLParser.ParseElement(const Node: TXmlNode; var Parent: TXmlNode; const Recursive: Boolean = False; const Optional: Boolean = False);
var
  Child: TXmlNode;
  ReferenceName, NodeType: String;
begin
  // check if it is referenced, if it is then skip
  if Recursive or not HasReference(Node) then begin
    if IsReferenced(Node, ReferenceName) then begin // referenced type
      ParseReference(Node, Parent, ReferenceName);
    end
    else if IsSimpleType(Node) then begin
      Parent:=Add(Node, Parent, '', True, Optional);
      Child:=Node.FindNode('simpleType', [ntElement], [nsRecursive]); // recursive
      if Child = Nil then
        Child:=Node;
      ParseSimpleType(Child, Parent);
      Parent:=Parent.ParentNode;
    end
    else if IsTypeDeclared(Node, NodeType) then begin // check if it is referenced by type declaration
      Parent:=Add(Node, Parent, '', True, Optional);
      ParseTypeReference(Node, Parent, NodeType);
      Parent:=Parent.ParentNode;
    end
    else if IsComplexType(Node) then begin // complex type
      Parent:=Add(Node, Parent, '', True, Optional);
      Child:=Node.FindNode('complexType', [ntElement], [nsRecursive]); // recursive
      if Child <> Nil then
        ParseComplexType(Child, Parent);
      Parent:=Parent.ParentNode;
    end;
  end;
end;

procedure TXSD2XMLParser.ParseSimpleType(const Node: TXmlNode; var Parent: TXmlNode);
var
//  simple_type: TXSDSimpleType;
  Child: TXmlNode;
begin
  Child:=Node;
//  simple_type:=GetSimpleType(Child);
//  if simple_type = stUnknown then begin
//    raise ENodeException.CreateFmt('Unknown node or child nodes not found!'#13#10'%s', [GetNodeInfo(Node)]);
//  end;

  Node.Attributes['resolved']:='false';

  Child:=Child.ChildNodes.First;
  while Child <> Nil do begin
    if IsSame(Child.Name, 'restriction') then
      ParseRestriction(Child, Parent)
//    else if IsSame(Child.Name, 'list') then
//      ParseList(Child, Parent)
    else if IsSame(Child.Name, 'union') then
      ParseUnion(Child, Parent)
    else if IsBuiltinSkipableElement(Child) then begin
      // Built in elements that can be skiped - do nothing
    end
    else begin
      raise ENodeException.CreateFmt('Unknown node!'#13#10'%s', [TNodeInfo.GetNodeInfo(Child)]);
    end;
    Child:=Child.NextSibling;
  end;

  Node.Attributes['resolved']:='true';
end;

procedure TXSD2XMLParser.ParseComplexType(const Node: TXmlNode; var Parent: TXmlNode);
var
//  complex_type: TXSDComplexType;
  Child: TXmlNode;
//  InternalNode: Boolean;
begin
  Child:=Node;
{  complex_type:=GetComplexType(Child);
  if complex_type = ctUnknown then begin
    raise ENodeException.CreateFmt('Unknown node or child nodes not found!'#13#10'%s', [TNodeInfo.GetNodeInfo(Node)]);
  end;

  InternalNode:=False;
  if complex_type = ctChoice then begin
    Parent:=Add(Node, Parent, 'Choice');
    InternalNode:=True;
  end;}
//  try
//    Child:=Node;

  Node.Attributes['resolved']:='false';

    Child:=Child.ChildNodes.First;
    while Child <> Nil do begin
      if IsSame(Child.Name, 'element') then
        ParseElement(Child, Parent)
      else if IsSame(Child.Name, 'extension') then
        ParseExtension(Child, Parent)
      else if IsSame(Child.Name, 'choice') then
        ParseChoice(Child, Parent)
      else if IsSame(Child.Name, 'sequence') then
        ParseSequence(Child, Parent)
      else if IsSame(Child.Name, 'simpleContent') then
        ParseComplexType(Child, Parent)
      else if IsSame(Child.Name, 'complexContent') then
        ParseComplexType(Child, Parent)
      else if IsSame(Child.Name, 'attribute') then
        ParseAttribute(Child, Parent)
      else if IsBuiltinSkipableElement(Child) then begin
        // Built in elements that can be skiped - do nothing
      end
      else begin
        raise ENodeException.CreateFmt('Unknown node!'#13#10'%s', [TNodeInfo.GetNodeInfo(Child)]);
      end;
      Child:=Child.NextSibling;
    end;
{  finally
    if InternalNode then
      Parent:=Parent.ParentNode;
  end;}

  Node.Attributes['resolved']:='true';
end;

procedure TXSD2XMLParser.ParseRestriction(const Node: TXmlNode; var Parent: TXmlNode);
var
  base: String;
  Child: TXmlNode;
  complex_type: TXSDComplexType;
begin
  base:='';
  if Node.HasAttribute('base') then
    base:=Node.Attributes['base'];

  if base <> '' then begin
    ParseTypeReference(Node, Parent, base);
    Child:=Node;
    complex_type:=GetComplexType(Child);
    if complex_type <> ctUnknown then begin // extension complex type parse
      Child:=Node;
      Child:=Child.ChildNodes.First;
      while Child <> Nil do begin
//        if IsSame(Child.Name, 'all') then
//          ParseAttribute(Child, Parent)
        if IsSame(Child.Name, 'choice') then
          ParseChoice(Child, Parent)
        else if IsSame(Child.Name, 'sequence') then
          ParseSequence(Child, Parent)
        else if IsSame(Child.Name, 'group') then
          ParseGroup(Child, Parent)
//        else if IsSame(Child.Name, 'complexType') then
//          ParseComplexType(Child, Parent)
        else if IsSame(Child.Name, 'attribute') then
          ParseAttribute(Child, Parent)
        else begin
          raise ENodeException.CreateFmt('Unknown node!'#13#10'%s', [TNodeInfo.GetNodeInfo(Child)]);
        end;
        Child:=Child.NextSibling;
      end;
    end
    else begin // extension attributes declaration parse
      if Child.ChildNodes.Count = 0 then
        Exit;

      Child:=Child.ChildNodes.First;
      while Child <> Nil do begin
        if IsSame(Child.Name, 'attribute') then
          ParseAttribute(Child, Parent)
        else if IsSame(Child.Name, 'enumeration') then
          ParseEnumeration(Child, Parent)
        else if IsBuiltinAttr(Child.Name) then
          ParseBuiltinAttr(Child, Parent)
        else begin
          raise ENodeException.CreateFmt('Unknown node!'#13#10'%s', [TNodeInfo.GetNodeInfo(Child)]);
        end;
        Child:=Child.NextSibling;
      end;
    end;
  end
  else begin
    raise ENodeException.CreateFmt('Restriction must have base type!'#13#10'%s', [TNodeInfo.GetNodeInfo(Node)]);
  end;
end;

procedure TXSD2XMLParser.ParseExtension(const Node: TXmlNode; var Parent: TXmlNode);
var
  base: String;
  Child: TXmlNode;
  complex_type: TXSDComplexType;
begin
  base:='';
  if Node.HasAttribute('base') then
    base:=Node.Attributes['base'];

  if base <> '' then begin
    ParseTypeReference(Node, Parent, base);
    Child:=Node;
    complex_type:=GetComplexType(Child);
    if complex_type <> ctUnknown then begin // extension complex type parse
      Child:=Node;
      Child:=Child.ChildNodes.First;
      while Child <> Nil do begin
//        if IsSame(Child.Name, 'all') then
//          ParseAttribute(Child, Parent)
        if IsSame(Child.Name, 'choice') then
          ParseChoice(Child, Parent)
        else if IsSame(Child.Name, 'sequence') then
          ParseSequence(Child, Parent)
        else if IsSame(Child.Name, 'group') then
          ParseGroup(Child, Parent)
//        else if IsSame(Child.Name, 'complexType') then
//          ParseComplexType(Child, Parent)
        else if IsSame(Child.Name, 'attribute') then
          ParseAttribute(Child, Parent)
        else begin
          raise ENodeException.CreateFmt('Unknown node!'#13#10'%s', [TNodeInfo.GetNodeInfo(Child)]);
        end;
        Child:=Child.NextSibling;
      end;
    end
    else begin // extension attributes declaration parse
      if Child.ChildNodes.Count = 0 then
        Exit;

      Child:=Child.ChildNodes.First;
      while Child <> Nil do begin
        if IsSame(Child.Name, 'attribute') then
          ParseAttribute(Child, Parent)
        else begin
          raise ENodeException.CreateFmt('Unknown node!'#13#10'%s', [TNodeInfo.GetNodeInfo(Child)]);
        end;
        Child:=Child.NextSibling;
      end;
    end;
  end
  else begin
    raise ENodeException.CreateFmt('Extension must have base type!'#13#10'%s', [TNodeInfo.GetNodeInfo(Node)]);
  end;
end;

procedure TXSD2XMLParser.ParseTypeReference(const Node: TXmlNode; var Parent: TXmlNode; const ReferenceName: String);

  function CheckCircularTypeReference(const CheckNode: TXmlNode): Boolean;
  var
    refPrefix, refName: String;
  begin
    Result:=True;
    // detect circular type reference
    if Node.HasAttribute('base') then
      TXmlNode.GetNameAndPrefix(Node.Attributes['base'], refName, refPrefix);
    if not CheckNode.HasAttribute('resolved') then
      Result:=False
    else if CheckNode.HasAttribute('resolved') and (CheckNode.Attributes['resolved'] = 'true') and
            (Node.HasAttribute('name') and Node.HasAttribute('type') and (Parent.Name = Node.Attributes['name']) and Parent.HasAttribute('type') and (Parent.Attributes['type'] = Node.Attributes['type'])) or
            (Node.HasAttribute('base') and CheckNode.HasAttribute('name') and (refName = CheckNode.Attributes['name'])) then
      Result:=False
    else begin
      if Node.HasAttribute('minOccurs') and (Node.Attributes['minOccurs'] = '0') and
         Node.HasAttribute('maxOccurs') and (LowerCase(Node.Attributes['maxOccurs']) = 'unbounded') then
        Exit
      else
        raise ENodeException.CreateFmt('Circular type reference!'#13#10'%s', [TNodeInfo.GetNodeInfo(Node)]);
    end;
  end;

var
  refPrefix, refName, attrValue: String;
//  i: Integer;
  SearchRoot, SearchNode: TXmlNode;
  attr: TXmlAttribute;
  XSDImport: TXSDImport;
begin
  TXmlNode.GetNameAndPrefix(ReferenceName, refName, refPrefix);

  if GetXSDBuiltinType(refName) <> etUnknown then begin
    ParseValue(Node, Parent);
    Exit;
  end;

  if FLastXSDImportsSearchRoot = Nil then begin
    FLastXSDImportsSearchRoot:=TXSDImportsSearchRootList.Create(False);
    FLastXSDImportsSearchRoot.Add(FXSD.DocumentElement);
  end;

  SearchRoot:=FLastXSDImportsSearchRoot.Last;
  if refPrefix <> '' then begin // check imports to search for type reference
    attr:=SearchRoot.AttributeList.Find('xmlns:' + refPrefix);
    if attr <> Nil then begin
      attrValue:=attr.Value;
      if attrValue <> '' then begin
        XSDImport:=FXSDImports.Find(attrValue);
        if (XSDImport <> Nil) and (XSDImport.Import <> Nil) and (XSDImport.Import.DocumentElement <> Nil) then begin
          if FXML.DocumentElement <> Nil then
            FXML.DocumentElement.SetAttribute(attr.Name, attr.Value);

          SearchRoot:=XSDImport.Import.DocumentElement;
          FLastXSDImportsSearchRoot.Add(SearchRoot);

          if FLastXSDImportsSearchRoot.Count > 1 then begin
            FIncludePrefix:=True;
            array_insert(refPrefix, FIncludePrefixString);
          end;
        end;
      end;
    end;
  end;

  SearchNode:=SearchRoot.FindNode{Recursive}('simpleType', 'name', refName, [ntElement], [nsSearchWithoutPrefix]);
  if SearchNode <> Nil then begin
    if not CheckCircularTypeReference(SearchNode) then
      ParseSimpleType(SearchNode, Parent);
  end
  else begin
    SearchNode:=SearchRoot.FindNode{Recursive}('complexType', 'name', refName, [ntElement], [nsSearchWithoutPrefix]);
    if SearchNode <> Nil then begin
      if not CheckCircularTypeReference(SearchNode) then
        ParseComplexType(SearchNode, Parent);
    end
    else begin
      raise ENodeException.CreateFmt('Type reference not found!'#13#10'%s', [TNodeInfo.GetNodeInfo(Node)]);
    end;
  end;

  if FLastXSDImportsSearchRoot.Count > 1 then begin
    FLastXSDImportsSearchRoot.Delete(FLastXSDImportsSearchRoot.Count - 1);
    if Length(FIncludePrefixString) > 1 then
      FIncludePrefixString:=array_delete(FIncludePrefixString, High(FIncludePrefixString));
  end;

  if FLastXSDImportsSearchRoot.Count <= 1 then begin
    FIncludePrefix:=False;
    SetLength(FIncludePrefixString, 0);
    array_insert('', FIncludePrefixString);
  end;
end;

procedure TXSD2XMLParser.ParseReference(const Node: TXmlNode; var Parent: TXmlNode; const ReferenceName: String);
var
  refPrefix, refName, attrValue: String;
//  i: Integer;
  SearchRoot, SearchNode: TXmlNode;
  attr: TXmlAttribute;
  XSDImport: TXSDImport;
begin
  TXmlNode.GetNameAndPrefix(ReferenceName, refName, refPrefix);

  if GetXSDBuiltinType(refName) <> etUnknown then
    Exit;

  if FLastXSDImportsSearchRoot = Nil then begin
    FLastXSDImportsSearchRoot:=TXSDImportsSearchRootList.Create(False);
    FLastXSDImportsSearchRoot.Add(FXSD.DocumentElement);
  end;

  SearchRoot:=FLastXSDImportsSearchRoot.Last;
  if refPrefix <> '' then begin // check imports to search for type reference
    attr:=SearchRoot.AttributeList.Find('xmlns:' + refPrefix);
    if attr <> Nil then begin
      attrValue:=attr.Value;
      if attrValue <> '' then begin
        XSDImport:=FXSDImports.Find(attrValue);
        if (XSDImport <> Nil) and (XSDImport.Import <> Nil) and (XSDImport.Import.DocumentElement <> Nil) then begin
          if FXML.DocumentElement <> Nil then
            FXML.DocumentElement.SetAttribute(attr.Name, attr.Value);

          SearchRoot:=XSDImport.Import.DocumentElement;
          FLastXSDImportsSearchRoot.Add(SearchRoot);

          if FLastXSDImportsSearchRoot.Count > 1 then begin
            FIncludePrefix:=True;
            array_insert(refPrefix, FIncludePrefixString);
          end;
        end;
      end;
    end;
  end;

  SearchNode:=SearchRoot.FindNode{Recursive}('element', 'name', refName, [ntElement], [nsSearchWithoutPrefix]);
  if SearchNode <> Nil then
    ParseElement(SearchNode, Parent, True, IsOptional(Node))
  else begin
    raise ENodeException.CreateFmt('Reference not found!'#13#10'%s', [TNodeInfo.GetNodeInfo(Node)]);
  end;

  if FLastXSDImportsSearchRoot.Count > 1 then begin
    FLastXSDImportsSearchRoot.Delete(FLastXSDImportsSearchRoot.Count - 1);
    if Length(FIncludePrefixString) > 1 then
      FIncludePrefixString:=array_delete(FIncludePrefixString, High(FIncludePrefixString));
  end;

  if FLastXSDImportsSearchRoot.Count <= 1 then begin
    FIncludePrefix:=False;
    SetLength(FIncludePrefixString, 0);
    array_insert('', FIncludePrefixString);
  end;
end;

procedure TXSD2XMLParser.ParseValue(const Node: TXmlNode; var Parent: TXmlNode);
var
  value, attr_value, attr_prefix: String;
begin
  if IsFixed(Node, value) then
    Parent.NodeValue:=value
  else if IsDefault(Node, value) then
    Parent.NodeValue:=value;

  if Node.HasAttribute('base') then begin
    attr_value:=Node.Attributes['base'];
    TXmlNode.GetNameAndPrefix(attr_value, attr_value, attr_prefix);
    Parent.SetAttribute('type', ifString(IsPascalType(attr_value), GetPascalType(attr_value), attr_value));
  end;
end;

procedure TXSD2XMLParser.ParseAttribute(const Node: TXmlNode; var Parent: TXmlNode);
var
  attr_name{, attr_value, attr_prefix}: String;
begin
  // check type of attribute
  attr_name:=Node.Attributes['name'];
  if Node.HasAttribute('fixed') then
    Parent.SetAttribute(attr_name, Node.Attributes['fixed'])
  else if Node.HasAttribute('default') then
    Parent.SetAttribute(attr_name, Node.Attributes['default']);
{
  if Node.HasAttribute('type') then begin
    attr_value:=Node.Attributes['type'];
    GetNameAndPrefix(attr_value, attr_value, attr_prefix);
    Parent.SetAttribute('type', ifString(IsPascalType(attr_value), GetPascalType(attr_value), attr_value));
  end;
}
end;

procedure TXSD2XMLParser.ParseEnumeration(const Node: TXmlNode; var Parent: TXmlNode);
var
  value: String;
begin
  if Node.HasAttribute('value') then
    Parent.NodeValue:=Parent.NodeValue + ';' + Node.Attributes['value'];

  if (Length(Parent.NodeValue) > 0) and (Parent.NodeValue[1] = ';') then begin
    value:=Parent.NodeValue;
    Delete(value, 1, 1);
    Parent.NodeValue:=value;
  end;
end;

procedure TXSD2XMLParser.ParseList(const Node: TXmlNode; var Parent: TXmlNode);
begin
  raise ENodeException.CreateFmt('Node not supported!'#13#10'%s', [TNodeInfo.GetNodeInfo(Node)]);
end;

procedure TXSD2XMLParser.ParseUnion(const Node: TXmlNode; var Parent: TXmlNode);
var
  Child: TXmlNode;
begin
  Child:=Node;
  Child:=Child.ChildNodes.First;
  while Child <> Nil do begin
    if IsSame(Child.Name, 'simpleType') then
      ParseSimpleType(Child, Parent)
    else begin
      raise ENodeException.CreateFmt('Unknown node!'#13#10'%s', [TNodeInfo.GetNodeInfo(Child)]);
    end;
    Child:=Child.NextSibling;
  end;
end;

procedure TXSD2XMLParser.ParseChoice(const Node: TXmlNode; var Parent: TXmlNode);
var
  Child: TXmlNode;
  InternalNode: Boolean;
begin
  // element | group | choice | sequence | any
  Parent:=Add(Node, Parent, 'Choice');
  try
    InternalNode:=False;
    if IsOptional(Node) then begin
      Parent:=Add(Node, Parent, 'Optional', False);
      InternalNode:=True;
    end;

    Child:=Node;
    Child:=Child.ChildNodes.First;
    while Child <> Nil do begin
      if IsSame(Child.Name, 'element') then
        ParseElement(Child, Parent)
      else if IsSame(Child.Name, 'group') then
        ParseGroup(Child, Parent)
      else if IsSame(Child.Name, 'choice') then
        ParseChoice(Child, Parent)
      else if IsSame(Child.Name, 'sequence') then
        ParseSequence(Child, Parent)
      else if IsBuiltinSkipableElement(Child) then begin
        // Built in elements that can be skiped - do nothing
      end
      else if IsBuiltinAttr(Child.Name) then
        ParseBuiltinAttr(Child, Parent)
      else begin
        raise ENodeException.CreateFmt('Unknown node!'#13#10'%s', [TNodeInfo.GetNodeInfo(Child)]);
      end;
      Child:=Child.NextSibling;
    end;

    if InternalNode then
      Parent:=Parent.ParentNode;
  finally
    Parent:=Parent.ParentNode;
  end;
end;

procedure TXSD2XMLParser.ParseGroup(const Node: TXmlNode; var Parent: TXmlNode);
begin
  raise ENodeException.CreateFmt('Node not supported!'#13#10'%s', [TNodeInfo.GetNodeInfo(Node)]);
end;

procedure TXSD2XMLParser.ParseSequence(const Node: TXmlNode; var Parent: TXmlNode);
var
  Child: TXmlNode;
  InternalNode: Boolean;
begin
  // element | group | choice | sequence | any
  InternalNode:=False;
  if IsOptional(Node) then begin
    Parent:=Add(Node, Parent, 'Optional', False);
    InternalNode:=True;
  end;

  // element | group | choice | sequence | any
  Child:=Node;
  Child:=Child.ChildNodes.First;
  while Child <> Nil do begin
    if IsSame(Child.Name, 'element') then
      ParseElement(Child, Parent)
    else if IsSame(Child.Name, 'group') then
      ParseGroup(Child, Parent)
    else if IsSame(Child.Name, 'choice') then
      ParseChoice(Child, Parent)
    else if IsSame(Child.Name, 'sequence') then
      ParseSequence(Child, Parent)
    else if IsBuiltinSkipableElement(Child) then begin
      // Built in elements that can be skiped - do nothing
    end
    else if IsBuiltinAttr(Child.Name) then
      ParseBuiltinAttr(Child, Parent)
    else begin
      raise ENodeException.CreateFmt('Unknown node!'#13#10'%s', [TNodeInfo.GetNodeInfo(Child)]);
    end;
    Child:=Child.NextSibling;
  end;

  if InternalNode then
    Parent:=Parent.ParentNode;
end;

procedure TXSD2XMLParser.ParseBuiltinAttr(const Node: TXmlNode; var Parent: TXmlNode);
var
  AttributeName: String;
begin
  AttributeName:=Node.Name;

  if IsSame(AttributeName, 'length') then
    Parent.SetAttribute(AttributeName, Node.Attributes['value'])
  else if IsSame(AttributeName, 'maxLength') then
    Parent.SetAttribute(AttributeName, Node.Attributes['value'])
  else if IsSame(AttributeName, 'minLength') then
    Parent.SetAttribute(AttributeName, Node.Attributes['value'])
  else if IsSame(AttributeName, 'pattern') then
    Parent.SetAttribute(AttributeName, Node.Attributes['value'])
  else if IsSame(AttributeName, 'whiteSpace') then
    Parent.SetAttribute(AttributeName, Node.Attributes['value'])
  else if IsSame(AttributeName, 'maxExclusive') then
    Parent.SetAttribute(AttributeName, Node.Attributes['value'])
  else if IsSame(AttributeName, 'maxInclusive') then
    Parent.SetAttribute(AttributeName, Node.Attributes['value'])
  else if IsSame(AttributeName, 'minExclusive') then
    Parent.SetAttribute(AttributeName, Node.Attributes['value'])
  else if IsSame(AttributeName, 'minInclusive') then
    Parent.SetAttribute(AttributeName, Node.Attributes['value'])
  else if IsSame(AttributeName, 'fractionDigits') then
    Parent.SetAttribute(AttributeName, Node.Attributes['value'])
  else if IsSame(AttributeName, 'totalDigits') then
    Parent.SetAttribute(AttributeName, Node.Attributes['value']);
end;

end.
