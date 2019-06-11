{
  XSD to XML Parser v.18.0

  Reads XSD schema and outputs XML structure described by schema,
    contains only default or fixed values for nodes and attributes.

  Author:
    (C) 2015-2019, Grzegorz Molenda; gmnevton@o2.pl

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
    v.16 - 2019.01.17 - GM - do not skip 'annotation' for element nodes, just resolve them if xsdParseAnnotations is in Options; replace ChildNodes.First by FirstChild
    v.17 - 2019.02.15 - GM - speedup conversion; added custom THashedStringList to hold parsed types; some minor fixes with regard to speedup
         + 2019.02.27 - GM - fixed MakeXSDImport to properly load shemas from local hard drive
         + 2019.03.10 - GM - added SSL/TLS support for schema downloading rutine; forced HTTP request to retrun UTF-8 encoded data
    v.18 - 2019.05.13 - GM - added "attributeGroup" parsing; moved parsing procedures to protected section and virtualized them; changed ENodeException to EXSDParseException
         +            - GM - added EXSDImportException; added error description constant strings; changed GetPascalType return values to better matching Pascal language
         + 2019.06.07 - GM - added EXSD2XMLParserException parent class; added ParseAttributeTypeReference and ParseAttributeValue
         +            - GM - fixed THashedStringList.Delete to automatically free owned objects; fixed destructor TXSD2XMLParser.Destroy to free runtime created objects (patched memory leak)
         +            - GM - speedup by fixing function TXSD2XMLParser.HasReference to detect if type is declared; changed procedure TXSD2XMLParser.ParseAttribute
}

unit uXSD2XMLParser;

interface

uses
  SysUtils,
  Classes,
  Generics.Defaults,
  Generics.Collections,
  Xml.VerySimple,
  uGMUtils;

type
  EXSD2XMLParserException = class(Exception);
  EXSDParseException = class(EXSD2XMLParserException);
  EXSDImportException = class(EXSD2XMLParserException);

  TXSDOption = (xsdNodeAutoIndent, xsdCompact, xsdParseProcessingInstr, xsdParseImportInstr, xsdPreserveWhiteSpace,
                xsdCaseInsensitive, xsdSearchAutoIncludeNamespacePrefix, xsdWriteBOM, xsdParseAnnotations);
  TXSDOptions = set of TXSDOption;

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
  TXSDReferencedNode = (rnUnknown, rnElement, rnAttributeGroup);

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
    FPath: String;

    procedure ParseImport;
  public
    ID: String;
    Namespace: String;
    SchemaLocation: String;

    constructor Create; virtual;
    destructor Destroy; override;

    function Add(const Path, Value: String): TXSDImport;

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

  TStringHashRec = record
    Value: String;
    Hash: LongWord;
{    case Boolean of
      False: (HashInsensitive: Integer);
      True: (HashSensitive: Integer);}
  end;
  PStringHashRec = ^TStringHashRec;

  TObjectIndex = 1..2;

  TStringRec = record
    Name: PStringHashRec;
    Value: PStringHashRec;
    ObjectRef1: TObject;
    ObjectRef2: TObject;
  end;
  PStringRec = ^TStringRec;

  THashedStringList = class(TPersistent)
  private
    FList: TList;
    FCaseSensitive: Boolean;
    FOwnsObjects: Boolean;
  protected
    function GetValue(Name: String): String; virtual;
    function GetItem(Index: Integer): PStringRec; virtual;
    function GetText(Index: Integer): String; virtual;
    function GetObject(Index: Integer; ObjIndex: TObjectIndex): TObject; virtual;
    procedure SetValue(Name: String; const Value: String); virtual;
    procedure SetItem(Index: Integer; const Value: PStringRec); virtual;
    procedure SetText(Index: Integer; const Value: String); virtual;
    procedure SetObject(Index: Integer; ObjIndex: TObjectIndex; const Value: TObject); virtual;
    function StringExists(const S: String): Boolean; overload;
    function StringExists(const S: String; var atIndex: Integer): Boolean; overload;
    function ValueExists(const S: String): Boolean; overload;
    function ValueExists(const S: String; var atIndex: Integer): Boolean; overload;
  public
    constructor Create; overload;
    constructor Create(const CaseSensitive: Boolean); overload;
    constructor Create(const CaseSensitive: Boolean; const OwnsObjects: Boolean); overload;
    destructor Destroy; override;
  public
    function Add(const S: String; const Value: String = ''): Integer;
    function AddObject(const S: String; const AObject1, AObject2: TObject): Integer;
    procedure Clear;
    function Count: Integer;
    function IndexOfName(const S: String): Integer;
    function IndexOfValue(const S: String): Integer;
    procedure Delete(Index: Integer; const ForceFreeObject: Boolean = False);
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; const S: String; const Value: String = ''); overload;
    procedure Insert(Index: Integer; const S: String; const AObject1, AObject2: TObject); overload;
  public
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property Names[Index: Integer]: String read GetText write SetText;
    property Values[Name: String]: String read GetValue write SetValue;
    property Items[Index: Integer]: PStringRec read GetItem write SetItem; default;
    property Objects[Index: Integer; ObjIndex: TObjectIndex]: TObject read GetObject write SetObject;
  end;

  TXSD2XMLParser = class(TObject)
  private
    FXSD: TXmlVerySimple;
    FXSDImports: TXSDImportList;
    FXML: TXmlVerySimple;
    FLastXSDImportsSearchRoot: TXSDImportsSearchRootList;
    FParsedTypes: THashedStringList;
    FSchemaPrefix: String;
    FSchemaNamespace: String;
    FSchemaTargetNamespace: String;
    FIncludePrefix: Boolean;
    FIncludePrefixString: TStringArray;
    FAbsolutePath: String;
  private const
    ESchemaNotLoaded = 'XSD schema not loaded!';
    EUnknownNode = 'Unknown node!';
    ENodeNotSupported = 'Node not supported!';
    ENodeWithoutName = 'Node does not have a name!';
    ERestrictionBaseTypeNeeded = 'Restriction must have base type!';
    EExtensionBaseTypeNeeded = 'Extension must have base type!';
    ECircularTypeReference = 'Circular type reference!';
    EReferenceListEmpty = 'Searching for type reference with empty list!';
    ETypeReferenceNotFound = 'Type reference not found!';
    EReferenceNotFound = 'Reference not found!';
  protected
    function GetXMLString: String;
    function IsSame(const Value1, Value2: String): Boolean;
    function GetPrefix: String;

    function HasReference(const Node: TXmlNode; const NodeType: TXSDReferencedNode): Boolean; virtual;

    function IsReferenced(const Node: TXmlNode; var ReferenceName: String): Boolean; virtual;
    function IsSimpleType(const Node: TXmlNode; out Child: TXmlNode): Boolean; virtual;
    function IsComplexType(const Node: TXmlNode; out Child: TXmlNode): Boolean; virtual;
    function IsTypeDeclared(const Node: TXmlNode; var TypeDeclared: String): Boolean; virtual;
    function IsAbstract(const Node: TXmlNode): Boolean; virtual;
    function IsOptional(const Node: TXmlNode): Boolean; virtual;
    function IsDefault(const Node: TXmlNode; var DefaultValue: String): Boolean; virtual;
    function IsFixed(const Node: TXmlNode; var FixedValue: String): Boolean; virtual;
    function IsSimpleContent(const Node: TXmlNode): Boolean; virtual; // text only child elements
    function IsMixed(const Node: TXmlNode): Boolean; virtual; // text and child elements inside
    function IsBuiltinSkipableElement(const Node: TXmlNode; const IncludeExtended: Boolean = False): Boolean; virtual;
    class function IsBuiltinAttr(const AttributeName: String; const IncludeExtended: Boolean = False): Boolean; virtual;
    function IsPascalType(const TypeName: String): Boolean; virtual;

    function GetXSDBuiltinType(const ElementTypeName: String): TXSDElementType; virtual;
    function GetSimpleType(var Node: TXmlNode): TXSDSimpleType; virtual;
    function GetComplexType(var Node: TXmlNode): TXSDComplexType; virtual;
    function GetPascalType(const TypeName: String): String; virtual;

    procedure ParseSchemaAttributes(const Node: TXmlNode); virtual;
    procedure ParseImport(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseElement(const Node: TXmlNode; var Parent: TXmlNode; const Recursive: Boolean = False; const Optional: Boolean = False); virtual;
    procedure ParseAnnotation(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseSimpleType(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseComplexType(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseRestriction(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseExtension(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseTypeReference(const Node: TXmlNode; var Parent: TXmlNode; const ReferenceName: String); virtual;
    procedure ParseAttributeTypeReference(const Node: TXmlNode; var Parent: TXmlNode; const ReferenceName: String); virtual;
    procedure ParseReference(const Node: TXmlNode; var Parent: TXmlNode; const ReferenceName: String); virtual;
    procedure ParseValue(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseAttribute(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseAttributeGroup(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseAttributeGroupReference(const Node: TXmlNode; var Parent: TXmlNode; const ReferenceName: String); virtual;
    procedure ParseAttributeValue(const Node: TXmlNode; var Parent: TXmlNode; const ReferenceName: String); virtual;
    procedure ParseEnumeration(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseList(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseUnion(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseChoice(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseGroup(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseSequence(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseBuiltinAttr(const Node: TXmlNode; var Parent: TXmlNode); virtual;
    procedure ParseError(const Message: String; const Node: TXmlNode); virtual;

    function Add(const Node, Parent: TXmlNode; const Name: String = ''; const CheckOptional: Boolean = True; const SetOptional: Boolean = False): TXmlNode;
  public
    Options: TXSDOptions;

    constructor Create; virtual;
    destructor Destroy; override;

    class function LoadFromFile(const FileName: String; const BufferSize: Integer = 4096): TXSD2XMLParser; virtual;

    procedure Clear; virtual;
    procedure Parse; virtual;

    property XSD: TXmlVerySimple read FXSD;
    property XML: TXmlVerySimple read FXML;
    property XMLString: String read GetXMLString;
  end;

implementation

uses
  StrUtils,
  IdHTTP,
  IdSSLOpenSSL,
  IdCompressorZLib;

const
  TXSDElementTypeString: Array[TXSDElementType] of String = (
    '--unknown--',
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
  ssl: TIdSSLIOHandlerSocketOpenSSL;
  reader: TStreamReader;
  output: TStringStream;
begin
  try
    if StartsStr('http', URI) then begin
      ssl:=Nil;
      if StartsStr('https', URI) then
        ssl:=TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
      zlib:=TIdCompressorZLib.Create(Nil);
      output:=TStringStream.Create('', TEncoding.UTF8);
      //
      http:=TIdHTTP.Create(Nil);
      try
        http.Request.AcceptCharSet:='UTF-8';
        http.Request.CharSet:='UTF-8';
        http.Request.ContentEncoding:='UTF-8';
        http.HandleRedirects:=True;
        if ssl <> Nil then
          http.IOHandler:=ssl;
        http.Compressor:=zlib;
        //
        http.Get(URI, output);
        output.Position:=0;
        Result:=output.DataString;
      finally
        output.Free;
        http.Compressor:=Nil;
        zlib.Free;
        if ssl <> Nil then begin
          http.IOHandler:=Nil;
          ssl.Free;
        end;
        http.Free;
      end;
    end
    else if (URI <> '') and FileExists(URI) then begin
      reader:=TStreamReader.Create(URI, TEncoding.UTF8, True);
      try
        Result:=reader.ReadToEnd;
      finally
        reader.Free;
      end;
    end
    else begin
      raise EXSDImportException.CreateFmt('Import URI not recognized!'#13#10'%s', [URI]);
    end;
  except
    Result:='';
    raise;
  end;
end;

function MakeXSDImport(const Node: TXmlNode; const Path: String): TXSDImport;
var
  Import: TXSDImport;
  LPath: String;
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
    LPath:=Import.SchemaLocation;
    if ExtractURLFullPath(LPath) = '' then begin
      if FileExists(Path + LPath) then
        LPath:=Path + LPath
      else
        LPath:=ExtractURLFullPath(Import.Namespace) + LPath;
    end;

    xsd:=LoadXSDImport(LPath);
    if xsd <> '' then
      Import.Add(Path, xsd)
    else begin
      raise EXSDImportException.Create('Import must not be empty!');
    end;
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

  FPath:='';
  FXSDImports.Free;
  inherited;
end;

procedure TXSDImport.ParseImport;
var
  includes_added: Boolean;
begin
  includes_added:=False;
  FImport.DocumentElement.ScanNodes('',
    function (Node: TXmlNode): Boolean // imports and includes
    var
      Import: TXSDImport;
      schema_Location, uri_path: String;
      xsd_include: TXmlVerySimple;
    begin
      if Node.Name = 'import' then begin
        Result:=True;
        Import:=MakeXSDImport(Node, FPath);
        if Import <> Nil then
          FXSDImports.Add(Import);
      end
      else if Node.Name = 'include' then begin
        Result:=True;
        if Node.HasAttribute('schemaLocation') then begin
          schema_Location:=Node.Attributes['schemaLocation'];
          uri_path:=ExtractURLPath(schema_Location);
          if uri_path = '' then begin
            if FileExists(FPath + schema_Location) then
              uri_path:=FPath
            else
              uri_path:=ExtractURLPath(SchemaLocation);
          end;
          xsd_include:=TXmlVerySimple.Create;
          try
            xsd_include.Options:=[doNodeAutoIndent];
            xsd_include.Xml:=LoadXSDImport(uri_path + ExtractURLFileName(schema_Location));
            FImport.DocumentElement.AddNodes(xsd_include.DocumentElement);
            includes_added:=True;
          finally
            xsd_include.Free;
          end;
        end;
      end
      else
        Result:=includes_added;
    end, True);
end;

function TXSDImport.Add(const Path, Value: String): TXSDImport;
begin
  Result:=Self;
  if not Assigned(FImport) then
    FImport:=TXmlVerySimple.Create;

  FPath:=Path;
  FImport.Xml:=Value;

  ParseImport;
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

{ THashedStringList }

function HashStringInsensitive(Value: String): LongWord;
var
  Index: Integer;
begin
  Value:=AnsiUpperCase(Value);
  Result:=0;
  for Index:=1 to Length(Value) do
    Result:=((Result shl 7) or (Result shr 25)) + Ord(Value[Index]);
  Value:='';
end;

function HashStringSensitive(const Value: String): LongWord;
var
  Index: Integer;
begin
  Result:=0;
  for Index:=1 to Length(Value) do
    Result:=((Result shl 7) or (Result shr 25)) + Ord(Value[Index]);
end;

constructor THashedStringList.Create;
begin
  Create(True, False);
end;

constructor THashedStringList.Create(const CaseSensitive: Boolean);
begin
  Create(CaseSensitive, False);
end;

constructor THashedStringList.Create(const CaseSensitive, OwnsObjects: Boolean);
begin
  inherited Create;
  FList:=TList.Create;
  FCaseSensitive:=CaseSensitive;
  FOwnsObjects:=OwnsObjects;
end;

destructor THashedStringList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function THashedStringList.Add(const S, Value: String): Integer;
var
  Data: PStringRec;
begin
  New(Data);
  New(Data.Name);
  New(Data.Value);
  Data.Name.Value:=S;
  if CaseSensitive then
    Data.Name.Hash{Sensitive}:=HashStringSensitive(S)
  else
    Data.Name.Hash{Insensitive}:=HashStringInsensitive(S);
  Data.Value.Value:=Value;
  if CaseSensitive then
    Data.Value.Hash{Sensitive}:=HashStringSensitive(Value)
  else
    Data.Value.Hash{Insensitive}:=HashStringInsensitive(Value);
  Result:=FList.Add(Data);
end;

function THashedStringList.AddObject(const S: String; const AObject1, AObject2: TObject): Integer;
var
  Data: PStringRec;
begin
  Result:=Add(S);
  Data:=FList[Result];
  Data.ObjectRef1:=AObject1;
  Data.ObjectRef2:=AObject2;
end;

procedure THashedStringList.Clear;
var
  Index: Integer;
//  StringData: PStringRec;
begin
  for Index:=FList.Count-1 downto 0 do
    Delete(Index);
end;

function THashedStringList.Count: Integer;
begin
  Result:=FList.Count;
end;

procedure THashedStringList.Delete(Index: Integer; const ForceFreeObject: Boolean);
var
  Data: PStringRec;
  Obj: TObject;
begin
  Data:=FList[Index];
  if FOwnsObjects or ForceFreeObject then begin
    Obj:=Data.ObjectRef1;
    if Obj <> Nil then
      Obj.Free;
    Data.ObjectRef1:=Nil;
    //
    Obj:=Data.ObjectRef2;
    if Obj <> Nil then
      Obj.Free;
    Data.ObjectRef2:=Nil;
  end;
  Dispose(Data.Name);
  Dispose(Data.Value);
  Dispose(Data);
  FList.Delete(Index);
end;

procedure THashedStringList.Exchange(Index1, Index2: Integer);
var
  Item1: PStringRec;
  Item2: PStringRec;
begin
  Item1:=FList[Index1];
  Item2:=FList[Index2];
  FList[Index1]:=Item2;
  FList[Index2]:=Item1;
end;

function THashedStringList.GetItem(Index: Integer): PStringRec;
begin
  Result:=FList[Index];
end;

function THashedStringList.GetText(Index: Integer): String;
begin
  Result:=PStringRec(FList[Index]).Name.Value;
end;

function THashedStringList.GetObject(Index: Integer; ObjIndex: TObjectIndex): TObject;
begin
  Result:=Nil;
  case ObjIndex of
    1: Result:=PStringRec(FList[Index]).ObjectRef1;
    2: Result:=PStringRec(FList[Index]).ObjectRef2;
  end;
end;

function THashedStringList.GetValue(Name: String): String;
var
  Index: Integer;
begin
  Result:=EmptyStr;
  if StringExists(Name, Index) then
    Result:=PStringRec(FList[Index]).Value.Value;
end;

function THashedStringList.IndexOfName(const S: String): Integer;
begin
  StringExists(S, Result);
end;

function THashedStringList.IndexOfValue(const S: String): Integer;
begin
  ValueExists(S, Result);
end;

procedure THashedStringList.Insert(Index: Integer; const S, Value: String);
begin
  Add(S, Value);
  Exchange(Index, FList.Count - 1);
end;

procedure THashedStringList.Insert(Index: Integer; const S: String; const AObject1, AObject2: TObject);
begin
  AddObject(S, AObject1, AObject2);
  Exchange(Index, FList.Count - 1);
end;

procedure THashedStringList.SetItem(Index: Integer; const Value: PStringRec);
var
  Data: PStringRec;
begin
  Data:=FList[Index];
  if OwnsObjects and ((Data.ObjectRef1 <> Nil) or (Data.ObjectRef2 <> Nil)) then begin
    if Data.ObjectRef1 <> Nil then
      Data.ObjectRef1.Free;
    if Data.ObjectRef2 <> Nil then
      Data.ObjectRef2.Free;
  end;
  Data.ObjectRef1:=Nil;
  Data.ObjectRef2:=Nil;
  Dispose(Data.Name);
  Dispose(Data.Value);
  Dispose(Data);
  FList[Index]:=Value;
end;

procedure THashedStringList.SetText(Index: Integer; const Value: String);
var
  Data: PStringRec;
begin
  Data:=FList[Index];
  Data.Name.Value:=Value;
  if CaseSensitive then
    Data.Name.Hash{Sensitive}:=HashStringSensitive(Value)
  else
    Data.Name.Hash{Insensitive}:=HashStringInsensitive(Value);
end;

procedure THashedStringList.SetObject(Index: Integer; ObjIndex: TObjectIndex; const Value: TObject);
var
  Data: PStringRec;
begin
  Data:=FList[Index];
  case ObjIndex of
    1: Data.ObjectRef1:=Value;
    2: Data.ObjectRef2:=Value;
  end;
end;

procedure THashedStringList.SetValue(Name: String; const Value: String);
var
  Index: Integer;
  Data: PStringRec;
begin
  if StringExists(Name, Index) then begin
    Data:=FList[Index];
    Data.Value.Value:=Value;
    if CaseSensitive then
      Data.Value.Hash{Sensitive}:=HashStringSensitive(Value)
    else
      Data.Value.Hash{Insensitive}:=HashStringInsensitive(Value);
  end;
end;

function THashedStringList.StringExists(const S: String): Boolean;
var
  Index: Integer;
begin
  Result:=StringExists(S, Index);
end;

function THashedStringList.StringExists(const S: String; var atIndex: Integer): Boolean;
var
  Index: Integer;
  Hash: LongWord;
begin
  Result:=False;
  atIndex:=-1;
  if CaseSensitive then begin
    Hash:=HashStringSensitive(S);
    for Index:=0 to FList.Count - 1 do
      if PStringRec(FList[Index]).Name.Hash{Sensitive} = Hash then begin
        atIndex:=Index;
        Result:=True;
        Exit;
      end;
  end
  else begin
    Hash:=HashStringInsensitive(S);
    for Index:=0 to FList.Count - 1 do
      if PStringRec(FList[Index]).Name.Hash{Insensitive} = Hash then begin
        atIndex:=Index;
        Result:=True;
        Exit;
      end;
  end;
end;

function THashedStringList.ValueExists(const S: String): Boolean;
var
  Index: Integer;
begin
  Result:=ValueExists(S, Index);
end;

function THashedStringList.ValueExists(const S: String; var atIndex: Integer): Boolean;
var
  Index: Integer;
  Hash: LongWord;
begin
  Result:=False;
  atIndex:=-1;
  if CaseSensitive then begin
    Hash:=HashStringSensitive(S);
    for Index:=0 to FList.Count - 1 do
      if PStringRec(FList[Index]).Value.Hash{Sensitive} = Hash then begin
        atIndex:=Index;
        Result:=True;
        Exit;
      end;
  end
  else begin
    Hash:=HashStringInsensitive(S);
    for Index:=0 to FList.Count - 1 do
      if PStringRec(FList[Index]).Value.Hash{Insensitive} = Hash then begin
        atIndex:=Index;
        Result:=True;
        Exit;
      end;
  end;
end;

{ TXSD2XMLParser }

constructor TXSD2XMLParser.Create;
begin
  Options := [xsdNodeAutoIndent, xsdParseImportInstr, xsdSearchAutoIncludeNamespacePrefix, xsdParseAnnotations];
  FXSD:=TXmlVerySimple.Create;
  FXSDImports:=TXSDImportList.Create;
  FXML:=TXmlVerySimple.Create;
  FXML.Options:=[doParseProcessingInstr];
  FXML.LineBreak:='';
  FXML.NodeIndentStr:='';
  FLastXSDImportsSearchRoot:=Nil;
  FParsedTypes:=THashedStringList.Create(True, False);
  FSchemaPrefix:='';
  FSchemaNamespace:='';
  FSchemaTargetNamespace:='';
  FIncludePrefix:=False;
  SetLength(FIncludePrefixString, 0);
  array_insert('', FIncludePrefixString);
end;

destructor TXSD2XMLParser.Destroy;
var
  i: Integer;
begin
  FAbsolutePath:='';
  for i:=FParsedTypes.Count - 1 downto 0 do begin
    FParsedTypes.Objects[i, 2].Free;
    FParsedTypes.Objects[i, 2]:=Nil;
  end;
  FParsedTypes.Free;
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

function TXSD2XMLParser.HasReference(const Node: TXmlNode; const NodeType: TXSDReferencedNode): Boolean;
var
  refName, typeDecl: String;
  n: TXmlNode;
begin
  Result:=False;

  if IsReferenced(Node, refName) or IsTypeDeclared(Node, typeDecl) then
    Exit;

  if Node.HasAttribute('name') then
    refName:=Node.Attributes['name']
  else begin
    ParseError(ENodeWithoutName, Node);
  end;

  if FLastXSDImportsSearchRoot = Nil then begin
    FLastXSDImportsSearchRoot:=TXSDImportsSearchRootList.Create(False);
    FLastXSDImportsSearchRoot.Add(FXSD.DocumentElement);
  end;

  n:=FLastXSDImportsSearchRoot.Last;
  if n <> Nil then begin
    case NodeType of
      rnElement: n:=n.FindNode('element', 'ref', refName, [ntElement], [nsRecursive, nsSearchWithoutPrefix]);
      rnAttributeGroup: n:=n.FindNode('attributeGroup', 'ref', refName, [ntElement], [nsRecursive, nsSearchWithoutPrefix]);
    else
      n:=Nil;
    end;
  end;
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

function TXSD2XMLParser.IsSimpleType(const Node: TXmlNode; out Child: TXmlNode): Boolean;
var
  TempNode: TXmlNode;
begin
  Result:=False;
  Child:=Node.FindNode('simpleType', [ntElement], [nsSearchWithoutPrefix]);
  if (not Node.HasAttribute('type') and not IsComplexType(Node, TempNode)) or (Child <> Nil) then
    Result:=True;
end;

function TXSD2XMLParser.IsComplexType(const Node: TXmlNode; out Child: TXmlNode): Boolean;
begin
  Child:=Node.FindNode('complexType', [ntElement], [nsSearchWithoutPrefix]);
  Result:=(Child <> Nil);
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

function TXSD2XMLParser.IsBuiltinSkipableElement(const Node: TXmlNode; const IncludeExtended: Boolean = False): Boolean;
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
    if IncludeExtended and (ElementName = 'annotation') then
      Result:=True
    else if IncludeExtended and (ElementName = 'documentation') then
      Result:=True
    else if ElementName = 'any' then
      Result:=True
    else if ElementName = 'anyAttribute' then
      Result:=True
    else if ElementName = 'appinfo' then
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
  Child:=Node.FirstChild;
  if Child = Nil then
    Exit;

  if LowerCase(Child.Name) = 'annotation' then // skip this element
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

  Child:=Node.FirstChild;
  if Child = Nil then
    Exit;

  if LowerCase(Child.Name) = 'annotation' then // skip this element
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
    etByte: Result:='byte';
    etInt: Result:='integer';
    etLong: Result:='int64';
    etNegativeInteger: Result:='integer(negative)';
    etNonNegativeInteger: Result:='integer(non-negative)';
    etNonPositiveInteger: Result:='integer(non-positive)';
    etPositiveInteger: Result:='integer(positive)';
    etShort: Result:='shortint';
    etUnsignedLong: Result:='int64';
    etUnsignedInt: Result:='longword';
    etUnsignedShort: Result:='byte';
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
  Result.FAbsolutePath:=ExtractFilePath(FileName);
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
  if Node = Nil then begin
    ParseError(ESchemaNotLoaded, Nil);
  end;

  ParseSchemaAttributes(Node);

  while Node <> Nil do begin
    if IsSame(Node.Name, 'schema') then begin
      Node:=Node.FirstChild;
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
  Import:=MakeXSDImport(Node, FAbsolutePath);
  if Import <> Nil then
    FXSDImports.Add(Import);
end;

procedure TXSD2XMLParser.ParseElement(const Node: TXmlNode; var Parent: TXmlNode; const Recursive: Boolean = False; const Optional: Boolean = False);
var
  Child, TempNode: TXmlNode;
  ReferenceName, NodeType: String;
begin
  // check if it is referenced, if it is then skip
  if Recursive or not HasReference(Node, rnElement) then begin
    if IsReferenced(Node, ReferenceName) then begin // referenced type
      ParseReference(Node, Parent, ReferenceName);
    end
    else begin
      if Node.HasChildNodes then begin
        Child:=Node.FirstChild;
        if (Child <> Nil) and (xsdParseAnnotations in Options) and IsSame(Child.Name, 'annotation') then begin
          TempNode:=Node;
          ParseAnnotation(Child, TempNode);
          TempNode:=Nil;
        end;
      end;
      //
      if IsTypeDeclared(Node, NodeType) then begin // check if it is referenced by type declaration
        Parent:=Add(Node, Parent, '', True, Optional);
        ParseTypeReference(Node, Parent, NodeType);
        Parent:=Parent.ParentNode;
      end
      else if IsSimpleType(Node, Child) then begin
        Parent:=Add(Node, Parent, '', True, Optional);
        //Child:=Node.FindNode('simpleType', [ntElement], [nsRecursive]); // recursive
        if Child = Nil then
          Child:=Node;
        ParseSimpleType(Child, Parent);
        Parent:=Parent.ParentNode;
      end
      else if IsComplexType(Node, Child) then begin // complex type
        Parent:=Add(Node, Parent, '', True, Optional);
        //Child:=Node.FindNode('complexType', [ntElement], [nsRecursive]); // recursive
        if Child <> Nil then
          ParseComplexType(Child, Parent);
        Parent:=Parent.ParentNode;
      end;
    end;
  end;
end;

procedure TXSD2XMLParser.ParseAnnotation(const Node: TXmlNode; var Parent: TXmlNode);
var
  Child: TXmlNode;
begin
  Child:=Node.FirstChild;
  while Child <> Nil do begin
    if IsSame(Child.Name, 'documentation') and (Child.Text <> '') then
      Parent.Attributes['info']:=Child.Text
    else begin
      ParseError(EUnknownNode, Child);
    end;
    Child:=Child.NextSibling;
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

  Child:=Child.FirstChild;
  while Child <> Nil do begin
    if IsSame(Child.Name, 'restriction') then
      ParseRestriction(Child, Parent)
//    else if IsSame(Child.Name, 'list') then
//      ParseList(Child, Parent)
    else if IsSame(Child.Name, 'union') then
      ParseUnion(Child, Parent)
    else if IsBuiltinSkipableElement(Child, True) then begin
      // Built in elements that can be skiped - do nothing
    end
    else begin
      ParseError(EUnknownNode, Child);
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

    Child:=Child.FirstChild;
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
      else if IsSame(Child.Name, 'attributeGroup') then
        ParseAttributeGroup(Child, Parent)
      else if IsBuiltinSkipableElement(Child, True) then begin
        // Built in elements that can be skiped - do nothing
      end
      else begin
        ParseError(EUnknownNode, Child);
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
      Child:=Child.FirstChild;
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
          ParseError(EUnknownNode, Child);
        end;
        Child:=Child.NextSibling;
      end;
    end
    else begin // extension attributes declaration parse
      if Child.ChildNodes.Count = 0 then
        Exit;

      Child:=Child.FirstChild;
      while Child <> Nil do begin
        if IsSame(Child.Name, 'attribute') then
          ParseAttribute(Child, Parent)
        else if IsSame(Child.Name, 'enumeration') then
          ParseEnumeration(Child, Parent)
        else if IsBuiltinAttr(Child.Name) then
          ParseBuiltinAttr(Child, Parent)
        else begin
          ParseError(EUnknownNode, Child);
        end;
        Child:=Child.NextSibling;
      end;
    end;
  end
  else begin
    ParseError(ERestrictionBaseTypeNeeded, Node);
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
      Child:=Child.FirstChild;
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
          ParseError(EUnknownNode, Child);
        end;
        Child:=Child.NextSibling;
      end;
    end
    else begin // extension attributes declaration parse
      if Child.ChildNodes.Count = 0 then
        Exit;

      Child:=Child.FirstChild;
      while Child <> Nil do begin
        if IsSame(Child.Name, 'attribute') then
          ParseAttribute(Child, Parent)
        else begin
          ParseError(EUnknownNode, Child);
        end;
        Child:=Child.NextSibling;
      end;
    end;
  end
  else begin
    ParseError(EExtensionBaseTypeNeeded, Node);
  end;
end;

procedure TXSD2XMLParser.ParseTypeReference(const Node: TXmlNode; var Parent: TXmlNode; const ReferenceName: String);

  function CheckCircularTypeReferenceResolved(const CheckNode: TXmlNode): Boolean;
  var
    refPrefix, refName: String;
    has_resolved: Boolean;
    resolved: Boolean;
  begin
    Result:=True;
    // detect circular type reference
    if Node.HasAttribute('base') then
      TXmlNode.GetNameAndPrefix(Node.Attributes['base'], refName, refPrefix);
    has_resolved:=CheckNode.HasAttribute('resolved');
    resolved:=False;
    if has_resolved then
      resolved:=(CheckNode.Attributes['resolved'] = 'true');
    if not has_resolved then
      Result:=False
    else if has_resolved and resolved and
            (Node.HasAttribute('name') and Node.HasAttribute('type') and
             (Parent.Name = Node.Attributes['name']) and Parent.HasAttribute('type') and (Parent.Attributes['type'] = Node.Attributes['type'])) or
            (Node.HasAttribute('base') and CheckNode.HasAttribute('name') and (refName = CheckNode.Attributes['name'])) then
      Result:=False
    else begin
      if Node.HasAttribute('minOccurs') and (Node.Attributes['minOccurs'] = '0') and
         Node.HasAttribute('maxOccurs') and (LowerCase(Node.Attributes['maxOccurs']) = 'unbounded') then
        Exit
      else begin
        ParseError(ECircularTypeReference, Node);
      end;
    end;
  end;

var
  refPrefix, refName, attrValue: String;
  i: Integer;
  SearchRoot, SearchNode, TypedNode: TXmlNode;
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

  i:=FParsedTypes.IndexOfName(ReferenceName);
  if i > -1 then begin
//    SearchNode:=TXmlNode(FParsedTypes.Objects[i, 1]);
//    if not CheckCircularTypeReferenceResolved(SearchNode) then begin
      SearchNode:=TXmlNode(FParsedTypes.Objects[i, 2]);
      Parent.AddNodes(SearchNode);
//    end;
  end
  else begin
    if SearchRoot = Nil then begin
      ParseError(EReferenceListEmpty, Node);
    end;

    SearchNode:=SearchRoot.FindNode{Recursive}('simpleType', 'name', refName, [ntElement], [nsSearchWithoutPrefix]);
    if SearchNode <> Nil then begin
      if not CheckCircularTypeReferenceResolved(SearchNode) then begin
        ParseSimpleType(SearchNode, Parent);
        TypedNode:=TXmlNode.Create(Parent);
        FParsedTypes.AddObject(ReferenceName, SearchNode, TypedNode);
      end;
    end
    else begin
      SearchNode:=SearchRoot.FindNode{Recursive}('complexType', 'name', refName, [ntElement], [nsSearchWithoutPrefix]);
      if SearchNode <> Nil then begin
        if not CheckCircularTypeReferenceResolved(SearchNode) then begin
          ParseComplexType(SearchNode, Parent);
          TypedNode:=TXmlNode.Create(Parent);
          FParsedTypes.AddObject(ReferenceName, SearchNode, TypedNode);
        end;
      end
      else begin
        ParseError(ETypeReferenceNotFound, Node);
      end;
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

procedure TXSD2XMLParser.ParseAttributeTypeReference(const Node: TXmlNode; var Parent: TXmlNode; const ReferenceName: String);

  function CheckCircularTypeReferenceResolved(const CheckNode: TXmlNode): Boolean;
  var
    refPrefix, refName: String;
    has_resolved: Boolean;
    resolved: Boolean;
  begin
    Result:=True;
    // detect circular type reference
    if Node.HasAttribute('base') then
      TXmlNode.GetNameAndPrefix(Node.Attributes['base'], refName, refPrefix);
    has_resolved:=CheckNode.HasAttribute('resolved');
    resolved:=False;
    if has_resolved then
      resolved:=(CheckNode.Attributes['resolved'] = 'true');
    if not has_resolved then
      Result:=False
    else if has_resolved and resolved and
            (Node.HasAttribute('name') and Node.HasAttribute('type') and
             (Parent.Name = Node.Attributes['name']) and Parent.HasAttribute('type') and (Parent.Attributes['type'] = Node.Attributes['type'])) or
            (Node.HasAttribute('base') and CheckNode.HasAttribute('name') and (refName = CheckNode.Attributes['name'])) then
      Result:=False
    else begin
      if Node.HasAttribute('minOccurs') and (Node.Attributes['minOccurs'] = '0') and
         Node.HasAttribute('maxOccurs') and (LowerCase(Node.Attributes['maxOccurs']) = 'unbounded') then
        Exit
      else begin
        ParseError(ECircularTypeReference, Node);
      end;
    end;
  end;

var
  refPrefix, refName, attrValue: String;
  i: Integer;
  SearchRoot, SearchNode, TypedNode: TXmlNode;
  attr: TXmlAttribute;
  XSDImport: TXSDImport;
begin
  TXmlNode.GetNameAndPrefix(ReferenceName, refName, refPrefix);

  if GetXSDBuiltinType(refName) <> etUnknown then begin
    ParseAttributeValue(Node, Parent, ReferenceName);
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

  i:=FParsedTypes.IndexOfName(ReferenceName);
  if i > -1 then begin
//    SearchNode:=TXmlNode(FParsedTypes.Objects[i, 1]);
//    if not CheckCircularTypeReferenceResolved(SearchNode) then begin
      SearchNode:=TXmlNode(FParsedTypes.Objects[i, 2]);
      Parent.AddNodes(SearchNode);
//    end;
  end
  else begin
    if SearchRoot = Nil then begin
      ParseError(EReferenceListEmpty, Node);
    end;

    SearchNode:=SearchRoot.FindNode{Recursive}('simpleType', 'name', refName, [ntElement], [nsSearchWithoutPrefix]);
    if SearchNode <> Nil then begin
      if not CheckCircularTypeReferenceResolved(SearchNode) then begin
        ParseSimpleType(SearchNode, Parent);
        TypedNode:=TXmlNode.Create(Parent);
        FParsedTypes.AddObject(ReferenceName, SearchNode, TypedNode);
      end;
    end
    else begin
      SearchNode:=SearchRoot.FindNode{Recursive}('complexType', 'name', refName, [ntElement], [nsSearchWithoutPrefix]);
      if SearchNode <> Nil then begin
        if not CheckCircularTypeReferenceResolved(SearchNode) then begin
          ParseComplexType(SearchNode, Parent);
          TypedNode:=TXmlNode.Create(Parent);
          FParsedTypes.AddObject(ReferenceName, SearchNode, TypedNode);
        end;
      end
      else begin
        ParseError(ETypeReferenceNotFound, Node);
      end;
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
    ParseError(EReferenceNotFound, Node);
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

procedure TXSD2XMLParser.ParseAttributeValue(const Node: TXmlNode; var Parent: TXmlNode; const ReferenceName: String);
var
  value, attr_name,{ attr_value, attr_prefix,} attr_use: String;
begin
  attr_name:=Node.Attributes['name'];
  attr_use:='';
  if Node.HasAttribute('use') then
    attr_use:=Node.Attributes['use'];
  //
//  TXmlNode.GetNameAndPrefix(ReferenceName, attr_value, attr_prefix);
  value:='';
  if IsFixed(Node, value) then begin
    if attr_use = 'required' then begin
      Parent.SetAttribute(attr_name, value);
    end
    else if attr_use = 'optional' then begin
      Parent.SetAttribute('@' + attr_name, value);
    end;
  end
  else if IsDefault(Node, value) then begin
    if attr_use = 'required' then begin
      Parent.SetAttribute(attr_name, value);
    end
    else if attr_use = 'optional' then begin
      Parent.SetAttribute('@' + attr_name, value);
    end;
  end
  else begin
    if attr_use = 'required' then begin
      Parent.SetAttribute(attr_name, '');
    end
    else if attr_use = 'optional' then begin
      Parent.SetAttribute('@' + attr_name, '');
    end;
  end;
end;

procedure TXSD2XMLParser.ParseAttribute(const Node: TXmlNode; var Parent: TXmlNode);
var
  attr_name,{ attr_value, attr_prefix,} attr_use: String;
  ReferenceName: String;
//  builtinType: TXSDElementType;
begin
  // check type of attribute
  attr_name:=Node.Attributes['name'];
  if not IsTypeDeclared(Node, ReferenceName) then begin
    if Node.HasAttribute('fixed') then
      Parent.SetAttribute(attr_name, Node.Attributes['fixed'])
    else if Node.HasAttribute('default') then
      Parent.SetAttribute(attr_name, Node.Attributes['default']);
  end
  else begin
    attr_use:='';
    if Node.HasAttribute('use') then
      attr_use:=Node.Attributes['use'];
    //TXmlNode.GetNameAndPrefix(ReferenceName, attr_value, attr_prefix);
    //Parent.SetAttribute('type', ifString(IsPascalType(attr_value), GetPascalType(attr_value), attr_value));
    if ReferenceName <> '' then begin // find type reference
      ParseAttributeTypeReference(Node, Parent, ReferenceName);
      Exit;
    end;
    if attr_use = 'required' then begin
//      Parent.SetAttribute(attr_use, ifString(IsPascalType(attr_value), GetPascalType(attr_value), attr_value));
      Parent.SetAttribute('required', 'true');
    end
    else if attr_use = 'optional' then begin
//      Parent.SetAttribute('@' + attr_name, ifString(IsPascalType(attr_value), GetPascalType(attr_value), attr_value));
      Parent.SetAttribute('required', 'optional');
    end;
  end;
end;

procedure TXSD2XMLParser.ParseAttributeGroup(const Node: TXmlNode; var Parent: TXmlNode);

  procedure ParseChildren;
  var
    Child: TXmlNode;
  begin
      Child:=Node;
      Child:=Child.FirstChild;
      while Child <> Nil do begin
        if IsSame(Child.Name, 'attribute') then
          ParseAttribute(Child, Parent)
        else begin
          ParseError(EUnknownNode, Child);
        end;
        Child:=Child.NextSibling;
      end;
  end;

var
  ReferenceName: String;
begin
  // check if it is referenced, if it is then skip
  if not HasReference(Node, rnAttributeGroup) then begin
    if IsReferenced(Node, ReferenceName) then begin // referenced type
      ParseAttributeGroupReference(Node, Parent, ReferenceName);
    end
    else begin
      ParseChildren;
    end;
  end
  else begin
    ParseChildren;
  end;
end;

procedure TXSD2XMLParser.ParseAttributeGroupReference(const Node: TXmlNode; var Parent: TXmlNode; const ReferenceName: String);
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

  SearchNode:=SearchRoot.FindNode{Recursive}('attributeGroup', 'name', refName, [ntElement], [nsSearchWithoutPrefix]);
  if SearchNode <> Nil then
    ParseAttributeGroup(SearchNode, Parent)
  else begin
    ParseError(EReferenceNotFound, Node);
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
  ParseError(ENodeNotSupported, Node);
end;

procedure TXSD2XMLParser.ParseUnion(const Node: TXmlNode; var Parent: TXmlNode);
var
  Child: TXmlNode;
begin
  Child:=Node;
  Child:=Child.FirstChild;
  while Child <> Nil do begin
    if IsSame(Child.Name, 'simpleType') then
      ParseSimpleType(Child, Parent)
    else begin
      ParseError(EUnknownNode, Child);
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
    Child:=Child.FirstChild;
    while Child <> Nil do begin
      if IsSame(Child.Name, 'element') then
        ParseElement(Child, Parent)
      else if IsSame(Child.Name, 'group') then
        ParseGroup(Child, Parent)
      else if IsSame(Child.Name, 'choice') then
        ParseChoice(Child, Parent)
      else if IsSame(Child.Name, 'sequence') then
        ParseSequence(Child, Parent)
      else if IsBuiltinSkipableElement(Child, True) then begin
        // Built in elements that can be skiped - do nothing
      end
      else if IsBuiltinAttr(Child.Name) then
        ParseBuiltinAttr(Child, Parent)
      else begin
        ParseError(EUnknownNode, Child);
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
  ParseError(ENodeNotSupported, Node);
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
  Child:=Child.FirstChild;
  while Child <> Nil do begin
    if IsSame(Child.Name, 'element') then
      ParseElement(Child, Parent)
    else if IsSame(Child.Name, 'group') then
      ParseGroup(Child, Parent)
    else if IsSame(Child.Name, 'choice') then
      ParseChoice(Child, Parent)
    else if IsSame(Child.Name, 'sequence') then
      ParseSequence(Child, Parent)
    else if IsBuiltinSkipableElement(Child, True) then begin
      // Built in elements that can be skiped - do nothing
    end
    else if IsBuiltinAttr(Child.Name) then
      ParseBuiltinAttr(Child, Parent)
    else begin
      ParseError(EUnknownNode, Child);
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

procedure TXSD2XMLParser.ParseError(const Message: String; const Node: TXmlNode);
begin
  raise EXSDParseException.CreateFmt(Message + #13#10'%s', [TNodeInfo.GetNodeInfo(Node)]);
end;

end.
