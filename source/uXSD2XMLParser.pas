{
  XSD to XML Parser v.23.0

  Reads XSD schema and outputs XML structure described by schema,
    contains only default or fixed values for nodes and attributes.

  Author:
    (C) 2015-2025, Grzegorz Molenda; gmnevton@o2.pl

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
    v.15 - 2018.12.19 - GM - fix, skip annotations from xml schema
    v.16 - 2019.01.17 - GM - do not skip 'annotation' for element nodes, just resolve them if xsdParseAnnotations is in Options; replace ChildNodes.First by FirstChild
    v.17 - 2019.02.15 - GM - speedup conversion; added custom THashedStringList to hold parsed types; some minor fixes with regard to speedup
         + 2019.02.27 - GM - fixed MakeXSDImport to properly load shemas from local hard drive
         + 2019.03.10 - GM - added SSL/TLS support for schema downloading rutine; forced HTTP request to retrun UTF-8 encoded data
    v.18 - 2019.05.13 - GM - added "attributeGroup" parsing; moved parsing procedures to protected section and virtualized them; changed ENodeException to EXSDParseException
         +            - GM - added EXSDImportException; added error description constant strings; changed GetPascalType return values to better matching Pascal language
         + 2019.06.07 - GM - added EXSD2XMLParserException parent class; added ParseAttributeTypeReference and ParseAttributeValue
         +            - GM - fixed THashedStringList.Delete to automatically free owned objects; fixed destructor TXSD2XMLParser.Destroy to free runtime created objects (patched memory leak)
         +            - GM - speedup by fixing function TXSD2XMLParser.HasReference to detect if type is declared; changed procedure TXSD2XMLParser.ParseAttribute
    v.19 - 2020.02.25 - GM - extend union parsing, refactoring to type recognition and parsing; added more checks for not resolved nodes
         + 2020.02.29 - GM - changed GetPascalType return values to match Pascal language; ParseTypeReference refactoring and speed up
         + 2020.03.2  - GM - changed parameter naming scheme from Node to xsdNode and Parent to xmlNode for better source readability
    v.20 - 2022.08.24 - GM - fixed type reference search,
         +            - GM - added function TXSDImportList.FindType
    v.21 - 2024.06.14 - GM - fix, skip comments from xml schema
    v.22 - 2024.11.28 - GM - changed GetPascalType return values to match shortened Pascal language types: string, boolean, integer, float, date, time, datetime
    v.23 - 2025.01.13 - GM - fixed infinite loop in TXSD2XMLParser.ParseTypeReference type searching
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
    class function AddQuotes(const AName: String): String;
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
    property Path: String read FPath;
  end;

  TXSDImportList = class(TObjectList<TXSDImport>)
  public
    [Weak] Parent: TXSD2XMLParser;
    function Add(const ANamespace: String): TXSDImport; overload; virtual;
    function Add(const ANamespace, ASchemaLocation: String): TXSDImport; overload; virtual;
    function Find(const ANamespace: String): TXSDImport; virtual;
    function FindType(const ATypeName: String): TXSDImport; virtual;
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
    function AddObject(const S: String; const AObject1, AObject2: TObject): Integer; overload;
    function AddObject(const S, Value: String; const AObject1, AObject2: TObject): Integer; overload;
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
    ETypeReferenceBaseTypeNotResolved = 'Type reference base type not resolved!';
    EAttributeTypeReferenceNotFound = 'Attribute type reference not found!';
    EAttributeTypeReferenceBaseTypeNotResolved = 'Attribute type reference base type not resolved!';
    EReferenceNotFound = 'Reference not found!';
    EUnionBaseTypeNeeded = 'Union must have base type!';
    EUnionResolvedBaseTypeMismatch = 'Union resolved base type mismatch!';
  protected
    function GetXMLString: String;
    function IsSame(const Value1, Value2: String): Boolean;
    function GetPrefix: String;

    function HasReference(const xsdNode: TXmlNode; const NodeType: TXSDReferencedNode): Boolean; virtual;

    function IsReferenced(const xsdNode: TXmlNode; var ReferenceName: String): Boolean; virtual;
    function IsSimpleType(const xsdNode: TXmlNode; out xsdChild: TXmlNode): Boolean; virtual;
    function IsComplexType(const xsdNode: TXmlNode; out xsdChild: TXmlNode): Boolean; virtual;
    function IsTypeDeclared(const xsdNode: TXmlNode; var TypeDeclared: String): Boolean; virtual;
    function IsAbstract(const xsdNode: TXmlNode): Boolean; virtual;
    function IsOptional(const xsdNode: TXmlNode): Boolean; virtual;
    function IsDefault(const xsdNode: TXmlNode; var DefaultValue: String): Boolean; virtual;
    function IsFixed(const xsdNode: TXmlNode; var FixedValue: String): Boolean; virtual;
    function IsSimpleContent(const xsdNode: TXmlNode): Boolean; virtual; // text only child elements
    function IsMixed(const xsdNode: TXmlNode): Boolean; virtual; // text and child elements inside
    function IsBuiltinSkipableElement(const xsdNode: TXmlNode; const IncludeExtended: Boolean = False): Boolean; virtual;
    class function IsBuiltinAttr(const AttributeName: String; const IncludeExtended: Boolean = False): Boolean; virtual;
    function IsPascalType(const TypeName: String): Boolean; virtual;

    function GetXSDBuiltinType(const ElementTypeName: String): TXSDElementType; virtual;
    function GetSimpleType(var xsdNode: TXmlNode): TXSDSimpleType; virtual;
    function GetComplexType(var xsdNode: TXmlNode): TXSDComplexType; virtual;
    function GetPascalType(const TypeName: String): String; virtual;
    function Fetch(var Value: String): String; virtual;

    procedure ParseSchemaAttributes(const xsdNode: TXmlNode); virtual;
    procedure ParseImport(const xsdNode: TXmlNode; var xmlNode: TXmlNode); virtual;
    procedure ParseElement(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String; const Recursive: Boolean = False; const Optional: Boolean = False); virtual;
    procedure ParseAnnotation(const xsdNode: TXmlNode; var xmlNode: TXmlNode); virtual;
    procedure ParseComment(const xsdNode: TXmlNode; var xmlNode: TXmlNode); virtual;
    procedure ParseSimpleType(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String); virtual;
    procedure ParseComplexType(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String); virtual;
    procedure ParseRestriction(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String); virtual;
    procedure ParseExtension(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String); virtual;
    procedure ParseTypeReference(const xsdNode: TXmlNode; var xmlNode: TXmlNode; const ReferenceName: String; var ResolvedBaseType: String); virtual;
    procedure ParseAttributeTypeReference(const xsdNode: TXmlNode; var xmlNode: TXmlNode; const ReferenceName: String; var ResolvedBaseType: String); virtual;
    procedure ParseReference(const xsdNode: TXmlNode; var xmlNode: TXmlNode; const ReferenceName: String; var ResolvedBaseType: String); virtual;
    procedure ParseValue(const xsdNode: TXmlNode; var xmlNode: TXmlNode); virtual;
    procedure ParseAttribute(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String); virtual;
    procedure ParseAttributeGroup(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String); virtual;
    procedure ParseAttributeGroupReference(const xsdNode: TXmlNode; var xmlNode: TXmlNode; const ReferenceName: String; var ResolvedBaseType: String); virtual;
    procedure ParseAttributeValue(const xsdNode: TXmlNode; var xmlNode: TXmlNode; const ReferenceName: String); virtual;
    procedure ParseEnumeration(const xsdNode: TXmlNode; var xmlNode: TXmlNode); virtual;
    procedure ParseList(const xsdNode: TXmlNode; var xmlNode: TXmlNode); virtual;
    procedure ParseUnion(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String); virtual;
    procedure ParseChoice(const xsdNode: TXmlNode; var xmlNode: TXmlNode); virtual;
    procedure ParseGroup(const xsdNode: TXmlNode; var xmlNode: TXmlNode); virtual;
    procedure ParseSequence(const xsdNode: TXmlNode; var xmlNode: TXmlNode); virtual;
    procedure ParseBuiltinAttr(const xsdNode: TXmlNode; var xmlNode: TXmlNode); virtual;
    procedure ParseError(const Message: String; const xsdNode: TXmlNode); virtual;

    procedure SetType(const xsdNode: TXmlNode; var xmlNode: TXmlNode; const ResolvedBaseType: String); virtual;

    function Add(const xsdNode, xmlNode: TXmlNode; const Name: String = ''; const CheckOptional: Boolean = True; const SetOptional: Boolean = False): TXmlNode;
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

class function TNodeInfo.AddQuotes(const AName: String): String;
begin
  Result:=AName;
  if Pos('.', Result) > 1 then
    Result:=AnsiQuotedStr(Result, '"');
end;

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
      if (LowerCase(Child.Name) <> 'choice') and (LowerCase(Child.Name) <> 'optional') and (LowerCase(Child.Name) <> 'sequence') then begin
        hasMaxOccurs:=Child.HasAttribute('maxOccurs');
        if not hasMaxOccurs then
          Result:=AddQuotes(Child.NameWithPrefix) + '.' + Result
        else begin
          maxOccurs:=LowerCase(AnsiDequotedStr(Child.Attributes['maxOccurs'], '"'));
          Result:=IfThen((maxOccurs = 'unbounded') or (StrToIntDef(maxOccurs, 1) > 1), '#' + AddQuotes(Child.NameWithPrefix), AddQuotes(Child.NameWithPrefix)) + '.' + Result;
        end;
      end;
      Child:=Child.ParentNode;
    end;
    Result:='.' + Result;
    if (LowerCase(Node.Name) <> 'choice') and (LowerCase(Node.Name) <> 'optional') and (LowerCase(Child.Name) <> 'sequence') then begin
      hasMaxOccurs:=Node.HasAttribute('maxOccurs');
      if not hasMaxOccurs then
        Result:=Result + AddQuotes(Node.NameWithPrefix)
      else begin
        maxOccurs:=LowerCase(AnsiDequotedStr(Node.Attributes['maxOccurs'], '"'));
        Result:=Result + IfThen((maxOccurs = 'unbounded') or (StrToIntDef(maxOccurs, 1) > 1), '#' + AddQuotes(Node.NameWithPrefix), AddQuotes(Node.NameWithPrefix));
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
  if FImport.DocumentElement = Nil then
    Exit;
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

function TXSDImportList.FindType(const ATypeName: String): TXSDImport;
var
  XSD, Import: TXSDImport;
  XML: TXmlVerySimple;
  Root, Node: TXmlNode;
  Options: TXmlOptions;
begin
  Result := Nil;
  for XSD in Self do begin
    XML:=XSD.Import;
    Options:=XML.Options;
    try
      XML.Options:=XML.Options + [doSearchExcludeNamespacePrefix];
      Root:=XML.DocumentElement;
      if Root <> Nil then begin
        Node:=Root.FindNode('simpleType', 'name', ATypeName);
        if Node <> Nil then begin
          Result:=XSD;
          Break;
        end
        else begin
          Node:=Root.FindNode('complexType', 'name', ATypeName);
          if Node <> Nil then begin
            Result:=XSD;
            Break;
          end;
        end;
      end;
    finally
      XML.Options:=Options;
    end;
  end;
  if Result <> Nil then
    Exit;
  // search recursive
  for XSD in Self do begin
    if XSD.XSDImports.Count > 0 then begin
      for Import in XSD.XSDImports do begin
        XML:=Import.Import;
        Options:=XML.Options;
        try
          XML.Options:=XML.Options + [doSearchExcludeNamespacePrefix];
          Root:=XML.DocumentElement;
          if Root <> Nil then begin
            Node:=Root.FindNode('simpleType', 'name', ATypeName);
            if Node <> Nil then begin
              Result:=Import;
              Exit;
            end
            else begin
              Node:=Root.FindNode('complexType', 'name', ATypeName);
              if Node <> Nil then begin
                Result:=Import;
                Exit;
              end;
            end;
          end;
        finally
          XML.Options:=Options;
        end;
      end;
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

function THashedStringList.AddObject(const S, Value: String; const AObject1, AObject2: TObject): Integer;
var
  Data: PStringRec;
begin
  Result:=Add(S, Value);
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

function TXSD2XMLParser.HasReference(const xsdNode: TXmlNode; const NodeType: TXSDReferencedNode): Boolean;
var
  refName, typeDecl: String;
  n: TXmlNode;
begin
  Result:=False;

  if IsReferenced(xsdNode, refName) or IsTypeDeclared(xsdNode, typeDecl) then
    Exit;

  if xsdNode.HasAttribute('name') then
    refName:=xsdNode.Attributes['name']
  else begin
    ParseError(ENodeWithoutName, xsdNode);
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

function TXSD2XMLParser.IsReferenced(const xsdNode: TXmlNode; var ReferenceName: String): Boolean;
begin
  ReferenceName:='';
  Result:=xsdNode.HasAttribute('ref');
  if Result then
    ReferenceName:=xsdNode.Attributes['ref'];
end;

function TXSD2XMLParser.IsSimpleType(const xsdNode: TXmlNode; out xsdChild: TXmlNode): Boolean;
var
  TempNode: TXmlNode;
begin
  Result:=False;
  xsdChild:=xsdNode.FindNode('simpleType', [ntElement], [nsSearchWithoutPrefix]);
  if (xsdChild <> Nil) or (not xsdNode.HasAttribute('type') and not IsComplexType(xsdNode, TempNode)) then
    Result:=True;
end;

function TXSD2XMLParser.IsComplexType(const xsdNode: TXmlNode; out xsdChild: TXmlNode): Boolean;
begin
  xsdChild:=xsdNode.FindNode('complexType', [ntElement], [nsSearchWithoutPrefix]);
  Result:=(xsdChild <> Nil);
end;

function TXSD2XMLParser.IsTypeDeclared(const xsdNode: TXmlNode; var TypeDeclared: String): Boolean;
begin
  TypeDeclared:='';
  Result:=xsdNode.HasAttribute('type');
  if Result then
    TypeDeclared:=xsdNode.Attributes['type'];
end;

function TXSD2XMLParser.IsAbstract(const xsdNode: TXmlNode): Boolean;
begin
  Result:=(xsdNode.HasAttribute('abstract') and (LowerCase(xsdNode.Attributes['abstract']) = 'true'));
end;

function TXSD2XMLParser.IsOptional(const xsdNode: TXmlNode): Boolean;
begin
  Result:=xsdNode.HasAttribute('minOccurs');
  if Result then
    Result:=(xsdNode.Attributes['minOccurs'] = '0');
end;

function TXSD2XMLParser.IsDefault(const xsdNode: TXmlNode; var DefaultValue: String): Boolean;
begin
  DefaultValue:='';
  Result:=xsdNode.HasAttribute('default');
  if Result then
    DefaultValue:=xsdNode.Attributes['default'];
end;

function TXSD2XMLParser.IsFixed(const xsdNode: TXmlNode; var FixedValue: String): Boolean;
begin
  FixedValue:='';
  Result:=xsdNode.HasAttribute('fixed');
  if Result then
    FixedValue:=xsdNode.Attributes['fixed'];
end;

function TXSD2XMLParser.IsSimpleContent(const xsdNode: TXmlNode): Boolean;
begin
  Result:=(xsdNode.FindNode('simpleContent', [ntElement], [nsSearchWithoutPrefix]) <> Nil);
end;

function TXSD2XMLParser.IsMixed(const xsdNode: TXmlNode): Boolean;
begin
  Result:=(xsdNode.HasAttribute('mixed') and (LowerCase(xsdNode.Attributes['mixed']) = 'true'));
end;

function TXSD2XMLParser.IsBuiltinSkipableElement(const xsdNode: TXmlNode; const IncludeExtended: Boolean = False): Boolean;
var
  ElementName: String;
begin
  Result:=False;
  if (xsdNode.Name = '') and (xsdNode.NodeType <> ntElement) then begin
    Result:=True;
    Exit;
  end
  else begin
    ElementName:=xsdNode.Name;
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

function TXSD2XMLParser.GetSimpleType(var xsdNode: TXmlNode): TXSDSimpleType;
var
  xsdChild: TXmlNode;
begin
  Result:=stUnknown;
  xsdChild:=xsdNode.FirstChild;
  if xsdChild = Nil then
    Exit;

  if LowerCase(xsdChild.Name) = 'annotation' then // skip this element
    xsdChild:=xsdChild.NextSibling;

  if xsdChild <> Nil then
    xsdNode:=xsdChild
  else
    Exit;

  if LowerCase(xsdChild.Name) = 'restriction' then
    Result:=stRestriction
  else if LowerCase(xsdChild.Name) = 'list' then
    Result:=stList
  else if LowerCase(xsdChild.Name) = 'union' then
    Result:=stUnion;
end;

function TXSD2XMLParser.GetComplexType(var xsdNode: TXmlNode): TXSDComplexType;
var
  xsdChild: TXmlNode;
begin
  Result:=ctUnknown;
  if xsdNode.ChildNodes.Count = 0 then
    Exit;

  xsdChild:=xsdNode.FirstChild;
  if xsdChild = Nil then
    Exit;

  if LowerCase(xsdChild.Name) = 'annotation' then // skip this element
    xsdChild:=xsdChild.NextSibling;

  if xsdChild <> Nil then
    xsdNode:=xsdChild
  else
    Exit;

  if LowerCase(xsdChild.Name) = 'all' then
    Result:=ctAll
  else if LowerCase(xsdChild.Name) = 'choice' then
    Result:=ctChoice
  else if LowerCase(xsdChild.Name) = 'sequence' then
    Result:=ctSequence
  else if LowerCase(xsdChild.Name) = 'simplecontent' then
    Result:=ctSimpleContent
  else if LowerCase(xsdChild.Name) = 'complexcontent' then
    Result:=ctComplexContent
  else if LowerCase(xsdChild.Name) = 'element' then begin
    Result:=ctElement;
    xsdNode:=xsdChild.ParentNode; // go back one level
  end;

  if Result = ctUnknown then
    xsdNode:=xsdNode.ParentNode;
end;

function TXSD2XMLParser.GetPascalType(const TypeName: String): String;
var
  et: TXSDElementType;
begin
  et:=GetXSDBuiltinType(TypeName);
  case et of
    etUnknown: Result:='';
    etToken: Result:='string';
    etString: Result:='string';
    etFloat: Result:='float';
    etDecimal: Result:='integer';
    etInteger: Result:='integer';
    etByte: Result:='integer';
    etInt: Result:='integer';
    etLong: Result:='integer';
    etNegativeInteger: Result:='integer';
    etNonNegativeInteger: Result:='integer';
    etNonPositiveInteger: Result:='integer';
    etPositiveInteger: Result:='integer';
    etShort: Result:='integer';
    etUnsignedLong: Result:='integer';
    etUnsignedInt: Result:='integer';
    etUnsignedShort: Result:='integer';
    etUnsignedByte: Result:='integer';
    etBoolean: Result:='boolean';
    etDate: Result:='date';
    etTime: Result:='time';
    etDateTime: Result:='datetime';
    etgDay: Result:='integer';
    etgMonth: Result:='integer';
    etgMonthDay: Result:='string';
    etgYear: Result:='integer';
    etgYearMonth: Result:='string';
    etNormalizedString: Result:='string';
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

function TXSD2XMLParser.Fetch(var Value: String): String;
var
  i: Integer;
begin
  Result:=Value;
  i:=Pos(' ', Value);
  if i > 0 then begin
    Delete(Result, i, MaxInt);
    Delete(Value, 1, i);
  end
  else
    Value:='';
end;

function TXSD2XMLParser.Add(const xsdNode, xmlNode: TXmlNode; const Name: String = ''; const CheckOptional: Boolean = True; const SetOptional: Boolean = False): TXmlNode;
var
  node_name: String;
  Attr: TXmlAttribute;
  idx: Integer;
begin
  if Name = '' then
    node_name:=xsdNode.Attributes['name']
  else
    node_name:=Name;

  if FIncludePrefix then begin
    idx:=High(FIncludePrefixString);
    node_name:=ifString(FIncludePrefixString[idx] <> '', FIncludePrefixString[idx] + ':' + node_name, node_name);
  end;

  if xmlNode = Nil then begin
    Result:=FXML.AddChild(node_name);
    if (FSchemaTargetNamespace <> '') and (FSchemaTargetNamespace <> FSchemaNamespace) then
      Result.SetAttribute('xmlns', FSchemaTargetNamespace)
    else if FSchemaNamespace <> '' then
      Result.SetAttribute('xmlns', FSchemaNamespace);
  end
  else
    Result:=xmlNode.AddChild(node_name);

  if (CheckOptional and IsOptional(xsdNode)) or SetOptional then
    Result.SetAttribute('optional', 'true');

  for Attr in xsdNode.AttributeList do
    if LowerCase(Attr.Name) <> 'name' then
      Result.SetAttribute(Attr.Name, Attr.Value);
end;

class function TXSD2XMLParser.LoadFromFile(const FileName: String; const BufferSize: Integer): TXSD2XMLParser;
var
  i: Integer;
begin
  Result:=TXSD2XMLParser.Create;
  Result.FXSD.LoadFromFile(FileName, BufferSize);
  Result.FAbsolutePath:=ExtractFilePath(FileName);
  Result.Parse;
  if FindCmdLineSwitch('DEBUG', ['-', '/'], true) then begin
    Result.FXSD.SaveToFile('parser_schema.xsd');
    with TStreamWriter.Create('parser_types.txt', False, TEncoding.UTF8) do try
      for i:=0 to Result.FParsedTypes.Count - 1 do begin
        WriteLine('TYPE: ' + Result.FParsedTypes.Names[i]);
        WriteLine('schema declaration:');
        WriteLine('-------------------');
        WriteLine(TXmlNode(Result.FParsedTypes.Objects[i, 1]).AsString);
        WriteLine('parsed result:');
        WriteLine('--------------');
        WriteLine(TXmlNode(Result.FParsedTypes.Objects[i, 2]).AsString);
        WriteLine;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TXSD2XMLParser.Clear;
var
  i: Integer;
begin
  FXML.Clear;
  if FLastXSDImportsSearchRoot <> Nil then
    FreeAndNil(FLastXSDImportsSearchRoot);
  for i:=FParsedTypes.Count - 1 downto 0 do begin
    FParsedTypes.Objects[i, 2].Free;
    FParsedTypes.Objects[i, 2]:=Nil;
  end;
  FParsedTypes.Clear;
  FSchemaPrefix:='';
  FSchemaNamespace:='';
  FSchemaTargetNamespace:='';
  FIncludePrefix:=False;
  SetLength(FIncludePrefixString, 0);
  array_insert('', FIncludePrefixString);
end;

procedure TXSD2XMLParser.Parse;
var
  xmlNode, xsdNode: TXmlNode;
  ResolvedBaseType: String;
begin
  Clear;

  xmlNode:=Nil; // the FXML node
  xsdNode:=FXSD.DocumentElement;
  if xsdNode = Nil then begin
    ParseError(ESchemaNotLoaded, Nil);
  end;

  ParseSchemaAttributes(xsdNode);

  while xsdNode <> Nil do begin
    if IsSame(xsdNode.Name, 'schema') then begin
      xsdNode:=xsdNode.FirstChild;
    end;

    if IsSame(xsdNode.Name, 'import') and (xsdParseImportInstr in Options) then
      ParseImport(xsdNode, xmlNode)
//    else if IsSame(xsdNode.Name, 'complexType') then
//      ParseComplexType(xsdNode, xmlNode)
//    else if IsSame(xsdNode.Name, 'simpleType') then
//      ParseSimpleType(xsdNode, xmlNode)
    else if IsSame(xsdNode.Name, 'element') then begin
      ResolvedBaseType:=''; // just for clarity
      ParseElement(xsdNode, xmlNode, ResolvedBaseType);
      ResolvedBaseType:='';
    end;

    xsdNode:=xsdNode.NextSibling;
  end;
end;

procedure TXSD2XMLParser.ParseSchemaAttributes(const xsdNode: TXmlNode);
var
  name, prefix, temp: String;
  attr: TXmlAttribute;
begin
  TXmlNode.GetNameAndPrefix(xsdNode.NameWithPrefix, temp, FSchemaPrefix);
  if xsdNode.AttributeList.Count > 0 then begin
    for attr in xsdNode.AttributeList do begin
      TXmlNode.GetNameAndPrefix(attr.Name, name, prefix);
      if (LowerCase(name) = 'xmlns') and (prefix = '') then
        FSchemaNamespace:=attr.Value
      else if (LowerCase(name) = 'targetnamespace') and (prefix = '') then
        FSchemaTargetNamespace:=attr.Value;
    end;
  end;
end;

procedure TXSD2XMLParser.ParseImport(const xsdNode: TXmlNode; var xmlNode: TXmlNode);
var
  Import: TXSDImport;
begin
  Import:=MakeXSDImport(xsdNode, FAbsolutePath);
  if Import <> Nil then
    FXSDImports.Add(Import);
end;

procedure TXSD2XMLParser.ParseElement(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String; const Recursive: Boolean = False; const Optional: Boolean = False);
var
  xsdChild, simple_xsdChild, complex_xsdChild, TempNode: TXmlNode;
  ReferenceName, NodeType: String;
  simpleType, complexType: Boolean;
begin
  // check if it is referenced, if it is then skip
  if Recursive or not HasReference(xsdNode, rnElement) then begin
    if IsReferenced(xsdNode, ReferenceName) then begin // referenced type
      ParseReference(xsdNode, xmlNode, ReferenceName, ResolvedBaseType);
    end
    else begin
      if xsdNode.HasChildNodes then begin
        xsdChild:=xsdNode.FirstChild;
        if (xsdChild <> Nil) and (xsdParseAnnotations in Options) and IsSame(xsdChild.Name, 'annotation') then begin
          TempNode:=xsdNode;
          ParseAnnotation(xsdChild, TempNode);
          TempNode:=Nil;
        end;
//        else if (xsdChild <> Nil) and not (xsdParseAnnotations in Options) and IsSame(xsdChild.Name, 'annotation') then begin // maybe we need to do something with it ???
//        end;
      end;
      //
      // we have 2 options here:
      // 1. xsd:element has name="xxx" and type="yyy" declared
      // 2. xsd:simpleType or xsd:complexType is declared as child node in xsd:element
      if IsTypeDeclared(xsdNode, NodeType) then begin // check if it is referenced by type declaration
        xmlNode:=Add(xsdNode, xmlNode, '', True, Optional);
        ParseTypeReference(xsdNode, xmlNode, NodeType, ResolvedBaseType);
        xmlNode:=xmlNode.ParentNode;
      end
      else begin
        simpleType:=IsSimpleType(xsdNode, simple_xsdChild);
        complexType:=IsComplexType(xsdNode, complex_xsdChild);
        //
        if not simpleType and not complexType then begin
          ParseError(EUnknownNode, xsdNode);
        end;
        //
        xmlNode:=Add(xsdNode, xmlNode, '', True, Optional);
        if simpleType then begin
          if simple_xsdChild = Nil then
            simple_xsdChild:=xsdNode;
          ResolvedBaseType:='';
          ParseSimpleType(simple_xsdChild, xmlNode, ResolvedBaseType);
          SetType(simple_xsdChild, xmlNode, ResolvedBaseType);
        end
        else if complexType then begin
          if complex_xsdChild <> Nil then begin
            ResolvedBaseType:='';
            ParseComplexType(complex_xsdChild, xmlNode, ResolvedBaseType);
            SetType(complex_xsdChild, xmlNode, ResolvedBaseType);
          end;
        end;
        xmlNode:=xmlNode.ParentNode;
      end;
    end;
  end;
end;

procedure TXSD2XMLParser.ParseAnnotation(const xsdNode: TXmlNode; var xmlNode: TXmlNode);
var
  xsdChild, TempNode: TXmlNode;
  temp: String;
begin
  xsdChild:=xsdNode.FirstChild;
  while xsdChild <> Nil do begin
    if IsSame(xsdChild.Name, 'documentation') and (xsdChild.NodeValue <> '') then begin
      temp:=xsdChild.NodeValue;
      temp:=ReplaceStr(temp, #13, '');
      temp:=ReplaceStr(temp, #10, '|');
      xmlNode.Attributes['info']:=temp;
      temp:='';
    end
    else if xsdChild.NodeType = ntComment then begin
      TempNode:=xsdNode;
      ParseComment(xsdChild, TempNode);
      TempNode:=Nil;
    end
    else begin
      ParseError(EUnknownNode, xsdChild);
    end;
    xsdChild:=xsdChild.NextSibling;
  end;
end;

procedure TXSD2XMLParser.ParseComment(const xsdNode: TXmlNode; var xmlNode: TXmlNode);
begin
// nothing to do here
end;

procedure TXSD2XMLParser.ParseSimpleType(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String);
var
//  simple_type: TXSDSimpleType;
  xsdChild: TXmlNode;
begin
  xsdChild:=xsdNode;
//  simple_type:=GetSimpleType(xsdChild);
//  if simple_type = stUnknown then begin
//    raise ENodeException.CreateFmt('Unknown node or child nodes not found!'#13#10'%s', [GetNodeInfo(xsdNode)]);
//  end;

  xsdNode.Attributes['resolved']:='false';

  xsdChild:=xsdChild.FirstChild;
  while xsdChild <> Nil do begin
    if IsSame(xsdChild.Name, 'restriction') then
      ParseRestriction(xsdChild, xmlNode, ResolvedBaseType)
//    else if IsSame(xsdChild.Name, 'list') then
//      ParseList(xsdChild, xmlNode)
    else if IsSame(xsdChild.Name, 'union') then
      ParseUnion(xsdChild, xmlNode, ResolvedBaseType)
    else if IsBuiltinSkipableElement(xsdChild, True) then begin
      // Built in elements that can be skiped - do nothing
    end
    else begin
      ParseError(EUnknownNode, xsdChild);
    end;
    xsdChild:=xsdChild.NextSibling;
  end;

  xsdNode.Attributes['resolved']:='true';
end;

procedure TXSD2XMLParser.ParseComplexType(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String);
var
//  complex_type: TXSDComplexType;
  xsdChild: TXmlNode;
//  InternalNode: Boolean;
begin
  xsdChild:=xsdNode;
{  complex_type:=GetComplexType(xsdChild);
  if complex_type = ctUnknown then begin
    raise ENodeException.CreateFmt('Unknown node or child nodes not found!'#13#10'%s', [TNodeInfo.GetNodeInfo(xsdNode)]);
  end;

  InternalNode:=False;
  if complex_type = ctChoice then begin
    Parent:=Add(xsdNode, xmlNode, 'Choice');
    InternalNode:=True;
  end;}
//  try
//    xsdChild:=xsdNode;

  xsdNode.Attributes['resolved']:='false';

    xsdChild:=xsdChild.FirstChild;
    while xsdChild <> Nil do begin
      if IsSame(xsdChild.Name, 'element') then
        ParseElement(xsdChild, xmlNode, ResolvedBaseType)
      else if IsSame(xsdChild.Name, 'extension') then
        ParseExtension(xsdChild, xmlNode, ResolvedBaseType)
      else if IsSame(xsdChild.Name, 'choice') then
        ParseChoice(xsdChild, xmlNode)
      else if IsSame(xsdChild.Name, 'sequence') then
        ParseSequence(xsdChild, xmlNode)
      else if IsSame(xsdChild.Name, 'simpleContent') then
        ParseComplexType(xsdChild, xmlNode, ResolvedBaseType)
      else if IsSame(xsdChild.Name, 'complexContent') then
        ParseComplexType(xsdChild, xmlNode, ResolvedBaseType)
      else if IsSame(xsdChild.Name, 'attribute') then
        ParseAttribute(xsdChild, xmlNode, ResolvedBaseType)
      else if IsSame(xsdChild.Name, 'attributeGroup') then
        ParseAttributeGroup(xsdChild, xmlNode, ResolvedBaseType)
      else if IsBuiltinSkipableElement(xsdChild, True) then begin
        // Built in elements that can be skiped - do nothing
      end
      else begin
        ParseError(EUnknownNode, xsdChild);
      end;
      xsdChild:=xsdChild.NextSibling;
    end;
{  finally
    if InternalNode then
      xmlNode:=xmlNode.ParentNode;
  end;}

  xsdNode.Attributes['resolved']:='true';
end;

procedure TXSD2XMLParser.ParseRestriction(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String);
var
  base: String;
  xsdChild: TXmlNode;
  complex_type: TXSDComplexType;
  refPrefix, refName: String;
begin
  if (xsdNode.AttributeList.Count = 0) and (xsdNode.ChildNodes.Count > 0) then begin // restriction can have simpleType
    xsdChild:=xsdNode.FirstChild;
    while xsdChild <> Nil do begin
      if IsSame(xsdChild.Name, 'simpleType') then
        ParseSimpleType(xsdChild, xmlNode, ResolvedBaseType)
      else if IsBuiltinAttr(xsdChild.Name) then
        ParseBuiltinAttr(xsdChild, xmlNode)
      else begin
        ParseError(EUnknownNode, xsdChild);
      end;
      xsdChild:=xsdChild.NextSibling;
    end;
  end
  else begin
    base:='';
    if xsdNode.HasAttribute('base') then
      base:=xsdNode.Attributes['base'];

    if base <> '' then begin
      TXmlNode.GetNameAndPrefix(base, refName, refPrefix);
      if GetXSDBuiltinType(refName) <> etUnknown then begin
        ResolvedBaseType:=refName;
      end
      else
        ParseTypeReference(xsdNode, xmlNode, base, ResolvedBaseType);
      xsdChild:=xsdNode;
      complex_type:=GetComplexType(xsdChild);
      if complex_type <> ctUnknown then begin // extension complex type parse
        xsdChild:=xsdNode;
        xsdChild:=xsdChild.FirstChild;
        while xsdChild <> Nil do begin
  //        if IsSame(xsdChild.Name, 'all') then
  //          ParseAttribute(xsdChild, xmlNode)
          if IsSame(xsdChild.Name, 'choice') then
            ParseChoice(xsdChild, xmlNode)
          else if IsSame(xsdChild.Name, 'sequence') then
            ParseSequence(xsdChild, xmlNode)
          else if IsSame(xsdChild.Name, 'group') then
            ParseGroup(xsdChild, xmlNode)
  //        else if IsSame(xsdChild.Name, 'complexType') then
  //          ParseComplexType(xsdChild, xmlNode)
          else if IsSame(xsdChild.Name, 'attribute') then
            ParseAttribute(xsdChild, xmlNode, ResolvedBaseType)
          else begin
            ParseError(EUnknownNode, xsdChild);
          end;
          xsdChild:=xsdChild.NextSibling;
        end;
      end
      else begin // extension attributes declaration parse
        if xsdChild.ChildNodes.Count = 0 then
          Exit;

        xsdChild:=xsdChild.FirstChild;
        while xsdChild <> Nil do begin
          if IsSame(xsdChild.Name, 'attribute') then
            ParseAttribute(xsdChild, xmlNode, ResolvedBaseType)
          else if IsSame(xsdChild.Name, 'enumeration') then
            ParseEnumeration(xsdChild, xmlNode)
          else if IsBuiltinAttr(xsdChild.Name) then
            ParseBuiltinAttr(xsdChild, xmlNode)
          else begin
            ParseError(EUnknownNode, xsdChild);
          end;
          xsdChild:=xsdChild.NextSibling;
        end;
      end;
    end
    else begin
      ParseError(ERestrictionBaseTypeNeeded, xsdNode);
    end;
  end;
end;

procedure TXSD2XMLParser.ParseExtension(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String);
var
  base: String;
  xsdChild: TXmlNode;
  complex_type: TXSDComplexType;
  refPrefix, refName: String;
begin
  base:='';
  if xsdNode.HasAttribute('base') then
    base:=xsdNode.Attributes['base'];

  if base <> '' then begin
    TXmlNode.GetNameAndPrefix(base, refName, refPrefix);
    if GetXSDBuiltinType(refName) <> etUnknown then begin
      ResolvedBaseType:=refName;
    end
    else
      ParseTypeReference(xsdNode, xmlNode, base, ResolvedBaseType);
    xsdChild:=xsdNode;
    complex_type:=GetComplexType(xsdChild);
    if complex_type <> ctUnknown then begin // extension complex type parse
      xsdChild:=xsdNode;
      xsdChild:=xsdChild.FirstChild;
      while xsdChild <> Nil do begin
//        if IsSame(xsdChild.Name, 'all') then
//          ParseAttribute(xsdChild, xmlNode)
        if IsSame(xsdChild.Name, 'choice') then
          ParseChoice(xsdChild, xmlNode)
        else if IsSame(xsdChild.Name, 'sequence') then
          ParseSequence(xsdChild, xmlNode)
        else if IsSame(xsdChild.Name, 'group') then
          ParseGroup(xsdChild, xmlNode)
//        else if IsSame(xsdChild.Name, 'complexType') then
//          ParseComplexType(xsdChild, xmlNode)
        else if IsSame(xsdChild.Name, 'attribute') then
          ParseAttribute(xsdChild, xmlNode, ResolvedBaseType)
        else begin
          ParseError(EUnknownNode, xsdChild);
        end;
        xsdChild:=xsdChild.NextSibling;
      end;
    end
    else begin // extension attributes declaration parse
      if xsdChild.ChildNodes.Count = 0 then
        Exit;

      xsdChild:=xsdChild.FirstChild;
      while xsdChild <> Nil do begin
        if IsSame(xsdChild.Name, 'attribute') then
          ParseAttribute(xsdChild, xmlNode, ResolvedBaseType)
        else begin
          ParseError(EUnknownNode, xsdChild);
        end;
        xsdChild:=xsdChild.NextSibling;
      end;
    end;
  end
  else begin
    ParseError(EExtensionBaseTypeNeeded, xsdNode);
  end;
end;

procedure TXSD2XMLParser.ParseTypeReference(const xsdNode: TXmlNode; var xmlNode: TXmlNode; const ReferenceName: String; var ResolvedBaseType: String);

  function CheckCircularTypeReferenceResolved(const CheckNode: TXmlNode): Boolean;
  var
    refPrefix, refName: String;
    has_resolved: Boolean;
    resolved: Boolean;
  begin
    Result:=True;
    // detect circular type reference
    if xsdNode.HasAttribute('base') then
      TXmlNode.GetNameAndPrefix(xsdNode.Attributes['base'], refName, refPrefix);
    has_resolved:=CheckNode.HasAttribute('resolved');
    resolved:=False;
    if has_resolved then
      resolved:=(CheckNode.Attributes['resolved'] = 'true');
    if not has_resolved then
      Result:=False
    else if has_resolved and resolved and
            (xsdNode.HasAttribute('name') and xsdNode.HasAttribute('type') and
             (xmlNode.Name = xsdNode.Attributes['name']) and xmlNode.HasAttribute('type') and (xmlNode.Attributes['type'] = xsdNode.Attributes['type'])) or
            (xsdNode.HasAttribute('base') and CheckNode.HasAttribute('name') and (refName = CheckNode.Attributes['name'])) then
      Result:=False
    else begin
      if xsdNode.HasAttribute('minOccurs') and (xsdNode.Attributes['minOccurs'] = '0') and
         xsdNode.HasAttribute('maxOccurs') and (LowerCase(xsdNode.Attributes['maxOccurs']) = 'unbounded') then
        Exit
      else begin
        ParseError(ECircularTypeReference, xsdNode);
      end;
    end;
  end;

var
  refPrefix, refName, attrValue: String;
  i, idx: Integer;
  SearchRoot, SearchNode, TypedNode: TXmlNode;
  attr: TXmlAttribute;
  XSDImport: TXSDImport;
  importFound: Boolean;
  removeType: Boolean;
begin
  TXmlNode.GetNameAndPrefix(ReferenceName, refName, refPrefix);

  if GetXSDBuiltinType(refName) <> etUnknown then begin
    ParseValue(xsdNode, xmlNode);
    Exit;
  end;

  i:=FParsedTypes.IndexOfName(ReferenceName);
  if i > -1 then begin
//    SearchNode:=TXmlNode(FParsedTypes.Objects[i, 1]);
//    if not CheckCircularTypeReferenceResolved(SearchNode) then begin
      SearchNode:=TXmlNode(FParsedTypes.Objects[i, 2]);
      xmlNode.AssignAttributes(SearchNode, True);
      xmlNode.AddNodes(SearchNode);
      SetType(xsdNode, xmlNode, FParsedTypes.Items[i].Value.Value);
      ResolvedBaseType:=FParsedTypes.Items[i].Value.Value;
//    end;
  end
  else begin
    if FLastXSDImportsSearchRoot = Nil then begin
      FLastXSDImportsSearchRoot:=TXSDImportsSearchRootList.Create(False);
      FLastXSDImportsSearchRoot.Add(FXSD.DocumentElement);
    end;

    importFound:=False;
    removeType:=False;
    SearchRoot:=FLastXSDImportsSearchRoot.Last;
    idx:=FLastXSDImportsSearchRoot.Count - 1;
    if SearchRoot = Nil then begin
      ParseError(EReferenceListEmpty, xsdNode);
    end;

    while SearchRoot <> Nil do begin
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
              removeType:=True;

              if FLastXSDImportsSearchRoot.Count > 1 then begin
                FIncludePrefix:=True;
                array_insert(refPrefix, FIncludePrefixString);
              end;
            end;
          end;
        end;
      end
      else begin
        XSDImport:=FXSDImports.FindType(refName);
        if (XSDImport <> Nil) and (XSDImport.Import <> Nil) and (XSDImport.Import.DocumentElement <> Nil) then begin
//          if FXML.DocumentElement <> Nil then
//            FXML.DocumentElement.SetAttribute(attr.Name, attr.Value);
          SearchRoot:=XSDImport.Import.DocumentElement;
          removeType:=False;
        end;
      end;

      SearchNode:=SearchRoot.FindNode{Recursive}('simpleType', 'name', refName, [ntElement], [nsSearchWithoutPrefix]);
      if SearchNode <> Nil then begin
        if not CheckCircularTypeReferenceResolved(SearchNode) then begin
          ParseSimpleType(SearchNode, xmlNode, ResolvedBaseType);
          if (Length(ReferenceName) = 0) and (Length(ResolvedBaseType) = 0) then
            ParseError(ETypeReferenceBaseTypeNotResolved, xsdNode);
          if (Length(ReferenceName) > 0) and (Length(ResolvedBaseType) = 0) then
            ResolvedBaseType:=ReferenceName;
          TypedNode:=TXmlNode.Create(xmlNode);
          FParsedTypes.AddObject(ReferenceName, ResolvedBaseType, SearchNode, TypedNode);
          SetType(xsdNode, xmlNode, ResolvedBaseType);
          importFound:=True;
        end
        else // we probably found the import already
          importFound:=True;
      end
      else begin
        SearchNode:=SearchRoot.FindNode{Recursive}('complexType', 'name', refName, [ntElement], [nsSearchWithoutPrefix]);
        if SearchNode <> Nil then begin
          if not CheckCircularTypeReferenceResolved(SearchNode) then begin
            ParseComplexType(SearchNode, xmlNode, ResolvedBaseType);
            if (Length(ReferenceName) = 0) and (Length(ResolvedBaseType) = 0) then
              ParseError(ETypeReferenceBaseTypeNotResolved, xsdNode);
            if (Length(ReferenceName) > 0) and (Length(ResolvedBaseType) = 0) then
              ResolvedBaseType:=ReferenceName;
            TypedNode:=TXmlNode.Create(xmlNode);
            FParsedTypes.AddObject(ReferenceName, ResolvedBaseType, SearchNode, TypedNode);
            SetType(xsdNode, xmlNode, ResolvedBaseType);
            importFound:=True;
          end
          else // we probably found the import already
            importFound:=True;
        end
        else begin
          Dec(idx);
          if idx > -1 then
            SearchRoot:=FLastXSDImportsSearchRoot.Items[idx]
          else
            SearchRoot:=Nil;
        end;
      end;
      if importFound then
        Break;
    end;
    if not importFound then begin
      ParseError(ETypeReferenceNotFound, xsdNode);
    end;

    if removeType and (FLastXSDImportsSearchRoot.Count > 1) then begin
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
end;

procedure TXSD2XMLParser.ParseAttributeTypeReference(const xsdNode: TXmlNode; var xmlNode: TXmlNode; const ReferenceName: String; var ResolvedBaseType: String);

  function CheckCircularTypeReferenceResolved(const CheckNode: TXmlNode): Boolean;
  var
    refPrefix, refName: String;
    has_resolved: Boolean;
    resolved: Boolean;
  begin
    Result:=True;
    // detect circular type reference
    if xsdNode.HasAttribute('base') then
      TXmlNode.GetNameAndPrefix(xsdNode.Attributes['base'], refName, refPrefix);
    has_resolved:=CheckNode.HasAttribute('resolved');
    resolved:=False;
    if has_resolved then
      resolved:=(CheckNode.Attributes['resolved'] = 'true');
    if not has_resolved then
      Result:=False
    else if has_resolved and resolved and
            (xsdNode.HasAttribute('name') and xsdNode.HasAttribute('type') and
             (xmlNode.Name = xsdNode.Attributes['name']) and xmlNode.HasAttribute('type') and (xmlNode.Attributes['type'] = xsdNode.Attributes['type'])) or
            (xsdNode.HasAttribute('base') and CheckNode.HasAttribute('name') and (refName = CheckNode.Attributes['name'])) then
      Result:=False
    else begin
      if xsdNode.HasAttribute('minOccurs') and (xsdNode.Attributes['minOccurs'] = '0') and
         xsdNode.HasAttribute('maxOccurs') and (LowerCase(xsdNode.Attributes['maxOccurs']) = 'unbounded') then
        Exit
      else begin
        ParseError(ECircularTypeReference, xsdNode);
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
    ParseAttributeValue(xsdNode, xmlNode, ReferenceName);
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
      xmlNode.AddNodes(SearchNode);
//    end;
  end
  else begin
    if SearchRoot = Nil then begin
      ParseError(EReferenceListEmpty, xsdNode);
    end;

    SearchNode:=SearchRoot.FindNode{Recursive}('simpleType', 'name', refName, [ntElement], [nsSearchWithoutPrefix]);
    if SearchNode <> Nil then begin
      if not CheckCircularTypeReferenceResolved(SearchNode) then begin
        ParseSimpleType(SearchNode, xmlNode, ResolvedBaseType);
        if (Length(ReferenceName) = 0) and (Length(ResolvedBaseType) = 0) then
          ParseError(EAttributeTypeReferenceBaseTypeNotResolved, xsdNode);
        if (Length(ReferenceName) > 0) and (Length(ResolvedBaseType) = 0) then
          ResolvedBaseType:=ReferenceName;
        TypedNode:=TXmlNode.Create(xmlNode);
        FParsedTypes.AddObject(ReferenceName, ResolvedBaseType, SearchNode, TypedNode);
      end;
    end
    else begin
      SearchNode:=SearchRoot.FindNode{Recursive}('complexType', 'name', refName, [ntElement], [nsSearchWithoutPrefix]);
      if SearchNode <> Nil then begin
        if not CheckCircularTypeReferenceResolved(SearchNode) then begin
          ParseComplexType(SearchNode, xmlNode, ResolvedBaseType);
          if (Length(ReferenceName) = 0) and (Length(ResolvedBaseType) = 0) then
            ParseError(EAttributeTypeReferenceBaseTypeNotResolved, xsdNode);
          if (Length(ReferenceName) > 0) and (Length(ResolvedBaseType) = 0) then
            ResolvedBaseType:=ReferenceName;
          TypedNode:=TXmlNode.Create(xmlNode);
          FParsedTypes.AddObject(ReferenceName, ResolvedBaseType, SearchNode, TypedNode);
        end;
      end
      else begin
        ParseError(EAttributeTypeReferenceNotFound, xsdNode);
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

procedure TXSD2XMLParser.ParseReference(const xsdNode: TXmlNode; var xmlNode: TXmlNode; const ReferenceName: String; var ResolvedBaseType: String);
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
    ParseElement(SearchNode, xmlNode, ResolvedBaseType, True, IsOptional(xsdNode))
  else begin
    ParseError(EReferenceNotFound, xsdNode);
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

procedure TXSD2XMLParser.ParseValue(const xsdNode: TXmlNode; var xmlNode: TXmlNode);
var
  value{, attr_value, attr_prefix}: String;
begin
  if IsFixed(xsdNode, value) then
    xmlNode.NodeValue:=value
  else if IsDefault(xsdNode, value) then
    xmlNode.NodeValue:=value;

//  if xsdNode.HasAttribute('base') then begin
//    attr_value:=xsdNode.Attributes['base'];
//    TXmlNode.GetNameAndPrefix(attr_value, attr_value, attr_prefix);
//    value:='';
//    if xmlNode.HasAttribute('type') then
//      value:=xmlNode.Attributes['type'];
//    value:=value + ' ' + ifString(IsPascalType(attr_value), GetPascalType(attr_value), attr_value);
//    value:=Trim(value);
//    xmlNode.SetAttribute('type', value);
//  end;
end;

procedure TXSD2XMLParser.SetType(const xsdNode: TXmlNode; var xmlNode: TXmlNode; const ResolvedBaseType: String);
var
  value, attr_value, attr_prefix: String;
begin
  TXmlNode.GetNameAndPrefix(ResolvedBaseType, attr_value, attr_prefix);

  value:=ifString(IsPascalType(attr_value), GetPascalType(attr_value), ResolvedBaseType);
  if Length(value) > 0 then
    xmlNode.SetAttribute('type', value);
end;

procedure TXSD2XMLParser.ParseAttributeValue(const xsdNode: TXmlNode; var xmlNode: TXmlNode; const ReferenceName: String);
var
  value, attr_name,{ attr_value, attr_prefix,} attr_use: String;
begin
  attr_name:=xsdNode.Attributes['name'];
  attr_use:='';
  if xsdNode.HasAttribute('use') then
    attr_use:=xsdNode.Attributes['use'];
  //
//  TXmlNode.GetNameAndPrefix(ReferenceName, attr_value, attr_prefix);
  value:='';
  if IsFixed(xsdNode, value) then begin
    if attr_use = 'required' then begin
      xmlNode.SetAttribute(attr_name, value);
    end
    else if attr_use = 'optional' then begin
      xmlNode.SetAttribute('@' + attr_name, value);
    end;
  end
  else if IsDefault(xsdNode, value) then begin
    if attr_use = 'required' then begin
      xmlNode.SetAttribute(attr_name, value);
    end
    else if attr_use = 'optional' then begin
      xmlNode.SetAttribute('@' + attr_name, value);
    end;
  end
  else begin
    if attr_use = 'required' then begin
      xmlNode.SetAttribute(attr_name, '');
    end
    else if attr_use = 'optional' then begin
      xmlNode.SetAttribute('@' + attr_name, '');
    end;
  end;
end;

procedure TXSD2XMLParser.ParseAttribute(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String);
var
  attr_name,{ attr_value, attr_prefix,} attr_use: String;
  ReferenceName: String;
//  builtinType: TXSDElementType;
begin
  // check type of attribute
  attr_name:=xsdNode.Attributes['name'];
  if not IsTypeDeclared(xsdNode, ReferenceName) then begin
    if xsdNode.HasAttribute('fixed') then
      xmlNode.SetAttribute(attr_name, xsdNode.Attributes['fixed'])
    else if xsdNode.HasAttribute('default') then
      xmlNode.SetAttribute(attr_name, xsdNode.Attributes['default']);
  end
  else begin
    attr_use:='';
    if xsdNode.HasAttribute('use') then
      attr_use:=xsdNode.Attributes['use'];
    //TXmlNode.GetNameAndPrefix(ReferenceName, attr_value, attr_prefix);
    //Parent.SetAttribute('type', ifString(IsPascalType(attr_value), GetPascalType(attr_value), attr_value));
    if ReferenceName <> '' then begin // find type reference
      ParseAttributeTypeReference(xsdNode, xmlNode, ReferenceName, ResolvedBaseType);
      Exit;
    end;
    if attr_use = 'required' then begin
//      Parent.SetAttribute(attr_use, ifString(IsPascalType(attr_value), GetPascalType(attr_value), attr_value));
      xmlNode.SetAttribute('required', 'true');
    end
    else if attr_use = 'optional' then begin
//      Parent.SetAttribute('@' + attr_name, ifString(IsPascalType(attr_value), GetPascalType(attr_value), attr_value));
      xmlNode.SetAttribute('required', 'optional');
    end;
  end;
end;

procedure TXSD2XMLParser.ParseAttributeGroup(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String);

  procedure ParseChildren;
  var
    xsdChild: TXmlNode;
  begin
      xsdChild:=xsdNode;
      xsdChild:=xsdChild.FirstChild;
      while xsdChild <> Nil do begin
        if IsSame(xsdChild.Name, 'attribute') then
          ParseAttribute(xsdChild, xmlNode, ResolvedBaseType)
        else begin
          ParseError(EUnknownNode, xsdChild);
        end;
        xsdChild:=xsdChild.NextSibling;
      end;
  end;

var
  ReferenceName: String;
begin
  // check if it is referenced, if it is then skip
  if not HasReference(xsdNode, rnAttributeGroup) then begin
    if IsReferenced(xsdNode, ReferenceName) then begin // referenced type
      ParseAttributeGroupReference(xsdNode, xmlNode, ReferenceName, ResolvedBaseType);
    end
    else begin
      ParseChildren;
    end;
  end
  else begin
    ParseChildren;
  end;
end;

procedure TXSD2XMLParser.ParseAttributeGroupReference(const xsdNode: TXmlNode; var xmlNode: TXmlNode; const ReferenceName: String; var ResolvedBaseType: String);
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
    ParseAttributeGroup(SearchNode, xmlNode, ResolvedBaseType)
  else begin
    ParseError(EReferenceNotFound, xsdNode);
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

procedure TXSD2XMLParser.ParseEnumeration(const xsdNode: TXmlNode; var xmlNode: TXmlNode);
var
  value: String;
begin
  if xsdNode.HasAttribute('value') then
    xmlNode.NodeValue:=xmlNode.NodeValue + ';' + xsdNode.Attributes['value'];

  if (Length(xmlNode.NodeValue) > 0) and (xmlNode.NodeValue[1] = ';') then begin
    value:=xmlNode.NodeValue;
    Delete(value, 1, 1);
    xmlNode.NodeValue:=value;
  end;
end;

procedure TXSD2XMLParser.ParseList(const xsdNode: TXmlNode; var xmlNode: TXmlNode);
begin
  ParseError(ENodeNotSupported, xsdNode);
end;

procedure TXSD2XMLParser.ParseUnion(const xsdNode: TXmlNode; var xmlNode: TXmlNode; var ResolvedBaseType: String);
var
  xsdChild: TXmlNode;
  types, base_type: String;
  firstResolvedBaseType: String;
begin
  firstResolvedBaseType:='';
  if ResolvedBaseType <> '' then
    firstResolvedBaseType:=ResolvedBaseType;
  //
  xsdChild:=xsdNode;
  if xsdChild.HasChildNodes then begin
    // check if memberTypes exists, get the base type for union and parse for simpleType addition
    if xsdChild.HasAttribute('memberTypes') then begin
      types:=xsdChild.Attributes['memberTypes'];
      while Length(types) > 0 do begin
        base_type:=Fetch(types);
        if Length(base_type) > 0 then begin
          ParseTypeReference(xsdNode, xmlNode, base_type, ResolvedBaseType);
          if (ResolvedBaseType <> '') and (firstResolvedBaseType = '') then
            firstResolvedBaseType:=ResolvedBaseType;
          if ResolvedBaseType <> firstResolvedBaseType then
            ParseError(EUnionResolvedBaseTypeMismatch, xsdNode);
        end;
      end;
    end;
    //
    xsdChild:=xsdChild.FirstChild;
    while xsdChild <> Nil do begin
      if IsSame(xsdChild.Name, 'simpleType') then begin
        ParseSimpleType(xsdChild, xmlNode, ResolvedBaseType);
        if (ResolvedBaseType <> '') and (firstResolvedBaseType = '') then
          firstResolvedBaseType:=ResolvedBaseType;
        if ResolvedBaseType <> firstResolvedBaseType then
          ParseError(EUnionResolvedBaseTypeMismatch, xsdNode);
      end
      else begin
        ParseError(EUnknownNode, xsdChild);
      end;
      xsdChild:=xsdChild.NextSibling;
    end;
  end
  else begin // <xsd:union memberTypes="etd:TKodKraju tns:TKodKrajuISO"/>
    if xsdChild.HasAttribute('memberTypes') then begin
      types:=xsdChild.Attributes['memberTypes'];
      while Length(types) > 0 do begin
        base_type:=Fetch(types);
        if Length(base_type) > 0 then begin
          ParseTypeReference(xsdNode, xmlNode, base_type, ResolvedBaseType);
          if (ResolvedBaseType <> '') and (firstResolvedBaseType = '') then
            firstResolvedBaseType:=ResolvedBaseType;
          if ResolvedBaseType <> firstResolvedBaseType then
            ParseError(EUnionResolvedBaseTypeMismatch, xsdNode);
        end;
      end;
    end
    else
      ParseError(EUnionBaseTypeNeeded, xsdNode);
  end;
end;

procedure TXSD2XMLParser.ParseChoice(const xsdNode: TXmlNode; var xmlNode: TXmlNode);
var
  xsdChild: TXmlNode;
  InternalNode: Boolean;
  ResolvedBaseType: String;
begin
  // element | group | choice | sequence | any
  xmlNode:=Add(xsdNode, xmlNode, 'Choice');
  try
    InternalNode:=False;
    if IsOptional(xsdNode) then begin
      xmlNode:=Add(xsdNode, xmlNode, 'Optional', False);
      InternalNode:=True;
    end;

    xsdChild:=xsdNode;
    xsdChild:=xsdChild.FirstChild;
    while xsdChild <> Nil do begin
      if IsSame(xsdChild.Name, 'element') then begin
        ResolvedBaseType:='';
        ParseElement(xsdChild, xmlNode, ResolvedBaseType);
        ResolvedBaseType:='';
      end
      else if IsSame(xsdChild.Name, 'group') then
        ParseGroup(xsdChild, xmlNode)
      else if IsSame(xsdChild.Name, 'choice') then
        ParseChoice(xsdChild, xmlNode)
      else if IsSame(xsdChild.Name, 'sequence') then
        ParseSequence(xsdChild, xmlNode)
      else if IsBuiltinSkipableElement(xsdChild, True) then begin
        // Built in elements that can be skiped - do nothing
      end
      else if IsBuiltinAttr(xsdChild.Name) then
        ParseBuiltinAttr(xsdChild, xmlNode)
      else begin
        ParseError(EUnknownNode, xsdChild);
      end;
      xsdChild:=xsdChild.NextSibling;
    end;

    if InternalNode then
      xmlNode:=xmlNode.ParentNode;
  finally
    xmlNode:=xmlNode.ParentNode;
  end;
end;

procedure TXSD2XMLParser.ParseGroup(const xsdNode: TXmlNode; var xmlNode: TXmlNode);
begin
  ParseError(ENodeNotSupported, xsdNode);
end;

procedure TXSD2XMLParser.ParseSequence(const xsdNode: TXmlNode; var xmlNode: TXmlNode);
var
  xsdChild: TXmlNode;
  InternalNode: Boolean;
  ResolvedBaseType: String;
begin
  // element | group | choice | sequence | any
  xmlNode:=Add(xsdNode, xmlNode, 'Sequence');
  try
    InternalNode:=False;
    if IsOptional(xsdNode) then begin
      xmlNode:=Add(xsdNode, xmlNode, 'Optional', False);
      InternalNode:=True;
    end;

    // element | group | choice | sequence | any
    xsdChild:=xsdNode;
    xsdChild:=xsdChild.FirstChild;
    while xsdChild <> Nil do begin
      if IsSame(xsdChild.Name, 'element') then begin
        ResolvedBaseType:='';
        ParseElement(xsdChild, xmlNode, ResolvedBaseType);
        ResolvedBaseType:='';
      end
      else if IsSame(xsdChild.Name, 'group') then
        ParseGroup(xsdChild, xmlNode)
      else if IsSame(xsdChild.Name, 'choice') then
        ParseChoice(xsdChild, xmlNode)
      else if IsSame(xsdChild.Name, 'sequence') then
        ParseSequence(xsdChild, xmlNode)
      else if IsBuiltinSkipableElement(xsdChild, True) then begin
        // Built in elements that can be skiped - do nothing
      end
      else if IsBuiltinAttr(xsdChild.Name) then
        ParseBuiltinAttr(xsdChild, xmlNode)
      else begin
        ParseError(EUnknownNode, xsdChild);
      end;
      xsdChild:=xsdChild.NextSibling;
    end;

    if InternalNode then
      xmlNode:=xmlNode.ParentNode;
  finally
    xmlNode:=xmlNode.ParentNode;
  end;
end;

procedure TXSD2XMLParser.ParseBuiltinAttr(const xsdNode: TXmlNode; var xmlNode: TXmlNode);
var
  AttributeName: String;
begin
  AttributeName:=xsdNode.Name;

  if IsSame(AttributeName, 'length') then
    xmlNode.SetAttribute(AttributeName, xsdNode.Attributes['value'])
  else if IsSame(AttributeName, 'maxLength') then
    xmlNode.SetAttribute(AttributeName, xsdNode.Attributes['value'])
  else if IsSame(AttributeName, 'minLength') then
    xmlNode.SetAttribute(AttributeName, xsdNode.Attributes['value'])
  else if IsSame(AttributeName, 'pattern') then
    xmlNode.SetAttribute(AttributeName, xsdNode.Attributes['value'])
  else if IsSame(AttributeName, 'whiteSpace') then
    xmlNode.SetAttribute(AttributeName, xsdNode.Attributes['value'])
  else if IsSame(AttributeName, 'maxExclusive') then
    xmlNode.SetAttribute(AttributeName, xsdNode.Attributes['value'])
  else if IsSame(AttributeName, 'maxInclusive') then
    xmlNode.SetAttribute(AttributeName, xsdNode.Attributes['value'])
  else if IsSame(AttributeName, 'minExclusive') then
    xmlNode.SetAttribute(AttributeName, xsdNode.Attributes['value'])
  else if IsSame(AttributeName, 'minInclusive') then
    xmlNode.SetAttribute(AttributeName, xsdNode.Attributes['value'])
  else if IsSame(AttributeName, 'fractionDigits') then
    xmlNode.SetAttribute(AttributeName, xsdNode.Attributes['value'])
  else if IsSame(AttributeName, 'totalDigits') then
    xmlNode.SetAttribute(AttributeName, xsdNode.Attributes['value']);
end;

procedure TXSD2XMLParser.ParseError(const Message: String; const xsdNode: TXmlNode);
begin
  raise EXSDParseException.CreateFmt(Message + #13#10'%s', [TNodeInfo.GetNodeInfo(xsdNode)]);
end;

end.
