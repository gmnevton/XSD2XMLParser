unit uMain;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  Graphics,
  ExtCtrls,
  SplitEx,
  VirtualTrees,
  StdCtrls,
  Xml.VerySimple,
  uXSD2XMLParser,
  PerformanceTimer;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    SplitterEx1: TSplitterEx;
    Panel3: TPanel;
    vstXSDSchemaTree: TVirtualStringTree;
    vstXMLTree: TVirtualStringTree;
    Button1: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Button4: TButton;
    FilterParamString: TEdit;
    //
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vstXSDSchemaTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure vstXSDSchemaTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstXSDSchemaTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure vstXMLTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure vstXMLTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstXMLTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure vstXMLTreeCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure vstXMLTreeEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
    procedure vstXMLTreeEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstXSDSchemaTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstXMLTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    XSDSchema: TXMLVerySimple;
    XSD2XMLParser: TXSD2XMLParser;
    TreeNodeDataSize: Integer;
    PerformanceTimer: TPerformanceTimer;
    procedure FilterXMLTree(const EnableFiltering: Boolean);
    function IsNodeFiltered(const NodeName: String): Boolean;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  uGMUtils,
  uclCPPStatementResolver,
  uclVariablesList;

type
  TXMLTreeNode = record
    XMLNode: TXmlNode;
  end;
  PXMLTreeNode = ^TXMLTreeNode;

procedure TForm1.FormCreate(Sender: TObject);
begin
  XSDSchema:=TXmlVerySimple.Create;
  XSD2XMLParser:=Nil;
  TreeNodeDataSize:=SizeOf(TXMLTreeNode);
  PerformanceTimer:=TPerformanceTimer.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  PerformanceTimer.Free;
  XSDSchema.Free;
  if XSD2XMLParser <> Nil then
    XSD2XMLParser.Free;
end;

procedure DoTree(const ATree: TVirtualStringTree; const ANode: TXmlNode; const TreeParent: PVirtualNode);
var
  TreeNode: PVirtualNode;
  NodeData: PXMLTreeNode;
  Node: TXmlNode;
begin
  if ANode <> Nil then begin
    TreeNode:=ATree.AddChild(TreeParent);
    NodeData:=ATree.GetNodeData(TreeNode);
    with NodeData^ do begin
      XMLNode:=ANode;
    end;

    if ANode.HasChildNodes then
      for Node in ANode.ChildNodes do
        DoTree(ATree, Node, TreeNode);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Enabled:=False;
  Application.ProcessMessages;
  try
    if OpenDialog1.Execute then begin
      XSDSchema.LoadFromFile(OpenDialog1.FileName);
      vstXSDSchemaTree.BeginUpdate;
      try
        vstXSDSchemaTree.Clear;
        DoTree(vstXSDSchemaTree, XSDSchema.DocumentElement, Nil);
      finally
        vstXSDSchemaTree.EndUpdate;
        vstXSDSchemaTree.FullExpand;
      end;
      //Button2Click(Nil);
    end;
  finally
    Button1.Enabled:=True;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  parse_time: Int64;
begin
  Button2.Enabled:=False;
  Application.ProcessMessages;
  try
    if XSD2XMLParser <> Nil then
      XSD2XMLParser.Free;
    PerformanceTimer.Start;
    XSD2XMLParser:=TXSD2XMLParser.LoadFromFile(OpenDialog1.FileName);
    PerformanceTimer.Stop;
    parse_time:=PerformanceTimer.ElapsedMiliseconds;
    XSD2XMLParser.XML.LineBreak:=#13#10;
    XSD2XMLParser.XML.NodeIndentStr:='    ';
    XSD2XMLParser.XML.NodeAutoIndent:=True;
    XSD2XMLParser.XML.SaveToFile(ChangeFileExt(OpenDialog1.FileName, '.xml'));
    vstXMLTree.BeginUpdate;
    try
      vstXMLTree.Clear;
      PerformanceTimer.Start;
      DoTree(vstXMLTree, XSD2XMLParser.XML.DocumentElement, Nil);
      PerformanceTimer.Stop;
    finally
      vstXMLTree.EndUpdate;
      vstXMLTree.FullExpand;
      Application.ProcessMessages;
      ShowMessage('Parsed in: ' + IntToStr(parse_time) + ' ms, tree maked in: ' + IntToStr(PerformanceTimer.ElapsedMiliseconds) + ' ms');
    end;
  finally
    Button2.Enabled:=True;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FilterXMLTree(CheckBox1.Checked);
end;

procedure TForm1.FilterXMLTree(const EnableFiltering: Boolean);
var
  Node, ParentNode: PVirtualNode;
  NodeData, ParentNodeData: PXMLTreeNode;
begin
  if EnableFiltering and (FilterParamString.Text <> '') then begin
//    mnuMoveStructureFiltered.Enabled:=True;
//
// Parameters:
//    XSD_DIR: String;
//    XSD_KOD_SYSTEMOWY: String;
//    XSD_URI: String;
//    XSD_SYSTEM: String;
//
//
// FFilterParamString:
//    system != "SPRFIN" and name == "*"; system == "SPRFIN" and name != "PozycjaUszczegolawiajaca*"
//
    vstXMLTree.BeginUpdate;
    try
      Node:=vstXMLTree.GetFirst;
      while Node <> Nil do begin
        NodeData:=vstXMLTree.GetNodeData(Node);
        //if (XSD_SYSTEM = 'SPRFIN') and StringMatchesMask(NodeData^.XMLNode.NodeName, 'PozycjaUszczegolawiajaca*') then
        if IsNodeFiltered(NodeData^.XMLNode.NodeName) then begin
          vstXMLTree.IsVisible[Node]:=False;
          ParentNode:=Node.Parent;
          if (ParentNode <> Nil) then begin
            ParentNodeData:=vstXMLTree.GetNodeData(ParentNode);
            if (ParentNodeData^.XMLNode.NodeName = 'Optional') and (ParentNode.ChildCount = 1) then
              vstXMLTree.IsVisible[ParentNode]:=False;
          end;
        end
        else
          vstXMLTree.IsVisible[Node]:=True;
        Node:=vstXMLTree.GetNext(Node);
      end;
    finally
      vstXMLTree.EndUpdate;
    end;
  end
  else begin
    //mnuMoveStructureFiltered.Enabled:=False;
    vstXMLTree.BeginUpdate;
    try
      Node:=vstXMLTree.GetFirst;
      while Node <> Nil do begin
        if not vstXMLTree.IsVisible[Node] then
          vstXMLTree.IsVisible[Node]:=True;
        Node:=vstXMLTree.GetNext(Node);
      end;
    finally
      vstXMLTree.EndUpdate;
    end;
  end;
end;

function TForm1.IsNodeFiltered(const NodeName: String): Boolean;
var
  solver: TCPPStatementResolver;
  statements: TStringArray;
  statement: String;
begin
  Result:=False;
  solver:=TCPPStatementResolver.Create;
  try
    statements:=explode(';', FilterParamString.Text);
    try
      for statement in statements do begin
        solver.Statement:=statement;
        //solver.AddVariable(TDataVariable.Create('XSD_DIR', XSD_DIR, True));
        //solver.AddVariable(TDataVariable.Create('XSD_KOD_SYSTEMOWY', XSD_KOD_SYSTEMOWY, True));
        //solver.AddVariable(TDataVariable.Create('XSD_URI', XSD_URI, True));
        //solver.AddVariable(TDataVariable.Create('XSD_SYSTEM', XSD_SYSTEM, True));
        solver.AddVariable(TDataVariable.Create('NODE_NAME', NodeName, True));
        ///
        Result:=Result or solver.Resolve;
      end;
//      Result:=not Result;
    finally
      SetLength(statements, 0);
    end;
  finally
    solver.Free;
  end;
end;

procedure TForm1.vstXSDSchemaTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NodeData: PXMLTreeNode;
begin
  NodeData:=Sender.GetNodeData(Node);
  NodeData.XMLNode:=Nil;
end;

procedure TForm1.vstXSDSchemaTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize:=TreeNodeDataSize;
end;

procedure TForm1.vstXSDSchemaTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData: PXMLTreeNode;
  temp: String;
begin
  NodeData:=Sender.GetNodeData(Node);
  if TextType = ttNormal then begin
    if NodeData.XMLNode.NodeType <> ntElement then
      CellText:='{' + NodeData.XMLNode.Text + '}'
    else
      CellText:=NodeData.XMLNode.NameWithPrefix;
  end
  else if TextType = ttStatic then begin
    CellText:='';
    if NodeData.XMLNode.HasAttribute('info') then
      CellText:=Format('[%s]', [NodeData.XMLNode.Attributes['info']])
    else begin
      if NodeData.XMLNode.NodeType <> ntComment then begin
        temp:=NodeData.XMLNode.AttributeList.AsString;
        if temp <> '' then
          CellText:=Format('(%s)', [temp])
        else begin
          temp:=NodeData.XMLNode.Text;
          if temp <> '' then
            CellText:=Format('"%s"', [temp]);
        end;
      end;
    end;
  end;
end;

procedure TForm1.vstXSDSchemaTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  NodeData: PXMLTreeNode;
begin
  NodeData:=Sender.GetNodeData(Node);
  if (TextType = ttNormal) then begin
    case NodeData.XMLNode.NodeType of
      ntText   : TargetCanvas.Font.Color:=clBlue;
      ntCData  : TargetCanvas.Font.Color:=clTeal;
      ntComment: TargetCanvas.Font.Color:=clGrayText;
    end;
  end
  else if TextType = ttStatic then begin
    if NodeData.XMLNode.HasAttribute('info') then
      TargetCanvas.Font.Color:=clGrayText
    else
      TargetCanvas.Font.Color:=clMaroon;
  end;
end;

procedure TForm1.vstXMLTreeCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
//var
//  NodeData: PXMLTreeNode;
begin
  if Node <> Nil then begin
//    NodeData:=Sender.GetNodeData(Node);
    EditLink:=TStringEditLink.Create;
//    if NodeData.XMLNode.HasAttribute('info') then
//      TStringEditLink(EditLink).Edit.Text:=NodeData.XMLNode.Attributes['info'];
  end;
end;

procedure TForm1.vstXMLTreeEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).TreeOptions.StringOptions:=[toShowStaticText];
  Sender.Invalidate;
end;

procedure TForm1.vstXMLTreeEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).TreeOptions.StringOptions:=[toShowStaticText];
  Sender.Invalidate;
end;

procedure TForm1.vstXMLTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NodeData: PXMLTreeNode;
begin
  NodeData:=Sender.GetNodeData(Node);
  NodeData.XMLNode:=Nil;
end;

procedure TForm1.vstXMLTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize:=TreeNodeDataSize;
end;

procedure TForm1.vstXMLTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData: PXMLTreeNode;
begin
  NodeData:=Sender.GetNodeData(Node);
  if Sender.IsEditing then begin
    TVirtualStringTree(Sender).TreeOptions.StringOptions:=[];
    Sender.Invalidate;
    CellText:='';
    if NodeData.XMLNode.HasAttribute('info') then
      CellText:=NodeData.XMLNode.Attributes['info'];
  end
  else begin
    if TextType = ttNormal then
      CellText:=NodeData.XMLNode.NameWithPrefix
    else if TextType = ttStatic then begin
      CellText:='';
      if NodeData.XMLNode.HasAttribute('info') then
        CellText:=Format('[%s]', [NodeData.XMLNode.Attributes['info']]);
    end;
  end;
end;

procedure TForm1.vstXMLTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
//var
//  NodeData: PXMLTreeNode;
begin
//  NodeData:=Sender.GetNodeData(Node);
  if TextType = ttStatic then
    TargetCanvas.Font.Color:=clGrayText;
end;

end.
