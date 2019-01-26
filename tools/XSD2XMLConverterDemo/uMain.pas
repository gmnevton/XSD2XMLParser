unit uMain;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  SplitEx,
  VirtualTrees,
  Vcl.StdCtrls,
  Xml.VerySimple,
  uXSD2XMLParser, Vcl.Dialogs;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    SplitterEx1: TSplitterEx;
    Panel3: TPanel;
    Panel4: TPanel;
    vstXSDSchemaTree: TVirtualStringTree;
    vstXMLTree: TVirtualStringTree;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
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
  private
    XSDSchema: TXMLVerySimple;
    XSD2XMLParser: TXSD2XMLParser;
    TreeNodeDataSize: Integer;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

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
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
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
        vstXSDSchemaTree.FullExpand;
      finally
        vstXSDSchemaTree.EndUpdate;
      end;
      Button2Click(Nil);
    end;
  finally
    Button1.Enabled:=True;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Button2.Enabled:=False;
  Application.ProcessMessages;
  try
    if XSD2XMLParser <> Nil then
      XSD2XMLParser.Free;
    XSD2XMLParser:=TXSD2XMLParser.LoadFromFile(OpenDialog1.FileName);
    vstXMLTree.BeginUpdate;
    try
      vstXMLTree.Clear;
      DoTree(vstXMLTree, XSD2XMLParser.XML.DocumentElement, Nil);
      vstXMLTree.FullExpand;
    finally
      vstXMLTree.EndUpdate;
    end;
  finally
    Button2.Enabled:=True;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Application.Terminate;
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

procedure TForm1.vstXMLTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize:=TreeNodeDataSize;
end;

procedure TForm1.vstXMLTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData: PXMLTreeNode;
begin
  NodeData:=Sender.GetNodeData(Node);
  if TextType = ttNormal then
    CellText:=NodeData.XMLNode.NameWithPrefix
  else if TextType = ttStatic then begin
    CellText:='';
    if NodeData.XMLNode.HasAttribute('info') then
      CellText:=Format('[%s]', [NodeData.XMLNode.Attributes['info']]);
  end;
end;

procedure TForm1.vstXMLTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  NodeData: PXMLTreeNode;
begin
  NodeData:=Sender.GetNodeData(Node);
  if TextType = ttStatic then
    TargetCanvas.Font.Color:=clGrayText;
end;

end.
