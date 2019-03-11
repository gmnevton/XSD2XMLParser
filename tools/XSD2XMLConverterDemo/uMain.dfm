object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'XSD 2 XML Converter Demo'
  ClientHeight = 462
  ClientWidth = 884
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 900
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterEx1: TSplitterEx
    Left = 305
    Top = 0
    Width = 7
    Height = 425
    AssignedControl = Panel2
    AutoSnap = False
    DrawSpacer = True
    MinSize = 300
    ResizeStyle = rsUpdate
    ExplicitHeight = 570
  end
  object Panel1: TPanel
    Left = 0
    Top = 425
    Width = 884
    Height = 37
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      884
      35)
    object Button1: TButton
      Left = 8
      Top = 4
      Width = 105
      Height = 25
      Caption = 'Load XSD schema'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button3: TButton
      Left = 770
      Top = 4
      Width = 105
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Exit'
      TabOrder = 1
      OnClick = Button3Click
    end
    object Button2: TButton
      Left = 119
      Top = 4
      Width = 105
      Height = 25
      Caption = 'Convert to XML'
      TabOrder = 2
      OnClick = Button2Click
    end
    object CheckBox1: TCheckBox
      Left = 240
      Top = 8
      Width = 74
      Height = 17
      Caption = 'Filter active'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object Button4: TButton
      Left = 640
      Top = 4
      Width = 105
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Filter'
      TabOrder = 4
      OnClick = Button4Click
    end
    object FilterParamString: TEdit
      Left = 320
      Top = 6
      Width = 314
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      Text = 'node_name == "PozycjaUszczegolawiajaca*"'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 305
    Height = 425
    Align = alLeft
    BevelEdges = []
    BevelOuter = bvNone
    Caption = 'Panel2'
    ShowCaption = False
    TabOrder = 1
    object vstXSDSchemaTree: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 305
      Height = 425
      Align = alClient
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.MainColumn = -1
      Header.Options = []
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoChangeScale]
      TreeOptions.MiscOptions = [toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toShowButtons, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toExtendedFocus]
      TreeOptions.StringOptions = [toShowStaticText]
      OnGetText = vstXSDSchemaTreeGetText
      OnPaintText = vstXSDSchemaTreePaintText
      OnGetNodeDataSize = vstXSDSchemaTreeGetNodeDataSize
      Columns = <>
    end
  end
  object Panel3: TPanel
    Left = 312
    Top = 0
    Width = 572
    Height = 425
    Align = alClient
    BevelEdges = [beLeft]
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = 'Panel3'
    ShowCaption = False
    TabOrder = 2
    object vstXMLTree: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 570
      Height = 425
      Align = alClient
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      EditDelay = 0
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.MainColumn = -1
      Header.Options = []
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoChangeScale]
      TreeOptions.MiscOptions = [toEditable, toWheelPanning, toEditOnDblClick]
      TreeOptions.PaintOptions = [toShowButtons, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toExtendedFocus, toAlwaysSelectNode]
      TreeOptions.StringOptions = [toShowStaticText]
      OnCreateEditor = vstXMLTreeCreateEditor
      OnEditCancelled = vstXMLTreeEditCancelled
      OnEdited = vstXMLTreeEdited
      OnGetText = vstXMLTreeGetText
      OnPaintText = vstXMLTreePaintText
      OnGetNodeDataSize = vstXMLTreeGetNodeDataSize
      Columns = <>
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.xsd'
    Filter = 'XML Schemas|*.xsd'
    Options = [ofPathMustExist, ofFileMustExist, ofNoTestFileCreate, ofEnableSizing]
    Left = 336
    Top = 376
  end
end
