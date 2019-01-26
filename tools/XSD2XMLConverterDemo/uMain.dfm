object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'XSD 2 XML Converter Demo'
  ClientHeight = 461
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
    Height = 424
    AssignedControl = Panel2
    AutoSnap = False
    DrawSpacer = True
    MinSize = 300
    ResizeStyle = rsUpdate
    ExplicitHeight = 570
  end
  object Panel1: TPanel
    Left = 0
    Top = 424
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
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 305
    Height = 424
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
      Height = 424
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
    Height = 424
    Align = alClient
    BevelEdges = [beLeft]
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = 'Panel3'
    ShowCaption = False
    TabOrder = 2
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 145
      Height = 424
      Align = alLeft
      BevelEdges = [beRight]
      BevelKind = bkFlat
      BevelOuter = bvNone
      Caption = 'Panel4'
      ShowCaption = False
      TabOrder = 0
      object Button2: TButton
        Left = 16
        Top = 16
        Width = 105
        Height = 25
        Caption = 'Convert to XML'
        TabOrder = 0
        OnClick = Button2Click
      end
    end
    object vstXMLTree: TVirtualStringTree
      Left = 145
      Top = 0
      Width = 425
      Height = 424
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
      TabOrder = 1
      TreeOptions.AutoOptions = [toAutoChangeScale]
      TreeOptions.MiscOptions = [toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toShowButtons, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toExtendedFocus]
      TreeOptions.StringOptions = [toShowStaticText]
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
