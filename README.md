# XSD2XMLParser

  XSD to XML Parser v.14.0
  XSD Schema to XML Parser / Converter for Delphi 2010-XE10.3

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
