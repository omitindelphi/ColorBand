unit ColorBandTestUnit;

interface
uses
   DUnitX.TestFramework
  ,Spring
  ,Spring.Collections
  ,SysUtils
  ,ClrBandInterface
  ,XML.XMLIntf
  ,XMl.XMLdoc
  ,Graphics
  ;

type

  [TestFixture]
  TColorBandTest = class( TObject)
  private
    FClrBand: IBandFill;
    procedure SubtractUsedPolygonKinds( CasesUnUsedYet: IList<integer>; Width,
      Height: integer);
    function ListToStr( Value: IList<integer>): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

     [Test]
     procedure TestForEmptyCell;

     [Test]
     [TestCase('test for correct band shifting','0')]
     [TestCase('test for correct band shifting+30','30')]
     [TestCase('test for correct band shifting-30','-30')]
     procedure TestForCorrectShift( BandShift:integer);

     [Test]
     [TestCase('Test-W240-H319-BW102', '319, 240, 102, 0')]
     procedure TestForCorrectBandsWithinDeclaredRect( Width, Height, BandWidth, BandShift:integer);


     [Test]
     [TestCase('Test-W184-H473-BW256-Shift+119 Neighborhood', '184, 473, 256, 139')]
     [TestCase('Test-W217-H216-BW431-Shift+123 Neighborhood', '217, 216, 431, 123')]
     [TestCase('Test-W319-H240-BW102-Neighborhood', '319, 240, 102, 0')]
     [TestCase('Test-W215-H489-BW273-Neighborhood', '215, 489, 273, 0')]
     [TestCase('Test-W184-H801-BW223-Neighborhood', '184, 801, 223, 0')]
     [TestCase('Test-W184-H801-BW223-Neighbor+shift', '184, 801, 223, 90')]
     [TestCase('Test-W184-H801-BW223-Neighbor-shift', '184, 801, 223, -90')]
     procedure TestForColorBandsStayContainedWithinRect( Width, Height, BandWidth, BandShift: integer);

     [Test]
     [TestCase('Test-SVG-W240-H319-BW102-Neighborhood', '319, 240, 102, 0')]
     procedure TestForSVGOutputPresence( Width, Height, BandWidth, BandShift: integer);

     [Test]
     procedure TestForPolygonKindsCoverage;
  end;

implementation
uses
    AutomatedTestMainForm
    ,TestBandSVG;

procedure TColorBandTest.TestForCorrectBandsWithinDeclaredRect( Width, Height, BandWidth, BandShift:integer);
var
  sTop, sBottom: string;
begin
  FClrBand.SetTestDimWithoutWhiteBorder(Width, Height, BandWidth, BandShift);
  sTop := FClrBand.PerimeterTop;
  sBottom := FClrBand.PerimeterBottom;
  Assert.AreEqual(sTop, sBottom, 'Band border coordinates by top and bottom line do not match');
  Assert.IsTrue(Length(sTop) > 10, 'Descriptor of top border too short');
  Assert.IsTrue(Pos('Unknown', sTop) <= 0, 'There are empty bands on picture');
end;

procedure TColorBandTest.TestForCorrectShift( BandShift:integer);
var
  sBottom, SVG: string;
  Doc: IXMLDocument;
  NodesLevel1: IXMLNodeList;
  PointsNode: IXMLNode;
  AllPoints: string;
  Vertexes: TArray<string>;
  LastVertex: string;
  LastPointXY: TArray<string>;
  XPointOfSecondStrip: integer;
  NormalizedBandShift: integer;
  ExpectedBorderPosition: integer;
begin
  NormalizedBandShift := BandShift mod ( 50);

  FClrBand.SetTestDimWithoutWhiteBorder(300, 300, 50, BandShift);
  sBottom := FClrBand.PerimeterBottom;
  SVG := FCLRBand.GetSVGFragment();

  Doc := TXMLDocument.Create(nil);
  Doc.LoadFromXML(AnsiString(SVG));
  Doc.Active := True;
  NodesLevel1 := Doc.DocumentElement.ChildNodes;
  PointsNode := NodesLevel1.Nodes[0];
  AllPoints := PointsNode.Attributes['points'];
  Vertexes := AllPoints.Split([' ']);
  Assert.IsTrue( Length(Vertexes) > 0, 'First SVG polygon has zero sizes');
  LastVertex :=  Vertexes[Length(Vertexes) - 1];
  LastPointXY := LastVertex.Split([',']);
  XPointOfSecondStrip := StrToInt(LastPointXY[0]);
  if BandShift >= 0 then
    ExpectedBorderPosition := 50 - NormalizedBandShift + 1
  else
    ExpectedBorderPosition := - NormalizedBandShift + 1;


  Assert.IsTrue(XPointOfSecondStrip = ExpectedBorderPosition,
                'Misplaced position of second strip, shift ' + IntToStr(BandShift));

end;

procedure TColorBandTest.TestForColorBandsStayContainedWithinRect(Width, Height, BandWidth, BandShift:integer);
var
  sTop, sBottom: string;
begin
  FClrBand.SetTestDimWhiteBorder(Width, Height, BandWidth, BandShift);
  sTop := FClrBand.PerimeterTop;
  sBottom := FClrBand.PerimeterBottom;
  Assert.IsTrue(Length(sTop) = 0, 'There are color changes beyond left - top border of picture');
  Assert.IsTrue(Length(sBottom) = 0, 'There are color changes beyond bottom - right border of picture');
end;

procedure TColorBandTest.TestForSVGOutputPresence(Width, Height, BandWidth,
  BandShift: integer);
var
  sTop, sBottom,SVG: string;
begin
  FClrBand.SetTestDimWithoutWhiteBorder(Width, Height, BandWidth, BandShift);
  sTop := FClrBand.PerimeterTop;
  sBottom := FClrBand.PerimeterBottom;
  SVG := FClrBand.GetSVGFragment();
  Assert.IsTrue(Length(SVG) > 0, 'No SVG generated at all');
end;

procedure TColorBandTest.SubtractUsedPolygonKinds(CasesUnUsedYet: IList<integer>; Width, Height: integer);
var
  SVG: string;
  CasesUsed: IList<integer>;
  SVGTester: ISVGTester;
begin
  SVGTester := TSVGTester.Create;
  FClrBand.SetTestDimWhiteBorder(Width,Height, 102, 0);
  SVG := FClrBand.GetSVGFragment();
  CasesUsed := nil;
  CasesUsed := SVGTester.ExtractUsedPolygonKindsFromSVG(SVG);
  SVGTester.SubtractUsedCases(CasesUnUsedYet, CasesUsed);
end;

procedure TColorBandTest.TestForPolygonKindsCoverage();
var
  SVG: string;
  CasesUsed: IList<integer>;
  CasesUnUsedYet: IList<integer>;
  SVGTester: ISVGTester;
  ListOfUnTestedCases: IEnumerable<integer>;
begin
  SVGTester := TSVGTester.Create;
  CasesUnUsedYet := TCollections.CreateList<integer>;
  CasesUnUsedYet.AddRange([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]);


  FClrBand.SetSingleCellWhiteBorder(319, 240, 102, 0);
  SVG := FClrBand.GetSVGFragment();
  CasesUsed := SVGTester.ExtractUsedPolygonKindsFromSVG(SVG);
  SVGTester.SubtractUsedCases(CasesUnUsedYet, CasesUsed);


  SubtractUsedPolygonKinds(CasesUnUsedYet, 319, 240);

  SubtractUsedPolygonKinds(CasesUnUsedYet, 215, 489);

  SubtractUsedPolygonKinds(CasesUnUsedYet, 319, 50);

  SubtractUsedPolygonKinds(CasesUnUsedYet, 48, 150);

  SubtractUsedPolygonKinds(CasesUnUsedYet, 75, 75);

  SubtractUsedPolygonKinds(CasesUnUsedYet, 30, 30);

  FClrBand.SetTestDimWhiteBorder(217, 216, 431, -123);
  SVG := FClrBand.GetSVGFragment();
  CasesUsed := SVGTester.ExtractUsedPolygonKindsFromSVG(SVG);
  SVGTester.SubtractUsedCases(CasesUnUsedYet, CasesUsed);


  FClrBand.SetTestDimWhiteBorder(90, 90, 60, 0);
  SVG := FClrBand.GetSVGFragment();
  CasesUsed := SVGTester.ExtractUsedPolygonKindsFromSVG(SVG);
  SVGTester.SubtractUsedCases(CasesUnUsedYet, CasesUsed);

  ListOfUnTestedCases := CasesUnUsedYet.Where(function(const CaseNumberStored: integer): boolean
                                              begin
                                               Result := (CaseNumberStored >= 0);
                                              end
                                              );
  Assert.IsTrue(ListOfUnTestedCases.Count = 0, 'Not all polygon configurations tested: '
                                             + ListToStr(CasesUnusedYet)
                                             + ' left yet'
                                             );
end;

procedure TColorBandTest.TestForEmptyCell;
var
  SVG: string;
  TestColors: IList<TColor>;
begin
  TestColors := TCollections.CreateList<TColor>;
  FClrBand.SetColorAndShift(TestColors, 0);
  SVG := FClrBand.GetSVGFragment();
  Assert.IsTrue(Length(SVG) > 0, 'No SVG generated at all');
end;

procedure TColorBandTest.Setup;
begin
  FClrBand := TAutomatedTestForm.Create(nil);
end;


procedure TColorBandTest.TearDown;
begin
  FClrBand.Teardown;
end;

function TColorBandTest.ListToStr( Value: IList<integer>): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Value.Count - 1 do
  begin
    if Value[i] >= 0 then
      Result := Result + IntToStr(Value[i]) + ',';
  end;
  if Length(Result) > 0 then
    Result := Copy( Result, 1, Length(Result) - 1);
end;

initialization
  TDUnitX.RegisterTestFixture(TColorBandTest);
end.
