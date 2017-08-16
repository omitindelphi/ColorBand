unit ColorBandTestUnit;

interface
uses
   DUnitX.TestFramework
  ,Spring
  ,Spring.Collections
  ,SysUtils
  ,ClrBandInterface
  ;

type

  [TestFixture]
  TColorBandTest = class(TObject)
  private
    FClrBand: IBandFill;
    procedure SubtractUsedPolygonKinds(CasesUnUsedYet: IList<integer>; Width,
      Height: integer);
    function ListToStr(Value: IList<integer>): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
     [Test]
     [TestCase('Test-W240-H319-BW102', '319, 240, 102, 0')]
     procedure TestForCorrectBandsWithinDeclaredRect(Width, Height, BandWidth, BandShift:integer);


     [Test]
     [TestCase('Test-W240-H319-BW102-Neighborhood', '319, 240, 102, 0')]
     [TestCase('Test-W215-H415-BW273-Neighborhood', '215, 489, 273, 0')]
     [TestCase('Test-W184-H801-BW223-Neighborhood', '184, 801, 223, 0')]
     [TestCase('Test-W184-H801-BW223-Neighbor+shift', '184, 801, 223, 90')]
     [TestCase('Test-W184-H801-BW223-Neighbor-shift', '184, 801, 223, -90')]
     procedure TestForColorBandsStayContainedWithinRect(Width, Height, BandWidth, BandShift: integer);

     [Test]
     [TestCase('Test-SVG-W240-H319-BW102-Neighborhood', '319, 240, 102, 0')]
     procedure TestForSVGOutputPresence(Width, Height, BandWidth, BandShift: integer);

     [Test]
     procedure TestForPolygonKindsCoverage;
  end;

implementation
uses
    AutomatedTestMainForm
    ,TestBandSVG;

procedure TColorBandTest.TestForCorrectBandsWithinDeclaredRect(Width, Height, BandWidth, BandShift:integer);
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
  CasesUnUsedYet.AddRange([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]);

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
