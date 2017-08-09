unit ColorBandTestUnit;

interface
uses
   DUnitX.TestFramework
  ,ClrBandInterface
  ;

type

  [TestFixture]
  TColorBandTest = class(TObject)
  private
    FClrBand: IBandFill;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
     [Test]
     [TestCase('Test-W240-H319-BW102', '319, 240, 102, 0')]
     procedure BandTest(Width, Height, BandWidth, BandShift:integer);

     [Test]
     [TestCase('Test-W240-H319-BW102-Neighborhood', '319, 240, 102, 0')]
     [TestCase('Test-W215-H415-BW273-Neighborhood', '215, 489, 273, 0')]
     [TestCase('Test-W184-H801-BW223-Neighborhood', '184, 801, 223, 0')]
     [TestCase('Test-W184-H801-BW223-Neighbor+shift', '184, 801, 223, 90')]
     [TestCase('Test-W184-H801-BW223-Neighbor-shift', '184, 801, 223, -90')]
     procedure NeighborTest(Width, Height, BandWidth, BandShift: integer);
  end;

implementation
uses
    FormUnit;

procedure TColorBandTest.BandTest(Width, Height, BandWidth, BandShift:integer);
var
  sTop, sBottom: string;
begin
  FClrBand.SetTestDim(Width, Height, BandWidth, BandShift, False);
  sTop := FClrBand.PerimeterTop;
  sBottom := FClrBand.PerimeterBottom;
  Assert.AreEqual(sTop, sBottom, 'Band border coordinates by top and bottom line do not match');
  Assert.IsTrue(Length(sTop) > 10, 'Descriptor of top border too short');
  Assert.IsTrue(Pos('Unknown', sTop) <= 0, 'There are empty bands on picture');

end;

procedure TColorBandTest.NeighborTest(Width, Height, BandWidth, BandShift:integer);
var
  sTop, sBottom: string;
begin
  FClrBand.SetTestDim(Width, Height, BandWidth, BandShift, True);
  sTop := FClrBand.PerimeterTop;
  sBottom := FClrBand.PerimeterBottom;
  Assert.IsTrue(Length(sTop) = 0, 'There are color changes beyond left - top border of picture');
  Assert.IsTrue(Length(sBottom) = 0, 'There are color changes beyond bottom - right border of picture');

end;

procedure TColorBandTest.Setup;
begin
  FClrBand := TForm1.Create(nil);
end;

procedure TColorBandTest.TearDown;
begin
  FClrBand.Teardown;
end;


initialization
  TDUnitX.RegisterTestFixture(TColorBandTest);
end.
