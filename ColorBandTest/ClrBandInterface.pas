unit ClrBandInterface;

interface
uses
  Spring.Collections;

type
   IBandFill = interface(IInvokable)
    ['{97595126-64A6-4246-8FF9-E53024C5223C}']
    procedure SetTestDimWithoutWhiteBorder(const ImgX, ImgY, bandWidth, bandShift: integer);
    procedure SetTestDimWhiteBorder(const ImgX, ImgY, bandWidth, bandShift: integer);
    procedure SetSingleCellWhiteBorder(const ImgX, ImgY, bandWidth, BandShift: integer);
    procedure Teardown;
    function PerimeterTop:string;
    function PerimeterBottom: string;
    function GetSVGFragment: string;
  end;
implementation

end.
