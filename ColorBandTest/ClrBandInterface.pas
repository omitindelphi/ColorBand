unit ClrBandInterface;

interface
type
   IBandFill = interface(IInvokable)
    ['{97595126-64A6-4246-8FF9-E53024C5223C}']
    procedure SetTestDim(const ImgX, ImgY, bandWidth, bandShift: integer; toDisplayNeighborhood: boolean);
    procedure Teardown;
    function PerimeterTop:string;
    function PerimeterBottom: string;
  end;

implementation

end.
