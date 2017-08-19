///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//    Function to draw color slanted stripes in cell on canvas               //
//      and place text over them with default canvas settings                //
//    Returns SVG presentation of the cell                                   //
//                                                                           //
//    Simple to see and interesting to get it working and test               //
//                                                                           //
//     Free for commercial and non-commercial use                            //
//                                                                           //
//     Designed by Oleg Mitin, 1998 - 2017                                   //
//     https://github.com/omitindelphi                                       //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

unit ClrBand;

interface
uses Classes,Windows,Graphics
,Variants
,Types
,SysUtils
,StrUtils
,Spring
,Spring.Collections
,Xml.VerySimple
;

function ColorBandsOfListMovable( Canvas:TCanvas;
                                  Rect:TRect;
                                  ColorList: IList<TColor> ;
                                  BandWidth, BandShift:integer;
                                  Text:string): string;
implementation

uses
  UITypes;

type

  GraphicStorage = record
    PenStyle: TPenStyle;
    PenColor: TColor;
    PenWidth: integer;
    PenMode: TPenMode;
    BrushColor: TColor;
    BrushStyle: TBrushStyle;
  end;

  TLocalCellDescriptor = record
    Text:string;
    Rect: TRect;
    Colors: IList<TColor>;
    BandWidth: integer;
    Bandshift: integer;
    Canvas: TCanvas;
    R: TRect;
  end;

  TParallelogram = record
    public
      function BandWidth: integer;
//      function Edges: TQuattroEdges;
    case Integer of
    0: (Vertexes: array[0..3] of TPoint);
    1: (  // A point of top left , clockwise
         A: TPoint;
         B: TPoint;
         C: TPoint;
         D: Tpoint;
       );
    2: (
          xa: integer;
          ya: integer;
          xb: integer;
          yb: integer;
          xc: integer;
          yc: integer;
          xd: integer;
          yd: integer
       );
   end;

   TpointArray = TArray<Tpoint>;

   TVertexesCase = record
     Vertexes: TpointArray;
     PolygonCase: integer;
   end;

  SerializablePolygon = interface
  ['{BF18BFBB-53D5-431B-94F6-A40C6EF4132E}']
    function SerializeAsSVGFragment: string;
    procedure Draw(Canvas: TCanvas);
  end;

  TCrossGeneratorFunction = function(Cell: TRect; Band: TParallelogram):TVertexesCase of object;

  TCanvasPolygon = class(TInterfacedObject, SerializablePolygon)
  private
    VertexesCase: TVertexesCase;
    ColorFill: Tcolor;
    FVertexPopulatorHolder: TCrossGeneratorFunction;
    function PolygonCase: integer; // we have 16 different configurations; and we must be sure that no configuration went untested, or ever unused
    class function SelectorOfIntersectionCase(Cell:TRect; Band: TParallelogram): TCrossGeneratorFunction;
    class function GenerateVertexCase01(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase02(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase03(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase04(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase05(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase06(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase07(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase08(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase09(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase10(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase11(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase12(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase13(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase14(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase15(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase16(Cell:TRect; Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase17(Cell: TRect;Band: Tparallelogram): TVertexesCase;
    class function GenerateVertexCase18(Cell: TRect;Band: Tparallelogram): TVertexesCase;
   public
    constructor CreateAsCellToCrossParallelogram(Cell:TRect; Band: TParallelogram; ColorFill: TColor);
    constructor CreateAsSingleColorRect(Cell: TRect; ColorToFill: Tcolor);
    function SerializeAsSVGFragment: string;
    procedure Draw(Canvas : TCanvas);
  end;

  TCanvasPolygonInsider = class helper for TCanvasPolygon
    function SerializeVortexes: string;
    function SerializeFillStyle: string;
    function SerializePolygonKind: string;
  end;

  TTextRectCanvas = record
                      Text: string;
                      Rect:Trect;
                      Canvas:TCanvas;
                      constructor Create(Text: string; Rect:TRect; Canvas: TCanvas);
                    end;

  TCellInsider = class
  private
    class function ColorToRGBString(Color: TColor): string; static;
    class function SVGTagWrap(SVGFragment: string; Rect: TRect): string; static;
    class function PolygonFragmentToSVG(TextRectCanvas: TLocalCellDescriptor; PolygonFragmentSVG: string): string; static;
    class function StoreCanvasPenBrush(Canvas: TCanvas): GraphicStorage; static;
    class procedure RestoreCanvasPenBrush(Storage: GraphicStorage; Canvas: TCanvas);
    class function TextToSVGFragment(TextRectCanvas: TLocalCellDescriptor): string; static;
    class function GetDefaultFontName: string; static;
    class function NormalizeBandWidth(BandWidth: integer): integer; static;
    class function NormalizeBandshift(CellDescriptor: TLocalCellDescriptor): integer; static;
    class function DrawSingleCellForSingleColor(
      CellDescriptor: TLocalCellDescriptor): string; static;
    class procedure ClearCellCanvas(CellDescriptor: TLocalCellDescriptor); static;
    class function InitializeXIterator(
      CellDescriptor: TlocalCelldescriptor): integer; static;
  end;

  function TCanvasPolygon.PolygonCase: integer;
  begin
    Result := VertexesCase.PolygonCase;
  end;

  class function TCanvasPolygon.SelectorOfIntersectionCase(Cell:TRect; Band: TParallelogram): TCrossGeneratorFunction;
  var
    R: Trect;
  begin
    Result := nil; // no cross between Cell and parallelogram
    R := Cell;
    with Band do
    begin
      if (band.xa <= Cell.Left)and(Band.xb <= Cell.Left)and(Band.xc <= Cell.Right)and(Band.xd <= Cell.Left) and (Band.xc > Cell.Left)   then // Triangle :1 bottom left
        begin Result := GenerateVertexCase01;  Exit; end;
      if (xa <= R.Left)and(xb<= R.Left)and (xc > R.Left)and(xc<= R.Right)and(xd> R.Left)  then // 4-angle   :2
        begin Result := GenerateVertexCase02;  Exit;   end;
      if (xa <= R.Left)and(xb<= R.Left)and(xc> R.Right)and(xd<= R.Left) then // 4-angle :3
        begin Result := GenerateVertexCase03;  Exit; end;
      if (xa <= R.Left)and(xb<= R.Left)and(xc> R.Right)and(xd> R.Left)and(xd<= R.Right) then  // Right Bottom 5-angle;   :4
        begin Result := GenerateVertexCase04;  Exit; end;
      if ((xb <=R.left)and(xd>R.right)) then   // 4-angle :5
        begin Result := GenerateVertexCase05;  Exit; end;
      if (xa<= R.Left)and(xb> R.Left) and (xb <= R.Right )and (xd > R.Right) then // 5-angle   :6
        begin Result := GenerateVertexCase06;  Exit; end;
      if ((xa>R.left)and(xc<=R.right)) then  // 4-angle : 7
        begin Result := GenerateVertexCase07;  Exit; end;
      if (xa> R.Left)and(xb<= R.Right)and(xd<= R.Right)and(xc> R.Right) then // 5-angle :8
        begin Result := GenerateVertexCase08;  Exit; end;
      if (xa> R.Left)and(xb<= R.Right)and(xd> R.Right) then // 4-angle  :9
        begin Result := GenerateVertexCase09;  Exit; end;
      if (xa> R.Left)and(xa<= R.Right)and(xb> R.Right)and(xd> R.Right)and(xc> R.Right) then // 3-angle :10 top right
        begin Result := GenerateVertexCase10;  Exit; end;
      if ((xa<=R.Left)and(xb > R.Left)and(xd > R.Left)and(xd <= R.Right)and(xc > R.Left)and(xc<=R.right)) then  // 5-angle : 11
        begin Result := GenerateVertexCase11;  Exit; end;
      if (xa<= R.Left)and(xb> R.Left)and(xd<= R.Left)and(xc<= R.Right) then // 4-angle :12
        begin Result := GenerateVertexCase12;  Exit; end;
      if (xa<= R.Right)and(xa> R.Left)and(Xb> R.Right)and(xd<= R.Right) then // 4-angle :13
        begin Result := GenerateVertexCase13;  Exit; end;
      if (xa<= R.Left)and(xb> R.Left)and(Xb<= R.Right)and(xd<= R.Right)and(xc> R.Right) and (xd > R.Left)then // 6-angle :14
        begin Result := GenerateVertexCase14;  Exit; end;
      if (xd<= R.Left)and(xb> R.Right) then // 4-angle Rect :15
        begin Result := GenerateVertexCase15;  Exit; end;
      if (xa<=R.left)and(xb>=R.right) and (xd > R.Right) then  // 4-angle : 16
        begin Result := GenerateVertexCase16;  Exit; end;
      if (xa<= R.Left)and(xb> R.Left)and(Xb<= R.Right)and(xd<= R.Right)and(xc> R.Right) and (xd <= R.Left)then // 5-angle :17
        begin Result := GenerateVertexCase17;  Exit; end;
      if (xa<=R.left)and(xb>=R.right) and (xd <= R.Right) then  // 5-angle : 18
        begin Result := GenerateVertexCase18;  Exit; end;
    end;
  end;

  constructor TCanvasPolygon.CreateAsSingleColorRect(Cell:TRect; ColorToFill:TColor);
  begin // complete Rect in case if only one color to fill
    inherited Create;
    Self.VertexesCase.PolygonCase := 0;
    SetLength(Self.VertexesCase.Vertexes, 4);
    Self.VertexesCase.Vertexes[0].X:= Cell.Left; Self.VertexesCase.Vertexes[0].Y:= Cell.Bottom;
    Self.VertexesCase.Vertexes[1].X:= Cell.Left; Self.VertexesCase.Vertexes[1].Y:= Cell.Top;
    Self.VertexesCase.Vertexes[2].X:= Cell.Right; Self.VertexesCase.Vertexes[2].Y:= Cell.Top;
    Self.VertexesCase.Vertexes[3].X:= Cell.Right; Self.VertexesCase.Vertexes[3].Y:= Cell.Bottom;
    Self.ColorFill := ColorToFill;
  end;

  class function TCanvasPolygon.GenerateVertexCase01(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    iterator: integer;
  begin                                                                        //  |\
      Result.PolygonCase := 1;                                                 //  | |
    iterator := Band.xd - Cell.Left;                                           //  |  \
    SetLength(Result.Vertexes, 3);                                             //  |   \
    Result.Vertexes[0].X:= Cell.Left; Result.Vertexes[0].Y:= Cell.Bottom;      //  |____\
    Result.Vertexes[1].X:= Cell.Left; Result.Vertexes[1].Y:= Cell.Bottom - iterator - Band.BandWidth;
    Result.Vertexes[2].X:= Cell.Left + iterator + band.BandWidth; Result.Vertexes[2].Y:= Cell.Bottom;
  end;

  class function TCanvasPolygon.GenerateVertexCase02(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    iterator: integer;
    R: TRect;                                                                //   |
  begin                                                                      //   |\
    Result.PolygonCase := 2;                                                 //   | \
    iterator := Band.xd - Cell.Left;                                         //   |  \
    R := Cell;                                                               //   \   \
    with Band do                                                             //    \   \
    begin                                                                    //     \___\
      SetLength(Result.Vertexes, 4);                                         //
      Result.Vertexes[0].X:=xd; Result.Vertexes[0].Y:=R.Bottom;
      Result.Vertexes[1].X:=R.Left; Result.Vertexes[1].Y:=R.Bottom-iterator;
      Result.Vertexes[2].X:=R.Left; Result.Vertexes[2].Y:=R.Bottom-iterator-BandWidth;
      Result.Vertexes[3].X:=xc; Result.Vertexes[3].Y:=R.Bottom;
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase03(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    iterator: integer;                                   //
    R: TRect;                                            //   .
  begin                                                  //   |\
    Result.PolygonCase := 3;                             //   | \
    iterator := Band.xd - Cell.Left;                     //   |  \
    R := Cell;                                           //   |   \
    with Band do                                         //   |    \
    begin                                                //   |____|
      SetLength(Result.Vertexes, 4);
      Result.Vertexes[0].X:= R.Left; Result.Vertexes[0].Y:= R.Bottom;
      Result.Vertexes[1].X:= R.Left; Result.Vertexes[1].Y:= R.Bottom + iterator - BandWidth;
      Result.Vertexes[2].X:= R.Right; Result.Vertexes[2].Y:= R.Bottom + iterator - BandWidth + Cell.Width;
      Result.Vertexes[3].X:= R.Right; Result.Vertexes[3].Y:= R.Bottom;
    end
  end;

  class function TCanvasPolygon.GenerateVertexCase04(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    iterator: integer;
    R: TRect;
  begin
    Result.PolygonCase := 4;                //     |
    iterator := Band.xd - Cell.Left;        //     |\
    R := Cell;                              //     | \
    with Band do                            //     \  \
    begin                                   //      \__|
      Setlength(Result.Vertexes,5);
      Result.Vertexes[0].X:=xd; Result.Vertexes[0].Y:= R.Bottom;
      Result.Vertexes[1].X:= R.Left; Result.Vertexes[1].Y:= R.Bottom - iterator;
      Result.Vertexes[2].X:= R.Left; Result.Vertexes[2].Y:= R.Bottom - iterator - BandWidth;
      Result.Vertexes[3].X:= R.Right; Result.Vertexes[3].Y := R.Bottom - (xc - R.Right);
      Result.Vertexes[4]:=Point(R.Right, R.Bottom);
    end
  end;

  class function TCanvasPolygon.GenerateVertexCase05(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    iterator: integer;
    R: TRect;
  begin
    Result.PolygonCase := 5;
    iterator := Band.xd - Cell.Left;
    R := Cell;
    with Band do
    begin
      Setlength(Result.Vertexes,4);
      Result.Vertexes[0].x:=R.left;	Result.Vertexes[0].y:=R.bottom - iterator;
      Result.Vertexes[1].x:=R.left;	Result.Vertexes[1].y:=R.bottom - iterator - BandWidth;
      Result.Vertexes[2].x:=R.right;	Result.Vertexes[2].y:=R.bottom - iterator + Cell.Width - BandWidth;
      Result.Vertexes[3].x:=R.right;	Result.Vertexes[3].y:=R.bottom - iterator + Cell.Width;
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase06(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    R: TRect;                        //  _
  begin                              // | \
    Result.PolygonCase := 6;         // |  |
    R := Cell;                       //  \ |
    with Band do                     //   \|
    begin                            //    |
      SetLength(Result.Vertexes,5);
      Result.Vertexes[0].X:=R.Right; Result.Vertexes[0].Y:= R.Bottom - (xd - R.Right);
      Result.Vertexes[1].X:= R.Left; Result.Vertexes[1].Y:=R.Top + (R.Left - xa);
      Result.Vertexes[2]:=Point(R.Left,R.Top);
      Result.Vertexes[3].X:=xb; Result.Vertexes[3].Y:= R.Top;
      Result.Vertexes[4].X:=R.Right; Result.Vertexes[4].Y:=R.Top + (R.Right - xb);
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase07(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    R: TRect;
  begin
    Result.PolygonCase := 7;
    R := Cell;
    with Band do
    begin
      SetLength(Result.Vertexes,4);
      Result.Vertexes[0].x := xd;		Result.Vertexes[0].y := R.bottom;
      Result.Vertexes[1].x := xa;		Result.Vertexes[1].y := R.top;
      Result.Vertexes[2].x := xb;	  Result.Vertexes[2].y := R.Top;
      Result.Vertexes[3].x := xc;	  Result.Vertexes[3].y := R.Bottom;
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase08(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    iterator: integer;                     //
    R: TRect;                              //
  begin                                    //     _____
    Result.PolygonCase := 8;               //     \    \
    iterator := Band.xd - Cell.Left;       //      \    \
    R := Cell;                             //       \    |
    with Band do                           //        \___|
    begin
      SetLength(Result.Vertexes,5);
      Result.Vertexes[0].X := xd;      Result.Vertexes[0].Y := R.Bottom;
      Result.Vertexes[1].X := xa;      Result.Vertexes[1].Y := R.Top;
      Result.Vertexes[2].X := xb;      Result.Vertexes[2].Y := R.Top;
      Result.Vertexes[3].X := R.Right; Result.Vertexes[3].Y := R.Bottom-(iterator-Cell.Width)-BandWidth;
      Result.Vertexes[4]:=Point(R.Right,R.Bottom);
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase09(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    iterator: integer;
    R: TRect;
  begin
    Result.PolygonCase := 9;
    iterator := Band.xd - Cell.Left;
    R := Cell;
    with Band do
    begin
      SetLength(Result.Vertexes,4);
      Result.Vertexes[0].X := R.Right; Result.Vertexes[0].Y := R.Bottom - iterator + Cell.Width;
      Result.Vertexes[1].X := xa;      Result.Vertexes[1].Y := R.Top;
      Result.Vertexes[2].X := xb;      Result.Vertexes[2].Y := R.Top;
      Result.Vertexes[3].X := R.Right; Result.Vertexes[3].Y := R.Bottom - (iterator - Cell.Width) - BandWidth;
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase10(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    iterator: integer;
    R: TRect;
  begin
    Result.PolygonCase := 10;                              //  ____
    iterator := Band.xd - Cell.Left;                       //  \   |
    R := Cell;                                             //   \  |
    with Band do                                           //    \ |
    begin                                                  //     \|
      SetLength(Result.Vertexes,3);                        //      |
      Result.Vertexes[0].X := R.Right; Result.Vertexes[0].Y := R.Bottom - iterator + Cell.Width;
      Result.Vertexes[1].X :=xa;       Result.Vertexes[1].Y := R.Top;
      Result.Vertexes[2] := Point(R.Right,R.Top);
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase11(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var                                        //    ____
    R: TRect;                                //   |    \
  begin                                      //   |     \
    Result.PolygonCase := 11;                //    \     \
    R := Cell;                               //     \_____\
    with Band do                             //
    begin                                    //
      SetLength(Result.Vertexes,5);
      Result.Vertexes[0].x :=xd;		   Result.Vertexes[0].y := R.bottom;
      Result.Vertexes[1].x :=R.Left;  Result.Vertexes[1].y := R.top + (R.Left - xa);
      Result.Vertexes[2].x :=R.Left;  Result.Vertexes[2].y := R.Top;
      Result.Vertexes[3].x :=xb;	     Result.Vertexes[3].y := R.Top;
      Result.Vertexes[4].x :=xc;	     Result.Vertexes[4].y := R.Bottom;
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase12(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    R: TRect;
  begin
    Result.PolygonCase := 12;
    R := Cell;
    with Band do
    begin
      Setlength(Result.Vertexes,4);
      Result.Vertexes[0] := Point(R.Left,R.Bottom);
      Result.Vertexes[1] := Point(R.Left,R.Top);
      Result.Vertexes[2] := Point(Xb,R.Top);
      Result.Vertexes[3] := Point(Xc,R.Bottom);
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase13(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    R: TRect;
  begin
    Result.PolygonCase := 13;
    R := Cell;
    with Band do
    begin
      SetLength(Result.Vertexes,4);
      Result.Vertexes[0] := Point(Xd,R.Bottom);
      Result.Vertexes[1] := Point(Xa,R.Top);
      Result.Vertexes[2] := Point(R.Right,R.Top);
      Result.Vertexes[3] := Point(R.Right,R.Bottom);
    end;
  end;

  function PlaceVertexIntoRect(Point: TPoint; Rect: TRect): TPoint;
  begin
    Result := Point;
    if Result.X < Rect.Left then Result.X := Rect.Left;
    if Result.X > Rect.Right then Result.X := Rect.Right;
    if Result.Y > Rect.Bottom then Result.Y := Rect.Bottom;
    if Result.Y < Rect.Top then Result.Y := Rect.Top;
  end;

  class function TCanvasPolygon.GenerateVertexCase14(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    R: TRect;
  begin
    Result.PolygonCase := 14;
    R := Cell;
    with Band do
    begin                                                           //   __
      Setlength(Result.Vertexes,6);                                 //  |  \
      Result.Vertexes[0] := Point(R.Left, R.Top + (R.Left - xa) );  //  |   \
      Result.Vertexes[1] := Point(R.Left,R.Top);                    //   \   \
      Result.Vertexes[2] := Point(Xb,R.Top);                        //    \   |
      Result.Vertexes[3] := Point(R.Right, R.Top + (R.Right - xb) );//     \__|
      Result.Vertexes[4] := Point(R.Right,R.Bottom);
      Result.Vertexes[5] := Point(xd, R.Bottom);
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase15(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    R: TRect;
  begin
    Result.PolygonCase := 15;
    R := Cell;
    with Band do
    begin
      SetLength(Result.Vertexes,4);
      Result.Vertexes[0]:=Point(R.Left,R.Bottom);        //    ______
      Result.Vertexes[1]:=Point(R.Left,R.Top);           //   |      |
      Result.Vertexes[2]:=Point(R.Right,R.Top);          //   |      |
      Result.Vertexes[3]:=Point(R.Right,R.Bottom);       //   |______|
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase16(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    iterator: integer;
    R: TRect;                             //    ___
  begin                                   //   |   |
    Result.PolygonCase := 16;             //   |   |
    iterator := Band.xd - Cell.Left;      //    \  |
    R := Cell;                            //     \ |
    with Band do                          //      \|
    begin                                 //       |
      SetLength(Result.Vertexes,4);
      Result.Vertexes[0].x:=R.left;		Result.Vertexes[0].y:=R.bottom - iterator;
      Result.Vertexes[1].x:=R.left;		Result.Vertexes[1].y:=R.top;
      Result.Vertexes[2].x:=R.right;	  Result.Vertexes[2].y:=R.top;
      Result.Vertexes[3].x:=R.right;	  Result.Vertexes[3].y:=R.bottom - iterator + Cell.Width;
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase17(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    R: TRect;
  begin
    Result.PolygonCase := 17;
    R := Cell;
    with Band do
    begin                                                           //   __
      Setlength(Result.Vertexes,5);                                 //  |  \
      Result.Vertexes[0] := Point(R.Left,R.Bottom);                 //  |   \
      Result.Vertexes[1] := Point(R.Left,R.Top);                    //  |    \
      Result.Vertexes[2] := Point(Xb,R.Top);                        //  |     |
      Result.Vertexes[3] := Point(R.Right, R.Top + (R.Right - xb) );//  |_____|
      Result.Vertexes[4] := Point(R.Right,R.Bottom);
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase18(Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    iterator: integer;
    R: TRect;                             //    ___
  begin                                   //   |   |
    Result.PolygonCase := 18;             //   |   |
    iterator := Band.xd - Cell.Left;      //    \  |
    R := Cell;                            //     \_|
    with Band do
    begin
      SetLength(Result.Vertexes,5);
      Result.Vertexes[0].x := R.left;		Result.Vertexes[0].y :=R.bottom - iterator;
      Result.Vertexes[1].x := R.left;		Result.Vertexes[1].y :=R.top;
      Result.Vertexes[2].x := R.right;	  Result.Vertexes[2].y :=R.top;
      Result.Vertexes[3].x := R.right;	  Result.Vertexes[3].y :=R.bottom;
      Result.Vertexes[4].x := Band.xd;	  Result.Vertexes[4].y := R.bottom;
    end;
  end;

  constructor TCanvasPolygon.CreateAsCellToCrossParallelogram(Cell:TRect; Band: TParallelogram; ColorFill: TColor);
  begin
    inherited Create;
    FVertexPopulatorHolder := SelectorOfIntersectionCase(Cell, Band);
    if Assigned(FVertexPopulatorHolder) then
    begin
       Self.VertexesCase := FVertexPopulatorHolder(Cell,Band);
       Self.ColorFill := ColorFill;
    end;
  end;

  procedure TCanvasPolygon.Draw(Canvas: TCanvas);
  begin
    if Length(VertexesCase.Vertexes) > 0 then
    begin
      Canvas.Pen.Mode := pmCopy;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := Self.ColorFill;
      Canvas.Pen.Width := 1;
      Canvas.Brush.Color := Self.ColorFill;
      Canvas.Brush.Style := bsSolid;
      Canvas.Polygon(VertexesCase.Vertexes);
    end;
  end;

function TCanvasPolygon.SerializeAsSVGFragment: string;
  begin
     // Example: <polygon points="200,10 250,190 160,210" style="stroke:none;fill:rgb(0,255,0)" />
    Result := '';
    if Length(VertexesCase.Vertexes) > 0 then
    begin
      Result := '<polygon points='
              + SerializeVortexes();
      Result := Result + ' ' + SerializeFillStyle();
      Result := Result
              + ' />';
      Result := Result + SerializePolygonKind();
    end;
  end;

{ TCanvasPolygonInsider }
function TCanvasPolygonInsider.SerializeVortexes: string;
var
    kVortex: TPoint;
 begin
     //Example as comma-pair, separated by space: "200,10 250,190 160,210"
     for kVortex in VertexesCase.Vertexes do
        Result := Result + Format('%d,%d ', [kVortex.X, kVortex.Y]);
    Result := AnsiLeftStr(Result, Length(Result) - 1);
    Result := AnsiQuotedStr(Result, '"');
end;

class function TCellInsider.TextToSVGFragment(TextRectCanvas: TLocalCellDescriptor): string;
// Example:   <text x="10" y="40"
//                 style="font-family: Times New Roman;
//                       font-size: 44px; stroke: #00ff00; fill: #0000ff;">
//             SVG text styling
//             </text>
var
  FontName: string;
  FontSize: integer;
  FontColor: TColor;
begin
  if Assigned(TextRectCanvas.Canvas) then
  begin
    FontName := TextRectCanvas.Canvas.Font.Name;
    FontSize := TextRectCanvas.Canvas.Font.Size;
    FontColor := TextRectCanvas.Canvas.Font.Color;
  end
  else
  begin
     FontName := TCellInsider.GetDefaultFontName;
     FontSize := 10;
     FontColor := clBlack;
  end;


    Result := Format('<text x="%d" y="%d" style="font-family: %s; font-size: %dpx; fill: %s;">',
                      [ TextRectCanvas.Rect.Left + 1, TextRectCanvas.Rect.Top + 1 + FontSize,
                        FontName,
                        FontSize,
                        ColorToRGBString(FontColor)
                      ])
             + TXmlVerySimple.Escape(TextRectCanvas.Text)
             +'</text>';

end;

class function TCellInsider.SVGTagWrap(SVGFragment: string; Rect: TRect): string;
begin
//Example: <svg height="30" width="200"> ...  </svg>
  Result := Format('<svg version="1.1" baseProfile="full" '
                 + 'height="%d" width="%d" xmlns="http://www.w3.org/2000/svg">' ,
                    [Rect.Height,Rect.Width])
           + #13#10 + SVGFragment + #13#10
           +'</svg>'+ #13#10;
end;

class procedure TCellInsider.ClearCellCanvas(
  CellDescriptor: TLocalCellDescriptor);
begin
    if Assigned(CellDescriptor.Canvas) then
    begin
      CellDescriptor.Canvas.Brush.Color:=clwindow;
      CellDescriptor.Canvas.Brush.Style := bsSolid;
      CellDescriptor.Canvas.FillRect(CellDescriptor.Rect);
    end;
end;

class function TCellInsider.ColorToRGBString(Color: TColor): string;
begin
  Result := Format('rgb(%d ,%d, %d)',
                    [getRValue(Color),
                     getGValue(Color),
                     getBValue(Color)
                    ]);
end;

function TCanvasPolygonInsider.SerializeFillStyle: string;
begin
  Result :=  ' '
           + Format('style="fill: %s;"',
                    [
                       TCellInsider.ColorToRGBString(Self.ColorFill)
                    ]);
end;

function TCanvasPolygonInsider.SerializePolygonKind: string;
begin
  Result := Char(13) + char(10)
          + Format('<!-- Polygon variation Case %d -->'+#13+#10,[PolygonCase()]);
end;


class function TCellInsider.StoreCanvasPenBrush(Canvas: TCanvas): GraphicStorage;
begin
  Result.PenStyle := Canvas.Pen.Style;
  Result.PenColor := Canvas.Pen.Color;
  Result.PenWidth := Canvas.Pen.Width;
  Result.BrushColor := canvas.Brush.Color;
  Result.BrushStyle := Canvas.Brush.Style;
end;

class procedure TCellInsider.RestoreCanvasPenBrush(Storage: GraphicStorage; Canvas: TCanvas);
begin
  Canvas.Pen.Style := Storage.PenStyle;
  Canvas.Pen.Color := Storage.PenColor;
  Canvas.Pen.Width := Storage.PenWidth;
  Canvas.Brush.Color := Storage.BrushColor;
  Canvas.Brush.Style := Storage.BrushStyle ;
end;

class function TCellInsider.DrawSingleCellForSingleColor(CellDescriptor: TLocalCellDescriptor): string;
var
  Polygon: SerializablePolygon;
begin
  Polygon := TCanvasPolygon.CreateAsSingleColorRect(CellDescriptor.Rect,CellDescriptor.Colors.Items[0]);
  Polygon.Draw(CellDescriptor.Canvas);
  CellDescriptor.Canvas.TextRect(CellDescriptor.Rect,
                                  CellDescriptor.Rect.Left+1,
                                  CellDescriptor.Rect.Top+1,
                                  CellDescriptor.Text);
  Result := Polygon.SerializeAsSVGFragment();
  Result := TCellInsider.PolygonFragmentToSVG( CellDescriptor, Result);
end;

class function TCellInsider.GetDefaultFontName: string;
var B: TBitmap;
begin
   B := TBitmap.Create;
   try
     B.Height := 10;
     B.Width := 10;
     Result := B.Canvas.Font.Name;
   finally
     B.Free;
   end;
end;

class function TCellInsider.NormalizeBandshift(CellDescriptor: TLocalCellDescriptor): integer;
begin
  Result := CellDescriptor.BandShift mod ( CellDescriptor.Colors.Count * Abs(CellDescriptor.BandWidth));
end;

class function TCellInsider.NormalizeBandWidth(BandWidth: integer): integer;
begin
  Result := Abs(BandWidth);
  if Result <= 2 then
     Result := 2;
end;

class function TCellInsider.InitializeXIterator(CellDescriptor: TlocalCelldescriptor): integer;
begin
    Result:= -(CellDescriptor.Bandwidth * CellDescriptor.Colors.Count) - CellDescriptor.BandShift ;

    while Result > -(CellDescriptor.Bandwidth * CellDescriptor.Colors.Count) do
      Result:= Result - (CellDescriptor.Bandwidth * CellDescriptor.Colors.Count);
end;

class function TCellInsider.PolygonFragmentToSVG(TextRectCanvas: TLocalCellDescriptor; PolygonFragmentSVG: string): string;
var
  TempString: string;
begin
    TempString := PolygonFragmentSVG;
    TempString := TempString + TCellInsider.TextToSVGFragment( TextRectCanvas);
    Result := TempString;
    Result := TCellInsider.SVGTagWrap(TempString, TextRectCanvas.Rect);
end;

function ColorBandsOfListMovable( Canvas:TCanvas;
                                  Rect:TRect;
                                  ColorList: IList<TColor> ;
                                  BandWidth, BandShift:integer;
                                  Text:string): string;
var
  xIterator,ColorIterator,w,h,xa,xb,xc,xd:integer;

  R:TRect;
  OriginalCanvasSettings: GraphicStorage;

  Band: TParallelogram;
  Polygon: SerializablePolygon;
  TextRectCanvas: TTextRectCanvas;
  CellDescriptor: TLocalCellDescriptor;
begin
  Result := '';
  OriginalCanvasSettings := TCellInsider.StoreCanvasPenBrush(Canvas);

  try
    CellDescriptor.Canvas := Canvas;
    CellDescriptor.Text := Text;
    CellDescriptor.Rect := Rect;
    CellDescriptor.Colors := ColorList;
    CellDescriptor.BandWidth := TCellInsider.NormalizeBandWidth(BandWidth);
    CellDescriptor.BandShift := BandShift;
    CellDescriptor.BandShift := TCellInsider.NormalizeBandshift(CellDescriptor);

    if ColorList.Count = 1 then
      begin
        Result := TCellInsider.DrawSingleCellForSingleColor(CellDescriptor);
        Exit;
      end;

   TCellInsider.ClearCellCanvas(CellDescriptor);

    ColorIterator := -1;
    xIterator := TcellInsider.InitializeXIterator(CellDescriptor);

    R:=Rect;
    R.Left:=Rect.Left+1;  R.Right:=Rect.Right-1;  R.Bottom:=Rect.Bottom-1;
    w:=R.Right-R.Left;
    h:=R.Bottom-R.Top;

    begin
     while xIterator < w+h
     do
      begin
        xIterator:=xIterator+BandWidth;
        ColorIterator := ColorIterator + 1;

        if Assigned(Canvas) then
          Canvas.Brush.Color := clWindow;

        xa:= R.Left+xIterator-h; xb:=xa+BandWidth;   xc:= R.Left+xIterator+bandwidth; xd:= R.Left+xIterator;
        Band.A := Point(xa, R.Top); Band.B := Point(xb, R.Top);
        Band.C := Point(xc,R.Bottom); Band.D := Point(xd, R.Bottom);

        Polygon := TCanvasPolygon.CreateAsCellToCrossParallelogram(R,Band, ColorList[ColorIterator mod ColorList.Count] );
        Polygon.Draw(Canvas);
        Result := Result + Polygon.SerializeAsSVGFragment();
        Polygon := nil;
      end;

    end;

    if Assigned (Canvas) then
    begin
      Canvas.Brush.Style:=bsClear;
      Canvas.TextRect(Rect,Rect.Left+1,Rect.Top+1,Text);
    end;
    TextRectCanvas := TTextRectCanvas.Create(Text, Rect, Canvas);
    Result := TCellInsider.PolygonFragmentToSVG( CellDescriptor, Result);
  finally
    if Assigned(Canvas) then
        TCellInsider.RestoreCanvasPenBrush(OriginalCanvasSettings, Canvas);
  end;

end;

{ Tparallelogram }
function Tparallelogram.BandWidth: integer;
begin
  Result := Self.B.X - Self.A.X;
  if Result <> self.C.X - self.D.X then
    raise Exception.Create(
                  Format('Attempt to use malformed parallelogram: '
                       + '(%d %d) (%d %d) (%d %d) (%d %d)',
                         [xa,ya,xb,yb,xc,yc,xd,yd])
                          );
end;

{ TTextRectCanvas }

constructor TTextRectCanvas.Create(Text: string; Rect: TRect; Canvas: TCanvas);
begin
   Self.text := Text;
   Self.Rect := Rect;
   Self.Canvas := Canvas;
end;

end.
