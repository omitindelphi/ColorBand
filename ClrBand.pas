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
//,Spring
//,Spring.Collections
//,Xml.VerySimple
,ClrBandInterface
;

function ColorBandsOfListMovable( Canvas:TCanvas;
                                  Rect:TRect;
                                  ColorList: TList;//IList<TColor> ;
                                  BandWidth, BandShift:integer;
                                  Text:string): string;
implementation

//uses
//  UITypes;

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
    RectNoBorders: TRect;
    Colors: TList;//IList<TColor>;
    BandWidth: integer;
    Bandshift: integer;
    Canvas: TCanvas;
    R: TRect;
  end;

  TParallelogram = record
//    public
//      function BandWidth: integer;
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

   TpointArray = Array of TPoint; //TArray<Tpoint>;

   TVertexesCase = record
     Vertexes: TpointArray;
     PolygonCase: integer;
   end;

  SerializablePolygon = interface
  ['{BF18BFBB-53D5-431B-94F6-A40C6EF4132E}']
    function SerializeAsSVGFragment: string;
    procedure Draw( Canvas: TCanvas);
  end;

  TCrossGeneratorFunction = function( Cell: TRect; Band: TParallelogram):TVertexesCase of object;

  TCanvasPolygon = class( TInterfacedObject, SerializablePolygon)
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
    class function NoVertexCase(Cell: TRect;Band: Tparallelogram): TVertexesCase;
   public
    constructor CreateAsCellToCrossParallelogram(Cell:TRect; Band: TParallelogram; ColorFill: TColor);
    constructor CreateAsSingleColorRect(Cell: TRect; ColorToFill: Tcolor);
    function SerializeAsSVGFragment: string;
    procedure Draw(Canvas : TCanvas);
    
    function SerializeVortexes: string;
    function SerializeFillStyle: string;
    function SerializePolygonKind: string;
  end;

  {
  TCanvasPolygonInsider = class(TCanvasPolygon) //helper for TCanvasPolygon
  public
    function SerializeVortexes: string;
    function SerializeFillStyle: string;
    function SerializePolygonKind: string;
  end;
  }

  TCellInsider = class
  private
    class function ColorToRGBString( Color: TColor): string; //static;
    class function SVGTagWrap( SVGFragment: string; Rect: TRect): string; //static;
    class function PolygonFragmentToSVG( TextRectCanvas: TLocalCellDescriptor; PolygonFragmentSVG: string): string; //static;
    class function StoreCanvasPenBrush( Canvas: TCanvas): GraphicStorage; //static;
    class procedure RestoreCanvasPenBrush( Storage: GraphicStorage; Canvas: TCanvas);
    class function TextToSVGFragment( TextRectCanvas: TLocalCellDescriptor): string; //static;
    class function GetDefaultFontName: string; //static;
    class function NormalizeBandWidth( BandWidth: integer): integer; //static;
    class function NormalizeBandshift( CellDescriptor: TLocalCellDescriptor): integer; //static;
    class function DrawSingleCellForSingleColor(
      CellDescriptor: TLocalCellDescriptor): string; //static;
    class procedure ClearCellCanvas( CellDescriptor: TLocalCellDescriptor); //static;
    class function InitializeXIterator(
      CellDescriptor: TlocalCelldescriptor): integer; //static;
    class function PopulateWorkingRectWithClippedBorder(
      OriginalCell: TRect): TRect; //static;
    class function PopulateParallelogram( XIterator: integer; R: TRect;
      CellDescriptor: TLocalCellDescriptor): TParallelogram; //static;
    class function PopulateCellDescriptor( Canvas: TCanvas; Rect: TRect;
      ColorList: TList;//IList<TColor>;
      BandWidth, BandShift: integer;
      Text: string): TLocalCellDescriptor; //static;
    class procedure PlaceTextToCellCanvas( CellDescriptor: TLocalCellDescriptor); //static;
  end;

 IStripGenerator = interface
   ['{3667D8D4-B437-4699-BFAB-7817E8CF86C0}']
   function DrawStripsAndReturnSVG( CellDescriptor: TLocalCellDescriptor): string;
 end;

  IStripGeneratorFactory = interface
  ['{00C49844-7218-46A1-A882-ED58E919F2B3}']
    function CreateStripGenerator( CellDescriptor: TLocalCellDescriptor): IStripGenerator;
  end;

  TStripGenerator = class(TInterfacedObject, IStripGenerator)
  protected
     function DrawStripsAndReturnSVG( CellDescriptor: TLocalCellDescriptor): string; virtual; abstract;
  end;

  TSingleColorStripGenerator = class( TStripGenerator)
  protected
    function DrawStripsAndReturnSVG( CellDescriptor: TLocalCellDescriptor): string; override;
  end;

  TMultiColorStripGenerator = class(TStripGenerator)
  protected
    function DrawStripsAndReturnSVG( CellDescriptor: TLocalCellDescriptor): string; override;
  end;

  TStripGeneratorFactory = class(TinterfacedObject, IStripGeneratorFactory)
  private
    function CreateStripGenerator( CellDescriptor: TLocalCellDescriptor): IStripGenerator;
  end;

function RectWidth(Rect: TRect): integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function RectHeight(Rect: TRect): integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function Escape(const Value: String): String;
begin
  Result := AnsiReplaceStr(Value, '&',  '&amp;');
  Result := AnsiReplaceStr(Result, '<', '&lt;');
  Result := AnsiReplaceStr(Result, '>', '&gt;');
  Result := AnsiReplaceStr(Result, '"', '&quot;');
  Result := AnsiReplaceStr(Result,'''', '&apos;')
end;

function ParallelogramBandWidth(Prgm: TParallelogram): integer;
begin
  Result := Prgm.B.X - Prgm.A.X;
  if Result <> Prgm.C.X - Prgm.D.X then
    raise Exception.Create(
                  Format('Attempt to use malformed parallelogram: '
                       + '(%d %d) (%d %d) (%d %d) (%d %d)',
                         [Prgm.xa,Prgm.ya,Prgm.xb,Prgm.yb,Prgm.xc,Prgm.yc,Prgm.xd,Prgm.yd])
                          );
end;

  {TStripGeneratorFactory }
  function TStripGeneratorFactory.CreateStripGenerator( CellDescriptor: TLocalCellDescriptor): IStripGenerator;
  begin
     if CellDescriptor.Colors.Count <= 1 then
     begin
       Result := TSingleColorStripGenerator.Create;
     end
     else
     begin
       Result := TMultiColorStripGenerator.Create;
     end;
  end;

  { TSingleColorStripGenerator }
  function TSingleColorStripGenerator.DrawStripsAndReturnSVG( CellDescriptor: TLocalCellDescriptor): string;
  var
    LocalDesc: TLocalCellDescriptor;
  begin
    LocalDesc := CellDescriptor;
    if LocalDesc.Colors.Count = 0 then
      LocalDesc.Colors.Add(Pointer(cardinal(clWindow)));
      Result := TCellInsider.DrawSingleCellForSingleColor(LocalDesc);
  end;

    { TMultiColorStripGenerator }
  function TMultiColorStripGenerator.DrawStripsAndReturnSVG( CellDescriptor: TLocalCellDescriptor): string;
  var
    ColorIterator: integer;
    XIterator: integer;
    Band: TParallelogram;
    Polygon: SerializablePolygon;
  begin
    Result := '';
    ColorIterator := -1;
    XIterator := TCellInsider.InitializeXIterator( CellDescriptor );

    begin
     while xIterator < RectWidth(CellDescriptor.RectNoBorders)  //CellDescriptor.RectNoBorders.Width
                       +
                       RectHeight(CellDescriptor.RectNoBorders)//CellDescriptor.RectNoBorders.Height
     do
      begin
        XIterator := XIterator + CellDescriptor.BandWidth;
        ColorIterator := ColorIterator + 1;

        Band := TCellInsider.PopulateParallelogram( XIterator, CellDescriptor.RectNoBorders, CellDescriptor);
        Polygon := TCanvasPolygon.CreateAsCellToCrossParallelogram( CellDescriptor.RectNoBorders,
                                                                    Band,
                                                                    TColor(Cardinal(CellDescriptor.Colors[ColorIterator mod CellDescriptor.Colors.Count]))
                                                                  );
        Polygon.Draw(CellDescriptor.Canvas);
        Result := Result + Polygon.SerializeAsSVGFragment();
        Polygon := nil;
      end;
    end;

   TCellInsider.PlaceTextToCellCanvas( CellDescriptor);

   Result := TCellInsider.PolygonFragmentToSVG( CellDescriptor, Result);

  end;

  { TCanvasPolygon }
  function TCanvasPolygon.PolygonCase: integer;
  begin
    Result := VertexesCase.PolygonCase;
  end;

  class function TCanvasPolygon.SelectorOfIntersectionCase( Cell:TRect; Band: TParallelogram): TCrossGeneratorFunction;
  var
    R: Trect;
  begin
    R := Cell;
    Result := NoVertexCase; // no cross between Cell and parallelogram

    with Band do
    begin
      if (band.xa <= Cell.Left) and (Band.xb <= Cell.Left) and (Band.xc <= Cell.Right) and (Band.xd <= Cell.Left) and (Band.xc > Cell.Left)   then // Triangle :1 bottom left
      begin
        Result := GenerateVertexCase01;
        Exit;
      end;
      if (xa <= R.Left) and (xb <= R.Left) and (xc > R.Left) and (xc <= R.Right) and (xd> R.Left)  then // 4-angle   :2
      begin
        Result := GenerateVertexCase02;
        Exit;
      end;
      if (xa <= R.Left)and(xb <= R.Left)and(xc > R.Right)and(xd <= R.Left) then // 4-angle :3
      begin
        Result := GenerateVertexCase03;
        Exit;
      end;
      if (xa <= R.Left) and (xb<= R.Left) and (xc> R.Right) and (xd> R.Left) and (xd<= R.Right) then  // Right Bottom 5-angle;   :4
      begin
        Result := GenerateVertexCase04;
        Exit;
      end;
      if (xb <= R.left) and ( xd > R.right) then   // 4-angle :5
      begin
        Result := GenerateVertexCase05;
        Exit;
      end;
      if (xa <= R.Left) and ( xb > R.Left) and (xb <= R.Right )and (xd > R.Right) then // 5-angle   :6
      begin
        Result := GenerateVertexCase06;
        Exit;
      end;
      if (xa > R.left) and (xc <= R.Right) then  // 4-angle : 7
      begin
        Result := GenerateVertexCase07;
        Exit;
      end;
      if (xa > R.Left) and (xb <= R.Right) and(xd <= R.Right) and (xc > R.Right) then // 5-angle :8
      begin
        Result := GenerateVertexCase08;
        Exit;
      end;
      if (xa > R.Left) and (xb <= R.Right) and (xd > R.Right) then // 4-angle  :9
      begin
        Result := GenerateVertexCase09;
        Exit;
      end;
      if (xa > R.Left) and (xa <= R.Right) and (xb > R.Right) and (xd > R.Right) and (xc > R.Right) then // 3-angle :10 top right
      begin
        Result := GenerateVertexCase10;
        Exit;
      end;
      if (xa <= R.Left) and (xb > R.Left) and ( xd > R.Left) and (xd <= R.Right) and (xc > R.Left) and (xc<=R.right) then  // 5-angle : 11
      begin
        Result := GenerateVertexCase11;
        Exit;
      end;
      if (xa <= R.Left) and (xb > R.Left) and (xd <= R.Left) and (xc <= R.Right) then // 4-angle :12
      begin
        Result := GenerateVertexCase12;
        Exit;
      end;
      if (xa <= R.Right) and (xa > R.Left) and (Xb > R.Right) and (xd <= R.Right) then // 4-angle :13
      begin
        Result := GenerateVertexCase13;
        Exit;
      end;
      if (xa <= R.Left) and (xb > R.Left) and (Xb <= R.Right) and (xd <= R.Right) and (xc > R.Right) and (xd > R.Left)then // 6-angle :14
      begin
        Result := GenerateVertexCase14;
        Exit;
      end;
      if (xd <= R.Left) and (xb > R.Right) then // 4-angle Rect :15
      begin
        Result := GenerateVertexCase15;
        Exit;
      end;
      if (xa <= R.Left) and (xb >= R.Right) and (xd > R.Right) then  // 4-angle : 16
      begin
        Result := GenerateVertexCase16;
        Exit;
      end;
      if (xa <= R.Left) and (xb > R.Left) and (Xb <= R.Right) and (xd <= R.Right)
          and (xc > R.Right) and (xd <= R.Left)
      then // 5-angle :17
      begin
        Result := GenerateVertexCase17;
        Exit;
      end;
      if (xa <= R.left)and( xb >= R.right) and (xd <= R.Right) then  // 5-angle : 18
      begin
        Result := GenerateVertexCase18;
        Exit;
      end;
    end;
  end;

  constructor TCanvasPolygon.CreateAsSingleColorRect( Cell:TRect; ColorToFill:TColor);
  begin // if only one color to fill then we draw full cell box in single color
    inherited Create;
    Self.VertexesCase.PolygonCase := 0;
    SetLength(Self.VertexesCase.Vertexes, 4);
    Self.VertexesCase.Vertexes[0].X:= Cell.Left; Self.VertexesCase.Vertexes[0].Y:= Cell.Bottom;
    Self.VertexesCase.Vertexes[1].X:= Cell.Left; Self.VertexesCase.Vertexes[1].Y:= Cell.Top;
    Self.VertexesCase.Vertexes[2].X:= Cell.Right; Self.VertexesCase.Vertexes[2].Y:= Cell.Top;
    Self.VertexesCase.Vertexes[3].X:= Cell.Right; Self.VertexesCase.Vertexes[3].Y:= Cell.Bottom;
    Self.ColorFill := ColorToFill;
  end;

  class function TCanvasPolygon.GenerateVertexCase01( Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    iterator: integer;
  begin                                                                        //  |\
      Result.PolygonCase := 1;                                                 //  | |
    iterator := Band.xd - Cell.Left;                                           //  |  \
    SetLength(Result.Vertexes, 3);                                             //  |   \
    Result.Vertexes[0].X:= Cell.Left; Result.Vertexes[0].Y:= Cell.Bottom;      //  |____\
    Result.Vertexes[1].X:= Cell.Left; Result.Vertexes[1].Y:= Cell.Bottom - iterator - ParallelogramBandWidth(Band);//Band.BandWidth;
    Result.Vertexes[2].X:= Cell.Left + iterator + ParallelogramBandWidth(Band);//Band.BandWidth;
    Result.Vertexes[2].Y:= Cell.Bottom;
  end;

  class function TCanvasPolygon.GenerateVertexCase02( Cell:TRect; Band: Tparallelogram): TVertexesCase;
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
      Result.Vertexes[2].X:=R.Left; Result.Vertexes[2].Y:=R.Bottom-iterator-ParallelogramBandWidth(Band);//BandWidth;
      Result.Vertexes[3].X:=xc; Result.Vertexes[3].Y:=R.Bottom;
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase03( Cell:TRect; Band: Tparallelogram): TVertexesCase;
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
      Result.Vertexes[1].X:= R.Left; Result.Vertexes[1].Y:= R.Bottom - iterator - ParallelogramBandWidth(Band);//BandWidth;
      Result.Vertexes[2].X:= R.Right; Result.Vertexes[2].Y:= R.Bottom - iterator - ParallelogramBandWidth(Band)//BandWidth
                                                                                 + RectWidth(Cell);//Cell.Width;
      Result.Vertexes[3].X:= R.Right; Result.Vertexes[3].Y:= R.Bottom;
    end
  end;

  class function TCanvasPolygon.GenerateVertexCase04( Cell:TRect; Band: Tparallelogram): TVertexesCase;
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
      Result.Vertexes[2].X:= R.Left; Result.Vertexes[2].Y:= R.Bottom - iterator - ParallelogramBandWidth(Band);//BandWidth;
      Result.Vertexes[3].X:= R.Right; Result.Vertexes[3].Y := R.Bottom - (xc - R.Right);
      Result.Vertexes[4]:=Point(R.Right, R.Bottom);
    end
  end;

  class function TCanvasPolygon.GenerateVertexCase05( Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    iterator: integer;
    R: TRect;
  begin                                         // |
    Result.PolygonCase := 5;                    // |\
    iterator := Band.xd - Cell.Left;            // | \
    R := Cell;                                  // \  |
    with Band do                                //  \ |
    begin                                       //   \|
      Setlength(Result.Vertexes,4);             //    |
      Result.Vertexes[0].x:=R.left;	Result.Vertexes[0].y:=R.bottom - iterator;
      Result.Vertexes[1].x:=R.left;	Result.Vertexes[1].y:=R.bottom - iterator - ParallelogramBandWidth(Band);//BandWidth;
      Result.Vertexes[2].x:=R.right;	Result.Vertexes[2].y:=R.bottom - iterator + RectWidth(Cell)//Cell.Width
                                                                                  - ParallelogramBandWidth(Band);//BandWidth;
      Result.Vertexes[3].x:=R.right;	Result.Vertexes[3].y:=R.bottom - iterator + RectWidth(Cell);//Cell.Width;
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase06( Cell:TRect; Band: Tparallelogram): TVertexesCase;
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

  class function TCanvasPolygon.GenerateVertexCase07( Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    R: TRect;                        // ____
  begin                              // \   \
    Result.PolygonCase := 7;         //  \   \
    R := Cell;                       //   \   \
    with Band do                     //    \   \
    begin                            //     \___\
      SetLength(Result.Vertexes,4);
      Result.Vertexes[0].x := xd;		Result.Vertexes[0].y := R.bottom;
      Result.Vertexes[1].x := xa;		Result.Vertexes[1].y := R.top;
      Result.Vertexes[2].x := xb;	  Result.Vertexes[2].y := R.Top;
      Result.Vertexes[3].x := xc;	  Result.Vertexes[3].y := R.Bottom;
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase08( Cell:TRect; Band: Tparallelogram): TVertexesCase;
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
      Result.Vertexes[3].X := R.Right; Result.Vertexes[3].Y := R.Bottom - (iterator - RectWidth(Cell)) //Cell.Width)
                                                                        - ParallelogramBandWidth(Band);//BandWidth;
      Result.Vertexes[4]:=Point(R.Right,R.Bottom);
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase09( Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    iterator: integer;                           //    ____
    R: TRect;                                    //    \   \
  begin                                          //     \   \
    Result.PolygonCase := 9;                     //      \   |
    iterator := Band.xd - Cell.Left;             //       \  |
    R := Cell;                                   //        \ |
    with Band do                                 //         \|
    begin                                        //          |
      SetLength(Result.Vertexes,4);
      Result.Vertexes[0].X := R.Right; Result.Vertexes[0].Y := R.Bottom - iterator + RectWidth(Cell);//Cell.Width;
      Result.Vertexes[1].X := xa;      Result.Vertexes[1].Y := R.Top;
      Result.Vertexes[2].X := xb;      Result.Vertexes[2].Y := R.Top;
      Result.Vertexes[3].X := R.Right; Result.Vertexes[3].Y := R.Bottom - (iterator - RectWidth(Cell))//Cell.Width)
                                                                        - ParallelogramBandWidth(Band);//BandWidth;
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase10( Cell:TRect; Band: Tparallelogram): TVertexesCase;
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
      Result.Vertexes[0].X := R.Right; Result.Vertexes[0].Y := R.Bottom - iterator + RectWidth(Cell);//Cell.Width;
      Result.Vertexes[1].X :=xa;       Result.Vertexes[1].Y := R.Top;
      Result.Vertexes[2] := Point(R.Right,R.Top);
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase11( Cell:TRect; Band: Tparallelogram): TVertexesCase;
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

  class function TCanvasPolygon.GenerateVertexCase12( Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    R: TRect;                                         //  ____
  begin                                               // |    \
    Result.PolygonCase := 12;                         // |     \
    R := Cell;                                        // |______\
    with Band do
    begin
      Setlength(Result.Vertexes,4);
      Result.Vertexes[0] := Point(R.Left,R.Bottom);
      Result.Vertexes[1] := Point(R.Left,R.Top);
      Result.Vertexes[2] := Point(Xb,R.Top);
      Result.Vertexes[3] := Point(Xc,R.Bottom);
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase13( Cell:TRect; Band: Tparallelogram): TVertexesCase;
  var
    R: TRect;                                       //   ________
  begin                                             //   \       |
    Result.PolygonCase := 13;                       //    \      |
    R := Cell;                                      //     \     |
    with Band do                                    //      \____|
    begin                                           //
      SetLength(Result.Vertexes,4);
      Result.Vertexes[0] := Point(Xd,R.Bottom);
      Result.Vertexes[1] := Point(Xa,R.Top);
      Result.Vertexes[2] := Point(R.Right,R.Top);
      Result.Vertexes[3] := Point(R.Right,R.Bottom);
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase14( Cell:TRect; Band: Tparallelogram): TVertexesCase;
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

  class function TCanvasPolygon.GenerateVertexCase15( Cell:TRect; Band: Tparallelogram): TVertexesCase;
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

  class function TCanvasPolygon.GenerateVertexCase16( Cell:TRect; Band: Tparallelogram): TVertexesCase;
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
      Result.Vertexes[3].x:=R.right;	  Result.Vertexes[3].y:=R.bottom - iterator + RectWidth(Cell); //Cell.Width;
    end;
  end;

  class function TCanvasPolygon.GenerateVertexCase17( Cell:TRect; Band: Tparallelogram): TVertexesCase;
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

  class function TCanvasPolygon.GenerateVertexCase18( Cell:TRect; Band: Tparallelogram): TVertexesCase;
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

class function TCanvasPolygon.NoVertexCase(Cell: TRect;
  Band: Tparallelogram): TVertexesCase;
begin
  Result.PolygonCase := -1;
  Setlength(Result.Vertexes, 0);
end;

constructor TCanvasPolygon.CreateAsCellToCrossParallelogram( Cell:TRect; Band: TParallelogram; ColorFill: TColor);
  begin
    inherited Create;
    FVertexPopulatorHolder := SelectorOfIntersectionCase( Cell, Band);
    if Assigned(FVertexPopulatorHolder) then
    begin
       Self.VertexesCase := FVertexPopulatorHolder( Cell,Band);
       Self.ColorFill := ColorFill;
    end;
  end;

  procedure TCanvasPolygon.Draw( Canvas: TCanvas);
  begin
    if Assigned(Canvas) then
      Canvas.Brush.Color := clWindow;

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
function {TCanvasPolygonInsider}TCanvasPolygon.SerializeVortexes: string;
var
    kVortex: TPoint;
    jterator: integer;
 begin
     //Example as comma-pair, separated by space: "200,10 250,190 160,210"
//     for kVortex in VertexesCase.Vertexes do
//        Result := Result + Format('%d,%d ', [kVortex.X, kVortex.Y]);
    Result := '';
    for jterator := 0 to High(VertexesCase.Vertexes) do
    begin
        kVortex := VertexesCase.Vertexes[jterator];
        Result := Result + Format('%d,%d ', [kVortex.X, kVortex.Y]);
    end;

    Result := AnsiLeftStr(Result, Length(Result) - 1);
    Result := AnsiQuotedStr(Result, '"');
end;


function {TCanvasPolygonInsider}TCanvasPolygon.SerializeFillStyle: string;
begin
  Result :=  ' '
           + Format('style="fill: %s;"',
                    [
                       TCellInsider.ColorToRGBString(Self.ColorFill)
                    ]);
end;

function {TCanvasPolygonInsider}TCanvasPolygon.SerializePolygonKind: string;
begin
  Result := Char(13) + char(10)
          + Format('<!-- Polygon variation Case %d -->'+#13+#10,[PolygonCase()]);
end;

{ TCellInsider }
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
             + {TXmlVerySimple.}Escape(TextRectCanvas.Text)
             +'</text>';

end;

class function TCellInsider.SVGTagWrap( SVGFragment: string; Rect: TRect): string;
begin
//Example: <svg height="30" width="200"> ...  </svg>
  Result := Format('<svg version="1.1" baseProfile="full" '
                 + 'height="%d" width="%d" xmlns="http://www.w3.org/2000/svg">' ,
                    [RectHeight(Rect), RectWidth(Rect)]//[Rect.Height,Rect.Width]
                  )
           + #13#10 + SVGFragment + #13#10
           +'</svg>'+ #13#10;
end;

class procedure TCellInsider.ClearCellCanvas( CellDescriptor: TLocalCellDescriptor);
begin
    if Assigned(CellDescriptor.Canvas) then
    begin
      CellDescriptor.Canvas.Brush.Color:=clwindow;
      CellDescriptor.Canvas.Brush.Style := bsSolid;
      CellDescriptor.Canvas.FillRect(CellDescriptor.Rect);
    end;
end;

class function TCellInsider.ColorToRGBString( Color: TColor): string;
var
  uColor: Longint;
begin
  uColor := ColorToRGB(Color); // to deal with system colors and avoid exception if Range Checking is ON in Project properties - Compiler
  Result := Format('rgb(%d ,%d, %d)',
                    [getRValue(uColor),
                     getGValue(uColor),
                     getBValue(uColor)
                    ]);
end;

class function TCellInsider.StoreCanvasPenBrush( Canvas: TCanvas): GraphicStorage;
begin
  Result.PenStyle := Canvas.Pen.Style;
  Result.PenColor := Canvas.Pen.Color;
  Result.PenWidth := Canvas.Pen.Width;
  Result.BrushColor := canvas.Brush.Color;
  Result.BrushStyle := Canvas.Brush.Style;
end;

class procedure TCellInsider.RestoreCanvasPenBrush( Storage: GraphicStorage; Canvas: TCanvas);
begin
  Canvas.Pen.Style := Storage.PenStyle;
  Canvas.Pen.Color := Storage.PenColor;
  Canvas.Pen.Width := Storage.PenWidth;
  Canvas.Brush.Color := Storage.BrushColor;
  Canvas.Brush.Style := Storage.BrushStyle ;
end;

class function TCellInsider.DrawSingleCellForSingleColor( CellDescriptor: TLocalCellDescriptor): string;
var
  Polygon: SerializablePolygon;
begin
  Polygon := TCanvasPolygon.CreateAsSingleColorRect( CellDescriptor.Rect, TColor(Cardinal(CellDescriptor.Colors.Items[0])));
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

class function TCellInsider.NormalizeBandshift( CellDescriptor: TLocalCellDescriptor): integer;
begin
  if CellDescriptor.Colors.Count = 0 then
    Result := 0
  else
    Result := CellDescriptor.BandShift mod ( CellDescriptor.Colors.Count * Abs(CellDescriptor.BandWidth));
end;

class function TCellInsider.NormalizeBandWidth( BandWidth: integer): integer;
begin
  Result := Abs(BandWidth);
  if Result <= 2 then
     Result := 2;
end;

class function TCellInsider.PopulateWorkingRectWithClippedBorder( OriginalCell:TRect): TRect;
begin
    Result := OriginalCell;
    Result.Left := Result.Left+1;
    Result.Right := Result.Right-1;
    Result.Bottom:=Result.Bottom-1;
end;

class function TCellInsider.InitializeXIterator( CellDescriptor: TlocalCelldescriptor): integer;
begin
    Result:= -(CellDescriptor.Bandwidth * CellDescriptor.Colors.Count) - CellDescriptor.BandShift ;

    while Result > -(CellDescriptor.Bandwidth * CellDescriptor.Colors.Count) do
      Result:= Result - (CellDescriptor.Bandwidth * CellDescriptor.Colors.Count);
end;

class function TCellInsider.PopulateCellDescriptor( Canvas:TCanvas;
                                  Rect:TRect;
                                  ColorList: TList;//IList<TColor> ;
                                  BandWidth, BandShift:integer;
                                  Text:string): TLocalCellDescriptor;
begin
    Result.Canvas := Canvas;
    Result.Text := Text;
    Result.Rect := Rect;
    Result.RectNoBorders :=  TCellInsider.PopulateWorkingRectWithClippedBorder(Rect);
    Result.Colors := ColorList;
    Result.BandWidth := TCellInsider.NormalizeBandWidth( BandWidth);
    Result.BandShift := BandShift;
    Result.BandShift := TCellInsider.NormalizeBandshift( Result);
end;

class function TCellInsider.PopulateParallelogram( XIterator: integer; R:TRect; CellDescriptor: TLocalCellDescriptor): TParallelogram;
var
  xa,xb,xc,xd: integer;
begin
  xa:= R.Left + XIterator - RectHeight(R);//R.Height;
  xb:=xa + CellDescriptor.BandWidth;
  xc:= R.Left + xIterator + cellDescriptor.Bandwidth;
  xd:= R.Left + xIterator;
  Result.A := Point(xa, R.Top); Result.B := Point(xb, R.Top);
  Result.C := Point(xc,R.Bottom); Result.D := Point(xd, R.Bottom);
end;

class procedure TCellInsider.PlaceTextToCellCanvas( CellDescriptor: TLocalCellDescriptor);
begin
    if Assigned (CellDescriptor.Canvas) then
    begin
      CellDescriptor.Canvas.Brush.Style:=bsClear;
      CellDescriptor.Canvas.TextRect( CellDescriptor.Rect,
                                      CellDescriptor.Rect.Left+1,
                                      CellDescriptor.Rect.Top+1,
                                      CellDescriptor.Text);
    end;
end;

class function TCellInsider.PolygonFragmentToSVG( TextRectCanvas: TLocalCellDescriptor; PolygonFragmentSVG: string): string;
var
  TempString: string;
begin
    TempString := PolygonFragmentSVG;
    TempString := TempString + TCellInsider.TextToSVGFragment( TextRectCanvas);
    Result := TempString;
    Result := TCellInsider.SVGTagWrap( TempString, TextRectCanvas.Rect);
end;



function ColorBandsOfListMovable( Canvas:TCanvas;
                                  Rect:TRect;
                                  ColorList: TList;//IList<TColor> ;
                                  BandWidth, BandShift:integer;
                                  Text:string): string;
var
  OriginalCanvasSettings: GraphicStorage;
  CellDescriptor: TLocalCellDescriptor;
  StripGeneratorFactory: IStripGeneratorFactory;
  StripGenerator: IStripGenerator;
begin
  Result := '';
  OriginalCanvasSettings := TCellInsider.StoreCanvasPenBrush(Canvas);

  try
    CellDescriptor := TCellInsider.PopulateCellDescriptor(Canvas,
                                                          Rect,
                                                          ColorList,
                                                          BandWidth, BandShift,
                                                          Text);
    TCellInsider.ClearCellCanvas(CellDescriptor);
    StripGeneratorFactory := TStripGeneratorFactory.Create;
    StripGenerator := StripGeneratorFactory.CreateStripGenerator(CellDescriptor);
     Result := StripGenerator.DrawStripsAndReturnSVG(CellDescriptor);
   finally
    if Assigned(Canvas) then
        TCellInsider.RestoreCanvasPenBrush(OriginalCanvasSettings, Canvas);
  end;

end;

{ Tparallelogram }
{
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
}



end.
