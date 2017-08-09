unit ClrBand;

interface
uses Classes,Windows,Graphics
,Variants
,Types
,SysUtils
,AnsiStrings
;

type
TColorList=class(TList)
protected
    function Get(Index: Integer): TColor;virtual;
    procedure Put(Index: Integer; Item: TColor);virtual;
public
    function Add(Item: TColor): Integer; virtual;
    property Items[Index: Integer]: TColor read Get write Put; default;
end;
PColorList=^TColorList;

function ColorBandsOfList(Canvas:TCanvas;Rect:TRect;ColorList:TColorList; BandWidth:integer; Text:string): string;
function ColorBandsOfListMovable(Canvas:TCanvas;Rect:TRect;ColorList:TColorList; BandWidth, BandShift:integer; Text:string): string;
procedure ColorStripedRect(Canvas:TCanvas;Rect:TRect; Colors:variant; BandWidth:integer;Text:String);
function ColorStripedRectShiftDebug(Canvas:TCanvas; const Rect:TRect; const Colors:variant; const BandWidth, BandShift:integer; const Text:String): string;

implementation
{$DEFINE LOGGING}
{$IFNDEF VER150}
uses
  UITypes;
{$ENDIF}

type
  TarrPoint =  Array[0..5] of TPoint;
  TCanvasPoligon = record
                    NofSides: integer;
                    Vortex: Array Of TPoint;
                    ColorFill: Tcolor;
                    PoligonKind: integer;
                    constructor Init(NofSides: integer; ColorFill: TColor; PoligonKindNumber: integer);
                    function SerializeAsSVGFragment: string;
                  end;

  TCanvasPoligonInsider = record helper for TCanvasPoligon
    function SerializeVortexes: string;
    function SerializeFillStyle: string;
    function SerializePoligonKind: string;
  end;

  constructor TCanvasPoligon.Init(NofSides: Integer; ColorFill: TColor; PoligonKindNumber: integer);
  begin
    SetLength(Vortex, NofSides);
    Self.NofSides := NofSides;
    Self.ColorFill := ColorFill;
    Self.PoligonKind := PoligonKindNumber;
  end;

  function TCanvasPoligon.SerializeAsSVGFragment: string;
  var
    kVortex: TPoint;
  begin
     // <polygon points="200,10 250,190 160,210" style="stroke:none;fill:rgb(0,255,0)" />
    Result := '<polygon points='
            + SerializeVortexes();

    Result := Result + ' ' + SerializeFillStyle();
    Result := Result
            + ' />';
    Result := Result + SerializePoligonKind();
  end;

{ TCanvasPoligonInsider }
function TCanvasPoligonInsider.SerializeVortexes: string;
var
    kVortex: TPoint;
 begin
     // "200,10 250,190 160,210"
     for kVortex in Self.Vortex do
        Result := Result + Format('%d,%d ', [kVortex.X, kVortex.Y]);
    Result := AnsiLeftStr(Result, Length(Result) - 1);
    Result := AnsiQuotedStr(Result, '"');
end;

function TCanvasPoligonInsider.SerializeFillStyle: string;
begin
  //style="stroke:none;fill:rgb(0,255,0)"
  Result :=  ' '
           + Format('"style="stroke:none;fill:"rgb(%d , %d, %d)"',
                                                  [getRValue(ColorFill),
                                                   getGValue(ColorFill),
                                                   getBValue(Colorfill)
                                                  ]);
end;

function TCanvasPoligonInsider.SerializePoligonKind: string;
begin
  Result := Char(13) + char(10)
          + Format('<!-- Poligon variation Case %d -->',[PoligonKind]);
end;

function Maxim(i1,i2:integer):integer;
begin
  if i1>=i2 then Result:=i1 else Result:=i2;
end;
function Minim(i1,i2:integer):integer;
begin
  if i1<=i2 then Result:=i1 else Result:=i2;
end;

function Logging( GeneralizedCoordinate: integer; // from left bottom corner up and to right
                  CaseNumber: integer; // internal ID
                  ColorNumber: integer;  // number of color in list
                  AColor: TColor;
                  Parr :array of TPoint
                  ): string;
{$IFDEF LOGGING}
var
   R,G,B: Byte;
{$ENDIF}
begin
  Result := '';
{$IFDEF LOGGING}
   R := GetRValue(AColor);
   G := GetGValue(Acolor);
   B := GetBValue(AColor);
   Result := Format('GenX: %d, Case %d, ClrN %d, RGB %d %d %d',[GeneralizedCoordinate, CaseNumber, ColorNumber, R,G,B])
                             +' ' + Format('(%d %d) (%d %d) (%d %d) (%d %d) (%d %d) (%d %d)',
                             [
                             Parr[0].X, Parr[0].Y, Parr[1].X, Parr[1].Y, Parr[2].X, Parr[2].Y,
                             Parr[3].X, Parr[3].Y, Parr[4].X, Parr[4].Y, Parr[5].X, Parr[5].Y
                             ])
{$ENDIF}
end;

procedure PolyDraw(Canvas: TCanvas; ColorList: TColorList; Parr: array of TPoint; const k, n: integer);
begin
      Canvas.Pen.Color:=ColorList.Items[k];
      Canvas.Brush.Color:=ColorList.Items[k];
      Canvas.Brush.Style := bsSolid;
      Canvas.Polygon(Slice(Parr,n));
end;

function InitParr(): TarrPoint;
var
    Parr: TarrPoint;
    i: integer;
begin
  for i := 0 to 5 do
    Parr[i] := Point(0,0);
  Result := Parr;
end;


function ColorBandsOfList(Canvas:TCanvas;Rect:TRect;ColorList:TColorList; BandWidth:integer; Text:string): string;
begin
  Result := ColorBandsOfListMovable(Canvas, Rect, ColorList, BandWidth, 0, Text);
end;

procedure ColorStripedRect(Canvas:TCanvas;Rect:TRect; Colors:variant; BandWidth:integer; Text:String);
var i,c:integer;   Clist:TColorList;
begin
if VarType(Colors)=varInteger then
  begin
   c:=colors;
   Canvas.Brush.Color:=c;
   Clist:=TColorList.Create;
   try
      CList.Add(c);
      ColorBandsOfList(Canvas,Rect,CList,BandWidth,Text);
   finally
     Clist.Free;
   end;
  end;
if (VarType(Colors) and varArray)=varArray then
  begin
    CList:=TColorList.Create;
    try
      for i:=VarArrayLowBound (Colors,1) to VarArrayHighBound(Colors,1) do
         begin
           c:=Colors[i];
             Clist.Add(TColor(c));
         end;
      ColorBandsOfList(Canvas,Rect,CList,BandWidth,Text);
    finally
      Clist.free;
    end;
  end;
end;

function ColorStripedRectShiftDebug(Canvas:TCanvas; const Rect:TRect; const Colors:variant; const BandWidth, BandShift:integer; const Text:String): string;
var i,c:integer;   Clist:TColorList;
begin
if VarType(Colors)=varInteger then
  begin
   c:=colors;
   Canvas.Brush.Color:=c;
   Clist:=TColorList.Create;
   try
      CList.Add(c);
      Result := ColorBandsOfListMovable(Canvas,Rect,CList,BandWidth,BandShift,Text);
      Exit;
   finally
     Clist.Free;
   end;
  end;
if (VarType(Colors) and varArray)=varArray then
  begin
    CList:=TColorList.Create;
    try
      for i:=VarArrayLowBound (Colors,1) to VarArrayHighBound(Colors,1) do
         begin
           c:=Colors[i];
             Clist.Add(TColor(c));
         end;
      Result := ColorBandsOfListMovable(Canvas,Rect,CList,BandWidth, BandShift, Text);
    finally
      Clist.free;
    end;
  end;
end;

function ColorBandsOfListMovable(Canvas:TCanvas;Rect:TRect;ColorList:TColorList; BandWidth, BandShift:integer; Text:string): string;
var
  i,j,k,w,h,xa,xb,xc,xd,n:integer;
  Parr:array[0..5] of TPoint;
  R:TRect;
  StoColor:TColor;
  StoStyle:TBrushStyle;
  sResult: string;
  iShift: integer;
begin
  sResult := '';
  if Bandwidth <= 0 then
    Bandwidth := 2;

  iShift := BandShift mod ( ColorList.Count * Abs(BandWidth));

  StoStyle:=Canvas.Brush.Style;
  StoColor:=Canvas.Brush.Color;
  if ColorList.Count=1 then
    begin
        Canvas.Brush.Color:=ColorList.Items[0];
        Canvas.FillRect(Rect);
        Canvas.TextRect(Rect,Rect.Left+1,Rect.Top+1,Text);
        Canvas.Brush.Color:=StoColor;
        Exit;
    end;

  Canvas.Brush.Color:=clwindow;
  Canvas.FillRect(Rect);

  j:=-1;
  i:= -(Bandwidth * ColorList.Count) - iShift ;
  while i > -(Bandwidth * ColorList.Count) do
    i:= i - (Bandwidth * ColorList.Count);

  R:=Rect;
  R.Left:=Rect.Left+1;  R.Right:=Rect.Right-1;  R.Bottom:=Rect.Bottom-1;
  w:=R.Right-R.Left;
  h:=R.Bottom-R.Top;

  begin
   while i < w+h
   do
    begin
      i:=i+BandWidth;
      Canvas.Brush.Color := clWindow;
      xa:= R.Left+i-h; xb:=xa+BandWidth;   xc:= R.Left+i+bandwidth; xd:= R.Left+i;
      J := j + 1;
      k:= j mod ColorList.Count;

        if (xa <= R.Left)and(xb <= R.Left)and(xc <= R.Right)and(xd <= R.Left) and (xc > R.Left)   then // Triangle :1 bottom left
          begin
            InitParr();
            n:=3;
            Parr[0].X:= R.Left; Parr[0].Y:= R.Bottom;
            Parr[1].X:= R.Left; Parr[1].Y:= R.Bottom-i-BandWidth;
            Parr[2].X:= R.Left+i+BandWidth; Parr[2].Y:= R.Bottom;
            PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 1, k,ColorList.Items[k], Parr);
            {$ENDIF}
            Continue;
          end;

        if (xa<= R.Left)and(xb<= R.Left)and (xc > R.Left)and(xc<= R.Right)and(xd> R.Left)  then // 4-angle   :2
          begin        // left 4-angle;
              InitParr();
              n:=4;
              Parr[0].X:=xd; Parr[0].Y:=R.Bottom;
              Parr[1].X:=R.Left; Parr[1].Y:=R.Bottom-i;
              Parr[2].X:=R.Left; Parr[2].Y:=R.Bottom-i-BandWidth;
              Parr[3].X:=xc; Parr[3].Y:=R.Bottom;
              PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 2, k,ColorList.Items[k], Parr);
            {$ENDIF}
              Continue;
          end;

          if (xa<= R.Left)and(xb<= R.Left)and(xc> R.Right)and(xd<= R.Left) then // 4-angle :3
          begin
              InitParr();
              n:=4;
              Parr[0].X:= R.Left; Parr[0].Y:= R.Bottom;
              Parr[1].X:= R.Left; Parr[1].Y:= R.Bottom+i-BandWidth;
              Parr[2].X:= R.Right; Parr[2].Y:= R.Bottom+i-BandWidth+w;
              Parr[3].X:= R.Right; Parr[3].Y:= R.Bottom;
              PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 3, k,ColorList.Items[k], Parr);
            {$ENDIF}
              Continue;
          end;

        if (xa<= R.Left)and(xb<= R.Left)and(xc> R.Right)and(xd> R.Left)and(xd<= R.Right) then  // Right Bottom 5-angle;   :4
          begin
              InitParr();
              n:=5;
              Parr[0].X:=xd; Parr[0].Y:= R.Bottom;
              Parr[1].X:= R.Left; Parr[1].Y:= R.Bottom-i;
              Parr[2].X:= R.Left; Parr[2].Y:= R.Bottom-i-BandWidth;
              Parr[3].X:= R.Right; Parr[3].Y := R.Bottom - (xc - R.Right);
              Parr[4]:=Point(R.Right, R.Bottom);
              PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 4, k,ColorList.Items[k], Parr);
            {$ENDIF}
              Continue;
          end;


        if ((xb<=R.left)and(xd>R.right)) then   // 4-angle :5
          begin
              InitParr();
              n:=4;
              Parr[0].x:=R.left;	Parr[0].y:=R.bottom-i;
              Parr[1].x:=R.left;	Parr[1].y:=R.bottom-i-BandWidth;
              Parr[2].x:=R.right;	Parr[2].y:=R.bottom-i+w-BandWidth;
              Parr[3].x:=R.right;	Parr[3].y:=R.bottom-i+w;
              PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 5, k,ColorList.Items[k], Parr);
            {$ENDIF}
              Continue;
          end;
        if (xa<= R.Left)and(xb> R.Left) and (xb <= R.Right )and (xd > R.Right) then // 5-angle   :6
          begin
              InitParr();
              n:=5;
              Parr[0].X:=R.Right; Parr[0].Y:= R.Bottom - (xd - R.Right);
              Parr[1].X:= R.Left; Parr[1].Y:=R.Top + (R.Left - xa);
              Parr[2]:=Point(R.Left,R.Top);
              Parr[3].X:=xb; Parr[3].Y:= R.Top;
              Parr[4].X:=R.Right; Parr[4].Y:=R.Top + (R.Right - xb);
              PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 6, k,ColorList.Items[k], Parr);
            {$ENDIF}
              Continue;
          end;

          if ((xa>R.left)and(xc<=R.right)) then  // 4-angle : 7
          begin
            InitParr();
            n:=4;
            Parr[0].x:=xd;		Parr[0].y:=R.bottom;
            Parr[1].x:=xa;		Parr[1].y:=R.top;
            Parr[2].x:=xb;	        Parr[2].y:=R.Top;
            Parr[3].x:=xc;	        Parr[3].y:=R.Bottom;
            PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 7, k,ColorList.Items[k], Parr);
            {$ENDIF}
            Continue;
          end;

        if (xa> R.Left)and(xb<= R.Right)and(xd<= R.Right)and(xc> R.Right) then // 5-angle :8
          begin
              InitParr();
              n:=5;
              Parr[0].X:=xd; Parr[0].Y:= R.Bottom;
              Parr[1].X:=xa; Parr[1].Y:= R.Top;
              Parr[2].X:=xb; Parr[2].Y:= R.Top;
              Parr[3].X:= R.Right; Parr[3].Y:= R.Bottom-(i-w)-BandWidth;
              Parr[4]:=Point(R.Right,R.Bottom);
              PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 8, k,ColorList.Items[k], Parr);
            {$ENDIF}
              Continue;
          end;

        if (xa> R.Left)and(xb<= R.Right)and(xd> R.Right) then // 4-angle  :9
          begin
              InitParr();
              n:=4;
              Parr[0].X:= R.Right; Parr[0].Y:= R.Bottom-i+w;
              Parr[1].X:=xa; Parr[1].Y:= R.Top;
              Parr[2].X:=xb; Parr[2].Y:= R.Top;
              Parr[3].X:= R.Right; Parr[3].Y:= R.Bottom-(i-w)-BandWidth;
              PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 9, k,ColorList.Items[k], Parr);
            {$ENDIF}
              Continue;
          end;

        if (xa> R.Left)and(xa<= R.Right)and(xb> R.Right)and(xd> R.Right)and(xc> R.Right) then // 3-angle :10 top right
          begin
              InitParr();
              n:=3;
              Parr[0].X:= R.Right; Parr[0].Y:= R.Bottom-i+w;
              Parr[1].X:=xa; Parr[1].Y:= R.Top;
              Parr[2]:=point(R.Right,R.Top);
              PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 10, k,ColorList.Items[k], Parr);
            {$ENDIF}
              Continue;
          end;

          if ((xa<=R.Left)and(xb > R.Left)and(xd > R.Left)and(xd <= R.Right)and(xc > R.Left)and(xc<=R.right)) then  // 5-angle : 11
          begin
            InitParr();
            n:=5;
            Parr[0].x:=xd;		Parr[0].y:=R.bottom;
            Parr[1].x:=R.Left;		Parr[1].y:=R.top + (R.Left - xa);
            Parr[2].x:=R.Left;    parr[2].y := R.Top;
            Parr[3].x:=xb;	        Parr[3].y:=R.Top;
            Parr[4].x:=xc;	        Parr[4].y:=R.Bottom;
            PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 11, k,ColorList.Items[k], Parr);
            {$ENDIF}
            Continue;
          end;

        if (xa<= R.Left)and(xb> R.Left)and(xd<= R.Left)and(xc<= R.Right) then // 4-angle :12
          begin
              InitParr();
              n:=4;
              Parr[0]:=Point(R.Left,R.Bottom);
              Parr[1]:=Point(R.Left,R.Top);
              Parr[2]:=Point(Xb,R.Top);
              Parr[3]:=Point(Xc,R.Bottom);
              PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 12, k,ColorList.Items[k], Parr);
            {$ENDIF}
              Continue;
          end;

        if (xa<= R.Right)and(xa> R.Left)and(Xb> R.Right)and(xd<= R.Right) then // 4-angle :13
          begin
              InitParr();
              n:=4;
              Parr[0]:=Point(Xd,R.Bottom);
              Parr[1]:=Point(Xa,R.Top);
              Parr[2]:=Point(R.Right,R.Top);
              Parr[3]:=Point(R.Right,R.Bottom);
              PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 13, k,ColorList.Items[k], Parr);
            {$ENDIF}
              Continue;
          end;


        if (xa<= R.Left)and(xb> R.Left)and(Xb<= R.Right)and(xd<= R.Right)and(xc> R.Right) then // 6-angle :14
          begin
              InitParr();
              n:=6;
              Parr[0]:=Point(R.Left, R.Top + (R.Left - xa) );
              Parr[1]:=Point(R.Left,R.Top);
              Parr[2]:=Point(Xb,R.Top);
              Parr[3]:=Point(R.Right, R.Top + (R.Right - xb) );
              Parr[4]:=Point(R.Right,R.Bottom);
              Parr[5] := point(xd , R.Bottom);
              PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 14, k,ColorList.Items[k], Parr);
            {$ENDIF}
              Continue;
          end;


        if (xd<= R.Left)and(xb> R.Right) then // 4-angle Rect :15
          begin
            InitParr();
            n:=4;
            Parr[0]:=Point(R.Left,R.Bottom);
            Parr[1]:=Point(R.Left,R.Top);
            Parr[2]:=Point(R.Right,R.Top);
            Parr[3]:=Point(R.Right,R.Bottom);
            PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 15, k,ColorList.Items[k], Parr);
            {$ENDIF}
            Continue;
          end;

        if ((xa<=R.left)and(xb>=R.right)) then  // 4-angle : 16
          begin
            InitParr();
            n:=4;
            Parr[0].x:=R.left;		Parr[0].y:=R.bottom-i;
            Parr[1].x:=R.left;		Parr[1].y:=R.top;
            Parr[2].x:=R.right;	        Parr[2].y:=R.top;
            Parr[3].x:=R.right;	        Parr[3].y:=R.bottom-i+w;
            PolyDraw(Canvas,ColorList, Parr,  k, n);
            {$IFDEF LOGGING}
            sResult := sResult + Logging( i, 16, k,ColorList.Items[k], Parr);
            {$ENDIF}
            Continue;
          end;
    end;
  end;
  Canvas.Brush.Style:=bsClear;
  Canvas.TextRect(Rect,Rect.Left+1,Rect.Top+1,Text);
  Canvas.Brush.Style:=StoStyle;
  Canvas.Brush.Color:=StoColor;
  Result := sResult;
end;



{TColorList}
function TColorList.Add(Item: TColor):Integer;
begin
 Result:= inherited Add(Pointer(Item));
end;

function TColorList.Get(Index: Integer): TColor;
begin
  Result:=TColor(inherited Get(Index));
end;

procedure TColorList.Put(Index: Integer; Item: TColor);
begin
  inherited Put(Index,Pointer(Item))
end;

end.
