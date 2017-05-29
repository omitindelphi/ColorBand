unit ClrBand;

interface
uses Classes,Windows,Graphics
,Variants
,Types
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

procedure ColorBandsOfList(Canvas:TCanvas;Rect:TRect;ColorList:TColorList; BandWidth:integer; Text:string);
procedure ColorStripedRect(Canvas:TCanvas;Rect:TRect; Colors:variant; BandWidth:integer;Text:String);

implementation

{$IFNDEF VER150}
uses
  UITypes;
{$ENDIF}

function Maxim(i1,i2:integer):integer;
begin
  if i1>=i2 then Result:=i1 else Result:=i2;
end;
function Minim(i1,i2:integer):integer;
begin
  if i1<=i2 then Result:=i1 else Result:=i2;
end;


procedure ColorBandsOfList(Canvas:TCanvas;Rect:TRect;ColorList:TColorList; BandWidth:integer; Text:string);
var i,j,k,w,h,xa,xb,xc,xd,n:integer; Parr:array[0..5] of TPoint; R:TRect;  StoColor:TColor; StoStyle:TBrushStyle;
begin
n:=0;
R:=Rect;
R.Left:=Rect.Left+1;  R.Right:=Rect.Right-1;  R.Bottom:=Rect.Bottom-1;
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
w:=R.Right-R.Left;
h:=R.Bottom-R.Top;
for i:=0 to 5 do Parr[i]:=Point(1,1);
i:=0; j:=-1;
with R do
begin
 while i<w+h do
  begin
    xa:=Left+i-h; xb:=xa+BandWidth;   xc:=Left+i+bandwidth; xd:=Left+i;
      if (xa<=Left)and(xb<=Left)and(xc<=Right) then // Triangle :1
          begin
            n:=3;
            Parr[0].X:=Left; Parr[0].Y:=Bottom;
            Parr[1].X:=Left; Parr[1].Y:=Bottom-i-BandWidth;
            Parr[2].X:=Left+i+BandWidth; Parr[2].Y:=Bottom;
          end;
      if (xa<=Left)and(xb<=Left)and(xc<=Right)and(xd>left) then // 4-angle   :2
        begin        // left 4-angle;
            n:=4;
            Parr[0].X:=xd; Parr[0].Y:=Bottom;
            Parr[1].X:=Left; Parr[1].Y:=Bottom-i;
            Parr[2].X:=Left; Parr[2].Y:=Bottom-i-BandWidth;
            Parr[3].X:=xc; Parr[3].Y:=Bottom;
        end;
      if (xa<=Left)and(xb<=Left)and(xc>Right)and(xd<=left) then // 4-angle :3
        begin        // Upper 4-angle;
            n:=4;
            Parr[0].X:=Left; Parr[0].Y:=Bottom;
            Parr[1].X:=Left; Parr[1].Y:=Bottom+i-BandWidth;
            Parr[2].X:=Right; Parr[2].Y:=Bottom+i-BandWidth+w;
            Parr[3].X:=Right; Parr[3].Y:=Bottom;
        end;

      if (xa<=Left)and(xb<=Left)and(xc>Right)and(xd>left)and(xd<=Right) then
        begin        // Right Bottom 5-angle;   :4
            n:=5;
            Parr[0].X:=xd; Parr[0].Y:=Bottom;
            Parr[1].X:=Left; Parr[1].Y:=Bottom-i;
            Parr[2].X:=Left; Parr[2].Y:=Bottom-i-BandWidth;
            Parr[3].X:=Right; Parr[3].Y:=Bottom-(i-w);
            Parr[4]:=Point(Right,Bottom);
        end;
      if ((xb<=R.left)and(xd>R.right)) then
	  begin
	    n:=4;
            Parr[0].x:=R.left;	Parr[0].y:=R.bottom-i;
            Parr[1].x:=R.left;	Parr[1].y:=R.bottom-i-BandWidth;
            Parr[2].x:=R.right;	Parr[2].y:=R.bottom-i+w-BandWidth;
            Parr[3].x:=R.right;	Parr[3].y:=R.bottom-i+w;
	  end;
      if (xa<=Left)and(xb>Left)and(xc<=Right)and(xd>left) then // 5-angle   :5
        begin        // left upper 5-angle;
            n:=5;
            Parr[0].X:=xd; Parr[0].Y:=Bottom;
            Parr[1].X:=Left; Parr[1].Y:=Bottom-i;
            Parr[2]:=Point(Left,Top);
            Parr[3].X:=xb; Parr[3].Y:=Top;
            Parr[4].X:=xc; Parr[4].Y:=Bottom;
        end;
      if (xa>Left)and(xc<=Right) then // 4-angle       :6
        begin
            n:=4;
            Parr[0].X:=xd; Parr[0].Y:=Bottom;
            Parr[1].X:=xa; Parr[1].Y:=Top;
            Parr[2].X:=xb; Parr[2].Y:=Top;
            Parr[3].X:=xc; Parr[3].Y:=Bottom;
        end;
      if (xa<=Left)and(xb>Left)and(xc>Right)and(xd<=Right)and(Xd>Left) then
        begin  // 6-angle :7
            n:=6;
            Parr[0].X:=xd; Parr[0].Y:=Bottom;
            Parr[1].X:=Left; Parr[1].Y:=Bottom-i;
            Parr[2]:=Point(Left,Top);
            Parr[3].X:=xb; Parr[3].Y:=Top;
            Parr[4].X:=Right; Parr[4].Y:=Bottom-i+w;
            Parr[5]:=Point(Right,Bottom);
        end;

      if (xa>Left)and(xb<=Right)and(xd<=Right)and(xc>Right) then // 5-angle :8
        begin
            n:=5;
            Parr[0].X:=xd; Parr[0].Y:=Bottom;
            Parr[1].X:=xa; Parr[1].Y:=Top;
            Parr[2].X:=xb; Parr[2].Y:=Top;
            Parr[3].X:=Right; Parr[3].Y:=Bottom-(i-w)-BandWidth;
            Parr[4]:=Point(Right,Bottom);
        end;

      if (xa>Left)and(xb<=Right)and(xd>Right) then // 4-angle  :9
        begin
            n:=4;
            Parr[0].X:=Right; Parr[0].Y:=Bottom-i+w;
            Parr[1].X:=xa; Parr[1].Y:=Top;
            Parr[2].X:=xb; Parr[2].Y:=Top;
            Parr[3].X:=Right; Parr[3].Y:=Bottom-(i-w)-BandWidth;
        end;
      if (xa>Left)and(xb>Right)and(xd>Right) then // 3-angle :10
        begin
            n:=3;
            Parr[0].X:=Right; Parr[0].Y:=Bottom-i+w;
            Parr[1].X:=xa; Parr[1].Y:=Top;
            Parr[2]:=point(Right,Top);
        end;

      if (xa<=Left)and(xb>Right)and(xd>Left)and(xd<=Right) then // 4-angle :11
        begin
            n:=4;
            Parr[0].X:=Xd; Parr[0].Y:=Bottom;
            Parr[1].X:=Left; Parr[1].Y:=Bottom-i;
            Parr[2]:=Point(Left,Top);
            Parr[3]:=Point(Right,Bottom);
        end;

      if (xa<=Left)and(xb>Left)and(xd<=Left)and(xc<=Right) then // 4-angle :12
        begin
            n:=4;
            Parr[0]:=Point(Left,Bottom);
            Parr[1]:=Point(Left,Top);
            Parr[2]:=Point(Xb,Top);
            Parr[3]:=Point(Xc,Bottom);
        end;

      if (xa<=Right)and(xa>Left)and(Xb>Right)and(xd<=Right) then // 4-angle :13
        begin
            n:=4;
            Parr[0]:=Point(Xd,Bottom);
            Parr[1]:=Point(Xa,Top);
            Parr[2]:=Point(Right,Top);
            Parr[3]:=Point(Right,Bottom);
        end;

      if (xa<=Left)and(xb>Left)and(Xb<=Right)and(xd<=Right)and(xc>Right) then // 5-angle :14
        begin
            n:=5;
            Parr[0]:=Point(Left,Bottom);
            Parr[1]:=Point(Left,Top);
            Parr[2]:=Point(Xb,Top);
            Parr[3]:=Point(Right,Bottom-i);
            Parr[4]:=Point(Right,Bottom);
        end;

      if (xd<=Left)and(xb>Right) then // Rect :15
        begin
          n:=4;
          Parr[0]:=Point(Left,Bottom);
          Parr[1]:=Point(Left,Top);
          Parr[2]:=Point(Right,Top);
          Parr[3]:=Point(Right,Bottom);
        end;

      if ((xa<=R.left)and(xb>=R.right)) then
	  begin
	    n:=4;
            Parr[0].x:=R.left;		Parr[0].y:=R.bottom-i;
            Parr[1].x:=R.left;		Parr[1].y:=R.top;
            Parr[2].x:=R.right;	        Parr[2].y:=R.top;
            Parr[3].x:=R.right;	        Parr[3].y:=R.bottom-i+w;
	  end;

      inc (J);
      k:= j mod ColorList.Count;
      Canvas.Pen.Color:=ColorList.Items[k];
      Canvas.Brush.Color:=ColorList.Items[k];
      Canvas.Polygon(Slice(Parr,n));
    i:=i+BandWidth;
  end;
end;
Canvas.Brush.Style:=bsClear;
Canvas.TextRect(Rect,Rect.Left+1,Rect.Top+1,Text);
Canvas.Brush.Style:=StoStyle;
Canvas.Brush.Color:=StoColor;
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
