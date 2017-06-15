unit FormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons,
  ClrBandInterface,
  ComCtrls
  ;

type

  TForm1 = class(TForm, IBandFill)
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    TrackBar1: TTrackBar;
    Image1: TImage;
    Label1: TLabel;
    TrackBar2: TTrackBar;
    CheckBox1: TCheckBox;
    SpeedButton1: TSpeedButton;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    FShowNeighborhood: boolean;
    FBandWidth:integer;
    FVarColors: Variant;
    function GetBandwidth(): integer;
    procedure SetBandwidth(Value: integer);
    procedure FillImage(BandWidth: integer);
    function ColorNaming(clr: Tcolor): string;
    //procedure FillFullForm(BandWidth: integer);
  protected
    procedure FillIn( BandWidth: integer);
    procedure SetTestDim(ImgX, ImgY, bandWidth: integer;  toDisplayNeighborhood: boolean);
    //procedure SetTestNeighborhood(ImgX, ImgY, bandWidth: integer);
    function PerimeterTop:string;
    function PerimeterBottom: string;
    procedure Teardown;
    property ShowNeighborhood: boolean read FShowNeighborhood write FShowNeighborhood;
  public
    { Public declarations }

  end;

var
  Form1: TForm1;

implementation
uses
  ClrBand;

{$R *.dfm}

procedure TForm1.FillImage(BandWidth: integer);
var
  B: TBitmap;
  R: TRect;
  a: TStringList;
  i: integer;
  sCases: string;
begin
  Label1.Caption := 'BandW: ' + IntToStr(Bandwidth) +' Pos ' + IntToStr(trackBar1.Position)
  +' h ' + IntTostr(Image1.height) + ' w ' + intToStr(Image1.Width);
  label1.Invalidate;
  B := TBitmap.Create;
  try
    B.Width := Image1.Width;
    B.height := Image1.Height;
    B.Canvas.Brush.Color:=clwindow;
    B.Canvas.Brush.Style:=bsSolid;
    B.Canvas.FillRect(Rect(0,0, B.Width - 1, B.Height - 1));

    if ShowNeighborhood then
      R := Rect(B.Width div 5, B.Height div 5, (B.Width * 4 div 5), (B. Height * 4) div 5)
    else
      R := Rect(0,0, B.Width - 1, B. Height - 1);
    a := TStringlist.Create;
    try
      a.CommaText := ClrBand.ColorStripedRectDebug(B.Canvas,R, FVarColors, BandWidth, '  A');
      sCases := Label2.Caption;
      for i := 0 to a.Count - 1 do
        if CompareText(a[i], 'Case') = 0 then
          if i< a.Count - 1 then
            if Pos(',' + a[i + 1] + ',', sCases) > 0 then
              System.Delete(sCases,
                     Pos(',' + a[i + 1] + ',', sCases) + 1,
                     Length(',' + a[i + 1] + ',') - 1
                    );
      Label2.Caption := sCases;
              //Label2.Caption := Label2.Caption + a[i + 1] + ',' ;
    finally
      a.Free;
    end;
    Image1.Picture.Bitmap := B;
  finally
    B.Free;
  end;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  FillImage( GetBandwidth);
end;


procedure TForm1.FillIn(BandWidth: integer);
begin
   SetTestDim(102, 240, 32, ShowNeighborhood);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   FVarColors := VarArrayCreate([0,1], varInteger);
   FVarColors[0] := clRed;
   fVarColors[1] := clLime;
   TrackBar1.Position := 32;
   Label2.Caption := ',1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,';
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
   self.ShowNeighborhood := CheckBox1.Checked;
end;

function TForm1.ColorNaming(clr: Tcolor):string;
begin
  case clr of
    clRed: Result := 'Red';
    clLime: Result := 'Green';
    else Result := 'Unknown';
  end;
end;

function TForm1.PerimeterTop: string;
var
  i: integer;
  clr: Tcolor;
  sClr: string;
  bmp: TBitmap;
begin
  Result := '';
  Bmp := Image1.Picture.Bitmap;
  clr := Bmp.Canvas.Pixels[1, Bmp.Height - 2];

  for i := Bmp.Height - 2 downto 0 do
  begin
    if clr <> Bmp.Canvas.Pixels[1, i] then
    begin
      Result := Result + Format('%d=%s-%s,',[Bmp.Height - 1 - i, ColorNaming(clr), ColorNaming(Bmp.Canvas.Pixels[1, i]) ]);
      clr :=  Bmp.Canvas.Pixels[1, i];
    end;
  end;
  for i := 1 to  Bmp.Width - 2 do
  begin
     if clr <> Bmp.Canvas.Pixels[i, 0] then
     begin
       Result := Result + Format('%d=%s-%s,',[Bmp.Height - 2 + i, ColorNaming(clr), ColorNaming(Bmp.Canvas.Pixels[i, 0])]);
       clr := Bmp.Canvas.Pixels[i, 0];
     end;
  end;
end;

procedure TForm1.SetTestDim(ImgX, ImgY, bandWidth: integer; toDisplayNeighborhood: boolean);
begin
   SetBandwidth(bandWidth);
   self.ShowNeighborhood := toDisplayNeighborhood;
   self.SetBounds(Left, Top, ImgX + 16, ImgY + 239);
   Self.Show;
   Application.ProcessMessages;
   Sleep(1000);
   Application.ProcessMessages;
end;

//procedure TForm1.SetTestNeighborhood(ImgX, ImgY, bandWidth: integer);
//begin

//end;

function TForm1.PerimeterBottom: string;
var
  i: integer;
  clr: Tcolor;
  Bmp: TBitmap;
begin
  Result := '';
  Bmp := Image1.Picture.Bitmap;
  clr := Bmp.Canvas.Pixels[1, Bmp.Height - 2];
  for i := 1 to  Bmp.Width - 2 do
  begin
     if clr <> Bmp.Canvas.Pixels[i, Bmp.Height -2] then
     begin
       Result := Result + Format('%d=%s-%s,',[i, ColorNaming(clr), ColorNaming(Bmp.Canvas.Pixels[i, Bmp.Height - 2])]);
       clr := Bmp.Canvas.Pixels[i, Bmp.Height - 2];
     end;
  end;
  for i := Bmp.Height - 2 downto 0 do
  begin
    if clr <> Bmp.Canvas.Pixels[Bmp.Width - 2, i] then
    begin
      Result := Result + Format('%d=%s-%s,',[Bmp.Width - 2 +(Bmp.Height -2 - i), ColorNaming(clr), ColorNaming(Bmp.Canvas.Pixels[Bmp.Width - 2, i])]);
      clr :=  Bmp.Canvas.Pixels[Bmp.Width - 2, i];
    end;
  end;

end;

function TForm1.GetBandwidth(): integer;
begin
  Result := FBandwidth;
end;

procedure TForm1.SetBandwidth(Value: integer);
begin
  TrackBar1.OnChange := nil;
  Trackbar1.Position := Round(Value * 100.0 / Image1.Width);
  FBandWidth := Value;
  TrackBar1.OnChange := TrackBar1Change;
end;

procedure TForm1.Teardown;
begin
  Self.Hide;
  Self.Release;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
var
  Bw: integer;
begin
  Bw := Round(Trackbar1.Position * Image1.Width /100.0);
  if Bw < 10 then
    Bw := 10;
  SetBandWidth(bw);
  FillImage(bw);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  BitBtn1.Left := (Self.ClientWidth - BitBtn1.Width ) div 2;
  Label1.Left := (Self.ClientWidth  - Label1.Width ) div 2;
  FillImage(GetBandwidth);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  SetTestDim(184, 801, -10, True);
end;

end.
