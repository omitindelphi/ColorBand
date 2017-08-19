unit FormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons,
  ClrBandInterface,
  ComCtrls
  ,Spring.Collections
  ,StrUtils
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
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
  private
    { Private declarations }
    FSVGFragment: string;
    FShowNeighborhood: boolean;
    FBandWidth:integer;
    FBandShift: integer;
    FColorList: IList<TColor>;
    function GetBandwidth(): integer;
    procedure SetBandwidth(Value: integer);
    procedure FillImage(BandWidth, BandShift: integer);
    function ColorNaming(clr: Tcolor): string;
  protected
    function GetSVGFragment: string;
    procedure SetTestDimWithoutWhiteBorder(const ImgX, ImgY, bandWidth, bandShift: integer);
    procedure SetSingleCellWhiteBorder(const ImgX, ImgY, bandWidth, bandShift: integer);
    procedure SetTestDimWhiteBorder(const ImgX, ImgY, bandWidth, bandShift: integer);
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


procedure TForm1.FillImage(BandWidth, BandShift: integer);
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
    Application.ProcessMessages;

    if ShowNeighborhood then
      R := Rect(B.Width div 5, B.Height div 5, (B.Width * 4 div 5), (B. Height * 4) div 5)
    else
      R := Rect(0,0, B.Width - 1, B. Height - 1);
    a := TStringlist.Create;
    try
      FSVGFragment := ColorBandsOfListMovable(B.Canvas,R, FColorList, BandWidth, BandShift, '  A some text');

      a.CommaText := FSVGFragment;
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
  FillImage( GetBandwidth, FBandShift);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   FColorList := Tcollections.CreateList<TColor>;
   FColorList.Add(clRed);
   FColorList.Add(clLime);
   TrackBar1.Position := 32;
   Label2.Caption := ',1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,';
   FBandShift := 0;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
   self.ShowNeighborhood := CheckBox1.Checked;
   FillImage(GetBandwidth, FBandShift);
end;

function TForm1.ColorNaming(clr: TColor):string;
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

procedure TForm1.SetTestDimWithoutWhiteBorder(const ImgX, ImgY, bandWidth, bandShift: integer);
begin
   Self.Hide;
   FColorList.Clear;
   FColorList.Add(clLime);
   FcolorList.Add(clRed);
   SetBandwidth(bandWidth);
   self.ShowNeighborhood := False;
   self.SetBounds(Left, Top, ImgX + 16, ImgY + 239);
   Self.Show;
   Application.ProcessMessages;
   Sleep(1000);
   Application.ProcessMessages;
end;

procedure TForm1.SetTestDimWhiteBorder(const ImgX, ImgY, bandWidth, bandShift: integer);
begin
   Self.Hide;
   FColorList.Clear;
   FColorList.Add(clLime);
   FcolorList.Add(clRed);
   SetBandwidth(bandWidth);
   self.ShowNeighborhood := True;
   self.SetBounds(Left, Top, ImgX + 16, ImgY + 239);
   Self.Show;
   Application.ProcessMessages;
   Sleep(1000);
   Application.ProcessMessages;
end;

procedure TForm1.SetSingleCellWhiteBorder(const ImgX, ImgY, bandWidth,
  bandShift: integer);
begin
   Self.Hide;
   FColorList.Clear;
   FColorList.Add(clBlue);
   SetBandwidth(bandWidth);
   self.ShowNeighborhood := True;
   self.SetBounds(Left, Top, ImgX + 16, ImgY + 239);
   Self.Show;
   Application.ProcessMessages;
   Sleep(1000);
   Application.ProcessMessages;
end;

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

function TForm1.GetSVGFragment: string;
begin
  Result := FSVGFragment;
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
  FillImage(bw, FBandShift);
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  FBandShift := TrackBar2.Position;
  FillImage(FBandWidth, FBandShift);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  BitBtn1.Left := (Self.ClientWidth - BitBtn1.Width ) div 2;
  Label1.Left := (Self.ClientWidth  - Label1.Width ) div 2;
  FillImage(GetBandwidth, FBandShift);
end;

end.
