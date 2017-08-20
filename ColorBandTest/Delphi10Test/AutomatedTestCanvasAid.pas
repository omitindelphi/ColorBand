unit AutomatedTestCanvasAid;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  DisplayCellInterface
  ;

type
  TCellCanvasAid = class
  public
    class function PerimeterTop(Canvas: TCanvas; Rect:TRect): string; static;
    class function PerimeterBottom(Canvas: TCanvas; Rect: TRect): string; static;
    class function ColorNaming(clr: TColor): string; static;
  end;
implementation


class function TCellCanvasAid.PerimeterTop(Canvas: TCanvas; Rect:TRect): string;
var
  i: integer;
  clr: Tcolor;
begin
  Result := '';
  clr := Canvas.Pixels[Rect.Left + 1, Rect.Bottom - 2];

  for i := Rect.Bottom - 2 downto Rect.Top do
  begin
    if clr <> Canvas.Pixels[Rect.Left + 1, i] then
    begin
      Result := Result + Format('%d=%s-%s,',[Rect.Bottom - 1 - i,
                                             ColorNaming(clr),
                                             ColorNaming(Canvas.Pixels[Rect.Left + 1, i]) ]);
      clr :=  Canvas.Pixels[Rect.Left + 1, i];
    end;
  end;
  for i := Rect.Left + 1 to  Rect.Right - 2 do
  begin
     if clr <> Canvas.Pixels[i, Rect.Top] then
     begin
       Result := Result + Format('%d=%s-%s,',[Rect.Bottom - 2 + i, ColorNaming(clr), ColorNaming(Canvas.Pixels[i, Rect.Top])]);
       clr := Canvas.Pixels[i, Rect.Top];
     end;
  end;
end;


class function TCellCanvasAid.PerimeterBottom(Canvas: TCanvas; Rect:TRect): string;
var
  i: integer;
  clr: Tcolor;
begin
  Result := '';

  clr := Canvas.Pixels[1, Rect.Height - 2];
  for i := 1 to  Rect.Width - 2 do
  begin
     if clr <> Canvas.Pixels[i, Rect.Height -2] then
     begin
       Result := Result + Format('%d=%s-%s,',[i, ColorNaming(clr), ColorNaming(Canvas.Pixels[i, Rect.Height - 2])]);
       clr := Canvas.Pixels[i, Rect.Height - 2];
     end;
  end;
  for i := Rect.Height - 2 downto 0 do
  begin
    if clr <> Canvas.Pixels[Rect.Width - 2, i] then
    begin
      Result := Result + Format('%d=%s-%s,',[ Rect.Width - 2 +(Rect.Height -2 - i),
                                              ColorNaming(clr),
                                              ColorNaming(Canvas.Pixels[Rect.Width - 2,
                                              i])]);
      clr :=  Canvas.Pixels[Rect.Width - 2, i];
    end;
  end;
end;

class function TCellCanvasAid.ColorNaming(clr: TColor):string;
begin
  case clr of
    clRed: Result := 'Red';
    clLime: Result := 'Green';
    else Result := 'Unknown';
  end;
end;

end.
