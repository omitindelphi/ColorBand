unit DisplayCellModalForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  DisplayCellInterface,
  AutomatedTestCanvasAid
  ;

type

  TDisplayCellForm = class(TForm,IDisplayCell)
    KillerTimer: TTimer;
    procedure KillerTimerTimer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
//    FWhiteBorderWidth: integer;
    FSVGCallBack: TSVGCallback;
    FCellInfo: TCellDescription;
    procedure AlignBorders;
    function CanvasCall(Canvas: TCanvas; CellInfo: TCellDescription;
      ClientArea: TRect): string;
    function FillBand(CellDescription: TCellDescription;
      SVGCallBack: TSVGCallback): string;
  protected
     function DisplayCell(CellDescription: TCellDescription; SVGCallBack: TSVGCallback): string;
  public
    { Public declarations }
  end;

var
  DisplayCellForm: TDisplayCellForm;

implementation

{$R *.dfm}
uses
  ClrBand;




procedure TDisplayCellForm.AlignBorders;
begin
 Self.ClientWidth := FCellInfo.Rect.Width + 2 * FCellInfo.WhiteBorderWidth;
 Self.ClientHeight := FCellInfo.Rect.Height + 2 * FCellInfo.WhiteBorderWidth;
end;

function TDisplayCellForm.FillBand(
  CellDescription: TCellDescription; SVGCallBack: TSVGCallback): string;
begin
  FSVGCallBack := SVGCallBack;
  FCellInfo := CellDescription;
  AlignBorders;
//  KillerTimer.Enabled := True;
  Self.OnPaint := FormPaint;
  Self.Show;
end;

function TDisplayCellForm.DisplayCell(
  CellDescription: TCellDescription; SVGCallBack: TSVGCallback): string;
begin
  FillBand(CellDescription, SVGCallBack);
end;



function TDisplayCellForm.CanvasCall( Canvas:TCanvas;
                                      CellInfo:TCellDescription;
                                      ClientArea: TRect
                                    ): string;
var
  DisplayRect:TRect;
  SVGReturn:  TSVGTestReturn;
begin
  if Assigned(Canvas) then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clWindow;
    Canvas.FillRect(ClientArea);
  end;
  DisplayRect := Rect( ClientArea.Left + FCellInfo.WhiteBorderWidth,
                       ClientArea.Top + FCellInfo.WhiteBorderWidth,
                       ClientArea.Left + FCellInfo.Rect.Width + FCellInfo.WhiteBorderWidth,
                       ClientArea.Top + FCellInfo.Rect.Height + FCellInfo.WhiteBorderWidth
                      );

  SVGreturn.SVG := ClrBand.ColorBandsOfListMovable(  Canvas,
                                           DisplayRect, CellInfo.Colors,
                                           CellInfo.BandWidth, CellInfo.Bandshift,
                                           CellInfo.Text
                                          );
  SVGReturn.PerimeterTop := TCellCanvasAid.PerimeterTop(Canvas, ClientArea);
  SVGreturn.PerimeterBottom := TCellCanvasAid.PerimeterBottom(Canvas, ClientArea);

  if Assigned(FSVGCallBack)
     and
     KillerTimer.Enabled
  then
    FSVGCallBack(SVGReturn);
end;

procedure TDisplayCellForm.FormPaint(Sender: TObject);
begin
  CanvasCall( Self.Canvas, FCellInfo, Self.ClientRect );
end;

procedure TDisplayCellForm.KillerTimerTimer(Sender: TObject);
begin
  KillerTimer.Enabled := False;
  Self.OnPaint := nil;
  Self.Close;
end;

end.
