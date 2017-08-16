unit AutomatedTestMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Buttons,
  Spring,
  Spring.Collections,
  DisplayCellInterface,
  ClrBandInterface
  ;

type


  TAutomatedTestForm = class(TForm, ISyncDisplayCell,IBandFill)
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
    FSVGReturn: TSVGTestReturn;
    FCallBackCounter: integer;
    function PopulateCellDescription(const ImgX, ImgY, BandWidth,
      BandShift: integer): TCellDescription;
  protected
    // IDisplayCell
    function SyncDisplayCell(CellDescription: TCellDescription): TSVGTestReturn;

    //IBandFill
    procedure SetTestDimWithoutWhiteBorder(const ImgX, ImgY, bandWidth, bandShift: integer);
    procedure SetTestDimWhiteBorder(const ImgX, ImgY, bandWidth, bandShift: integer);
    procedure SetSingleCellWhiteBorder(const ImgX, ImgY, bandWidth, BandShift: integer);
    procedure Teardown;
    function PerimeterTop:string;
    function PerimeterBottom: string;
    function GetSVGFragment: string;

  public
    { Public declarations }
    procedure SVGCallback(SVGContent: TSVGTestReturn);
    function FillImage( CellInfo: TCellDescription): string;
  end;

var
  AutomatedTestForm: TAutomatedTestForm;

implementation
uses
  DisplayCellModalForm;
{$R *.dfm}

{ TAtomatedTestForm }



function TAutomatedTestForm.FillImage(CellInfo: TCellDescription): string;
begin
  Result := '';
end;

function TAutomatedTestForm.GetSVGFragment: string;
begin
   Result := FSVGReturn.SVG;
end;

function TAutomatedTestForm.PerimeterBottom: string;
begin
  Result := FSVGReturn.PerimeterBottom;
end;

function TAutomatedTestForm.PerimeterTop: string;
begin
    Result := FSVGReturn.PerimeterTop;
end;

function TAutomatedTestForm.PopulateCellDescription(const ImgX, ImgY,
  BandWidth, BandShift: integer):TCellDescription;
var
  CellDescription: TCellDescription ;
begin
   CellDescription.Rect := Rect (0,0, ImgX, ImgY);
   CellDescription.Text := ' Some testing';
   CellDescription.BandWidth := BandWidth;
   CellDescription.BandShift := BandShift;
   CellDescription.Colors := TCollections.CreateList<TColor>;
   CellDescription.Colors.AddRange([clRed,clLime]);
   Result := CellDescription;
end;

procedure TAutomatedTestForm.SetSingleCellWhiteBorder(const ImgX, ImgY,
  BandWidth, BandShift: integer);
var
  CellDescription: TCellDescription ;
  SVGreturned: TSVGTestReturn;
begin
  CellDescription := PopulateCellDescription(ImgX, ImgY,BandWidth, BandShift);
  CellDescription.WhiteBorderWidth := 50;
  CellDescription.Colors.Clear;
  CellDescription.Colors.Add(clBlue);
   SVGreturned := SyncDisplayCell(CellDescription);
end;

procedure TAutomatedTestForm.SetTestDimWhiteBorder(const ImgX, ImgY, bandWidth,
  bandShift: integer);
var
  CellDescription: TCellDescription ;
  SVGreturned: TSVGTestReturn;
begin
  CellDescription := PopulateCellDescription(ImgX, ImgY,BandWidth, BandShift);
  CellDescription.WhiteBorderWidth := 50;
   SVGreturned := SyncDisplayCell(CellDescription);
end;

procedure TAutomatedTestForm.SetTestDimWithoutWhiteBorder(const ImgX, ImgY,
  bandWidth, bandShift: integer);
var
  CellDescription: TCellDescription;
  SVGreturned: TSVGTestReturn;
begin
   CellDescription := PopulateCellDescription(ImgX, ImgY, BandWidth, BandShift);
   CellDescription.WhiteBorderWidth := 0;
   SVGreturned := SyncDisplayCell(CellDescription);
end;

procedure TAutomatedTestForm.SpeedButton1Click(Sender: TObject);
var
  SVGreturned: TSVGTestReturn;
  CellDescription: TCellDescription;
begin
   CellDescription := PopulateCellDescription(200, 300, 50, 0);
   CellDescription.WhiteBorderWidth := 50;
   SVGreturned := SyncDisplayCell(CellDescription);
end;

procedure TAutomatedTestForm.SpeedButton2Click(Sender: TObject);
var
  SVGreturned: TSVGTestReturn;
  CellDescription: TCellDescription;
begin
   CellDescription := PopulateCellDescription(200, 300, 50, 0);
   CellDescription.WhiteBorderWidth := 0;
   SVGreturned := SyncDisplayCell(CellDescription);
end;

procedure TAutomatedTestForm.SVGCallback(SVGContent: TSVGTestReturn);
begin
  FSVGReturn := SVGContent;
  FCallBackCounter := FCallBackCounter + 1;
end;

function TAutomatedTestForm.SyncDisplayCell(
  CellDescription: TCellDescription): TSVGTestReturn;
const
    WaiterCounterSeconds: double = 3.0;
var
    SecondaryDisplayForm : IDisplayCell;
    OriginalCallbackCounter: integer;
    OriginalCallMoment: TDateTime;
begin

   OriginalCallbackCounter := FCallbackCounter;
   SecondaryDisplayForm := TDisplayCellForm.Create(nil);
   try
     OriginalCallMoment := Now;

     SecondaryDisplayForm.DisplayCell(CellDescription, SVGCallback);

     while (OriginalCallbackCounter = FCallbackCounter)
           or
           ( Now < OriginalCallMoment + WaiterCounterSeconds/86400.0 )
     do
     begin
        Application.Processmessages;
        Sleep(200);
     end;
   finally
      SecondaryDisplayForm.Free;
   end;

   if  OriginalCallbackCounter = FCallBackCounter then
     Raise Exception.Create (' Waiting period for SVG return expired');

   Result := FSVGReturn;
end;

procedure TAutomatedTestForm.Teardown;
begin
  Self.Close;
end;

end.
