unit ColorDataSetExample_;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, DB, DBClient, StdCtrls
  ,ClrBand, ExtCtrls, ComCtrls
  ,MidasLib, DBCtrls
  ,Spring.Collections
  ;

type
  TRowStatus = (red, green, Yellow);
  TRowStatusSet = set of TRowstatus;

  TForm1 = class(TForm)
    Memo1: TMemo;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    DBGridDemo: TDBGrid;
    TrackBarColorBandWidth: TTrackBar;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    cbRed: TCheckBox;
    cbGreen: TCheckBox;
    cbYellow: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure DBGridDemoDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure TrackBarColorBandWidthChange(Sender: TObject);
    procedure ClientDataSet1FilterRecord(DataSet: TDataSet;
      var Accept: Boolean);
    procedure cbClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    procedure Populate1Execute(Sender: TObject);
    function RowIsSelected(State: TGridDrawState): boolean;
    function isFiltered: boolean;
    function getRowStatuses(const id: integer): TRowStatusSet;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ClientDataSet1.FieldDefs.Add('ID', ftInteger, 0, True);
  ClientDataSet1.FieldDefs.Add('Name', ftString, 20, True);
  ClientDataSet1.FieldDefs.Add('Date', ftDateTime, 0, True);
  ClientDataSet1.FieldDefs.Add('Salary', ftCurrency, 0, True);
  ClientDataSet1.CreateDataSet;

  Populate1Execute(nil);
  FormResize(nil);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  dbGridDemo.Columns[1].Width := (dbGridDemo.ClientWidth - dbGridDemo.Columns[0].Width - dbGridDemo.Columns[3].Width) div 2;
  dbGridDemo.Columns[2].Width := dbGridDemo.Columns[1].Width;
  cbRed.Left := GroupBox1.ClientWidth div 4 - cbRed.Width div 2;
  cbGreen.Left := GroupBox1.ClientWidth div 2 - cbGreen.Width div 2;
  cbYellow.Left := (3 * GroupBox1.ClientWidth) div 4 - cbYellow.Width div 2;
end;

function TForm1.getRowStatuses(const id: integer): TRowStatusSet;
begin
   Result := [];
   if (id mod 2)=0 then Result := Result + [Green];
   if (id mod 3 =0) then Result := Result + [Red];
   if (id mod 5)=0 then Result := Result + [Yellow];
end;

function TForm1.isFiltered: boolean;
begin
  Result := cbRed.Checked or cbGreen.Checked or cbYellow.Checked;
end;

procedure TForm1.Populate1Execute(Sender: TObject);
const
 FirstNames: array[0 .. 29] of string = ('John', 'Sarah', 'Fred', 'Beth',
 'Eric', 'Tina', 'Thomas', 'Judy', 'Robert', 'Angela', 'Tim', 'Traci',
 'David', 'Paula', 'Bruce', 'Jessica', 'Richard', 'Carla', 'James',
 'Mary','Jennifer','Ariadna','Felicia', 'Elise', 'Sonny', 'Viola', 'Alia', 'Georg', 'Stephan', 'William');
 LastNames: array[0 .. 11] of string = ('Parker', 'Johnson', 'Jones',
 'Thompson', 'Smith', 'Baker', 'Wallace', 'Harper', 'Parson', 'Edwards',
 'Mandel', 'Stone');
var
 Index: Integer;

begin
 RandSeed := 0;

 ClientDataSet1.DisableControls;
 try
   ClientDataSet1.EmptyDataSet;
   for Index := 1 to Length(FirstNames) do begin
    ClientDataSet1.Append;
    ClientDataSet1.FieldByName('ID').AsInteger := Index;
    ClientDataSet1.FieldByName('Name').AsString := FirstNames[Random(20)] + ' ' +
    LastNames[Random(12)];
    ClientDataSet1.FieldByName('Date').AsDateTime := Now() -10.0 +  Random(20);
    ClientDataSet1.FieldByName('Salary').AsFloat := 20000.0 + Random(600) * 100;
    ClientDataSet1.Post;
   end;
   ClientDataSet1.First;
 finally
   ClientDataSet1.EnableControls;
 end;

end;

function DimColor(Color:TColor; Dim:integer):TColor;
var
  R,G,B:byte;
begin
   R:=GetRValue(Color);
   G:=GetGValue(Color);
   B:=GetBValue(Color);

   if R-Dim <0 then R:=R-Dim;
   if G-Dim <0 then G:=G-Dim;
   if B-Dim <0 then B:=B-Dim;
   Result:= RGB(R,G,B);
end;

function TForm1.RowIsSelected( State: TGridDrawState): boolean;
begin
  Result := (gdSelected in State) or (gdFocused in State) ;
end;

procedure TForm1.cbClick(Sender: TObject);
begin
  DbGridDemo.DataSource.Dataset.DisableControls;
  DbGridDemo.DataSource.Dataset.Filtered := false;
  DbGridDemo.DataSource.Dataset.Filtered := isFiltered(); // to initiate re-filtering on reassignment
  DbGridDemo.DataSource.Dataset.EnableControls;
  DbGridDemo.Invalidate;
end;

procedure TForm1.ClientDataSet1FilterRecord(DataSet: TDataSet;
  var Accept: Boolean);
var
  RowStatusSet: TRowStatusSet;
begin
  Accept := True;
  RowStatusSet := getRowStatuses(Dataset.Fields[0].Asinteger);
  if isFiltered() then
    Accept :=  ( (Red in RowStatusSet)  and cbRed.Checked)
               or
               ((Green in RowStatusSet) and cbGreen.Checked)
               or
               ((Yellow in RowStatusSet) and cbYellow.Checked);

end;

procedure TForm1.DBGridDemoDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
const
  Dim=128;
var
   Dataset:Tdataset;
   id:integer;
   colorList:IList<TColor>;
   RowStatusSet: TRowStatusSet;
   cRed1,cLime1,cYellow1:Tcolor;
begin
   Dataset:=(Sender as TDBGrid).DataSource.DataSet;
   id:=Dataset.Fieldbyname('ID').AsInteger;

   RowStatusSet := getRowStatuses(id);

   DbGridDemo.Canvas.Font.Color := clBlack;
   DbGridDemo.Canvas.Font.Style := [];
   if RowIsSelected(State) then begin
          cRed1 :=clRed; cLime1:=clLime; cYellow1 := clYellow;
          DbGridDemo.Canvas.Font.Color := clBlue;
          DbGridDemo.Canvas.Font.Style := [fsBold];
   end else begin
        cred1:=DimColor(clRed,dim); cLime1:=DimColor(clLime,dim); cYellow1:=DimColor(clYellow,dim);
   end;

    colorList := TCollections.CreateList<TColor>;
    if Red in RowStatusSet  then
      colorList.Add(cRed1);
    if Green in RowStatusSet then
      colorList.Add(cLime1);
    if Yellow in RowStatusSet then
      colorList.Add(cYellow1);

    if RowStatusSet = [] then
       if RowIsSelected(State) then
          colorlist.Add(clSilver)
       else
          colorList.Add(clWindow) ;
    ColorBandsOfListMovable(DbGridDemo.Canvas, Rect, colorList,
              TrackBarColorBandWidth.Position, // it is width of color strip
              Rect.Left,                  // this is color-band shift, in pixels
              Dataset.FieldByName(Column.FieldName).AsString );


end;

procedure TForm1.TrackBarColorBandWidthChange(Sender: TObject);
begin
    DbGridDemo.Invalidate;
end;

end.
