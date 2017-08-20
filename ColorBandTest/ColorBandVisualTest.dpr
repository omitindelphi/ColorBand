program ColorBandVisualTest;

uses
  Forms,
  FormUnit in 'FormUnit.pas' {Form1},
  ClrBand in '..\ClrBand.pas';

{$R *.res}

begin
  Application.Initialize;
  //Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
