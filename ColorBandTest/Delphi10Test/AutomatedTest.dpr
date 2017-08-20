program AutomatedTest;

uses
  Vcl.Forms,
  AutomatedTestMainForm in 'AutomatedTestMainForm.pas' {AutomatedTestForm},
  DisplayCellModalForm in 'DisplayCellModalForm.pas' {DisplayCellForm},
  DisplayCellInterface in 'DisplayCellInterface.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAutomatedTestForm, AutomatedTestForm);
  Application.Run;
end.
