program TestUIElementsVCL;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {frmMain},
  Gamolf.RTL.Joystick in '..\..\..\src\Gamolf.RTL.Joystick.pas',
  Gamolf.RTL.UIElements in '..\..\..\src\Gamolf.RTL.UIElements.pas',
  Gamolf.VCL.Joystick in '..\..\..\src\Gamolf.VCL.Joystick.pas',
  Gamolf.RTL.Joystick.DirectInput.Win in '..\..\..\src\Gamolf.RTL.Joystick.DirectInput.Win.pas',
  uDMUIElements in 'uDMUIElements.pas' {dmUIElements: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
