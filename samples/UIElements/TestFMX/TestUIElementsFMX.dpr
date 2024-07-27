program TestUIElementsFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {frmMain},
  Gamolf.RTL.UIElements in '..\..\..\src\Gamolf.RTL.UIElements.pas',
  Gamolf.FMX.Joystick in '..\..\..\src\Gamolf.FMX.Joystick.pas',
  Gamolf.RTL.Joystick.DirectInput.Win in '..\..\..\src\Gamolf.RTL.Joystick.DirectInput.Win.pas',
  Gamolf.RTL.Joystick.Mac in '..\..\..\src\Gamolf.RTL.Joystick.Mac.pas',
  Gamolf.RTL.Joystick in '..\..\..\src\Gamolf.RTL.Joystick.pas',
  iOSapi.GameController in '..\..\..\src\iOSapi.GameController.pas',
  Macapi.GameController in '..\..\..\src\Macapi.GameController.pas',
  uDMUIElements in '..\TestVCL\uDMUIElements.pas' {dmUIElements: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
