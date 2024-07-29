program FMXGamepadDetectedSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form1},
  Gamolf.RTL.GamepadDetected in '..\..\..\..\src\Gamolf.RTL.GamepadDetected.pas',
  Gamolf.FMX.Joystick in '..\..\..\..\src\Gamolf.FMX.Joystick.pas',
  Gamolf.RTL.Joystick.DirectInput.Win in '..\..\..\..\src\Gamolf.RTL.Joystick.DirectInput.Win.pas',
  Gamolf.RTL.Joystick.Mac in '..\..\..\..\src\Gamolf.RTL.Joystick.Mac.pas',
  Gamolf.RTL.Joystick in '..\..\..\..\src\Gamolf.RTL.Joystick.pas',
  iOSapi.GameController in '..\..\..\..\src\iOSapi.GameController.pas',
  Macapi.GameController in '..\..\..\..\src\Macapi.GameController.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
