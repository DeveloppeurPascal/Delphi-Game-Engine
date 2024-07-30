program FMXGamepadDetectedSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  fMain in 'fMain.pas' {Form1},
  Gamolf.RTL.GamepadDetected in '..\..\..\..\src\Gamolf.RTL.GamepadDetected.pas',
  Gamolf.FMX.Joystick in '..\..\..\..\src\Gamolf.FMX.Joystick.pas',
  Gamolf.RTL.Joystick.DirectInput.Win in '..\..\..\..\src\Gamolf.RTL.Joystick.DirectInput.Win.pas',
  Gamolf.RTL.Joystick.Mac in '..\..\..\..\src\Gamolf.RTL.Joystick.Mac.pas',
  Gamolf.RTL.Joystick in '..\..\..\..\src\Gamolf.RTL.Joystick.pas',
  iOSapi.GameController in '..\..\..\..\src\iOSapi.GameController.pas',
  Macapi.GameController in '..\..\..\..\src\Macapi.GameController.pas',
  Olf.Skia.SVGToBitmap in '..\..\..\..\lib-externes\librairies\src\Olf.Skia.SVGToBitmap.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
