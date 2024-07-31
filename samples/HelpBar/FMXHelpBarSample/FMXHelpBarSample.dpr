program FMXHelpBarSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  fMain in 'fMain.pas' {Form2},
  Gamolf.FMX.HelpBar in '..\..\..\src\Gamolf.FMX.HelpBar.pas',
  Olf.Skia.SVGToBitmap in '..\..\..\lib-externes\librairies\src\Olf.Skia.SVGToBitmap.pas',
  Gamolf.FMX.Joystick in '..\..\..\src\Gamolf.FMX.Joystick.pas',
  Gamolf.RTL.Joystick.DirectInput.Win in '..\..\..\src\Gamolf.RTL.Joystick.DirectInput.Win.pas',
  Gamolf.RTL.Joystick.Mac in '..\..\..\src\Gamolf.RTL.Joystick.Mac.pas',
  Gamolf.RTL.Joystick in '..\..\..\src\Gamolf.RTL.Joystick.pas',
  iOSapi.GameController in '..\..\..\src\iOSapi.GameController.pas',
  Macapi.GameController in '..\..\..\src\Macapi.GameController.pas',
  USVGKenneyInputKeys in '..\assets\KenneyInputKeys\USVGKenneyInputKeys.pas',
  USVGKenneyInputXbox in '..\assets\KenneyInputXbox\USVGKenneyInputXbox.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
