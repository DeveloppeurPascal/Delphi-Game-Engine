program Joystick;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form2},
  Gamolf.FMX.Joystick in '..\..\..\src\Gamolf.FMX.Joystick.pas',
  Gamolf.RTL.Joystick.DirectInput.Win in '..\..\..\src\Gamolf.RTL.Joystick.DirectInput.Win.pas',
  Gamolf.RTL.Joystick.Mac in '..\..\..\src\Gamolf.RTL.Joystick.Mac.pas',
  Gamolf.RTL.Joystick in '..\..\..\src\Gamolf.RTL.Joystick.pas',
  iOSapi.GameController in '..\..\..\src\iOSapi.GameController.pas',
  Macapi.GameController in '..\..\..\src\Macapi.GameController.pas',
  Gamolf.RTL.Joystick.Helpers in '..\..\..\src\Gamolf.RTL.Joystick.Helpers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
