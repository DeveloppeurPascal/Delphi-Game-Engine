program Joystick;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form2},
  Gamolf.FMX.Joystick in '..\..\src\Gamolf.FMX.Joystick.pas',
  Gamolf.RTL.Joystick.Windows in '..\..\src\Gamolf.RTL.Joystick.Windows.pas',
  Gamolf.RTL.Joystick in '..\..\src\Gamolf.RTL.Joystick.pas',
  Gamolf.RTL.Joystick.Mac in '..\..\src\Gamolf.RTL.Joystick.Mac.pas',
  Macapi.GameController in '..\..\src\Macapi.GameController.pas',
  iOSapi.GameController in '..\..\src\iOSapi.GameController.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
