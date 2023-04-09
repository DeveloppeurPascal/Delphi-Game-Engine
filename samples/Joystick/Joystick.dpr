program Joystick;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form2},
  Gamolf.FMX.Joystick in '..\..\src\Gamolf.FMX.Joystick.pas',
  Gamolf.FMX.Joystick.Windows in '..\..\src\Gamolf.FMX.Joystick.Windows.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
