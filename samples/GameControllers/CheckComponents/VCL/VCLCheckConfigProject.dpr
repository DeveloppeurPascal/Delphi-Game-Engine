program VCLCheckConfigProject;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {Form1},
  Gamolf.RTL.Joystick.DirectInput.Win in '..\..\..\..\src\Gamolf.RTL.Joystick.DirectInput.Win.pas',
  Gamolf.RTL.Joystick in '..\..\..\..\src\Gamolf.RTL.Joystick.pas',
  Gamolf.VCL.Joystick in '..\..\..\..\src\Gamolf.VCL.Joystick.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
