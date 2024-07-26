program OldScoresSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form1},
  Gamolf.RTL.Scores in '..\..\..\src\Gamolf.RTL.Scores.pas',
  u_scores in '..\..\..\src\u_scores.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
