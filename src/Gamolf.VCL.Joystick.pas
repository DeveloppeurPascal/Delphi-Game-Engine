unit Gamolf.VCL.Joystick;

{
  Accès aux contrôleurs de jeu et à leurs informations
  sous forme d'interface pour les projets Delphi VLC.

  Logiciel open source distribué sous licence AGPL.
  Open source software distributed under the AGPL license

  Copyright Patrick Prémartin

  Find the original source code on
  https://github.com/DeveloppeurPascal/Delphi-Game-Engine
}
interface

uses
  Gamolf.RTL.Joystick;

function GetGamolfJoystickService: IGamolfJoystickService;

implementation

uses
  Gamolf.RTL.Joystick.DirectInput.Win;

var
  JoystickService: IGamolfJoystickService;

function GetGamolfJoystickService: IGamolfJoystickService;
begin
  result := JoystickService;
end;

initialization

JoystickService := TGamolfJoystickWinDirectInputService.Create;

finalization

end.
