unit Gamolf.FMX.Joystick;

{
  Accès aux contrôleurs de jeu et à leurs informations
  sous forme de service de plateforme pour les projets
  Delphi FireMonkey.

  Logiciel open source distribué sous licence AGPL.
  Open source software distributed under the AGPL license

  Copyright Patrick Prémartin

  Find the original source code on
  https://github.com/DeveloppeurPascal/FMXGameEngine
}
interface

implementation

{$IF Defined(MSWINDOWS)}

uses FMX.Platform, Gamolf.RTL.Joystick, Gamolf.RTL.Joystick.Windows;
{$ENDIF}

initialization

{$IF Defined(MSWINDOWS)}
  TPlatformServices.Current.AddPlatformService(IGamolfJoystickService,
  TGamolfJoystickWindowsService.Create);
{$ENDIF}

finalization

{$IF Defined(MSWINDOWS)}
  TPlatformServices.Current.RemovePlatformService(IGamolfJoystickService);
{$ENDIF}

end.
