unit Gamolf.FMX.Joystick;

{
  Acc�s aux contr�leurs de jeu et � leurs informations
  sous forme de service de plateforme pour les projets
  Delphi FireMonkey.

  Logiciel open source distribu� sous licence AGPL.
  Open source software distributed under the AGPL license

  Copyright Patrick Pr�martin

  Find the original source code on
  https://github.com/DeveloppeurPascal/Delphi-Game-Engine
}
interface

implementation

uses FMX.Platform, Gamolf.RTL.Joystick,
{$IF Defined(MSWINDOWS)}
  Gamolf.RTL.Joystick.Windows
{$ELSEIF Defined(IOS)}
  Gamolf.RTL.Joystick.iOS
{$ELSEIF Defined(MACOS)}
  Gamolf.RTL.Joystick.Mac
{$ENDIF};

initialization

{$IF Defined(MSWINDOWS)}
  TPlatformServices.Current.AddPlatformService(IGamolfJoystickService,
  TGamolfJoystickWindowsService.Create);
{$ELSEIF Defined(IOS)}
  TPlatformServices.Current.AddPlatformService(IGamolfJoystickService,
  TGamolfJoystickIOSService.getinstance);
{$ELSEIF Defined(MACOS)}
  TPlatformServices.Current.AddPlatformService(IGamolfJoystickService,
  TGamolfJoystickmacService.getinstance);
{$ENDIF}

finalization

if TPlatformServices.Current.SupportsPlatformService(IGamolfJoystickService)
then
  TPlatformServices.Current.RemovePlatformService(IGamolfJoystickService);

end.
