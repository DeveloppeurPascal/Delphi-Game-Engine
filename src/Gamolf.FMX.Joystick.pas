unit Gamolf.FMX.Joystick;

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
