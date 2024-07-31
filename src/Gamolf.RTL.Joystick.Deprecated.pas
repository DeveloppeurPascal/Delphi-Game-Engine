unit Gamolf.RTL.Joystick.Deprecated;

interface

uses
  System.Classes,
  Gamolf.RTL.Joystick;

{$MESSAGE WARN 'Unit Gamolf.RTL.Joystick.Deprecated contains only deprecated classes for migration purpose. You should use non deprecated features in your projects.'}

type
  /// <summary>
  /// DEPRECATED component, use TDGEGamepadManager
  /// </summary>
{$IF CompilerVersion >= 33.0}
  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux or
    pfidAndroid or pfidiOS)]
{$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidOSX64 or
    pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid or
    pidAndroid64 or pidLinux64)]
{$ENDIF}

  TGamepadManager = class(TDGEGamepadManager)
  end;

  /// <summary>
  /// DEPRECATED component, use TDGEGamepad
  /// </summary>
{$IF CompilerVersion >= 33.0}
  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux or
    pfidAndroid or pfidiOS)]
{$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidOSX64 or
    pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid or
    pidAndroid64 or pidLinux64)]
{$ENDIF}

  TGamepad = class(TDGEGamepad)
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gamolf', [TGamepadManager, TGamepad]);
end;

end.
