unit Gamolf.RTL.Joystick.Deprecated;

interface

uses
  Gamolf.RTL.Joystick;

{$MESSAGE WARN 'Unit Gamolf.RTL.Joystick.Deprecated contains only deprecated classes for migration purpose. You should use non deprecated features in your projects.'}

type
  /// <summary>
  /// DEPRECATED component, use TDGEGamepadManager
  /// </summary>
  TGamepadManager = class(TDGEGamepadManager)
  end;

  /// <summary>
  /// DEPRECATED component, use TDGEGamepad
  /// </summary>
  TGamepad = class(TDGEGamepad)
  end;

procedure Register;

implementation

uses
  System.Classes;

procedure Register;
begin
  RegisterComponents('Gamolf', [TGamepadManager, TGamepad]);
end;

end.
