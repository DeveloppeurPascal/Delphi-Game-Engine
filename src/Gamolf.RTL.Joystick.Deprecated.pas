/// <summary>
/// ***************************************************************************
///
/// Delphi Game Engine
///
/// Copyright 2021-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// Delphi Game Engine contains libraries and components to use in VCL or
/// FireMonkey game (or classic) projects.
///
/// If you want to play sounds or musics, use game controllers, pilot your
/// user interface with the keyboard or a game controller, it's the good place.
///
/// ***************************************************************************
/// File last update : 31/07/2024 11:18:56
/// Signature : 3e298948ad96c30201376a00bffc813f31a6529f
/// ***************************************************************************
/// </summary>

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
