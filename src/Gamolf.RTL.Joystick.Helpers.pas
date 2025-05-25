(* C2PP
  ***************************************************************************

  Delphi Game Engine

  Copyright 2021-2025 Patrick Prémartin under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  ***************************************************************************

  Delphi Game Engine contains libraries and components to use in VCL or
  FireMonkey game (or classic) projects.

  If you want to play sounds or musics, use game controllers, pilot your
  user interface with the keyboard or a game controller, it's the good place.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://delphigameengine.developpeur-pascal.fr

  Project site :
  https://github.com/DeveloppeurPascal/Delphi-Game-Engine

  ***************************************************************************
  File last update : 2025-02-09T11:03:38.745+01:00
  Signature : 03a3289d0fd3a6499d4623f5ba537f78abc1c822
  ***************************************************************************
*)

unit Gamolf.RTL.Joystick.Helpers;

interface

uses
  Gamolf.RTL.Joystick;

type
  TJoystickButtonsHelpers = record helper for TJoystickButtons
    function ToString: string;
  end;

  TJoystickButtonsSetHelpers = record helper for TJoystickButtonsSet
    function ToString: string;
  end;

  TJoystickDPadHelpers = record helper for TJoystickDPad
    function ToString: string;
  end;

  TJoystickAxesHelpers = record helper for TJoystickAxes
    function ToString: string;
  end;

  TJoystickAxesSetHelpers = record helper for TJoystickAxesSet
    function ToString: string;
  end;

implementation

uses
  System.TypInfo,
  System.SysUtils;

{ TJoystickButtonsSetHelpers }

function TJoystickButtonsSetHelpers.ToString: string;
var
  jb: TJoystickButtons;
begin
  result := '';
  for jb in self do
    if result.isempty then
      result := jb.ToString
    else
      result := result + ', ' + jb.ToString;
end;

{ TJoystickAxesSetHelpers }

function TJoystickAxesSetHelpers.ToString: string;
var
  ja: TJoystickAxes;
begin
  result := '';
  for ja in self do
    if result.isempty then
      result := ja.ToString
    else
      result := result + ', ' + ja.ToString;
end;

{ TJoystickButtonsHelpers }

function TJoystickButtonsHelpers.ToString: string;
begin
  result := GetEnumName(TypeInfo(TJoystickButtons), ord(self));
end;

{ TJoystickDPadHelpers }

function TJoystickDPadHelpers.ToString: string;
begin
  case TJoystickDPad(self) of
    TJoystickDPad.Top:
      result := 'Top';
    TJoystickDPad.Right:
      result := 'Right';
    TJoystickDPad.Bottom:
      result := 'Bottom';
    TJoystickDPad.Left:
      result := 'Left';
    TJoystickDPad.Center:
      result := 'Center';
    TJoystickDPad.TopRight:
      result := 'TopRight';
    TJoystickDPad.RightBottom:
      result := 'TopRight';
    TJoystickDPad.BottomLeft:
      result := 'BottomLeft';
    TJoystickDPad.LeftTop:
      result := 'LeftTop';
  else
    result := 'Unknonw (' + ord(self).ToString + ')';
  end;
end;

{ TJoystickAxesHelpers }

function TJoystickAxesHelpers.ToString: string;
begin
  case TJoystickAxes(self) of
    TJoystickAxes.LeftStickX:
      result := 'LeftStickX';
    TJoystickAxes.LeftSticky:
      result := 'LeftSticky';
    TJoystickAxes.RightStickX:
      result := 'RightStickX';
    TJoystickAxes.RightStickY:
      result := 'RightStickY';
    TJoystickAxes.LeftTrigger:
      result := 'LeftTrigger';
    TJoystickAxes.RightTrigger:
      result := 'RightTrigger';
  else
    result := 'Unknonw (' + ord(self).ToString + ')';
  end;
end;

end.
