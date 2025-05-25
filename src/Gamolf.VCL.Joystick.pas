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
  File last update : 2025-02-09T11:03:38.750+01:00
  Signature : bd5213fa716cf3886c18dd673c98dffe4c109c1d
  ***************************************************************************
*)

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
