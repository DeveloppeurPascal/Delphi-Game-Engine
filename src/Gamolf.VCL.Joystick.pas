(* C2PP
  ***************************************************************************

  Delphi Game Engine
  Copyright (c) 2021-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  File last update : 2025-05-25T17:36:34.052+02:00
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
