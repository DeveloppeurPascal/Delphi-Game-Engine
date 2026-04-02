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
  File last update : 2025-05-25T17:36:33.967+02:00
  Signature : 171e24526964dbcdc4f6ce872285d82104b50b3d
  ***************************************************************************
*)

unit Gamolf.FMX.Joystick;

{
  Accès aux contrôleurs de jeu et à leurs informations
  sous forme de service de plateforme pour les projets
  Delphi FireMonkey.

  Logiciel open source distribué sous licence AGPL.
  Open source software distributed under the AGPL license

  Copyright Patrick Prémartin

  Find the original source code on
  https://github.com/DeveloppeurPascal/Delphi-Game-Engine
}
interface

implementation

uses
  FMX.Platform,
  Gamolf.RTL.Joystick
{$IF Defined(MSWINDOWS)}
    , Gamolf.RTL.Joystick.DirectInput.Win
{$ELSEIF Defined(IOS) or Defined(MACOS)}
    , Gamolf.RTL.Joystick.Mac
{$ENDIF};

initialization

{$IF Defined(MSWINDOWS)}
  TPlatformServices.Current.AddPlatformService(IGamolfJoystickService,
  TGamolfJoystickWinDirectInputService.Create);
{$ELSEIF Defined(IOS) or Defined(MACOS)}
  TPlatformServices.Current.AddPlatformService(IGamolfJoystickService,
  TGamolfJoystickService.Create);
{$ENDIF}

finalization

if TPlatformServices.Current.SupportsPlatformService(IGamolfJoystickService)
then
  TPlatformServices.Current.RemovePlatformService(IGamolfJoystickService);

end.
