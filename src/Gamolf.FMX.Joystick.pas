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
/// File last update : 26/07/2024 11:27:42
/// Signature : bd722dcca26b03d8b4122d47b1732c177b734acc
/// ***************************************************************************
/// </summary>

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
