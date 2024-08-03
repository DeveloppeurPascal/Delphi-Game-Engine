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
/// File last update : 31/07/2024 14:24:24
/// Signature : 44e94fc486d7303d1ef802a84106e2e012062687
/// ***************************************************************************
/// </summary>

program FMXHelpBarSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  fMain in 'fMain.pas' {Form2},
  Gamolf.FMX.HelpBar in '..\..\..\src\Gamolf.FMX.HelpBar.pas',
  Olf.Skia.SVGToBitmap in '..\..\..\lib-externes\librairies\src\Olf.Skia.SVGToBitmap.pas',
  Gamolf.FMX.Joystick in '..\..\..\src\Gamolf.FMX.Joystick.pas',
  Gamolf.RTL.Joystick.DirectInput.Win in '..\..\..\src\Gamolf.RTL.Joystick.DirectInput.Win.pas',
  Gamolf.RTL.Joystick.Mac in '..\..\..\src\Gamolf.RTL.Joystick.Mac.pas',
  Gamolf.RTL.Joystick in '..\..\..\src\Gamolf.RTL.Joystick.pas',
  iOSapi.GameController in '..\..\..\src\iOSapi.GameController.pas',
  Macapi.GameController in '..\..\..\src\Macapi.GameController.pas',
  USVGKenneyInputKeys in '..\assets\KenneyInputKeys\USVGKenneyInputKeys.pas',
  USVGKenneyInputXbox in '..\assets\KenneyInputXbox\USVGKenneyInputXbox.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
