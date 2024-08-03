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
/// File last update : 27/07/2024 13:10:24
/// Signature : 428280a5270eb07edee745c927866f2fc22493be
/// ***************************************************************************
/// </summary>

program TestUIElementsVCL;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {frmMain},
  Gamolf.RTL.Joystick in '..\..\..\src\Gamolf.RTL.Joystick.pas',
  Gamolf.RTL.UIElements in '..\..\..\src\Gamolf.RTL.UIElements.pas',
  Gamolf.VCL.Joystick in '..\..\..\src\Gamolf.VCL.Joystick.pas',
  Gamolf.RTL.Joystick.DirectInput.Win in '..\..\..\src\Gamolf.RTL.Joystick.DirectInput.Win.pas',
  uDMUIElements in 'uDMUIElements.pas' {dmUIElements: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
