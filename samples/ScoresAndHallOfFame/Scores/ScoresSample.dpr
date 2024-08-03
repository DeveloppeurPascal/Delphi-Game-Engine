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
/// File last update : 26/07/2024 10:11:32
/// Signature : 677dada55bb35e41e827ca8462bc4c37793afa8a
/// ***************************************************************************
/// </summary>

program ScoresSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form1},
  Gamolf.RTL.Scores in '..\..\..\src\Gamolf.RTL.Scores.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
