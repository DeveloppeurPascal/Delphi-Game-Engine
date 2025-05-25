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
  File last update : 2025-02-09T11:03:40.000+01:00
  Signature : 502739c7e03595f54464db1c068387b0fdfd296a
  ***************************************************************************
*)

program VCLCheckConfigProject;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {Form1},
  Gamolf.RTL.Joystick.DirectInput.Win in '..\..\..\..\src\Gamolf.RTL.Joystick.DirectInput.Win.pas',
  Gamolf.RTL.Joystick in '..\..\..\..\src\Gamolf.RTL.Joystick.pas',
  Gamolf.VCL.Joystick in '..\..\..\..\src\Gamolf.VCL.Joystick.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
