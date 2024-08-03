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
/// File last update : 31/07/2024 09:58:40
/// Signature : dcfe49fa7ac79789571cb3ea68494093275de209
/// ***************************************************************************
/// </summary>

unit fMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Gamolf.RTL.GamepadDetected,
  Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    DGEGamepadDetected1: TDGEGamepadDetected;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  if random(10) < 5 then
    DGEGamepadDetected1.Test(true)
  else
    DGEGamepadDetected1.Test(false);
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
randomize;

end.
