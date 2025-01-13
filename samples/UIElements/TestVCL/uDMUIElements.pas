/// <summary>
/// ***************************************************************************
///
/// Delphi Game Engine
///
/// Copyright 2021-2025 Patrick Prémartin under AGPL 3.0 license.
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
///
/// Author(s) :
/// Patrick PREMARTIN
///
/// Site :
/// https://delphigameengine.developpeur-pascal.fr
///
/// Project site :
/// https://github.com/DeveloppeurPascal/Delphi-Game-Engine
///
/// ***************************************************************************
/// File last update : 2025-01-13T19:52:26.827+01:00
/// Signature : 189762a6da6cb008feca8ce0e61da30d24bd51dc
/// ***************************************************************************
/// </summary>

unit uDMUIElements;

interface

uses
  System.SysUtils,
  System.Classes,
  Gamolf.RTL.Joystick,
  Gamolf.RTL.UIElements;

type
  TdmUIElements = class(TDataModule)
    GamepadManager1: TDGEGamepadManager;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure GamepadManager1ButtonDown(const GamepadID: Integer;
      const Button: TJoystickButtons);
    procedure GamepadManager1DirectionPadChange(const GamepadID: Integer;
      const Value: TJoystickDPad);
  private
    FUserInterface: TUIElementsList;
    procedure SetUserInterface(const Value: TUIElementsList);
  protected
  public
    property UserInterface: TUIElementsList read FUserInterface
      write SetUserInterface;
  end;

function GetUserInterface: TUIElementsList;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

var
  dmUIElements: TdmUIElements;

procedure TdmUIElements.DataModuleCreate(Sender: TObject);
begin
  FUserInterface := TUIElementsList.Create;
end;

procedure TdmUIElements.DataModuleDestroy(Sender: TObject);
begin
  FUserInterface.free;
end;

procedure TdmUIElements.GamepadManager1ButtonDown(const GamepadID: Integer;
  const Button: TJoystickButtons);
var
  Handled: boolean;
begin
  UserInterface.GamepadButtonDown(Button, Handled);
end;

procedure TdmUIElements.GamepadManager1DirectionPadChange(const GamepadID
  : Integer; const Value: TJoystickDPad);
begin
  UserInterface.GamepadMove(Value);
end;

procedure TdmUIElements.SetUserInterface(const Value: TUIElementsList);
begin
  FUserInterface := Value;
end;

function GetUserInterface: TUIElementsList;
begin
  result := dmUIElements.UserInterface;
end;

initialization

dmUIElements := TdmUIElements.Create(nil);

finalization

dmUIElements.free;

end.
