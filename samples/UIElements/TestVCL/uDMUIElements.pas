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
/// File last update : 27/07/2024 13:10:02
/// Signature : c2942620f74ff42a538ef231208fc4b512b99e44
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
    GamepadManager1: TGamepadManager;
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
