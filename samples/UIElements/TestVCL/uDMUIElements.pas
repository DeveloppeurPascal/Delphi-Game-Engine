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
  File last update : 2025-05-25T17:36:33.952+02:00
  Signature : 3606de9321702987f6d9829a027778e88c3af97b
  ***************************************************************************
*)

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
