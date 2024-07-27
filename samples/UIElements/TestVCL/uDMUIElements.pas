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
