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
  Vcl.ExtCtrls,
  Gamolf.RTL.Joystick,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    GamepadManager1: TGamepadManager;
    Gamepad1: TGamepad;
    Panel1: TPanel;
    Memo1: TMemo;
    Timer1: TTimer;
    Label1: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GamepadManager1ButtonDown(const GamepadID: Integer;
      const Button: TJoystickButtons);
    procedure GamepadManager1ButtonUp(const GamepadID: Integer;
      const Button: TJoystickButtons);
    procedure Gamepad1DirectionPadChange(const GamepadID: Integer;
      const Value: TJoystickDPad);
    procedure GamepadManager1NewGamepadDetected(const GamepadID: Integer);
    procedure GamepadManager1GamepadLost(const GamepadID: Integer);
    procedure Gamepad1Lost(const GamepadID: Integer);
  private
  protected
    x, y: Single;
    vx, vy: Single;
  public
    procedure AddLog(const Text: string); overload;
    procedure AddLog(const Value: Integer); overload;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AddLog(const Text: string);
begin
  Memo1.lines.insert(0, Text);
end;

procedure TForm1.AddLog(const Value: Integer);
begin
  AddLog(Value.tostring);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  x := Panel1.Left;
  y := Panel1.Top;
  vx := 0;
  vy := 0;
end;

procedure TForm1.Gamepad1DirectionPadChange(const GamepadID: Integer;
  const Value: TJoystickDPad);
begin
  case Value of
    TJoystickDPad.LeftBottom, TJoystickDPad.Left, TJoystickDPad.Lefttop:
      vx := -1;
    TJoystickDPad.RightTop, TJoystickDPad.Right, TJoystickDPad.RightBottom:
      vx := 1;
  else
    vx := 0;
  end;
  case Value of
    TJoystickDPad.Top, TJoystickDPad.TopRight, TJoystickDPad.TopLeft:
      vy := -1;
    TJoystickDPad.BottomRight, TJoystickDPad.Bottom, TJoystickDPad.BottomLeft:
      vy := 1;
  else
    vy := 0;
  end;
end;

procedure TForm1.Gamepad1Lost(const GamepadID: Integer);
begin
  AddLog('From ' + GamepadID.tostring + ' : bye bye.');
end;

procedure TForm1.GamepadManager1ButtonDown(const GamepadID: Integer;
  const Button: TJoystickButtons);
begin
  AddLog('Gamepad ' + GamepadID.tostring + ' button ' + ord(Button).tostring +
    ' : DOWN');
  Gamepad1.ID := GamepadID;
end;

procedure TForm1.GamepadManager1ButtonUp(const GamepadID: Integer;
  const Button: TJoystickButtons);
begin
  AddLog('Gamepad ' + GamepadID.tostring + ' button ' + ord(Button).tostring
    + ' : UP');
end;

procedure TForm1.GamepadManager1GamepadLost(const GamepadID: Integer);
begin
  AddLog('Gamepad ' + GamepadID.tostring + ' lost.');
end;

procedure TForm1.GamepadManager1NewGamepadDetected(const GamepadID: Integer);
begin
  AddLog('Gamepad ' + GamepadID.tostring + ' detected.');
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  x := x + vx;
  y := y + vy;
  Panel1.Left := round(x);
  Panel1.Top := round(y);
end;

end.
