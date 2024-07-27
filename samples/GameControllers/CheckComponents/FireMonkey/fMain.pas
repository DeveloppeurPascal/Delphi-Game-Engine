unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  Gamolf.RTL.Joystick,
  FMX.ScrollBox,
  FMX.Memo;

type
  TForm2 = class(TForm)
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
    vx, vy: single;
  public
    procedure AddLog(const Text: string); overload;
    procedure AddLog(const Value: Integer); overload;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.AddLog(const Text: string);
begin
  Memo1.lines.insert(0, Text);
end;

procedure TForm2.AddLog(const Value: Integer);
begin
  AddLog(Value.tostring);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  vx := 0;
  vy := 0;
end;

procedure TForm2.Gamepad1DirectionPadChange(const GamepadID: Integer;
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

procedure TForm2.Gamepad1Lost(const GamepadID: Integer);
begin
  AddLog('From ' + GamepadID.tostring + ' : bye bye.');
end;

procedure TForm2.GamepadManager1ButtonDown(const GamepadID: Integer;
  const Button: TJoystickButtons);
begin
  AddLog('Gamepad ' + GamepadID.tostring + ' button ' + ord(Button).tostring +
    ' : DOWN');
  Gamepad1.ID := GamepadID;
end;

procedure TForm2.GamepadManager1ButtonUp(const GamepadID: Integer;
  const Button: TJoystickButtons);
begin
  AddLog('Gamepad ' + GamepadID.tostring + ' button ' + ord(Button).tostring
    + ' : UP');
end;

procedure TForm2.GamepadManager1GamepadLost(const GamepadID: Integer);
begin
  AddLog('Gamepad ' + GamepadID.tostring + ' lost.');
end;

procedure TForm2.GamepadManager1NewGamepadDetected(const GamepadID: Integer);
begin
  AddLog('Gamepad ' + GamepadID.tostring + ' detected.');
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  Panel1.position.x := Panel1.position.x + vx;
  Panel1.position.y := Panel1.position.y + vy;
end;

end.
