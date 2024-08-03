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
/// File last update : 27/07/2024 13:45:54
/// Signature : c3d3cd99ca991d21af941a8eb0ef615f81bc2762
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
  Vcl.StdCtrls,
  Gamolf.RTL.Joystick;

type
  TfrmMain = class(TForm)
    GamepadManager1: TGamepadManager;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    procedure GamepadManager1GamepadLost(const GamepadID: Integer);
    procedure GamepadManager1NewGamepadDetected(const GamepadID: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  protected
    procedure UIButtonEnter(Sender: TObject);
    procedure UIButtonExit(Sender: TObject);
  public
    procedure AddLog(const Text: string);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  Gamolf.RTL.UIElements,
  uDMUIElements;

procedure TfrmMain.AddLog(const Text: string);
begin
  Memo1.Lines.Insert(0, Text);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  if Sender is tcontrol then
    AddLog((Sender as tcontrol).name);
end;

procedure TfrmMain.FormHide(Sender: TObject);
begin
  GetUserInterface.RemoveLayout;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  KeyChar: WideChar;
begin
  KeyChar := #0;
  GetUserInterface.KeyDown(Key, KeyChar, Shift);
end;

procedure TfrmMain.FormShow(Sender: TObject);
  procedure AddDefaultShortcuts;
  var
    e: TUIElement;
  begin
    // Click on the focused element by 'A' or Space on keyboard and A button for a gamepad
    e := GetUserInterface.AddUIItem;
    e.GamePadButtons := [TJoystickButtons.a];
    e.KeyShortcuts.Add(VK_SPACE, #0, []);
    e.KeyShortcuts.Add(ord('A'), #0, []);
    e.OnClickProc := procedure(const Sender: TObject)
      var
        e: TUIElement;
      begin
        e := GetUserInterface.Focused;
        if assigned(e) then
          e.DoClick;
      end;

    // Click on the focused element by ESC on keyboard and X button for a gamepad
    e := GetUserInterface.AddUIItem;
    e.GamePadButtons := [TJoystickButtons.X];
    e.KeyShortcuts.Add(VK_ESCAPE, #0, []);
    e.OnClickProc := procedure(const Sender: TObject)
      begin
        close;
      end;
  end;
  procedure AddButtonToUserInterface(const Btn: TButton);
  var
    e: TUIElement;
  begin
    if not assigned(Btn) then
      exit;

    e := GetUserInterface.AddUIItem;
    e.TagObject := Btn;
    e.OnClickProc := procedure(const Sender: TObject)
      begin
        if assigned((e.TagObject as TButton).onclick) then
          (e.TagObject as TButton).onclick((e.TagObject as TButton));
      end;
    e.OnPaintProc := procedure(const Sender: TObject)
      begin
        if e.IsFocused then
          (e.TagObject as TButton).SetFocus;
      end;
    Btn.OnEnter := UIButtonEnter;
    Btn.OnExit := UIButtonExit;
    Btn.OnKeyDown := FormKeyDown;
  end;

var
  i: Integer;
begin
  GetUserInterface.NewLayout;

  AddDefaultShortcuts;

  for i := 0 to Componentcount - 1 do
    if components[i] is TButton then
      AddButtonToUserInterface(components[i] as TButton);

  GetUserInterface.GetElementByTagObject(Button1).RightItem :=
    GetUserInterface.GetElementByTagObject(Button2);
  GetUserInterface.GetElementByTagObject(Button1).BottomItem :=
    GetUserInterface.GetElementByTagObject(Button4);

  GetUserInterface.GetElementByTagObject(Button2).RightItem :=
    GetUserInterface.GetElementByTagObject(Button3);
  GetUserInterface.GetElementByTagObject(Button2).BottomItem :=
    GetUserInterface.GetElementByTagObject(Button5);

  GetUserInterface.GetElementByTagObject(Button3).BottomItem :=
    GetUserInterface.GetElementByTagObject(Button6);

  GetUserInterface.GetElementByTagObject(Button4).RightItem :=
    GetUserInterface.GetElementByTagObject(Button5);
  GetUserInterface.GetElementByTagObject(Button4).BottomItem :=
    GetUserInterface.GetElementByTagObject(Button7);

  GetUserInterface.GetElementByTagObject(Button5).RightItem :=
    GetUserInterface.GetElementByTagObject(Button6);
  GetUserInterface.GetElementByTagObject(Button5).BottomItem :=
    GetUserInterface.GetElementByTagObject(Button8);

  GetUserInterface.GetElementByTagObject(Button6).BottomItem :=
    GetUserInterface.GetElementByTagObject(Button9);

  GetUserInterface.GetElementByTagObject(Button7).RightItem :=
    GetUserInterface.GetElementByTagObject(Button8);

  GetUserInterface.GetElementByTagObject(Button8).RightItem :=
    GetUserInterface.GetElementByTagObject(Button9);
end;

procedure TfrmMain.GamepadManager1GamepadLost(const GamepadID: Integer);
begin
  AddLog('Gamepad ' + GamepadID.tostring + ' lost.');
end;

procedure TfrmMain.GamepadManager1NewGamepadDetected(const GamepadID: Integer);
begin
  AddLog('Gamepad ' + GamepadID.tostring + ' detected.');
end;

procedure TfrmMain.UIButtonEnter(Sender: TObject);
var
  e: TUIElement;
begin
  e := GetUserInterface.GetElementByTagObject(Sender);
  if assigned(e) then
    e.SetFocus;
end;

procedure TfrmMain.UIButtonExit(Sender: TObject);
var
  e: TUIElement;
begin
  e := GetUserInterface.GetElementByTagObject(Sender);
  if assigned(e) then
    e.ResetFocus;
end;

end.
