object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 456
  ClientWidth = 794
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 794
    Height = 13
    Align = alTop
    Caption = 
      'To move the panel use the DPAD. Click on a game controller butto' +
      'n to activate it when detected. Alt+F4 to close the program.'
    ExplicitWidth = 607
  end
  object Memo1: TMemo
    Left = 0
    Top = 256
    Width = 794
    Height = 200
    Align = alBottom
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
    ExplicitTop = 210
    ExplicitWidth = 635
  end
  object Panel1: TPanel
    Left = 312
    Top = 128
    Width = 185
    Height = 41
    Caption = 'Panel1'
    TabOrder = 0
  end
  object GamepadManager1: TGamepadManager
    SynchronizedEvents = True
    OnNewGamepadDetected = GamepadManager1NewGamepadDetected
    OnGamepadLost = GamepadManager1GamepadLost
    OnButtonUp = GamepadManager1ButtonUp
    OnButtonDown = GamepadManager1ButtonDown
    Left = 248
    Top = 88
  end
  object Gamepad1: TGamepad
    SynchronizedEvents = True
    OnDirectionPadChange = Gamepad1DirectionPadChange
    OnLost = Gamepad1Lost
    Left = 344
    Top = 88
  end
  object Timer1: TTimer
    Interval = 16
    OnTimer = Timer1Timer
    Left = 312
    Top = 152
  end
end
