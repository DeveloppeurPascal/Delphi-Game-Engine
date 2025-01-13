object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  TextHeight = 15
  object Memo1: TMemo
    Left = 0
    Top = 241
    Width = 624
    Height = 200
    Align = alBottom
    Lines.Strings = (
      'Memo1')
    TabOrder = 9
    ExplicitLeft = 64
    ExplicitTop = 288
    ExplicitWidth = 185
  end
  object Button1: TButton
    Left = 104
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 200
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 296
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button4: TButton
    Left = 104
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Button4'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button5: TButton
    Left = 200
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Button5'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button6: TButton
    Left = 296
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Button6'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button7: TButton
    Left = 104
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Button7'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button8: TButton
    Left = 200
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Button8'
    TabOrder = 7
    OnClick = Button1Click
  end
  object Button9: TButton
    Left = 296
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Button9'
    TabOrder = 8
    OnClick = Button1Click
  end
  object GamepadManager1: TDGEGamepadManager
    SynchronizedEvents = True
    OnNewGamepadDetected = GamepadManager1NewGamepadDetected
    OnGamepadLost = GamepadManager1GamepadLost
    Left = 528
    Top = 176
  end
end
