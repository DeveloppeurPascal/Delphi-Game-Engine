object dmUIElements: TdmUIElements
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 480
  Width = 640
  object GamepadManager1: TDGEGamepadManager
    SynchronizedEvents = True
    OnButtonDown = GamepadManager1ButtonDown
    OnDirectionPadChange = GamepadManager1DirectionPadChange
    Left = 304
    Top = 224
  end
end
