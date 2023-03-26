object MusicLoop: TMusicLoop
  OnCreate = DataModuleCreate
  Height = 150
  Width = 215
  object audio: TMediaPlayer
    Left = 48
    Top = 48
  end
  object audioCheck: TTimer
    OnTimer = audioCheckTimer
    Left = 144
    Top = 48
  end
end
