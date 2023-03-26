unit Gamolf.FMX.MusicLoop;

{
  Lancement de musiques d'ambiance en boucle ou de
  bruitages ponctuels pour des jeux vidéo et autres
  logiciels développés avec FireMonkey sous Delphi.

  Logiciel open source distribué sous licence AGPL.
  Open source software distributed under the AGPL license

  Copyright Patrick Prémartin

  Find the original source code on
  https://github.com/DeveloppeurPascal/FMXGameEngine
}
interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Media;

type
  /// <summary>
  /// Sound and music volume level
  /// </summary>
  TVolumeSonore = 0 .. 100;

  TMusicLoop = class(TDataModule)
    audio: TMediaPlayer;
    audioCheck: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure audioCheckTimer(Sender: TObject);
  private
    FaudioOn: boolean;
    FaudioActif: boolean;
    FaudioEnBoucle: boolean;
    procedure SetaudioActif(const Value: boolean);
    procedure SetaudioOn(const Value: boolean);
    procedure SetaudioEnBoucle(const Value: boolean);
    procedure SetVolume(const Value: TVolumeSonore);
    function getVolume: TVolumeSonore;
    property audioActif: boolean read FaudioActif write SetaudioActif;
    property audioOn: boolean read FaudioOn write SetaudioOn;
    property audioEnBoucle: boolean read FaudioEnBoucle write SetaudioEnBoucle;
  public
    /// <summary>
    /// Set or get volume level for current playing sound or music
    /// </summary>
    property Volume: TVolumeSonore read getVolume write SetVolume;
    /// <summary>
    /// Load a music or sound (file formats depend on the operating system).
    /// You can try MP3 for music and WAV for sounds
    /// </summary>
    function Load(Filename: string): TMusicLoop;
    /// <summary>
    /// Load and play a sound or music
    /// </summary>
    procedure Play(Filename: string; LectureEnBoucle: boolean = true); overload;
    /// <summary>
    /// Play current soound or music in an infinite loop or one time
    /// </summary>
    procedure Play(LectureEnBoucle: boolean = true); overload;
    /// <summary>
    /// Just play current sound once time
    /// </summary>
    procedure PlaySound;
    /// <summary>
    /// Stop current play (sound or music)
    /// </summary>
    procedure Stop;
    /// <summary>
    /// Check if a sound or music is playing
    /// </summary>
    function IsPlaying: boolean;
    /// <summary>
    /// Check if a sound or music file is loaded
    /// </summary>
    function IsActive: boolean;
  end;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}

uses System.IOUtils;
{ TMusicLoop }

procedure TMusicLoop.audioCheckTimer(Sender: TObject);
begin
  if (audioActif and audioOn) then
  begin
    if (audio.State = TMediaState.Stopped) then
    begin
      audio.CurrentTime := 0;
      if audioEnBoucle then
        audio.Play;
    end
    else if (audio.State = TMediaState.Playing) and
      (audio.CurrentTime >= audio.Duration) then
    begin
      audio.CurrentTime := 0;
      if not audioEnBoucle then
        Stop;
    end;
  end;
end;

procedure TMusicLoop.DataModuleCreate(Sender: TObject);
begin
  audioCheck.Enabled := false;
  audioCheck.Interval := 100;
  FaudioOn := false;
  FaudioActif := false;
end;

function TMusicLoop.getVolume: TVolumeSonore;
begin
  result := round(audio.Volume * 100);
end;

function TMusicLoop.IsActive: boolean;
begin
  result := FaudioActif;
end;

function TMusicLoop.Load(Filename: string): TMusicLoop;
begin
  if (not Filename.IsEmpty) and (tfile.Exists(Filename)) then
    try
      audio.Filename := Filename;
      audioActif := true;
      audioOn := false;
    except
      audioActif := false;
    end;
  result := self;
end;

procedure TMusicLoop.Play(LectureEnBoucle: boolean);
begin
  Play('', LectureEnBoucle);
end;

procedure TMusicLoop.PlaySound;
begin
  Play('', false);
end;

function TMusicLoop.IsPlaying: boolean;
begin
  result := FaudioActif and FaudioOn;
end;

procedure TMusicLoop.Play(Filename: string; LectureEnBoucle: boolean);
begin
  if not Filename.IsEmpty then
    Load(Filename);
  if audioActif then
  begin
    audioEnBoucle := LectureEnBoucle;
    audioOn := true;
  end;
end;

procedure TMusicLoop.SetaudioActif(const Value: boolean);
begin
  FaudioActif := Value;
  audioCheck.Enabled := Value;
end;

procedure TMusicLoop.SetaudioEnBoucle(const Value: boolean);
begin
  FaudioEnBoucle := Value;
end;

procedure TMusicLoop.SetaudioOn(const Value: boolean);
begin
  FaudioOn := Value;
  if audioActif then
    if Value and (audio.State <> TMediaState.Playing) and
      (not audio.Filename.IsEmpty) then
      audio.Play
    else if (not Value) and (audio.State = TMediaState.Playing) then
      audio.Stop;
end;

procedure TMusicLoop.SetVolume(const Value: TVolumeSonore);
begin
  if (Value >= 0) and (Value <= 100) then
    audio.Volume := Value / 100;
end;

procedure TMusicLoop.Stop;
begin
  audioOn := false;
end;

end.
