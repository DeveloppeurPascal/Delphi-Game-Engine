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
  FMX.Types, FMX.Media, System.Generics.Collections;

type
  /// <summary>
  /// Sound and music volume level
  /// </summary>
  TVolumeSonore = 0 .. 100;

  /// <summary>
  /// Class to play an individual sound or a music (as background loop or not)
  /// </summary>
  TMusicLoop = class
  private
    class var FCurrent: TMusicLoop;
    function getFileName: string;

  var
    MediaPlayer: TMediaPlayer;
    AudioCheckTimer: TTimer;
    FaudioOn: boolean;
    FaudioActif: boolean;
    FaudioEnBoucle: boolean;
    Ftag: integer;
    procedure Settag(const Value: integer);
    procedure SetaudioActif(const Value: boolean);
    procedure SetaudioOn(const Value: boolean);
    procedure SetaudioEnBoucle(const Value: boolean);
    procedure SetVolume(const Value: TVolumeSonore);
    function getVolume: TVolumeSonore;
    procedure AudioCheckTimerTimer(Sender: TObject);
  protected
    property tag: integer read Ftag write Settag;
    property audioActif: boolean read FaudioActif write SetaudioActif;
    property audioOn: boolean read FaudioOn write SetaudioOn;
    property audioEnBoucle: boolean read FaudioEnBoucle write SetaudioEnBoucle;
  public
    property Filename: string read getFileName;
    /// <summary>
    /// Create an instance of a TMusicLoop or use "TMusicLoop.Current" to use the default one
    /// </summary>
    constructor Create; virtual;
    /// <summary>
    /// Destroy this instance
    /// </summary>
    destructor Destroy; override;
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

    /// <summary>
    /// Get the instance of the default music loop to use it as a singleton
    /// </summary>
    class function Current: TMusicLoop;
  end;

  /// <summary>
  /// Class to manage all sounds (or musics) of a game
  /// Use it if you want to register sounds once and play them as many time you need
  /// </summary>
  TSoundList = class
  class var
    FCurrent: TSoundList;

  var
    /// <summary>
    /// List of sounds/musics filenames
    /// </summary>
    FSounds: TDictionary<integer, string>;
    /// <summary>
    /// List of TMusicLoop players used to play those sounds/musics
    /// </summary>
    FPlayers: TObjectList<TMusicLoop>;
  private
    FVolume: TVolumeSonore;
    procedure SetVolume(const Value: TVolumeSonore);
  public
    /// <summary>
    /// Create an instance of a TSoundList or use "TSoundList.Current" to use the default one
    /// </summary>
    constructor Create; virtual;
    /// <summary>
    /// Destroy this instance
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    /// Get the instance of the default sound list to use it as a singleton
    /// </summary>
    class function Current: TSoundList;
    /// <summary>
    /// Add the "filename" sound to the available sounds list and return it's index
    /// </summary>
    function Add(Filename: string): integer; overload;
    /// <summary>
    /// Add the "filename" sound to the available sounds list at the SoundID position and return True if it's ok
    /// </summary>
    function Add(SoundID: integer; Filename: string): boolean; overload;
    /// <summary>
    /// Play the sound "SoundID", in a loop or not, and return its Player ID
    /// If the sound is played, the result is >= 0
    /// If it returns -1, an error happened
    /// </summary>
    function Play(SoundID: integer; Loop: boolean = false): integer;
    /// <summary>
    /// Stop a player by its PlayerID
    /// </summary>
    procedure Stop(PlayerID: integer);
    /// <summary>
    /// Stop all players playing the SoundID
    /// </summary>
    procedure Mute(SoundID: integer);
    /// <summary>
    /// Stop all active players
    /// </summary>
    procedure MuteAll;
    /// <summary>
    /// Set or get volume level for all sounds / musics of this TSoundList
    /// </summary>
    property Volume: TVolumeSonore read FVolume write SetVolume;
  end;

  /// <summary>
  /// Return the default instance for the TMusicLoop class
  /// </summary>
function MusicLoop: TMusicLoop;

/// <summary>
/// Return the default instance for the TSoundList class
/// </summary>
function SoundList: TSoundList;

implementation

uses System.IOUtils, System.SysUtils;

function MusicLoop: TMusicLoop;
begin
  result := TMusicLoop.Current;
end;

function SoundList: TSoundList;
begin
  result := TSoundList.Current;
end;
{ TMusicLoop }

procedure TMusicLoop.AudioCheckTimerTimer(Sender: TObject);
begin
  if (audioActif and audioOn) then
  begin
    if (MediaPlayer.State = TMediaState.Stopped) then
    begin
      MediaPlayer.CurrentTime := 0;
      if audioEnBoucle then
        MediaPlayer.Play;
    end
    else if (MediaPlayer.State = TMediaState.Playing) and
      (MediaPlayer.CurrentTime >= MediaPlayer.Duration) then
    begin
      MediaPlayer.CurrentTime := 0;
      if not audioEnBoucle then
        Stop;
    end;
  end;
end;

constructor TMusicLoop.Create;
begin
  MediaPlayer := TMediaPlayer.Create(nil);
  AudioCheckTimer := TTimer.Create(nil);
  AudioCheckTimer.Enabled := false;
  AudioCheckTimer.Interval := 100;
  AudioCheckTimer.OnTimer := AudioCheckTimerTimer;
  FaudioOn := false;
  FaudioActif := false;
end;

destructor TMusicLoop.Destroy;
begin
  if (FCurrent = self) then
    FCurrent := nil;

  AudioCheckTimer.Free;
  MediaPlayer.Free;
  inherited;
end;

class function TMusicLoop.Current: TMusicLoop;
begin
  if not assigned(FCurrent) then
    FCurrent := TMusicLoop.Create;
  result := FCurrent;
end;

function TMusicLoop.getFileName: string;
begin
  result := MediaPlayer.Filename;
end;

function TMusicLoop.getVolume: TVolumeSonore;
begin
  result := round(MediaPlayer.Volume * 100);
end;

function TMusicLoop.IsActive: boolean;
begin
  result := FaudioActif;
end;

function TMusicLoop.Load(Filename: string): TMusicLoop;
begin
  if (not Filename.IsEmpty) and (tfile.Exists(Filename)) then
    try
      MediaPlayer.Filename := Filename;
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
  AudioCheckTimer.Enabled := Value;
end;

procedure TMusicLoop.SetaudioEnBoucle(const Value: boolean);
begin
  FaudioEnBoucle := Value;
end;

procedure TMusicLoop.SetaudioOn(const Value: boolean);
begin
  FaudioOn := Value;
  if audioActif then
    if Value and (MediaPlayer.State <> TMediaState.Playing) and
      (not MediaPlayer.Filename.IsEmpty) then
      MediaPlayer.Play
    else if (not Value) and (MediaPlayer.State = TMediaState.Playing) then
      MediaPlayer.Stop;
end;

procedure TMusicLoop.Settag(const Value: integer);
begin
  Ftag := Value;
end;

procedure TMusicLoop.SetVolume(const Value: TVolumeSonore);
begin
  if (Value >= 0) and (Value <= 100) then
    MediaPlayer.Volume := Value / 100;
end;

procedure TMusicLoop.Stop;
begin
  audioOn := false;
end;

{ TSoundList }

function TSoundList.Add(Filename: string): integer;
var
  i: integer;
  max: integer;
begin
  result := -1;
  if Filename.IsEmpty or (not tfile.Exists(Filename)) then
    raise exception.Create('File "' + Filename + '" not found !');
  // TODO : utiliser une TSoundListException
  if assigned(FSounds) then
  begin
    max := -1;
    for i in FSounds.Keys do
      if (Filename = FSounds.Items[i]) then
      begin
        result := i;
        break;
      end
      else if (max < i) then
        max := i;
    if (result < 0) then
    begin
      result := max + 1;
      FSounds.Add(result, Filename);
    end;
  end
  else
    raise exception.Create('No sounds list for this instance !');
  // TODO : utiliser une TSoundListException
end;

function TSoundList.Add(SoundID: integer; Filename: string): boolean;
begin
  result := false;
  if Filename.IsEmpty or (not tfile.Exists(Filename)) then
    raise exception.Create('File "' + Filename + '" not found ! (SoundID=' +
      SoundID.ToString + ')');
  // TODO : utiliser une TSoundListException
  if assigned(FSounds) then
    result := FSounds.TryAdd(SoundID, Filename)
  else
    raise exception.Create('No sounds list for this instance !');
  // TODO : utiliser une TSoundListException
end;

constructor TSoundList.Create;
begin
  FSounds := TDictionary<integer, string>.Create;
  FPlayers := TObjectList<TMusicLoop>.Create;
end;

class function TSoundList.Current: TSoundList;
begin
  if not assigned(FCurrent) then
    FCurrent := TSoundList.Create;
  result := FCurrent;
end;

destructor TSoundList.Destroy;
begin
  FPlayers.Free;
  FSounds.Free;
  inherited;
end;

procedure TSoundList.Mute(SoundID: integer);
var
  i: integer;
begin
  if assigned(FPlayers) and (FPlayers.Count > 0) then
    for i := 0 to FPlayers.Count - 1 do
      if FPlayers[i].tag = SoundID then
        FPlayers[i].Stop;
end;

procedure TSoundList.MuteAll;
var
  i: integer;
begin
  if assigned(FPlayers) and (FPlayers.Count > 0) then
    for i := 0 to FPlayers.Count - 1 do
      FPlayers[i].Stop;
end;

function TSoundList.Play(SoundID: integer; Loop: boolean): integer;
var
  i: integer;
  player: TMusicLoop;
begin
  result := -1;
  if assigned(FSounds) and FSounds.ContainsKey(SoundID) and assigned(FPlayers)
  then
  begin
    if (FPlayers.Count > 0) then
      for i := 0 to FPlayers.Count - 1 do
        if (FPlayers[i].tag = SoundID) and (not FPlayers[i].IsPlaying) then
        begin
          result := i;
          break;
        end;
    if (result < 0) then
    begin
      player := TMusicLoop.Create;
      player.tag := SoundID;
      player.Volume := FVolume;
      player.Load(FSounds[SoundID]);
      result := FPlayers.Add(player);
    end;
    FPlayers[result].Play(Loop);
  end;
end;

procedure TSoundList.SetVolume(const Value: TVolumeSonore);
var
  ml: TMusicLoop;
begin
  FVolume := Value;
  if assigned(FPlayers) and (FPlayers.Count > 0) then
    for ml in FPlayers do
      ml.Volume := FVolume;
end;

procedure TSoundList.Stop(PlayerID: integer);
begin
  if assigned(FPlayers) and (PlayerID >= 0) and (PlayerID < FPlayers.Count) then
    FPlayers[PlayerID].Stop;
end;

initialization

TMusicLoop.FCurrent := nil;
TSoundList.FCurrent := nil;

finalization

if assigned(TMusicLoop.FCurrent) then
begin
  TMusicLoop.FCurrent.Free;
  TMusicLoop.FCurrent := nil;
end;

if assigned(TSoundList.FCurrent) then
begin
  TSoundList.FCurrent.Free;
  TSoundList.FCurrent := nil;
end;

end.
