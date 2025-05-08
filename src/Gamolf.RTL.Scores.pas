/// <summary>
/// ***************************************************************************
///
/// Delphi Game Engine
///
/// Copyright 2021-2025 Patrick Prémartin under AGPL 3.0 license.
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
///
/// Author(s) :
/// Patrick PREMARTIN
///
/// Site :
/// https://delphigameengine.developpeur-pascal.fr
///
/// Project site :
/// https://github.com/DeveloppeurPascal/Delphi-Game-Engine
///
/// ***************************************************************************
/// File last update : 2025-05-08T18:43:30.000+02:00
/// Signature : 8db85a1cb8d81d830484a74f3725935fd653ea79
/// ***************************************************************************
/// </summary>

unit Gamolf.RTL.Scores;

{
  Stockage automatisé de scores pour jeux vidéos développés en Pascal.
  (utilise les unités de Delphi, peut-être compatible avec d'autres environnements)

  Logiciel open source distribué sous licence AGPL.
  Open source software distributed under the AGPL license

  Copyright Patrick Prémartin

  Find the original source code on
  https://github.com/DeveloppeurPascal/Delphi-Game-Engine
}
interface

uses
  system.generics.collections;

type
  /// <summary>
  /// Score item (user name, points, level and things for Internet synchronization)
  /// </summary>
  TScore = class(TObject)
  private
    FPseudo: string;
    FPoints: cardinal;
    FLevel: cardinal;
    FModerated: boolean;
    FUploaded: boolean;
    procedure SetLevel(const Value: cardinal);
    procedure SetPoints(const Value: cardinal);
    procedure SetPseudo(const Value: string);
    procedure SetModerated(const Value: boolean);
    procedure SetUploaded(const Value: boolean);
  public
    /// <summary>
    /// User name or pseudo
    /// </summary>
    property Pseudo: string read FPseudo write SetPseudo;
    /// <summary>
    /// Score value (nb points)
    /// </summary>
    property Points: cardinal read FPoints write SetPoints;
    /// <summary>
    /// Level number
    /// </summary>
    property Level: cardinal read FLevel write SetLevel;
    /// <summary>
    /// User name moderated (for public display on other computer after a synchronization)
    /// </summary>
    property Moderated: boolean read FModerated write SetModerated;
    /// <summary>
    /// Score uploaded to the synchronization server on the Internet (or a local network)
    /// </summary>
    property Uploaded: boolean read FUploaded write SetUploaded;
  end;

  /// <summary>
  /// Generic list of scores registered for a game
  /// (use TScoreList if you don't need the genericity)
  /// </summary>
  TScoreList<T: TScore, constructor> = class(TObjectList<T>)
  private
    FEditorName, FGameName, FScoreFileName: string;
  public
    /// <summary>
    /// Scores list constructor
    /// </summary>
    constructor Create(EditorName, GameName: string;
      ScoreFileName: string = 'score.dat'); overload; virtual;
    /// <summary>
    /// Save the list of scores in the default file (GetScoreFileName)
    /// </summary>
    procedure Save;
    /// <summary>
    /// Save the list of scores in AFileName (and create the path if needed and possible)
    /// </summary>
    procedure SaveToFile(AFileName: string);
    /// <summary>
    /// Load the list of scores from the default file (GetScoreFileName)
    /// </summary>
    procedure Load;
    /// <summary>
    /// Load the list of scores from FileName (if it exists)
    /// </summary>
    procedure LoadFromFile(AFileName: string);
    /// <summary>
    /// Return default file name for the file list storrage
    /// </summary>
    function GetScoreFileName: string;
    /// <summary>
    /// Add a new score to the list
    /// </summary>
    function Add(APseudo: string; APoints: cardinal; ASaveList: boolean = true)
      : boolean; overload;
    /// <summary>
    /// Add a new score to the list
    /// </summary>
    function Add(APseudo: string; APoints: cardinal; ALevel: cardinal;
      ASaveList: boolean = true): boolean; overload;
    /// <summary>
    /// Return old default file name (from u_scores.pas) for compatibility reasons
    /// </summary>
    function GetOldScoreFileName(EditorName: string = '';
      GameName: string = ''): string;
    /// <summary>
    /// Sort the scores list by pseudo in alphabetical order
    /// (and level+score if same pseudo is present more than once time)
    /// </summary>
    procedure SortByPseudoAsc;
    /// <summary>
    /// Sort the scores list by pseudo in inverse alphabetical order
    /// (and level+score if same pseudo is present more than once time)
    /// </summary>
    procedure SortByPseudoDesc;
    /// <summary>
    /// Sort the scores list by level / points / pseudo
    /// (bottom from top)
    /// </summary>
    procedure SortByPointsAsc(WithLevel: boolean = true);
    /// <summary>
    /// Sort the scores list by level / points / pseudo
    /// (top from bottom)
    /// </summary>
    procedure SortByPointsDesc(WithLevel: boolean = true);
  end;

  /// <summary>
  /// List of scores registered for a game
  /// </summary>
  TScoreList = TScoreList<TScore>;

implementation

uses
  system.classes,
  system.SysUtils,
  system.ioutils,
  system.Types,
  system.generics.Defaults;

{ TScore }

procedure TScore.SetLevel(const Value: cardinal);
begin
  FLevel := Value;
end;

procedure TScore.SetPoints(const Value: cardinal);
begin
  FPoints := Value;
end;

procedure TScore.SetPseudo(const Value: string);
begin
  FPseudo := Value;
end;

procedure TScore.SetModerated(const Value: boolean);
begin
  FModerated := Value;
end;

procedure TScore.SetUploaded(const Value: boolean);
begin
  FUploaded := Value;
end;

{ TScoreList }

function TScoreList<T>.Add(APseudo: string; APoints, ALevel: cardinal;
  ASaveList: boolean): boolean;
var
  score: T;
begin
  result := false;
  APseudo := APseudo.Trim;
  if not APseudo.isempty then
  begin
    score := T.Create;
    try
      score.Pseudo := APseudo;
      score.Points := APoints;
      score.Level := ALevel;
      score.Moderated := false;
      score.Uploaded := false;
      Add(score);
      if ASaveList then
        Save;
      result := true;
    except
      score.Free;
      raise;
    end;
  end;
end;

function TScoreList<T>.Add(APseudo: string; APoints: cardinal;
  ASaveList: boolean): boolean;
begin
  result := Add(APseudo, APoints, 0, ASaveList);
end;

constructor TScoreList<T>.Create(EditorName, GameName: string;
  ScoreFileName: string);
var
  LScoreFileName: string;
begin
  inherited Create(true);

  LScoreFileName := ScoreFileName.Trim;
  if LScoreFileName.isempty then
    raise exception.Create('Need a ScoreFileName.');
  FEditorName := EditorName.Trim;
  FGameName := GameName.Trim;
  FScoreFileName := LScoreFileName;
end;

function TScoreList<T>.GetOldScoreFileName(EditorName,
  GameName: string): string;
var
  en, gn: string;
begin
  en := EditorName.Trim;
  if en.isempty then
    en := 'OlfSoftware';
  gn := GameName.Trim;
  if gn.isempty then
    gn := 'Unknow';
  result := tpath.Combine(tpath.Combine(tpath.Combine(tpath.GetDocumentsPath,
    FEditorName), FGameName), 'score.dat');
end;

function TScoreList<T>.GetScoreFileName: string;
var
  Suffixe: string;
begin
{$IFDEF DEBUG}
  result := tpath.GetDocumentsPath;
  Suffixe := '-debug';
{$ELSE}
  result := tpath.GetHomePath;
  Suffixe := '';
{$ENDIF}
  if not FEditorName.isempty then
    result := tpath.Combine(result, FEditorName + Suffixe);
  if not FGameName.isempty then
    result := tpath.Combine(result, FGameName + Suffixe);
  result := tpath.Combine(result, FScoreFileName);
end;

procedure TScoreList<T>.Load;
begin
  LoadFromFile(GetScoreFileName);
end;

procedure TScoreList<T>.LoadFromFile(AFileName: string);
var
  FileName: string;
  liste: tstringlist;
  ligne: string;
  score: T;
  champs: tarray<string>;
begin
  if AFileName.isempty then
    FileName := GetScoreFileName
  else
    FileName := AFileName;
  Clear;
  if tfile.Exists(FileName) then
  begin
    liste := tstringlist.Create;
    try
      liste.LoadFromFile(FileName);
      for ligne in liste do
      begin
        champs := ligne.Split([Tabulator]);
        if (Length(champs) = 5) then
        begin
          score := T.Create;
          try
            score.Pseudo := champs[0];
            score.Points := champs[1].ToInteger;
            score.Level := champs[2].ToInteger;
            score.Moderated := champs[3] = 'O';
            score.Uploaded := champs[4] = 'O';
            Add(score);
          except
            score.Free;
            raise;
          end;
        end;
      end;
    finally
      liste.Free;
    end;
  end;
end;

procedure TScoreList<T>.Save;
begin
  SaveToFile(GetScoreFileName);
end;

procedure TScoreList<T>.SaveToFile(AFileName: string);
var
  score: T;
  liste: tstringlist;
  ligne: string;
  FileName, Path: string;
begin
  liste := tstringlist.Create;
  try
    for score in Self do
    begin
      ligne := score.Pseudo + Tabulator + score.Points.ToString + Tabulator +
        score.Level.ToString + Tabulator;
      if score.FUploaded then
        ligne := ligne + 'O' + Tabulator
      else
        ligne := ligne + 'N' + Tabulator;
      if score.Moderated then
        ligne := ligne + 'O'
      else
        ligne := ligne + 'N';
      liste.Add(ligne);
    end;
    if AFileName.isempty then
      FileName := GetScoreFileName
    else
      FileName := AFileName;
    Path := tpath.GetDirectoryName(FileName);
    if not tdirectory.Exists(Path) then
      tdirectory.CreateDirectory(Path);
    if liste.Count > 0 then
      liste.SaveToFile(FileName)
    else if tfile.Exists(FileName) then
      tfile.Delete(FileName);
  finally
    liste.Free;
  end;
end;

procedure TScoreList<T>.SortByPointsAsc(WithLevel: boolean);
begin
  Sort(TComparer<T>.Construct(
    function(const a, b: T): Integer
    var
      s1, s2: TScore;
    begin
      if a is TScore then
        s1 := a as TScore
      else
        raise exception.Create('Wrong type for A.');
      if b is TScore then
        s2 := b as TScore
      else
        raise exception.Create('Wrong type for B.');
      result := 0;
      if WithLevel then
        if s1.Level < s2.Level then
          result := -1
        else if s1.Level > s2.Level then
          result := +1;
      if result = 0 then
        if s1.Points < s2.Points then
          result := -1
        else if s1.Points > s2.Points then
          result := +1;
      if result = 0 then
        result := string.compare(s1.Pseudo, s2.Pseudo, true);
    end));
end;

procedure TScoreList<T>.SortByPointsDesc(WithLevel: boolean);
begin
  Sort(TComparer<T>.Construct(
    function(const a, b: T): Integer
    var
      s1, s2: TScore;
    begin
      if a is TScore then
        s1 := a as TScore
      else
        raise exception.Create('Wrong type for A.');
      if b is TScore then
        s2 := b as TScore
      else
        raise exception.Create('Wrong type for B.');
      result := 0;
      if WithLevel then
        if s1.Level < s2.Level then
          result := -1
        else if s1.Level > s2.Level then
          result := +1;
      if result = 0 then
        if s1.Points < s2.Points then
          result := -1
        else if s1.Points > s2.Points then
          result := +1;
      if result = 0 then
        result := string.compare(s1.Pseudo, s2.Pseudo, true);
      // => ASC to DESC order
      result := -result;
    end));
end;

procedure TScoreList<T>.SortByPseudoAsc;
begin
  Sort(TComparer<T>.Construct(
    function(const a, b: T): Integer
    var
      s1, s2: TScore;
    begin
      if a is TScore then
        s1 := a as TScore
      else
        raise exception.Create('Wrong type for A.');
      if b is TScore then
        s2 := b as TScore
      else
        raise exception.Create('Wrong type for B.');
      result := string.compare(s1.Pseudo, s2.Pseudo, true);
      if result = 0 then
        if s1.Level < s2.Level then
          result := -1
        else if s1.Level > s2.Level then
          result := +1;
      if result = 0 then
        if s1.Points < s2.Points then
          result := -1
        else if s1.Points > s2.Points then
          result := +1;
    end));
end;

procedure TScoreList<T>.SortByPseudoDesc;
begin
  Sort(TComparer<T>.Construct(
    function(const a, b: T): Integer
    var
      s1, s2: TScore;
    begin
      if a is TScore then
        s1 := a as TScore
      else
        raise exception.Create('Wrong type for A.');
      if b is TScore then
        s2 := b as TScore
      else
        raise exception.Create('Wrong type for B.');
      result := string.compare(s1.Pseudo, s2.Pseudo, true);
      if result = 0 then
        if s1.Level < s2.Level then
          result := -1
        else if s1.Level > s2.Level then
          result := +1;
      if result = 0 then
        if s1.Points < s2.Points then
          result := -1
        else if s1.Points > s2.Points then
          result := +1;

      // => ASC to DESC order
      result := -result;
    end));
end;

end.
