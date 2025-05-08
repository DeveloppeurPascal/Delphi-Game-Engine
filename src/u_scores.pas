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
/// File last update : 2025-05-08T18:40:44.000+02:00
/// Signature : f3a1866ae371421a223437354b161b4745ff109d
/// ***************************************************************************
/// </summary>

unit u_scores;

{
  Stockage automatisé de scores pour jeux vidéos développés sous Delphi.

  **********
  * Deprecated, use Gamolf.RTL.Scores.pas
  **********

  Logiciel open source distribué sous licence AGPL.
  Open source software distributed under the AGPL license

  Copyright Patrick Prémartin

  Find the original source code on
  https://github.com/DeveloppeurPascal/Delphi-Game-Engine
}
interface

uses
  Gamolf.RTL.Scores;

type
  /// <summary>
  /// Class to manage one score (current score or score from a scores list)
  /// </summary>
  /// <remarks>
  /// Use this class if you used u_score.pas from the
  /// "DeveloppeurPascal/Librairies" project in the past.
  ///
  /// It's recommanded to move to Gamolf.RTL.Scores unit and class.
  /// </remarks>
  TScore = class(Gamolf.RTL.Scores.TScore)
  private
    function GetNiveau: cardinal;
    function GetPseudoModere: boolean;
    function GetScoreTransfere: boolean;
    procedure SetNiveau(const Value: cardinal);
    procedure SetPseudoModere(const Value: boolean);
    procedure SetScoreTransfere(const Value: boolean);
  public
    /// <summary>
    /// Game level
    /// </summary>
    property niveau: cardinal read GetNiveau write SetNiveau;
    /// <summary>
    /// True if the user pseudo has been moderated. False by default.
    /// </summary>
    /// <remarks>
    /// It was supposed to be used with a scores management server on Internet.
    /// </remarks>
    property pseudo_modere: boolean read GetPseudoModere write SetPseudoModere;
    /// <summary>
    /// True if this score has been sent to the scores management server on Internet.
    /// </summary>
    property score_transfere: boolean read GetScoreTransfere
      write SetScoreTransfere;
  end;

  /// <summary>
  /// List of scores items
  /// </summary>
  TScoreListe = Gamolf.RTL.Scores.TScoreList<TScore>;

  /// <summary>
  /// Initialize default scores list and load it from the storrage
  /// </summary>
procedure score_init(editeur, logiciel: string);
/// <summary>
/// Get the default scores list
/// </summary>
function score_liste_get: TScoreListe;
/// <summary>
/// Add a score to the default scores list
/// </summary>
function score_add(pseudo: string; points: cardinal;
  niveau: cardinal = 0): boolean;

implementation

uses
  system.classes,
  system.SysUtils,
  system.ioutils,
  system.Types;

Const
  CCompanyName = 'OlfSoftware';
  CSoftwareName = 'Unknow';

var
  score_liste: TScoreListe;
  nom_editeur, nom_logiciel: string;

procedure score_init(editeur, logiciel: string);
begin
  editeur := editeur.Trim;
  if (editeur.Length > 0) then
    nom_editeur := editeur;
  logiciel := logiciel.Trim;
  if (logiciel.Length > 0) then
    nom_logiciel := logiciel;
end;

function score_nom_fichier_get: string;
begin
  result := score_liste.GetOldScoreFileName(nom_editeur, nom_logiciel);
end;

procedure score_load;
begin
  if (not assigned(score_liste)) then
    score_liste := TScoreListe.Create(nom_editeur, nom_logiciel, 'score.dat');
  score_liste.LoadFromFile(score_nom_fichier_get);
end;

procedure score_save;
begin
  if assigned(score_liste) then
    score_liste.SaveToFile(score_nom_fichier_get);
end;

function score_liste_get: TScoreListe;
begin
  if (not assigned(score_liste)) then
    score_load;
  result := score_liste;
end;

function score_add(pseudo: string; points: cardinal;
  niveau: cardinal = 0): boolean;
begin
  if (not assigned(score_liste)) then
    score_load;
  result := score_liste.Add(pseudo, points, niveau, false);
  score_save;
end;

{ tScore }

function TScore.GetNiveau: cardinal;
begin
  result := Level;
end;

function TScore.GetPseudoModere: boolean;
begin
  result := Moderated;
end;

function TScore.GetScoreTransfere: boolean;
begin
  result := Uploaded;
end;

procedure TScore.SetNiveau(const Value: cardinal);
begin
  Level := Value;
end;

procedure TScore.SetPseudoModere(const Value: boolean);
begin
  Moderated := Value;
end;

procedure TScore.SetScoreTransfere(const Value: boolean);
begin
  Uploaded := Value;
end;

initialization

score_liste := nil;
nom_editeur := CCompanyName;
nom_logiciel := CSoftwareName;

{$MESSAGE WARN 'deprecated unit, use Gamolf.RTL.Scores version'}

finalization

if assigned(score_liste) then
  score_liste.Free;

end.
