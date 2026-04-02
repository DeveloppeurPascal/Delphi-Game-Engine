(* C2PP
  ***************************************************************************

  Delphi Game Engine
  Copyright (c) 2021-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  ***************************************************************************

  Delphi Game Engine contains libraries and components to use in VCL or
  FireMonkey game (or classic) projects.

  If you want to play sounds or musics, use game controllers, pilot your
  user interface with the keyboard or a game controller, it's the good place.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://delphigameengine.developpeur-pascal.fr

  Project site :
  https://github.com/DeveloppeurPascal/Delphi-Game-Engine

  ***************************************************************************
  File last update : 2025-05-25T17:36:34.087+02:00
  Signature : e446a8bd60d9d6a518165e022120ad4c8c5556bf
  ***************************************************************************
*)

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
