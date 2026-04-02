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
  File last update : 2025-05-25T17:36:33.921+02:00
  Signature : 9e297fb9ebc869768f453432a4af6dede5b49d99
  ***************************************************************************
*)

unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Memo.Types,
  FMX.Layouts,
  FMX.Edit,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  Gamolf.RTL.Scores;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    btnAddScore: TButton;
    Edit1: TEdit;
    Layout1: TLayout;
    btnLoadScore: TButton;
    btnSaveScore: TButton;
    btnSortByPoint: TButton;
    btnSortByPseudo: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnAddScoreClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadScoreClick(Sender: TObject);
    procedure btnSaveScoreClick(Sender: TObject);
    procedure btnSortByPseudoClick(Sender: TObject);
    procedure btnSortByPointClick(Sender: TObject);
  private
    procedure DisplayScoreList;
  public
    ScoreList: TScoreList;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnAddScoreClick(Sender: TObject);
var
  pseudo: string;
  points, level: integer;
begin
  pseudo := Edit1.Text.Trim;
  points := random(maxint);
  if pseudo.IsEmpty then
  begin
    if random(100) < 50 then
    begin
      if ScoreList.Add(points, false) then
        Memo1.Lines.Add('Add score : ' + points.ToString)
      else
        Memo1.Lines.Add('Score not added');
    end
    else
    begin
      level := random(maxint);
      if ScoreList.Add(points, level, false) then
        Memo1.Lines.Add('Add score : ' + points.ToString + ' (level ' +
          level.ToString + ')')
      else
        Memo1.Lines.Add('Score not added');
    end;
  end
  else
  begin
    if random(100) < 50 then
    begin
      if ScoreList.Add(pseudo, points, false) then
        Memo1.Lines.Add('Add score : ' + pseudo + ' => ' + points.ToString)
      else
        Memo1.Lines.Add('Score not added : ' + pseudo);
    end
    else
    begin
      level := random(maxint);
      if ScoreList.Add(pseudo, points, level, false) then
        Memo1.Lines.Add('Add score : ' + pseudo + ' => ' + points.ToString +
          ' (level ' + level.ToString + ')')
      else
        Memo1.Lines.Add('Score not added : ' + pseudo);
    end;
  end;
end;

procedure TForm1.btnLoadScoreClick(Sender: TObject);
begin
  ScoreList.Load;
  DisplayScoreList;
end;

procedure TForm1.btnSaveScoreClick(Sender: TObject);
begin
  ScoreList.Save;
  Memo1.Lines.Add('Score list saved in ' + ScoreList.GetScoreFileName);
end;

procedure TForm1.btnSortByPointClick(Sender: TObject);
begin
  ScoreList.SortByPointsDesc;
  DisplayScoreList;
end;

procedure TForm1.btnSortByPseudoClick(Sender: TObject);
begin
  ScoreList.SortByPseudoAsc;
  DisplayScoreList;
end;

procedure TForm1.DisplayScoreList;
var
  score: TScore;
begin
  Memo1.Lines.Add('**********');
  for score in ScoreList do
  begin
    Memo1.Lines.Add(score.pseudo + ' => ' + score.points.ToString + ' (level ' +
      score.level.ToString + ')');
  end;
  Memo1.Lines.Add('**********');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ScoreList := TScoreList.Create('FMXGameEngine', 'ScoresSample');
  Memo1.Lines.Add('GetScoreFileName=' + ScoreList.GetScoreFileName);
  Memo1.Lines.Add('GetOldScoreFileName=' + ScoreList.GetOldScoreFileName);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ScoreList.Free;
end;

initialization

randomize;

end.
