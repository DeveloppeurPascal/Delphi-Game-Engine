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
/// File last update : 2025-05-08T19:04:22.000+02:00
/// Signature : 6a8e07c04096f2246a28d90db60b21cf8ac20d86
/// ***************************************************************************
/// </summary>

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
