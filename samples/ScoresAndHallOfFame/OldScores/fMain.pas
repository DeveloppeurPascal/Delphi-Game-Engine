/// <summary>
/// ***************************************************************************
///
/// Delphi Game Engine
///
/// Copyright 2021-2024 Patrick Prémartin under AGPL 3.0 license.
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
/// File last update : 28/05/2024 12:18:52
/// Signature : 6bee90a43663c6efab27506a1cb5f5bd8452f1c3
/// ***************************************************************************
/// </summary>

unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Layouts;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    Memo1: TMemo;
    Edit1: TEdit;
    btnAddScore: TButton;
    btnDisplayScoreList: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnAddScoreClick(Sender: TObject);
    procedure btnDisplayScoreListClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses u_scores;

procedure TForm1.btnAddScoreClick(Sender: TObject);
begin
  score_add(Edit1.Text.Trim, random(maxint));
  Memo1.Lines.Add('score added');
end;

procedure TForm1.btnDisplayScoreListClick(Sender: TObject);
var
  list: TScoreListe;
  score: tscore;
begin
  list := score_liste_get;
  Memo1.Lines.Add('**********');
  for score in list do
  begin
    Memo1.Lines.Add(score.pseudo + ' => ' + score.points.ToString + ' (level ' +
      score.niveau.ToString + ')');
  end;
  Memo1.Lines.Add('**********');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  score_init('FMXGameEngine', 'OldScoresSample');
end;

end.
