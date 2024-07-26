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
