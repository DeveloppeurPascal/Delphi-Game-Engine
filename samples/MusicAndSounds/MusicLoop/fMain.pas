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
/// Signature : b1f2c569a410f9eb304e8dd670956c9eb95072ea
/// ***************************************************************************
/// </summary>

unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, Gamolf.FMX.MusicLoop,
  FMX.ListBox;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Layout1: TLayout;
    Layout2: TLayout;
    Edit2: TEdit;
    Button3: TButton;
    Button4: TButton;
    Layout3: TLayout;
    Edit3: TEdit;
    Button5: TButton;
    Button6: TButton;
    Layout4: TLayout;
    Edit4: TEdit;
    Button7: TButton;
    Button8: TButton;
    Layout5: TLayout;
    Layout6: TLayout;
    Edit5: TEdit;
    Button9: TButton;
    ListBox1: TListBox;
    Button10: TButton;
    Button11: TButton;
    Layout7: TLayout;
    Button12: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CurrentMusicLoopPauseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button9Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses System.IOUtils;

procedure TForm1.Button10Click(Sender: TObject);
begin
  if assigned(ListBox1.Selected) then
    soundlist.play(ListBox1.Selected.Tag);
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  if assigned(ListBox1.Selected) then
    soundlist.Mute(ListBox1.Selected.Tag);
end;

procedure TForm1.Button12Click(Sender: TObject);
begin
  soundlist.MuteAll;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if assigned(Button1.Parent.TagObject) then
    (Button1.Parent.TagObject as tmusicloop).play
  else
  begin
    Button1.Parent.TagObject := tmusicloop.Current;
    (Button1.Parent.TagObject as tmusicloop).play(Edit1.Text);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if assigned(Button4.Parent.TagObject) then
    (Button4.Parent.TagObject as tmusicloop).play
  else
  begin
    Button4.Parent.TagObject := tmusicloop.Create;
    (Button4.Parent.TagObject as tmusicloop).play(Edit2.Text);
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  if assigned(Button6.Parent.TagObject) then
    (Button6.Parent.TagObject as tmusicloop).play
  else
  begin
    Button6.Parent.TagObject := tmusicloop.Create;
    (Button6.Parent.TagObject as tmusicloop).play(Edit3.Text);
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  if assigned(Button8.Parent.TagObject) then
    (Button8.Parent.TagObject as tmusicloop).play
  else
  begin
    Button8.Parent.TagObject := tmusicloop.Create;
    (Button8.Parent.TagObject as tmusicloop).play(Edit4.Text);
  end;
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  item: tlistboxitem;
  i: integer;
  l: TStringDynArray;
  ext: string;
begin
  Edit5.Text := Edit5.Text.Trim;
  if TDirectory.Exists(Edit5.Text) then
  begin
    ListBox1.Clear;
    l := TDirectory.GetFiles(Edit5.Text);
    for i := 0 to length(l) - 1 do
    begin
      ext := tpath.GetExtension(l[i]).ToLower;
      if (ext = '.wav') or (ext = '.mp3') then
      begin
        item := tlistboxitem.Create(self);
        item.Text := tpath.GetFileNameWithoutExtension(l[i]);
        item.tagString := l[i];
        item.Tag := soundlist.Add(l[i]);
        ListBox1.AddObject(item);
        if (ListBox1.Count > 10) then
          break;
      end;
    end;
  end;
end;

procedure TForm1.CurrentMusicLoopPauseClick(Sender: TObject);
var
  o: TObject;
  MusicLoop: tmusicloop;
begin
  o := (Sender as TButton).Parent.TagObject;
  if assigned(o) and (o is tmusicloop) then
  begin
    MusicLoop := o as tmusicloop;
    if MusicLoop.IsPlaying then
      MusicLoop.Stop
    else
      MusicLoop.play;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: integer;
begin
  for i := 0 to ChildrenCount - 1 do
  begin
    if assigned(Children[i].TagObject) and (Children[i].TagObject is tmusicloop)
    then
    begin
      (Children[i].TagObject as tmusicloop).free;
      Children[i].TagObject := nil;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.Text := tpath.Combine(tpath.GetDocumentsPath,
    'mp3\2007-BigBeatImpact- 30.mp3');
  Edit2.Text := tpath.Combine(tpath.GetDocumentsPath, 'mp3\2008-OnTheQ 30.mp3');
  Edit3.Text := tpath.Combine(tpath.GetDocumentsPath,
    'mp3\Accentica Stinger4.mp3');
  Edit4.Text := tpath.Combine(tpath.GetDocumentsPath,
    'mp3\Dynamic Keys - 30.mp3');
  Edit5.Text := tpath.Combine(tpath.GetDocumentsPath, 'wav');
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
