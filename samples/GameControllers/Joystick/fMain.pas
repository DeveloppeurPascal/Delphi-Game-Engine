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
/// File last update : 27/07/2024 12:27:18
/// Signature : 1b54d8db982440d065775cba239e1224c70eaf40
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
  FMX.Objects,
  Gamolf.FMX.Joystick,
  Gamolf.RTL.Joystick,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TForm2 = class(TForm)
    Rectangle1: TRectangle;
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    JoystickService: IGamolfJoystickService;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  FMX.Platform,
  Gamolf.RTL.Joystick.Helpers;

procedure TForm2.FormCreate(Sender: TObject);
begin
  if not TPlatformServices.Current.SupportsPlatformService
    (IGamolfJoystickService, JoystickService) then
  begin
    showmessage('Joystick Service not supported');
    tthread.ForceQueue(nil,
      procedure
      begin
        close;
      end);
  end
  else
    Timer1.Enabled := true;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
var
  // num: integer;
  ji: TJoystickInfo;
begin
  // for num := 0 to JoystickService.Count - 1 do
  // begin
  // try
  // JoystickService.getInfo(num, ji);
  // if length(ji.PressedButtons) > 0 then
  // begin
  // Rectangle1.Position.X := Rectangle1.Position.X + ji.Axes[0];
  // Rectangle1.Position.y := Rectangle1.Position.y + ji.Axes[1];
  // end;
  // except
  // on e: EJoystickUnpluggedException do;
  // end;
  // end;
  Label1.Text := 'Nb devices (registered by the OS, not detected/connected) : '
    + JoystickService.Count.ToString;
  Label2.Text := 'Pressed buttons : ';
  Label4.Text := 'DPad angle :';
  Label5.Text := 'Left stick :';
  Label6.Text := 'Right stick :';
  Label7.Text := 'Left trigger pression :';
  Label8.Text := 'Right trigger pression :';
  JoystickService.ForEach(ji,
    procedure(JoystickID: TJoystickID; var JoystickInfo: TJoystickInfo;
      hadError: boolean)
    var
      i: integer;
    begin
      if (not hadError) and JoystickService.isConnected(JoystickID) then
      begin
        Label2.Text := Label2.Text + 'J' + JoystickID.ToString + '=' +
          length(ji.PressedButtons).ToString + ' ';

        Label9.Text := 'Pressed buttons : ';
        for i := 0 to length(ji.PressedButtons) - 1 do
          Label9.Text := Label9.Text + ji.PressedButtons[i].ToString + '-' +
            tjoystickbuttons(ji.PressedButtons[i]).ToString + ', ';

        Label4.Text := Label4.Text + 'J' + JoystickID.ToString + '=' +
          ji.DPad.ToString + ' ';

        Label5.Text := Label5.Text + 'J' + JoystickID.ToString + '=' +
          ji.Axes[0].ToString + ' | ' + ji.Axes[1].ToString + ' ';

        Label6.Text := Label6.Text + 'J' + JoystickID.ToString + '=' +
          ji.Axes[2].ToString + ' | ' + ji.Axes[3].ToString + ' ';

        Label7.Text := Label7.Text + 'J' + JoystickID.ToString + '=' +
          ji.Axes[4].ToString + ' ';

        Label8.Text := Label8.Text + 'J' + JoystickID.ToString + '=' +
          ji.Axes[5].ToString + ' ';

        if (length(ji.PressedButtons) > 0) then
        begin
          Rectangle1.Position.X := Rectangle1.Position.X + round(ji.Axes[0]);
          Rectangle1.Position.y := Rectangle1.Position.y + round(ji.Axes[1]);
        end;
      end;
    end);
end;

end.
