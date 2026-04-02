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
  File last update : 2025-05-25T17:36:33.842+02:00
  Signature : 0504ac5e4d560aeb206080c9849ca9888afe5b43
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
