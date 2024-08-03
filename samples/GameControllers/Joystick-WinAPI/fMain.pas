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
/// File last update : 25/07/2024 12:14:08
/// Signature : f083c931feb7e17041ba12855366bae9907cd704
/// ***************************************************************************
/// </summary>

unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    btnJoyGetNumDevs: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    Label2: TLabel;
    Layout2: TLayout;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Memo1: TMemo;
    procedure btnJoyGetNumDevsClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure FormCreate(Sender: TObject);
  private
    FNbJoystick: integer;
    FCheckActiveJoystick: boolean;
    procedure SetNbJoystick(const Value: integer);
    procedure SetCheckActiveJoystick(const Value: boolean);
    { Déclarations privées }
  public
    { Déclarations publiques }
    property NbJoystick: integer read FNbJoystick write SetNbJoystick;
    property CheckActiveJoystick: boolean read FCheckActiveJoystick
      write SetCheckActiveJoystick;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses winapi.Windows,
  winapi.MMSystem;

procedure TForm1.btnJoyGetNumDevsClick(Sender: TObject);
var
  i: integer;
  JoyInfo: tjoyinfo;
  ErrNum: MMRESULT;
  JoyCapsW: TJoyCapsW;
begin
  CheckActiveJoystick := false;
  NbJoystick := joyGetNumDevs;
  // https://learn.microsoft.com/en-us/windows/win32/api/joystickapi/nf-joystickapi-joygetnumdevs
  ListBox1.Clear;
  Memo1.lines.Clear;
  for i := 0 to NbJoystick - 1 do
  begin
    ErrNum := joyGetPos(i, @JoyInfo);
    case ErrNum of
      // https://learn.microsoft.com/en-us/windows/win32/api/joystickapi/nf-joystickapi-joygetpos
      MMSYSERR_NOERROR:
        with TListBoxItem.Create(self) do
        begin
          parent := ListBox1;
          Enabled := true;
          tag := i;
          case joyGetDevCapsW(i, @JoyCapsW, sizeof(JoyCapsW)) of
            // https://learn.microsoft.com/en-us/windows/win32/api/joystickapi/nf-joystickapi-joygetdevcapsw
            // https://learn.microsoft.com/en-us/windows/win32/api/joystickapi/ns-joystickapi-joycapsw
            MMSYSERR_NODRIVER:
              raise exception.Create('The joystick driver is not present.');
            MMSYSERR_INVALPARAM:
              raise exception.Create('An invalid parameter was passed.');
          else
            Text := 'Joystick ' + (i + 1).ToString;
            // text := JoyCapsW.szPname; // Null-terminated string containing the joystick product name.
            // text := JoyCapsW.szregkey; // Null-terminated string containing the registry key for the joystick.
            // text := JoyCapsW.szoemvxd; // Null-terminated string identifying the joystick driver OEM.
            if ((JoyCapsW.wcaps and JOYCAPS_HASPOV) > 0) then
              TagFloat := 1
            else
              TagFloat := 0;
            Memo1.lines.Add(Text);
            Memo1.lines.Add('wXmin ' + JoyCapsW.wXmin.ToString + ' / ' +
              'wXmax ' + JoyCapsW.wXmax.ToString);
            Memo1.lines.Add('wYmin ' + JoyCapsW.wYmin.ToString + ' / ' +
              'wYmax ' + JoyCapsW.wYmax.ToString);
            Memo1.lines.Add('wZmin ' + JoyCapsW.wzmin.ToString + ' / ' +
              'wZmax ' + JoyCapsW.wZmax.ToString + ' / ' +
              ((JoyCapsW.wcaps and JOYCAPS_HASZ) > 0).ToString);
            Memo1.lines.Add('wRmin ' + JoyCapsW.wrmin.ToString + ' / ' +
              'wRmax ' + JoyCapsW.wrmax.ToString + ' / ' +
              ((JoyCapsW.wcaps and JOYCAPS_HASR) > 0).ToString);
            Memo1.lines.Add('wUmin ' + JoyCapsW.wumin.ToString + ' / ' +
              'wUmax ' + JoyCapsW.wumax.ToString + ' / ' +
              ((JoyCapsW.wcaps and JOYCAPS_HASU) > 0).ToString);
            Memo1.lines.Add('wVmin ' + JoyCapsW.wvmin.ToString + ' / ' +
              'wVmax ' + JoyCapsW.wvmax.ToString + ' / ' +
              ((JoyCapsW.wcaps and JOYCAPS_HASv) > 0).ToString);
            Memo1.lines.Add('wMaxAxes ' + JoyCapsW.wMaxAxes.ToString + ' / ' +
              'wNumAxes ' + JoyCapsW.wNumAxes.ToString + ' / ' + 'wNumButtons '
              + JoyCapsW.wNumButtons.ToString + ' / ' + 'wMaxButtons ' +
              JoyCapsW.wMaxButtons.ToString);
          end;
        end;
      MMSYSERR_NODRIVER:
        raise exception.Create('The joystick driver is not present.');
      MMSYSERR_INVALPARAM:
        raise exception.Create('An invalid parameter was passed.');
      JOYERR_UNPLUGGED:
        with TListBoxItem.Create(self) do
        begin
          parent := ListBox1;
          Text := 'Joystick ' + (i + 1).ToString + ' (unplugged)';
          Enabled := false;
          tag := i;
        end;
    else
      with TListBoxItem.Create(self) do
      begin
        parent := ListBox1;
        Text := (i + 1).ToString + ' - error ' + ErrNum.ToString;
        Enabled := false;
        tag := -1;
      end;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CheckActiveJoystick := false;
end;

procedure TForm1.ListBox1ItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  Timer1.tag := Item.tag;
  Timer1.TagFloat := Item.TagFloat;
  CheckActiveJoystick := Item.Enabled;
end;

procedure TForm1.SetCheckActiveJoystick(const Value: boolean);
begin
  FCheckActiveJoystick := Value;
  Layout2.Visible := FCheckActiveJoystick;
  Timer1.Enabled := FCheckActiveJoystick;
end;

procedure TForm1.SetNbJoystick(const Value: integer);
begin
  FNbJoystick := Value;
  Label1.Text := 'Nb Joystick : ' + FNbJoystick.ToString;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  JoyInfoEx: TJoyInfoEx;
  ErrNum: MMRESULT;
begin
  if not CheckActiveJoystick then
    exit;
  if (Timer1.tag >= 0) and (Timer1.tag < FNbJoystick) then
  begin
    JoyInfoEx.dwSize := sizeof(JoyInfoEx);
    JoyInfoEx.dwFlags := JOY_RETURNALL;
    ErrNum := joyGetPosex(Timer1.tag, @JoyInfoEx);
    case ErrNum of
      // https://learn.microsoft.com/en-us/windows/win32/api/joystickapi/nf-joystickapi-joygetpos
      MMSYSERR_NOERROR:
        begin
          // wXpos: UINT;         { x position }
          Label3.Text := 'x position : ' + JoyInfoEx.wXpos.ToString;
          // wYpos: UINT;         { y position }
          Label4.Text := 'y position : ' + JoyInfoEx.wypos.ToString;
          // wZpos: UINT;         { z position }
          Label5.Text := 'z position : ' + JoyInfoEx.wzpos.ToString;
          // dwRpos: DWORD;		 { rudder/4th axis position }
          Label6.Text := 'rudder/4th axis position : ' +
            JoyInfoEx.dwrpos.ToString;
          // dwUpos: DWORD;		 { 5th axis position }
          Label7.Text := '5th axis position : ' + JoyInfoEx.dwupos.ToString;
          // dwVpos: DWORD;		 { 6th axis position }
          Label8.Text := '6th axis position : ' + JoyInfoEx.dwvpos.ToString;
          // wButtons: UINT;      { button states }
          Label9.Text := 'button states : ' + JoyInfoEx.wbuttons.ToString;
          // TODO : afficher en binaire
          // dwButtonNumber: DWORD;  { current button number pressed }
          Label10.Text := 'current button number pressed : ' +
            JoyInfoEx.dwButtonNumber.ToString;
          // dwPOV: DWORD;           { point of view state }
          if Timer1.TagFloat = 1 then
            Label11.Text := 'point of view state : ' + JoyInfoEx.dwpov.ToString
          else
            Label11.Text := 'point of view state : none';
        end;
      MMSYSERR_NODRIVER:
        raise exception.Create('The joystick driver is not present.');
      MMSYSERR_INVALPARAM:
        raise exception.Create('An invalid parameter was passed.');
      MMSYSERR_BADDEVICEID:
        raise exception.Create('The specified joystick identifier is invalid.');
      JOYERR_UNPLUGGED:
        begin
          CheckActiveJoystick := false;
          ListBox1.Selected.Text := ListBox1.Selected.Text + ' (unplugged)';
          ListBox1.Selected.Enabled := false;
          ListBox1.ItemIndex := -1;
        end;
      JOYERR_PARMS:
        raise exception.Create('The specified joystick identifier is invalid.');
    else
      Label2.Text := 'Error ' + ErrNum.ToString;
    end;
  end;
end;

end.
