unit Gamolf.FMX.Joystick.Windows;

interface

implementation

{$IF Defined(MSWINDOWS) }

uses winapi.Windows, winapi.MMSystem, Gamolf.FMX.Joystick, FMX.platform,
  System.SysUtils;

type
  TLocalJoyCaps = record
    JoyCapsW: TJoyCapsW;
    XMiddle, YMiddle, ZMiddle, RMiddle, UMiddle, VMiddle: single;
  end;

  TGamolfJoystickWindowsService = class(TInterfacedObject,
    IGamolfJoystickService)
  private
    FTabDevCaps: array of TLocalJoyCaps;
    procedure getDevCaps(JoystickID: TJoystickID);
  protected
    FNbControllers: byte;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    /// <summary>
    /// Return the number of joysticks managed by the system
    /// </summary>
    function Count: byte;
    /// <summary>
    /// Return "true" if the JoystickID controller is connected and available
    /// </summary>
    function isConnected(JoystickID: TJoystickID): boolean;
    /// <summary>
    /// Return a TJoystickInfo for the JoystickID controller
    /// </summary>
    procedure getInfo(JoystickID: TJoystickID; var Joystick: TJoystickInfo);
    /// <summary>
    /// Check if button "ButtonID" from controller "JoystickID" is pressed or not
    /// </summary>
    function isPressed(JoystickID: TJoystickID; ButtonID: TButtonID): boolean;
    /// <summary>
    /// Return X,Y axes values for JoystickID controller
    /// </summary>
    procedure getXY(JoystickID: TJoystickID; var x, y: single);
    /// <summary>
    /// Return X axes values for JoystickID controller
    /// </summary>
    function getX(JoystickID: TJoystickID): single;
    /// <summary>
    /// Return Y axes values for JoystickID controller
    /// </summary>
    function getY(JoystickID: TJoystickID): single;
    /// <summary>
    /// Return Z axes values for JoystickID controller
    /// </summary>
    function getZ(JoystickID: TJoystickID): single;
    /// <summary>
    /// Return the DPad value between (0-359° or 65535)
    /// Compare it to Top, TopRight/RightTop, Right, BottomRight/RightBottom, Bottom, BottomLeft/LeftBottom, Left, LeftTop/TopLeft, Center values from TJoystickDPad enumeration
    /// </summary>
    function getDPad(JoystickID: TJoystickID): word;
    /// <summary>
    /// Check is the DPad / POV of the JoystickID controller is in a standard position
    /// </summary>
    function isDPad(JoystickID: TJoystickID;
      JoystickDPad: TJoystickDPad): boolean;
  end;

  { TGamolfJoystickWindowsService }

function TGamolfJoystickWindowsService.Count: byte;
begin
  result := FNbControllers;
end;

constructor TGamolfJoystickWindowsService.Create;
begin
  FNbControllers := joyGetNumDevs;
end;

destructor TGamolfJoystickWindowsService.Destroy;
begin

  inherited;
end;

procedure TGamolfJoystickWindowsService.getDevCaps(JoystickID: TJoystickID);
var
  i: TJoystickID;
begin
  for i := length(FTabDevCaps) to JoystickID do
  begin
    setlength(FTabDevCaps, length(FTabDevCaps) + 1);
    case joyGetDevCapsW(i, @FTabDevCaps[i].JoyCapsW,
      sizeof(FTabDevCaps[i].JoyCapsW)) of
      // https://learn.microsoft.com/en-us/windows/win32/api/joystickapi/nf-joystickapi-joygetdevcapsw
      // https://learn.microsoft.com/en-us/windows/win32/api/joystickapi/ns-joystickapi-joycapsw
      MMSYSERR_NODRIVER:
        raise EJoystickServiceException.Create
          ('The joystick driver is not present.');
      MMSYSERR_INVALPARAM:
        raise EJoystickServiceException.Create
          ('An invalid parameter was passed.');
    else
      FTabDevCaps[i].XMiddle :=
        (FTabDevCaps[i].JoyCapsW.wXmax - FTabDevCaps[i].JoyCapsW.wXmin) div 2;
      FTabDevCaps[i].YMiddle :=
        (FTabDevCaps[i].JoyCapsW.wYmax - FTabDevCaps[i].JoyCapsW.wymin) div 2;
      FTabDevCaps[i].ZMiddle :=
        (FTabDevCaps[i].JoyCapsW.wZmax - FTabDevCaps[i].JoyCapsW.wzmin) div 2;
      FTabDevCaps[i].RMiddle :=
        (FTabDevCaps[i].JoyCapsW.wRmax - FTabDevCaps[i].JoyCapsW.wrmin) div 2;
      FTabDevCaps[i].UMiddle :=
        (FTabDevCaps[i].JoyCapsW.wUmax - FTabDevCaps[i].JoyCapsW.wumin) div 2;
      FTabDevCaps[i].VMiddle :=
        (FTabDevCaps[i].JoyCapsW.wVmax - FTabDevCaps[i].JoyCapsW.wvmin) div 2;
    end;
  end;
end;

function TGamolfJoystickWindowsService.getDPad(JoystickID: TJoystickID): word;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) > 0) then
    result := Joystick.DPad;
end;

procedure TGamolfJoystickWindowsService.getInfo(JoystickID: TJoystickID;
  var Joystick: TJoystickInfo);
var
  JoyInfoEx: TJoyInfoEx;
  ErrNum: MMRESULT;
  btn: word;
begin
  if (JoystickID >= 0) and (JoystickID < FNbControllers) then
  begin
    JoyInfoEx.dwSize := sizeof(JoyInfoEx);
    JoyInfoEx.dwFlags := JOY_RETURNALL;
    ErrNum := joyGetPosex(JoystickID, @JoyInfoEx);
    // https://learn.microsoft.com/en-us/windows/win32/api/joystickapi/nf-joystickapi-joygetposex
    case ErrNum of
      MMSYSERR_NOERROR:
        begin
          if (length(FTabDevCaps) <= JoystickID) then
            getDevCaps(JoystickID);

          if (length(Joystick.Axes) < 6) then
            setlength(Joystick.Axes, 6);
          Joystick.Axes[0] :=
            (JoyInfoEx.wXpos - FTabDevCaps[JoystickID].XMiddle) / FTabDevCaps
            [JoystickID].XMiddle;
          Joystick.Axes[1] :=
            (JoyInfoEx.wypos - FTabDevCaps[JoystickID].YMiddle) / FTabDevCaps
            [JoystickID].YMiddle;
          if ((FTabDevCaps[JoystickID].JoyCapsW.wcaps and joycaps_hasz) > 0)
          then
            Joystick.Axes[2] :=
              (JoyInfoEx.wzpos - FTabDevCaps[JoystickID].ZMiddle) / FTabDevCaps
              [JoystickID].ZMiddle
          else
            Joystick.Axes[2] := 0;
          if ((FTabDevCaps[JoystickID].JoyCapsW.wcaps and joycaps_hasr) > 0)
          then
            Joystick.Axes[3] :=
              (JoyInfoEx.dwrpos - FTabDevCaps[JoystickID].RMiddle) / FTabDevCaps
              [JoystickID].RMiddle
          else
            Joystick.Axes[3] := 0;
          if ((FTabDevCaps[JoystickID].JoyCapsW.wcaps and joycaps_hasu) > 0)
          then
            Joystick.Axes[4] :=
              (JoyInfoEx.dwupos - FTabDevCaps[JoystickID].UMiddle) / FTabDevCaps
              [JoystickID].UMiddle
          else
            Joystick.Axes[4] := 0;
          if ((FTabDevCaps[JoystickID].JoyCapsW.wcaps and joycaps_hasv) > 0)
          then
            Joystick.Axes[5] :=
              (JoyInfoEx.dwvpos - FTabDevCaps[JoystickID].VMiddle) / FTabDevCaps
              [JoystickID].VMiddle
          else
            Joystick.Axes[5] := 0;

          if (length(Joystick.Buttons) < FTabDevCaps[JoystickID]
            .JoyCapsW.wNumButtons) then
            setlength(Joystick.Buttons,
              FTabDevCaps[JoystickID].JoyCapsW.wNumButtons);
          // TODO : optimize PressedButtons memory usage
          setlength(Joystick.PressedButtons, 0);
          for btn := 0 to FTabDevCaps[JoystickID].JoyCapsW.wNumButtons - 1 do
          begin
            Joystick.Buttons[btn] := (JoyInfoEx.wbuttons and (1 shl btn)) > 0;
            if Joystick.Buttons[btn] then
            begin
              setlength(Joystick.PressedButtons,
                length(Joystick.PressedButtons) + 1);
              Joystick.PressedButtons[length(Joystick.PressedButtons) -
                1] := btn;
            end;
          end;

          if ((FTabDevCaps[JoystickID].JoyCapsW.wcaps and JOYCAPS_HASPOV) > 0)
          then
            Joystick.DPad := JoyInfoEx.dwpov div 100
          else
            Joystick.DPad := ord(TJoystickDPad.Center);
        end;
      MMSYSERR_NODRIVER:
        raise EJoystickServiceException.Create
          ('The joystick driver is not present.');
      MMSYSERR_INVALPARAM:
        raise EJoystickServiceException.Create
          ('An invalid parameter was passed.');
      MMSYSERR_BADDEVICEID, JOYERR_UNPLUGGED, JOYERR_PARMS:
        raise EJoystickUnpluggedException.Create
          ('Controller ' + JoystickID.ToString + ' is not available.');
    else
      raise EJoystickServiceException.Create('Unknown error for controller ' +
        JoystickID.ToString);
    end;
  end
  else
    raise EJoystickServiceException.Create
      ('The specified joystick identifier is invalid.');
end;

function TGamolfJoystickWindowsService.getX(JoystickID: TJoystickID): single;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) > 0) then
    result := Joystick.Axes[0];
end;

procedure TGamolfJoystickWindowsService.getXY(JoystickID: TJoystickID;
  var x, y: single);
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) > 0) then
    x := Joystick.Axes[0];
  if (length(Joystick.Axes) > 1) then
    y := Joystick.Axes[1];
end;

function TGamolfJoystickWindowsService.getY(JoystickID: TJoystickID): single;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) > 1) then
    result := Joystick.Axes[1];
end;

function TGamolfJoystickWindowsService.getZ(JoystickID: TJoystickID): single;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) > 2) then
    result := Joystick.Axes[2];
end;

function TGamolfJoystickWindowsService.isConnected
  (JoystickID: TJoystickID): boolean;
var
  JoyInfo: tjoyinfo;
  ErrNum: MMRESULT;
begin
  result := false;
  if (JoystickID >= 0) and (JoystickID < FNbControllers) then
  begin
    ErrNum := joyGetPos(JoystickID, @JoyInfo);
    // https://learn.microsoft.com/en-us/windows/win32/api/joystickapi/nf-joystickapi-joygetpos
    case ErrNum of
      MMSYSERR_NOERROR:
        result := true;
      MMSYSERR_NODRIVER:
        raise EJoystickServiceException.Create
          ('The joystick driver is not present.');
      MMSYSERR_INVALPARAM:
        raise EJoystickServiceException.Create
          ('An invalid parameter was passed.');
      // JOYERR_UNPLUGGED:
      // raise EJoystickUnpluggedException.Create
      // ('Controller ' + JoystickID.tostring + ' is not available.');
    else
      // unknown error (device doesn't exists but is declared)
    end;
  end;
end;

function TGamolfJoystickWindowsService.isDPad(JoystickID: TJoystickID;
  JoystickDPad: TJoystickDPad): boolean;
begin
  result := (getDPad(JoystickID) = ord(JoystickDPad));
end;

function TGamolfJoystickWindowsService.isPressed(JoystickID: TJoystickID;
  ButtonID: TButtonID): boolean;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  result := (ButtonID >= 0) and (ButtonID < length(Joystick.Buttons)) and
    Joystick.Buttons[ButtonID];
end;
{$ENDIF }

initialization

{$IF Defined(MSWINDOWS)}
  TPlatformServices.Current.AddPlatformService(IGamolfJoystickService,
  TGamolfJoystickWindowsService.Create);
{$ENDIF}

finalization

{$IF Defined(MSWINDOWS)}
  TPlatformServices.Current.RemovePlatformService(IGamolfJoystickService);
{$ENDIF}

end.
