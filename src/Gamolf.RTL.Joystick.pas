unit Gamolf.RTL.Joystick;

interface

uses System.SysUtils;

type
  /// <summary>
  /// Base class for all Joystick exceptions
  /// </summary>
  EJoystickServiceException = class(exception)
  end;

  /// <summary>
  /// Raised if the controller is not available
  /// </summary>
  EJoystickUnpluggedException = class(EJoystickServiceException)

  end;

  /// <summary>
  /// Type for Joystick ID
  /// </summary>
  TJoystickID = byte;

  /// <summary>
  /// Type for Button ID
  /// </summary>
  TButtonID = byte;

  /// <summary>
  /// Joystick/gamepad controller datas
  /// </summary>
  TJoystickInfo = record
    /// <summary>
    /// Values for each axes managed by the joystick
    /// 0 => X, 1 => Y, 2 => Z
    /// 3 => R, 4 => U, 5 => V
    /// Values between -1 and 1.
    /// -1 means left, 0 means center, 1 means right
    /// can be around the real value and not egal the values, try round()
    /// </summary>
    Axes: array of single;
    /// <summary>
    /// List of all available buttons on this controller.
    /// Value is true if the button is pressed, false if not
    /// </summary>
    Buttons: array of boolean;
    /// <summary>
    /// List of pressed buttons for this controller
    /// </summary>
    PressedButtons: array of TButtonID;
    /// <summary>
    /// DPad value between 0 (top) and 359,
    /// for center, the value is higher than 359
    /// </summary>
    DPad: word;
  end;

{$SCOPEDENUMS on}

  /// <summary>
  /// DPad standard values (from 0 to 365° and 65535 when not value is selected)
  /// </summary>
  TJoystickDPad = (Top = 0, TopRight = 45, RightTop = 45, Right = 90,
    BottomRight = 135, RightBottom = 135, Bottom = 180, BottomLeft = 225,
    LeftBottom = 225, Left = 270, LeftTop = 315, TopLeft = 315, Center = 65535);

  /// <summary>
  /// Platform service to access to joystick/gamepad controllers on a computer
  /// </summary>
  IGamolfJoystickService = interface(IInterface)
    ['{74BA65B4-B468-41E2-A5FF-1FB92A79E9F4}']
    /// <summary>
    /// Return the number of joysticks managed by the system
    /// </summary>
    function Count: byte;
    /// <summary>
    /// Return "true" if the JoystickID controller is connected and available
    /// </summary>
    function isConnected(JoystickID: TJoystickID): boolean;
    /// <summary>
    /// Return a TJoystick for the JoystickID controller
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
    /// Check is the DPad / POV is in a standard position
    /// </summary>
    function isDPad(JoystickID: TJoystickID;
      JoystickDPad: TJoystickDPad): boolean;
  end;

implementation

end.
