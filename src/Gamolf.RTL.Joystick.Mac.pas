unit Gamolf.RTL.Joystick.Mac;

{
  Accès aux contrôleurs de jeu et à leurs informations
  depuis l'API macOS ou iOS (framework GameController).

  Logiciel open source distribué sous licence AGPL.
  Open source software distributed under the AGPL license

  Copyright Patrick Prémartin

  Find the original source code on
  https://github.com/DeveloppeurPascal/Delphi-Game-Engine
}
interface

{ $IF Defined(MACOS) or Defined(IOS) }

uses
  Gamolf.RTL.Joystick;

type
  TGamolfJoystickService = class(TGamolfCustomJoystickService)
  private
  protected
    procedure WirelessControllerDiscoveryFinished;
  public
    /// <summary>
    /// Scan for available devices (if some are already used, their ID could change)
    /// </summary>
    procedure StartDiscovery; override;
    /// <summary>
    /// Return the number of joysticks managed by the system
    /// </summary>
    function Count: byte; override;
    /// <summary>
    /// Return "true" if the JoystickID controller is connected and available
    /// </summary>
    function isConnected(JoystickID: TJoystickID): boolean; override;
    /// <summary>
    /// Return a TJoystick for the JoystickID controller
    /// </summary>
    procedure getInfo(JoystickID: TJoystickID;
      var Joystick: TJoystickInfo); override;
    /// <summary>
    /// Check if the game controller has a DPad/POV button
    /// </summary>
    function hasDPad(JoystickID: TJoystickID): boolean; override;
    /// <summary>
    /// Constructor of the class
    /// </summary>
    constructor Create; override;
    /// <summary>
    /// macOS API knows where are the buttons and list them
    /// </summary>
    function hasJoystickButtonsAPI: boolean; override;
  end;

implementation

uses
  System.Generics.Collections,
  System.TypInfo,
  macapi.objcruntime,
  macapi.ObjectiveC,
  macapi.Helpers,
{$IF Defined(IOS)}
  iosapi.Foundation,
  iosapi.CocoaTypes,
  iosapi.GameController,
  iosapi.Helpers;
{$ELSE}
macapi.Foundation,
  macapi.CocoaTypes,
  macapi.GameController;
{$ENDIF}

/// <summary>
/// Get the DPad direction
/// </summary>
function getDPadPosition(const dpad: gccontrollerdirectionpad): TJoystickDPad;
begin
  // choosed to used isPressed but we can check Value for pression level on compatible devices
  if dpad.left.isPressed then
  begin
    if dpad.up.isPressed then
      result := TJoystickDPad.LeftTop
    else if dpad.down.isPressed then
      result := TJoystickDPad.LeftBottom
    else
      result := TJoystickDPad.left;
  end
  else if dpad.right.isPressed then
  begin
    if dpad.up.isPressed then
      result := TJoystickDPad.righttop
    else if dpad.down.isPressed then
      result := TJoystickDPad.rightBottom
    else
      result := TJoystickDPad.right;
  end
  else if dpad.up.isPressed then
    result := TJoystickDPad.top
  else if dpad.down.isPressed then
    result := TJoystickDPad.bottom
  else
    result := TJoystickDPad.center;
end;

type
  IControllersNotificationHandler = interface(NSObject)
    ['{94946360-33B2-47B3-BE69-D732268BB707}']
    procedure ControllerDidConnect(ANotification: pointer); cdecl;
    procedure ControllerDidDisconnect(ANotification: pointer); cdecl;
  end;

  TControllersNotificationHandler = class(toclocal)
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure ControllerDidConnect(ANotification: pointer); cdecl;
    procedure ControllerDidDisconnect(ANotification: pointer); cdecl;
  end;

var
  ControllersNotificationHandler: TControllersNotificationHandler;

type
  TControllerItem = class
  private
    FController: GCController;
    FisConnected: boolean;
    procedure SetController(const Value: GCController);
    procedure SetisConnected(const Value: boolean);
  public
    property isConnected: boolean read FisConnected write SetisConnected;
    property Controller: GCController read FController write SetController;
    constructor Create(AController: GCController); virtual;
    destructor Destroy; override;
  end;

  TControllerList = TList<TControllerItem>;

  TControllers = class
  private
    FControllerList: TControllerList;
  protected
    function getCount: integer;
  public
    property Count: integer read getCount;
    procedure Add(AController: GCController);
    // function IndexOf(AController: GCController): integer; overload; deprecated;
    function IndexOf(AControllerItem: TControllerItem): integer; overload;
    function ItemAt(Index: integer): TControllerItem;
    function ControllerAt(Index: integer): GCController;
    function isConnected(Index: integer): boolean; overload;
    // function isConnected(AController: GCController): boolean; overload;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

var
  Controllers: TControllers;

  { TGamolfJoystickService }

function TGamolfJoystickService.Count: byte;
begin
  result := Controllers.Count;
end;

constructor TGamolfJoystickService.Create;
begin
  inherited;
  TNSNotificationCenter.wrap(TNSNotificationCenter.OCClass.defaultCenter)
    .addObserver(ControllersNotificationHandler.GetObjectID,
    sel_getUid('ControllerDidConnect:'),
    nsstringtoid(GCControllerDidConnectNotification), nil);
  TNSNotificationCenter.wrap(TNSNotificationCenter.OCClass.defaultCenter)
    .addObserver(ControllersNotificationHandler.GetObjectID,
    sel_getUid('ControllerDidDisconnect:'),
    nsstringtoid(GCControllerDidDisconnectNotification), nil);
end;

procedure TGamolfJoystickService.getInfo(JoystickID: TJoystickID;
  var Joystick: TJoystickInfo);
var
  LControllerItem: TControllerItem;
  LController: GCController;
  LExtendedGamepad: GCExtendedGamepad;
  LMicroGamepad: GCMicroGamepad;
  i: integer;
begin
  try
    // TODO : add something to check if this controller is the current one (last used by the user)
    // TODO : add battery level
    // TODO : add motion sensors
    LControllerItem := Controllers.ItemAt(JoystickID);
    if assigned(LControllerItem) and LControllerItem.isConnected then
      LController := LControllerItem.Controller
    else
      LController := nil;
    if assigned(LController) and assigned(LController.extendedGamepad) then
    begin
      LExtendedGamepad := LController.extendedGamepad;
      //
      // Initialize axes list
      //
      if (length(Joystick.Axes) <> 6) then
        setlength(Joystick.Axes, 6);
      // X
      Joystick.Axes[0] := LExtendedGamepad.leftThumbstick.xAxis.Value;
      // Y
      Joystick.Axes[1] := -LExtendedGamepad.leftThumbstick.yAxis.Value;
      // Z
      Joystick.Axes[2] := LExtendedGamepad.rightThumbstick.xAxis.Value;
      // R
      Joystick.Axes[3] := -LExtendedGamepad.rightThumbstick.yAxis.Value;
      // U
      Joystick.Axes[4] := LExtendedGamepad.leftTrigger.Value;
      // V
      Joystick.Axes[5] := LExtendedGamepad.rightTrigger.Value;
      //
      // Initialize buttons list
      //
      Joystick.initButtonsToJoystickButtons;
      Joystick.setPressed(TJoystickButtons.a,
        LExtendedGamepad.buttonA.isPressed);
      Joystick.setPressed(TJoystickButtons.b,
        LExtendedGamepad.buttonb.isPressed);
      Joystick.setPressed(TJoystickButtons.x,
        LExtendedGamepad.buttonx.isPressed);
      Joystick.setPressed(TJoystickButtons.y,
        LExtendedGamepad.buttony.isPressed);
      Joystick.setPressed(TJoystickButtons.Home,
        LExtendedGamepad.buttonHome.isPressed);
      Joystick.setPressed(TJoystickButtons.Options,
        LExtendedGamepad.buttonOptions.isPressed);
      Joystick.setPressed(TJoystickButtons.Menu,
        LExtendedGamepad.buttonMenu.isPressed);
      Joystick.setPressed(TJoystickButtons.leftShoulder,
        LExtendedGamepad.leftShoulder.isPressed);
      Joystick.setPressed(TJoystickButtons.rightShoulder,
        LExtendedGamepad.rightShoulder.isPressed);
      Joystick.setPressed(TJoystickButtons.leftTrigger,
        LExtendedGamepad.leftTrigger.isPressed);
      Joystick.setPressed(TJoystickButtons.rightTrigger,
        LExtendedGamepad.rightTrigger.isPressed);
      Joystick.setPressed(TJoystickButtons.leftThumbstick,
        LExtendedGamepad.leftThumbstickButton.isPressed);
      Joystick.setPressed(TJoystickButtons.rightThumbstick,
        LExtendedGamepad.rightThumbstickButton.isPressed);
      //
      // Initialize D-pad
      //
      Joystick.dpad := ord(getDPadPosition(LExtendedGamepad.dpad));
    end
    else if assigned(LController) and assigned(LController.microGamepad) then
    begin
      LMicroGamepad := LController.microGamepad;
      initJoystick(Joystick);
      Joystick.setPressed(TJoystickButtons.a, LMicroGamepad.buttonA.isPressed);
      Joystick.setPressed(TJoystickButtons.x, LMicroGamepad.buttonx.isPressed);
      Joystick.setPressed(TJoystickButtons.Menu,
        LMicroGamepad.buttonMenu.isPressed);
      Joystick.dpad := ord(getDPadPosition(LMicroGamepad.dpad));
    end
    else
      initJoystick(Joystick);
  except
    // TODO : find a better way to manage exceptions
    Controllers.ItemAt(JoystickID).isConnected := false;
    initJoystick(Joystick);
  end;
end;

function TGamolfJoystickService.hasDPad(JoystickID: TJoystickID): boolean;
var
  LController: GCController;
begin
  LController := Controllers.ControllerAt(JoystickID);
  if not assigned(LController) then
    result := false
  else
    result := (LController.extendedGamepad <> nil) or
      (LController.microGamepad <> nil);
end;

function TGamolfJoystickService.hasJoystickButtonsAPI: boolean;
begin
  result := true;
end;

function TGamolfJoystickService.isConnected(JoystickID: TJoystickID): boolean;
begin
  result := Controllers.isConnected(JoystickID);
end;

procedure TGamolfJoystickService.StartDiscovery;
begin
  TGCController.OCClass.startWirelessControllerDiscoveryWithCompletionHandler
    (WirelessControllerDiscoveryFinished);
end;

procedure TGamolfJoystickService.WirelessControllerDiscoveryFinished;
var
  LControllers: NSArray;
  LController: GCController;
  i: integer;
begin
  // TODO : à compléter
  for i := 0 to 100 do;
  (* LControllers := TGCController.OCClass.Controllers;
    for i := 0 to LControllers.Count - 1 do
    begin
    LController := TGCController.wrap(LControllers.objectAtIndex(i));
    if (Controllers.IndexOf(LController) < 0) then
    Controllers.Add(LController);
    end; *)
end;

{ TControllersNotificationHandler }

procedure TControllersNotificationHandler.ControllerDidConnect
  (ANotification: pointer); cdecl;
var
  LNotification: nsnotification;
  LController: GCController;
  Index: integer;
begin
  LNotification := TNSNotification.wrap(ANotification);
  LController := TGCController.wrap(LNotification.&object);
  // TODO : try to find a uniq ID to identify each GCController and check if it's already in the list
  // index := Controllers.IndexOf(LController);
  // if (index < 0) then
  Controllers.Add(LController)
  // else
  // Controllers.ItemAt(index).isConnected := true;
end;

procedure TControllersNotificationHandler.ControllerDidDisconnect
  (ANotification: pointer); cdecl;
var
  LNotification: nsnotification;
  LController: GCController;
  LItem: TControllerItem;
begin
  // TODO : try to get controller MAC (or other uniq id) to check if its already connected

  // LNotification := TNSNotification.wrap(ANotification);
  // LController := TGCController.wrap(LNotification.&object);
  // LItem := Controllers.ItemAt(Controllers.IndexOf(LController));
  // if assigned(LItem) then
  // LItem.isConnected := false;
end;

function TControllersNotificationHandler.GetObjectiveCClass: PTypeInfo;
begin
  result := TypeInfo(IControllersNotificationHandler);
end;

{ TControllerItem }

constructor TControllerItem.Create(AController: GCController);
begin
  FisConnected := true;
  FController := AController;
end;

destructor TControllerItem.Destroy;
begin
  // FController.release;
  // Done by Objective-C (ARC)
  inherited;
end;

procedure TControllerItem.SetController(const Value: GCController);
begin
  FController := Value;
end;

procedure TControllerItem.SetisConnected(const Value: boolean);
begin
  FisConnected := Value;
end;

{ TControllers }

procedure TControllers.Add(AController: GCController);
begin
  FControllerList.Add(TControllerItem.Create(AController));
end;

function TControllers.ControllerAt(Index: integer): GCController;
begin
  if (index >= 0) and (index < FControllerList.Count) then
    result := FControllerList[index].Controller
  else
    result := nil;
end;

constructor TControllers.Create;
begin
  FControllerList := TControllerList.Create;
end;

destructor TControllers.Destroy;
begin
  FControllerList.Free;
  inherited;
end;

function TControllers.getCount: integer;
begin
  result := FControllerList.Count;
end;

// No usable because Apple send a new pointer to same controllers with
// notification. The only place where looking for a controller in the
// list is useful.
//
// function TControllers.IndexOf(AController: GCController): integer;
// var
// i: integer;
// begin
// result := -1;
// for i := 0 to FControllerList.Count - 1 do
// if (FControllerList[i].Controller = AController) then
// begin
// result := i;
// break;
// end;
// end;

function TControllers.IndexOf(AControllerItem: TControllerItem): integer;
begin
  result := FControllerList.IndexOf(AControllerItem);
end;

// function TControllers.isConnected(AController: GCController): boolean;
// var
// Index: integer;
// begin
// index := IndexOf(AController);
// if (index >= 0) and (index < FControllerList.Count) then
// result := FControllerList[index].isConnected
// else
// result := false;
// end;

function TControllers.isConnected(Index: integer): boolean;
begin
  if (index >= 0) and (index < FControllerList.Count) then
    result := FControllerList[index].isConnected
  else
    result := false;
end;

function TControllers.ItemAt(Index: integer): TControllerItem;
begin
  if (index >= 0) and (index < FControllerList.Count) then
    result := FControllerList[index]
  else
    result := nil;
end;

initialization

Controllers := TControllers.Create;
ControllersNotificationHandler := TControllersNotificationHandler.Create;

finalization

ControllersNotificationHandler.Free;
Controllers.Free;
{ $ELSE }

// implementation

{ $ENDIF }

end.
