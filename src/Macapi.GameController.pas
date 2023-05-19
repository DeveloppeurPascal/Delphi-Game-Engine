{ *********************************************************** }
{ }
{ CodeGear Delphi Runtime Library }
{ }
{ Copyright(c) 2012-2014 Embarcadero Technologies, Inc. }
{ }
{ *********************************************************** }

//
// Delphi-Objective-C Bridge
// Interfaces for Cocoa framework GameController
//

unit Macapi.GameController;

interface

uses
  Macapi.AppKit,
  Macapi.CocoaTypes,
  Macapi.CoreFoundation,
  Macapi.Dispatch,
  Macapi.Foundation,
  Macapi.IOKit,
  Macapi.Mach,
  Macapi.ObjCRuntime,
  Macapi.ObjectiveC;

const
  GCDevicePhysicalInputElementUnknownChange = -1;
  GCDevicePhysicalInputElementNoChange = 0;
  GCDevicePhysicalInputElementChanged = 1;
  GCDeviceBatteryStateUnknown = -1;
  GCDeviceBatteryStateDischarging = 0;
  GCDeviceBatteryStateCharging = 1;
  GCDeviceBatteryStateFull = 2;
  GCSystemGestureStateEnabled = 0;
  GCSystemGestureStateAlwaysReceive = 1;
  GCSystemGestureStateDisabled = 2;
  GCTouchStateUp = 0;
  GCTouchStateDown = 1;
  GCTouchStateMoving = 2;
  GCDualSenseAdaptiveTriggerDiscretePositionCount = 10;
  GCDualSenseAdaptiveTriggerModeOff = 0;
  GCDualSenseAdaptiveTriggerModeFeedback = 1;
  GCDualSenseAdaptiveTriggerModeWeapon = 2;
  GCDualSenseAdaptiveTriggerModeVibration = 3;
  GCDualSenseAdaptiveTriggerModeSlopeFeedback = 4;
  GCDualSenseAdaptiveTriggerStatusUnknown = -1;
  GCDualSenseAdaptiveTriggerStatusFeedbackNoLoad = 0;
  GCDualSenseAdaptiveTriggerStatusFeedbackLoadApplied = 1;
  GCDualSenseAdaptiveTriggerStatusWeaponReady = 2;
  GCDualSenseAdaptiveTriggerStatusWeaponFiring = 3;
  GCDualSenseAdaptiveTriggerStatusWeaponFired = 4;
  GCDualSenseAdaptiveTriggerStatusVibrationNotVibrating = 5;
  GCDualSenseAdaptiveTriggerStatusVibrationIsVibrating = 6;
  GCDualSenseAdaptiveTriggerStatusSlopeFeedbackReady = 7;
  GCDualSenseAdaptiveTriggerStatusSlopeFeedbackApplyingLoad = 8;
  GCDualSenseAdaptiveTriggerStatusSlopeFeedbackFinished = 9;
  GCExtendedGamepadSnapshotDataVersion1 = 256;
  GCExtendedGamepadSnapshotDataVersion2 = 257;
  GCMicroGamepadSnapshotDataVersion1 = 256;
  GCControllerPlayerIndexUnset = -1;
  GCControllerPlayerIndex1 = 0;
  GCControllerPlayerIndex2 = 1;
  GCControllerPlayerIndex3 = 2;
  GCControllerPlayerIndex4 = 3;

type

  // ===== Forward declarations =====
{$M+}
  GCColor = interface;
  GCPhysicalInputProfile = interface;
  GCDevice = interface;
  GCPhysicalInputElementName = interface;
  GCButtonElementName = interface;
  GCAxisElementName = interface;
  GCSwitchElementName = interface;
  GCDirectionPadElementName = interface;
  GCPhysicalInputElement = interface;
  GCPhysicalInputElementCollection = interface;
  GCLinearInput = interface;
  GCPressedStateInput = interface;
  GCTouchedStateInput = interface;
  GCButtonElement = interface;
  GCAxisInput = interface;
  GCRelativeInput = interface;
  GCAxisElement = interface;
  GCSwitchPositionInput = interface;
  GCSwitchElement = interface;
  GCDirectionPadElement = interface;
  GCDevicePhysicalInputState = interface;
  GCDevicePhysicalInputStateDiff = interface;
  GCDevicePhysicalInput = interface;
  GCDeviceLight = interface;
  GCDeviceBattery = interface;
  GCControllerElement = interface;
  GCControllerAxisInput = interface;
  GCControllerButtonInput = interface;
  GCControllerDirectionPad = interface;
  GCControllerTouchpad = interface;
  GCDualSenseAdaptiveTrigger = interface;
  GCDeviceCursor = interface;
  GCController = interface;
  GCMotion = interface;
  GCGamepadSnapshot = interface;
  GCGamepad = interface;
  GCExtendedGamepadSnapshot = interface;
  GCExtendedGamepad = interface;
  GCKeyboardInput = interface;
  GCMouseInput = interface;
  GCXboxGamepad = interface;
  GCDualShockGamepad = interface;
  GCDualSenseGamepad = interface;
  GCMicroGamepadSnapshot = interface;
  GCMicroGamepad = interface;
  GCDirectionalGamepad = interface;
  GCDeviceHaptics = interface;
  GCKeyboard = interface;
  GCMouse = interface;
  GCEventViewController = interface;
  GCRacingWheelInput = interface;
  GCRacingWheel = interface;
  GCButtonInput = interface;
  GCSwitchInput = interface;
  GCSteeringWheelElement = interface;
  GCGearShifterElement = interface;
  GCRacingWheelInputState = interface;

  // ===== Framework typedefs =====
{$M+}
  dispatch_queue_t = Pointer;
  Pdispatch_queue_t = ^dispatch_queue_t;
  GCInputElementName = NSString;
  PGCInputElementName = ^GCInputElementName;
  GCInputButtonName = NSString;
  PGCInputButtonName = ^GCInputButtonName;
  GCInputAxisName = NSString;
  PGCInputAxisName = ^GCInputAxisName;
  GCInputSwitchName = NSString;
  PGCInputSwitchName = ^GCInputSwitchName;
  GCInputDirectionPadName = NSString;
  PGCInputDirectionPadName = ^GCInputDirectionPadName;
  NSUInteger = LongWord;
  PNSUInteger = ^NSUInteger;

  Key = NSString;
  PKey = ^Key;
  Element = Pointer;
  PElement = ^Element;
  TGameControllerValueDidChangeHandler = procedure(param1: Pointer;
    param2: Pointer; param3: Single) of object;
  NSTimeInterval = Double;
  PNSTimeInterval = ^NSTimeInterval;

  TGameControllerPressedDidChangeHandler = procedure(param1: Pointer;
    param2: Pointer; param3: Boolean) of object;
  NSInteger = LongInt;
  PNSInteger = ^NSInteger;

  TGameControllerPositionDidChangeHandler = procedure(param1: Pointer;
    param2: Pointer; param3: NSInteger) of object;

  _NSRange = record
    location: NSUInteger;
    length: NSUInteger;
  end;

  P_NSRange = ^_NSRange;
  NSRange = _NSRange;
  PNSRange = ^NSRange;

  GCDevicePhysicalInputElementChange = NSInteger;
  TGameControllerElementValueDidChangeHandler = procedure(param1: Pointer;
    param2: Pointer) of object;
  TGameControllerInputStateAvailableHandler = procedure(param1: Pointer)
    of object;
  GCDeviceBatteryState = NSInteger;
  GCSystemGestureState = NSInteger;
  GCControllerAxisValueChangedHandler = procedure(param1: GCControllerAxisInput;
    param2: Single) of object;
  GCControllerButtonValueChangedHandler = procedure
    (param1: GCControllerButtonInput; param2: Single; param3: Boolean)
    of object;
  GCControllerButtonTouchedChangedHandler = procedure
    (param1: GCControllerButtonInput; param2: Single; param3: Boolean;
    param4: Boolean) of object;
  GCControllerDirectionPadValueChangedHandler = procedure
    (param1: GCControllerDirectionPad; param2: Single; param3: Single)
    of object;
  GCTouchState = NSInteger;
  GCControllerTouchpadHandler = procedure(param1: GCControllerTouchpad;
    param2: Single; param3: Single; param4: Single; param5: Boolean) of object;
  GCDualSenseAdaptiveTriggerMode = NSInteger;
  GCDualSenseAdaptiveTriggerStatus = NSInteger;

  GCDualSenseAdaptiveTriggerPositionalAmplitudes = record
    values: array [0 .. 9] of Single;
  end;

  PGCDualSenseAdaptiveTriggerPositionalAmplitudes = ^
    GCDualSenseAdaptiveTriggerPositionalAmplitudes;

  GCDualSenseAdaptiveTriggerPositionalResistiveStrengths = record
    values: array [0 .. 9] of Single;
  end;

  PGCDualSenseAdaptiveTriggerPositionalResistiveStrengths = ^
    GCDualSenseAdaptiveTriggerPositionalResistiveStrengths;

  GCAcceleration = record
    x: Double;
    y: Double;
    z: Double;
  end;

  PGCAcceleration = ^GCAcceleration;

  GCRotationRate = record
    x: Double;
    y: Double;
    z: Double;
  end;

  PGCRotationRate = ^GCRotationRate;

  GCEulerAngles = record
    pitch: Double;
    yaw: Double;
    roll: Double;
  end;

  PGCEulerAngles = ^GCEulerAngles;

  GCQuaternion = record
    x: Double;
    y: Double;
    z: Double;
    w: Double;
  end;

  PGCQuaternion = ^GCQuaternion;

  GCMotionValueChangedHandler = procedure(param1: GCMotion) of object;
  TGameControllerValueDidChangeHandler1 = procedure
    (param1: GCPhysicalInputProfile; param2: GCControllerElement) of object;
  GCGamepadValueChangedHandler = procedure(param1: GCGamepad;
    param2: GCControllerElement) of object;

  GCGamepadSnapShotDataV100 = record
    version: Word;
    size: Word;
    dpadX: Single;
    dpadY: Single;
    buttonA: Single;
    buttonB: Single;
    buttonX: Single;
    buttonY: Single;
    leftShoulder: Single;
    rightShoulder: Single;
  end;

  PGCGamepadSnapShotDataV100 = ^GCGamepadSnapShotDataV100;

  GCExtendedGamepadValueChangedHandler = procedure(param1: GCExtendedGamepad;
    param2: GCControllerElement) of object;
  GCExtendedGamepadSnapshotDataVersion = NSInteger;

  GCExtendedGamepadSnapshotData = record
    version: Word;
    size: Word;
    dpadX: Single;
    dpadY: Single;
    buttonA: Single;
    buttonB: Single;
    buttonX: Single;
    buttonY: Single;
    leftShoulder: Single;
    rightShoulder: Single;
    leftThumbstickX: Single;
    leftThumbstickY: Single;
    rightThumbstickX: Single;
    rightThumbstickY: Single;
    leftTrigger: Single;
    rightTrigger: Single;
    supportsClickableThumbsticks: Boolean;
    leftThumbstickButton: Boolean;
    rightThumbstickButton: Boolean;
  end;

  PGCExtendedGamepadSnapshotData = ^GCExtendedGamepadSnapshotData;

  GCExtendedGamepadSnapShotDataV100 = record
    version: Word;
    size: Word;
    dpadX: Single;
    dpadY: Single;
    buttonA: Single;
    buttonB: Single;
    buttonX: Single;
    buttonY: Single;
    leftShoulder: Single;
    rightShoulder: Single;
    leftThumbstickX: Single;
    leftThumbstickY: Single;
    rightThumbstickX: Single;
    rightThumbstickY: Single;
    leftTrigger: Single;
    rightTrigger: Single;
  end;

  PGCExtendedGamepadSnapShotDataV100 = ^GCExtendedGamepadSnapShotDataV100;

  CFIndex = LongInt;
  PCFIndex = ^CFIndex;

  GCKeyCode = CFIndex;
  PGCKeyCode = ^GCKeyCode;
  GCKeyboardValueChangedHandler = procedure(param1: GCKeyboardInput;
    param2: GCControllerButtonInput; param3: GCKeyCode; param4: Boolean)
    of object;
  GCMouseMoved = procedure(param1: GCMouseInput; param2: Single; param3: Single)
    of object;
  GCMicroGamepadValueChangedHandler = procedure(param1: GCMicroGamepad;
    param2: GCControllerElement) of object;
  GCMicroGamepadSnapshotDataVersion = NSInteger;

  GCMicroGamepadSnapshotData = record
    version: Word;
    size: Word;
    dpadX: Single;
    dpadY: Single;
    buttonA: Single;
    buttonX: Single;
  end;

  PGCMicroGamepadSnapshotData = ^GCMicroGamepadSnapshotData;

  GCMicroGamepadSnapShotDataV100 = record
    version: Word;
    size: Word;
    dpadX: Single;
    dpadY: Single;
    buttonA: Single;
    buttonX: Single;
  end;

  PGCMicroGamepadSnapShotDataV100 = ^GCMicroGamepadSnapShotDataV100;

  GCControllerPlayerIndex = NSInteger;
  TGameControllerControllerPausedHandler = procedure(param1: GCController)
    of object;
  TGameControllerCompletionHandler = procedure() of object;
  IOHIDDeviceRef = Pointer;
  PIOHIDDeviceRef = ^IOHIDDeviceRef;
  GCHapticsLocality = NSString;
  PGCHapticsLocality = ^GCHapticsLocality;
  // ===== Interface declarations =====

  GCColorClass = interface(NSObjectClass)
    ['{37F7921A-9A35-4A60-A58A-B431E52CDB6D}']
  end;

  GCColor = interface(NSObject)
    ['{F6D9DC80-073C-44D8-A497-799B22C71716}']
    function initWithRed(red: Single; green: Single; blue: Single)
      : Pointer { instancetype }; cdecl;
    function red: Single; cdecl;
    function green: Single; cdecl;
    function blue: Single; cdecl;
  end;

  TGCColor = class(TOCGenericImport<GCColorClass, GCColor>)
  end;

  PGCColor = Pointer;

  GCPhysicalInputProfileClass = interface(NSObjectClass)
    ['{4AF0C435-A503-4282-94FB-CAB0277EC02F}']
  end;

  GCPhysicalInputProfile = interface(NSObject)
    ['{FF9AACC3-0B69-43D7-9AD9-0DAAD0D4BCB3}']
    function device: Pointer; cdecl;
    function lastEventTimestamp: NSTimeInterval; cdecl;
    function hasRemappedElements: Boolean; cdecl;
    procedure setValueDidChangeHandler(valueDidChangeHandler
      : TGameControllerValueDidChangeHandler1); cdecl;
    function valueDidChangeHandler
      : TGameControllerValueDidChangeHandler1; cdecl;
    function elements: NSDictionary; cdecl;
    function buttons: NSDictionary; cdecl;
    function axes: NSDictionary; cdecl;
    function dpads: NSDictionary; cdecl;
    function touchpads: NSDictionary; cdecl;
    function allElements: NSSet; cdecl;
    function allButtons: NSSet; cdecl;
    function allAxes: NSSet; cdecl;
    function allDpads: NSSet; cdecl;
    function allTouchpads: NSSet; cdecl;
    function objectForKeyedSubscript(Key: NSString): GCControllerElement; cdecl;
    function capture: Pointer { instancetype }; cdecl;
    procedure setStateFromPhysicalInput(physicalInput
      : GCPhysicalInputProfile); cdecl;
    function mappedElementAliasForPhysicalInputName(inputName: NSString)
      : NSString; cdecl;
    function mappedPhysicalInputNamesForElementAlias(elementAlias: NSString)
      : NSSet; cdecl;
  end;

  TGCPhysicalInputProfile = class(TOCGenericImport<GCPhysicalInputProfileClass,
    GCPhysicalInputProfile>)
  end;

  PGCPhysicalInputProfile = Pointer;

  GCPhysicalInputElementCollectionClass = interface(NSObjectClass)
    ['{99D2A321-E8DF-4037-9C60-CCB19CFB9CA9}']
  end;

  GCPhysicalInputElementCollection = interface(NSObject)
    ['{29BBB785-3F2E-419B-B639-783290532A93}']
    function count: NSUInteger; cdecl;
    function elementForAlias(alias: Key): Element; cdecl;
    function objectForKeyedSubscript(Key: Key): Element; cdecl;
    function elementEnumerator: NSEnumerator; cdecl;
  end;

  TGCPhysicalInputElementCollection = class
    (TOCGenericImport<GCPhysicalInputElementCollectionClass,
    GCPhysicalInputElementCollection>)
  end;

  PGCPhysicalInputElementCollection = Pointer;

  GCDeviceLightClass = interface(NSObjectClass)
    ['{7E9BF2A7-0885-40BB-AB7B-B22CAB3926C4}']
  end;

  GCDeviceLight = interface(NSObject)
    ['{CD7DA353-51C0-40E1-8059-004555752371}']
    procedure setColor(color: GCColor); cdecl;
    function color: GCColor; cdecl;
  end;

  TGCDeviceLight = class(TOCGenericImport<GCDeviceLightClass, GCDeviceLight>)
  end;

  PGCDeviceLight = Pointer;

  GCDeviceBatteryClass = interface(NSObjectClass)
    ['{1B72AD2A-595F-48C9-884E-8325543934E6}']
  end;

  GCDeviceBattery = interface(NSObject)
    ['{73878BA5-9504-4CCC-8072-A0ABB349F588}']
    function batteryLevel: Single; cdecl;
    function batteryState: GCDeviceBatteryState; cdecl;
  end;

  TGCDeviceBattery = class(TOCGenericImport<GCDeviceBatteryClass,
    GCDeviceBattery>)
  end;

  PGCDeviceBattery = Pointer;

  GCControllerElementClass = interface(NSObjectClass)
    ['{7F0AB30B-9A81-49BA-9471-ED3632A0891A}']
  end;

  GCControllerElement = interface(NSObject)
    ['{E47D2640-8659-4230-9929-D276E5D4EA0E}']
    function collection: GCControllerElement; cdecl;
    function isAnalog: Boolean; cdecl;
    function isBoundToSystemGesture: Boolean; cdecl;
    procedure setPreferredSystemGestureState(preferredSystemGestureState
      : GCSystemGestureState); cdecl;
    function preferredSystemGestureState: GCSystemGestureState; cdecl;
    procedure setSfSymbolsName(sfSymbolsName: NSString); cdecl;
    function sfSymbolsName: NSString; cdecl;
    procedure setLocalizedName(localizedName: NSString); cdecl;
    function localizedName: NSString; cdecl;
    procedure setUnmappedSfSymbolsName(unmappedSfSymbolsName: NSString); cdecl;
    function unmappedSfSymbolsName: NSString; cdecl;
    procedure setUnmappedLocalizedName(unmappedLocalizedName: NSString); cdecl;
    function unmappedLocalizedName: NSString; cdecl;
    function aliases: NSSet; cdecl;
  end;

  TGCControllerElement = class(TOCGenericImport<GCControllerElementClass,
    GCControllerElement>)
  end;

  PGCControllerElement = Pointer;

  GCControllerAxisInputClass = interface(GCControllerElementClass)
    ['{9F9C9854-1317-4D6C-B4B8-FC7132A054C4}']
  end;

  GCControllerAxisInput = interface(GCControllerElement)
    ['{BC99A962-FE2B-4D71-BCA1-0F896B25895D}']
    procedure setValueChangedHandler(valueChangedHandler
      : GCControllerAxisValueChangedHandler); cdecl;
    function valueChangedHandler: GCControllerAxisValueChangedHandler; cdecl;
    function value: Single; cdecl;
    procedure setValue(value: Single); cdecl;
  end;

  TGCControllerAxisInput = class(TOCGenericImport<GCControllerAxisInputClass,
    GCControllerAxisInput>)
  end;

  PGCControllerAxisInput = Pointer;

  GCControllerButtonInputClass = interface(GCControllerElementClass)
    ['{E9A97D30-05FF-49D6-8B99-D7C6DEE34F49}']
  end;

  GCControllerButtonInput = interface(GCControllerElement)
    ['{26E61CC9-1008-477D-8ECD-48BEFD3E1987}']
    procedure setValueChangedHandler(valueChangedHandler
      : GCControllerButtonValueChangedHandler); cdecl;
    function valueChangedHandler: GCControllerButtonValueChangedHandler; cdecl;
    procedure setPressedChangedHandler(pressedChangedHandler
      : GCControllerButtonValueChangedHandler); cdecl;
    function pressedChangedHandler
      : GCControllerButtonValueChangedHandler; cdecl;
    procedure setTouchedChangedHandler(touchedChangedHandler
      : GCControllerButtonTouchedChangedHandler); cdecl;
    function touchedChangedHandler
      : GCControllerButtonTouchedChangedHandler; cdecl;
    function value: Single; cdecl;
    function isPressed: Boolean; cdecl;
    function isTouched: Boolean; cdecl;
    procedure setValue(value: Single); cdecl;
  end;

  TGCControllerButtonInput = class
    (TOCGenericImport<GCControllerButtonInputClass, GCControllerButtonInput>)
  end;

  PGCControllerButtonInput = Pointer;

  GCControllerDirectionPadClass = interface(GCControllerElementClass)
    ['{1CE9F6C4-0C0A-4155-9A6C-B6DA09EF317D}']
  end;

  GCControllerDirectionPad = interface(GCControllerElement)
    ['{A78EBAA5-CC43-4A14-9D0A-EA7A9DAC1A57}']
    procedure setValueChangedHandler(valueChangedHandler
      : GCControllerDirectionPadValueChangedHandler); cdecl;
    function valueChangedHandler
      : GCControllerDirectionPadValueChangedHandler; cdecl;
    function xAxis: GCControllerAxisInput; cdecl;
    function yAxis: GCControllerAxisInput; cdecl;
    function up: GCControllerButtonInput; cdecl;
    function down: GCControllerButtonInput; cdecl;
    function left: GCControllerButtonInput; cdecl;
    function right: GCControllerButtonInput; cdecl;
    procedure setValueForXAxis(xAxis: Single; yAxis: Single); cdecl;
  end;

  TGCControllerDirectionPad = class
    (TOCGenericImport<GCControllerDirectionPadClass, GCControllerDirectionPad>)
  end;

  PGCControllerDirectionPad = Pointer;

  GCControllerTouchpadClass = interface(GCControllerElementClass)
    ['{5C4D4F4A-2086-4087-AC3F-13ECE4D1A910}']
  end;

  GCControllerTouchpad = interface(GCControllerElement)
    ['{59D32078-F412-4BDE-8E28-52B8608555AA}']
    function button: GCControllerButtonInput; cdecl;
    procedure setTouchDown(touchDown: GCControllerTouchpadHandler); cdecl;
    function touchDown: GCControllerTouchpadHandler; cdecl;
    procedure setTouchMoved(touchMoved: GCControllerTouchpadHandler); cdecl;
    function touchMoved: GCControllerTouchpadHandler; cdecl;
    procedure setTouchUp(touchUp: GCControllerTouchpadHandler); cdecl;
    function touchUp: GCControllerTouchpadHandler; cdecl;
    function touchSurface: GCControllerDirectionPad; cdecl;
    function touchState: GCTouchState; cdecl;
    procedure setReportsAbsoluteTouchSurfaceValues
      (reportsAbsoluteTouchSurfaceValues: Boolean); cdecl;
    function reportsAbsoluteTouchSurfaceValues: Boolean; cdecl;
    procedure setValueForXAxis(xAxis: Single; yAxis: Single; touchDown: Boolean;
      buttonValue: Single); cdecl;
  end;

  TGCControllerTouchpad = class(TOCGenericImport<GCControllerTouchpadClass,
    GCControllerTouchpad>)
  end;

  PGCControllerTouchpad = Pointer;

  GCDualSenseAdaptiveTriggerClass = interface(GCControllerButtonInputClass)
    ['{4355C806-B0E4-41C3-868D-8B9353F082A8}']
  end;

  GCDualSenseAdaptiveTrigger = interface(GCControllerButtonInput)
    ['{1B767EB4-4BA9-4155-B330-0F0518A824EF}']
    function mode: GCDualSenseAdaptiveTriggerMode; cdecl;
    function status: GCDualSenseAdaptiveTriggerStatus; cdecl;
    function armPosition: Single; cdecl;
    procedure setModeSlopeFeedbackWithStartPosition(startPosition: Single;
      endPosition: Single; startStrength: Single; endStrength: Single); cdecl;
    procedure setModeFeedbackWithStartPosition(startPosition: Single;
      resistiveStrength: Single); cdecl;
    procedure setModeFeedbackWithResistiveStrengths(positionalResistiveStrengths
      : GCDualSenseAdaptiveTriggerPositionalResistiveStrengths); cdecl;
    procedure setModeWeaponWithStartPosition(startPosition: Single;
      endPosition: Single; resistiveStrength: Single); cdecl;
    procedure setModeVibrationWithStartPosition(startPosition: Single;
      amplitude: Single; frequency: Single); cdecl;
    procedure setModeVibrationWithAmplitudes(positionalAmplitudes
      : GCDualSenseAdaptiveTriggerPositionalAmplitudes;
      frequency: Single); cdecl;
    procedure setModeOff; cdecl;
  end;

  TGCDualSenseAdaptiveTrigger = class
    (TOCGenericImport<GCDualSenseAdaptiveTriggerClass,
    GCDualSenseAdaptiveTrigger>)
  end;

  PGCDualSenseAdaptiveTrigger = Pointer;

  GCDeviceCursorClass = interface(GCControllerDirectionPadClass)
    ['{C5D54E73-0E79-429D-9815-DBBA3862EA51}']
  end;

  GCDeviceCursor = interface(GCControllerDirectionPad)
    ['{2A0DAC48-A68C-4532-9A4A-BCEDE160C0B0}']
  end;

  TGCDeviceCursor = class(TOCGenericImport<GCDeviceCursorClass, GCDeviceCursor>)
  end;

  PGCDeviceCursor = Pointer;

  GCControllerClass = interface(NSObjectClass)
    ['{0318DF7B-DB02-4293-A79A-63974585109D}']
    { class } function controllers: NSArray; cdecl;
    { class } procedure startWirelessControllerDiscoveryWithCompletionHandler
      (completionHandler: TGameControllerCompletionHandler); cdecl;
    { class } procedure stopWirelessControllerDiscovery; cdecl;
    { class } function controllerWithMicroGamepad: GCController; cdecl;
    { class } function controllerWithExtendedGamepad: GCController; cdecl;
    { class } function supportsHIDDevice(device: IOHIDDeviceRef)
      : Boolean; cdecl;
  end;

  GCController = interface(NSObject)
    ['{904D0BBF-AC25-48C5-9B9F-B73FC7A2D00A}']
    procedure setControllerPausedHandler(controllerPausedHandler
      : TGameControllerControllerPausedHandler); cdecl;
    function controllerPausedHandler
      : TGameControllerControllerPausedHandler; cdecl;
    procedure setCurrent(current: GCController); cdecl;
    function current: GCController; cdecl;
    procedure setShouldMonitorBackgroundEvents(shouldMonitorBackgroundEvents
      : Boolean); cdecl;
    function shouldMonitorBackgroundEvents: Boolean; cdecl;
    function isAttachedToDevice: Boolean; cdecl;
    function isSnapshot: Boolean; cdecl;
    procedure setPlayerIndex(playerIndex: GCControllerPlayerIndex); cdecl;
    function playerIndex: GCControllerPlayerIndex; cdecl;
    function battery: GCDeviceBattery; cdecl;
    function physicalInputProfile: GCPhysicalInputProfile; cdecl;
    function gamepad: GCGamepad; cdecl;
    function microGamepad: GCMicroGamepad; cdecl;
    function extendedGamepad: GCExtendedGamepad; cdecl;
    function motion: GCMotion; cdecl;
    function light: GCDeviceLight; cdecl;
    function haptics: GCDeviceHaptics; cdecl;
    function capture: GCController; cdecl;
  end;

  TGCController = class(TOCGenericImport<GCControllerClass, GCController>)
  end;

  PGCController = Pointer;

  GCMotionClass = interface(NSObjectClass)
    ['{4430412C-7CA9-4187-831F-301B4DD7A8F7}']
  end;

  GCMotion = interface(NSObject)
    ['{546E65C1-E493-4C93-A5E5-812FAF541BF4}']
    function controller: GCController; cdecl;
    procedure setValueChangedHandler(valueChangedHandler
      : GCMotionValueChangedHandler); cdecl;
    function valueChangedHandler: GCMotionValueChangedHandler; cdecl;
    function sensorsRequireManualActivation: Boolean; cdecl;
    procedure setSensorsActive(sensorsActive: Boolean); cdecl;
    function sensorsActive: Boolean; cdecl;
    function hasGravityAndUserAcceleration: Boolean; cdecl;
    function gravity: GCAcceleration; cdecl;
    function userAcceleration: GCAcceleration; cdecl;
    function acceleration: GCAcceleration; cdecl;
    function hasAttitudeAndRotationRate: Boolean; cdecl;
    function hasAttitude: Boolean; cdecl;
    function hasRotationRate: Boolean; cdecl;
    function attitude: GCQuaternion; cdecl;
    function rotationRate: GCRotationRate; cdecl;
    procedure setGravity(gravity: GCAcceleration); cdecl;
    procedure setUserAcceleration(userAcceleration: GCAcceleration); cdecl;
    procedure setAcceleration(acceleration: GCAcceleration); cdecl;
    procedure setAttitude(attitude: GCQuaternion); cdecl;
    procedure setRotationRate(rotationRate: GCRotationRate); cdecl;
    procedure setStateFromMotion(motion: GCMotion); cdecl;
  end;

  TGCMotion = class(TOCGenericImport<GCMotionClass, GCMotion>)
  end;

  PGCMotion = Pointer;

  GCGamepadClass = interface(GCPhysicalInputProfileClass)
    ['{47DB8A0F-3A1D-46FD-BB3E-A4F7619F6E93}']
  end;

  GCGamepad = interface(GCPhysicalInputProfile)
    ['{168E76B6-CC41-4BCE-B119-198B145DDB15}']
    function controller: GCController; cdecl;
    procedure setValueChangedHandler(valueChangedHandler
      : GCGamepadValueChangedHandler); cdecl;
    function valueChangedHandler: GCGamepadValueChangedHandler; cdecl;
    function saveSnapshot: GCGamepadSnapshot; cdecl;
    function dpad: GCControllerDirectionPad; cdecl;
    function buttonA: GCControllerButtonInput; cdecl;
    function buttonB: GCControllerButtonInput; cdecl;
    function buttonX: GCControllerButtonInput; cdecl;
    function buttonY: GCControllerButtonInput; cdecl;
    function leftShoulder: GCControllerButtonInput; cdecl;
    function rightShoulder: GCControllerButtonInput; cdecl;
  end;

  TGCGamepad = class(TOCGenericImport<GCGamepadClass, GCGamepad>)
  end;

  PGCGamepad = Pointer;

  GCGamepadSnapshotClass = interface(GCGamepadClass)
    ['{5F9D486A-2016-477F-BB77-FCEEB5B3667D}']
  end;

  GCGamepadSnapshot = interface(GCGamepad)
    ['{DE21E2BA-5A27-4A87-97EC-990B5D22B185}']
    procedure setSnapshotData(snapshotData: NSData); cdecl;
    function snapshotData: NSData; cdecl;
    function initWithSnapshotData(data: NSData)
      : Pointer { instancetype }; cdecl;
    function initWithController(controller: GCController; snapshotData: NSData)
      : Pointer { instancetype }; cdecl;
  end;

  TGCGamepadSnapshot = class(TOCGenericImport<GCGamepadSnapshotClass,
    GCGamepadSnapshot>)
  end;

  PGCGamepadSnapshot = Pointer;

  GCExtendedGamepadClass = interface(GCPhysicalInputProfileClass)
    ['{188F55C0-1308-4957-945B-018330C0C4AB}']
  end;

  GCExtendedGamepad = interface(GCPhysicalInputProfile)
    ['{C15F66D1-BFD2-4243-AF48-1C3139300DA7}']
    function controller: GCController; cdecl;
    procedure setValueChangedHandler(valueChangedHandler
      : GCExtendedGamepadValueChangedHandler); cdecl;
    function valueChangedHandler: GCExtendedGamepadValueChangedHandler; cdecl;
    function saveSnapshot: GCExtendedGamepadSnapshot; cdecl;
    function dpad: GCControllerDirectionPad; cdecl;
    function buttonA: GCControllerButtonInput; cdecl;
    function buttonB: GCControllerButtonInput; cdecl;
    function buttonX: GCControllerButtonInput; cdecl;
    function buttonY: GCControllerButtonInput; cdecl;
    function buttonMenu: GCControllerButtonInput; cdecl;
    function buttonOptions: GCControllerButtonInput; cdecl;
    function buttonHome: GCControllerButtonInput; cdecl;
    function leftThumbstick: GCControllerDirectionPad; cdecl;
    function rightThumbstick: GCControllerDirectionPad; cdecl;
    function leftShoulder: GCControllerButtonInput; cdecl;
    function rightShoulder: GCControllerButtonInput; cdecl;
    function leftTrigger: GCControllerButtonInput; cdecl;
    function rightTrigger: GCControllerButtonInput; cdecl;
    function leftThumbstickButton: GCControllerButtonInput; cdecl;
    function rightThumbstickButton: GCControllerButtonInput; cdecl;
    procedure setStateFromExtendedGamepad(extendedGamepad
      : GCExtendedGamepad); cdecl;
  end;

  TGCExtendedGamepad = class(TOCGenericImport<GCExtendedGamepadClass,
    GCExtendedGamepad>)
  end;

  PGCExtendedGamepad = Pointer;

  GCExtendedGamepadSnapshotClass = interface(GCExtendedGamepadClass)
    ['{FAC4707E-98BB-456A-BF70-5586ED3787D5}']
  end;

  GCExtendedGamepadSnapshot = interface(GCExtendedGamepad)
    ['{D9375909-05EE-47FB-B35C-0028F15AF587}']
    procedure setSnapshotData(snapshotData: NSData); cdecl;
    function snapshotData: NSData; cdecl;
    function initWithSnapshotData(data: NSData)
      : Pointer { instancetype }; cdecl;
    function initWithController(controller: GCController; snapshotData: NSData)
      : Pointer { instancetype }; cdecl;
  end;

  TGCExtendedGamepadSnapshot = class
    (TOCGenericImport<GCExtendedGamepadSnapshotClass,
    GCExtendedGamepadSnapshot>)
  end;

  PGCExtendedGamepadSnapshot = Pointer;

  GCKeyboardInputClass = interface(GCPhysicalInputProfileClass)
    ['{63F877D8-D47B-432C-9245-0CB29590F2D0}']
  end;

  GCKeyboardInput = interface(GCPhysicalInputProfile)
    ['{F7E3FFBE-7BBA-445E-9964-82175C9C3217}']
    procedure setKeyChangedHandler(keyChangedHandler
      : GCKeyboardValueChangedHandler); cdecl;
    function keyChangedHandler: GCKeyboardValueChangedHandler; cdecl;
    function isAnyKeyPressed: Boolean; cdecl;
    function buttonForKeyCode(code: GCKeyCode): GCControllerButtonInput; cdecl;
  end;

  TGCKeyboardInput = class(TOCGenericImport<GCKeyboardInputClass,
    GCKeyboardInput>)
  end;

  PGCKeyboardInput = Pointer;

  GCMouseInputClass = interface(GCPhysicalInputProfileClass)
    ['{6104CD86-8B7E-4148-910E-6C87198D3A0C}']
  end;

  GCMouseInput = interface(GCPhysicalInputProfile)
    ['{6408D652-10A8-48C2-A640-81B11DDE0E21}']
    procedure setMouseMovedHandler(mouseMovedHandler: GCMouseMoved); cdecl;
    function mouseMovedHandler: GCMouseMoved; cdecl;
    function scroll: GCDeviceCursor; cdecl;
    function leftButton: GCControllerButtonInput; cdecl;
    function rightButton: GCControllerButtonInput; cdecl;
    function middleButton: GCControllerButtonInput; cdecl;
    function auxiliaryButtons: NSArray; cdecl;
  end;

  TGCMouseInput = class(TOCGenericImport<GCMouseInputClass, GCMouseInput>)
  end;

  PGCMouseInput = Pointer;

  GCXboxGamepadClass = interface(GCExtendedGamepadClass)
    ['{5830BB5B-0E36-4562-830F-F46206FFC0D2}']
  end;

  GCXboxGamepad = interface(GCExtendedGamepad)
    ['{252D1FFE-7E9E-4B90-9F66-E368DF6D449B}']
    function paddleButton1: GCControllerButtonInput; cdecl;
    function paddleButton2: GCControllerButtonInput; cdecl;
    function paddleButton3: GCControllerButtonInput; cdecl;
    function paddleButton4: GCControllerButtonInput; cdecl;
    function buttonShare: GCControllerButtonInput; cdecl;
  end;

  TGCXboxGamepad = class(TOCGenericImport<GCXboxGamepadClass, GCXboxGamepad>)
  end;

  PGCXboxGamepad = Pointer;

  GCDualShockGamepadClass = interface(GCExtendedGamepadClass)
    ['{FEAA58ED-A630-488A-BBED-78FD9F9F20E9}']
  end;

  GCDualShockGamepad = interface(GCExtendedGamepad)
    ['{DE41F6D7-66CE-430D-A81C-0E24955B5251}']
    function touchpadButton: GCControllerButtonInput; cdecl;
    function touchpadPrimary: GCControllerDirectionPad; cdecl;
    function touchpadSecondary: GCControllerDirectionPad; cdecl;
  end;

  TGCDualShockGamepad = class(TOCGenericImport<GCDualShockGamepadClass,
    GCDualShockGamepad>)
  end;

  PGCDualShockGamepad = Pointer;

  GCDualSenseGamepadClass = interface(GCExtendedGamepadClass)
    ['{E8BCF82B-E0C3-461C-B3E6-B48904AC3020}']
  end;

  GCDualSenseGamepad = interface(GCExtendedGamepad)
    ['{BED83831-3AC3-407F-8AEC-506719AEFD79}']
    function touchpadButton: GCControllerButtonInput; cdecl;
    function touchpadPrimary: GCControllerDirectionPad; cdecl;
    function touchpadSecondary: GCControllerDirectionPad; cdecl;
    function leftTrigger: GCDualSenseAdaptiveTrigger; cdecl;
    function rightTrigger: GCDualSenseAdaptiveTrigger; cdecl;
  end;

  TGCDualSenseGamepad = class(TOCGenericImport<GCDualSenseGamepadClass,
    GCDualSenseGamepad>)
  end;

  PGCDualSenseGamepad = Pointer;

  GCMicroGamepadClass = interface(GCPhysicalInputProfileClass)
    ['{C2B61AF5-C52F-4811-8CC6-01F357CEA056}']
  end;

  GCMicroGamepad = interface(GCPhysicalInputProfile)
    ['{95527DD2-D9C7-4551-9812-8039532397BF}']
    function controller: GCController; cdecl;
    procedure setValueChangedHandler(valueChangedHandler
      : GCMicroGamepadValueChangedHandler); cdecl;
    function valueChangedHandler: GCMicroGamepadValueChangedHandler; cdecl;
    function saveSnapshot: GCMicroGamepadSnapshot; cdecl;
    function dpad: GCControllerDirectionPad; cdecl;
    function buttonA: GCControllerButtonInput; cdecl;
    function buttonX: GCControllerButtonInput; cdecl;
    function buttonMenu: GCControllerButtonInput; cdecl;
    procedure setReportsAbsoluteDpadValues(reportsAbsoluteDpadValues
      : Boolean); cdecl;
    function reportsAbsoluteDpadValues: Boolean; cdecl;
    procedure setAllowsRotation(allowsRotation: Boolean); cdecl;
    function allowsRotation: Boolean; cdecl;
    procedure setStateFromMicroGamepad(microGamepad: GCMicroGamepad); cdecl;
  end;

  TGCMicroGamepad = class(TOCGenericImport<GCMicroGamepadClass, GCMicroGamepad>)
  end;

  PGCMicroGamepad = Pointer;

  GCMicroGamepadSnapshotClass = interface(GCMicroGamepadClass)
    ['{E9FCF954-8DC2-4103-A999-C43E153B5A98}']
  end;

  GCMicroGamepadSnapshot = interface(GCMicroGamepad)
    ['{6C3EF8E5-650D-48C1-AA6E-F63D4DD1B6F3}']
    procedure setSnapshotData(snapshotData: NSData); cdecl;
    function snapshotData: NSData; cdecl;
    function initWithSnapshotData(data: NSData)
      : Pointer { instancetype }; cdecl;
    function initWithController(controller: GCController; snapshotData: NSData)
      : Pointer { instancetype }; cdecl;
  end;

  TGCMicroGamepadSnapshot = class(TOCGenericImport<GCMicroGamepadSnapshotClass,
    GCMicroGamepadSnapshot>)
  end;

  PGCMicroGamepadSnapshot = Pointer;

  GCDirectionalGamepadClass = interface(GCMicroGamepadClass)
    ['{C6F57983-6DAB-4B9F-AC90-740F5BF4E084}']
  end;

  GCDirectionalGamepad = interface(GCMicroGamepad)
    ['{BDB8F0EC-8871-4C40-B69D-CDDFC8FC8ADB}']
  end;

  TGCDirectionalGamepad = class(TOCGenericImport<GCDirectionalGamepadClass,
    GCDirectionalGamepad>)
  end;

  PGCDirectionalGamepad = Pointer;

  GCDeviceHapticsClass = interface(NSObjectClass)
    ['{C4DAD54F-CF21-4DF7-B057-5ADA39579EC8}']
  end;

  GCDeviceHaptics = interface(NSObject)
    ['{41C74B23-4FB9-4C26-BF9E-48B47A7C89ED}']
    function supportedLocalities: NSSet; cdecl;
    function createEngineWithLocality(locality: GCHapticsLocality)
      : CHHapticEngine; cdecl;
  end;

  TGCDeviceHaptics = class(TOCGenericImport<GCDeviceHapticsClass,
    GCDeviceHaptics>)
  end;

  PGCDeviceHaptics = Pointer;

  GCKeyboardClass = interface(NSObjectClass)
    ['{C418BB84-723F-435E-8880-5792BD8EE947}']
  end;

  GCKeyboard = interface(NSObject)
    ['{068E29D2-32EC-470D-B2D7-CA84389D00C9}']
    function keyboardInput: GCKeyboardInput; cdecl;
    procedure setCoalescedKeyboard(coalescedKeyboard: GCKeyboard); cdecl;
    function coalescedKeyboard: GCKeyboard; cdecl;
  end;

  TGCKeyboard = class(TOCGenericImport<GCKeyboardClass, GCKeyboard>)
  end;

  PGCKeyboard = Pointer;

  GCMouseClass = interface(NSObjectClass)
    ['{7A8A93B7-BF6F-4A71-A05D-875908F550A8}']
    { class } function mice: NSArray; cdecl;
  end;

  GCMouse = interface(NSObject)
    ['{D4B37429-6BBF-4CA8-81BE-0E09F60BF5F5}']
    function mouseInput: GCMouseInput; cdecl;
    procedure setCurrent(current: GCMouse); cdecl;
    function current: GCMouse; cdecl;
  end;

  TGCMouse = class(TOCGenericImport<GCMouseClass, GCMouse>)
  end;

  PGCMouse = Pointer;

  GCEventViewControllerClass = interface(NSViewControllerClass)
    ['{DC6F76F5-A7A6-43DB-B0CB-CFCA2FD82E6F}']
  end;

  GCEventViewController = interface(NSViewController)
    ['{DEBBB729-1221-4577-9754-2163CD97E276}']
    procedure setControllerUserInteractionEnabled
      (controllerUserInteractionEnabled: Boolean); cdecl;
    function controllerUserInteractionEnabled: Boolean; cdecl;
  end;

  TGCEventViewController = class(TOCGenericImport<GCEventViewControllerClass,
    GCEventViewController>)
  end;

  PGCEventViewController = Pointer;

  GCRacingWheelInputStateClass = interface(NSObjectClass)
    ['{31DC8B3F-1CCC-4006-B3A6-A9E739F56398}']
  end;

  GCRacingWheelInputState = interface(NSObject)
    ['{E9290D52-98EA-4493-9F24-E0A96ADC4CE9}']
    function wheel: GCSteeringWheelElement; cdecl;
    function acceleratorPedal: Pointer; cdecl;
    function brakePedal: Pointer; cdecl;
    function clutchPedal: Pointer; cdecl;
    function shifter: GCGearShifterElement; cdecl;
  end;

  TGCRacingWheelInputState = class
    (TOCGenericImport<GCRacingWheelInputStateClass, GCRacingWheelInputState>)
  end;

  PGCRacingWheelInputState = Pointer;

  GCRacingWheelInputClass = interface(GCRacingWheelInputStateClass)
    ['{81DD2086-57B9-4B55-AF29-D0A928498727}']
  end;

  GCRacingWheelInput = interface(GCRacingWheelInputState)
    ['{3A1AD0E2-DAE5-4536-8EE4-94C9BE224405}']
    function capture: GCRacingWheelInputState; cdecl;
    function nextInputState: GCRacingWheelInputState; cdecl;
  end;

  TGCRacingWheelInput = class(TOCGenericImport<GCRacingWheelInputClass,
    GCRacingWheelInput>)
  end;

  PGCRacingWheelInput = Pointer;

  GCRacingWheelClass = interface(NSObjectClass)
    ['{F311FE19-8877-43A5-9230-F1FC48575487}']
  end;

  GCRacingWheel = interface(NSObject)
    ['{F90B1418-B2B3-4D34-9E77-0F5B270648A1}']
    function connectedRacingWheels: NSSet; cdecl;
    function acquireDeviceWithError(error: NSError): Boolean; cdecl;
    procedure relinquishDevice; cdecl;
    function isAcquired: Boolean; cdecl;
    function wheelInput: GCRacingWheelInput; cdecl;
    function isSnapshot: Boolean; cdecl;
    function capture: GCRacingWheel; cdecl;
  end;

  TGCRacingWheel = class(TOCGenericImport<GCRacingWheelClass, GCRacingWheel>)
  end;

  PGCRacingWheel = Pointer;

  GCSteeringWheelElementClass = interface(NSObjectClass)
    ['{DD78A640-438A-49CC-A750-0C8620651AAB}']
  end;

  GCSteeringWheelElement = interface(NSObject)
    ['{72F87068-1E0E-441D-A382-91F3B607626F}']
    function maximumDegreesOfRotation: Single; cdecl;
  end;

  TGCSteeringWheelElement = class(TOCGenericImport<GCSteeringWheelElementClass,
    GCSteeringWheelElement>)
  end;

  PGCSteeringWheelElement = Pointer;

  GCGearShifterElementClass = interface(NSObjectClass)
    ['{BB654328-4E96-4E21-AA6B-35A5B9DA1F42}']
  end;

  GCGearShifterElement = interface(NSObject)
    ['{79EA387B-6577-4EF9-AF15-95BEBF945873}']
    function patternInput: Pointer; cdecl;
    function sequentialInput: Pointer; cdecl;
  end;

  TGCGearShifterElement = class(TOCGenericImport<GCGearShifterElementClass,
    GCGearShifterElement>)
  end;

  PGCGearShifterElement = Pointer;

  // ===== Protocol declarations =====

  GCDevice = interface(IObjectiveC)
    ['{4A7CDE07-A0D6-4A5E-8FB9-034C036BAE4E}']
    procedure setHandlerQueue(handlerQueue: dispatch_queue_t); cdecl;
    function handlerQueue: dispatch_queue_t; cdecl;
    function vendorName: NSString; cdecl;
    function productCategory: NSString; cdecl;
    function physicalInputProfile: GCPhysicalInputProfile; cdecl;
  end;

  GCPhysicalInputElementName = interface(IObjectiveC)
    ['{3DE0C869-DF77-45ED-ADA5-4D1BFBAD6DD5}']
  end;

  GCButtonElementName = interface(IObjectiveC)
    ['{02D4757D-2051-4D5E-8DC0-C706553A1090}']
  end;

  GCAxisElementName = interface(IObjectiveC)
    ['{1B3720B1-068F-49CA-89FD-636BF7C7AA4E}']
  end;

  GCSwitchElementName = interface(IObjectiveC)
    ['{F74AAA29-6E15-4E75-9113-ACA11DAAC3E9}']
  end;

  GCDirectionPadElementName = interface(IObjectiveC)
    ['{A640D84C-1141-45A9-9078-1F2262DBF325}']
  end;

  GCPhysicalInputElement = interface(IObjectiveC)
    ['{52FAB589-B1C2-4964-8510-3CC3ABF6AB87}']
    function sfSymbolsName: NSString; cdecl;
    function localizedName: NSString; cdecl;
    function aliases: NSSet; cdecl;
  end;

  GCLinearInput = interface(IObjectiveC)
    ['{F0335E84-F142-44E1-A60F-E5C3E3173444}']
    procedure setValueDidChangeHandler(valueDidChangeHandler
      : TGameControllerValueDidChangeHandler); cdecl;
    function valueDidChangeHandler: TGameControllerValueDidChangeHandler; cdecl;
    function value: Single; cdecl;
    function isAnalog: Boolean; cdecl;
    function canWrap: Boolean; cdecl;
    function lastValueTimestamp: NSTimeInterval; cdecl;
    function lastValueLatency: NSTimeInterval; cdecl;
  end;

  GCPressedStateInput = interface(IObjectiveC)
    ['{DABD6025-5D84-4062-AD5C-BBA5C721BB14}']
    procedure setPressedDidChangeHandler(pressedDidChangeHandler
      : TGameControllerPressedDidChangeHandler); cdecl;
    function pressedDidChangeHandler
      : TGameControllerPressedDidChangeHandler; cdecl;
    function isPressed: Boolean; cdecl;
    function lastPressedStateTimestamp: NSTimeInterval; cdecl;
    function lastPressedStateLatency: NSTimeInterval; cdecl;
  end;

  GCTouchedStateInput = interface(IObjectiveC)
    ['{2F28FFCD-5F6B-40E6-9F87-77D95860CEAE}']
    procedure setTouchedDidChangeHandler(touchedDidChangeHandler
      : TGameControllerPressedDidChangeHandler); cdecl;
    function touchedDidChangeHandler
      : TGameControllerPressedDidChangeHandler; cdecl;
    function isTouched: Boolean; cdecl;
    function lastTouchedStateTimestamp: NSTimeInterval; cdecl;
    function lastTouchedStateLatency: NSTimeInterval; cdecl;
  end;

  GCButtonElement = interface(IObjectiveC)
    ['{B1CC18F6-0517-4D67-8B76-5FB3E1CA2134}']
    function pressedInput: Pointer; cdecl;
    function touchedInput: Pointer; cdecl;
  end;

  GCAxisInput = interface(IObjectiveC)
    ['{C2CE88B6-A8DE-4A45-9E57-CF1A50696C6A}']
    procedure setValueDidChangeHandler(valueDidChangeHandler
      : TGameControllerValueDidChangeHandler); cdecl;
    function valueDidChangeHandler: TGameControllerValueDidChangeHandler; cdecl;
    function value: Single; cdecl;
    function isAnalog: Boolean; cdecl;
    function canWrap: Boolean; cdecl;
    function lastValueTimestamp: NSTimeInterval; cdecl;
    function lastValueLatency: NSTimeInterval; cdecl;
  end;

  GCRelativeInput = interface(IObjectiveC)
    ['{9A1C8172-1AA7-4D12-AED0-833F39884E96}']
    procedure setDeltaDidChangeHandler(deltaDidChangeHandler
      : TGameControllerValueDidChangeHandler); cdecl;
    function deltaDidChangeHandler: TGameControllerValueDidChangeHandler; cdecl;
    function delta: Single; cdecl;
    function isAnalog: Boolean; cdecl;
    function lastDeltaTimestamp: NSTimeInterval; cdecl;
    function lastDeltaLatency: NSTimeInterval; cdecl;
  end;

  GCAxisElement = interface(IObjectiveC)
    ['{61934D66-FE53-411A-AF51-0F81F55D4EE5}']
    function absoluteInput: Pointer; cdecl;
    function relativeInput: Pointer; cdecl;
  end;

  GCSwitchPositionInput = interface(IObjectiveC)
    ['{2831B69B-2FF0-44CF-AC31-66B654862328}']
    procedure setPositionDidChangeHandler(positionDidChangeHandler
      : TGameControllerPositionDidChangeHandler); cdecl;
    function positionDidChangeHandler
      : TGameControllerPositionDidChangeHandler; cdecl;
    function position: NSInteger; cdecl;
    function positionRange: NSRange; cdecl;
    function isSequential: Boolean; cdecl;
    function canWrap: Boolean; cdecl;
    function lastPositionTimestamp: NSTimeInterval; cdecl;
    function lastPositionLatency: NSTimeInterval; cdecl;
  end;

  GCSwitchElement = interface(IObjectiveC)
    ['{1D88BD2C-6DE2-4D51-8280-D35B28944E18}']
    function positionInput: Pointer; cdecl;
  end;

  GCDirectionPadElement = interface(IObjectiveC)
    ['{96B9B4D3-C6A1-4498-9DDB-EAB0A8B1B160}']
    function xAxis: Pointer; cdecl;
    function yAxis: Pointer; cdecl;
    function up: Pointer; cdecl;
    function down: Pointer; cdecl;
    function left: Pointer; cdecl;
    function right: Pointer; cdecl;
  end;

  GCDevicePhysicalInputState = interface(IObjectiveC)
    ['{D6F2C576-052C-4415-B91D-0DE0D524757A}']
    function device: Pointer; cdecl;
    function lastEventTimestamp: NSTimeInterval; cdecl;
    function lastEventLatency: NSTimeInterval; cdecl;
    function elements: GCPhysicalInputElementCollection; cdecl;
    function buttons: GCPhysicalInputElementCollection; cdecl;
    function axes: GCPhysicalInputElementCollection; cdecl;
    function switches: GCPhysicalInputElementCollection; cdecl;
    function dpads: GCPhysicalInputElementCollection; cdecl;
    function objectForKeyedSubscript(Key: NSString): Pointer; cdecl;
  end;

  GCDevicePhysicalInputStateDiff = interface(IObjectiveC)
    ['{B99DEFE5-EA4D-48A4-A5A8-F450BC8023AF}']
    function changeForElement(Element: Pointer)
      : GCDevicePhysicalInputElementChange; cdecl;
    function changedElements: NSEnumerator; cdecl;
  end;

  GCDevicePhysicalInput = interface(IObjectiveC)
    ['{E8D7336B-2617-4BBA-8FD4-2CDC2B3E85B7}']
    function device: Pointer; cdecl;
    procedure setElementValueDidChangeHandler(elementValueDidChangeHandler
      : TGameControllerElementValueDidChangeHandler); cdecl;
    function elementValueDidChangeHandler
      : TGameControllerElementValueDidChangeHandler; cdecl;
    function capture: Pointer; cdecl;
    procedure setInputStateAvailableHandler(inputStateAvailableHandler
      : TGameControllerInputStateAvailableHandler); cdecl;
    function inputStateAvailableHandler
      : TGameControllerInputStateAvailableHandler; cdecl;
    procedure setInputStateQueueDepth(inputStateQueueDepth: NSInteger); cdecl;
    function inputStateQueueDepth: NSInteger; cdecl;
    function nextInputState: Pointer; cdecl;
  end;

  GCButtonInput = interface(IObjectiveC)
    ['{8D7DA618-B3D8-4CC8-AF18-88858B3FA981}']
  end;

  GCSwitchInput = interface(IObjectiveC)
    ['{91660833-C9A3-4B25-B726-0891CF4497AE}']
  end;

  // ===== Exported string consts =====

function GCInputButtonA: Pointer;
function GCInputButtonB: Pointer;
function GCInputButtonX: Pointer;
function GCInputButtonY: Pointer;
function GCInputDirectionPad: Pointer;
function GCInputLeftThumbstick: Pointer;
function GCInputRightThumbstick: Pointer;
function GCInputLeftShoulder: Pointer;
function GCInputRightShoulder: Pointer;
function GCInputLeftTrigger: Pointer;
function GCInputRightTrigger: Pointer;
function GCInputLeftThumbstickButton: Pointer;
function GCInputRightThumbstickButton: Pointer;
function GCInputButtonHome: Pointer;
function GCInputButtonMenu: Pointer;
function GCInputButtonOptions: Pointer;
function GCInputButtonShare: Pointer;
function GCInputXboxPaddleOne: Pointer;
function GCInputXboxPaddleTwo: Pointer;
function GCInputXboxPaddleThree: Pointer;
function GCInputXboxPaddleFour: Pointer;
function GCInputDualShockTouchpadOne: Pointer;
function GCInputDualShockTouchpadTwo: Pointer;
function GCInputDualShockTouchpadButton: Pointer;
function GCInputSteeringWheel: Pointer;
function GCInputShifter: Pointer;
function GCInputPedalAccelerator: Pointer;
function GCInputPedalBrake: Pointer;
function GCInputPedalClutch: Pointer;
function GCInputLeftPaddle: Pointer;
function GCInputRightPaddle: Pointer;
function GCCurrentExtendedGamepadSnapshotDataVersion: Pointer;
function GCKeyCodeKeyA: Pointer;
function GCKeyCodeKeyB: Pointer;
function GCKeyCodeKeyC: Pointer;
function GCKeyCodeKeyD: Pointer;
function GCKeyCodeKeyE: Pointer;
function GCKeyCodeKeyF: Pointer;
function GCKeyCodeKeyG: Pointer;
function GCKeyCodeKeyH: Pointer;
function GCKeyCodeKeyI: Pointer;
function GCKeyCodeKeyJ: Pointer;
function GCKeyCodeKeyK: Pointer;
function GCKeyCodeKeyL: Pointer;
function GCKeyCodeKeyM: Pointer;
function GCKeyCodeKeyN: Pointer;
function GCKeyCodeKeyO: Pointer;
function GCKeyCodeKeyP: Pointer;
function GCKeyCodeKeyQ: Pointer;
function GCKeyCodeKeyR: Pointer;
function GCKeyCodeKeyS: Pointer;
function GCKeyCodeKeyT: Pointer;
function GCKeyCodeKeyU: Pointer;
function GCKeyCodeKeyV: Pointer;
function GCKeyCodeKeyW: Pointer;
function GCKeyCodeKeyX: Pointer;
function GCKeyCodeKeyY: Pointer;
function GCKeyCodeKeyZ: Pointer;
function GCKeyCodeOne: Pointer;
function GCKeyCodeTwo: Pointer;
function GCKeyCodeThree: Pointer;
function GCKeyCodeFour: Pointer;
function GCKeyCodeFive: Pointer;
function GCKeyCodeSix: Pointer;
function GCKeyCodeSeven: Pointer;
function GCKeyCodeEight: Pointer;
function GCKeyCodeNine: Pointer;
function GCKeyCodeZero: Pointer;
function GCKeyCodeReturnOrEnter: Pointer;
function GCKeyCodeEscape: Pointer;
function GCKeyCodeDeleteOrBackspace: Pointer;
function GCKeyCodeTab: Pointer;
function GCKeyCodeSpacebar: Pointer;
function GCKeyCodeHyphen: Pointer;
function GCKeyCodeEqualSign: Pointer;
function GCKeyCodeOpenBracket: Pointer;
function GCKeyCodeCloseBracket: Pointer;
function GCKeyCodeBackslash: Pointer;
function GCKeyCodeNonUSPound: Pointer;
function GCKeyCodeSemicolon: Pointer;
function GCKeyCodeQuote: Pointer;
function GCKeyCodeGraveAccentAndTilde: Pointer;
function GCKeyCodeComma: Pointer;
function GCKeyCodePeriod: Pointer;
function GCKeyCodeSlash: Pointer;
function GCKeyCodeCapsLock: Pointer;
function GCKeyCodeF1: Pointer;
function GCKeyCodeF2: Pointer;
function GCKeyCodeF3: Pointer;
function GCKeyCodeF4: Pointer;
function GCKeyCodeF5: Pointer;
function GCKeyCodeF6: Pointer;
function GCKeyCodeF7: Pointer;
function GCKeyCodeF8: Pointer;
function GCKeyCodeF9: Pointer;
function GCKeyCodeF10: Pointer;
function GCKeyCodeF11: Pointer;
function GCKeyCodeF12: Pointer;
function GCKeyCodeF13: Pointer;
function GCKeyCodeF14: Pointer;
function GCKeyCodeF15: Pointer;
function GCKeyCodeF16: Pointer;
function GCKeyCodeF17: Pointer;
function GCKeyCodeF18: Pointer;
function GCKeyCodeF19: Pointer;
function GCKeyCodeF20: Pointer;
function GCKeyCodePrintScreen: Pointer;
function GCKeyCodeScrollLock: Pointer;
function GCKeyCodePause: Pointer;
function GCKeyCodeInsert: Pointer;
function GCKeyCodeHome: Pointer;
function GCKeyCodePageUp: Pointer;
function GCKeyCodeDeleteForward: Pointer;
function GCKeyCodeEnd: Pointer;
function GCKeyCodePageDown: Pointer;
function GCKeyCodeRightArrow: Pointer;
function GCKeyCodeLeftArrow: Pointer;
function GCKeyCodeDownArrow: Pointer;
function GCKeyCodeUpArrow: Pointer;
function GCKeyCodeKeypadNumLock: Pointer;
function GCKeyCodeKeypadSlash: Pointer;
function GCKeyCodeKeypadAsterisk: Pointer;
function GCKeyCodeKeypadHyphen: Pointer;
function GCKeyCodeKeypadPlus: Pointer;
function GCKeyCodeKeypadEnter: Pointer;
function GCKeyCodeKeypad1: Pointer;
function GCKeyCodeKeypad2: Pointer;
function GCKeyCodeKeypad3: Pointer;
function GCKeyCodeKeypad4: Pointer;
function GCKeyCodeKeypad5: Pointer;
function GCKeyCodeKeypad6: Pointer;
function GCKeyCodeKeypad7: Pointer;
function GCKeyCodeKeypad8: Pointer;
function GCKeyCodeKeypad9: Pointer;
function GCKeyCodeKeypad0: Pointer;
function GCKeyCodeKeypadPeriod: Pointer;
function GCKeyCodeKeypadEqualSign: Pointer;
function GCKeyCodeNonUSBackslash: Pointer;
function GCKeyCodeApplication: Pointer;
function GCKeyCodePower: Pointer;
function GCKeyCodeInternational1: Pointer;
function GCKeyCodeInternational2: Pointer;
function GCKeyCodeInternational3: Pointer;
function GCKeyCodeInternational4: Pointer;
function GCKeyCodeInternational5: Pointer;
function GCKeyCodeInternational6: Pointer;
function GCKeyCodeInternational7: Pointer;
function GCKeyCodeInternational8: Pointer;
function GCKeyCodeInternational9: Pointer;
function GCKeyCodeLANG1: Pointer;
function GCKeyCodeLANG2: Pointer;
function GCKeyCodeLANG3: Pointer;
function GCKeyCodeLANG4: Pointer;
function GCKeyCodeLANG5: Pointer;
function GCKeyCodeLANG6: Pointer;
function GCKeyCodeLANG7: Pointer;
function GCKeyCodeLANG8: Pointer;
function GCKeyCodeLANG9: Pointer;
function GCKeyCodeLeftControl: Pointer;
function GCKeyCodeLeftShift: Pointer;
function GCKeyCodeLeftAlt: Pointer;
function GCKeyCodeLeftGUI: Pointer;
function GCKeyCodeRightControl: Pointer;
function GCKeyCodeRightShift: Pointer;
function GCKeyCodeRightAlt: Pointer;
function GCKeyCodeRightGUI: Pointer;
function GCKeyA: NSString;
function GCKeyB: NSString;
function GCKeyC: NSString;
function GCKeyD: NSString;
function GCKeyE: NSString;
function GCKeyF: NSString;
function GCKeyG: NSString;
function GCKeyH: NSString;
function GCKeyI: NSString;
function GCKeyJ: NSString;
function GCKeyK: NSString;
function GCKeyL: NSString;
function GCKeyM: NSString;
function GCKeyN: NSString;
function GCKeyO: NSString;
function GCKeyP: NSString;
function GCKeyQ: NSString;
function GCKeyR: NSString;
function GCKeyS: NSString;
function GCKeyT: NSString;
function GCKeyU: NSString;
function GCKeyV: NSString;
function GCKeyW: NSString;
function GCKeyX: NSString;
function GCKeyY: NSString;
function GCKeyZ: NSString;
function GCKeyOne: NSString;
function GCKeyTwo: NSString;
function GCKeyThree: NSString;
function GCKeyFour: NSString;
function GCKeyFive: NSString;
function GCKeySix: NSString;
function GCKeySeven: NSString;
function GCKeyEight: NSString;
function GCKeyNine: NSString;
function GCKeyZero: NSString;
function GCKeyReturnOrEnter: NSString;
function GCKeyEscape: NSString;
function GCKeyDeleteOrBackspace: NSString;
function GCKeyTab: NSString;
function GCKeySpacebar: NSString;
function GCKeyHyphen: NSString;
function GCKeyEqualSign: NSString;
function GCKeyOpenBracket: NSString;
function GCKeyCloseBracket: NSString;
function GCKeyBackslash: NSString;
function GCKeyNonUSPound: NSString;
function GCKeySemicolon: NSString;
function GCKeyQuote: NSString;
function GCKeyGraveAccentAndTilde: NSString;
function GCKeyComma: NSString;
function GCKeyPeriod: NSString;
function GCKeySlash: NSString;
function GCKeyCapsLock: NSString;
function GCKeyF1: NSString;
function GCKeyF2: NSString;
function GCKeyF3: NSString;
function GCKeyF4: NSString;
function GCKeyF5: NSString;
function GCKeyF6: NSString;
function GCKeyF7: NSString;
function GCKeyF8: NSString;
function GCKeyF9: NSString;
function GCKeyF10: NSString;
function GCKeyF11: NSString;
function GCKeyF12: NSString;
function GCKeyF13: NSString;
function GCKeyF14: NSString;
function GCKeyF15: NSString;
function GCKeyF16: NSString;
function GCKeyF17: NSString;
function GCKeyF18: NSString;
function GCKeyF19: NSString;
function GCKeyF20: NSString;
function GCKeyPrintScreen: NSString;
function GCKeyScrollLock: NSString;
function GCKeyPause: NSString;
function GCKeyInsert: NSString;
function GCKeyHome: NSString;
function GCKeyPageUp: NSString;
function GCKeyDeleteForward: NSString;
function GCKeyEnd: NSString;
function GCKeyPageDown: NSString;
function GCKeyRightArrow: NSString;
function GCKeyLeftArrow: NSString;
function GCKeyDownArrow: NSString;
function GCKeyUpArrow: NSString;
function GCKeyKeypadNumLock: NSString;
function GCKeyKeypadSlash: NSString;
function GCKeyKeypadAsterisk: NSString;
function GCKeyKeypadHyphen: NSString;
function GCKeyKeypadPlus: NSString;
function GCKeyKeypadEnter: NSString;
function GCKeyKeypad1: NSString;
function GCKeyKeypad2: NSString;
function GCKeyKeypad3: NSString;
function GCKeyKeypad4: NSString;
function GCKeyKeypad5: NSString;
function GCKeyKeypad6: NSString;
function GCKeyKeypad7: NSString;
function GCKeyKeypad8: NSString;
function GCKeyKeypad9: NSString;
function GCKeyKeypad0: NSString;
function GCKeyKeypadPeriod: NSString;
function GCKeyKeypadEqualSign: NSString;
function GCKeyNonUSBackslash: NSString;
function GCKeyApplication: NSString;
function GCKeyPower: NSString;
function GCKeyInternational1: NSString;
function GCKeyInternational2: NSString;
function GCKeyInternational3: NSString;
function GCKeyInternational4: NSString;
function GCKeyInternational5: NSString;
function GCKeyInternational6: NSString;
function GCKeyInternational7: NSString;
function GCKeyInternational8: NSString;
function GCKeyInternational9: NSString;
function GCKeyLANG1: NSString;
function GCKeyLANG2: NSString;
function GCKeyLANG3: NSString;
function GCKeyLANG4: NSString;
function GCKeyLANG5: NSString;
function GCKeyLANG6: NSString;
function GCKeyLANG7: NSString;
function GCKeyLANG8: NSString;
function GCKeyLANG9: NSString;
function GCKeyLeftControl: NSString;
function GCKeyLeftShift: NSString;
function GCKeyLeftAlt: NSString;
function GCKeyLeftGUI: NSString;
function GCKeyRightControl: NSString;
function GCKeyRightShift: NSString;
function GCKeyRightAlt: NSString;
function GCKeyRightGUI: NSString;
function GCInputMicroGamepadDpad: NSString;
function GCInputMicroGamepadButtonA: NSString;
function GCInputMicroGamepadButtonX: NSString;
function GCInputMicroGamepadButtonMenu: NSString;
function GCCurrentMicroGamepadSnapshotDataVersion: Pointer;
function GCInputDirectionalDpad: NSString;
function GCInputDirectionalTouchSurfaceButton: NSString;
function GCInputDirectionalCardinalDpad: NSString;
function GCInputDirectionalCenterButton: NSString;
function GCProductCategoryDualSense: NSString;
function GCProductCategoryDualShock4: NSString;
function GCProductCategoryMFi: NSString;
function GCProductCategoryXboxOne: NSString;
function GCProductCategoryHID: NSString;
function GCProductCategorySiriRemote1stGen: NSString;
function GCProductCategorySiriRemote2ndGen: NSString;
function GCProductCategoryControlCenterRemote: NSString;
function GCProductCategoryUniversalElectronicsRemote: NSString;
function GCProductCategoryCoalescedRemote: NSString;
function GCProductCategoryMouse: NSString;
function GCProductCategoryKeyboard: NSString;
function GCControllerDidConnectNotification: NSString;
function GCControllerDidDisconnectNotification: NSString;
function GCControllerDidBecomeCurrentNotification: NSString;
function GCControllerDidStopBeingCurrentNotification: NSString;
function GCControllerUserCustomizationsDidChangeNotification: NSString;
function GCKeyboardDidConnectNotification: NSString;
function GCKeyboardDidDisconnectNotification: NSString;
function GCMouseDidConnectNotification: NSString;
function GCMouseDidDisconnectNotification: NSString;
function GCMouseDidBecomeCurrentNotification: NSString;
function GCMouseDidStopBeingCurrentNotification: NSString;
function GCRacingWheelDidConnectNotification: NSString;
function GCRacingWheelDidDisconnectNotification: NSString;
function GCHapticsLocalityDefault: Pointer;
function GCHapticsLocalityAll: Pointer;
function GCHapticsLocalityHandles: Pointer;
function GCHapticsLocalityLeftHandle: Pointer;
function GCHapticsLocalityRightHandle: Pointer;
function GCHapticsLocalityTriggers: Pointer;
function GCHapticsLocalityLeftTrigger: Pointer;
function GCHapticsLocalityRightTrigger: Pointer;
function GCHapticDurationInfinite: Pointer;


// ===== External functions =====

const
  libGameController =
    '/System/Library/Frameworks/GameController.framework/GameController';
function GCGamepadSnapShotDataV100FromNSData(snapshotData
  : PGCGamepadSnapShotDataV100; data: Pointer { NSData } ): Boolean; cdecl;
  external libGameController name _PU + 'GCGamepadSnapShotDataV100FromNSData';
function NSDataFromGCGamepadSnapShotDataV100(snapshotData
  : PGCGamepadSnapShotDataV100): Pointer { NSData }; cdecl;
  external libGameController name _PU + 'NSDataFromGCGamepadSnapShotDataV100';
function GCExtendedGamepadSnapshotDataFromNSData(snapshotData
  : PGCExtendedGamepadSnapshotData; data: Pointer { NSData } ): Boolean; cdecl;
  external libGameController name _PU +
  'GCExtendedGamepadSnapshotDataFromNSData';
function NSDataFromGCExtendedGamepadSnapshotData(snapshotData
  : PGCExtendedGamepadSnapshotData): Pointer { NSData }; cdecl;
  external libGameController name _PU +
  'NSDataFromGCExtendedGamepadSnapshotData';
function GCExtendedGamepadSnapShotDataV100FromNSData(snapshotData
  : PGCExtendedGamepadSnapShotDataV100; data: Pointer { NSData } ): Boolean;
  cdecl; external libGameController name _PU +
  'GCExtendedGamepadSnapShotDataV100FromNSData';
function NSDataFromGCExtendedGamepadSnapShotDataV100(snapshotData
  : PGCExtendedGamepadSnapShotDataV100): Pointer { NSData }; cdecl;
  external libGameController name _PU +
  'NSDataFromGCExtendedGamepadSnapShotDataV100';
function GCMicroGamepadSnapshotDataFromNSData(snapshotData
  : PGCMicroGamepadSnapshotData; data: Pointer { NSData } ): Boolean; cdecl;
  external libGameController name _PU + 'GCMicroGamepadSnapshotDataFromNSData';
function NSDataFromGCMicroGamepadSnapshotData(snapshotData
  : PGCMicroGamepadSnapshotData): Pointer { NSData }; cdecl;
  external libGameController name _PU + 'NSDataFromGCMicroGamepadSnapshotData';
function GCMicroGamepadSnapShotDataV100FromNSData(snapshotData
  : PGCMicroGamepadSnapShotDataV100; data: Pointer { NSData } ): Boolean; cdecl;
  external libGameController name _PU +
  'GCMicroGamepadSnapShotDataV100FromNSData';
function NSDataFromGCMicroGamepadSnapShotDataV100(snapshotData
  : PGCMicroGamepadSnapShotDataV100): Pointer { NSData }; cdecl;
  external libGameController name _PU +
  'NSDataFromGCMicroGamepadSnapShotDataV100';

implementation

function GCKeyA: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyA');
end;

function GCKeyB: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyB');
end;

function GCKeyC: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyC');
end;

function GCKeyD: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyD');
end;

function GCKeyE: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyE');
end;

function GCKeyF: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF');
end;

function GCKeyG: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyG');
end;

function GCKeyH: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyH');
end;

function GCKeyI: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyI');
end;

function GCKeyJ: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyJ');
end;

function GCKeyK: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyK');
end;

function GCKeyL: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyL');
end;

function GCKeyM: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyM');
end;

function GCKeyN: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyN');
end;

function GCKeyO: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyO');
end;

function GCKeyP: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyP');
end;

function GCKeyQ: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyQ');
end;

function GCKeyR: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyR');
end;

function GCKeyS: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyS');
end;

function GCKeyT: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyT');
end;

function GCKeyU: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyU');
end;

function GCKeyV: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyV');
end;

function GCKeyW: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyW');
end;

function GCKeyX: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyX');
end;

function GCKeyY: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyY');
end;

function GCKeyZ: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyZ');
end;

function GCKeyOne: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyOne');
end;

function GCKeyTwo: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyTwo');
end;

function GCKeyThree: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyThree');
end;

function GCKeyFour: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyFour');
end;

function GCKeyFive: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyFive');
end;

function GCKeySix: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeySix');
end;

function GCKeySeven: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeySeven');
end;

function GCKeyEight: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyEight');
end;

function GCKeyNine: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyNine');
end;

function GCKeyZero: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyZero');
end;

function GCKeyReturnOrEnter: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyReturnOrEnter');
end;

function GCKeyEscape: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyEscape');
end;

function GCKeyDeleteOrBackspace: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyDeleteOrBackspace');
end;

function GCKeyTab: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyTab');
end;

function GCKeySpacebar: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeySpacebar');
end;

function GCKeyHyphen: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyHyphen');
end;

function GCKeyEqualSign: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyEqualSign');
end;

function GCKeyOpenBracket: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyOpenBracket');
end;

function GCKeyCloseBracket: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyCloseBracket');
end;

function GCKeyBackslash: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyBackslash');
end;

function GCKeyNonUSPound: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyNonUSPound');
end;

function GCKeySemicolon: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeySemicolon');
end;

function GCKeyQuote: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyQuote');
end;

function GCKeyGraveAccentAndTilde: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyGraveAccentAndTilde');
end;

function GCKeyComma: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyComma');
end;

function GCKeyPeriod: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyPeriod');
end;

function GCKeySlash: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeySlash');
end;

function GCKeyCapsLock: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyCapsLock');
end;

function GCKeyF1: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF1');
end;

function GCKeyF2: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF2');
end;

function GCKeyF3: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF3');
end;

function GCKeyF4: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF4');
end;

function GCKeyF5: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF5');
end;

function GCKeyF6: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF6');
end;

function GCKeyF7: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF7');
end;

function GCKeyF8: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF8');
end;

function GCKeyF9: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF9');
end;

function GCKeyF10: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF10');
end;

function GCKeyF11: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF11');
end;

function GCKeyF12: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF12');
end;

function GCKeyF13: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF13');
end;

function GCKeyF14: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF14');
end;

function GCKeyF15: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF15');
end;

function GCKeyF16: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF16');
end;

function GCKeyF17: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF17');
end;

function GCKeyF18: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF18');
end;

function GCKeyF19: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF19');
end;

function GCKeyF20: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF20');
end;

function GCKeyPrintScreen: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyPrintScreen');
end;

function GCKeyScrollLock: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyScrollLock');
end;

function GCKeyPause: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyPause');
end;

function GCKeyInsert: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInsert');
end;

function GCKeyHome: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyHome');
end;

function GCKeyPageUp: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyPageUp');
end;

function GCKeyDeleteForward: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyDeleteForward');
end;

function GCKeyEnd: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyEnd');
end;

function GCKeyPageDown: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyPageDown');
end;

function GCKeyRightArrow: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyRightArrow');
end;

function GCKeyLeftArrow: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLeftArrow');
end;

function GCKeyDownArrow: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyDownArrow');
end;

function GCKeyUpArrow: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyUpArrow');
end;

function GCKeyKeypadNumLock: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadNumLock');
end;

function GCKeyKeypadSlash: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadSlash');
end;

function GCKeyKeypadAsterisk: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadAsterisk');
end;

function GCKeyKeypadHyphen: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadHyphen');
end;

function GCKeyKeypadPlus: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadPlus');
end;

function GCKeyKeypadEnter: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadEnter');
end;

function GCKeyKeypad1: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad1');
end;

function GCKeyKeypad2: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad2');
end;

function GCKeyKeypad3: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad3');
end;

function GCKeyKeypad4: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad4');
end;

function GCKeyKeypad5: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad5');
end;

function GCKeyKeypad6: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad6');
end;

function GCKeyKeypad7: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad7');
end;

function GCKeyKeypad8: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad8');
end;

function GCKeyKeypad9: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad9');
end;

function GCKeyKeypad0: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad0');
end;

function GCKeyKeypadPeriod: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadPeriod');
end;

function GCKeyKeypadEqualSign: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadEqualSign');
end;

function GCKeyNonUSBackslash: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyNonUSBackslash');
end;

function GCKeyApplication: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyApplication');
end;

function GCKeyPower: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyPower');
end;

function GCKeyInternational1: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational1');
end;

function GCKeyInternational2: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational2');
end;

function GCKeyInternational3: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational3');
end;

function GCKeyInternational4: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational4');
end;

function GCKeyInternational5: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational5');
end;

function GCKeyInternational6: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational6');
end;

function GCKeyInternational7: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational7');
end;

function GCKeyInternational8: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational8');
end;

function GCKeyInternational9: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational9');
end;

function GCKeyLANG1: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG1');
end;

function GCKeyLANG2: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG2');
end;

function GCKeyLANG3: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG3');
end;

function GCKeyLANG4: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG4');
end;

function GCKeyLANG5: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG5');
end;

function GCKeyLANG6: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG6');
end;

function GCKeyLANG7: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG7');
end;

function GCKeyLANG8: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG8');
end;

function GCKeyLANG9: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG9');
end;

function GCKeyLeftControl: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLeftControl');
end;

function GCKeyLeftShift: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLeftShift');
end;

function GCKeyLeftAlt: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLeftAlt');
end;

function GCKeyLeftGUI: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLeftGUI');
end;

function GCKeyRightControl: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyRightControl');
end;

function GCKeyRightShift: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyRightShift');
end;

function GCKeyRightAlt: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyRightAlt');
end;

function GCKeyRightGUI: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyRightGUI');
end;

function GCInputMicroGamepadDpad: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCInputMicroGamepadDpad');
end;

function GCInputMicroGamepadButtonA: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCInputMicroGamepadButtonA');
end;

function GCInputMicroGamepadButtonX: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCInputMicroGamepadButtonX');
end;

function GCInputMicroGamepadButtonMenu: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCInputMicroGamepadButtonMenu');
end;

function GCInputDirectionalDpad: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCInputDirectionalDpad');
end;

function GCInputDirectionalTouchSurfaceButton: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCInputDirectionalTouchSurfaceButton');
end;

function GCInputDirectionalCardinalDpad: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCInputDirectionalCardinalDpad');
end;

function GCInputDirectionalCenterButton: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCInputDirectionalCenterButton');
end;

function GCProductCategoryDualSense: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCProductCategoryDualSense');
end;

function GCProductCategoryDualShock4: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCProductCategoryDualShock4');
end;

function GCProductCategoryMFi: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCProductCategoryMFi');
end;

function GCProductCategoryXboxOne: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCProductCategoryXboxOne');
end;

function GCProductCategoryHID: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCProductCategoryHID');
end;

function GCProductCategorySiriRemote1stGen: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCProductCategorySiriRemote1stGen');
end;

function GCProductCategorySiriRemote2ndGen: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCProductCategorySiriRemote2ndGen');
end;

function GCProductCategoryControlCenterRemote: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCProductCategoryControlCenterRemote');
end;

function GCProductCategoryUniversalElectronicsRemote: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCProductCategoryUniversalElectronicsRemote');
end;

function GCProductCategoryCoalescedRemote: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCProductCategoryCoalescedRemote');
end;

function GCProductCategoryMouse: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCProductCategoryMouse');
end;

function GCProductCategoryKeyboard: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCProductCategoryKeyboard');
end;

function GCControllerDidConnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCControllerDidConnectNotification');
end;

function GCControllerDidDisconnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCControllerDidDisconnectNotification');
end;

function GCControllerDidBecomeCurrentNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCControllerDidBecomeCurrentNotification');
end;

function GCControllerDidStopBeingCurrentNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCControllerDidStopBeingCurrentNotification');
end;

function GCControllerUserCustomizationsDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCControllerUserCustomizationsDidChangeNotification');
end;

function GCKeyboardDidConnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCKeyboardDidConnectNotification');
end;

function GCKeyboardDidDisconnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCKeyboardDidDisconnectNotification');
end;

function GCMouseDidConnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCMouseDidConnectNotification');
end;

function GCMouseDidDisconnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCMouseDidDisconnectNotification');
end;

function GCMouseDidBecomeCurrentNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCMouseDidBecomeCurrentNotification');
end;

function GCMouseDidStopBeingCurrentNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCMouseDidStopBeingCurrentNotification');
end;

function GCRacingWheelDidConnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCRacingWheelDidConnectNotification');
end;

function GCRacingWheelDidDisconnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCRacingWheelDidDisconnectNotification');
end;

function GCInputButtonA: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonA');
end;

function GCInputButtonB: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonB');
end;

function GCInputButtonX: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonX');
end;

function GCInputButtonY: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonY');
end;

function GCInputDirectionPad: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputDirectionPad');
end;

function GCInputLeftThumbstick: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputLeftThumbstick');
end;

function GCInputRightThumbstick: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputRightThumbstick');
end;

function GCInputLeftShoulder: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputLeftShoulder');
end;

function GCInputRightShoulder: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputRightShoulder');
end;

function GCInputLeftTrigger: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputLeftTrigger');
end;

function GCInputRightTrigger: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputRightTrigger');
end;

function GCInputLeftThumbstickButton: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputLeftThumbstickButton');
end;

function GCInputRightThumbstickButton: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCInputRightThumbstickButton');
end;

function GCInputButtonHome: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonHome');
end;

function GCInputButtonMenu: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonMenu');
end;

function GCInputButtonOptions: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonOptions');
end;

function GCInputButtonShare: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonShare');
end;

function GCInputXboxPaddleOne: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputXboxPaddleOne');
end;

function GCInputXboxPaddleTwo: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputXboxPaddleTwo');
end;

function GCInputXboxPaddleThree: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputXboxPaddleThree');
end;

function GCInputXboxPaddleFour: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputXboxPaddleFour');
end;

function GCInputDualShockTouchpadOne: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputDualShockTouchpadOne');
end;

function GCInputDualShockTouchpadTwo: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputDualShockTouchpadTwo');
end;

function GCInputDualShockTouchpadButton: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCInputDualShockTouchpadButton');
end;

function GCInputSteeringWheel: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputSteeringWheel');
end;

function GCInputShifter: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputShifter');
end;

function GCInputPedalAccelerator: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputPedalAccelerator');
end;

function GCInputPedalBrake: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputPedalBrake');
end;

function GCInputPedalClutch: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputPedalClutch');
end;

function GCInputLeftPaddle: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputLeftPaddle');
end;

function GCInputRightPaddle: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputRightPaddle');
end;

function GCCurrentExtendedGamepadSnapshotDataVersion: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCCurrentExtendedGamepadSnapshotDataVersion');
end;

function GCKeyCodeKeyA: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyA');
end;

function GCKeyCodeKeyB: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyB');
end;

function GCKeyCodeKeyC: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyC');
end;

function GCKeyCodeKeyD: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyD');
end;

function GCKeyCodeKeyE: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyE');
end;

function GCKeyCodeKeyF: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyF');
end;

function GCKeyCodeKeyG: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyG');
end;

function GCKeyCodeKeyH: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyH');
end;

function GCKeyCodeKeyI: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyI');
end;

function GCKeyCodeKeyJ: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyJ');
end;

function GCKeyCodeKeyK: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyK');
end;

function GCKeyCodeKeyL: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyL');
end;

function GCKeyCodeKeyM: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyM');
end;

function GCKeyCodeKeyN: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyN');
end;

function GCKeyCodeKeyO: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyO');
end;

function GCKeyCodeKeyP: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyP');
end;

function GCKeyCodeKeyQ: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyQ');
end;

function GCKeyCodeKeyR: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyR');
end;

function GCKeyCodeKeyS: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyS');
end;

function GCKeyCodeKeyT: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyT');
end;

function GCKeyCodeKeyU: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyU');
end;

function GCKeyCodeKeyV: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyV');
end;

function GCKeyCodeKeyW: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyW');
end;

function GCKeyCodeKeyX: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyX');
end;

function GCKeyCodeKeyY: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyY');
end;

function GCKeyCodeKeyZ: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyZ');
end;

function GCKeyCodeOne: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeOne');
end;

function GCKeyCodeTwo: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeTwo');
end;

function GCKeyCodeThree: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeThree');
end;

function GCKeyCodeFour: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeFour');
end;

function GCKeyCodeFive: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeFive');
end;

function GCKeyCodeSix: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeSix');
end;

function GCKeyCodeSeven: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeSeven');
end;

function GCKeyCodeEight: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeEight');
end;

function GCKeyCodeNine: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeNine');
end;

function GCKeyCodeZero: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeZero');
end;

function GCKeyCodeReturnOrEnter: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeReturnOrEnter');
end;

function GCKeyCodeEscape: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeEscape');
end;

function GCKeyCodeDeleteOrBackspace: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeDeleteOrBackspace');
end;

function GCKeyCodeTab: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeTab');
end;

function GCKeyCodeSpacebar: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeSpacebar');
end;

function GCKeyCodeHyphen: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeHyphen');
end;

function GCKeyCodeEqualSign: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeEqualSign');
end;

function GCKeyCodeOpenBracket: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeOpenBracket');
end;

function GCKeyCodeCloseBracket: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeCloseBracket');
end;

function GCKeyCodeBackslash: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeBackslash');
end;

function GCKeyCodeNonUSPound: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeNonUSPound');
end;

function GCKeyCodeSemicolon: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeSemicolon');
end;

function GCKeyCodeQuote: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeQuote');
end;

function GCKeyCodeGraveAccentAndTilde: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCKeyCodeGraveAccentAndTilde');
end;

function GCKeyCodeComma: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeComma');
end;

function GCKeyCodePeriod: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodePeriod');
end;

function GCKeyCodeSlash: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeSlash');
end;

function GCKeyCodeCapsLock: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeCapsLock');
end;

function GCKeyCodeF1: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF1');
end;

function GCKeyCodeF2: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF2');
end;

function GCKeyCodeF3: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF3');
end;

function GCKeyCodeF4: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF4');
end;

function GCKeyCodeF5: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF5');
end;

function GCKeyCodeF6: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF6');
end;

function GCKeyCodeF7: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF7');
end;

function GCKeyCodeF8: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF8');
end;

function GCKeyCodeF9: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF9');
end;

function GCKeyCodeF10: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF10');
end;

function GCKeyCodeF11: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF11');
end;

function GCKeyCodeF12: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF12');
end;

function GCKeyCodeF13: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF13');
end;

function GCKeyCodeF14: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF14');
end;

function GCKeyCodeF15: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF15');
end;

function GCKeyCodeF16: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF16');
end;

function GCKeyCodeF17: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF17');
end;

function GCKeyCodeF18: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF18');
end;

function GCKeyCodeF19: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF19');
end;

function GCKeyCodeF20: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF20');
end;

function GCKeyCodePrintScreen: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodePrintScreen');
end;

function GCKeyCodeScrollLock: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeScrollLock');
end;

function GCKeyCodePause: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodePause');
end;

function GCKeyCodeInsert: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInsert');
end;

function GCKeyCodeHome: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeHome');
end;

function GCKeyCodePageUp: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodePageUp');
end;

function GCKeyCodeDeleteForward: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeDeleteForward');
end;

function GCKeyCodeEnd: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeEnd');
end;

function GCKeyCodePageDown: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodePageDown');
end;

function GCKeyCodeRightArrow: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeRightArrow');
end;

function GCKeyCodeLeftArrow: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLeftArrow');
end;

function GCKeyCodeDownArrow: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeDownArrow');
end;

function GCKeyCodeUpArrow: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeUpArrow');
end;

function GCKeyCodeKeypadNumLock: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadNumLock');
end;

function GCKeyCodeKeypadSlash: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadSlash');
end;

function GCKeyCodeKeypadAsterisk: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadAsterisk');
end;

function GCKeyCodeKeypadHyphen: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadHyphen');
end;

function GCKeyCodeKeypadPlus: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadPlus');
end;

function GCKeyCodeKeypadEnter: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadEnter');
end;

function GCKeyCodeKeypad1: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad1');
end;

function GCKeyCodeKeypad2: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad2');
end;

function GCKeyCodeKeypad3: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad3');
end;

function GCKeyCodeKeypad4: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad4');
end;

function GCKeyCodeKeypad5: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad5');
end;

function GCKeyCodeKeypad6: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad6');
end;

function GCKeyCodeKeypad7: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad7');
end;

function GCKeyCodeKeypad8: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad8');
end;

function GCKeyCodeKeypad9: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad9');
end;

function GCKeyCodeKeypad0: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad0');
end;

function GCKeyCodeKeypadPeriod: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadPeriod');
end;

function GCKeyCodeKeypadEqualSign: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadEqualSign');
end;

function GCKeyCodeNonUSBackslash: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeNonUSBackslash');
end;

function GCKeyCodeApplication: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeApplication');
end;

function GCKeyCodePower: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodePower');
end;

function GCKeyCodeInternational1: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational1');
end;

function GCKeyCodeInternational2: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational2');
end;

function GCKeyCodeInternational3: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational3');
end;

function GCKeyCodeInternational4: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational4');
end;

function GCKeyCodeInternational5: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational5');
end;

function GCKeyCodeInternational6: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational6');
end;

function GCKeyCodeInternational7: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational7');
end;

function GCKeyCodeInternational8: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational8');
end;

function GCKeyCodeInternational9: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational9');
end;

function GCKeyCodeLANG1: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG1');
end;

function GCKeyCodeLANG2: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG2');
end;

function GCKeyCodeLANG3: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG3');
end;

function GCKeyCodeLANG4: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG4');
end;

function GCKeyCodeLANG5: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG5');
end;

function GCKeyCodeLANG6: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG6');
end;

function GCKeyCodeLANG7: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG7');
end;

function GCKeyCodeLANG8: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG8');
end;

function GCKeyCodeLANG9: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG9');
end;

function GCKeyCodeLeftControl: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLeftControl');
end;

function GCKeyCodeLeftShift: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLeftShift');
end;

function GCKeyCodeLeftAlt: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLeftAlt');
end;

function GCKeyCodeLeftGUI: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLeftGUI');
end;

function GCKeyCodeRightControl: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeRightControl');
end;

function GCKeyCodeRightShift: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeRightShift');
end;

function GCKeyCodeRightAlt: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeRightAlt');
end;

function GCKeyCodeRightGUI: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeRightGUI');
end;

function GCCurrentMicroGamepadSnapshotDataVersion: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCCurrentMicroGamepadSnapshotDataVersion');
end;

function GCHapticsLocalityDefault: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCHapticsLocalityDefault');
end;

function GCHapticsLocalityAll: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCHapticsLocalityAll');
end;

function GCHapticsLocalityHandles: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCHapticsLocalityHandles');
end;

function GCHapticsLocalityLeftHandle: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCHapticsLocalityLeftHandle');
end;

function GCHapticsLocalityRightHandle: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCHapticsLocalityRightHandle');
end;

function GCHapticsLocalityTriggers: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCHapticsLocalityTriggers');
end;

function GCHapticsLocalityLeftTrigger: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCHapticsLocalityLeftTrigger');
end;

function GCHapticsLocalityRightTrigger: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCHapticsLocalityRightTrigger');
end;

function GCHapticDurationInfinite: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCHapticDurationInfinite');
end;

end.
