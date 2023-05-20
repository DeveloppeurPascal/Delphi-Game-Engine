# 20230520 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* renamed the unit Gamolf.RTL.Joystick.Windows.pas into Gamolf.RTL.Joystick.DirectInput.Win.pas
* renamed the service type TGamolfJoystickWindowsService into TGamolfJoystickWinDirectInputService
* renamed the structure info TGamolfJoystickJoyCaps into TGamolfJoystickDirectInputJoyCaps
* updated the Joystick sample project
* added the microGamePad for game controllers with a DPad entry in hasDPad (macOS/iOS/tvO)
* added a common function to get DPad direction for macOS/iOS
* added DPad position to getInfo() function for macOS/iOS
* fixed an error for microGamepad controller in getInfo() function for macOS/iOS
* added the display of DPad orientation in the sample "Joystick" program
* added the display of Joystick (left+right) axes in the sample "Joystick" program
* added the display of trigger (left+right) pression in the sample "Joystick" program
* added a round() on axes values in the Joystick sample program for moving the rectangle on screen
* fixed axes values for macOS/iOS game controllers
* passed test the game controller FMX platform service on macOS
* passed test the game controller FMX platform service on iOS
