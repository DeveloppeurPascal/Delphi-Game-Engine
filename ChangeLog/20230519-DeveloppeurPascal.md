# 20230519 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* added macOS GameController framework from macOS 13.1 ARM
* added iOS GameController framework from iOS 16.2
* added compiler condition for macOS only in Macapi.GameController.pas
* added compiler condition for iOS only in iOSapi.GameController.pas
* changed compiler condition for Windows only in Gamolf.RTL.Joystick.Windows.pas
* commented references to CoreHaptic framework (in macOS framework import)
* added macOS and iOS game controllers platform services
* removed haptics dependencies from IOSAPI GameController imported unit
* added new common features to RTL Joystick library
* added Mac and iOS platform service (common unit)
* updated sample Joystick program
* updated the FMX Platform game controller registering for Mac, iOS and Windows
* added info.plist parameters for game controller use in the Joystick sample program
* added GameControler framework dependency to the README FR/EN files
* updated readme FR/EN docs after renaming the project and its repository
