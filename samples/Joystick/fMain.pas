unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  Gamolf.FMX.Joystick;

type
  TForm2 = class(TForm)
    Rectangle1: TRectangle;
    Timer1: TTimer;
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
  FMX.Platform;

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
  num: integer;
  ji: TJoystickInfo;
begin
  for num := 0 to JoystickService.Count - 1 do
  begin
    try
      JoystickService.getInfo(num, ji);
      if length(ji.PressedButtons) > 0 then
      begin
        Rectangle1.Position.X := Rectangle1.Position.X + ji.Axes[0];
        Rectangle1.Position.y := Rectangle1.Position.y + ji.Axes[1];
      end;
    except
      on e: EJoystickUnpluggedException do;
    end;
  end;
end;

end.
