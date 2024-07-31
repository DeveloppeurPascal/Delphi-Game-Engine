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
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  Gamolf.FMX.HelpBar;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DGEFMXHelpBar1: TDGEFMXHelpBar;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  Olf.Skia.SVGToBitmap,
  USVGKenneyInputKeys,
  USVGKenneyInputXbox;

procedure TForm2.Button1Click(Sender: TObject);
begin
  DGEFMXHelpBar1.CloseHelpBar;
  DGEFMXHelpBar1.OpenHelpBar;
  DGEFMXHelpBar1.HorzAlign := TDGEFMXHelpBarHorzAlign.Center;
  DGEFMXHelpBar1.VertAlign := TDGEFMXHelpBarVertAlign.Bottom;
  DGEFMXHelpBar1.AddItem(TSVGKenneyInputKeys.KeyboardArrowDown,
    TSVGKenneyInputXbox.XboxDpadRoundDown, 'down');
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  DGEFMXHelpBar1.CloseHelpBar;
  DGEFMXHelpBar1.OpenHelpBar;
  DGEFMXHelpBar1.HorzAlign := TDGEFMXHelpBarHorzAlign.Left;
  DGEFMXHelpBar1.VertAlign := TDGEFMXHelpBarVertAlign.Top;
  DGEFMXHelpBar1.AddItem(TSVGKenneyInputKeys.KeyboardArrowDown,
    TSVGKenneyInputXbox.XboxDpadRoundDown, 'down');
  DGEFMXHelpBar1.AddItem(TSVGKenneyInputKeys.KeyboardEscape,
    TSVGKenneyInputXbox.XboxButtonColorX, 'quit');
  DGEFMXHelpBar1.AddItem(TSVGKenneyInputKeys.KeyboardSpace,
    TSVGKenneyInputXbox.XboxButtonColorA, 'go');
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  DGEFMXHelpBar1.CloseHelpBar;
  DGEFMXHelpBar1.OpenHelpBar;
  DGEFMXHelpBar1.HorzAlign := TDGEFMXHelpBarHorzAlign.right;
  DGEFMXHelpBar1.VertAlign := TDGEFMXHelpBarVertAlign.Top;
  DGEFMXHelpBar1.AddItem(TSVGKenneyInputKeys.KeyboardSpace,
    TSVGKenneyInputXbox.XboxButtonColorA);
  DGEFMXHelpBar1.AddItem(TSVGKenneyInputKeys.KeyboardEscape,
    TSVGKenneyInputXbox.XboxButtonColorX, 'quit');
  DGEFMXHelpBar1.AddItem(TSVGKenneyInputKeys.KeyboardSpace,
    TSVGKenneyInputXbox.XboxButtonColorA, 'go');
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  DGEFMXHelpBar1.IconGamepadBitmapListIndex := TOlfSVGBitmapList.AddAList;
  TOlfSVGBitmapList.AddItem(DGEFMXHelpBar1.IconGamepadBitmapListIndex,
    SVGKenneyInputXbox);

  DGEFMXHelpBar1.IconKeyBitmapListIndex := TOlfSVGBitmapList.AddAList;
  TOlfSVGBitmapList.AddItem(DGEFMXHelpBar1.IconKeyBitmapListIndex,
    SVGKenneyInputKeys);
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
