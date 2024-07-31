unit USVGKenneyInputXbox;

// ****************************************
// * SVG from folder :
// * C:\Users\patrickpremartin\Documents\Embarcadero\Studio\Projets\___librairies-et-composants\Delphi-Game-Engine\samples\HelpBar\assets\KenneyInputXbox\uSVGKenneyInputXbox.pas
// ****************************************
//
// This file contains a list of contants and 
// an enumeration to access to SVG source codes 
// from the generated array of strings.
//
// ****************************************
// File generator : SVG Folder to Delphi Unit (1.0)
// Website : https://svgfolder2delphiunit.olfsoftware.fr/
// Generation date : 31/07/2024 14:23:21
//
// Don't do any change on this file.
// They will be erased by next generation !
// ****************************************

interface

const
  CSVGXboxButtonColorA = 0;
  CSVGXboxButtonColorX = 1;
  CSVGXboxDpadRoundDown = 2;

type
{$SCOPEDENUMS ON}
  TSVGKenneyInputXboxIndex = (
    XboxButtonColorA = CSVGXboxButtonColorA,
    XboxButtonColorX = CSVGXboxButtonColorX,
    XboxDpadRoundDown = CSVGXboxDpadRoundDown);

  TSVGKenneyInputXbox = class
  private
  class var
    FTag: integer;
    FTagBool: Boolean;
    FTagFloat: Single;
    FTagObject: TObject;
    FTagString: string;
    class procedure SetTag(const Value: integer); static;
    class procedure SetTagBool(const Value: Boolean); static;
    class procedure SetTagFloat(const Value: Single); static;
    class procedure SetTagObject(const Value: TObject); static;
    class procedure SetTagString(const Value: string); static;
  public const
    XboxButtonColorA = CSVGXboxButtonColorA;
    XboxButtonColorX = CSVGXboxButtonColorX;
    XboxDpadRoundDown = CSVGXboxDpadRoundDown;
    class property Tag: integer read FTag write SetTag;
    class property TagBool: Boolean read FTagBool write SetTagBool;
    class property TagFloat: Single read FTagFloat write SetTagFloat;
    class property TagObject: TObject read FTagObject write SetTagObject;
    class property TagString: string read FTagString write SetTagString;
    class function SVG(const Index: Integer): string; overload;
    class function SVG(const Index: TSVGKenneyInputXboxIndex) : string; overload;
    class function Count : Integer;
    class constructor Create;
  end;

var
  SVGKenneyInputXbox : array of String;

implementation

uses
  System.SysUtils;

{ TSVGKenneyInputXbox }

class constructor TSVGKenneyInputXbox.Create;
begin
  inherited;
  FTag := 0;
  FTagBool := false;
  FTagFloat := 0;
  FTagObject := nil;
  FTagString := '';
end;

class procedure TSVGKenneyInputXbox.SetTag(const Value: integer);
begin
  FTag := Value;
end;

class procedure TSVGKenneyInputXbox.SetTagBool(const Value: Boolean);
begin
  FTagBool := Value;
end;

class procedure TSVGKenneyInputXbox.SetTagFloat(const Value: Single);
begin
  FTagFloat := Value;
end;

class procedure TSVGKenneyInputXbox.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

class procedure TSVGKenneyInputXbox.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

class function TSVGKenneyInputXbox.SVG(const Index: Integer): string;
begin
  if (index < Count) then
    result := SVGKenneyInputXbox[index]
  else
    raise Exception.Create('SVG not found. Index out of range.');
end;

class function TSVGKenneyInputXbox.SVG(const Index : TSVGKenneyInputXboxIndex): string;
begin
  result := SVG(ord(index));
end;

class function TSVGKenneyInputXbox.Count: Integer;
begin
  result := length(SVGKenneyInputXbox);
end;

initialization

SetLength(SVGKenneyInputXbox, 3);

{$TEXTBLOCK NATIVE XML}
SVGKenneyInputXbox[CSVGXboxButtonColorA] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#7DB700" d="M56 32 Q56 42 48.95 48.95 42 56 32 56 22.05 56 15 48.95 8 42 8 32 8 22.05 15 15 22.05 8 32 8 42 8 48.95 15 56 22.05 56 32 M38 42 L42 42 34 22 30 22 22 42 26 42 27.6 38 36.4 38 38 42 M32 27 L34.8 34 29.2 34 32 27"/>
  </g>
</svg>
''';
SVGKenneyInputXbox[CSVGXboxButtonColorX] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#009FEB" d="M56 32 Q56 42 48.95 48.95 42 56 32 56 22.05 56 15 48.95 8 42 8 32 8 22.05 15 15 22.05 8 32 8 42 8 48.95 15 56 22.05 56 32 M23 25 L29 32 23 39 23 40 25.25 42 26.4 42 32 35.5 37.65 42 38.75 42 41 40 41 39 35 32 41 25 41 24 38.75 22 37.65 22 32 28.55 26.4 22 25.25 22 23 24 23 25"/>
  </g>
</svg>
''';
SVGKenneyInputXbox[CSVGXboxDpadRoundDown] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M56 32 Q56 42 48.95 48.95 42 56 32 56 22.05 56 15 48.95 8 42 8 32 8 22.05 15 15 22.05 8 32 8 42 8 48.95 15 56 22.05 56 32 M44 38 L48 38 Q52 38 52 34 L52 30 Q52 26 48 26 L44 26 Q41.55 25.9 39.85 24.15 38.05 22.4 38 20 L38 16 Q38 12 34 12 L30 12 Q26 12 26 16 L26 20 Q25.95 22.4 24.15 24.15 22.45 25.9 20 26 L16 26 Q12 26 12 30 L12 34 Q12 38 16 38 L20 38 Q22.45 38.1 24.15 39.85 25.95 41.6 26 44 L26 48 Q26 52 30 52 L34 52 Q38 52 38 48 L38 44 Q38.05 41.6 39.85 39.85 41.55 38.1 44 38"/>
    <path stroke="none" fill="#E73246" d="M44 38 Q41.55 38.1 39.85 39.85 38.05 41.6 38 44 L38 48 Q38 52 34 52 L30 52 Q26 52 26 48 L26 44 Q25.95 41.6 24.15 39.85 22.45 38.1 20 38 L44 38"/>
  </g>
</svg>
''';

end.
