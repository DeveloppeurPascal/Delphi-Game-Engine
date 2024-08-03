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
/// File last update : 31/07/2024 14:23:12
/// Signature : e30d480377197f003a2405707717afe98d7d77d4
/// ***************************************************************************
/// </summary>

unit USVGKenneyInputKeys;

// ****************************************
// * SVG from folder :
// * C:\Users\patrickpremartin\Documents\Embarcadero\Studio\Projets\___librairies-et-composants\Delphi-Game-Engine\samples\HelpBar\assets\KenneyInputKeys\uSVGKenneyInputKeys.pas
// ****************************************
//
// This file contains a list of contants and 
// an enumeration to access to SVG source codes 
// from the generated array of strings.
//
// ****************************************
// File generator : SVG Folder to Delphi Unit (1.0)
// Website : https://svgfolder2delphiunit.olfsoftware.fr/
// Generation date : 31/07/2024 14:23:12
//
// Don't do any change on this file.
// They will be erased by next generation !
// ****************************************

interface

const
  CSVGKeyboardArrowDown = 0;
  CSVGKeyboardEscape = 1;
  CSVGKeyboardSpace = 2;

type
{$SCOPEDENUMS ON}
  TSVGKenneyInputKeysIndex = (
    KeyboardArrowDown = CSVGKeyboardArrowDown,
    KeyboardEscape = CSVGKeyboardEscape,
    KeyboardSpace = CSVGKeyboardSpace);

  TSVGKenneyInputKeys = class
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
    KeyboardArrowDown = CSVGKeyboardArrowDown;
    KeyboardEscape = CSVGKeyboardEscape;
    KeyboardSpace = CSVGKeyboardSpace;
    class property Tag: integer read FTag write SetTag;
    class property TagBool: Boolean read FTagBool write SetTagBool;
    class property TagFloat: Single read FTagFloat write SetTagFloat;
    class property TagObject: TObject read FTagObject write SetTagObject;
    class property TagString: string read FTagString write SetTagString;
    class function SVG(const Index: Integer): string; overload;
    class function SVG(const Index: TSVGKenneyInputKeysIndex) : string; overload;
    class function Count : Integer;
    class constructor Create;
  end;

var
  SVGKenneyInputKeys : array of String;

implementation

uses
  System.SysUtils;

{ TSVGKenneyInputKeys }

class constructor TSVGKenneyInputKeys.Create;
begin
  inherited;
  FTag := 0;
  FTagBool := false;
  FTagFloat := 0;
  FTagObject := nil;
  FTagString := '';
end;

class procedure TSVGKenneyInputKeys.SetTag(const Value: integer);
begin
  FTag := Value;
end;

class procedure TSVGKenneyInputKeys.SetTagBool(const Value: Boolean);
begin
  FTagBool := Value;
end;

class procedure TSVGKenneyInputKeys.SetTagFloat(const Value: Single);
begin
  FTagFloat := Value;
end;

class procedure TSVGKenneyInputKeys.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

class procedure TSVGKenneyInputKeys.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

class function TSVGKenneyInputKeys.SVG(const Index: Integer): string;
begin
  if (index < Count) then
    result := SVGKenneyInputKeys[index]
  else
    raise Exception.Create('SVG not found. Index out of range.');
end;

class function TSVGKenneyInputKeys.SVG(const Index : TSVGKenneyInputKeysIndex): string;
begin
  result := SVG(ord(index));
end;

class function TSVGKenneyInputKeys.Count: Integer;
begin
  result := length(SVGKenneyInputKeys);
end;

initialization

SetLength(SVGKenneyInputKeys, 3);

{$TEXTBLOCK NATIVE XML}
SVGKenneyInputKeys[CSVGKeyboardArrowDown] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M8 48 L8 16 Q8 8 16 8 L48 8 Q56 8 56 16 L56 48 Q56 56 48 56 L16 56 Q8 56 8 48 M32 42 L40 34 40 32 36 32 36 22 28 22 28 32 24 32 24 34 32 42"/>
  </g>
</svg>
''';
SVGKenneyInputKeys[CSVGKeyboardEscape] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M16 8 L48 8 Q56 8 56 16 L56 48 Q56 56 48 56 L16 56 Q8 56 8 48 L8 16 Q8 8 16 8 M44.8 37 Q46.05 35.8 46 34.3 L46 32.8 43 32.8 43 34.3 42.75 34.8 42 35 41.2 34.8 41 34.3 41 28.75 41.2 28.3 41.25 28.25 Q41.55 28 42 28 42.45 28 42.8 28.3 L43 28.75 43 30.25 46 30.25 46 28.75 Q46 27.15 44.75 26.05 43.55 25 42 25 40.4 25 39.25 26.05 L39.2 26.05 Q38 27.15 38 28.75 L38 34.3 Q38 35.8 39.15 36.95 L39.2 37.05 39.3 37.1 Q40.4 38 42 38 43.55 38 44.7 37.1 L44.8 37 M18 25 L18 38 26 38 26 35 21 35 21 33 26 33 26 30 21 30 21 28 26 28 26 25 18 25 M32 28 L36 28 36 25 32 25 Q30.4 25 29.2 26.2 28 27.4 28 29 28 30.6 29.2 31.8 30.4 33 31.95 33 L32.05 33 Q32.4 33 32.7 33.3 L33 34 32.7 34.7 32 35 28 35 28 38 32 38 Q33.6 38 34.8 36.8 36 35.6 36 34 36 32.4 34.8 31.2 33.6 30 32.05 30 L31.95 30 31.3 29.7 Q31 29.4 31 29 31 28.6 31.3 28.3 31.6 28 32 28"/>
  </g>
</svg>
''';
SVGKenneyInputKeys[CSVGKeyboardSpace] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
<path stroke="none" fill="#FFFFFF" d="M41.1 36.25 Q42 35.35 42 34.25 L41.7 33.55 Q41.4 33.25 41 33.25 40.6 33.25 40.3 33.55 40 33.85 40 34.25 L39.75 34.8 39.75 34.75 39 35 38.2 34.75 38 34.25 38 29.8 Q38 29.45 38.25 29.2 L38.2 29.3 Q38.55 29 39 29 39.45 29 39.75 29.3 L39.75 29.25 40 29.8 Q40 30.2 40.3 30.5 L41 30.8 41.7 30.5 42 29.8 Q42 28.65 41.1 27.8 L41.05 27.75 Q40.15 27 39 27 37.75 27 36.9 27.75 L36.85 27.8 Q36 28.65 36 29.8 L36 34.25 Q36 35.35 36.85 36.2 L36.9 36.3 Q37.75 37 39 37 40.15 37 41.05 36.3 L41.1 36.25 M29 29.8 L29 36 Q29 36.4 29.3 36.7 L30 37 Q30.4 37 30.7 36.7 L31 36 31 34 33 34 33 36 Q33 36.4 33.3 36.7 L34 37 Q34.4 37 34.7 36.7 L35 36 35 29.8 Q35 28.65 34.1 27.8 L34.05 27.75 Q33.15 27 32 27 30.75 27 29.9 27.75 L29.85 27.8 Q29 28.65 29 29.8 M31 29.8 Q31 29.45 31.25 29.2 L31.2 29.3 Q31.55 29 32 29 32.45 29 32.75 29.3 L32.75 29.25 33 29.8 33 32 31 32 31 29.8 M48 27 L44 27 Q43.6 27 43.3 27.3 43 27.6 43 28 L43 36 Q43 36.4 43.3 36.7 43.6 37 44 37 L48 37 Q48.4 37 48.7 36.7
L49 36 48.7 35.3 Q48.4 35 48 35 L45 35 45 33 48 33 Q48.4 33 48.7 32.7 L49 32 48.7 31.3 Q48.4 31 48 31 L45 31 45 29 48 29 Q48.4 29 48.7 28.7 L49 28 48.7 27.3 Q48.4 27 48 27 M16 18 L48 18 Q56 18 56 26 L56 38 Q56 42.9 53 44.8 51.1 46 48 46 L16 46 Q12.9 46 11 44.8 8 42.9 8 38 L8 26 Q8 18 16 18 M18 29 L20 29 20.7 28.7 21 28 20.7 27.3 Q20.4 27 20 27 L18 27 Q16.8 27 15.9 27.9 15 28.8 15 30 15 31.2 15.9 32.1 16.8 33 17.95 33 L18.05 33 Q18.4 33 18.7 33.3 L19 34 18.7 34.7 Q18.4 35 18 35 L16 35 Q15.6 35 15.3 35.3 15 35.6 15 36 15 36.4 15.3 36.7 L16 37 18 37 Q19.2 37 20.1 36.1 21 35.2 21 34 21 32.8 20.1 31.9 19.2 31 18.05 31 L17.95 31 Q17.6 31 17.3 30.7 17 30.4 17 30 17 29.6 17.3 29.3 17.6 29 18 29 M25.1 33 L26.35 32.7 26.45 32.65 27.1 32.15 27.1 32.1 Q28 31.25 28 30 28 28.75 27.1 27.85 26.25 27 25.05 27 L23 27 Q22.6 27 22.3 27.3 22 27.6 22 28 L22 36 Q22 36.4 22.3 36.7 22.6 37 23 37 L23.7 36.7 24 36 24 33 25.1 33 M25.1 31 L24 31 24 29 25.05 29 25.7 29.25 26 30 25.75 30.7 25.7 30.7 25.45 30.9 25.1
31"/>
  </g>
</svg>
''';

end.
