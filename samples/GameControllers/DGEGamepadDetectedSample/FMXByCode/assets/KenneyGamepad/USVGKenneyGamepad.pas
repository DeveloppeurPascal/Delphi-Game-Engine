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
/// File last update : 30/07/2024 12:17:18
/// Signature : e8d2dda180daebdb4088b8e8f43d5fefc18ed7ad
/// ***************************************************************************
/// </summary>

unit USVGKenneyGamepad;

// ****************************************
// * SVG from folder :
// * C:\Users\patrickpremartin\Documents\Embarcadero\Studio\Projets\Nouveau dossier\assets\KenneyGamepad\uSVGKenneyGamepad.pas
// ****************************************
//
// This file contains a list of contants and 
// an enumeration to access to SVG source codes 
// from the generated array of strings.
//
// ****************************************
// File generator : SVG Folder to Delphi Unit (1.0)
// Website : https://svgfolder2delphiunit.olfsoftware.fr/
// Generation date : 30/07/2024 12:17:18
//
// Don't do any change on this file.
// They will be erased by next generation !
// ****************************************

interface

const
  CSVGControllerDisconnected = 0;
  CSVGControllerGeneric = 1;

type
{$SCOPEDENUMS ON}
  TSVGKenneyGamepadIndex = (
    ControllerDisconnected = CSVGControllerDisconnected,
    ControllerGeneric = CSVGControllerGeneric);

  TSVGKenneyGamepad = class
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
    ControllerDisconnected = CSVGControllerDisconnected;
    ControllerGeneric = CSVGControllerGeneric;
    class property Tag: integer read FTag write SetTag;
    class property TagBool: Boolean read FTagBool write SetTagBool;
    class property TagFloat: Single read FTagFloat write SetTagFloat;
    class property TagObject: TObject read FTagObject write SetTagObject;
    class property TagString: string read FTagString write SetTagString;
    class function SVG(const Index: Integer): string; overload;
    class function SVG(const Index: TSVGKenneyGamepadIndex) : string; overload;
    class function Count : Integer;
    class constructor Create;
  end;

var
  SVGKenneyGamepad : array of String;

implementation

uses
  System.SysUtils;

{ TSVGKenneyGamepad }

class constructor TSVGKenneyGamepad.Create;
begin
  inherited;
  FTag := 0;
  FTagBool := false;
  FTagFloat := 0;
  FTagObject := nil;
  FTagString := '';
end;

class procedure TSVGKenneyGamepad.SetTag(const Value: integer);
begin
  FTag := Value;
end;

class procedure TSVGKenneyGamepad.SetTagBool(const Value: Boolean);
begin
  FTagBool := Value;
end;

class procedure TSVGKenneyGamepad.SetTagFloat(const Value: Single);
begin
  FTagFloat := Value;
end;

class procedure TSVGKenneyGamepad.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

class procedure TSVGKenneyGamepad.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

class function TSVGKenneyGamepad.SVG(const Index: Integer): string;
begin
  if (index < Count) then
    result := SVGKenneyGamepad[index]
  else
    raise Exception.Create('SVG not found. Index out of range.');
end;

class function TSVGKenneyGamepad.SVG(const Index : TSVGKenneyGamepadIndex): string;
begin
  result := SVG(ord(index));
end;

class function TSVGKenneyGamepad.Count: Integer;
begin
  result := length(SVGKenneyGamepad);
end;

initialization

SetLength(SVGKenneyGamepad, 2);

{$TEXTBLOCK NATIVE XML}
SVGKenneyGamepad[CSVGControllerDisconnected] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
<path stroke="none" fill="#FCFCFC" d="M34 38 L23 38 15.35 45.65 Q14.7 46.4 13.75 46.75 L11.85 46.95 Q10.15 46.85 9 45.45 7.85 44.05 8 42.4 L11.15 22.7 Q11.5 20.6 12.7 18.8 13.85 17 15.65 15.8 16.95 14.9 18.5 14.45 20.05 13.95 21.65 14 L42.75 14 Q45.95 14.05 48.7 16 51.4 18 52.4 21 L53.1 24.25 54.75 34.55 Q53.95 34 53 34 L49 34 Q47.75 34 46.9 34.9 L45 36.75 43.15 34.9 Q42.25 34 41 34 L37 34 Q35.75 34 34.9 34.9 34 35.75 34 37 L34 38 M55.9 41.85 Q56.1 42.75 55.9 43.65 55.65 44.75 54.95 45.5 L54.35 46.1 53.25 45 55.15 43.15 55.9 41.85 M44 31 Q44 30.15 43.4 29.55 42.85 29 42 29 41.15 29 40.55 29.55 40 30.15 40 31 40 31.85 40.55 32.4 41.15 33 42 33 42.85 33 43.4 32.4 44 31.85 44 31 M48 27 Q48 26.15 47.4 25.55 46.85 25 46 25 45.15 25 44.55 25.55 44 26.15 44 27 44 27.85 44.55 28.4 45.15 29 46 29 46.85 29 47.4 28.4 48 27.85 48 27 M44 23 Q44 22.15 43.4 21.55 42.85 21 42 21 41.15 21 40.55 21.55 40 22.15 40 23 40 23.85 40.55 24.4 41.15 25 42 25 42.85 25 43.4 24.4 44 23.85 44 23 M40 27 Q40 26.15
39.4 25.55 38.85 25 38 25 37.15 25 36.55 25.55 36 26.15 36 27 36 27.85 36.55 28.4 37.15 29 38 29 38.85 29 39.4 28.4 40 27.85 40 27 M20 21 L20 25 16 25 16 29 20 29 20 33 24 33 24 29 28 29 28 25 24 25 24 21 20 21"/>
    <path stroke="none" fill="#FFFFFF" fill-opacity="0" d="M54.75 34.55 L55.15 34.9 Q56 35.75 56 37 L56 41 55.9 41.85 55.15 43.15 53.25 45 54.35 46.1 55.15 46.9 Q56 47.75 56 49 L56 53 Q56 54.25 55.15 55.15 54.25 56 53 56 L49 56 Q47.75 56 46.9 55.15 L45 53.25 43.15 55.15 Q42.25 56 41 56 L37 56 Q35.75 56 34.9 55.15 34 54.25 34 53 L34 49 Q34 47.75 34.9 46.9 L36.75 45 34.9 43.15 Q34 42.25 34 41 L34 38 34 37 Q34 35.75 34.9 34.9 35.75 34 37 34 L41 34 Q42.25 34 43.15 34.9 L45 36.75 46.9 34.9 Q47.75 34 49 34 L53 34 Q53.95 34 54.75 34.55 M37 41 L41 45 37 49 37 53 41 53 45 49 49 53 53 53 53 49 49 45 53 41 53 37 49 37 45 41 41 37 37 37 37 41"/>
    <path stroke="none" fill="#FFFFFF" d="M37 41 L37 37 41 37 45 41 49 37 53 37 53 41 49 45 53 49 53 53 49 53 45 49 41 53 37 53 37 49 41 45 37 41"/>
  </g>
</svg>
''';
SVGKenneyGamepad[CSVGControllerGeneric] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
<path stroke="none" fill="#FCFCFC" d="M15.65 15.8 Q16.95 14.9 18.5 14.45 20.05 13.95 21.65 14 L42.75 14 Q45.95 14.05 48.7 16 51.4 18 52.4 21 L53.1 24.25 55.85 41.6 Q56.1 42.65 55.9 43.65 55.65 44.75 54.95 45.5 54.1 46.55 52.7 46.9 L50.1 46.65 Q49.5 46.45 48.85 45.85 L47.75 44.75 44.35 41.35 41 38 23 38 15.35 45.65 Q14.7 46.4 13.75 46.75 L11.85 46.95 Q10.15 46.85 9 45.45 7.85 44.05 8 42.4 L11.15 22.7 Q11.5 20.6 12.7 18.8 13.85 17 15.65 15.8 M40 27 Q40 26.15 39.4 25.55 38.85 25 38 25 37.15 25 36.55 25.55 36 26.15 36 27 36 27.85 36.55 28.4 37.15 29 38 29 38.85 29 39.4 28.4 40 27.85 40 27 M20 21 L20 25 16 25 16 29 20 29 20 33 24 33 24 29 28 29 28 25 24 25 24 21 20 21 M44 31 Q44 30.15 43.4 29.55 42.85 29 42 29 41.15 29 40.55 29.55 40 30.15 40 31 40 31.85 40.55 32.4 41.15 33 42 33 42.85 33 43.4 32.4 44 31.85 44 31 M48 27 Q48 26.15 47.4 25.55 46.85 25 46 25 45.15 25 44.55 25.55 44 26.15 44 27 44 27.85 44.55 28.4 45.15 29 46 29 46.85 29 47.4 28.4 48 27.85 48 27 M44 23 Q44 22.15 43.4 21.55
42.85 21 42 21 41.15 21 40.55 21.55 40 22.15 40 23 40 23.85 40.55 24.4 41.15 25 42 25 42.85 25 43.4 24.4 44 23.85 44 23"/>
  </g>
</svg>
''';

end.
