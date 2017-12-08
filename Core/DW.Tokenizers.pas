unit DW.Tokenizers;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  System.SysUtils;

type
  TTokenProc = reference to procedure(const AToken: string);
  TTextReplaceFunc = reference to function(const AText: string): string;

  TTokenizer = class(TObject)
  private
    const cDefaultWhitespace: TCharArray = [#0, #9, #10, #13, #32, #160];
  private
    FText: string;
    FWhitespace: TCharArray;
    function IsWhitespace(const AChar: Char): Boolean;
  public
    constructor Create;
    procedure Tokenize(const ATokenProc: TTokenProc);
    function Replace(const AReplaceFunction: TTextReplaceFunc): string;
    property Text: string read FText write FText;
    property Whitespace: TCharArray read FWhitespace write FWhitespace;
  end;

implementation

uses
  System.Character, System.Math;

{ TTokenizer }

constructor TTokenizer.Create;
begin
  inherited;
  FWhitespace := cDefaultWhitespace;
end;

function TTokenizer.IsWhitespace(const AChar: Char): Boolean;
begin
  Result := AChar.IsInArray(FWhitespace);
end;

procedure TTokenizer.Tokenize(const ATokenProc: TTokenProc);
var
  LIndex, LLastIndex, LStartIndex, LLow: Integer;
begin
  if Text.Trim.IsEmpty then
    Exit; // <======
  LLow := Low(string);
  LIndex := 0;
  LLastIndex := Length(Text) + LLow;
  while LIndex < LLastIndex do
  begin
    while IsWhitespace(Text[LIndex + LLow]) and (LIndex < LLastIndex) do
      Inc(LIndex);
    if LIndex < LLastIndex then
    begin
      LStartIndex := LIndex;
      while not IsWhitespace(Text[LIndex + LLow]) and (LIndex < Length(Text) + LLow) do
        Inc(LIndex);
      ATokenProc(Text.Substring(LStartIndex, LIndex - LStartIndex));
    end;
  end;
end;

function TTokenizer.Replace(const AReplaceFunction: TTextReplaceFunc): string;
var
  LIndex, LStartIndex, LLow: Integer;
  LToken, LReplaceWith: string;
begin
  if Text.Trim.IsEmpty then
    Exit(Text); // <======
  Result := Text;
  LLow := Low(string); // 0 on mobile, 1 on Win etc
  LIndex := 0;
  while LIndex < Length(Result) + LLow do
  begin
    while IsWhitespace(Result[LIndex + LLow]) and (LIndex < Length(Result) + LLow) do
      Inc(LIndex);
    if LIndex < Length(Result) + LLow then
    begin
      LStartIndex := LIndex;
      while not IsWhitespace(Result[LIndex + LLow]) and (LIndex < Length(Result) + LLow) do
        Inc(LIndex);
      LToken := Result.Substring(LStartIndex, LIndex - LStartIndex);
      LReplaceWith := AReplaceFunction(LToken);
      Result := Result.Remove(LStartIndex, Length(LToken));
      Result := Result.Insert(LStartIndex, LReplaceWith);
      Inc(LIndex, Length(LReplaceWith) - Length(LToken));
    end;
  end;
end;

end.
