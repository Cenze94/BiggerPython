unit stringutilstests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stringutils, basetypes;

  procedure StartStringutilsTest();
  procedure CopyWordTest();
  procedure GrabWordTest();
  procedure GrabBetweenTest();
  procedure SplitStringTest();
  procedure SplitOnAllTest();
  procedure SplitLinesTest();
  procedure SplitCharsTest();
  procedure GetIntegerTest();
  procedure GetFloatTest();
  procedure GetStringTest();
  procedure LastIndexOfTest();
  procedure FirstIndexOfTest();
  procedure FirstByPrefixTest();
  procedure FirstContainingTest();
  procedure LookupByPrefix();
  procedure DeblankTest();
  procedure TrimmedBlanksTest();
  procedure StringAsArrayTest();
  procedure SaveToFileTest();
  procedure FlattenStringTest();
  procedure SnipStringTest();
  procedure SnipStringAllTest();
  procedure CountInStringTest();
  procedure CleanStringTest();
  procedure AppendToLengthTest();
  procedure FixLineBreaksTest();
  procedure UncasedCompareTest();
  procedure AsSimpleStringTest();
  procedure CopyStringTest();
  procedure AppendToStringList();
  procedure CleanFileNameTest();
  procedure CheckHexTest();
  procedure ReplaceHexCodesTest();
  procedure PosUncasedTest();
  procedure TagStringTest();
  procedure GetTaggedFieldTest();
  procedure GetTaggedFieldsTest();
  procedure DecodeQPTest();
  procedure ConvertHtmlTest();
  procedure HtmlToAsciiTest();
  procedure BasicASCIITest();
  procedure ReadAsSimpleStringTest();
  procedure ReadKeysAsValuesTest();
  procedure RightJustifyTest();
  procedure LeftJustifyTest();

implementation

procedure StartStringutilsTest();
begin
  // CopyWordTest();
  // GrabWordTest();
  // GrabBetweenTest();
  // SplitStringTest();
  // SplitOnAllTest();
  // SplitLinesTest();
  // SplitCharsTest();
  // GetIntegerTest();
  // GetFloatTest();
  // GetStringTest();
  // LastIndexOfTest();
  // FirstIndexOfTest();
  // FirstByPrefixTest();
  // FirstContainingTest();
  // LookupByPrefix();
  // DeblankTest();
  // TrimmedBlanksTest();
  // StringAsArrayTest();
  // SaveToFileTest();
  // FlattenStringTest();
  // SnipStringTest();
  // SnipStringAllTest();
  // CountInStringTest();
  // CleanStringTest();
  // AppendToLengthTest();
  // FixLineBreaksTest();
  // UncasedCompareTest();
  // AsSimpleStringTest();
  // CopyStringTest();
  // AppendToStringList();
  // CleanFileNameTest();
  // CheckHexTest();
  // ReplaceHexCodesTest();
  // PosUncasedTest();
  // TagStringTest();
  // GetTaggedFieldTest();
  // GetTaggedFieldsTest();
  // DecodeQPTest();
  // ConvertHtmlTest();
  // HtmlToAsciiTest();
  // BasicASCIITest();
  // ReadAsSimpleStringTest();
  // ReadKeysAsValuesTest();
  // RightJustifyTest();
  LeftJustifyTest();
end;

procedure CopyWordTest();
var s,sf:string;

begin
  s:='Il gatto è sopra il tavolo.';
  sf:=CopyWord(s, ' ');
  WriteLn(s + 'f');
  WriteLn(sf + 'f');
end;

procedure GrabWordTest();
var s,sf:string;

begin
  s:='Il gatto è sopra il tavolo.';
  sf:=GrabWord(s, ' ');
  WriteLn(s + 'f');
  WriteLn(sf + 'f');
end;

procedure GrabBetweenTest();
var s, sf:string;

begin
  s:='Il gatto è sopra il| tavolo.';
  sf:=GrabBetween(s, ' ', '|');
  WriteLn(s + 'f');
  WriteLn(sf + 'f');
end;

procedure SplitStringTest();
var s:string; x:integer; ss:TSimpleStrings; ts:TStringList;

begin
  s:='Il gatto è sopra il tavolo';
  ss:=SplitString(s, ' ');
  for x:=0 to High(ss) do
  begin
    WriteLn(ss[x]);
  end;
  WriteLn('');

  s:='Il gatto è sopra il tavolo';
  ts:=TStringList.Create;
  SplitString(s, ts, ' ');
  for x:=0 to ts.Count-1 do  begin
    WriteLn(ts[x]);
  end;
  WriteLn('');

  s:='Il gatto è sopra il tavolo';
  ss:=SplitString(s);
  for x:=0 to High(ss) do
  begin
    WriteLn(ss[x]);
  end;
end;

procedure SplitOnAllTest();
begin

end;

procedure SplitLinesTest();
begin

end;

procedure SplitCharsTest();
begin

end;

procedure GetIntegerTest();
var val:integer;

begin
  WriteLn(GetInteger('1  2', 1, 4, val));
  WriteLn(val);
  WriteLn(GetInteger('1  2', 1, 4));
  WriteLn('');
  WriteLn(GetInteger('12f', 1, 3, val));
  WriteLn(val);
  WriteLn(GetInteger('12f', 1, 3));
  WriteLn('');
  WriteLn(GetInteger('aaa12bbb', 4, 5, val));
  WriteLn(val);
  WriteLn(GetInteger('aaa12bbb', 4, 5));
end;

procedure GetFloatTest();
var val:double;

begin
  WriteLn(GetFloat('1  2 . 7   8', 1, 12, val));
  WriteLn(val);
  WriteLn(GetFloat('1  2 . 7   8', 1, 12));
  WriteLn('');
  WriteLn(GetFloat('12.78f', 1, 6, val));
  WriteLn(val);
  WriteLn(GetFloat('12.78f', 1, 6));
  WriteLn('');
  WriteLn(GetFloat('aaa12.78bbb', 4, 8, val));
  WriteLn(val);
  WriteLn(GetFloat('aaa12.78bbb', 4, 8));
  WriteLn('');
  WriteLn(GetFloat('12', 1, 2, val));
  WriteLn(val);
  WriteLn(GetFloat('12', 1, 2));
  WriteLn('');
  WriteLn(GetFloat('12,78', 1, 5, val));
  WriteLn(val);
  WriteLn(GetFloat('12,78', 1, 5));
end;

procedure GetStringTest();
begin
  WriteLn(GetString('ban ana', 3, 5));
end;

procedure LastIndexOfTest();
var subs,s1:string; ts:TSimpleStrings; ti:TIntegers;

begin
  AddToArray(3, ti);
  AddToArray(2, ti);
  AddToArray(5, ti);
  AddToArray(2, ti);
  AddToArray(4, ti);
  WriteLn(LastIndexOf(2, ti));
  AddToArray('banana', ts);
  AddToArray('mela', ts);
  AddToArray('pera', ts);
  AddToArray('mela', ts);
  AddToArray('fico', ts);
  WriteLn(LastIndexOf('mela', ts));
  // Secondo me questa funzione restituisce il risultato sbagliato, perché la
  // lunghezza della stringa di esempio è 27 e la posizione restituita è 23,
  // quando tra la g di gatto e la fine della stringa ci sono altri 5 caratteri
  s1:='Il gatto è sopra il gatto.';
  subs:='gatto';
  WriteLn(IntToStr(LastIndexOf(subs, s1)) + ' total length: ' + IntToStr(s1.Length));
  // Qui succedono due errori: innanzitutto la parola viene individuata
  // anche se non c'è dentro a s1, soltanto perché l'iniziale è identica. Inoltre
  // l'indice è diminuito di 1 rispetto a prima, perché viene considerata la
  // lunghezza della parola di subs, che sarebbe corretto se il controllo
  // funzionasse correttamente
  subs:='garage';
  WriteLn(LastIndexOf(subs, s1));
end;

procedure FirstIndexOfTest();
begin

end;

procedure FirstByPrefixTest();
begin

end;

procedure FirstContainingTest();
begin

end;

procedure LookupByPrefix();
begin

end;

procedure DeblankTest();
begin
  WriteLn(Deblank(' ban ana ') + 'f')
end;

procedure TrimmedBlanksTest();
begin

end;

procedure StringAsArrayTest();
begin

end;

procedure SaveToFileTest();
begin

end;

procedure FlattenStringTest();
begin

end;

procedure SnipStringTest();
begin

end;

procedure SnipStringAllTest();
begin

end;

procedure CountInStringTest();
begin

end;

procedure CleanStringTest();
begin

end;

procedure AppendToLengthTest();
begin

end;

procedure FixLineBreaksTest();
begin

end;

procedure UncasedCompareTest();
begin

end;

procedure AsSimpleStringTest();
begin

end;

procedure CopyStringTest();
begin

end;

procedure AppendToStringList();
begin

end;

procedure CleanFileNameTest();
begin

end;

procedure CheckHexTest();
begin

end;

procedure ReplaceHexCodesTest();
begin

end;

procedure PosUncasedTest();
begin

end;

procedure TagStringTest();
begin

end;

procedure GetTaggedFieldTest();
begin

end;

procedure GetTaggedFieldsTest();
begin

end;

procedure DecodeQPTest();
begin

end;

procedure ConvertHtmlTest();
begin

end;

procedure HtmlToAsciiTest();
begin

end;

procedure BasicASCIITest();
begin

end;

procedure ReadAsSimpleStringTest();
begin

end;

procedure ReadKeysAsValuesTest();
begin

end;

procedure RightJustifyTest();
begin
  WriteLn(RightJustify(126356, 5));
  WriteLn(RightJustify(126356, 8));
  WriteLn(RightJustify(1263.56, 5, 1));
  WriteLn(RightJustify(1263.56, 8, 1));
  WriteLn(RightJustify(1263.56, 5, 3));
  WriteLn(RightJustify(1263.56, 8, 3));
  WriteLn(RightJustify(1263.56, 5, 5));
  WriteLn(RightJustify(1263.56, 8, 5));
end;

procedure LeftJustifyTest();
begin
  WriteLn(LeftJustify('banana', 4)+'f');
  WriteLn(LeftJustify('banana', 8)+'f');
end;

end.

