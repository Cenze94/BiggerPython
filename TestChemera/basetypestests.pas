unit basetypestests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basetypes;

  procedure StartBasetypesTest();
  procedure AddToArrayTest();
  procedure ForceNotZeroTest();
  procedure PushIntoArrayTest();
  procedure RemoveFromArrayTest();
  procedure ConcatenateTest();
  procedure SliceTest();
  procedure CountInArrayTest();
  procedure IndexOfTest();
  procedure IsInArrayTest();
  procedure AddUniqueToArrayTest();
  procedure AppendToTest();
  procedure MinTest();
  procedure MaxTest();
  procedure MinIxTest();
  procedure MaxIxTest();
  procedure SumTest();
  procedure MinValIxTest();
  procedure MaxValIxTest();
  procedure CoordTest();
  procedure StringsToFloatsTest();
  procedure FilledIntsTest();
  procedure FilledFloatsTest();
  procedure IsEqualTest();
  procedure StringToFloatTest();
  procedure ScaleMatrixTest();
  procedure AddMatricesTest();
  procedure StringToFloatsTest();
  procedure InContactTest();
  procedure AverageTest();
  procedure MedianTest();
  procedure VarianceTest();
  procedure IsBetweenTest();
  procedure GetTickCountTest();
  procedure GetTimeIntervalTest();

implementation

procedure StartBasetypesTest();
begin
  // AddToArrayTest();
  // ForceNotZeroTest();
  // PushIntoArrayTest();
  // RemoveFromArrayTest();
  // ConcatenateTest();
  // SliceTest();
  // CountInArrayTest();
  // IndexOfTest();
  // IsInArrayTest();
  // AddUniqueToArrayTest();
  // AppendToTest();
  // MinTest();
  // MaxTest();
  // MinIxTest();
  // MaxIxTest();
  // SumTest();
  // MinValIxTest();
  // MaxValIxTest();
  CoordTest();
  // StringsToFloatsTest();
  // IsEqualTest();
  // StringToFloatTest();
  // ScaleMatrixTest();
  // AddMatricesTest();
  // StringToFloatsTest();
  // ScaleMatrixTest();
  // AddMatricesTest();
  // StringToFloatsTest();
  // InContactTest();
  // AverageTest();
  // MedianTest();
  // VarianceTest();
  // IsBetweenTest();
  // GetTickCountTest();
  // GetTimeIntervalTest();
end;

procedure AddToArrayTest();
var a:TSimpleStrings; c:TCoord; ca:TCoords; x:Integer;

begin
  for x:=0 to 10 do
  begin
    AddToArray('banana' + IntToStr(x), a);
    WriteLn(a[x]);
  end;
  for x:=0 to 10 do
  begin
    c[0]:=x;
    c[1]:=x;
    c[2]:=x;
    AddToArray(c, ca);
    WriteLn(FloatToStr(ca[x][0])+' '+FloatToStr(ca[x][1])+' '+
            FloatToStr(ca[x][2]));
  end;

end;

procedure ForceNotZeroTest();
var v:TFloat;

begin
  v := 0.0000000000000001;
  WriteLn(FloatToStr(v));
  ForceNotZero(v);
  WriteLn(FloatToStr(v));
end;

procedure PushIntoArrayTest();
var ti:TIntegers; ts:TSimpleStrings; x:integer;

begin
  PushIntoArray(123, ti);
  PushIntoArray(45, ti);
  PushIntoArray(7, ti);
  for x:=0 to High(ti) do
  begin
    WriteLn(FloatToStr(ti[x]));
  end;

  PushIntoArray('banana', ts);
  PushIntoArray('mela', ts);
  PushIntoArray('pera', ts);
  for x:=0 to High(ts) do
  begin
    WriteLn(ts[x]);
  end;
end;

procedure RemoveFromArrayTest();
var ai:TIntegers; af:TFloats; ac:TCardinals; aco:TCoords; ass:TSimpleStrings;
  Ixs, A:TIntegers; x:integer; c:TCoord;

begin
  for x:=0 to 10 do
  begin
    AddToArray(x, ai);
    AddToArray(x + 0.01, af);
    AddToArray(x, ac);
    c[0]:=x;
    c[1]:=x;
    c[2]:=x;
    AddToArray(c, aco);
    AddToArray('banana' + IntToStr(x), ass);
  end;
  RemoveFromArray(4, ai);
  RemoveFromArray(5, af);
  RemoveFromArray(6, ac);
  RemoveFromArray(7, aco);
  RemoveFromArray(8, ass);
  for x:=0 to High(ai) do
  begin
    WriteLn(ai[x]);
  end;
  for x:=0 to High(af) do
  begin
    WriteLn(af[x]);
  end;
  for x:=0 to High(ac) do
  begin
    WriteLn(ac[x]);
  end;
  for x:=0 to High(aco) do
  begin
    WriteLn(FloatToStr(aco[x][0])+' '+FloatToStr(aco[x][1])+' '+
            FloatToStr(aco[x][2]));
  end;
  for x:=0 to High(ass) do
  begin
    WriteLn(ass[x]);
  end;
  WriteLn('');

  for x:=0 to 10 do
  begin
    AddToArray(x, A);
  end;
  AddToArray(5, Ixs);
  AddToArray(6, Ixs);
  AddToArray(7, Ixs);
  AddToArray(2, Ixs);
  AddToArray(11, Ixs);
  RemoveFromArray(Ixs, A);
  for x:=0 to High(A) do
  begin
    WriteLn(A[x]);
  end;
end;

procedure ConcatenateTest();
var a1, a2:TCoords; a:Tcoord; s1, s2:TSimpleStrings; x:integer;

begin
  for x:=0 to 4 do
  begin
    a[0]:=x;
    a[1]:=x;
    a[2]:=x;
    AddToArray(a, a1);
  end;
  for x:=8 to 10 do
  begin
    a[0]:=x;
    a[1]:=x;
    a[2]:=x;
    AddToArray(a, a2);
  end;
  a1 := Concatenate(a1, a2);
  for x:=0 to High(a1) do
  begin
    WriteLn(FloatToStr(a1[x][0])+' '+FloatToStr(a1[x][1])+' '+
            FloatToStr(a1[x][2]));
  end;
  AddToArray('ferro', s1);
  AddToArray('calcio', s1);
  AddToArray('argon', s1);
  AddToArray('banana', s2);
  AddToArray('mela', s2);
  AddToArray('pera', s2);
  AddToArray('kiwi', s2);
  AddToArray('ananas', s2);
  s1 := Concatenate(s1, s2);
  for x:=0 to High(s1) do
  begin
    WriteLn(s1[x]);
  end;
end;

procedure SliceTest();
var Ints, v:TIntegers; Coords:TCoords; c:TCoord; x:integer;

begin
  AddToArray(5, Ints);
  AddToArray(7, Ints);
  AddToArray(8, Ints);
  AddToArray(11, Ints);
  AddToArray(2, Ints);
  AddToArray(9, Ints);
  Ints:=Slice(Ints, 2, 4);
  for x:=0 to High(Ints) do
  begin
    WriteLn(Ints[x]);
  end;
  for x:=0 to 5 do
  begin
    c[0]:=x;
    c[1]:=x;
    c[2]:=x;
    AddToArray(c, Coords);
  end;
  AddToArray(2, v);
  AddToArray(3, v);
  AddToArray(5, v);
  Coords:=Slice(Coords, v);
  for x:=0 to High(Coords) do
  begin
    WriteLn(FloatToStr(Coords[x][0])+' '+FloatToStr(Coords[x][1])+' '+
            FloatToStr(Coords[x][2]));
  end;
end;

procedure CountInArrayTest();
var Ints:TIntegers;

begin
  AddToArray(5, Ints);
  AddToArray(3, Ints);
  AddToArray(4, Ints);
  AddToArray(5, Ints);
  AddToArray(5, Ints);
  AddToArray(1, Ints);
  WriteLn(CountInArray(5, Ints));
end;

procedure IndexOfTest();
var a:TIntegers; s:TSimpleStrings;

begin
  AddToArray(5, a);
  AddToArray(3, a);
  AddToArray(4, a);
  AddToArray(5, a);
  AddToArray(5, a);
  AddToArray(1, a);
  WriteLn(IndexOf(4, a));
  AddToArray('banana', s);
  AddToArray('kiwi', s);
  AddToArray('pera', s);
  AddToArray('mela', s);
  AddToArray('ananas', s);
  AddToArray('anguria', s);
  WriteLn(IndexOf('mela', s));
end;

procedure IsInArrayTest();
var a:TIntegers; s:TSimpleStrings;

begin
  AddToArray(5, a);
  AddToArray(3, a);
  AddToArray(4, a);
  AddToArray(5, a);
  AddToArray(5, a);
  AddToArray(1, a);
  WriteLn(IsInArray(4, a));
  AddToArray('banana', s);
  AddToArray('kiwi', s);
  AddToArray('pera', s);
  AddToArray('mela', s);
  AddToArray('ananas', s);
  AddToArray('anguria', s);
  WriteLn(IsInArray('pela', s));
end;

procedure AddUniqueToArrayTest();
begin

end;

procedure AppendToTest();
begin

end;

procedure MinTest();
begin

end;

procedure MaxTest();
begin

end;

procedure MinIxTest();
begin

end;

procedure MaxIxTest();
begin

end;

procedure SumTest();
begin

end;

procedure MinValIxTest();
begin

end;

procedure MaxValIxTest();
begin

end;

procedure CoordTest();
var c:TCoord;

begin
  c:=Coord(5, 6, 7);
  WriteLn(FloatToStr(c[0])+' '+FloatToStr(c[1])+' '+
            FloatToStr(c[2]));
end;

procedure StringsToFloatsTest();
begin

end;

procedure FilledIntsTest();
begin

end;

procedure FilledFloatsTest();
begin

end;

procedure IsEqualTest();
begin

end;

procedure StringToFloatTest();
begin

end;

procedure ScaleMatrixTest();
begin

end;

procedure AddMatricesTest();
begin

end;

procedure StringToFloatsTest();
begin

end;

procedure InContactTest();
begin

end;

procedure AverageTest();
begin

end;

procedure MedianTest();
begin

end;

procedure VarianceTest();
begin

end;

procedure IsBetweenTest();
begin

end;

procedure GetTickCountTest();
begin

end;

procedure GetTimeIntervalTest();
begin

end;

end.

