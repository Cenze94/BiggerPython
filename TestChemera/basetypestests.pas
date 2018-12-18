unit basetypestests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basetypes, Crt;

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
  // CoordTest();
  // StringsToFloatsTest();
  // FilledIntsTest();
  // FilledFloatsTest();
  // IsEqualTest();
  // StringToFloatTest();
  // ScaleMatrixTest();
  // AddMatricesTest();
  StringToFloatsTest();
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
var fa:TFloats; ia:TIntegers; ca:TCoords; c,c2:TCoord; f,i:Integer;
  tf,tf2:TFloat; m:TMatrix;

begin
  SetLength(fa, 3);
  fa[0]:=4.3;
  fa[1]:=9.1;
  fa[2]:=5;
  tf:=Min(fa);
  WriteLn(FloatToStr(tf));

  WriteLn('');
  SetLength(ia, 3);
  ia[0]:=4;
  ia[1]:=9;
  ia[2]:=5;
  f:=Min(ia);
  WriteLn(IntToStr(f));

  WriteLn('');
  SetLength(ca, 3);
  c[0]:=3;
  c[1]:=7;
  c[2]:=4;
  ca[0]:=c;
  c[0]:=5;
  c[1]:=1;
  c[2]:=6;
  ca[1]:=c;
  c[0]:=2;
  c[1]:=2;
  c[2]:=9;
  ca[2]:=c;
  c:=Min(ca);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));

  WriteLn('');
  SetLength(m, 3);
  SetLength(m[0], 3);
  SetLength(m[1], 3);
  SetLength(m[2], 3);
  m[0,0]:=5;
  m[0,1]:=2;
  m[0,2]:=7;
  m[1,0]:=4;
  m[1,1]:=1;
  m[1,2]:=9;
  m[2,0]:=3;
  m[2,1]:=7;
  m[2,2]:=3;
  tf:=Min(m);
  WriteLn(FloatToStr(tf));

  WriteLn('');
  f:=4;
  i:=5;
  f:=Min(f, i);
  WriteLn(IntToStr(f));

  WriteLn('');
  tf:=4.5;
  tf2:=5.1;
  tf:=Min(tf, tf2);
  WriteLn(FloatToStr(tf));

  WriteLn('');
  c[0]:=4;
  c[1]:=5;
  c[2]:=9;
  c2[0]:=5;
  c2[1]:=7;
  c2[2]:=6;
  c:=Min(c, c2);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));
end;

procedure MaxTest();
var fa:TFloats; ia:TIntegers; ca:TCoords; c,c2:TCoord; f,i:Integer;
  tf,tf2:TFloat; m:TMatrix;

begin
  SetLength(fa, 3);
  fa[0]:=4.3;
  fa[1]:=9.1;
  fa[2]:=5;
  tf:=Max(fa);
  WriteLn(FloatToStr(tf));

  WriteLn('');
  SetLength(ia, 3);
  ia[0]:=4;
  ia[1]:=9;
  ia[2]:=5;
  f:=Max(ia);
  WriteLn(IntToStr(f));

  WriteLn('');
  SetLength(ca, 3);
  c[0]:=3;
  c[1]:=7;
  c[2]:=4;
  ca[0]:=c;
  c[0]:=5;
  c[1]:=1;
  c[2]:=6;
  ca[1]:=c;
  c[0]:=2;
  c[1]:=2;
  c[2]:=9;
  ca[2]:=c;
  c:=Max(ca);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));

  WriteLn('');
  SetLength(m, 3);
  SetLength(m[0], 3);
  SetLength(m[1], 3);
  SetLength(m[2], 3);
  m[0,0]:=5;
  m[0,1]:=2;
  m[0,2]:=7;
  m[1,0]:=4;
  m[1,1]:=1;
  m[1,2]:=9;
  m[2,0]:=3;
  m[2,1]:=7;
  m[2,2]:=3;
  tf:=Max(m);
  WriteLn(FloatToStr(tf));

  WriteLn('');
  f:=4;
  i:=5;
  f:=Max(f, i);
  WriteLn(IntToStr(f));

  WriteLn('');
  tf:=4.5;
  tf2:=5.1;
  tf:=Max(tf, tf2);
  WriteLn(FloatToStr(tf));

  WriteLn('');
  c[0]:=4;
  c[1]:=5;
  c[2]:=9;
  c2[0]:=5;
  c2[1]:=7;
  c2[2]:=6;
  c:=Max(c, c2);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));
end;

procedure MinIxTest();
begin

end;

procedure MaxIxTest();
begin

end;

procedure SumTest();
var fa1,fa2:TFloats; ia:TIntegers; ca:TCoords; c:TCoord; f:Integer; tf:TFloat;

begin
  SetLength(fa1, 3);
  SetLength(fa2, 4);
  fa1[0]:=4.3;
  fa1[1]:=9.1;
  fa1[2]:=5;
  fa2[0]:=7.4;
  fa2[1]:=8.9;
  fa2[2]:=9;
  fa2[3]:=1.2;
  fa1:=Sum(fa1, fa2);
  for f:=0 to High(fa1) do
  begin
    WriteLn(FloatToStr(fa1[f]));
  end;

  WriteLn('');
  tf:=Sum(fa2);
  WriteLn(FloatToStr(tf));

  WriteLn('');
  SetLength(ia, 3);
  ia[0]:=4;
  ia[1]:=9;
  ia[2]:=5;
  f:=Sum(ia);
  WriteLn(IntToStr(f));

  WriteLn('');
  SetLength(ca, 3);
  c[0]:=3;
  c[1]:=7;
  c[2]:=4;
  ca[0]:=c;
  c[0]:=5;
  c[1]:=1;
  c[2]:=6;
  ca[1]:=c;
  c[0]:=2;
  c[1]:=2;
  c[2]:=1;
  ca[2]:=c;
  c:=Sum(ca);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));
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
var ia:TIntegers; f:Integer;

begin
  ia:=FilledInts(5, 2);
  for f:=0 to High(ia) do
  begin
    WriteLn(IntToStr(ia[f]));
  end;
end;

procedure FilledFloatsTest();
var fa:TFloats; f:Integer;

begin
  fa:=FilledFloats(5, 3.7);
  for f:=0 to High(fa) do
  begin
    WriteLn(FloatToStr(fa[f]));
  end;
end;

procedure IsEqualTest();
begin

end;

procedure StringToFloatTest();
begin
  WriteLn(FloatToStr(StringToFloat('6.57')));
  WriteLn(FloatToStr(StringToFloat('348,532')));
end;

procedure ScaleMatrixTest();
begin

end;

procedure AddMatricesTest();
begin

end;

procedure StringToFloatsTest();
var fa:TFloats; f:Integer;

begin
  fa:=StringToFloats('6.54 56.93              8.74 12,7645');
  for f:=0 to High(fa) do
  begin
    WriteLn(FloatToStr(fa[f]));
  end;
end;

procedure InContactTest();
var c:TCoord; c1,c2:TCuboid;

begin
  c[0]:=3;
  c[1]:=6;
  c[2]:=9;
  c1[0]:=c;
  c[0]:=1;
  c[1]:=5;
  c[2]:=7;
  c1[1]:=c;
  c[0]:=6;
  c[1]:=7;
  c[2]:=3;
  c2[0]:=c;
  c[0]:=7;
  c[1]:=3;
  c[2]:=2;
  c2[1]:=c;
  WriteLn(BoolToStr(InContact(c1, c2))); // 0 è False, -1 è True
  c[0]:=9;
  c[1]:=9;
  c[2]:=9;
  c2[1]:=c;
  c1[1]:=c;
  WriteLn(BoolToStr(InContact(c1, c2)));
end;

procedure AverageTest();
var fa:TFloats; f:TFloat; ia:TIntegers;

begin
  SetLength(fa, 5);
  fa[0]:=3.2;
  fa[1]:=4.1;
  fa[2]:=6.8;
  fa[3]:=2.9;
  fa[4]:=9.4;
  f:=Average(fa);
  WriteLn(FloatToStr(f));
  SetLength(ia, 5);
  ia[0]:=3;
  ia[1]:=4;
  ia[2]:=6;
  ia[3]:=2;
  ia[4]:=9;
  f:=Average(ia);
  WriteLn(FloatToStr(f));
end;

procedure MedianTest();
var fa:TFloats; f:TFloat; ia:TIntegers; i:Integer;

begin
  SetLength(fa, 5);
  fa[0]:=3.2;
  fa[1]:=4.1;
  fa[2]:=6.8;
  fa[3]:=2.9;
  fa[4]:=9.4;
  f:=Median(fa);
  WriteLn(FloatToStr(f));
  SetLength(ia, 5);
  ia[0]:=3;
  ia[1]:=4;
  ia[2]:=6;
  ia[3]:=2;
  ia[4]:=9;
  i:=Median(ia);
  WriteLn(IntToStr(i));
end;

procedure VarianceTest();
var fa:TFloats; v:TFloat;

begin
  SetLength(fa, 3);
  fa[0]:=5.4;
  fa[1]:=3.8;
  fa[2]:=6.5;
  v:=Variance(fa, 5.5);
  WriteLn(FloatToStr(v));
end;

procedure IsBetweenTest();
begin
  WriteLn(BoolToStr(IsBetween(6.4, 5.8, 7.1))); // -1 è true, 0 è false
  WriteLn(BoolToStr(IsBetween(5.4, 5.8, 5.1)));
  WriteLn(BoolToStr(IsBetween(7.2, 4.7, 6.7)));
  WriteLn(BoolToStr(IsBetween(3.2, 4.7, 6.7)));
end;

procedure GetTickCountTest();
var f:Integer;

begin
  f:=GetTickCount();
  WriteLn(IntToStr(f));
end;

procedure GetTimeIntervalTest();
var s,d:DWORD;

begin
  s:=GetTickCount();
  Delay(5000);
  d:=GetTimeInteval(s);
  WriteLn(IntToStr(s) + ' ' + IntToStr(d));
end;

end.

