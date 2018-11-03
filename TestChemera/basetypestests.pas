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
  AddToArrayTest();
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
begin

end;

procedure PushIntoArrayTest();
begin

end;

procedure RemoveFromArrayTest();
begin

end;

procedure ConcatenateTest();
begin

end;

procedure SliceTest();
begin

end;

procedure CountInArrayTest();
begin

end;

procedure IndexOfTest();
begin

end;

procedure IsInArrayTest();
begin

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
begin

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

