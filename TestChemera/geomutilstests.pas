unit geomutilstests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, geomutils, basetypes;

  procedure StartGeomutilsTest();
  procedure QuaternionTest();
  procedure AddTest();
  procedure SubtractTest();
  procedure MultiplyTest();
  procedure DotProductTest();
  procedure CrossProductTest();
  procedure NormTest();
  procedure ConjugatedTest();
  procedure NormalizeTest();
  procedure ScaledTest();
  procedure SimmetricTest();
  procedure DistanceTest();
  procedure DistanceSquaredTest();
  procedure MidPointTest();
  procedure RotateTest();
  procedure RotationQuaternionTest();
  procedure RotationToTest();
  procedure StaticRMSDTest();
  procedure Intersection2DTest();
  procedure DistanceToLine2DTest();
  procedure DistanceToSegment2DTest();
  procedure DistanceToNormalizedAxisTest();
  procedure OrthogonalCoordsTest();
  procedure RotAndPlaceTest();
  procedure BuildRotationTest();
  procedure XRotationTest();
  procedure YRotationTest();
  procedure ZRotationTest();
  procedure InvertBaseTest();
  procedure BuildBaseTest();

implementation

procedure StartGeomutilsTest();
begin
  // QuaternionTest();
  // AddTest();
  // SubtractTest();
  // MultiplyTest();
  // DotProductTest();
  // CrossProductTest();
  // NormTest();
  // ConjugatedTest();
  // NormalizeTest();
  // ScaledTest();
  // SimmetricTest();
  // DistanceTest();
  // DistanceSquaredTest();
  MidPointTest();
  // RotateTest();
  // RotationQuaternionTest();
  // RotationToTest();
  // StaticRMSDTest();
  // Intersection2DTest();
  // DistanceToLine2DTest();
  // DistanceToSegment2DTest();
  // DistanceToNormalizedAxisTest();
  // OrthogonalCoordsTest();
  // RotAndPlaceTest();
  // BuildRotationTest();
  // XRotationTest();
  // YRotationTest();
  // ZRotationTest();
  // InvertBaseTest();
  // BuildBaseTest();
end;

procedure QuaternionTest();
var q:TQuaternion;

begin
  q:=Quaternion(0, 1, 2, 3);
  WriteLn(FloatToStr(q[0]) + ' ' + FloatToStr(q[1]) + ' ' + FloatToStr(q[2]) +
  ' ' + FloatToStr(q[3]));
end;

procedure AddTest();
var c,c2:TCoord; ca:TCoords; fa:TFloats; f:Integer;

begin
  c[0]:=1;
  c[1]:=2;
  c[2]:=3;
  c:=Add(c, 2.0);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));

  c[0]:=1;
  c[1]:=2;
  c[2]:=3;
  c2[0]:=2;
  c2[1]:=3;
  c2[2]:=4;
  c:=Add(c, c2);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));

  WriteLn('');
  SetLength(ca, 3);
  c[0]:=3;
  c[1]:=8;
  c[2]:=1;
  ca[0]:=c;
  c[0]:=6;
  c[1]:=4;
  c[2]:=6;
  ca[1]:=c;
  c[0]:=0;
  c[1]:=2;
  c[2]:=5;
  ca[2]:=c;
  c[0]:=5;
  c[1]:=1;
  c[2]:=3;
  ca:=Add(c, ca);
  for f:=0 to High(ca) do
  begin
    WriteLn(FloatToStr(ca[f][0]) + ' ' + FloatToStr(ca[f][1]) + ' ' +
    FloatToStr(ca[f][2]));
  end;

  WriteLn('');
  SetLength(fa, 3);
  fa[0]:=3;
  fa[1]:=6;
  fa[2]:=4;
  fa:=Add(fa, 2);
  for f:=0 to High(fa) do
  begin
    WriteLn(fa[f]);
  end;

  WriteLn('');
  c[0]:=3;
  c[1]:=8;
  c[2]:=1;
  ca[0]:=c;
  c[0]:=6;
  c[1]:=4;
  c[2]:=6;
  ca[1]:=c;
  c[0]:=0;
  c[1]:=2;
  c[2]:=5;
  ca[2]:=c;
  fa[0]:=3;
  fa[1]:=6;
  fa[2]:=4;
  ca:=Add(ca, fa);
  for f:=0 to High(ca) do
  begin
    WriteLn(FloatToStr(ca[f][0]) + ' ' + FloatToStr(ca[f][1]) + ' ' +
    FloatToStr(ca[f][2]));
  end;
end;

procedure SubtractTest();
var c,c2:TCoord; ca:TCoords; f:Integer;

begin
  c[0]:=0;
  c[1]:=1;
  c[2]:=2;
  c:=Subtract(c, 1);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));

  c[0]:=6;
  c[1]:=3;
  c[2]:=9;
  c2[0]:=3;
  c2[1]:=5;
  c2[2]:=1;
  c:=Subtract(c, c2);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));

  WriteLn('');
  SetLength(ca, 3);
  c[0]:=3;
  c[1]:=8;
  c[2]:=1;
  ca[0]:=c;
  c[0]:=6;
  c[1]:=4;
  c[2]:=6;
  ca[1]:=c;
  c[0]:=0;
  c[1]:=2;
  c[2]:=5;
  ca[2]:=c;
  c[0]:=5;
  c[1]:=1;
  c[2]:=3;
  ca:=Subtract(ca, c);
  for f:=0 to High(ca) do
  begin
    WriteLn(FloatToStr(ca[f][0]) + ' ' + FloatToStr(ca[f][1]) + ' ' +
    FloatToStr(ca[f][2]));
  end;
end;

procedure MultiplyTest();
var c:TCoord; fa:TFloats; ca:TCoords; q1,q2:TQuaternion; r1,r2:TRotMatrix;
  f:Integer;

begin
  c[0]:=0;
  c[1]:=1;
  c[2]:=2;
  c:=Multiply(c, 2);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));

  WriteLn('');
  SetLength(fa, 3);
  fa[0]:=3;
  fa[1]:=6;
  fa[2]:=4;
  fa:=Multiply(fa, 2);
  for f:=0 to High(fa) do
  begin
    WriteLn(fa[f]);
  end;

  WriteLn('');
  SetLength(ca, 3);
  c[0]:=3;
  c[1]:=8;
  c[2]:=1;
  ca[0]:=c;
  c[0]:=6;
  c[1]:=4;
  c[2]:=6;
  ca[1]:=c;
  c[0]:=0;
  c[1]:=2;
  c[2]:=5;
  ca[2]:=c;
  ca:=Multiply(ca, 2);
  for f:=0 to High(ca) do
  begin
    WriteLn(FloatToStr(ca[f][0]) + ' ' + FloatToStr(ca[f][1]) + ' ' +
    FloatToStr(ca[f][2]));
  end;

  WriteLn('');
  q1[0]:=2;
  q1[1]:=4;
  q1[2]:=3;
  q1[3]:=5;
  q2[0]:=1;
  q2[1]:=3;
  q2[2]:=5;
  q2[3]:=2;
  q1:=Multiply(q1, q2);
  WriteLn(FloatToStr(q1[0]) + ' ' + FloatToStr(q1[1]) + ' ' + FloatToStr(q1[2])
  + ' ' + FloatToStr(q1[3]));

  WriteLn('');
  r1[0][0]:=2;
  r1[0][1]:=4;
  r1[0][2]:=5;
  r1[1][0]:=3;
  r1[1][1]:=1;
  r1[1][2]:=1;
  r1[2][0]:=3;
  r1[2][1]:=2;
  r1[2][2]:=2;
  r2[0][0]:=3;
  r2[0][1]:=1;
  r2[0][2]:=1;
  r2[1][0]:=2;
  r2[1][1]:=5;
  r2[1][2]:=5;
  r2[2][0]:=4;
  r2[2][1]:=2;
  r2[2][2]:=3;
  r1:=Multiply(r1, r2);
  WriteLn(FloatToStr(r1[0][0]) + ' ' + FloatToStr(r1[0][1]) + ' ' +
  FloatToStr(r1[0][2]));
  WriteLn(FloatToStr(r1[1][0]) + ' ' + FloatToStr(r1[1][1]) + ' ' +
  FloatToStr(r1[1][2]));
  WriteLn(FloatToStr(r1[2][0]) + ' ' + FloatToStr(r1[2][1]) + ' ' +
  FloatToStr(r1[2][2]));
end;

procedure DotProductTest();
begin

end;

procedure CrossProductTest();
begin

end;

procedure NormTest();
begin

end;

procedure ConjugatedTest();
var q:TQuaternion;

begin
  q[0]:=1;
  q[1]:=2;
  q[2]:=3;
  q[3]:=4;
  q:=Conjugated(q);
  WriteLn(FloatToStr(q[0]) + ' ' + FloatToStr(q[1]) + ' ' + FloatToStr(q[2])
  + ' ' + FloatToStr(q[3]));
end;

procedure NormalizeTest();
begin

end;

procedure ScaledTest();
begin

end;

procedure SimmetricTest();
var c:TCoord;

begin
  c[0]:=3;
  c[1]:=8;
  c[2]:=1;
  c:=Simmetric(c);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));
end;

procedure DistanceTest();
var c,c1:TCoord; ca,ca1:TCoords; q1,q2:TQuaternion; d:TFLoat; da:TFloats;
  f:Integer;

begin
  c[0]:=3;
  c[1]:=8;
  c[2]:=1;
  c1[0]:=4;
  c1[1]:=5;
  c1[2]:=2;
  d:=Distance(c, c1);
  WriteLn(d);

  WriteLn('');
  SetLength(ca, 3);
  SetLength(ca1, 3);
  c[0]:=5;
  c[1]:=1;
  c[2]:=1;
  ca[0]:=c;
  c[0]:=4;
  c[1]:=6;
  c[2]:=8;
  ca[1]:=c;
  c[0]:=1;
  c[1]:=2;
  c[2]:=1;
  ca[2]:=c;
  c[0]:=4;
  c[1]:=3;
  c[2]:=5;
  ca1[0]:=c;
  c[0]:=2;
  c[1]:=0;
  c[2]:=6;
  ca1[1]:=c;
  c[0]:=7;
  c[1]:=3;
  c[2]:=6;
  ca1[2]:=c;
  da:=Distance(ca, ca1);
  for f:=0 to High(da) do
  begin
    WriteLn(da[f]);
  end;

  WriteLn('');
  q1[0]:=4;
  q1[1]:=2;
  q1[2]:=1;
  q1[3]:=3;
  q2[0]:=2;
  q2[1]:=2;
  q2[2]:=3;
  q2[3]:=1;
  d:=Distance(q1, q2);
  WriteLn(d);
end;

procedure DistanceSquaredTest();
begin

end;

procedure MidPointTest();
var ca:TCoords; c:TCoord;

begin
  SetLength(ca, 3);
  c[0]:=4;
  c[1]:=6;
  c[2]:=9;
  ca[0]:=c;
  c[0]:=1;
  c[1]:=4;
  c[2]:=1;
  ca[1]:=c;
  c[0]:=3;
  c[1]:=8;
  c[2]:=4;
  ca[2]:=c;
  c:=MidPoint(ca);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));
end;

procedure RotateTest();
var c:TCoord; ca:TCoords; r:TRotMatrix; q:TQuaternion; f:Integer;

begin
  c[0]:=1;
  c[1]:=2;
  c[2]:=3;
  r[0][0]:=4;
  r[0][1]:=3;
  r[0][2]:=1;
  r[1][0]:=5;
  r[1][1]:=2;
  r[1][2]:=3;
  r[2][0]:=5;
  r[2][1]:=3;
  r[2][2]:=1;
  c:=Rotate(c, r);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));

  WriteLn('');
  SetLength(ca, 3);
  c[0]:=3;
  c[1]:=8;
  c[2]:=1;
  ca[0]:=c;
  c[0]:=6;
  c[1]:=4;
  c[2]:=6;
  ca[1]:=c;
  c[0]:=0;
  c[1]:=2;
  c[2]:=5;
  ca[2]:=c;
  ca:=Rotate(ca, r);
  for f:=0 to High(ca) do
  begin
    WriteLn(FloatToStr(ca[f][0]) + ' ' + FloatToStr(ca[f][1]) + ' ' +
    FloatToStr(ca[f][2]));
  end;

  WriteLn('');
  q[0]:=2;
  q[1]:=3;
  q[2]:=1;
  q[3]:=4;
  c[0]:=1;
  c[1]:=2;
  c[2]:=3;
  c:=Rotate(c, q);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));

  WriteLn('');
  c[0]:=3;
  c[1]:=8;
  c[2]:=1;
  ca[0]:=c;
  c[0]:=6;
  c[1]:=4;
  c[2]:=6;
  ca[1]:=c;
  c[0]:=0;
  c[1]:=2;
  c[2]:=5;
  ca[2]:=c;
  ca:=Rotate(ca, q);
  for f:=0 to High(ca) do
  begin
    WriteLn(FloatToStr(ca[f][0]) + ' ' + FloatToStr(ca[f][1]) + ' ' +
    FloatToStr(ca[f][2]));
  end;
end;

procedure RotationQuaternionTest();
begin

end;

procedure RotationToTest();
begin

end;

procedure StaticRMSDTest();
begin

end;

procedure Intersection2DTest();
begin

end;

procedure DistanceToLine2DTest();
begin

end;

procedure DistanceToSegment2DTest();
begin

end;

procedure DistanceToNormalizedAxisTest();
begin

end;

procedure OrthogonalCoordsTest();
begin

end;

procedure RotAndPlaceTest();
begin

end;

procedure BuildRotationTest();
begin

end;

procedure XRotationTest();
begin

end;

procedure YRotationTest();
begin

end;

procedure ZRotationTest();
begin

end;

procedure InvertBaseTest();
begin

end;

procedure BuildBaseTest();
begin

end;

end.

