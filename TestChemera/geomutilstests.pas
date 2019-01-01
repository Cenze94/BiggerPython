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
  // MidPointTest();
  // RotateTest();
  RotationQuaternionTest();
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
var c1,c2:TCoord; f:TFloat;

begin
  c1[0]:=3.2;
  c1[1]:=8.6;
  c1[2]:=1.4;
  c2[0]:=4.0;
  c2[1]:=5.2;
  c2[2]:=2.5;
  f:=DotProduct(c1, c2);
  WriteLn(FloatToStr(f));
end;

procedure CrossProductTest();
var c1,c2:TCoord;

begin
  c1[0]:=3.2;
  c1[1]:=8.6;
  c1[2]:=1.4;
  c2[0]:=4.0;
  c2[1]:=5.2;
  c2[2]:=2.5;
  c1:=CrossProduct(c1, c2);
  WriteLn(FloatToStr(c1[0]) + ' ' + FloatToStr(c1[1]) + ' ' + FloatToStr(c1[2]));
end;

procedure NormTest();
var c:TCoord; f:TFloat;

begin
  c[0]:=0.7;
  c[1]:=2.2;
  c[2]:=5.8;
  f:=Norm(c);
  WriteLn(FloatToStr(f));
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
var c:TCoord; q:TQuaternion;

begin
  c[0]:=3.6;
  c[1]:=6.2;
  c[2]:=8.7;
  Normalize(c);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));

  q[0]:=2.7;
  q[1]:=7.2;
  q[2]:=6.4;
  q[3]:=7.0;
  Normalize(q);
  WriteLn(FloatToStr(q[0]) + ' ' + FloatToStr(q[1]) + ' ' + FloatToStr(q[2]) +
  ' ' + FloatToStr(q[3]));
end;

procedure ScaledTest();
var c:TCoord;

begin
  c[0]:=3.6;
  c[1]:=6.2;
  c[2]:=8.7;
  c:=Scaled(c, 3.5);
  WriteLn(FloatToStr(c[0]) + ' ' + FloatToStr(c[1]) + ' ' + FloatToStr(c[2]));
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
var c:TCoord; q:TQuaternion;

begin
  c[0]:=4.7;
  c[1]:=2.7;
  c[2]:=7.3;
  q:=RotationQuaternion(c, 3.5);
  WriteLn(FloatToStr(q[0]) + ' ' + FloatToStr(q[1]) + ' ' + FloatToStr(q[2]) +
  ' ' + FloatToStr(q[3]));
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
var c1,c2:TCoord; f:TFloat;

begin
  c1[0]:=3.6;
  c1[1]:=6.4;
  c1[2]:=2.5;
  c2[0]:=4.7;
  c2[1]:=2.2;
  c2[2]:=0.4;
  f:=DistanceToNormalizedAxis(c1, c2);
  WriteLn(FloatToStr(f));
end;

procedure OrthogonalCoordsTest();
var c1,c2:TCoord; x,y:TFloat;

begin
  c1[0]:=3.6;
  c1[1]:=6.4;
  c1[2]:=2.5;
  c2[0]:=4.7;
  c2[1]:=2.2;
  c2[2]:=0.4;
  OrthogonalCoords(c1, c2, x, y);
  WriteLn(FloatToStr(x) + ' ' + FloatToStr(y));
end;

procedure RotAndPlaceTest();
var m:TRotMatrix; p,v:TCoord; va:TCoords; f:Integer;

begin
  m[0, 0]:=5.6;
  m[0, 1]:=4.0;
  m[0, 2]:=8.2;
  m[1, 0]:=5.8;
  m[1, 1]:=3.3;
  m[1, 2]:=6.2;
  m[2, 0]:=7.4;
  m[2, 1]:=9.1;
  m[2, 2]:=1.3;
  p[0]:=3.6;
  p[1]:=6.4;
  p[2]:=2.5;
  v[0]:=4.7;
  v[1]:=2.2;
  v[2]:=0.4;
  v:=RotAndPlace(m, p, v);
  WriteLn(FloatToStr(v[0]) + ' ' + FloatToStr(v[1]) + ' ' + FloatToStr(v[2]));

  WriteLn('');
  SetLength(va, 3);
  va[0][0]:=4.8;
  va[0][1]:=4.1;
  va[0][2]:=6.0;
  va[1][0]:=0.6;
  va[1][1]:=3.8;
  va[1][2]:=7.2;
  va[2][0]:=4.1;
  va[2][1]:=6.6;
  va[2][2]:=8.0;
  RotAndPlace(m, p, va);
  for f:=0 to High(va) do
  begin
    WriteLn(FloatToStr(va[f][0]) + ' ' + FloatToStr(va[f][1]) + ' ' +
    FloatToStr(va[f][2]));
  end;
end;

procedure BuildRotationTest();
var c:TCoord; m:TRotMatrix; f:Integer;

begin
  c[0]:=4.8;
  c[1]:=0.2;
  c[2]:=7.2;
  m:=BuildRotation(c, MCXYZRotation);
  for f:=0 to 2 do
  begin
    WriteLn(FloatToStr(m[f][0]) + ' ' + FloatToStr(m[f][1]) + ' ' +
    FloatToStr(m[f][2]));
    WriteLn('');
  end;
  WriteLn('');
  m:=BuildRotation(c, MCRotationXYAxis);
  for f:=0 to 2 do
  begin
    WriteLn(FloatToStr(m[f][0]) + ' ' + FloatToStr(m[f][1]) + ' ' +
    FloatToStr(m[f][2]));
    WriteLn('');
  end;
  WriteLn('');
  m:=BuildRotation(c, 2);
  for f:=0 to 2 do
  begin
    WriteLn(FloatToStr(m[f][0]) + ' ' + FloatToStr(m[f][1]) + ' ' +
    FloatToStr(m[f][2]));
    WriteLn('');
  end;
end;

procedure XRotationTest();
var m:TRotMatrix; f:Integer;

begin
  m:=XRotation(43.59);
  for f:=0 to 2 do
  begin
    WriteLn(FloatToStr(m[f][0]) + ' ' + FloatToStr(m[f][1]) + ' ' +
    FloatToStr(m[f][2]));
  end;
end;

procedure YRotationTest();
var m:TRotMatrix; f:Integer;

begin
  m:=YRotation(43.59);
  for f:=0 to 2 do
  begin
    WriteLn(FloatToStr(m[f][0]) + ' ' + FloatToStr(m[f][1]) + ' ' +
    FloatToStr(m[f][2]));
  end;
end;

procedure ZRotationTest();
var m:TRotMatrix; f:Integer;

begin
  m:=ZRotation(43.59);
  for f:=0 to 2 do
  begin
    WriteLn(FloatToStr(m[f][0]) + ' ' + FloatToStr(m[f][1]) + ' ' +
    FloatToStr(m[f][2]));
  end;
end;

procedure InvertBaseTest();
var m:TRotMatrix; f:Integer;

begin
  m[0, 0]:=5.9;
  m[0, 1]:=3.2;
  m[0, 2]:=6.3;
  m[1, 0]:=7.4;
  m[1, 1]:=0.2;
  m[1, 2]:=5.0;
  m[2, 0]:=5.2;
  m[2, 1]:=7.8;
  m[2, 2]:=7.3;
  m:=InvertBase(m);
  for f:=0 to 2 do
  begin
    WriteLn(FloatToStr(m[f][0]) + ' ' + FloatToStr(m[f][1]) + ' ' +
    FloatToStr(m[f][2]));
  end;
end;

procedure BuildBaseTest();
begin

end;

end.

