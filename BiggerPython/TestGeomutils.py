import geomutils
import basetypes


def QuaternionTest():
    q = geomutils.Quaternion(0, 1, 2, 3)
    print(str(q[0]) + ' ' + str(q[1]) + ' ' + str(q[2]) + ' ' + str(q[3]))


def AddTest():
    c = basetypes.TCoord(1, 2, 3)
    c = geomutils.Add(c, 2)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))

    c = basetypes.TCoord(1, 2, 3)
    c2 = basetypes.TCoord(2, 3, 4)
    c = geomutils.Add(c, c2)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))

    print('')
    ca = []
    c = basetypes.TCoord(3, 8, 1)
    ca.append(c)
    c = basetypes.TCoord(6, 4, 6)
    ca.append(c)
    c = basetypes.TCoord(0, 2, 5)
    ca.append(c)
    c = basetypes.TCoord(5, 1, 3)
    ca = geomutils.Add(c, ca)
    for f in range(len(ca)):
        print(str(ca[f][0]) + ' ' + str(ca[f][1]) + ' ' + str(ca[f][2]))

    print('')
    fa = [3.0, 6.0, 4.0]
    fa = geomutils.Add(fa, 2)
    for f in range(len(fa)):
        print(str(fa[f]))

    print('')
    ca = []
    c = basetypes.TCoord(3, 8, 1)
    ca.append(c)
    c = basetypes.TCoord(6, 4, 6)
    ca.append(c)
    c = basetypes.TCoord(0, 2, 5)
    ca.append(c)
    fa = [3, 6, 4]
    ca = geomutils.Add(ca, fa)
    for f in range(len(ca)):
        print(str(ca[f][0]) + ' ' + str(ca[f][1]) + ' ' + str(ca[f][2]))


def SubtractTest():
    c = basetypes.TCoord(0, 1, 2)
    c = geomutils.Subtract(c, 1)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))

    c = basetypes.TCoord(6, 3, 9)
    c2 = basetypes.TCoord(3, 5, 1)
    c = geomutils.Subtract(c, c2)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))

    print('')
    ca = []
    c = basetypes.TCoord(3, 8, 1)
    ca.append(c)
    c = basetypes.TCoord(6, 4, 6)
    ca.append(c)
    c = basetypes.TCoord(0, 2, 5)
    ca.append(c)
    c = basetypes.TCoord(5, 1, 3)
    ca = geomutils.Subtract(ca, c)
    for f in range(len(ca)):
        print(str(ca[f][0]) + ' ' + str(ca[f][1]) + ' ' + str(ca[f][2]))


def MultiplyTest():
    c = basetypes.TCoord(0, 1, 2)
    c = geomutils.Multiply(c, 2)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))

    print('')
    fa = [3.0, 6.0, 4.0]
    fa = geomutils.Multiply(fa, 2)
    for f in range(len(fa)):
        print(str(fa[f]))

    print('')
    ca = []
    c = basetypes.TCoord(3, 8, 1)
    ca.append(c)
    c = basetypes.TCoord(6, 4, 6)
    ca.append(c)
    c = basetypes.TCoord(0, 2, 5)
    ca.append(c)
    ca = geomutils.Multiply(ca, 2)
    for f in range(len(ca)):
        print(str(ca[f][0]) + ' ' + str(ca[f][1]) + ' ' + str(ca[f][2]))

    print('')
    q1 = geomutils.TQuaternion(2, 4, 3, 5)
    q2 = geomutils.TQuaternion(1, 3, 5, 2)
    q1 = geomutils.Multiply(q1, q2)
    print(str(q1[0]) + ' ' + str(q1[1]) + ' ' + str(q1[2]) + ' ' + str(q1[3]))

    print('')
    r1 = geomutils.TRotMatrix(2, 4, 5, 3, 1, 1, 3, 2, 2)
    r2 = geomutils.TRotMatrix(3, 1, 1, 2, 5, 5, 4, 2, 3)
    r1 = geomutils.Multiply(r1, r2)
    print(str(r1[0, 0]) + ' ' + str(r1[0, 1]) + ' ' + str(r1[0, 2]) + '\n' + str(r1[1, 0]) + ' ' + str(r1[1, 1]) + ' '
          + str(r1[1, 2]) + '\n' + str(r1[2, 0]) + ' ' + str(r1[2, 1]) + ' ' + str(r1[2, 2]))


def ConjugatedTest():
    q = geomutils.TQuaternion(1, 2, 3, 4)
    q = geomutils.Conjugated(q)
    print(str(q[0]) + ' ' + str(q[1]) + ' ' + str(q[2]) + ' ' + str(q[3]))


def SimmetricTest():
    c = basetypes.TCoord(3, 8, 1)
    c = geomutils.Simmetric(c)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))


def DistanceTest():
    c = basetypes.TCoord(3, 8, 1)
    c1 = basetypes.TCoord(4, 5, 2)
    d = geomutils.Distance(c, c1)
    print(d)

    print('')
    ca = []
    ca1 = []
    c = basetypes.TCoord(5, 1, 1)
    ca.append(c)
    c = basetypes.TCoord(4, 6, 8)
    ca.append(c)
    c = basetypes.TCoord(1, 2, 1)
    ca.append(c)
    c = basetypes.TCoord(4, 3, 5)
    ca1.append(c)
    c = basetypes.TCoord(2, 0, 6)
    ca1.append(c)
    c = basetypes.TCoord(7, 3, 6)
    ca1.append(c)
    da = geomutils.Distance(ca, ca1)
    for f in range(len(da)):
        print(da[f])

    print('')
    q1 = geomutils.TQuaternion(4, 2, 1, 3)
    q2 = geomutils.TQuaternion(2, 2, 3, 1)
    d = geomutils.Distance(q1, q2)
    print(d)


def MidPointTest():
    ca = [basetypes.TCoord(4, 6, 9), basetypes.TCoord(1, 4, 1), basetypes.TCoord(3, 8, 4)]
    c = geomutils.MidPoint(ca)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))


def RotateTest():
    c = basetypes.TCoord(1, 2, 3)
    r = geomutils.TRotMatrix(4, 3, 1, 5, 2, 3, 5, 3, 1)
    c = geomutils.Rotate(c, r)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))

    print('')
    ca = []
    c = basetypes.TCoord(3, 8, 1)
    ca.append(c)
    c = basetypes.TCoord(6, 4, 6)
    ca.append(c)
    c = basetypes.TCoord(0, 2, 5)
    ca.append(c)
    ca = geomutils.Rotate(ca, r)
    for f in range(len(ca)):
        print(str(ca[f][0]) + ' ' + str(ca[f][1]) + ' ' + str(ca[f][2]))

    print('')
    q = geomutils.TQuaternion(2, 3, 1, 4)
    c = basetypes.TCoord(1, 2, 3)
    c = geomutils.Rotate(c, q)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))

    print('')
    ca = []
    c = basetypes.TCoord(3, 8, 1)
    ca.append(c)
    c = basetypes.TCoord(6, 4, 6)
    ca.append(c)
    c = basetypes.TCoord(0, 2, 5)
    ca.append(c)
    ca = geomutils.Rotate(ca, q)
    for f in range(len(ca)):
        print(str(ca[f][0]) + ' ' + str(ca[f][1]) + ' ' + str(ca[f][2]))


def RotAndPlaceTest():
    m = geomutils.TRotMatrix(5.6, 4.0, 8.2, 5.8, 3.3, 6.2, 7.4, 9.1, 1.3)
    p = basetypes.TCoord(3.6, 6.4, 2.5)
    v = basetypes.TCoord(4.7, 2.2, 0.4)
    v = geomutils.RotAndPlace(m, p, v)
    print(str(v[0]) + ' ' + str(v[1]) + ' ' + str(v[2]))

    print('')
    va = [basetypes.TCoord(4.8, 4.1, 6.0), basetypes.TCoord(0.6, 3.8, 7.2), basetypes.TCoord(4.1, 6.6, 8.0)]
    geomutils.RotAndPlace(m, p, va)
    for f in range(len(va)):
        print(str(va[f][0]) + ' ' + str(va[f][1]) + ' ' + str(va[f][2]))


def BuildRotationTest():
    c = basetypes.TCoord(4.8, 0.2, 7.2)
    m = geomutils.BuildRotation(c, geomutils.MCXYZRotation)
    for f in range(3):
        print(str(m[f, 0]) + ' ' + str(m[f, 1]) + ' ' + str(m[f, 2]))
        print('')
    print('')
    m = geomutils.BuildRotation(c, geomutils.MCRotationXYAxis)
    for f in range(3):
        print(str(m[f, 0]) + ' ' + str(m[f, 1]) + ' ' + str(m[f, 2]))
        print('')
    print('')
    m = geomutils.BuildRotation(c, 2)
    for f in range(3):
        print(str(m[f, 0]) + ' ' + str(m[f, 1]) + ' ' + str(m[f, 2]))
        print('')


def XRotationTest():
    m = geomutils.XRotation(43.59)
    for f in range(3):
        print(str(m[f, 0]) + ' ' + str(m[f, 1]) + ' ' + str(m[f, 2]))


def YRotationTest():
    m = geomutils.YRotation(43.59)
    for f in range(3):
        print(str(m[f, 0]) + ' ' + str(m[f, 1]) + ' ' + str(m[f, 2]))


def ZRotationTest():
    m = geomutils.ZRotation(43.59)
    for f in range(3):
        print(str(m[f, 0]) + ' ' + str(m[f, 1]) + ' ' + str(m[f, 2]))


def InvertBaseTest():
    m = geomutils.TRotMatrix(5.9, 3.2, 6.3, 7.4, 0.2, 5.0, 5.2, 7.8, 7.3)
    m = geomutils.InvertBase(m)
    for f in range(3):
        print(str(m[f, 0]) + ' ' + str(m[f, 1]) + ' ' + str(m[f, 2]))


# QuaternionTest()
# AddTest()
# SubtractTest()
# MultiplyTest()
# DotProductTest()
# CrossProductTest()
# NormTest()
# ConjugatedTest()
# NormalizeTest()
# ScaledTest()
# SimmetricTest()
# DistanceTest()
# DistanceSquaredTest()
# MidPointTest()
# RotateTest()
# RotationQuaternionTest()
# RotationToTest()
# StaticRMSDTest()
# Intersection2DTest()
# DistanceToLine2DTest()
# DistanceToSegment2DTest()
# DistanceToNormalizedAxisTest()
# OrthogonalCoordsTest()
# RotAndPlaceTest()
BuildRotationTest()
# XRotationTest()
# YRotationTest()
# ZRotationTest()
# InvertBaseTest()
# BuildBaseTest()
