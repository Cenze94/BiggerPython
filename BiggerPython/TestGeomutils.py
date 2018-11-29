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
DistanceTest()
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
# BuildRotationTest()
# XRotationTest()
# YRotationTest()
# ZRotationTest()
# InvertBaseTest()
# BuildBaseTest()
