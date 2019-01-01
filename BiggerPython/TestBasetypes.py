import basetypes
import time


def AddToArrayTest():
    a = []
    for x in range(10):
        basetypes.AddToArray('banana' + str(x), a)
        print(a[x])
    ca = []
    for x in range(10):
        c = basetypes.TCoord(x, x, x)
        basetypes.AddToArray(c, ca)
        print(str(ca[x][0]) + ' ' + str(ca[x][1]) + ' ' + str(ca[x][2]))


def ForceNotZeroTest():
    v = 0.0000000000000001
    print(str(v))
    v = basetypes.ForceNotZero(v)
    print(str(v))


def PushIntoArrayTest():
    a = []
    basetypes.PushIntoArray('banana', a)
    basetypes.PushIntoArray(5, a)
    basetypes.PushIntoArray('pera', a)
    for x in range(len(a)):
        print(str(a[x]))


def RemoveFromArrayTest():
    lnum = [0.01, 1.01, 2.01, 3.01, 4.01, 5.01, 6.01, 7.01, 8.01, 9.01, 10.01]
    lstr = []
    lcrd = []
    for x in range(11):
        lstr.append('banana' + str(x))
        c = basetypes.TCoord(x, x, x)
        lcrd.append(c)
    basetypes.RemoveFromArray(5, lnum)
    basetypes.RemoveFromArray(6, lcrd)
    basetypes.RemoveFromArray(8, lstr)
    for x in range(len(lnum)):
        print(str(lnum[x]))
    for x in range(len(lcrd)):
        print(str(lcrd[x][0]) + ' ' + str(lcrd[x][1]) + ' ' + str(lcrd[x][2]))
    for x in range(len(lstr)):
        print(str(lstr[x]))
    print('')

    olist = []
    for x in range(11):
        olist.append(x)
    rlist = [5, 6, 7, 2, 11]
    basetypes.RemoveFromArray(rlist, olist)
    for x in range(len(olist)):
        print(str(olist[x]))


def ConcatenateTest():
    a1 = []
    a2 = []
    for x in range(5):
        c = basetypes.TCoord(x, x, x)
        a1.append(c)
    for x in range(8, 11):
        c = basetypes.TCoord(x, x, x)
        a2.append(c)
    a1 = basetypes.Concatenate(a1, a2)
    for x in range(len(a1)):
        print(str(a1[x][0]) + ' ' + str(a1[x][1]) + ' ' + str(a1[x][2]))


def SliceTest():
    Ints = [5, 7, 8, 11, 2, 9]
    Ints = basetypes.Slice(Ints, 2, 4)
    for x in range(len(Ints)):
        print(Ints[x])
    Coords = []
    for x in range(6):
        c = basetypes.TCoord(x, x, x)
        Coords.append(c)
    v = [2, 3, 5]
    Coords = basetypes.Slice(Coords, v)
    for x in range(len(Coords)):
        print(str(Coords[x][0]) + ' ' + str(Coords[x][1]) + ' ' + str(Coords[x][2]))


def CountInArrayTest():
    Ints = [5, 3, 4, 5, 5, 1]
    print(str(basetypes.CountInArray(5, Ints)))


def IndexOfTest():
    a = [5, 3, 4, 5, 5, 1]
    print(str(basetypes.IndexOf(4, a)))
    s = ['banana', 'kiwi', 'pera', 'mela', 'ananas', 'anguria']
    print(str(basetypes.IndexOf('mela', s)))


def IsInArrayTest():
    a = [5, 3, 4, 5, 5, 1]
    print(str(basetypes.IsInArray(4, a)))
    s = ['banana', 'kiwi', 'pera', 'mela', 'ananas', 'anguria']
    print(str(basetypes.IsInArray('pela', s)))


def MinTest():
    fa = [4.3, 9.1, 5]
    tf = basetypes.Min(fa)
    print(str(tf))

    print('')
    ia = [4, 9, 5]
    f = basetypes.Min(ia)
    print(str(f))

    print('')
    ca = []
    c = basetypes.TCoord(3, 7, 4)
    ca.append(c)
    c = basetypes.TCoord(5, 1, 6)
    ca.append(c)
    c = basetypes.TCoord(2, 2, 9)
    ca.append(c)
    c = basetypes.Min(ca)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))

    print('')
    m = [[5, 2, 7], [4, 1, 9], [3, 7, 3]]
    tf = basetypes.Min(m)
    print(str(tf))

    print('')
    f = 4
    i = 5
    f = basetypes.Min(f, i)
    print(str(f))

    print('')
    tf = 4.5
    tf2 = 5.1
    tf = basetypes.Min(tf, tf2)
    print(str(tf))

    print('')
    c = basetypes.TCoord(4, 5, 9)
    c2 = basetypes.TCoord(5, 7, 6)
    c = basetypes.Min(c, c2)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))


def MaxTest():
    fa = [4.3, 9.1, 5]
    tf = basetypes.Max(fa)
    print(str(tf))

    print('')
    ia = [4, 9, 5]
    f = basetypes.Max(ia)
    print(str(f))

    print('')
    ca = []
    c = basetypes.TCoord(3, 7, 4)
    ca.append(c)
    c = basetypes.TCoord(5, 1, 6)
    ca.append(c)
    c = basetypes.TCoord(2, 2, 9)
    ca.append(c)
    c = basetypes.Max(ca)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))

    print('')
    m = [[5, 2, 7], [4, 1, 9], [3, 7, 3]]
    tf = basetypes.Max(m)
    print(str(tf))

    print('')
    f = 4
    i = 5
    f = basetypes.Max(f, i)
    print(str(f))

    print('')
    tf = 4.5
    tf2 = 5.1
    tf = basetypes.Max(tf, tf2)
    print(str(tf))

    print('')
    c = basetypes.TCoord(4, 5, 9)
    c2 = basetypes.TCoord(5, 7, 6)
    c = basetypes.Max(c, c2)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))


def SumTest():
    fa1 = [4.3, 9.1, 5]
    fa2 = [7.4, 8.9, 9, 1.2]
    fa1 = basetypes.Sum(fa1, fa2)
    for f in range(len(fa1)):
        print(str(fa1[f]))

    print('')
    tf = basetypes.Sum(fa2)
    print(str(tf))

    print('')
    ia = [4, 9, 5]
    f = basetypes.Sum(ia)
    print(str(f))

    print('')
    ca = []
    c = basetypes.TCoord(3, 7, 4)
    ca.append(c)
    c = basetypes.TCoord(5, 1, 6)
    ca.append(c)
    c = basetypes.TCoord(2, 2, 1)
    ca.append(c)
    c = basetypes.Sum(ca)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))


def MinValIxTest():
    tf = [3.7, 2.1, 8.9, 9.6]
    f, i = basetypes.MinValIx(tf)
    print(str(f) + ' ' + str(i))


def MaxValIxTest():
    tf = [3.7, 2.1, 8.9, 9.6]
    f, i = basetypes.MaxValIx(tf)
    print(str(f) + ' ' + str(i))


def CoordTest():
    c = basetypes.Coord(5, 6, 7)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))


def FilledIntsTest():
    ia = basetypes.FilledInts(5, 2)
    for f in range(len(ia)):
        print(str(ia[f]))


def FilledFloatsTest():
    fa = basetypes.FilledFloats(5, 3.7)
    for f in range(len(fa)):
        print(str(fa[f]))


def StringToFloatTest():
    print(str(basetypes.StringToFloat('6.57')))
    print(str(basetypes.StringToFloat('348,532')))


def StringToFloatsTest():
    fa = basetypes.StringToFloats('6.54 56.93   8.74 12,7645')
    for f in range(len(fa)):
        print(str(fa[f]))


def InContactTest():
    c1 = basetypes.TCuboid(basetypes.TCoord(3, 6, 9), basetypes.TCoord(1, 5, 7))
    c2 = basetypes.TCuboid(basetypes.TCoord(6, 7, 3), basetypes.TCoord(7, 3, 2))
    print(basetypes.InContact(c1, c2))
    c2[1][0] = 9
    c2[1][1] = 9
    c2[1][2] = 9
    c1[1][0] = 9
    c1[1][1] = 9
    c1[1][2] = 9
    print(basetypes.InContact(c1, c2))


def AverageTest():
    fa = [3.2, 4.1, 6.8, 2.9, 9.4]
    f = basetypes.Average(fa)
    print(str(f))
    ia = [3, 4, 6, 2, 9]
    f = basetypes.Average(ia)
    print(str(f))


def MedianTest():
    fa = [3.2, 4.1, 6.8, 2.9, 9.4]
    f = basetypes.Median(fa)
    print(str(f))
    ia = [3, 4, 6, 2, 9]
    i = basetypes.Median(ia)
    print(str(i))


def VarianceTest():
    fa = [5.4, 3.8, 6.5]
    v = basetypes.Variance(fa, 5.5)
    print(str(v))


def IsBetweenTest():
    print(str(basetypes.IsBetween(6.4, 5.8, 7.1)))
    print(str(basetypes.IsBetween(5.4, 5.8, 5.1)))
    print(str(basetypes.IsBetween(7.2, 4.7, 6.7)))
    print(str(basetypes.IsBetween(3.2, 4.7, 6.7)))


def GetTickCountTest():
    f = basetypes.GetTickCount()
    print(str(f))


def GetTimeIntervalTest():
    s = basetypes.GetTickCount()
    time.sleep(5)
    d, s = basetypes.GetTimeInterval(s)
    print(str(s) + ' ' + str(d))


# AddToArrayTest()
# ForceNotZeroTest()
# PushIntoArrayTest()
# RemoveFromArrayTest()
# ConcatenateTest()
# SliceTest()
# CountInArrayTest()
# IndexOfTest()
# IsInArrayTest()
# AddUniqueToArrayTest()
# AppendToTest()
# MinTest()
# MaxTest()
# MinIxTest()
# MaxIxTest()
# SumTest()
MinValIxTest()
MaxValIxTest()
# CoordTest()
# StringsToFloatsTest()
# FilledIntsTest()
# FilledFloatsTest()
# IsEqualTest()
# StringToFloatTest()
# ScaleMatrixTest()
# AddMatricesTest()
# StringToFloatsTest()
# InContactTest()
# AverageTest()
# MedianTest()
# VarianceTest()
# IsBetweenTest()
# GetTickCountTest()
# GetTimeIntervalTest()
