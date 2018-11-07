import basetypes


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


def CoordTest():
    c = basetypes.Coord(5, 6, 7)
    print(str(c[0]) + ' ' + str(c[1]) + ' ' + str(c[2]))


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
# MinValIxTest()
# MaxValIxTest()
CoordTest()
# StringsToFloatsTest()
# IsEqualTest()
# StringToFloatTest()
# ScaleMatrixTest()
# AddMatricesTest()
# StringToFloatsTest()
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
