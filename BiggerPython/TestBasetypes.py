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


AddToArrayTest()
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
# CoordTest()
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
