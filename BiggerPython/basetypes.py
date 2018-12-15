import numpy as np
import math
import time
import quicksort


# Class to avoid global variables
class TypesConstants:
    Tiny = 1e-12


# The purpose of this class is to distinguish between coordinates and arrays of there numbers
class TCoord:
    def __init__(self, coord1=None, coord2=None, coord3=None):
        self.coord = []
        if coord1 is None:
            coord1 = 0
        if coord2 is None:
            coord2 = 0
        if coord3 is None:
            coord3 = 0
        coord1 = float(coord1)
        coord2 = float(coord2)
        coord3 = float(coord3)
        self.coord.append(coord1)
        self.coord.append(coord2)
        self.coord.append(coord3)

    def __getitem__(self, item):
        return self.coord[item]

    def __setitem__(self, key, value):
        self.coord[key] = value


class TCuboid:
    def __init__(self, coord1=None, coord2=None):
        self.coords = []
        if coord1 is None or not isinstance(coord1, TCoord):
            coord1 = TCoord(0, 0, 0)
        if coord2 is None or not isinstance(coord2, TCoord):
            coord2 = TCoord(0, 0, 0)
        self.coords.append(coord1)
        self.coords.append(coord2)

    def __getitem__(self, item):
        return self.coords[item]

    def __setitem__(self, key, value):
        self.coords[key] = value


# This method could be inline, it's defined only to help the transposition of Pascal code to Python one
# s = string OR TCoord OR TInteger OR TFloat OR Cardinal OR Boolean,
# a = TSimpleStrings OR TCoords OR TIntegers OR TFloats OT TCardinals OR TBooleans
def AddToArray(s, a):
    a.append(s)


# This method is different from the original one, because it's impossible to modify Val and the original variable, so
# the only way is to return the new value
# Val = TFloat
def ForceNotZero(Val):
    if (Val < 0) and (Val > -TypesConstants.Tiny):
        return -TypesConstants.Tiny
    elif (Val >= 0) and (Val < TypesConstants.Tiny):
        return TypesConstants.Tiny


# I = integer OR string, A = TIntegers OR TSimpleStrings
def PushIntoArray(I, A):
    A.insert(0, I)


# First method: v = Integer, a = TIntegers OT TFloats OR TCardinals OR TCoords OR TSimpleStrings
# Second method: Ixs, A = TIntegers
def RemoveFromArray(Ixs, A):
    if isinstance(Ixs, list):
        top = len(A)
        f = 0
        while f < top:
            if IsInArray(f, Ixs):
                top = top - 1
                A[f] = A[top]
                A.pop()
            f = f + 1
    else:
        A[Ixs] = A[len(A) - 1]
        A.pop()


# a1, a2 = TCoords OR TSimpleStrings
def Concatenate(a1, a2):
    Result = []
    for f in range(len(a1)):
        Result.append(a1[f])
    for f in range(len(a2)):
        Result.append(a2[f])
    # Return TCoords OR TSimpleStrings
    return Result


# Ints = const TIntegers OR const TFloats, Ix1, Ix2 = Integer
# Ints = const TCoords, Ix1 = TIntegers
def Slice(Ints, Ix1, Ix2=None):
    Result = []
    if Ix2 is None:
        for f in range(len(Ix1)):
            Result.append(Ints[Ix1[f]])
    else:
        for f in range(Ix1, Ix2+1):
            Result.append(Ints[f])
    # Return TIntegers OR TFloats OR TCoords
    return Result


# I = Integer, Ints = const TIntegers
def CountInArray(I, Ints):
    Result = 0
    for f in range(len(Ints)):
        if Ints[f] is I:
            Result = Result + 1
    # Return Integer
    return Result


# i = const Integer OR const Cardinal OR const string, a = const TIntegers OR const TCardinals OR const TSimpleStrings
def IndexOf(i, a):
    Result = len(a) - 1
    while (Result >= 0) and (a[Result] is not i):
        Result = Result - 1
    # Return Integer
    return Result


# i = const Integer OR const Cardinal OR const string, a = const TIntegers OR const TCardinals OR const TSimpleStrings
def IsInArray(i, a):
    # return Boolean
    return IndexOf(i, a) >= 0


# Elm = const String OR const Cardinal OR const Integer, Arr = TSimpleStrings OR TCardinals OR TIntegers
def AddUniqueToArray(elm, Arr):
    if IndexOf(elm, Arr) < 0:
        AddToArray(elm, Arr)


# Suffix = const TCoords OR const TSimpleStrings OR const TCardinals OR const TIntegers OR TFloats,
# Arr = TCoords OR TSimpleStrings OR TCardinals OR TIntegers OR TFloats
def AppendToArray(Suffix, Arr):
    if Suffix is not None:
        for f in range(len(Suffix)):
            Arr.append(Suffix[f])


# First function: vals = TFloats
# Second function: vals = TMatrix
# Third function: vals = TCoords
# Fourth function: vals = TIntegers
# Fifth function: vals, vals2 = TCoord
# Sixth function: vals, vals2 = TFloat
# Seventh function: vals, vals2 = Integer
def Min(vals, vals2=None):
    Result = 0
    if vals is not None:
        if vals2 is not None:
            if isinstance(vals2, TCoord):
                # Fifth function
                Result = TCoord()
                if vals[0] < vals2[0]:
                    Result[0] = vals[0]
                else:
                    Result[0] = vals2[0]
                if vals[1] < vals2[1]:
                    Result[1] = vals[1]
                else:
                    Result[1] = vals2[1]
                if vals[2] < vals2[2]:
                    Result[2] = vals[2]
                else:
                    Result[2] = vals2[2]
            else:
                # Sixth and seventh functions
                if vals < vals2:
                    Result = vals
                else:
                    Result = vals2
        else:
            if isinstance(vals[0], list) and vals[0] is not None:
                # Second function
                Result = vals[0][0]
                for f in range(len(vals)):
                    for g in range(len(vals[f])):
                        if vals[f][g] < Result:
                            Result = vals[f][g]
            elif isinstance(vals[0], TCoord):
                # Third function
                if len(vals) > 0:
                    Result = vals[0]
                    for f in range(1, len(vals)):
                        Result = Min(Result, vals[f])
                else:
                    Result = TCoord(0, 0, 0)
            else:
                # First and fourth functions
                Result = vals[0]
                for f in range(1, len(vals)):
                    if vals[f] < Result:
                        Result = vals[f]
    # Return TFloat OR TCoord OR Integer
    return Result


# First function: vals = TFloats
# Second function: vals = TMatrix
# Third function: vals = TCoords
# Fourth function: vals = TIntegers
# Fifth function: vals, vals2 = TCoord
# Sixth function: vals, vals2 = TFloat
# Seventh function: vals, vals2 = Integer
def Max(vals, vals2=None):
    Result = 0
    if vals is not None:
        if vals2 is not None:
            if isinstance(vals2, TCoord):
                # Fifth function
                Result = TCoord()
                if vals[0] > vals2[0]:
                    Result[0] = vals[0]
                else:
                    Result[0] = vals2[0]
                if vals[1] > vals2[1]:
                    Result[1] = vals[1]
                else:
                    Result[1] = vals2[1]
                if vals[2] > vals2[2]:
                    Result[2] = vals[2]
                else:
                    Result[2] = vals2[2]
            else:
                # Sixth and seventh functions
                if vals > vals2:
                    Result = vals
                else:
                    Result = vals2
        else:
            if isinstance(vals[0], list) and vals[0] is not None:
                # Second function
                Result = vals[0][0]
                for f in range(len(vals)):
                    for g in range(len(vals[f])):
                        if vals[f][g] > Result:
                            Result = vals[f][g]
            elif isinstance(vals[0], TCoord):
                # Third function
                if len(vals) > 0:
                    Result = vals[0]
                    for f in range(1, len(vals)):
                        Result = Max(Result, vals[f])
                else:
                    Result = TCoord(0, 0, 0)
            else:
                # First and fourth functions
                Result = vals[0]
                for f in range(1, len(vals)):
                    if vals[f] > Result:
                        Result = vals[f]
    # Return TFloat OR TCoord OR Integer
    return Result


# vals = TFloats
def MinIx(vals):
    Result = -1
    if vals is not None:
        Result = 0
        mi = vals[0]
        for f in range(len(vals)):
            if vals[f] < mi:
                mi = vals[f]
                Result = f
    # Return Integer
    return Result


# vals = TFloats
def MaxIx(vals):
    Result = -1
    if vals is not None:
        Result = 0
        mi = vals[0]
        for f in range(len(vals)):
            if vals[f] > mi:
                mi = vals[f]
                Result = f
    # Return Integer
    return Result


# vals = TFloats OR TCoords OR TIntegers
# vals, vals2 = const TFLoats
def Sum(vals, vals2=None):
    Result = 0
    if vals is not None:
        if vals2 is not None:
            Result = []
            length = min(len(vals), len(vals2))
            for f in range(length):
                Result.append(vals[f] + vals2[f])
        else:
            if isinstance(vals[0], TCoord):
                Result = TCoord()
                for f in range(len(vals)):
                    Result[0] = Result[0] + vals[f][0]
                    Result[1] = Result[1] + vals[f][1]
                    Result[2] = Result[2] + vals[f][2]
            else:
                for f in range(len(vals)):
                    Result = Result + vals[f]
    # Return TFloat OR TCoord OR TFloats OR Integer
    return Result


# Vals = TFloats, MinVal = TFloat, MinIx = Integer (the last 2 are not present because they are useless)
def MinValIx(Vals):
    MinIx = -1
    MinVal = 0
    if Vals is not None:
        MinIx = 0
        MinVal = Vals[0]
        for f in range(len(Vals)):
            if Vals[f] < MinVal:
                MinVal = Vals[f]
                MinIx = f
    # Return TFloat, Integer
    return MinVal, MinIx


# Vals = TFloats, MaxVal = TFloat, MaxIx = Integer (the last 2 are not present because they are useless)
def MaxValIx(Vals):
    MaxIx = -1
    MaxVal = 0
    if Vals is not None:
        MaxIx = 0
        MaxVal = Vals[0]
        for f in range(len(Vals)):
            if Vals[f] > MaxVal:
                MaxVal = Vals[f]
                MaxIx = f
    # Return TFloat, Integer
    return MaxVal, MaxIx


# X, Y, Z = TFloat
def Coord(X, Y, Z):
    Result = TCoord()
    Result[0] = X
    Result[1] = Y
    Result[2] = Z
    # Return TCoord
    return Result


# SS = TSimpleStrings
def StringsToFloats(SS):
    Result = []
    for f in range(len(SS)):
        Result.append(StringToFloat(SS[f]))
    # Return TFloats
    return Result


# Len, Val = Integer
def FilledInts(Len, Val):
    Result = []
    for f in range(Len):
        Result.append(Val)
    # Return TIntegers
    return Result


# Len = Integer, Val = TFloat
def FilledFloats(Len, Val):
    # FilledInts and FilledFloats are the same in Python, so it's possible to use FilledInts inside FilledFloats
    # Return TFloats
    return FilledInts(Len, Val)


# A1, A2 = TIntegers
def IsEqual(A1, A2):
    Result = len(A1) is len(A2)
    if Result:
        for f in range(len(A1)):
            if A1[f] is not A2[f]:
                return False
    # Return Boolean
    return Result


# S = const String
def StringToFloat(S):
    # Return TFloat
    return float(S)


# Mat = TMatrix, Scale = TFloat
def ScaleMatrix(Mat, Scale):
    Result = np.copy(Mat)
    for x in range(len(Mat)):
        for y in range(len(Mat[x])):
            Result[x, y] = Result[x, y] * Scale
    # Return TMatrix
    return Result


# Mat1, Mat2 = TMatrix
def AddMatrices(Mat1, Mat2):
    if (Mat1 is None) or (Mat2 is None) or (len(Mat1) is not len(Mat2)) or (len(Mat1[0]) is not len(Mat2[0])):
        return None
    else:
        Result = np.copy(Mat1)
        for x in range(len(Mat1)):
            for y in range(len(Mat1[x])):
                Result[x, y] = Result[x, y] + Mat2[x, y]
        return Result


# S = string
def StringToFloats(S):
    t = ''
    if S is None:
        return None
    Result = []
    for f in range(len(S)):
        if S[f] > ' ':
            t = t + S[f]
        elif t is not '':
            Result.append(StringToFloat(t))
            t = ''
    if t is not '':
        Result.append(StringToFloat(t))
    # Return TFloats
    return Result


# Cuboid1, Cuboid2 = TCuboid
def InContact(Cuboid1, Cuboid2):
    # c1, c2 = TCoord
    c1 = Max(Cuboid1[0], Cuboid2[0])
    c2 = Min(Cuboid1[1], Cuboid2[1])
    # Return Boolean
    return (c1[0] <= c2[0]) and (c1[1] <= c2[1]) and (c1[2] <= c2[2])


# Fs = TFloats OR TIntegers
def Average(Fs):
    Result = 0
    if Fs:
        for f in range(len(Fs)):
            Result = Result + Fs[f]
        Result = Result / len(Fs)
    # Return TFloat
    return Result


# GFs = TFloats OR TIntegers
def Median(Fs):
    Result = 0
    if Fs:
        ixs = quicksort.QSAscendingIndex(Fs)
        ix = int(len(Fs) / 2)
        Result = Fs[ixs[ix]]
    # Return TFloat OR Integer
    return Result


# Fs = TFloats, Avg = TFloat
def Variance(Fs, Avrg):
    Result = 0
    for f in range(len(Fs)):
        Result = Result + (Fs[f] - Avrg) ** 2
    # Return TFloat
    return Result / len(Fs)


# Val, Extreme1, Extreme2 = TFloat
def IsBetween(Val, Extreme1, Extreme2):
    # return Boolean
    return ((Val >= Extreme1) and (Val <= Extreme2)) or ((Val <= Extreme1) and (Val >= Extreme2))


def GetTickCount():
    # Time is in milliseconds
    # Return DWORD
    return int(time.time() * 1000)


# StartTick = DWORD
def GetTimeInterval(StartTick):
    endtick = GetTickCount()
    # endtick is returned as the new StartTick, because it can't be modifies inside the method
    # Return Integer
    return endtick - StartTick, endtick
