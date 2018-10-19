

# Class to avoid global variables
class TypesConstants:
    Tiny = 1e-12


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


# v = Integer, v = TIntegers OT TFloats OR TCardinals OR TCoords OR TSimpleStrings
def RemoveFromArray(v, a):
    a.pop(v)


# a1, a2 = TCoords OR TSimpleStrings
def Concatenate(a1, a2):
    Result = []
    for f in range(len(a1)):
        Result.append(a1[f])
    for f in range(len(a2)):
        Result.append(a2[f])
    # Return TCoords OR TSimpleStrings
    return Result


# Ints = const TIntegers OR const TFloats, Ix1, Ix2 = Integer; Ints = const TCoords, Ix1 = TIntegers
def Slice(Ints, Ix1, Ix2=None):
    Result = []
    if Ix2 is None:
        for f in range(len(Ix1)):
            Result[f] = Ints(Ix1[f])
    else:
        for f in range(Ix1, Ix2):
            Result[f - Ix1] = Ints[Ix1]
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


# Ixs, A = TIntegers
def RemoveFromArray(Ixs, A):
    top = len(A)
    f = 0
    while f < top:
        if IsInArray(f, Ixs):
            top = top - 1
            A[f] = A[top]
        f = f + 1
    for f in range(top, len(A)):
        del A[f]


# i = const Integer OR const Cardinal OR const string, a = const TIntegers OR const TCardinals OR const TSimpleStrings
def IndexOf(i, a):
    Result = len(a) - 1
    while (Result >= 0) and (a[Result] is not i):
        Result = Result -1
    # Return Integer
    return Result


# i = const Integer OR const Cardinal OR const string, a = const TIntegers OR const TCardinals OR const TSimpleStrings
def IsInArray(i, a):
    # return Boolean
    return IndexOf(i, a) >= 0
