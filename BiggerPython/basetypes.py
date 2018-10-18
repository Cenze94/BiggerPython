

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
