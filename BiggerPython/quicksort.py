

# Ix = TIntegers
def QSIndexAsRanking(Ix):
    Result = []
    for f in range(len(Ix)):
        Result.append(0)
    for f in range(len(Ix)):
        Result[Ix[f]] = f
    # Return TIntegers
    return Result


# Ints = array of Integer
def QSIntsAsVals(Ints):
    Result = []
    for f in range(len(Ints)):
        Result.append(Ints[f])
    # Return TFloats
    return Result


# ini, fim = Integer, Values = const TFloats OR const TQSInt64Vals OR const TIntegers, Result = TIntegers
def Quick(ini, fim, Values, Result):
    vpiv = Values[Result[ini]]
    le = ini
    ri = fim
    while True:
        while Values[Result[le]] < vpiv:
            le = le + 1
        while Values[Result[ri]] > vpiv:
            ri = ri - 1
        if ri >= le:
            t = Result[ri]
            Result[ri] = Result[le]
            Result[le] = t
            le = le + 1
            ri = ri - 1
        if ri < le:
            break
    if ri > ini:
        Quick(ini, ri, Values, Result)
    if fim > le:
        Quick(le, fim, Values, Result)


# Values = const TFloats OR const TQSInt64Vals OR const TIntegers
def QSAscendingIndex(Values):
    Result = []
    for t in range(len(Values)):
        Result.append(t)
    if len(Result) > 0:
        Quick(0, len(Result) - 1, Values, Result)
    # Return TIntegers
    return Result


# Count = Integer
def QSZeroBasedIndex(Count):
    Result = []
    for f in range(len(Count)):
        Result.append(f)
    # Return TIntegers
    return Result


# Values = const TIntegers OR const TFloats
def QSSorted(Values):
    ixs = QSAscendingIndex(Values)
    Result = []
    for f in range(len(ixs)):
        Result.append(Values[ixs[f]])
    # Return TIntegers OR TFloats
    return Result
