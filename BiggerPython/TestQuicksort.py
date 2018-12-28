import quicksort


def QSAscendingIndexTest():
    Ixs = [5, 6, 7, 2, 11]
    Ixs = quicksort.QSAscendingIndex(Ixs)
    for x in range(len(Ixs)):
        print(Ixs[x])
    print('')

    Ixs = [9, 7, 5, 2, 1]
    Ixs = quicksort.QSAscendingIndex(Ixs)
    for x in range(len(Ixs)):
        print(Ixs[x])
    print('')

    Ixs = [5, 6, 7, 9, 11]
    Ixs = quicksort.QSAscendingIndex(Ixs)
    for x in range(len(Ixs)):
        print(Ixs[x])


def QuickTest():
    Ixs = [5, 6, 7, 2, 11]
    Index = [0, 1, 2, 3, 4]
    quicksort.Quick(0, len(Ixs) - 1, Ixs, Index)
    for x in range(len(Index)):
        print(Index[x])
    print('')

    Ixs = [9, 7, 5, 2, 1]
    Index = [0, 1, 2, 3, 4]
    quicksort.Quick(0, len(Ixs) - 1, Ixs, Index)
    for x in range(len(Index)):
        print(Index[x])
    print('')

    Ixs = [5, 6, 7, 9, 11]
    Index = [0, 1, 2, 3, 4]
    quicksort.Quick(0, len(Ixs) - 1, Ixs, Index)
    for x in range(len(Index)):
        print(Index[x])


# QSIndexAsRankingTest()
# QSIntsAsValsTest()
# QuickTest()
QSAscendingIndexTest()
# QSZeroBasedIndexTest()
# QSSortedTest()
