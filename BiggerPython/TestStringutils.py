import stringutils


def CopyWordTest():
    s = "Il gatto è sopra il tavolo."
    sf = stringutils.CopyWord(s, ' ')
    print(s + 'f')
    print(sf + 'f')


def GrabWordTest():
    s = "Il gatto è sopra il tavolo."
    sf, s = stringutils.GrabWord(s, ' ')
    print(s + 'f')
    print(sf + 'f')


def GrabBetweenTest():
    s = 'Il gatto è sopra il| tavolo.'
    sf, s = stringutils.GrabBetween(s, ' ', '|')
    print(s + 'f')
    print(sf + 'f')


def SplitStringTest():
    s = "Il gatto è sopra il tavolo"
    ss = stringutils.SplitString(s, ' ')
    for x in range(len(ss)):
        print(ss[x])
    print('')

    s = "Il gatto è sopra il tavolo"
    ts = []
    stringutils.SplitString(s, ts, ' ')
    for x in range(len(ss)):
        print(ss[x])
    print('')

    s = "Il gatto è sopra il tavolo"
    ss = stringutils.SplitString(s)
    for x in range(len(ss)):
        print(ss[x])


def GetIntegerTest():
    val = 0
    test, val = stringutils.GetInteger('1  2', 1, 4, val)
    print(test)
    print(val)
    print(stringutils.GetInteger('1  2', 1, 4))
    print('')
    test, val = stringutils.GetInteger('12f', 1, 3, val)
    print(test)
    print(val)
    print(stringutils.GetInteger('12f', 1, 3))
    print('')
    test, val = stringutils.GetInteger('aaa12bbb', 4, 5, val)
    print(test)
    print(val)
    print(stringutils.GetInteger('aaa12bbb', 4, 5))


def GetFloatTest():
    val = 0
    test, val = stringutils.GetFloat('1  2 . 7   8', 1, 12, val)
    print(test)
    print(val)
    print(stringutils.GetFloat('1  2 . 7   8', 1, 12))
    print('')
    test, val = stringutils.GetFloat('12.78f', 1, 6, val)
    print(test)
    print(val)
    print(stringutils.GetFloat('12.78f', 1, 6))
    print('')
    test, val = stringutils.GetFloat('aaa12.78bbb', 4, 8, val)
    print(test)
    print(val)
    print(stringutils.GetFloat('aaa12.78bbb', 4, 8))
    print('')
    test, val = stringutils.GetFloat('12', 1, 2, val)
    print(test)
    print(val)
    print(stringutils.GetFloat('12', 1, 2))
    print('')
    test, val = stringutils.GetFloat('12,78', 1, 5, val)
    print(test)
    print(val)
    print(stringutils.GetFloat('12,78f', 1, 5))


def GetStringTest():
    print(stringutils.GetString('ban ana', 3, 5))


def LastIndexOfTest():
    ti = [3, 2, 5, 2, 4]
    print(str(stringutils.LastIndexOf(2, ti)))
    ts = ['banana', 'mela', 'pera', 'mela', 'fico']
    print(str(stringutils.LastIndexOf('mela', ts)))
    s1 = "Il gatto è sopra il gatto."
    subs = "gatto"
    print(str(stringutils.LastIndexOf(subs, s1)) + ' total length: ' + str(len(s1)))
    subs = "garage"
    print(str(stringutils.LastIndexOf(subs, s1)))


def DeblankTest():
    print(stringutils.Deblank(' ban ana ') + 'f')


def RightJustifyTest():
    print(stringutils.RightJustify(126356, 5))
    print(stringutils.RightJustify(126356, 8))
    print(stringutils.RightJustify(1263.56, 5, 1))
    print(stringutils.RightJustify(1263.56, 8, 1))
    print(stringutils.RightJustify(1263.56, 5, 3))
    print(stringutils.RightJustify(1263.56, 8, 3))
    print(stringutils.RightJustify(1263.56, 5, 5))
    print(stringutils.RightJustify(1263.56, 8, 5))


def LeftJustifyTest():
    print(stringutils.LeftJustify('banana', 4) + 'f')
    print(stringutils.LeftJustify('banana', 8) + 'f')


# CopyWordTest()
# GrabWordTest()
# GrabBetweenTest()
# SplitStringTest()
# SplitOnAllTest()
# SplitLinesTest()
# SplitCharsTest()
# GetIntegerTest()
# GetFloatTest()
# GetStringTest()
# LastIndexOfTest()
# FirstIndexOfTest()
# FirstByPrefixTest()
# FirstContainingTest()
# LookupByPrefix()
# DeblankTest()
# TrimmedBlanksTest()
# StringAsArrayTest()
# SaveToFileTest()
# FlattenStringTest()
# SnipStringTest()
# SnipStringAllTest()
# CountInStringTest()
# CleanStringTest()
# AppendToLengthTest()
# FixLineBreaksTest()
# UncasedCompareTest()
# AsSimpleStringTest()
# CopyStringTest()
# AppendToStringList()
# CleanFileNameTest()
# CheckHexTest()
# ReplaceHexCodesTest()
# PosUncasedTest()
# TagStringTest()
# GetTaggedFieldTest()
# GetTaggedFieldsTest()
# DecodeQPTest()
# ConvertHtmlTest()
# HtmlToAsciiTest()
# BasicASCIITest()
# ReadAsSimpleStringTest()
# ReadKeysAsValuesTest()
# RightJustifyTest()
LeftJustifyTest()
