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


# CopyWordTest()
# GrabWordTest()
# GrabBetweenTest()
SplitStringTest()
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
# LeftJustifyTest()
