import basetypes


# Returns substring to separator. Text is not altered
# Text, Sep = const string
def CopyWord(Text, Sep):
    p = Text.find(Sep)
    if p < 0:
        # Return string
        return Text
    else:
        # Return string
        return Text[:p]


# Deletes separator if found at beginning. Returns substring to separator deleting that from Text
# Text = string, Sep = const string
def GrabWord(Text, Sep=' '):
    # TODO: Check if the first position is 0 or 1
    while Text.find(Sep) is 0:
        Text = Text[len(Sep) + 1:]
    p = Text.find(Sep)
    if p < 0:
        # Return string (and string in Python)
        return Text, ''
    else:
        # Return string (and string in Python)
        return Text[:p], Text[len(Sep) + 1:]


# Returns substring between separators deleting that from Text
# Text = string, Sep1, Sep2 = string
def GrabBetween(Text, Sep1, Sep2):
    p1 = Text.find(Sep1)
    p2 = Text.find(Sep2)
    if p1 < p2:
        # Return string (and string in Python)
        return Text[p1 + len(Sep1):p2], (Text[:p1 - 1] + Text[p2 + len(Sep2):])


# First method: Text = string, Sep = const string; Splits using grabword (i.e. skipping repeated separators)
# Second method: Text = string, Words = TStrings, Sep = const string;
#                same as above, but adds result to the Words TStringList, which must have been created by caller
# Third method: Text = string; splits on all whitespace (<=32)
def SplitString(Text, Words=None, Sep=None):
    if Words is not None:
        if Sep is None:
            # First method
            Sep = Words
            Result = []
            while Text is not '':
                PartialText, Text = GrabWord(Text, Sep)
                basetypes.AddToArray(PartialText, Result)
            # Return TSimpleStrings
            return Result
        else:
            # Second method
            while Text is not '':
                PartialText, Text = GrabWord(Text, Sep)
                Words.append(PartialText)
    else:
        # Third method
        s = ''
        Result = []
        for f in range(1, len(Text)):
            # 32 in ASCII is space char
            if ord(Text[f]) > 32:
                s = s + Text[f]
            else:
                if s is not '':
                    basetypes.AddToArray(s, Result)
                    s = ''
        if s is not '':
            basetypes.AddToArray(s, Result)
        # Return TSimpleStrings
        return Result


# Splits string on all separators, including empty strings if repeated separators
# Text = string, Sep = const string
def SplitOnAll(Text, Sep):
    Result = []
    c = len(Sep)
    s = ''
    f = 1
    while f <= len(Text):
        if Text[f:c] is Sep:
            basetypes.AddToArray(s, Result)
            s = ''
            f = f + c
        else:
            s = s + Text[f]
            f = f +1
    # Return TSimpleStrings
    return Result


# Splits on all end of line chars (10) rejecting #13 but keeping empty lines)
# Text = string
def SplitLines(Text):
    s = ''
    Result = []
    for f in range(1, len(Text)):
        # 10 in ASCII is "Line Feed", called also "LF" and "\n"
        if ord(Text[f]) is 10:
            basetypes.AddToArray(s, Result)
            s = ''
        # 13 in ASCII is "Carriage Return", called also "CR" and "\r"
        elif ord(Text[f]) is not 13:
            s = s + Text[f]
    basetypes.AddToArray(s, Result)
    # Return TSimpleStrings
    return Result


# Split one char per string
# Text = string
def SplitChars(Text):
    Result = []
    for f in range(len(Text)):
        Result[f] = Text[f+1]
    # Return TSimpleStrings
    return Result


# First method: AString = string, Start, Finish = Integer, Val = Integer
# Second method: AString = string, Start, Finish = Integer
def GetInteger(AString, Start, Finish, Val=None):
    s = ''
    f = Start
    while (f <= len(AString)) and (f <= Finish):
        if AString[f] is not ' ':
            s = s + AString[f]
        f = f + 1
    try:
        Value = int(s)
        if Val is None:
            # Second method
            # Return Integer
            return Value
        # First method
        # Return Boolean (and integer in Python)
        return True, Value
    except ValueError:
        if Val is None:
            # Second method
            # Return Integer
            return -1
        # First method
        # Return Boolean (and integer in Python)
        return False, 0


# First method: AString = string, Start, Finish = Integer, Val = Double
# Second method: AString = string, Start, Finish = Integer
def GetFloat(AString, Start, Finish, Val=None):
    s = ''
    f = Start
    while (f <= len(AString)) and (f <= Finish):
        if (AString[f] is ',') or (AString[f] is '.'):
            s = s + '.'
        elif AString[f] is not ' ':
            s = s + AString[f]
        f = f + 1
    try:
        Value = float(s)
        if Val is None:
            # Second method
            # Return Double
            return Value
        # First method
        # Return Boolean (and float in Python)
        return True, Value
    except ValueError:
        if Val is None:
            # Second method
            # Return Double
            return -1
        # First method
        # Return Boolean (and float in Python)
        return False, 0


# Returns substring from Start to Finish indexes without spaces
# AString = string, Start, Finish = Integer
def GetString(AString, Start, Finish):
    Result = ''
    f = Start
    while (f <= len(AString)) and (f <= Finish):
        if AString[f] is not ' ':
            Result = Result + AString[f]
        f = f + 1
    # Return Result
    return Result


# Returns last index of string, -1 if not found
# s = string OR Integer OR const string, a = TSimpleStrings OR TIntegers OR const string
def LastIndexOf(s, a):


# -1 if not found
# s = string OR Integer OR const string, a = TSimpleStrings OR TIntegers OR const string
def FirstIndexOf(s, a):


# First index starting with Prefix. Case sensitive
# Prefix = string, SStrings = TSimpleStrings
def FirstByPrefix(Prefix, SStrings):


# First index of string containing Substr
# Substr = string, SStrings = TSimpleStrings
def FirstContaining(Substr, SStrings):


# Returns string after first prefix, or '' if not found
# Substr = string, SStrings = TSimpleStrings
def LookupByPrefix(Substr, SStrings):


# Returns string minus blank spaces (only >32)
# s = string
def Deblank(s):


# Removes all <=' ' characters from the extremities of the string
# S = const string
def TrimmedBlanks(S):


# s = string
def StringAsArray(s):


# ss = TSimpleStrings, filename = string
def SaveToFile(ss, filename):


# SS = TSimpleStrings, Sep = string
def FlattenStrings(SS, Sep):


# Cuts S to first spacer, removing spacer, returns part before spacer. Returns empty string and leaves S if spacer
# is not found
# S = string, Spacer = const string
def SnipString(S, Spacer):


# Cuts S to first spacer, removing spacer, returns part before spacer. Returns S and empties S if spacer is not found
# S = string, Spacer = const string
def SnipStringAll(S, Spacer):


# S = const string, C = const Char
def CountInString(S, C):


# First method: returns S minus all instances of C
# Second method: returns all characters >= space
# S = const string, C = const Char
def CleanString(S, C=None):


# Returns S filled with C until length Len
# S = const string, C = const char, Len = const Integer
def AppendToLength(S, C, Len):


# Converts to current OS
# s = string
def FixLineBreaks(s):


# String comparison indifferent to case
# S1, S2 = string
def UncasedCompare(S1, S2):


# S1 = const TStrings, FirstIx, LastIx = Integer
def AsSimpleStrings(S1, FirstIx=None, LastIx=None):


# SS = TSimpleStrings, FirstIx, LastIx = Integer
def CopyStrings(SS, FirstIx, LastIx):


# Ss = TSimpleStrings, S1 = TStrings
def AppendToStringList(Ss, S1):


# Removes illegal characters from filename
# FileName = string
def CleanFileName(FileName):


# Replace "=HH" hex codes with character of same hex code
# S = string
def ReplaceHexCodes(S):


# Returns position of Substr in Str, ignoring case
# Substr, Str = string, Start = Integer
def PosUncased(Substr, Str, Start=1):


# Returns the string with <tag>... </tag>
# Tag, Text = string
def TagString(Tag, Text):


# Returns the string between tags, deleting from text. If ForceEnd then will force end of field at next Tag
# Tag, Text = string, ForceEnd = Boolean
def GetTaggedField(Tag, Text, ForceEnd=False):


# Returns all fields with this tag
# Tag, Text = string, ForceEnd = Boolean
def GetTaggedFields(Tag, Text, ForceEnd=False):


# Decodes quoted-printable text
# Text = const string
def DecodeQP(Text):


# Converts special & html chars to ascii
# Text = const string
def HtmltoAscii(Text):


# Text = const string
def BasicASCII(Text):


# FileName = const string
def ReadAsSimpleStrings(FileName):


# Decomposes an array of key=value strings into keys and values
# Raw, Keys, Values = TSimpleStrings
def ReadKeysAsValues(Raw, Keys, Values):


# First method: Int, Len = Integer
# Second method: Int = TFloat, Len, Decimal = Integer
def RightJustify(Int, Len, Decimal=None):
    if Decimal is None:

    else:


# Text = string, Len = Integer
def LeftJustify(Text, Len):
