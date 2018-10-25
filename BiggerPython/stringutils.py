

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
        # Return string
        return Text, ''
    else:
        # Return string
        return Text[:p], Text[len(Sep) + 1:]


# Returns substring between separators deleting that from Text
# Text = string, Sep1, Sep2 = string
def GrabBetween(Text, Sep1, Sep2):


# First method: Text = string, Sep = const string; Splits using grabword (i.e. skipping repeated separators)
# Second method: Text = string, Words = TStrings, Sep = const string;
#                same as above, but adds result to the Words TStringList, which must have been created by caller
# Third method: Text = string; splits on all whitespace (<=32)
def SplitString(Text, Words=None, Sep=None):
    if Sep is None:
        if Words is None:
            # First method
        else:
            # Second method
            Sep = Words
    else:


# Splits string on all separators, including empty strings if repeated separators
# Text = string, Sep = const string
def SplitOnAll(Text, Sep):


# Splits on all end of line chars (10) rejecting #13 but keeping empty lines)
# Text = string
def SplitLines(Text):


# Split one char per string
# Text = string
def SplitChars(Text):


# AString = string, Start, Finish = Integer, Val = Integer
def GetInteger(AString, Start, Finish, Val=None):


# AString = string, Start, Finish = Integer, Val = Double
def GetFloat(AString, Start, Finish, Val=None):


# AString = string, Start, Finish = Integer
def GetString(AString, Start, Finish):


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
