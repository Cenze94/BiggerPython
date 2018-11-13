import os


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
        Text = Text[len(Sep):]
    p = Text.find(Sep)
    if p < 0:
        # Return string (and string in Python)
        return Text, ''
    else:
        # Return string (and string in Python)
        return Text[:p], Text[p + 1:]


# Returns substring between separators deleting that from Text
# Text = string, Sep1, Sep2 = string
def GrabBetween(Text, Sep1, Sep2):
    p1 = Text.find(Sep1)
    p2 = Text.find(Sep2)
    if p1 < p2:
        # Return string (and string in Python)
        return Text[p1 + len(Sep1):p2], (Text[:p1] + Text[p2 + len(Sep2):])


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
                Result.append(PartialText)
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
        for f in range(len(Text)):
            # 32 in ASCII is space char
            if ord(Text[f]) > 32:
                s = s + Text[f]
            else:
                if s is not '':
                    Result.append(s)
                    s = ''
        if s is not '':
            Result.append(s)
        # Return TSimpleStrings
        return Result


# Splits string on all separators, including empty strings if repeated separators
# Text = string, Sep = const string
def SplitOnAll(Text, Sep):
    Result = []
    c = len(Sep)
    s = ''
    f = 0
    while f <= len(Text):
        if Text[f:c] is Sep:
            Result.append(s)
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
    for f in range(len(Text)):
        # 10 in ASCII is "Line Feed", called also "LF" and "\n"
        if ord(Text[f]) is 10:
            Result.append(s)
            s = ''
        # 13 in ASCII is "Carriage Return", called also "CR" and "\r"
        elif ord(Text[f]) is not 13:
            s = s + Text[f]
    Result.append(s)
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
    if isinstance(a, list):
        Result = len(a) - 1
        while (Result >= 0) and (a[Result] is not s):
            Result = Result - 1
        # Return Integer
        return Result
    else:
        # The original version of this function is wrong, because it checks only the first character of s instead of
        # the whole word, so a word with the same initial character of one of the words of a will return a positive
        # result. This version is not corrected to avoid different behaviours from the original program, besides
        # probably it'isnt an important function, otherwise it should be correct
        # s = substring, a = string
        leng = len(s)
        for f in range(len(a) - len(s), -1, -1):
            g = 0
            while (g <= leng) and (s[g] is a[f + g]):
                g = g + 1
            if g < leng:
                # Return Integer
                return f + 1
    # Return Integer
    return -1


# -1 if not found
# s = string OR Integer OR const string, a = TSimpleStrings OR TIntegers OR const string
def FirstIndexOf(s, a):
    if isinstance(a, list):
        for f in range(len(a)):
            if s is a[f]:
                # Return Integer
                return f
    else:
        # s = substring, a = string
        leng = len(s)
        for f in range(len(a) - len(s)):
            g = 0
            while (g <= leng) and (s[g] is a[f + g]):
                g = g + 1
            if g < leng:
                # Return Integer
                return f + 1
    # Return Integer
    return -1


# First index starting with Prefix. Case sensitive
# Prefix = string, SStrings = TSimpleStrings
def FirstByPrefix(Prefix, SStrings):
    for f in range(len(SStrings)):
        if SStrings[f].find(Prefix) is 1:
            # Return Integer
            return f
    # Return Integer
    return -1


# First index of string containing Substr
# Substr = string, SStrings = TSimpleStrings
def FirstContaining(Substr, SStrings):
    for f in range(len(SStrings)):
        if SStrings[f].find(Substr) > 0:
            # Return Integer
            return f
    # Return Integer
    return -1


# Returns string after first prefix, or '' if not found
# Prefix = string, SStrings = TSimpleStrings
def LookupByPrefix(Prefix, SStrings):
    ix = FirstByPrefix(Prefix, SStrings)
    if ix < 0:
        # Return string
        return ''
    else:
        # Return string
        return SStrings[ix[len(Prefix):len(SStrings) - 1]]


# Returns string minus blank spaces (only >32)
# s = string
def Deblank(s):
    Result = ''
    for f in range(len(s)):
        if s[f] > ' ':
            Result = Result + s[f]
    # Return string
    return Result


# Removes all <=' ' characters from the extremities of the string
# S = const string
def TrimmedBlanks(S):
    ix2 = len(S)
    ix1 = 1
    # Find first non space
    while (ix1 < ix2) and (S[ix1] <= ' '):
        ix1 = ix1 + 1
    # Find last non space
    while (ix2 > 0) and S[ix2] <= ' ':
        ix2 = ix2 - 1
    if ix2 >= ix1:
        # Return string
        return S[ix1:ix2]
    else:
        # Return string
        return ''


# s = string
def StringAsArray(s):
    # Return TSimpleStrings
    return [s]


# Saves strings in a specific file, separated by end line char
# ss = TSimpleStrings, filename = string
def SaveToFile(ss, filename):
    with open(filename, 'w') as f:
        for item in ss:
            f.write("%s\n" % item)


# Save strings of a list in a single container, separated by Sep
# SS = TSimpleStrings, Sep = string
def FlattenStrings(SS, Sep):
    if len(SS) > 0:
        Result = SS[0]
        for f in range(1, len(SS)):
            Result = Result + Sep + SS[f]
        # Return string
        return Result
    # Return string
    return ''


# Cuts S to first spacer, removing spacer, returns part before spacer. Returns empty string and leaves S if spacer
# is not found
# S = string, Spacer = const string
def SnipString(S, Spacer):
    ix = S.find(Spacer)
    if ix > 0:
        # Return string (and string in Python)
        return S[:ix - 1], S[ix + len(Spacer):]
    # Return string (and string in Python)
    return '', S


# Cuts S to first spacer, removing spacer, returns part before spacer. Returns S and empties S if spacer is not found
# S = string, Spacer = const string
def SnipStringAll(S, Spacer):
    Result, S = SnipString(S, Spacer)
    if Result is '':
        # Return string (and string in Python)
        return S, ''
    # Return string (and string in Python)
    return Result, S


# S = const string, C = const Char
def CountInString(S, C):
    Result = 0
    for f in range(len(S)):
        if S[f] is C:
            Result = Result + 1
    # Return Integer
    return Result


# First method: returns S minus all instances of C
# Second method: returns all characters >= space
# S = const string, C = const Char
def CleanString(S, C=None):
    Result = ''
    if C is not None:
        # First method
        for f in range(len(S)):
            if S[f] is not C:
                Result = Result + S[f]
    else:
        # Second method
        for f in range(len(S)):
            if S[f] >= ' ':
                Result = Result + S[f]
    # Return string
    return Result


# Returns S filled with C until length Len
# S = const string, C = const char, Len = const Integer
def AppendToLength(S, C, Len):
    lens = len(S)
    if lens >= Len:
        # Nothing to append
        # Return string
        return S
    Result = ''
    for f in range(lens):
        Result = Result + S[f]
    for f in range(lens, Len):
        Result = Result + C
    # Return string
    return Result


# Converts to current OS
# s = string
def FixLineBreaks(s):
    Result = ''
    for f in range(len(s)):
        if ord(s[f]) is 10:
            Result = Result + os.linesep
        else:
            Result = Result + s[f]
    # return string
    return Result


# String comparison indifferent to case
# S1, S2 = string
def UncasedCompare(S1, S2):
    # String comparison indifferent to case
    # Return Boolean
    return S1.upper() is S2.upper()


# S1 = const TStrings, FirstIx, LastIx = Integer
def AsSimpleStrings(S1, FirstIx=None, LastIx=None):
    if FirstIx is None:
        # First method
        # Originally this method was used to convert a TStrings type to a TSimpleStrings one, in Python they are the
        # same
        # Return TSimpleStrings
        return S1
    else:
        # Second method
        # This method is the same of the previous case but for strings with an index that is between FirstIx and
        # LastIx
        # Return TSimpleStrings
        return S1[FirstIx:LastIx]


# Return a new list with the strings of SS which index is between FirstIx and LastIx
# SS = TSimpleStrings, FirstIx, LastIx = Integer
def CopyStrings(SS, FirstIx, LastIx):
    # Return TSimpleStrings
    return SS[FirstIx:LastIx]


# Append Ss strings to S1 list
# Ss = TSimpleStrings, S1 = TStrings
def AppendToStringList(Ss, S1):
    for f in range(Ss):
        S1.append(Ss[f])


# Removes illegal characters from filename
# FileName = string
def CleanFileName(FileName):
    Result = []
    c = 0
    for f in range(len(FileName)):
        if (FileName[f] >= ' ') and (FileName[f] is not '/') and (FileName[f] is not '?') \
            and (FileName[f] is not '<') and (FileName[f] is not '>') and (FileName[f] is not '\\') \
            and (FileName[f] is not ':') and (FileName[f] is not '*') and (FileName[f] is not '|') \
            and (FileName[f] is not '"') and (FileName[f] is not '^') and (FileName[f] is not "'"):
            c = c + 1
            Result[c] = FileName[f]
    # Return string
    return ''.join(Result)


# Utility function for the next method
def CheckHex(c):
    if (c is not '0') and (c is not '1') and (c is not '2') and (c is not '3') and (c is not '4') and (c is not '5') \
        and (c is not '6') and (c is not '7') and (c is not '8') and (c is not '9') and (c is not 'A') \
        and (c is not 'B') and (c is not 'C') and (c is not 'D') and (c is not 'E') and (c is not 'F'):
        return False
    return True


# Replace "=HH" hex codes with character of same hex code
# S = string
def ReplaceHexCodes(S):
    Result = ''
    f = 1
    while f <= len(S):
        if (f <= len(S) - 2) and (S[f] is '=') and CheckHex(S[f+1]) and CheckHex(S[f+2]):
            Result = Result + str(int(S[f+1] + S[f+2], 16))
            f = f + 3
        else:
            Result = Result + S[f]
            f = f + 1
    # Return string
    return Result


# Returns position of Substr in Str, ignoring case (unless Substr contains uppercase)
# Substr, Str = string, Start = Integer
def PosUncased(Substr, Str, Start=0):
    ucsubstr = Substr
    ucsubstr.upper()
    Result = -1
    last = len(Substr)
    max = len(Str) - last
    f = Start - 1
    while (Result < 0) and (f < max):
        Result = f + 1
        for g in range(last, 1, -1):
            if (Substr[g] is not Str[f + g]) and (ucsubstr[g] is not Str[f + g]):
                Result = -1
                break
        f = f +1
    # Return Integer
    return Result


# Returns the string with <tag>... </tag>
# Tag, Text = string
def TagString(Tag, Text):
    # Return string
    return '<' + Tag + '>' + Text + '</' + Tag + '>'


# Returns the string between tags, deleting from text. If ForceEnd then will force end of field at next Tag
# Tag, Text = string, ForceEnd = Boolean
def GetTaggedField(Tag, Text, ForceEnd=False):
    pstart = PosUncased('<' + Tag + '>', Text)
    staglen = len(Tag) + 2
    pend = PosUncased('</' + Tag + '>', Text, pstart + staglen + 1)
    etaglen = len(Tag) + 3
    if ForceEnd:
        pnext = PosUncased('<' + Tag + '>', Text, pstart + 3 + len(Tag))
        if (pend < 1) or (pnext < pend):
            pend = pnext
            etaglen = len(Tag) + 2
        if pend < 1:
            pend = len(Text)
            etaglen = 0
    Result = ''
    if (pstart > 0) and (pend > 0) and (pend > pstart):
        Result = Text[pstart + staglen:pend]
        del Text[pstart:pend + etaglen - 1]
    # Return string
    return Result


# Returns all fields with this tag
# Tag, Text = string, ForceEnd = Boolean
def GetTaggedFields(Tag, Text, ForceEnd=False):
    Result = []
    fld = GetTaggedField(Tag, Text, ForceEnd)
    while fld is not '':
        Result.append(fld)
        fld = GetTaggedField(Tag, Text, ForceEnd)
    # Return TSimpleStrings
    return Result


# Decodes quoted-printable text
# Text = const string
def DecodeQP(Text):
    totlen = len(Text)
    Result = []
    current = 0
    f = 0
    while f < totlen:
        if (Text[f] is '=') and (f < totlen - 1):
            if Text[f + 1] < ' ':
                f = f + 2
                if Text[f] < ' ':
                    f = f + 1 # Skip lines with cr+nl
            else:
                current = current + 1
                if CheckHex(Text[f+1]) and CheckHex(Text[f+2]) and f < totlen - 2:
                    Result[current] = str(int(Text[f + 1] + Text[f + 2], 16))
                    f = f + 3
                    # Check if line break was replaced by space, but still broken in file
                    if Result[current] is ' ':
                        if Text[f] < ' ':
                            f = f + 1
                        if Text[f] < ' ':
                            f = f + 1
                else:
                    current = current + 1
                    Result[current] = Text[f]
                    f = f + 1
        else:
            current = current + 1
            Result[current] = Text[f]
            f = f + 1
    # Return string
    return ''.join(Result)


# Utility function for the next one
# s = string
def ConvertHtml(s):
    s.replace('<', '&lt')
    s.replace('>', '&gt')
    s.replace('<br/>', chr(10) + chr(13))
    s.replace('</div><div>', chr(10) + chr(13))
    s.replace('</div>', chr(10) + chr(13))
    s.replace('<div>', chr(10) + chr(13))
    s.replace('<br>', chr(10) + chr(13))
    s.replace('&quot;', '"')
    s.replace('&amp;', '&')
    s.replace('&apos;', "'")
    s.replace('&nbsp;', ' ')


# Converts special & html chars to ascii
# Text = const string
def HtmltoAscii(Text):
    totlen = len(Text)
    Result = []
    current = 0
    f = 1
    while f < totlen - 1:
        if (Text[f] is '&') and (Text[f+1] is '#'):
            f = f + 2
            tmp = ''
            while (f < totlen) and (Text[f] is not ';'):
                tmp = tmp + Text[f]
                f = f + 1
            f = f + 1
            try:
                Result[current] = chr(int(tmp))
                current = current + 1
            except ValueError:
                pass
        else:
            if Text[f] is '<':
                istag = True
                while Text[f] is not '>':
                    if Text[f] is ' ':
                        istag = False
                    if istag:
                        Result[current] = Text[f]
                        current = current + 1
                    f = f + 1
                Result[current] = Text[f]
                current = current + 1
                f = f +1
            else:
                Result[current] = Text[f]
                current = current + 1
                f = f + 1
    Result = ''.join(Result)
    ConvertHtml(Result)
    # return string
    return ''.join(Result)


# Text = const string
def BasicASCII(Text):
    c = 0
    Result = []
    for f in range(len(Text)):
        if int(Text[f]) <= 127:
            c = c - 1
            Result[c] = Text[f]
    # Return string
    return ''.join(Result)


# FileName = const string
def ReadAsSimpleStrings(FileName):
    with open(FileName, 'r') as f:
        Result = f.readlines()
    # Return TSimpleStrings
    return Result


# Decomposes an array of key=value strings into keys and values
# Raw, Keys, Values = TSimpleStrings
def ReadKeysAsValues(Raw, Keys, Values):
    for f in range(len(Raw)):
        s = Raw[f]
        if s.find(';') > 0:
            del s[s.find(';'):]
        if s.find('=') > 0:
            tmp = SplitString(s, '=')
            Keys.append(TrimmedBlanks(tmp[0]))
            if len(Keys) > 1:
                Values.append(TrimmedBlanks(tmp[1]))
            else:
                Values.append('')


# First method: Int, Len = Integer
# Second method: Int = TFloat, Len, Decimal = Integer
def RightJustify(Int, Len, Decimal=None):
    if Decimal is None:
        Result = []
        tmp = str(Int)
        dif = Len - len(tmp)
        for f in range(Len):
            if f - dif > 0:
                Result[f] = tmp[f - dif]
            else:
                Result[f] = ' '
        # Return string
        return ''.join(Result)
    else:
        Result = []
        tmp = ('{:' + Decimal + 'f}').format(Int)
        dif = Len - len(tmp)
        for f in range(Len):
            if f - dif > 0:
                Result[f] = tmp[f - dif]
            else:
                Result[f] = ' '
        # Return string
        return ''.join(Result)


# Text = string, Len = Integer
def LeftJustify(Text, Len):
    Result = []
    for f in range(Len):
        if f <= len(Text):
            Result[f] = Text[f]
        else:
            Result[f] = ' '
    # Return string
    return ''.join(Result)
