

# Returns substring to separator. Text is not altered
# Text, Sep = const string
def CopyWord(Text, Sep):


# Deletes separator if found at beginning. Returns substring to separator deleting that from Text
# Text = string, Sep = const string
def Grabword(Text, Sep):


# Returns substring between separators deleting that from Text
# Text = string, Sep1, Sep2 = string
def GrabBetween(Text, Sep1, Sep2):


# First method: Text = string, Sep = const string; Splits using grabword (i.e. skipping repeated separators)
# Second method: Text = string, Words = TStrings, Sep = const string;
#                same as above, but adds result to the Words TStringList, which must have been created by caller
# Third method: Text = string; splits on all whitespace (<=32)
def SplitString(Text, Words=None, Sep=None):
