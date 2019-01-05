import stringutils
import basetypes
import os


class TOCLConfig:
    # OCLPath, AppPath, AppConfig, OCLConfig, MonomersPath = string, DefaultAtomicRadius = TFloat
    def __init__(self, OCLPath, AppPath, AppConfig, OCLConfig, MonomersPath, DefaultAtomicRadius):
        self.OCLPath = OCLPath
        self.AppPath = AppPath
        self.AppConfig = AppConfig
        self.OCLConfig = OCLConfig
        self.MonomersPath = MonomersPath
        self.DefaultAtomicRadius = DefaultAtomicRadius


class TOCLAtomData:
    # Elements sorted by atomic number (= to index +1)
    # Symbol, Name = string, CovalentRadius, VdWRadius, UnitedRadius = TFloat, CPKColor = TCoord
    def __init__(self, Symbol, Name, CovalentRadius, VdWRadius, UnitedRadius, CPKColor):
        self.Symbol = Symbol
        self.Name = Name
        self.CovalentRadius = CovalentRadius
        self.VdWRadius = VdWRadius
        self.UnitedRadius = UnitedRadius
        self.CPKColor = CPKColor


class TOCLAAData:
    # Name, TLCode = string, OLCode, Category = Char, Hydropathy = Single (it's like a double but with 4 bytes
    # instead of 8)
    def __init__(self, Name, TLCode, OLCode, Category, Hydropathy):
        self.Name = Name
        self.TLCode = TLCode
        self.OLCode = OLCode
        self.Category = Category
        self.Hydropathy = Hydropathy


AtomDataFile = 'atomdata.txt'
AADataFile = 'aminoaciddata.txt'
Config = None
AtomData = []
AAData = []


# Remove comments, which are recognized by the '#' char
# S1 = TStringList
def RemoveComments(S1):
    f = 0
    while f < len(S1):
        ix = S1[f].find('#')
        if ix < 0:
            f = f + 1
        elif ix is 0:
            del S1[f]
        else:
            del S1[f][ix:]
            f = f + 1


def DefaultConfig():
    # In Python is stored the script name, while data will be stored in the Data folder
    global Config
    exename = os.path.basename('chemera')
    AppPath = os.path.dirname(__file__)
    OCLPath = os.path.join(AppPath, "Data")
    OCLConfig = os.path.join(OCLPath, "oclibrary.ini")
    AppPath = os.path.join(AppPath, exename)
    AppConfig = os.path.join(AppPath, "chemera.ini")
    MonomersPath = os.path.join(OCLPath, "monomers\\")
    DefaultAtomicRadius = 1.5
    Config = TOCLConfig(OCLPath, AppPath, AppConfig, OCLConfig, MonomersPath, DefaultAtomicRadius)


def LoadAtomData():
    global AtomData
    global Config
    global AtomDataFile
    try:
        with open(os.path.join(Config.OCLPath, AtomDataFile), 'r') as f:
            s1 = f.readlines()
        RemoveComments(s1)
        for f in range(len(s1)):
            tmp = stringutils.SplitString(s1[f], chr(9))
            AtomData.append(TOCLAtomData(tmp[0], tmp[4], float(tmp[1]), float(tmp[2]), float(tmp[3]),
                                         basetypes.Coord(int(tmp[5]) / 255, int(tmp[6]) / 255, int(tmp[7])
                                                         / 255)))
    except:
        raise Exception('Error loading ' + os.path.join(Config.OCLPath, AtomDataFile))


def LoadAAData():
    global AADataFile
    global Config
    global AAData
    try:
        with open(os.path.join(Config.OCLPath, AADataFile), 'r') as f:
            s1 = f.readlines()
        RemoveComments(s1)
        for f in range(len(s1)):
            tmp = stringutils.SplitString(s1[f], chr(9))
            AAData.append(TOCLAAData(tmp[0], tmp[1], tmp[2][0], tmp[3][0], float(tmp[4])))
    except:
        raise Exception('Error loading ' + os.path.join(Config.OCLPath, AADataFile))


# Symbol = string
def AtomicNumber(Symbol):
    global AtomData
    Result = len(AtomData) - 1
    while (Result >= 0) and (AtomData[Result].Symbol is not Symbol):
        Result = Result - 1
    if Result >= 0:
        # Return Integer
        return Result + 1
    # Return Integer
    return Result


# AtomicNumber = Integer
def Element(AtomicNumber):
    global AtomData
    if (AtomicNumber > 0) and (AtomicNumber <= len(AtomData)):
        # Return string
        return AtomData[AtomicNumber - 1].Symbol
    else:
        # Return string
        return '??'


# AtomicNumber = Integer
def VdWRadius(AtomicNumber):
    global AtomData
    global Config
    if AtomicNumber < 0:
        # Return TFloat
        return Config.DefaultAtomicRadius
    # Return TFloat
    return AtomData[AtomicNumber - 1].VdWRadius


# Code = string
def AAIndex(Code):
    global AAData
    Result = len(AAData) - 1
    if len(Code) is 3:
        while (Result >= 0) and (AAData[Result].TLCode is not Code):
            Result = Result - 1
    else:
        while (Result >= 0) and (AAData[Result].OLCode is not Code):
            Result = Result - 1
    # Return Integer
    return Result


# TCL = string
def AAOneLetterCode(TLC):
    global AAData
    ix = AAIndex(TLC)
    if ix >= 0:
        # Return string
        return AAData[ix].OLCode
    else:
        # Return string
        return ''
