import stringutils
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


# Remove comments, which are recognized by the '#' char
# S1 = TStringList
def RemoveComments(S1):
    f = 0
    while f < len(S1):
        ix = S1[f].find('#')
        if ix < 1:
            ix = ix + 1
        elif ix is 1:
            del S1[f]
        else:
            del S1[f][ix:]
            f = f + 1

# TODO: Complete this method if it's necessary
'''
def DefaultConfig():
    DecimalSeparator = '.'
    # In Python is stored the script name
    exename = os.path.basename(__file__)
    OCLPath = 
    Config = TOCLConfig()
'''


def LoadAtomData():


def LoadAAData():


# Symbol = string
def AtomicNumber(Symbol):


# AtomicNumber = Integer
def Element(AtomicNumber):


# AtomicNumber = Integer
def VdWRadius(AtomicNumber):


# Code = string
def AAIndex(Code):


# TCL = string
def AAOneLetterCode(TCL):
