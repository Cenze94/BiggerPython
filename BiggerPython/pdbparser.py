import stringutils
import basetypes


class TPDBAtom:
    # IsHet = Boolean, Serial = Integer, AtomName = string[4], AltLoc = string[1], ResName = string[3]
    # ChainID = string[1], ResSeq = Integer, ICode = string[1], Coords = TCoord, Occupancy = Single, OccTemp = Single,
    # Temp = Single, Element = string[2], Charge = string[2], ModelNum, ChainNum = Integer
    def __init__(self, IsHet, Serial, AtomName, AltLoc, ResName, ChainID, ResSeq, ICode, Coords, Occupancy, OccTemp,
                 Temp, Element, Charge, ModelNum, ChainNum):
        self.IsHet = IsHet  # Heteroatom
        self.Serial = Serial
        self.AtomName = AtomName
        self.AltLoc = AltLoc
        self.ResName = ResName
        self.ChainID = ChainID
        self.ResSeq = ResSeq
        self.ICode = ICode
        self.Coords = Coords
        self.Occupancy = Occupancy
        self.OccTemp = OccTemp  # Merge of occupation and temp, as used in some cases
        self.Temp = Temp
        self.Element = Element
        self.Charge = Charge
        self.ModelNum = ModelNum  # Given by MODEL or 1 if no MODEL; incremented by ENDMDL
        self.ChainNum = ChainNum  # Count incremented on TER, starting at 0. Can be used to index the chainIDs


class TPDBConnection:
    # AtomID = Integer, Connects = TIntegers
    def __init__(self, AtomID, Connects):
        self. AtomID = AtomID
        self.Connects = Connects


class TPDBInfo:
    # Header, Title, Compound, Source, KeyWords, ExpTechnique, Author, Journal, Remarks, UserComments = TStrings
    def __init__(self, Header, Title, Compound, Source, KeyWords, ExpTechnique, Author, Journal, Remarks,
                 UserComments):
        self.Header = Header
        self.Title = Title
        self.Compound = Compound
        self.Source = Source
        self.KeyWords = KeyWords
        self.ExpTechnique = ExpTechnique
        self.Author = Author
        self.Journal = Journal
        self.Remarks = Remarks
        self.UserComments = UserComments


class TPDBReader:
    # FromFile = string
    def __init__(self, FromFile = ''):
        if FromFile is not '':
            self.Load(FromFile)

    def IndexChains(self):  # Creates FChainIDs

    # FromFile = string
    def Load(self, FromFile):

    # Buf = TStringList
    def ReadFromList(self, Buf):

    def Clear(self):
'''
1 - 6          Record name     "ATOM"
7 - 11         Integer         serial        Atom serial number
13 - 16        Atom            name          Atom name
17             Character       altLoc        Alternate location indicator
18 - 20        Residue name    resName       Residue name
22             Character       chainID       Chain identifier
23 - 26        Integer         resSeq        Residue sequence number
27             Character       iCode         Code for insertion of residues
31 - 38        Real(8.3)       x             Orthogonal coordinates for X
39 - 46        Real(8.3)       y             Orthogonal coordinates for Y
47 - 54        Real(8.3)       z             Orthogonal coordinates for Z
55 - 60        Real(6.2)       occupancy     Occupancy
61 - 66        Real(6.2)       tempFactor    Temperature factor
68 - 70        Integer         ftNote        Footnote number, being deprecated
73 - 76        LString(4)      segID         Segment identifier, left-justified
77 - 78        LString(2)      element       Element symbol, right-justified
79 - 80        LString(2)      charge        Charge on the atom, IUPAC form
'''
# AtomName, ResName, ChainName = string, AtomId, ResId = Integer, Position = TCoord, Element = string,
# Occupancy, Temperature = TFloat
def AtomRecord(AtomName, ResName, ChainName, AtomId, ResId, Position, Element, Occupancy = 1.0, Temperature = 1.0):
    if len(AtomName) < 4:
        AtomName = ' ' + AtomName
        # This seems to be the rule on pdb files, although that is not what the documentation states
    # Return string
    return 'ATOM  ' + \
        stringutils.RightJustify(AtomId, 5) + ' ' + \
        stringutils.LeftJustify(AtomName, 4) + ' ' + \
        stringutils.LeftJustify(ResName, 4) + ' ' + \
        stringutils.LeftJustify(ChainName, 1) + \
        stringutils.RightJustify(ResId, 4) + '    ' + \
        stringutils.RightJustify(Position[0], 8, 3) + \
        stringutils.RightJustify(Position[1], 8, 3) + \
        stringutils.RightJustify(Position[2], 8, 3) + \
        stringutils.RightJustify(Occupancy, 6, 2) + \
        stringutils.RightJustify(Temperature, 6, 2) + '           ' + \
        stringutils.LeftJustify(Element, 2)
