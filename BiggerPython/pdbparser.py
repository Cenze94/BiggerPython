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
        self.AtomID = AtomID
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


# Class of the PDB loader
class TPDBReader:
    # FromFile = string
    def __init__(self, FromFile = ''):
        self.FAtoms = []
        self.FConnections = []
        self.FAtomCount = 0
        self.FModelCount = 0
        self.FChainCount = 0
        self.FChainIDs = []
        if FromFile is not '':
            self.Load(FromFile)

    def IndexChains(self):  # Creates FChainIDs
        self.FChainIDs.clear()
        for f in range(self.FChainCount):
            self.FChainIDs.append('')
        for f in range(len(self.FAtoms)):
            self.FChainIDs[self.FAtoms[f].ChainNum] = self.FAtoms[f].ChainID

    # Only PDB files are expected
    # FromFile = string
    def Load(self, FromFile):
        buf = []
        with(open(FromFile)) as f:
            for line in f:
                buf.append(line)
        self.ReadFromList(buf)

    # s, OldChain = string
    def GetPdbAtom(self, s, OldChain):
        ChainID = stringutils.GetString(s, 22, 22)
        if ChainID is not OldChain:
            self.FChainCount = self.FChainCount + 1
            OldChain = ChainID
        Coords = basetypes.TCoord(stringutils.GetFloat(s, 31, 38), stringutils.GetFloat(s, 39, 46),
                                  stringutils.GetFloat(s, 47, 54))
        Element = stringutils.GetString(s, 77, 78)
        # Sometimes charge comes right after the element name
        if (len(Element) > 1) and not (Element[1].isalpha()):
            Element = Element[0]
        # Return TPDBAtom (and string in Python)
        return TPDBAtom(False,  # IsHet
                        stringutils.GetInteger(s, 7, 11),  # Serial
                        stringutils.Deblank(stringutils.GetString(s, 13, 16)),  # AtomName
                        stringutils.GetString(s, 17, 17),  # AltLoc
                        stringutils.GetString(s, 18, 20),  # ResName
                        ChainID,  # ChainID
                        stringutils.GetInteger(s, 23, 26),  # ResSeq
                        stringutils.GetString(s, 27, 27),  # ICode
                        Coords,  # Coords
                        stringutils.GetFloat(s, 55, 60),  # Occupancy
                        stringutils.GetFloat(s, 55, 66),  # OccTemp
                        stringutils.GetFloat(s, 61, 66),  # Temp
                        Element,  # Element
                        '',  # Charge
                        self.FModelCount,  # ModelNum
                        self.FChainCount), OldChain  # ChainNum

    # s = string
    def GetPdbConnect(self, s):
        '''
        1 - 6 Record name "CONNECT"
        7 - 11 Integer serial Atom serial number
        12 - 16 Integer serial Serial number of bonded atom
        17 - 21 Integer serial Serial number of bonded atom
        22 - 26 Integer serial Serial number of bonded atom
        27 - 31 Integer serial Serial number of bonded atom
        '''

        i = 0
        Result = TPDBConnection(stringutils.GetInteger(s, 7, 11), [])
        b, i = stringutils.GetInteger(s, 12, 16, i)
        if b:
            Result.Connects.append(i)
        b, i = stringutils.GetInteger(s, 17, 21, i)
        if b:
            Result.Connects.append(i)
        b, i = stringutils.GetInteger(s, 22, 26, i)
        if b:
            Result.Connects.append(i)
        b, i = stringutils.GetInteger(s, 27, 31, i)
        if b:
            Result.Connects.append(i)
        # Return TPDBConnection
        return Result

    # Buf = TStringList
    def ReadLines(self, Buf):
        self.FChainCount = -1
        oldchain = '***'
        # FChainCount is increased by GetPDBAtom whenever the chain changes
        for f in range(len(Buf)):
            s = Buf[f]
            if (s.find('ATOM ') is 0) or (s.find('HETATM') is 0):
                atom, oldchain = self.GetPdbAtom(s, oldchain)
                self.FAtoms.append(atom)
                self.FAtoms[self.FAtomCount].IsHet = (s.find('HETATM') is 1)
                self.FAtomCount = self.FAtomCount + 1
            elif s.find('CONNECT') is 1:
                self.FConnections.append(self.GetPdbConnect(s))
            elif s.find('TER') is 1:
                oldchain = '***'
                # This forces an FChainCount increase, even if the chain ID remains the same
            elif s.find('MODEL') is 1:
                oldchain = '***'
                self.FModelCount = int(stringutils.Deblank(s[8:]))
            elif s.find('ENDMDL') is 1:
                self.FModelCount = self.FModelCount + 1

    # Buf = TStringList
    def ReadFromList(self, Buf):
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

        self.Clear()
        self.ReadLines(Buf)
        if self.FAtomCount > 0:
            # In case one TER was missing
            self.FChainCount = self.FAtoms[len(self.FAtoms) - 1].ChainNum + 1
            # 0 if no models. This may be redundant, but with PDB files one never knows...
            self.FModelCount = self.FAtoms[len(self.FAtoms) - 1].ModelNum
            self.IndexChains()

    def Clear(self):
        self.FAtoms = []
        self.FConnections = []
        self.FAtomCount = 0
        self.FModelCount = 0
        self.FChainCount = 0


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
