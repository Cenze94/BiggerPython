from enum import Enum


# Enumerations, I don't know what is their purpose
class PDBChargeOrigin(Enum):
    pdbNone = 1,
    pdbOccTemp = 2
    pdbOccupancy = 3
    pdbCharge = 4


class PDBResidueTypes(Enum):
    resAA = 1,
    resNonAA = 2


class TTemplate:
    # Name = String, Atoms = TSimpleStrings, AtomicNumbers = TIntegers, AtomIDs = TIntegers, Coords = TCoords,
    # Connects = array of TIntegers
    def __init__(self, Name, Atoms, AtomicNumbers, AtomIDs, Coords, Connects):
        self.Name = Name
        self.Atoms = Atoms
        self.AtomicNumbers = AtomicNumbers
        self.AtomIDs = AtomIDs
        self.Coords = Coords
        self.Connects = Connects


class TPDBModel:
    # templates = TTemplates, AID = Integer
    def __init__(self, templates, AID):
        self.FTemplates = templates
        self.FProtein = TMolecule.Create('', AID, None)

    def Free(self):
        if self is not None:
            self.ClearChains()

    def ClearChains(self):
        self.FProtein.CleanUp()

    # IDs = TSimpleStrings
    def CreateChains(self, IDs):
        # TODO: check IDs type, because if it is an ordinal type (e. g. a number) range must stop at the highest value
        for f in range(IDs):
            self.FProtein.NewGroup(IDs[f], f+1)

    # Options = PDBResidueTypes, ResTypes = TSimpleStrings, OnDelete = TOnDeleteCallback
    def DeleteResidues(self, Options, ResTypes, OnDelete=None):
        for c in range(self.FProtein.GroupCount-1):
            # chain = TMolecule
            chain = self.FProtein.GetGroup(c)
            chain.TagAllAtoms(0)
            for r in range(chain.GroupCount-1):
                # res = TMolecules
                res = chain.GetGroup(r)
                if ((Options.name is 'resAA') and (AAOneLetterCode(res.Name) is not '')) or \
                        ((Options.name is 'resNonAA') and (AAOneLetterCode(res.Name) is '')) or \
                        (LastIndexOf(res.Name, ResTypes) > 0):
                    res.TagAllAtoms(1)
            chain.DeleteTaggedAtoms(1, OnDelete)
        self.FProtein.DeleteEmptyGroups()

    # ChainName = string, ChainID = integer
    def NewEmptyChain(self, ChainName, ChainID):
        # Return TMolecule
        return self.FProtein.NewGroup(ChainName, ChainID)

    # ChainName = string, ChainID = integer, Size = integer
    def NewChain(self, ChainName, ChainID, Size):
        # Return TMolecule
        Result = self.NewEmptyChain(ChainName, ChainID)
        Result.CreateEmptyGroups(Size)
        return Result

    # ChainIx = integer OR ChainIX = string
    def GetChain(self, ChainIx):
        return self.FProtein.GetGroup(ChainIx)

    # Chain = TMolecule
    def AppendChain(self, Chain):
        self.FProtein.AddGroup(Chain)

    # ChainIx = string OR ChainIx = integer, ResIx = integer OR ResIx = Integer
    def GetResidue(self, ChainIx, ResIx):
        if isinstance(ChainIx, int):
            # Result = TMolecule
            Result = self.FProtein.GetGroup(ChainIx)
            if Result is not None:
                return Result.GetGroup(ResIx)
        else:
            # Result = TMolecule
            Result = self.FProtein.GetChain(ChainIx)
            if Result is not None:
                return Result.GetGroupById(ResIx)

    # ChainIx = integer, ResIx = integer, AtomIx = integer
    def GetAtom(self, ChainIx, ResIx, AtomIx):
        # mol = TMolecule
        mol = self.FProtein.GetGroup(ChainIx)
        if mol is not None:
            mol = mol.GetGroup(ResIx)
            if mol is not None:
                # return TAtom
                return mol.GetAtom(AtomIx)
        return None

    # PdbFileName = string, ChargeFrom = PDBChargeOrigin
    def LoadPDB(self, PdbFileName, ChargeFrom):
        self.ClearChains()
        self.FFileName = PdbFileName
        # parser = TPDBReader
        parser = TPDBReader.Create(PdbFileName)
        self.FProtein.Name = self.ChangeFileExt(self.ExtractFileName(PdbFileName), '')
        self.CreateChains(parser.ChainIDs)

        # Read atoms
        # cc = integer
        cc = -1  # Current chain
        # TODO: check parser.Atoms type, because if it is an ordinal type (e. g. a number) range must stop at the
        # highest value
        for f in range(parser.Atoms):
            patom = parser.Atoms[f]
            cr = 0  # Originally there wasn't an initialization of cr
            if cc is not patom.ChainNum():  # ChainNumber is always >= 0
                cc = patom.ChainName()
                # cr = integer
                cr = -1  # Current residue number, also >= 0
            if patom.ResSeq is not cr:  # Get new residue
                cres = self.FProtein.GetGroup(cc).NewGroup(patom.ResName, patom.ResSeq)
                cr = patom.ResSeq
            atom = cres.NewAtom(patom.AtomName, patom.Serial)
            atom.Coords = patom.Coords
            # Element may be present in the PDB file. However, this is superseded if there is monomer template data
            # AtomicNumber is a function from "oclconfiguration"
            atom.AtomicNumber = AtomicNumber(patom.Element)
            # VdWRadius is a function of "oclconfiguration"
            atom.Radius = VdWRadius(atom.AtomicNumber)
            if ChargeFrom is PDBChargeOrigin.pdbOccupancy:
                atom.Charge = TPDBAtom.Occupancy
            elif ChargeFrom is PDBChargeOrigin.pdbOccTemp:
                atom.Charge = TPDBAtom.OccTemp
            elif ChargeFrom is PDBChargeOrigin.pdbCharge:
                if len(patom.Charge) is 2:
                    atom.Charge = patom.Charge[1]
                if patom.Charge[2] is '-':
                    atom.Charge = -atom.Charge
        # Assign element, radius, charge, et al, from the monomer templates will replace information on PDB
        self.AssignAtomicData()
        # Return TMolecule
        return self.FProtein

    # ATemplates = TTemplates
    def ResetTemplates(self, ATemplates):
        self.FTemplates = ATemplates

    # Return Integer
    def ChainCount(self):
        return self.FProtein.GroupCount

    # Return TSimpleStrings
    def ListChains(self):
        return self.FProtein.ListGroupNames

    # FirstChar = Char, LastChar = Char
    def RenameChains(self, FirstChar, LastChar):
        name = FirstChar
        for f in range(self.FProtein.GroupCount - 1):
            self.FProtein.GetGroup(f).Name = name
            name = ord(name) + 1
            if name > LastChar:
                name = FirstChar

    # ChainIx = Integer, return Integer
    def ResidueCount(self, ChainIx):
        return self.FProtein.GetGroup(ChainIx).GroupCount()

    def AssignAtomicData(self):
        # atoms = TAtoms
        atoms = self.FProtein.AllAtoms()
        # TODO: check parser.Atoms type, because if it is an ordinal type (e. g. a number) range must stop at the
        # highest value
        for f in range(atoms):
            # tempix = Integer
            tempix = self.TemplateIx(atoms[f].Parent.Name)
            if tempix >= 0:
                # atomix = Integer, LastIndexOf is a function of "stringutils"
                atomix = LastIndexOf(atoms[f].Name, self.FTemplates[tempix].Atoms)
                if atomix >= 0:
                    atoms[f].AtomicNumber = self.FTemplates[tempix].AtomicNumbers[atomix]
                    if atoms[f].Atomicumber > 0:
                        # AtomData is an array of "oclconfiguration"
                        atoms[f].Radius = AtomData[atoms[f].AtomicNumber - 1].VdWradius


class TPDBModels:
    def __init__(self, file):
        self.LoadLayer(file)

    def LoadLayer(self, file):
        print("banana")
