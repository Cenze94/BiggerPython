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


class TPDBModels:
    def __init__(self, file):
        self.LoadLayer(file)

    def LoadLayer(self, file):
        print("banana")
