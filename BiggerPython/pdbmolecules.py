from enum import Enum
import os
import glob
import basetypes
import stringutils


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
    # Templates = TTemplates, AID = Integer
    def __init__(self, Templates, AID):
        self.FTemplates = Templates
        self.FProtein = TMolecule('', AID, None)
        self.FInfo = TPDBInfo
        self.FFileName = ""

    def __del__(self):
        if self is not None:
            self.ClearChains()

    def ClearChains(self):
        self.FProtein.CleanUp()

    # IDs = TSimpleStrings
    def CreateChains(self, IDs):
        # TODO: check IDs type, because if it is an ordinal type (e. g. a number) range must stop at the highest value
        for f in range(IDs):
            self.FProtein.NewGroup(IDs[f], f+1)

    # First method: Options = PDBResidueTypes, ResTypes = TSimpleStrings, OnDelete = TOnDeleteCallback
    # Second method: Options = Name = string, ResTypes = OnDelete = TOnDeleteCallback
    def DeleteResidues(self, Options, ResTypes=None, OnDelete=None):
        if isinstance(Options, PDBResidueTypes):
            for c in range(self.FProtein.GroupCount-1):
                # chain = TMolecule
                chain = self.FProtein.GetGroup(c)
                chain.TagAllAtoms(0)
                for r in range(chain.GroupCount-1):
                    # res = TMolecules
                    res = chain.GetGroup(r)
                    if ((Options.name is 'resAA') and (AAOneLetterCode(res.Name) is not '')) or \
                            ((Options.name is 'resNonAA') and (AAOneLetterCode(res.Name) is '')) or \
                            (stringutils.LastIndexOf(res.Name, ResTypes) > 0):
                        res.TagAllAtoms(1)
                chain.DeleteTaggedAtoms(1, OnDelete)
            self.FProtein.DeleteEmptyGroups()
        else:
            Name = Options
            OnDelete = ResTypes
            # names = TSimpleStrings
            names = []
            names[0] = Name
            self.DeleteResidues(None, names, OnDelete)

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
        # Extract filename and remove extension, then save it as the protein name
        filename = os.path.basename(PdbFileName)
        self.FProtein.Name = os.path.splitext(filename)[0]
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
        # TODO: check atoms type, because if it is an ordinal type (e. g. a number) range must stop at the
        # highest value
        for f in range(atoms):
            # tempix = Integer
            tempix = self.TemplateIx(atoms[f].Parent.Name)
            if tempix >= 0:
                # atomix = Integer, LastIndexOf is a function of "stringutils"
                atomix = stringutils.LastIndexOf(atoms[f].Name, self.FTemplates[tempix].Atoms)
                if atomix >= 0:
                    atoms[f].AtomicNumber = self.FTemplates[tempix].AtomicNumbers[atomix]
                    if atoms[f].Atomicumber > 0:
                        # AtomData is an array of "oclconfiguration"
                        atoms[f].Radius = AtomData[atoms[f].AtomicNumber - 1].VdWradius

    # Name = string
    def TemplateIx(self, Name):
        Result = len(self.FTemplates)
        while Result > 0 and Name is not self.FTemplates[Result].Name:
            Result = Result - 1
        # Result = Integer
        return Result

    # Chains = TSimpleStrings
    def CopyChains(self, Chains):
        if Chains is None:
            return None
        Result = TMolecule.Create(self.FProtein.Name, self.FProtein.ID, None)
        # TODO: check atoms type, because if it is an ordinal type (e. g. a number) range must stop at the highest value
        for f in range(Chains):
            Result.AddGroup(TMolecule.CopyFrom(self.FProtein.GetGroup(Chains[f]), Result))
        # Result = TMolecule
        return Result

    # OnDelete = TOnDeleteCallback
    def DeleteNonAAResidues(self, OnDelete = None):
        self.DeleteResidues(PDBResidueTypes.resNonAA, None, OnDelete)

    # OnDelete = TOnDeleteCallback
    def DeleteWater(self, OnDelete = None):
        self.DeleteResidues('HOH', OnDelete)

    # ChainName = string, OnDelete = TOnDeleteCallback
    def DeleteChainsByName(self, ChainName, OnDelete = None):
        self.FProtein.TagAllAtoms(0)
        for c in range(self.FProtein.GroupCount() - 1):
            chain = self.FProtein.GetGroup(c)
            if chain.Name is ChainName:
                chain.TagAllAtoms(1)
        self.FProtein.DeleteTaggedAtoms(1, OnDelete)
        self.FProtein.DeleteEmptyGroups()


class TPDBModelMan:
    def __init__(self, molCIFPath):
        self.FTemplates = []
        self.FLayers = []
        self.LoadTemplates(molCIFPath)

    # srec = TSearchRec, pdbparser = TPdbReader
    # For the moment I passed only the string with file path as srec
    def SetTemplate(self, srec, pdbparser):
        # TODO: Check if we have to take the last value of FTemplates or the last
        t = self.FTemplates[len(self.FTemplates) - 1]
        # Extract name and remove file extension
        t.Name = os.path.splitext(srec.Name)[0]
        # Fill atom data
        for f in t.Atoms:
            t.Atoms[f] = pdbparser.Atoms[f].AtomName
            t.AtomIds[f] = pdbparser.Atoms[f].Serial
            t.Coords[f] = pdbparser.Atoms[f].Coords
            # AtomicNumber is a method of "oclconfiguration"
            t.AtomicNumbers[f] = AtomicNumber(pdbparser.Atoms[f].Element)
            # Empty connections
            t.Connects[f] = None
            # TODO: check pdbparser.Connections type, because if it is an ordinal type (e. g. a number) range must
            # stop at the highest value
        for f in range(len(pdbparser.Connections) - 1):
            # IndexOf and Copy are functions of "basetypes"
            ix = basetypes.IndexOf(pdbparser.Connections[f].AtomID, t.AtomIDs)
            copy = []
            for x in range(len(pdbparser.Connections[f].Connects)):
                copy.append(pdbparser.Connections[f].Connects[x])
            t.Connects[ix] = copy

    # Path = string
    def LoadTemplates(self, Path):
        # Remove references to templates in layers
        for f in range(len(self.FLayers) - 1):
            self.FLayers[f].ResetTemplates(self.FTemplates)
        # Clear and load
        self.FTemplates = None
        # TPdbReader is a class of "pdbparser"
        pdbparser = TPdbReader()
        # srec shouldn't be needed
        # srec = TSearchRec
        fileList = glob.glob(Path + "*.pdb")
        if not fileList:
            for f in fileList:
                pdbparser.Load(f)
                self.SetTemplate(f, pdbparser)
        # This instruction shouldn't be necessary due to garbage collector
        # pdbparser.Free()
        # Reassign templates to layers
        for f in range(len(self.FLayers)):
            self.FLayers[f].ResetTemplates(self.FTemplates)

    def Count(self):
        # Return integer
        return len(self.FLayers)

    # Ix = integer
    def LayerByIx(self, Ix):
        # Return TPDBModel
        return self.FLayers[Ix]

    # FileName = string
    def LayerByFileName(self, FileName):
        for f in range(len(self.FLayers)):
            if self.FLayers[f].FileName is FileName:
                # Return TPDBModel
                return self.FLayers[f]
        return None

    def AddNewLayer(self):
        Result = TPDBModel(self.FTemplates, len(self.FLayers))
        self.FLayers[len(self.FLayers)] = Result
        # Return TPDBModel
        return Result

    # PDBFileName = string, ChargeFrom = PDBChargeOrigin
    def LoadLayer(self, PDBFileName, ChargeFrom):
        # Return TMolecule
        return self.AddNewLayer().LoadPDB(PDBFileName, ChargeFrom)

    # Layer = Integer, Indexes = TIntegers OR TSimpleStrings
    def GetChains(self, Layer, Indexes):
        Result = []
        for f in range(len(Indexes)):
            Result[f] = self.LayerByIx(Layer).GetChain(Indexes[f])
        # Return TMolecules => list of TMolecule
        return Result

    def ClearLayers(self):
        """ These instructions shouldn't be necessary due to garbage collector
        for f in range(len(self.FLayers)):
            self.FLayers[f].Free()"""
        self.FLayers = None

    def __del__(self):
        if self is not None:
            self.ClearLayers()


# Atom = TAtom
def AtomIsAABackbone(Atom):
    # Return Boolean
    return (Atom.Name is "A") or (Atom.Name is "O") or (Atom.Name is "CA") or (Atom.Name is "C")

# Atoms = TAtoms => list of TAtom
def BackboneOnly(Atoms):
    Result = []
    c = 0
    for f in range(len(Atoms)):
        if AtomIsAABackbone((Atoms[f])):
            Result[c] = Atoms[f]
            c = c + 1
    # Return TAtoms
    return Result

# Atoms = TAtoms => list of TAtom
def NoBackbone(Atoms):
    Result = []
    c = 0
    for f in range(len(Atoms)):
        if not AtomIsAABackbone((Atoms[f])):
            Result[c] = Atoms[f]
            c = c + 1
    # Return TAtoms
    return Result

# Residue = TMolecule
def ResidueIsAminoAcid(Residue):
    # AAOneLetterCode is a function of "oclconfiguration"
    # Return Boolean
    return (Residue.GroupCount() is 0) and (AAOneLetterCode(Residue.Name) is not "")

# Chain = TMolecule, MissingMarker = string
def ChainSequence(Chain, MissingMarker = "X"):
    Result = ""
    if Chain.GroupCount() > 0:
        # previd = integer
        previd = Chain.GetGroup(0).ID - 1
        for f in range(Chain.GroupCount() - 1):
            previd = previd + 1
            # res = TMolecule
            res = Chain.GetGroup(f)
            while previd < res.ID:
                previd = previd + 1
                Result = Result + MissingMarker
            # AAOneLetterCode is a function of "oclconfiguration"
            # tmp = string
            tmp = AAOneLetterCode(res.Name)
            if tmp is "":
                Result = Result + MissingMarker
            else:
                Result = Result + tmp
    # Return string
    return Result

# Molecule = TMolecule, FileName = string
def SaveToPDB(Molecule, FileName):
    oldchain = None
    # s1 = TStringList
    s1 = []
    atoms = Molecule.AllAtoms()
    for f in range(len(atoms)):
        res = atoms[f].Parent()
        rname = ""
        rid = 0
        chname = ""
        if res is not None:
            rname = res.Name
            rid = res.ID
            chain = res.Parent
            if chain is not None:
                chname = chain.Name
                if (oldchain is not None) and (oldchain is not chain):
                    s1.append('TER       ')
                oldchain = chain
        # AtomRecord is a function of "pdbparser", Element is a function of "oclconfiguration"
        s1.append(AtomRecord(atoms[f].Name, rname, chname, atoms[f].ID, rid, atoms[f].Coords,
                             Element(atoms[f].AtomicNumber)))
    with open(FileName, 'w') as f:
        for item in s1:
            f.write(item + "\n")

# Protein = const TMolecule, ChainName = const string, ResId = Integer
def GetResidue(Protein, ChainName, ResId):
    Result = Protein.GetGroup(ChainName)
    if Result is not None:
        Result = Result.GetGroupById(ResId)
    # Return TMolecule
    return Result
