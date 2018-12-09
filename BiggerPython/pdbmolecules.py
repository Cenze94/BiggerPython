from enum import Enum
import os
import glob
import basetypes
import stringutils
import oclconfiguration
import pdbparser
import molecules


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
        self.FProtein = molecules.TMolecule('', AID, None)
        self.FInfo = pdbparser.TPDBInfo('', '', '', '', '', '', '', '', '', '')
        self.FFileName = ""

    def __del__(self):
        if self is not None:
            self.ClearChains()

    def ClearChains(self):
        self.FProtein.CleanUp()

    # IDs = TSimpleStrings
    def CreateChains(self, IDs):
        for f in range(len(IDs)):
            self.FProtein.NewGroup(IDs[f], f+1)

    # First method: Options = PDBResidueTypes, ResTypes = TSimpleStrings, OnDelete = TOnDeleteCallback
    # Second method: Options = string, ResTypes = TOnDeleteCallback
    def DeleteResidues(self, Options, ResTypes=None, OnDelete=None):
        if isinstance(Options, PDBResidueTypes):
            for c in range(self.FProtein.GroupCount() - 1):
                # chain = TMolecule
                chain = self.FProtein.GetGroup(c)
                chain.TagAllAtoms(0)
                for r in range(chain.GroupCount-1):
                    # res = TMolecules
                    res = chain.GetGroup(r)
                    if ((Options.name is 'resAA') and (oclconfiguration.AAOneLetterCode(res.Name) is not '')) or \
                            ((Options.name is 'resNonAA') and (oclconfiguration.AAOneLetterCode(res.Name) is '')) or \
                            (stringutils.LastIndexOf(res.Name, ResTypes) > 0):
                        res.TagAllAtoms(1)
                chain.DeleteTaggedAtoms(1, OnDelete)
            self.FProtein.DeleteEmptyGroups()
        else:
            Name = Options
            OnDelete = ResTypes
            names = [Name]
            self.DeleteResidues(None, names, OnDelete)

    # ChainName = string, ChainID = integer
    def NewEmptyChain(self, ChainName, ChainID):
        # Return TMolecule
        return self.FProtein.NewGroup(ChainName, ChainID)

    # ChainName = string, ChainID = integer, Size = integer
    def NewChain(self, ChainName, ChainID, Size):
        Result = self.NewEmptyChain(ChainName, ChainID)
        Result.CreateEmptyGroups(Size)
        # Return TMolecule
        return Result

    # ChainIx = integer OR ChainIX = string
    def GetChain(self, ChainIx):
        # Return TMolecule
        return self.FProtein.GetGroup(ChainIx)

    # Chain = TMolecule
    def AppendChain(self, Chain):
        self.FProtein.AddGroup(Chain)

    # ChainIx = string OR integer, ResIx = integer OR Integer
    def GetResidue(self, ChainIx, ResIx):
        if isinstance(ChainIx, int):
            Result = self.FProtein.GetGroup(ChainIx)
            if Result is not None:
                # Return TMolecule
                return Result.GetGroup(ResIx)
        else:
            Result = self.GetChain(ChainIx)
            if Result is not None:
                # Return TMolecule
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
    def LoadPDB(self, PdbFileName, ChargeFrom = PDBChargeOrigin.pdbNone):
        self.ClearChains()
        self.FFileName = PdbFileName
        # parser = TPDBReader
        parser = pdbparser.TPDBReader(PdbFileName)
        # Extract filename and remove extension, then save it as the protein name
        filename = os.path.basename(PdbFileName)
        endSplit = filename.rfind('.')
        self.FProtein.FName = filename[:endSplit]
        self.CreateChains(parser.FChainIDs)

        # Read atoms
        # cc = integer
        cc = -1  # Current chain
        cr = 0  # This should be the standard value of uninitialized integers
        cres = molecules.TMolecule('', -1, None)
        for f in range(len(parser.FAtoms)):
            patom = parser.FAtoms[f]
            if cc != patom.ChainNum:  # ChainNumber is always >= 0
                cc = patom.ChainNum
                cr = -1  # Current residue number, also >= 0
            if patom.ResSeq != cr:  # Get new residue
                cres = self.FProtein.GetGroup(cc).NewGroup(patom.ResName, patom.ResSeq)
                cr = patom.ResSeq
            atom = cres.NewAtom(patom.AtomName, patom.Serial)
            atom.Coords = patom.Coords
            # Element may be present in the PDB file. However, this is superseded if there is monomer template data
            atom.AtomicNumber = oclconfiguration.AtomicNumber(patom.Element)
            atom.Radius = oclconfiguration.VdWRadius(atom.AtomicNumber)
            if ChargeFrom is PDBChargeOrigin.pdbOccupancy:
                atom.Charge = patom.Occupancy
            elif ChargeFrom is PDBChargeOrigin.pdbOccTemp:
                atom.Charge = patom.OccTemp
            elif ChargeFrom is PDBChargeOrigin.pdbCharge:
                if len(patom.Charge) is 2:
                    atom.Charge = float(patom.Charge[1])
                if patom.Charge[2] is '-':
                    atom.Charge = -atom.Charge
        # Assign element, radius, charge, et al, from the monomer templates will replace information on PDB
        self.AssignAtomicData()
        # Return TMolecule
        return self.FProtein

    # ATemplates = TTemplates
    def ResetTemplates(self, ATemplates):
        self.FTemplates = ATemplates

    def ChainCount(self):
        # Return Integer
        return self.FProtein.GroupCount

    def ListChains(self):
        # Return TSimpleStrings
        return self.FProtein.ListGroupNames

    # FirstChar = Char, LastChar = Char
    def RenameChains(self, FirstChar = 'A', LastChar = 'Z'):
        name = FirstChar
        for f in range(self.FProtein.GroupCount() - 1):
            self.FProtein.GetGroup(f).Name = name
            name = chr(ord(name) + 1)
            if name > LastChar:
                name = FirstChar

    # ChainIx = Integer, return Integer
    def ResidueCount(self, ChainIx):
        return self.FProtein.GetGroup(ChainIx).GroupCount()

    def AssignAtomicData(self):
        # atoms = TAtoms
        atoms = self.FProtein.AllAtoms()
        for f in range(len(atoms)):
            # tempix = Integer
            tempix = self.TemplateIx(atoms[f].FParent.FName)
            if tempix >= 0:
                # atomix = Integer
                atomix = stringutils.LastIndexOf(atoms[f].FName, self.FTemplates[tempix].Atoms)
                if atomix >= 0:
                    atoms[f].FAtomicNumber = self.FTemplates[tempix].AtomicNumbers[atomix]
                    if atoms[f].FAtomicumber > 0:
                        atoms[f].FRadius = oclconfiguration.AtomData[atoms[f].FAtomicNumber - 1].VdWradius

    # Name = string
    def TemplateIx(self, Name):
        Result = len(self.FTemplates) - 1
        while Result >= 0 and Name is not self.FTemplates[Result].Name:
            Result = Result - 1
        # Result = Integer
        return Result

    # Chains = TSimpleStrings
    def CopyChains(self, Chains):
        if Chains is None:
            return None
        Result = molecules.TMolecule(self.FProtein.FName, self.FProtein.FID, None)
        # TODO: check atoms type, because if it is an ordinal type (e. g. a number) range must stop at the highest value
        for f in range(Chains):
            Result.AddGroup(Result.GetCopy(self.FProtein.GetGroup(Chains[f]), Result))
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
        t = self.FTemplates[len(self.FTemplates) - 1]
        # Extract name and remove file extension
        startSplit = srec.rfind('\\')
        endSplit = srec.rfind('.')
        t.Name = srec[startSplit + 1:endSplit]
        # Fill atom data
        for f in range(len(t.Atoms)):
            t.Atoms.append(pdbparser.FAtoms[f].AtomName)
            t.AtomIds.append(pdbparser.FAtoms[f].Serial)
            t.Coords.append(pdbparser.FAtoms[f].Coords)
            t.AtomicNumbers.append(oclconfiguration.AtomicNumber(pdbparser.FAtoms[f].Element))
            # Empty connections
            t.Connects.append(None)
        for f in range(len(pdbparser.FConnections) - 1):
            ix = basetypes.IndexOf(pdbparser.FConnections[f].AtomID, t.AtomIDs)
            acopy = []
            for x in range(len(pdbparser.FConnections[f].Connects)):
                acopy.append(pdbparser.FConnections[f].Connects[x])
            t.Connects[ix] = acopy

    # Path = string
    def LoadTemplates(self, Path):
        # Remove references to templates in layers
        for f in range(len(self.FLayers) - 1):
            self.FLayers[f].ResetTemplates(self.FTemplates)
        # Clear and load
        self.FTemplates = []
        tpdbparser = pdbparser.TPDBReader()
        # srec shouldn't be needed
        # srec = TSearchRec
        fileList = glob.glob(Path + "*.pdb")
        if fileList:
            for f in fileList:
                self.FTemplates.append(TTemplate('', [], [], [], [], []))
                tpdbparser.Load(f)
                self.SetTemplate(f, tpdbparser)
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
        self.FLayers.append(Result)
        # Return TPDBModel
        return Result

    # PDBFileName = string, ChargeFrom = PDBChargeOrigin
    def LoadLayer(self, PDBFileName, ChargeFrom = PDBChargeOrigin.pdbNone):
        # Return TMolecule
        return self.AddNewLayer().LoadPDB(PDBFileName, ChargeFrom)

    # Layer = Integer, Indexes = TIntegers OR TSimpleStrings
    def GetChains(self, Layer, Indexes):
        Result = []
        for f in range(len(Indexes)):
            Result.append(self.LayerByIx(Layer).GetChain(Indexes[f]))
        # Return TMolecules
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
            Result.append(Atoms[f])
            c = c + 1
    # Return TAtoms
    return Result


# Atoms = TAtoms => list of TAtom
def NoBackbone(Atoms):
    Result = []
    c = 0
    for f in range(len(Atoms)):
        if not AtomIsAABackbone((Atoms[f])):
            Result.append(Atoms[f])
            c = c + 1
    # Return TAtoms
    return Result


# Residue = TMolecule
def ResidueIsAminoAcid(Residue):
    # Return Boolean
    return (Residue.GroupCount() is 0) and (oclconfiguration.AAOneLetterCode(Residue.Name) is not "")


# Returns string with one letter aa code for each residue. Non AA residues and gaps in the sequence of residue
# IDs are filled with the MissingMarker
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
            # tmp = string
            tmp = oclconfiguration.AAOneLetterCode(res.Name)
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
        s1.append(pdbparser.AtomRecord(atoms[f].Name, rname, chname, atoms[f].ID, rid, atoms[f].Coords,
                                       oclconfiguration.Element(atoms[f].AtomicNumber)))
    with open(FileName, 'w') as f:
        for item in s1:
            f.write(item + "\n")


# Assumes Protein is a protein, with chains and residues
# Protein = const TMolecule, ChainName = const string, ResId = Integer
def GetResidue(Protein, ChainName, ResId):
    Result = Protein.GetGroup(ChainName)
    if Result is not None:
        Result = Result.GetGroupById(ResId)
    # Return TMolecule
    return Result
