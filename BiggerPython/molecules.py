import basetypes
import geomutils


# Bond types
SingleBond = 0
DoubleBond = 1
HBond = 2
Aromatic = 3

# Special tag
TagNoe = 0
TagToDelete = 1


class TAtom:
    # FName = string, FID = Integer, FParent = TMolecule, FAtomicNumber = Integer, FCoord = TCoord,
    # FRadius, FCharge, FMass = TFloat, Tag=Integer
    def __init__(self, FName, FID, FParent, FAtomicNumber=-1, FCoord=None, FRadius=0, FCharge=0, FMass=0, Tag=0):
        self.FName = FName  # Data for each atom in molecule. Bond data is in parent group
        self.FID = FID  # Atom properties, should be unique for each molecule
        self.FParent = FParent  # Data for each atom in molecule. Bond data is in parent group
        # Data for each atom in molecule. Bond data is in parent group
        self.FAtomicNumber = FAtomicNumber  # Data for each atom in molecule. Bond data is in parent group
        #  AtomicNumber<=0 means undetermined
        self.FCoord = FCoord  # Atom properties
        self.FRadius = FRadius  # Atom properties
        self.FCharge = FCharge  # Atom properties
        self.FMass = FMass  # Atom properties
        self.Tag = Tag  # Temporary tag for selection, deletion, etc. Should not be relied on to store persistent
        # information. It is meant to be used and discarded in the same function (such as deleting atoms)

    # AAtom = TAtom, NewParent = TMolecule
    def __copy__(self, AAtom, NewParent):
        self.FName = AAtom.FName
        self.FParent = NewParent
        self.FID = AAtom.FID
        self.FAtomicNumber = AAtom.FAtomicNumber
        self.FCoord = AAtom.FCoord
        self.FRadius = AAtom.FRadius
        self.FCharge = AAtom.FCharge
        self.FMass = AAtom.FMass


class TAtomBond:
    # Atom1, Atom2 = TAtom, BondType, Tag = TInteger
    def __init__(self, Atom1=None, Atom2=None, BondType=0, Tag=0):
        self.Atom1 = Atom1
        self.Atom2 = Atom2
        self.BondType = BondType
        self.Tag = Tag # Tag is temporary, not suitable for persistent data


class TMolecule:
    '''Class for nested hierarchy of molecular fragments. It can have both atoms (e.g a residue) or groups (e.g. a
    chain) though, chemically, it doesn't make sense to have both'''

    def __init__(self):

    # AAtom = TAtom
    def AtomInex(self, AAtom):
        Result = len(self.FAtoms) - 1
        while (Result >= 0) and (AAtom is not self.FAtoms[Result]):
            Result = Result - 1
        # Return Integer
        return Result

    # Group = TMolecule
    def GroupIndex(self, Group):

    # Atom = TAtom
    def AddAtom(self, Atom):

    # AMolecule, NewParent = TMolecule
    def __copy__(self, AMolecule, NewParent):

    # Adds an existing group
    # Group = TMolecule
    def AddGroup(self, Group):

    # Start = Integer
    def RenumberAtoms(self, Start=-1):

    # Creates and adds group
    # GName = string, GID = Integer
    def NewGroup(self, GName, GID):

    # Creates and adds atom;
    # AName = string, AID = Integer
    def NewAtom(self, AName, AID):

    def AllAtoms(self):

    def AllCoords(self):

    def AllTerminalGroups(self):

    def AllBonds(self):

    def GroupCount(self):

    def AtomCount(self):

    def BondCount(self):

    # GroupIx = Integer OR string
    def GetGroup(self, GroupIx):

    # GroupId = Integer
    def GetGroupById(self, GroupId):

    # AtomIx = Integer OR string
    def GetAtom(self, AtomIx):

    def ListGroupNames(self):

    # Tag = Integer
    def TagAllAtoms(self, Tag):

    # Tag = Integer
    def TagAllBonds(self, Tag):

    # Atoms = TAtoms, Tag = Integer
    def TagAtoms(self, Atoms, Tag):

    # Bonds = TAtomBonds, Tag = Integer
    def TagBonds(self, Bonds, Tag):

    # Tag = Integer, OnDelete = TOnDeleteCallback
    def DeleteTaggedAtoms(self, Tag, OnDelete=None):

    # Tag = Integer, OnDelete = TOnDeleteCallback
    def RemoveTaggedAtomsBonds(self, Tag, OnDelete=None):

    # Tag = Integer, OnDelete = TOnDeleteCallback
    def RemoveTaggedBonds(self, Tag, OnDelete=None):

    # Atoms = TAtoms, OnDelete = TOnDeleteCallback
    def DeleteAtoms(self, Atoms, OnDelete=None):

    def DeleteEmptyGroups(self):

    # Looks also recursively in groups
    # AId = Integer
    def AtomById(self, AId):

    # Subtracts center, rotates then translates
    # First method: Center = TCoord
    # Second method: Center = TRotMatrix
    # Third method: Center = TQuaternion
    # Fourth method: Center = TCoord, Rotation = TRotMatrix
    # Fifth method: Center = TCoord, Rotation = TRotMatrix, Translation = TCoord
    def Transform(self, Center, Rotation, Translation):

    # These procedures clear all groups or atoms, respectively. They are meant to be used on empty molecules since
    # they do not callback to onDelete events

    # Count = Integer
    def CreateEmptyGroups(self, Count):

    # Count = Integer
    def CreateEmptyAtoms(self, Count):


    # OnDelete = TOnDeleteCallback
    def ClearAtoms(self, Onelete=None):

    # OnDelete = TOnDeleteCallback
    def ClearGroups(self, Onelete=None):

    # OnDelete = TOnDeleteCallback
    def ClearBonds(self, Onelete=None):

    # This is a recursive version of ClearBonds to clear all bonds from offspring too
    # OnDelete = TOnDeleteCallback
    def ClearAllBonds(self, OnDelete=None):

    # OnDelete = TOnDeleteCallback
    def CleanUp(self, OnDelete=None):

    def __del__(self):


# Original, ToAppend = const TAtoms
def AppendAtomsToArray(Original, ToAppend):
    Result = []
    for f in range(len(Original)):
        Result.append(Original[f])
    for f in range(len(ToAppend)):
        Result.append(ToAppend[f])
    # Return TAtoms
    return Result


# Group = const TMolecule, Groups = TMolecules
def AppendGroupToArray(Group, Groups):
    Groups.append(Group)


# Group = TMolecules, Groups = const TMolecules
def AppendGroupsToArray(Original, ToAppend):
    for f in range(len(ToAppend)):
        Original.append(ToAppend[f])
