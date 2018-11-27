import geomutils
import copy


# Bond types
SingleBond = 0
DoubleBond = 1
HBond = 2
Aromatic = 3

# Special tag
TagNone = 0
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

    # Originally this was the copy constructor, but Python hasn't it. This version is sufficient for the uses found
    # AAtom = TAtom, NewParent = TMolecule
    def GetCopy(self, AAtom, NewParent):
        # (return TAtom in Python)
        return TAtom(AAtom.FName, NewParent, AAtom.FID, AAtom.FAtomicNumber, AAtom.FCoord, AAtom.FRadius,
                     AAtom.FCharge, AAtom.FMass)


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

    # AName = string, AID = Integer, AParent = TMolecule (FAtoms = TAtoms, FGroups = TMolecules, FBondsTable =
    # TAtomBonds, FType = string)
    def __init__(self, AName, AID, AParent):
        self.FName = AName  # Identifier, managed externally
        self.FID = AID  # Identifier, managed externally
        self.FParent = AParent  # If a monomer in a larger complex. Can make trees of arbitrary depth
        self.FAtoms = []  # Atoms that do not belong in groups
        self.FGroups = []  # Groups such as monomers. If molecule has groups it should not have atoms. Will not
        # assume that it only can have atoms or groups, but that makes more sense for modelling actual molecules
        self.FBondsTable = []  # Best if there is only one bonds table at the top level of the molecule hierarchy,
        # for performance. But this is not mandatory
        self.FType = ''

    # Index in FAtoms
    # AAtom = TAtom
    def AtomInex(self, AAtom):
        Result = len(self.FAtoms) - 1
        while (Result >= 0) and (AAtom is not self.FAtoms[Result]):
            Result = Result - 1
        # Return Integer
        return Result

    # Index in FGroups
    # Group = TMolecule
    def GroupIndex(self, Group):
        Result = len(self.FGroups) - 1
        while (Result >= 0) and (Group is not self.FGroups[Result]):
            Result = Result - 1
        # Return Integer
        return Result

    # Adds an existing atom
    # Atom = TAtom
    def AddAtom(self, Atom):
        Atom.FParent = self
        self.FAtoms.append(Atom)

    # Originally this was the copy constructor, but Python hasn't it. This version is sufficient for the uses found
    # AMolecule, NewParent = TMolecule
    def GetCopy(self, AMolecule, NewParent):
        Result = TMolecule(AMolecule.FName, AMolecule.FID, NewParent)
        Result.FGroups = []
        for f in range(len(AMolecule.FGroups)):
            # Here the first parameter shouldn't be needed, but self parameter is required
            Result.FGroups.append(TMolecule.GetCopy(AMolecule.FGroups[f], AMolecule.FGroups[f], self))
        Result.FAtoms = []
        for f in range(len(AMolecule.FAtoms)):
            # Here the first parameter shouldn't be needed, but self parameter is required
            Result.FAtoms.append(TAtom.GetCopy(AMolecule.FAtoms[f], AMolecule.FAtoms[f], self))
        # Copy only bonds between atoms in the new molecule this means some bonds may be lost when copying a subset
        # of chains...
        Result.FBondsTable = []
        for f in range(len(AMolecule.FBondsTable)):
            at1 = AMolecule.AtomById(AMolecule.FBondsTable[f].Atom1.ID)
            at2 = AMolecule.AtomById(AMolecule.FBondsTable[f].Atom2.ID)
            if (at1 is not None) and (at2 is not None):
                Result.FBondsTable.append(TAtomBond(at1, at2, AMolecule.FBondsTable[f].BondType))
        # (return TMolecule in Python)
        return Result

    # Adds an existing group
    # Group = TMolecule
    def AddGroup(self, Group):
        Group.FParent = self
        self.FGroups.append(Group)

    # Start = Integer
    def RenumberAtoms(self, Start=1):
        atomlist = self.AllAtoms()
        for f in range(len(atomlist)):
            atomlist[f].ID = Start + f

    # Creates and adds group
    # GName = string, GID = Integer
    def NewGroup(self, GName, GID):
        Result = TMolecule(GName, GID, self)
        self.AddGroup(Result)
        # Return TMolecule
        return Result

    # Creates and adds atom;
    # AName = string, AID = Integer
    def NewAtom(self, AName, AID):
        Result = TAtom(AName, AID, self)
        self.AddAtom(Result)
        # Return TAtom
        return Result

    def AllAtoms(self):
        # Flattens hierarchy into a single array of atoms, returns a copy array but each element is the atom object
        Result = copy.deepcopy(self.FGroups)
        # Add to atoms at this level all the offspring atoms
        for f in range(len(self.FGroups)):
            tmp = self.FGroups[f].AllAtoms()
            for g in range(len(tmp)):
                Result.append(tmp[g])
        # Return TAtoms
        return Result

    def AllCoords(self):
        # Returns a copy of all coords in all atoms
        Result = []
        for f in range(len(self.FAtoms)):
            Result.append(self.FAtoms[f].Coords)
        # Add to this level all the offspring levels
        for f in range(len(self.FGroups)):
            tmp = self.FGroups[f].AllCoords()
            for g in range(len(tmp)):
                Result.append(tmp[g])
        # Return TCoords
        return Result

    def AllTerminalGroups(self):
        Result = []
        for f in range(len(self.FGroups)):
            if self.FGroups[f].Groups is None:
                AppendGroupsToArray(self.FGroups[f], Result)

    def AllBonds(self):
        # Flattens hierarchy into a single array of atom bonds, returns a copy array, so changes in elements do not
        # affect the bonds tables in the molecule (but changes in the atoms will, as these are objects)
        tot = self.BondCount()
        Result = []
        f = 0
        while f < len(self.FBondsTable):
            Result.append(self.FBondsTable[f])
            f = f + 1
        # Add to atoms at this level all the offspring atoms
        for i in range(len(self.FGroups)):
            tmp = self.FGroups[i].AllBonds
            j = 0
            while j < len(tmp):
                Result.append(tmp[j])
                j = j + 1
                f = f + 1
        # Return TAtomBonds
        return Result

    def GroupCount(self):
        # Return Integer
        return len(self.FGroups)

    def AtomCount(self):
        # Total number of atoms in molecule and groups
        Result = len(self.FAtoms)
        for f in range(len(self.FGroups)):
            Result = Result + self.FGroups[f].AtomCount()
        # Return Integer
        return Result

    def BondCount(self):
        Result = len(self.FBondsTable)
        for f in range(len(self.FGroups)):
            Result = Result + self.FGroups[f].BondCount()
        # Return Integer
        return Result

    # GroupIx = Integer OR string
    def GetGroup(self, GroupIx):
        if isinstance(GroupIx, int):
            assert (GroupIx < len(self.FGroups)) and GroupIx > 0, 'Invalid group index'
            # Return TMolecule
            return self.FGroups[GroupIx]
        else:
            Result = None
            for f in range(len(self.FGroups)):
                if self.FGroups[f].Name is GroupIx:
                    Result = self.FGroups[f]
                    break
            # Return TMolecule
            return Result

    # GroupId = Integer
    def GetGroupById(self, GroupId):
        Result = None
        for f in range(len(self.FGroups)):
            if self.FGroups[f].ID is GroupId:
                Result = self.FGroups[f]
                break
        # Return TMolecule
        return Result

    # AtomIx = Integer OR string
    def GetAtom(self, AtomIx):
        if isinstance(AtomIx, int):
            assert (AtomIx < len(self.FGroups)) and (AtomIx >= 0), 'Invalid atom index'
            # Return TAtom
            return self.FAtoms[AtomIx]
        else:
            Result = None
            for f in range(len(self.FAtoms)):
                if self.FAtoms[f].Name is AtomIx:
                    Result = self.FAtoms[f]
                    break
            # Return TMolecule
            return Result

    def ListGroupNames(self):
        Result = []
        for f in range(len(self.FGroups)):
            Result.append(self.FGroups[f].Name)
        # Return TSimpleStrings
        return Result

    # Tag = Integer
    def TagAllAtoms(self, Tag):
        # Tag all atoms, including in offspring, recursively
        for f in range(len(self.FAtoms)):
            self.FAtoms[f].Tag = Tag
        for f in range(len(self.FGroups)):
            self.FGroups[f].TagAllAtoms(Tag)

    # Tag = Integer
    def TagAllBonds(self, Tag):
        # Tag all bonds, including in offspring, recursively
        for f in range(len(self.FBondsTable)):
            self.FBondsTable[f].Tag = Tag
        for f in range(len(self.FGroups)):
            self.FGroups[f].TagAllBonds(Tag)

    # Atoms = TAtoms, Tag = Integer
    def TagAtoms(self, Atoms, Tag):
        # Tag a list of atoms
        for f in range(len(Atoms)):
            Atoms[f].Tag = Tag

    # Bonds = TAtomBonds, Tag = Integer
    def TagBonds(self, Bonds, Tag):
        for f in range(len(Bonds)):
            Bonds[f].Tag = Tag

    # Tag = Integer, OnDelete = TOnDeleteCallback
    def DeleteTaggedAtoms(self, Tag, OnDelete=None):
        # Recursive through groups, calls removeTaggedBonds first
        self.RemoveTaggedAtomsBonds(Tag)  # Remove bonds before deleting atoms
        tmp = []  # New atoms array
        # If atom is not to delete, copy into new array
        for f in range(len(self.FAtoms)):
            if self.FAtoms[f].Tag is not Tag:
                tmp.append(self.FAtoms[f])
            else:
                del self.FAtoms[f]
        # Store and delete atoms in offspring
        self.FAtoms = tmp
        for f in range(len(self.FGroups)):
            self.FGroups[f].DeleteTaggedAtoms(Tag)

    # Tag = Integer, OnDelete = TOnDeleteCallback
    def RemoveTaggedAtomsBonds(self, Tag, OnDelete=None):
        # Recursive through groups, removes bonds with at least one tagged atom. This procedure is meant for cleaning
        # bonds with atoms about to be deleted
        # Callback to warn about deletions
        if OnDelete is not None:
            OnDelete(Tag)
        tmp = []  # New bonds table
        # If not to be deleted, copy to new bonds table
        for f in range(len(self.FBondsTable)):
            if (self.FBondsTable[f].Atom1.Tag is not Tag) and (self.FBondsTable[f].Atom2.Tag is not Tag):
                tmp.append(self.FBondsTable[f])
        # Store
        self.FBondsTable = tmp
        for f in range(len(self.FGroups)):
            # No more callbacks, as one should be enough to warn about all deletions involving tagged atoms
            self.FGroups[f].RemoveTaggedAtomBonds(Tag)

    # Tag = Integer, OnDelete = TOnDeleteCallback
    def RemoveTaggedBonds(self, Tag, OnDelete=None):
        # Recursive through groups, removes bonds that are tagged
        # Callback to warn about deletions
        if OnDelete is not None:
            OnDelete(Tag)
        tmp = []  # New bonds table
        # If not to be deleted, copy to new bonds table
        for f in range(len(self.FBondsTable)):
            if self.FBondsTable[f].Tag is not Tag:
                tmp.append(self.FBondsTable[f])
        # Store
        self.FBondsTable = tmp
        for f in range(len(self.FGroups)):
            # No more callbacks, as one should be enough to warn about all deletions of tagged bonds
            self.RemoveTaggedBonds(Tag)

    # Atoms = TAtoms, OnDelete = TOnDeleteCallback
    def DeleteAtoms(self, Atoms, OnDelete=None):
        # Deletes those atoms from the Atoms set that belong to this molecule or to offspring groups. Note that Atoms
        # array becomes unusable, and that atoms outside this molecule will be tagged for deletion but not deleted.
        # Reset tags first so that none get deleted that shouldn't
        self.TagAllAtoms(TagNone)
        self.TagAtoms(Atoms, TagToDelete)
        self.DeleteTaggedAtoms(TagToDelete, OnDelete)

    def DeleteEmptyGroups(self):
        tmp = []
        for f in range(len(self.FGroups)):
            if self.FGroups[f].AtomCount() > 0:
                self.FGroups[f].DeleteEmptyGroups()
                tmp.append(self.FGroups[f])
            else:
                del self.FGroups[f]
        self.FGroups = tmp

    # Looks also recursively in groups
    # AId = Integer
    def AtomById(self, AId):
        # WARNING: if IDs are repeated, will return the first atom. It's up to caller to guarantee no repeated ids.
        Result = None
        for f in range(len(self.FAtoms)):
            if self.FAtoms[f].Id is AId:
                Result = self.FAtoms[f]
                if Result is not None:
                    break
        if Result is None:
            for f in range(len(self.FGroups)):
                Result = self.FGroups[f].AtomById(AId)
                if Result is not None:
                    break
        # Return TAtom
        return Result

    # Subtracts center, rotates then translates
    # First method: Center = TCoord
    # Second method: Center = TRotMatrix
    # Third method: Center = TQuaternion
    # Fourth method: Center = TCoord, Rotation = TRotMatrix
    # Fifth method: Center = TCoord, Rotation = TRotMatrix, Translation = TCoord
    def Transform(self, Center, Rotation=None, Translation=None):
        if Rotation is not None:
            if Translation is not None:
                # Fifth method
                ats = self.AllAtoms()
                for f in range(len(ats)):
                    ats[f].Coords = geomutils.Subtract(ats[f].Coords, Center)
                    ats[f].Coords = geomutils.Rotate(ats[f].Coords, Rotation)
                    ats[f].Coords = geomutils.Add(ats[f].Coords, Translation)
            else:
                # Fourth method
                ats = self.AllAtoms()
                for f in range(len(ats)):
                    ats[f].Coords = geomutils.Add(ats[f].Coords, Center)
                    ats[f].Coords = geomutils.Rotate(ats[f].Coords, Rotation)
        else:
            if isinstance(Center, geomutils.TRotMatrix):
                # Second method
                ats = self.AllAtoms()
                for f in range(len(ats)):
                    ats[f].Coords = geomutils.Rotate(ats[f].Coords, Center)
            elif isinstance(Center, geomutils.TQuaternion):
                # Third method
                ats = self.AllAtoms()
                for f in range(len(ats)):
                    ats[f].Coords = geomutils.Rotate(ats[f].Coords, Center)
            else:
                # First method
                ats = self.AllAtoms()
                for f in range(len(ats)):
                    ats[f].Coords = geomutils.Add(ats[f].Coords, Center)

    # These procedures clear all groups or atoms, respectively. They are meant to be used on empty molecules since
    # they do not callback to onDelete events

    # Count = Integer
    def CreateEmptyGroups(self, Count):
        # Sets molecule with an array of empty groups, clears all existing groups first.
        self.ClearGroups()
        for f in range(Count):
            self.FGroups[f] = TMolecule('', f, self)

    # Count = Integer
    def CreateEmptyAtoms(self, Count):
        # Sets molecule with an array of empty atoms, clears all existing atoms first.
        self.ClearAtoms()
        for f in range(Count):
            self.FAtoms[f] = TAtom('', f, self)

    # OnDelete = TOnDeleteCallback
    def ClearAtoms(self, OnDelete=None):
        # Clears atoms at this level. Not recursive, does not delete atoms in offespring.
        self.DeleteAtoms(self.FAtoms, OnDelete)
        self.FAtoms = []

    # OnDelete = TOnDeleteCallback
    def ClearGroups(self, OnDelete=None):
        # First, tag all atoms in all offspring groups
        for f in range(len(self.FGroups)):
            self.FGroups[f].TagAllAtoms(TagToDelete)
        # Warn deletion before deleting anything
        if OnDelete is not None:
            OnDelete(TagToDelete)
        self.ClearAllBonds()
        # Finally, free each group (atoms are freed along with the group)
        for f in range(len(self.FGroups)):
            del self.FGroups[f]
        self.FGroups = []

    # OnDelete = TOnDeleteCallback
    def ClearBonds(self, OnDelete=None):
        # Warn of deletion if callback provided, by tagging all bonds
        if OnDelete is not None:
            self.TagBonds(self.FBondsTable, TagToDelete)
            OnDelete(TagToDelete)
        self.FBondsTable = []

    # This is a recursive version of ClearBonds to clear all bonds from offspring too
    # OnDelete = TOnDeleteCallback
    def ClearAllBonds(self, OnDelete=None):
        # Clears all bonds in this level and, recursively, in all descendants; useful for cleaning up everything,
        # before deleting all atoms
        # If needs to warn about deletion
        if OnDelete is not None:
            # Tag all bonds first
            self.TagAllBonds(TagToDelete)
            for f in range(len(self.FGroups)):
                self.FGroups[f].TagAllBonds(TagToDelete)
            OnDelete(TagToDelete)
        # Clear bonds with no more callbacks, even in offspring
        self.ClearBonds()
        for f in range(len(self.FGroups)):
            self.FGroups[f].ClearAllBonds()

    # OnDelete = TOnDeleteCallback
    def CleanUp(self, OnDelete=None):
        # Warn first of all deletions if callback provided
        if OnDelete is not None:
            self.TagAllAtoms(TagToDelete)
            OnDelete(TagToDelete)
        self.ClearAllBonds(OnDelete)  # Clear bonds first so no time spent checking them for deleted atoms
        self.ClearAtoms(OnDelete)
        self.ClearGroups(OnDelete)

    def __del__(self):
        # When freeing
        if self is not None:
            self.CleanUp()


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
