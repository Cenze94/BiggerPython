import basetypes
import geomutils
import geomhash
import molecules


'''In the Pascal version there is also TSelection, which is an object that belongs to the graphical window. Since there
isn't a graphical window in Python, this object is not implemented.'''

# Molecule = TMolecule
def CenterMolecule(Molecule):
    cent = FindCenter(Molecule)
    Molecule.Transform(geomutils.Simmetric(cent))


# Molecule = TMolecule
def FindCenter(Molecule):
    coords = Molecule.AllCoords()
    # Return TCoord
    return geomutils.MidPoint(coords)


# First method: Molecule = TMolecule
# Second method: Molecule = const TMolecule, AtomName = const string
def ListCoords(Molecule, AtomName=None):
    atoms = Molecule.AllAtoms()
    Result = []
    if AtomName is None:
        for f in range(len(atoms)):
            Result.append(atoms[f].Coords)
    else:
        for f in range(len(atoms)):
            if atoms[f] is AtomName:
                Result.append(atoms[f].Coords)
    # Return TCoords
    return Result


# Molecule = TMolecule
def ListRadii(Molecule):
    atoms = Molecule.AllAtoms()
    Result = []
    for f in range(len(atoms)):
        Result.append(atoms[f].Radius)
    # Return TFloats
    return Result


# Contact is center to center distance, not surface to surface
# Atoms1, Atoms2 = const TAtoms, Dist = const TFloat
def AtomsInContact(Atoms1, Atoms2, Dist):
    Result = False
    for f in range(len(Atoms1)):
        for g in range(len(Atoms2)):
            if geomutils.Distance(Atoms1[f].Coords, Atoms2[f].Coords) <= Dist:
                # Return Boolean
                return True
    # Return Boolean
    return False


# Returns the cuboid region defined by all atom surfaces expanded by the Rad value
# Atoms = TAtoms, Rad = TFloat
def CalcHull(Atoms, Rad):
    if Atoms is None:
        # Return TCuboid
        return basetypes.TCuboid()
    else:
        c1 = Atoms[0].Coords
        c2 = c1
        for f in range(1, len(Atoms)):
            c1 = basetypes.Min(c1, geomutils.Add(Atoms[f].Coords, -Atoms[f].Radius))
            c2 = basetypes.Max(c2, geomutils.Add(Atoms[f].Coords, Atoms[f].Radius))
        for f in range(3):
            c1[f] = c1[f] - Rad
            c2[f] = c2[f] + Rad
        # Return TCuboid
        return basetypes.TCuboid(c1, c2)


# Returns the cuboid regions defined by all atom surfaces expanded by the Rad value
# Groups = TMolecules, Rad = TFloat
def CalcHulls(Groups, Rad):
    Result = []
    for f in range(len(Groups)):
        Result.append(CalcHull(Groups[f].AllAtoms(), Rad))
    # Return TCuboids
    return Result

# Returns the cuboid region defined by all atom centers expanded by the Dist value
# Atoms = TAtoms, Dist = TFloat
def CalcCenterHull(Atoms, Dist):
    if Atoms is None:
        # Return TCuboid
        return basetypes.TCuboid()
    else:
        c1 = Atoms[0].Coords
        c2 = c1
        for f in range(1, len(Atoms)):
            c1 = basetypes.Min(c1, Atoms[f].Coords)
            c2 = basetypes.Max(c2, Atoms[f].Coords)
        c1 = geomutils.Subtract(c1, Dist)
        c2 = geomutils.Add(c2, Dist)
        # Return TCuboid
        return basetypes.TCuboid(c1, c2)


# Returns indexes of groups of each Mol1 and Mol2 within distance of the other groups must be terminal (with only
# atoms, not groups)
# Groups1, Groups2 = const TMolecules, Dist = const TFloat, Interface1, Interface2 = TIntegers
def GroupsInContact(Groups1, Groups2, Dist, Interface1, Interface2):


# Returns indexes of each pairwise contact from Mol1 to Mol2 within distance groups must be terminal (with only atoms,
# not groups)
# Groups1, Groups2 = const TMolecules, Dist = const TFloat, Interface1, Interface2 = TIntegers
def GroupContacts(Groups1, Groups2, Dist, Interface1, Interface2):


# Returns an array with the indexes of FromCoords that are within Dist of any ToCoords uses geomhash for efficiency
# FromCoords, ToCoords = TCoords, Dist = TFloat
def NeighbourIndexes(FromCoords, ToCoords, Dist):
