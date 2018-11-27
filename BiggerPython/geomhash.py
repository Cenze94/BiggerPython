import basetypes
import geomutils
import math
import copy


class TGeomHasher:
    # Only stores indexes in the grid. The size of the grid cell must be > 1e-6 and the points array must not be empty

    # FHashGrid = array of array of array of TIntegers, indexes of hashed points
    # FShiftToGrid = TCoord
    # FInvGridStep = TFloat, 1 / GridStep
    # FHighX, FHighY, FHighZ = Integer, max grid cells
    # FPoints = TCoords
    # FRads = TFloats
    # Points = TCoords, GridStep = TFloat, Rads = TFloats
    def __init__(self, Points, GridStep, Rads=None):
        assert Points is not None, 'Empty points array for hashing'
        assert GridStep > 1e-6, 'GridStep too small'
        self.FShiftToGrid = None
        self.FPoints = None
        self.FRads = None
        self.FHighX = 0
        self.FHighY = 0
        self.FHighZ = 0
        self.FInvGridStep = 1 / GridStep
        self.Setup(Points, GridStep, Rads)
        for x in range(self.FHighX + 1):
            for y in range(self.FHighY + 1):
                for z in range(self.FHighZ + 1):
                    self.FHashGrid[x, y, z] = None
        for f in range(len(Points)):
            c = geomutils.Add(Points[f], self.FShiftToGrid)
            x = math.trunc(c[0] * self.FInvGridStep)
            y = math.trunc(c[1] * self.FInvGridStep)
            z = math.trunc(c[2] * self.FInvGridStep)
            basetypes.AddToArray(f, self.FHashGrid)

    def Setup(self, Points, GridStep, Rads):
        maxc = max(Points)
        minc = min(Points)
        self.FShiftToGrid = geomutils.Simmetric(minc)
        self.FHighX = math.trunc((maxc[0] - minc[0]) / GridStep)
        self.FHighY = math.trunc((maxc[1] - minc[1]) / GridStep)
        self.FHighZ = math.trunc((maxc[2] - minc[2]) / GridStep)

        if Rads is None:
            # Geomhasher used only for indexing regions
            self.FPoints = None
            self.FRads = None
        else:
            # Used to detect collisions, inner points, etc
            assert len(Points) is len(Rads), 'Different number of radii and points'
            self.FPoints = Points.copy()
            self.FRads = Rads.copy()

    # Computes bounds of neighboring cells in one dimension, with max of Hi
    # B1, B2 = Integer, Val = const TFloat, Hi = const Integer
    def GridBounds(self, B1, B2, Val, Hi):
        B1 = math.trunc(Val) - 1
        B2 = math.trunc(Val) + 1
        if B1 > Hi:
            B1 = Hi
        if B1 < 0:
            B1 = 0
        if B2 > Hi:
            B2 = Hi
        if B2 < 0:
            B2 = 0
        # (return Integer, Integer in Python)
        return B1, B2

    # Returns all indexes in the 27 grid cells belonging to the neighbourhood of the cell corresponding to C
    # C = TCoord
    def ListNeighbours(self, C):

    # Use only if Rads supplied in create; otherwise points are not kept
    # C = TCoord
    def IsInnerPoint(self, C):
