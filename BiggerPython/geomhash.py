import geomutils
import math
import basetypes


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
        self.FShiftToGrid = basetypes.TCoord()
        self.FPoints = []
        self.FRads = []
        self.FHashGrid = []
        self.FHighX = 0
        self.FHighY = 0
        self.FHighZ = 0
        self.FInvGridStep = 1 / GridStep
        self.Setup(Points, GridStep, Rads)
        self.FHashGrid = []
        for x in range(self.FHighX + 1):
            self.FHashGrid.append([])
            for y in range(self.FHighY + 1):
                self.FHashGrid[x].append([])
                for z in range(self.FHighZ + 1):
                    self.FHashGrid[x][y].append([])
        for f in range(len(Points)):
            c = geomutils.Add(Points[f], self.FShiftToGrid)
            x = math.trunc(c[0] * self.FInvGridStep)
            y = math.trunc(c[1] * self.FInvGridStep)
            z = math.trunc(c[2] * self.FInvGridStep)
            self.FHashGrid[x][y][z].append(f)

    def Setup(self, Points, GridStep, Rads):
        maxc = basetypes.Max(Points)
        minc = basetypes.Min(Points)
        self.FShiftToGrid = geomutils.Simmetric(minc)
        self.FHighX = math.trunc((maxc[0] - minc[0]) / GridStep)
        self.FHighY = math.trunc((maxc[1] - minc[1]) / GridStep)
        self.FHighZ = math.trunc((maxc[2] - minc[2]) / GridStep)

        if Rads is None:
            # Geomhasher used only for indexing regions
            self.FPoints = []
            self.FRads = []
        else:
            # Used to detect collisions, inner points, etc
            assert len(Points) == len(Rads), 'Different number of radii and points'
            self.FPoints = []
            self.FRads = []
            for f in range(len(Points)):
                self.FPoints.append(Points[f])
                self.FRads.append(Rads[f])

    # Computes bounds of neighboring cells in one dimension, with max of Hi
    # (B1, B2 = Integer), Val = const TFloat, Hi = const Integer
    def GridBounds(self, Val, Hi):
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
        C = geomutils.Multiply(geomutils.Add(C, self.FShiftToGrid), self.FInvGridStep)
        Result = []
        x1, x2 = self.GridBounds(C[0], self.FHighX)
        y1, y2 = self.GridBounds(C[1], self.FHighY)
        z1, z2 = self.GridBounds(C[2], self.FHighZ)
        for x in range(x1, x2 + 1):
            for y in range(y1, y2 + 1):
                for z in range(z1, z2 + 1):
                    for f in range(len(self.FHashGrid[x][y][z])):
                        Result.append(self.FHashGrid[x][y][z][f])
        # Return TIntegers
        return Result

    # Use only if Rads supplied in create; otherwise points are not kept
    # C = TCoord
    def IsInnerPoint(self, C):
        tmpc = geomutils.Multiply(geomutils.Add(C, self.FShiftToGrid), self.FInvGridStep)
        Result = False
        x1, x2 = self.GridBounds(tmpc[0], self.FHighX)
        y1, y2 = self.GridBounds(tmpc[1], self.FHighY)
        z1, z2 = self.GridBounds(tmpc[2], self.FHighZ)
        for x in range(x1, x2 + 1):
            for y in range(y1, y2 + 1):
                for z in range(z1, z2 + 1):
                    for f in range(len(self.FHashGrid[x][y][z])):
                        ix = self.FHashGrid[x][y][z][f]
                        if geomutils.Distance(C, self.FPoints[ix]) < self.FRads[ix]:
                            Result = True
                            break
                    if Result:
                        break
                if Result:
                    break
            if Result:
                break
        # Return Boolean
        return Result
