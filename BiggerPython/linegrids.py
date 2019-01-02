import basetypes
import geomutils
import geomhash


# Start, end of segment
class TLineSegment:
    def __init__(self, coord1 = None, coord2 = None):
        self.coord = []
        if coord1 is None:
            coord1 = 0
        if coord2 is None:
            coord2 = 0
        coord1 = int(coord1)
        coord2 = int(coord2)
        self.coord.append(coord1)
        self.coord.append(coord2)

    def __getitem__(self, item):
        return self.coord[item]

    def __setitem__(self, key, value):
        self.coord[key] = value


class TGridShape:
    # Grid = TGridPlane, NonEmpty = TLineArray, CellCounts = array of array of Integer, TotalCount, ZMax = Integer
    def __init__(self):
        self.Grid = []
        self.NonEmpty = []  # X oriented array of non-empty gridlines
        self.CellCounts = []
        self.TotalCount = 0
        self.ZMax = 0


# For domain representation


class TDomainBlock:
    # Values for domain cuboid hull
    # ProbeEndX, ProbeEndY, ProbeEndZ, TargetEndX, TargetEndY, TargetEndZ, XOffset, YOffset, ZOffset, DomainEndX,
    # DomainEndY, DomainEndZ = Integer
    def __init__(self):
        self.ProbeEndX = 0
        self.ProbeEndY = 0
        self.ProbeEndZ = 0
        self.TargetEndX = 0
        self.TargetEndY = 0
        self.TargetEndZ = 0
        self.XOffset = 0
        self.YOffset = 0
        self.ZOffset = 0
        self.DomainEndX = 0
        self.DomainEndY = 0
        self.DomainEndZ = 0


class TDomainGrid:
    def __init__(self):
        self.Block = TDomainBlock()
        self.Shape = TGridShape()  # Full domain


# Grids associated with a structure. Includes base, core and surface
class TDockingGrid:
    # FTransVec = TCoord, FResolution = TFloat, FCoords, FCoreCutCoords = TCoords, FRads, FCoreCutRads = TFloats,
    # FBase, FSurf, FCore = TGridShape
    # AResolution = TFloat
    def __init__(self, AResolution):
        # Translation vector for adjusting coordinates
        # From coords to grid: add
        # From grid to coords: subtract
        self.FTransVec = basetypes.TCoord()
        self.FResolution = AResolution
        # Atomic coordinates are shifted so that min-rad-FSurfThickness==0
        self.FCoords = []
        self.FCoreCutCoords = []
        self.FRads = []
        self.FCoreCutRads = []
        self.FBase = TGridShape()
        self.FSurf = TGridShape()
        self.FCore = TGridShape()

    # X, Y = Integer, Neighs, Surfs, Cores = TIntegers
    def SetSurfCoreLine(self, X, Y, Neighs, Surfs, Cores):
        for z in range(len(self.FBase.Grid[X][Y])):
            for zz in range(self.FBase.Grid[X][Y][z][0], self.FBase.Grid[X][Y][z][1] + 1):
                if Neighs[zz] >= 27:
                    Cores[zz] = 1
                else:
                    Surfs[zz] = 1
        self.FCore.Grid[X][Y] = IntegersToLine(Cores)
        self.FSurf.Grid[X][Y] = IntegersToLine(Surfs)

    # Line = TGridLine, ValArray = TIntegers, Val = Integer
    def SetLineVals(self, Line, ValArray, Val):
        for z in range(len(Line)):
            for zz in range(Line[z][0], Line[z][1] + 1):
                ValArray[zz] = Val

    # Line = TGridLine, Neighs = TIntegers, Mult = Integer
    def UpdateNeighbours(self, Line, Neighs, Mult):
        for z in range(len(Line)):
            bot = Line[z][0]
            top = Line[z][1]
            Neighs[bot - 1] = Neighs[bot - 1] + Mult
            Neighs[top + 1] = Neighs[top + 1] + Mult
            Neighs[bot] = Neighs[bot] + Mult
            if bot < top:
                Neighs[bot] = Neighs[bot] + Mult
                Neighs[top] = Neighs[top] + 2 * Mult
            for zz in range(bot + 1, top):
                Neighs[zz] = Neighs[zz] + 3 * Mult

    # Recomputes neighbour counts and resets surf and core vals to zero
    # X, Y = Integer, Neighs, Surfs, Cores = TIntegers
    def ResetBaselines(self, X, Y, Neighs, Surfs, Cores):
        for f in range(len(Neighs)):
            Neighs[f] = 0
            Surfs[f] = 0
            Cores[f] = 0
        for f in range(X - 1, X + 2):
            for g in range(Y - 1, Y + 2):
                self.UpdateNeighbours(self.FBase.Grid[f][g], Neighs, 1)

    # Incrementally changes Y to next value and updates neighbour surf and core vals
    # X, Y = Integer, Neighs, Surfs, Cores = TIntegers
    def BaseLineIncY(self, X, Y, Neighs, Surfs, Cores):
        self.UpdateNeighbours(self.FBase.Grid[X - 1][Y - 1], Neighs, -1)
        self.UpdateNeighbours(self.FBase.Grid[X][Y - 1], Neighs, -1)
        self.UpdateNeighbours(self.FBase.Grid[X + 1][Y - 1], Neighs, -1)
        self.SetLineVals(self.FBase.Grid[X][Y], Surfs, 0)
        self.SetLineVals(self.FBase.Grid[X][Y], Cores, 0)
        Y = Y + 1
        self.UpdateNeighbours(self.FBase.Grid[X - 1][Y + 1], Neighs, 1)
        self.UpdateNeighbours(self.FBase.Grid[X][Y + 1], Neighs, 1)
        self.UpdateNeighbours(self.FBase.Grid[X + 1][Y + 1], Neighs, 1)
        # In Python return Integer
        return Y

    # Translates structure and creates FBase
    def BuildBaseGrid(self):
        # Adjust coordinates
        maxrad = basetypes.Max(basetypes.Max(self.FRads), self.FResolution)
        # Translate and size guaranteeing one layer of empty grid cells
        self.FTransVec = basetypes.Min(self.FCoords)
        self.FTransVec = geomutils.Add(geomutils.Simmetric(self.FCoords), maxrad + self.FResolution)
        self.FCoords = geomutils.Add(self.FTransVec, self.FCoords)
        top = geomutils.Add(basetypes.Max(self.FCoords), maxrad + 1.5 * self.FResolution)
        self.FBase.Grid = []
        for f in range(round(top[0] / self.FResolution) + 1):
            self.FBase.Grid.append([])
            for g in range(round(top[1] / self.FResolution) + 1):
                self.FBase.Grid[f].append([])

        zline = []
        for f in range(round(top[2] / self.FResolution) + 1):
            zline.append(0)
        self.FBase.ZMax = len(zline) - 1
        hash = geomhash.TGeomHasher(self.FCoords, maxrad, self.FRads)
        halfres = 0.5 * self.FResolution
        xyzpoint = basetypes.TCoord()
        for x in range(len(self.FBase.Grid)):
            xyzpoint[0] = x * self.FResolution + halfres
            for y in range(len(self.FBase.Grid[x])):
                xyzpoint[1] = y * self.FResolution + halfres
                for z in range(len(zline)):
                    xyzpoint[2] = z * self.FResolution + halfres
                    if hash.IsInnerPoint(xyzpoint):
                        zline[z] = 1
                    else:
                        zline[z] = 0
                    self.FBase.Grid[x][y] = IntegersToLine(zline)

    # Builds from Base grid
    def BuildSurfCoreGrids(self):
        self.FCore.Grid = []
        self.FSurf.Grid = []
        for f in range(len(self.FBase.Grid)):
            self.FCore.Grid.append([])
            self.FSurf.Grid.append([])
            for g in range(len(self.FBase.Grid[0])):
                self.FCore.Grid.append([])
                self.FSurf.Grid.append([])
        neighs = []
        surfs = []
        cores = []
        for f in range(self.FBase.ZMax + 1):
            neighs.append(0)
            surfs.append(0)
            cores.append(0)
        for x in range(len(self.FBase.Grid) - 1):
            y = 1
            self.ResetBaselines(x, y, neighs, surfs, cores)
            while True:
                self.SetSurfCoreLine(x, y, neighs, surfs, cores)
                y = self.BaseLineIncY(x, y, neighs, surfs, cores)
                if y >= len(self.FBase.Grid[x]) - 2:
                    break
                self.SetSurfCoreLine(x, y, neighs, surfs, cores)

    # Coords = TCoords, Rads = TFloats, CoreCuts = TCoords, CoreCutRads = TFloats
    def BuildFromSpheres(self, Coords, Rads, CoreCuts = None, CoreCutRads = None):
        assert (len(Coords) == len(Rads)), 'Coordinates and radius values do not match'
        self.FCoords = []
        self.FRads = []
        for f in range(len(Coords)):
            self.FCoords.append(Coords[f])
            self.FRads.append(Rads[f])
        self.FCoreCutCoords = []
        for f in range(len(CoreCuts)):
            self.FCoreCutCoords.append(CoreCuts[f])
        self.FCoreCutRads = []
        for f in range(len(CoreCutRads)):
            self.FCoreCutRads.append(CoreCutRads[f])
        self.BuildBaseGrid()
        self.BuildSurfCoreGrids()
        ComputeShapeStats(self.FSurf)
        ComputeShapeStats(self.FCore)
        self.FCoords = []
        self.FRads = []
        self.FCoreCutCoords = []
        self.FCoreCutRads = []


# First function: Line1, Line2 = TGridLine
# Second function: Line = TGridLine, NewMin, NewMax = const Integer
def Intersect(Line, NewMin, NewMax):
    if isinstance(NewMin, list):
        # First function
        Line1 = Line
        Line2 = NewMin
        Result = []
        ix = 0
        i1 = 0
        i2 = 0
        ll1 = len(Line1)
        ll2 = len(Line2)
        while (i1 < ll1) and (i2 < ll2):
            top = basetypes.Min(Line1[i1][1], Line2[i2][1])
            bot = basetypes.Max(Line1[i1][0], Line2[i2][0])
            if top >= bot:
                Result.append([])
                Result[ix] = TLineSegment(bot, top)
                ix = ix + 1
            if Line1[i1][1] >= Line2[i2][1]:
                ni2 = i2 + 1
            else:
                ni2 = i2
            if Line[i2][1] <= Line2[i2][1]:
                ni1 = i1 + 1
            else:
                ni1 = i1
            i1 = ni1
            i2 = ni2
        # Return TGridLine
        return Result
    else:
        # Second function
        lastvalid = -1
        for f in range(len(Line)):
            # Line segment is inside new interval
            if (Line[f][1] > NewMin) and (Line[f][0] <= NewMax):
                Line[f][0] = basetypes.Max(NewMin, Line[f][0])
                Line[f][1] = basetypes.Min(NewMax, Line[f][1])
                lastvalid = lastvalid + 1
                if lastvalid < f:
                    Line[lastvalid] = Line[f]
        for f in range(len(Line) - 1, lastvalid, -1):
            del Line[f]


# Line = TGridLine, Displacement = Integer
def DisplaceLine(Line, Displacement):
    for f in range(len(Line)):
        Line[f][0] = Line[f][0] + Displacement
        Line[f][1] = Line[f][1] + Displacement


# Ints = const TIntegers, Threshold = const Integer, Limit1, Limit2 = Integer
def IntegersToLine(Ints, Threshold = 0, Limit1 = -1, Limit2 = -1):
    if Limit1 < 0:
        Limit1 = 0
    if Limit2 < 0:
        Limit2 = len(Ints) - 1
    Result = []
    curr = -1
    isin = False
    for f in range(Limit1, Limit2 + 1):
        if Ints[f] > Threshold:
            if not isin:
                isin = True
                curr = curr + 1
                Result.append([])
                Result[curr] = TLineSegment(f)
        elif isin:
            Result[curr][1] = f - 1
            isin = False
    if isin:
        Result[curr][1] = Limit2
    for f in range(Limit2 - Limit1 + 1, curr, -1):
        del Result[f]
    # Return TGridLine
    return Result


# Line = const TGridLine, Ints = TIntegers, Val = Integer
def SetIntegersLine(Line, Ints, Val):
    for f in range(len(Line)):
        for ff in range(Line[f][0], Line[f][1] + 1):
            Ints[ff] = Val


# Shape = TGridShape
def ComputeShapeStats(Shape):
    Shape.ZMax = 0
    Shape.TotalCount = 0
    for x in range(len(Shape.Grid)):
        tmpline = basetypes.FilledInts(len(Shape.Grid[0]), 0)
        for y in range(len(Shape.Grid[0])):
            if Shape.Grid[x][y] is not None and len(Shape.Grid[x][y]) != 0:
                tmpline[y] = 1
                if Shape.ZMax < Shape.Grid[x][y][len(Shape.Grid[x][y])][1]:
                    Shape.ZMax = Shape.Grid[x][y][len(Shape.Grid[x][y])][1]
                Shape.CellCounts[x][y] = CountCells(Shape.Grid[x][y])
                Shape.TotalCount = Shape.TotalCount + Shape.CellCounts[x][y]
        Shape.NonEmpty[x] = IntegersToLine(tmpline, 0)


# Grid ? TGridPlane
def CountSegments(Grid):
    Result = 0
    for x in range(len(Grid)):
        for y in range(len(Grid[x])):
            Result = Result + len(Grid[x][y])
    # Return Integer
    return Result


# Grid = const TGridPlane OR const TGridLine
def CountCells(Grid):
    Result = 0
    if isinstance(Grid[0], TLineSegment):
        for z in range(len(Grid)):
            Result = Result + Grid[z][1] - Grid[z][0] + 1
    else:
        for x in range(len(Grid)):
            for y in range(len(Grid[x])):
                for z in range(len(Grid[x][y])):
                    Result = Result + Grid[x][y][z][1] - Grid[x][y][z][0] + 1
    # Return Integer
    return Result


# Line = const TGridLine, MinZ, MaxZ = const Integer
def CountCellsBetween(Line, MinZ, MaxZ):
    Result = 0
    for z in range(len(Line)):
        if (Line[z][0] <= MaxZ) and (Line[z][1] >= MinZ):
            Result = Result + basetypes.Min(MaxZ, Line[z][1]) - basetypes.Max(Line[z][0], MinZ) + 1
    # Return Integer
    return Result


# Line = const TGridLine
def GetLineExtremes(Line):
    if Line is None or len(Line) == 0:
        LineMin = -1
        LineMax = -2
    else:
        LineMin = Line[0][0]
        LineMax = Line[len(Line) - 1][1]
    # In Python return Integer, Integer
    return LineMin, LineMax


# Ix1, Ix2 = Integer
def NewLine(Ix1, Ix2):
    # Return TGridLine
    return [TLineSegment(Ix1, Ix2)]
