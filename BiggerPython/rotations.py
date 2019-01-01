import surface
import basetypes
import geomutils
import math


class TRotationManager:
    # Copies points
    # FAxes = TCoords, FRotationsPerAxis, FSelectedAxes = TIntegers, FAxialDistance = TFloat, FParaMatrix,
    # FOrtoMatrix, FAxisDistances = TMatrix, FForcedSingleRotation = TFloat, FRotations = TQuaternions,
    # FRotationSelectedAxis = TIntegers, FRotationPoints = TCoordGroups
    # APoints = TCoords
    def __init__(self, APoints):
        # Points defining the shape of the protein
        self.FPoints = []
        for f in range(len(APoints)):
            self.FPoints.append(APoints[f])
        self.FAxes = []
        self.FRotationsPerAxis = []
        self.FSelectedAxes = []
        self.FAxialDistance = 0.0
        # These matrices, axes x points, have the distances parallel and orthogonal to each axis for each point
        self.FParaMatrix = []
        self.FOrtoMatrix = []
        # Squared distances between axes
        self.FAxisDistances = []
        # Set >0 to force one single rotation per quaternion
        self.FForcedSingleRotation = 0.0
        self.FRotations = []
        self.FRotationSelectedAxis = []
        self.FRotationPoints = []

    # Maximum atom displacement for two axes
    # Axis1, Axis2 = Integer
    def AxisDistance(self, Axis1, Axis2):
        if Axis1 is Axis2:
            #Return TFloat
            return 0.0
        else:
            x, y = geomutils.OrthogonalCoords(self.FAxes[Axis1], self.FAxes[Axis2])
            dist = 0
            for g in range(len(self.FPoints)):
                a1x = self.FParaMatrix[Axis1][g]
                a1y = self.FOrtoMatrix[Axis2][g]

                # Points are parallel*(x,y)+- ortho*(y,-x)
                tmpa = self.FParaMatrix[Axis2][g] * x
                tmpb = self.FOrtoMatrix[Axis2][g] * y
                p1x = tmpa + tmpb
                p2x = tmpa - tmpb
                tmpa = self.FParaMatrix[Axis2][g] * y
                tmpb = - self.FOrtoMatrix[Axis2][g] * x
                p1y = tmpa + tmpb
                p2y = tmpa - tmpb

                sp1x = (p1x - a1x) ** 2
                sp2x = (p2x - a1x) ** 2

                dist1 = basetypes.Min(sp1x + (p1y - a1y) ** 2, sp2x + (p2y - a1y) ** 2)
                dist2 = basetypes.Min(sp1x + (p1y + a1y) ** 2, sp2x + (p2y + a1y) ** 2)
                dist = basetypes.Max(dist, basetypes.Max(dist1, dist2))

                dist1 = basetypes.Min(sp1x + (p1y - a1y) ** 2, sp1x + (p1y + a1y) ** 2)
                dist2 = basetypes.Min(sp2x + (p2y - a1y) ** 2, sp2x + (p2y + a1y) ** 2)
                dist = basetypes.Max(dist, basetypes.Max(dist1, dist2))
            # Return TFloat
            return math.sqrt(dist)

    # Returns distance from one axis to all others
    # Axis = Integer
    def AxisDistances(self, Axis):
        Result = []
        for f in range(len(self.FAxes)):
            Result.append(self.AxisDistance(Axis, f))
        # Return TFloats
        return Result

    # Returns rows covered by each column
    # Distances = TMatrix, Displacement = TFloat
    def CoverByColumn(self, Distances, Displacement):
        Result = []
        for f in range(len(Distances)):
            Result.append(0)
            for g in range(len(Distances)):
                if Distances[g][f] < Displacement:
                    Result[f] = Result[f] + 1
        # Return TIntegers
        return Result

    # Counts number of columns below threshold
    # Vec = const TFloats, ColCoverCount, Cols = TIntegers, Thresh = TFloat
    def CountBelow(self, Vec, ColCoverCount, Cols, Thresh):
        Result = 0
        for f in range(len(Cols)):
            if Thresh > Vec[Cols[f]]:
                Result = Result + 1 / (ColCoverCount[Cols[f]] + 1)
        # Return TFloat
        return Result

    # Returns indexes of Cols for those below threshold
    # Vec = const TFloats, Cols = TIntegers, Thresh = TFloat
    def ListBelow(self, Vec, Cols, Thresh):
        Result = []
        for f in range(len(Cols)):
            if Thresh > Vec[Cols[f]]:
                Result.append(f)
        # Return TIntegers
        return Result

    # Returns index of Rows for row with mose coverage
    # Distances = const TMatrix, Displacement = const TFloat, ColCoverCount, Rows, Cols = const TIntegers
    def MaxCoverageRow(self, Distances, Displacement, ColCoverCount, Rows, Cols):
        maxcount = -1
        for f in range(len(Rows)):
            count = self.CountBelow(Distances[Rows[f]], ColCoverCount, Cols, Displacement)
            if count > maxcount:
                maxcount = count
                Result = f
        # Return Integer
        return Result

    # Finds row in Rows with best coverage of columns Cols, removes this from rows, adds to Cols. Due to optimization,
    # rows and cols can be unsorted.
    # Distances = const TMatrix, Displacement = const TFloat, ColCoverCount, Rows, Cols, Selecters = const TIntegers
    def PopMaxCoverageRow(self, Distances, Displacement, ColCoverCount, Rows, Cols, Selecters):
        maxix = self.MaxCoverageRow(Distances, Displacement, ColCoverCount, Rows, Cols)
        coldrops = self.ListBelow(Distances[Rows[maxix]], Cols, Displacement)
        Selecters.append(Rows[maxix])
        basetypes.RemoveFromArray(maxix, Rows)
        basetypes.RemoveFromArray(coldrops, Cols)

    # Minimum per column of distances in rows RowIxs
    # Distances = TMatrix, RowIxs = TIntegers
    def MinDistances(self, Distances, RowIxs):
        if RowIxs is not None:
            Result = []
            for f in range(len(Distances[RowIxs[0]])):
                Result.append(Distances[RowIxs[0]][f])
            for f in range(1, len(RowIxs)):
                row = Distances[RowIxs[f]]
                for g in range(len(Result)):
                    if Result[g] < row[g]:
                        Result[g] = row[g]
            # Return TFloats
            return Result
        else:
            # Return TFloats
            return None

    # Creates a set of quaternions for the axis spaced according to the displacement; if FForcedSingleRotation>0 then
    # only that rotation is created
    # Axis = Integer, Mindist = TFloat
    def CreateQuaternions(self, Axis, MinDist):
        Result = []
        if self.FForcedSingleRotation > 0:
            Result.append(geomutils.RotationQuaternion(self.FAxes[Axis], self.FForcedSingleRotation))
        else:
            maxdist = basetypes.Max(self.FOrtoMatrix[Axis])
            leng = round(2 * maxdist * geomutils.PI / MinDist)
            if leng > 0:
                step = 2 * maxdist * geomutils.PI / leng
                for f in range(leng):
                    Result.append(geomutils.RotationQuaternion(self.FAxes[Axis], f * step))
        # Return TQuaternions
        return Result

    # True if there is no set of points in FRotationPoints with no point outside mindist
    # Points = TCoords, MinDist = TFloat
    def IsRendundant(self, Points, MinDist):
        Result = False
        for f in range(len(self.FRotationPoints)):
            Result = True
            for g in range(len(Points)):
                if geomutils.Distance(Points[g], self.FRotationPoints[f][g]) > MinDist:
                    Result = False
                    break
            if Result:
                break
        # Return Boolean
        return Result

    # Adds a filtered set of quaternions to FRotations
    # Axis = Integer, MinDist, Filter = TFloat, SelectedIndex = Integer
    def AddQuaternions(self, Axis, MinDist, Filter, SelectedIndex):
        quaternions = self.CreateQuaternions(Axis, MinDist)
        points = []
        toinsert = []
        insertcount = 0
        for f in range(len(quaternions)):
            points.append(geomutils.Rotate(self.FPoints, quaternions[f]))
            toinsert.append(not self.IsRendundant(points[f], Filter))
            if toinsert[f]:
                insertcount = insertcount + 1
        if insertcount > 0:
            for f in range(len(toinsert)):
                if toinsert[f]:
                    self.FRotations.append(quaternions[f])
                    self.FRotationSelectedAxis.append(SelectedIndex)
                    self.FRotationPoints.append(points[f])
        # Return Integer
        return insertcount

    def Rotations(self):
        Result = []
        for f in range(len(self.FRotations)):
            Result.append(self.FRotations[f])
        # Return TQuaternions
        return Result

    def SelectedAxes(self):
        Result = []
        for f in range(len(self.FSelectedAxes)):
            Result.append(self.FAxes[self.FSelectedAxes[f]])
        # Return TCoords
        return Result

    def AxisIndexes(self):
        Result = []
        for f in range(len(self.FRotationSelectedAxis)):
            Result.append(self.FRotationSelectedAxis[f])
        # Return TIntegers
        return Result

    def ClearMatrices(self):
        self.FAxes = []
        self.FSelectedAxes = []
        self.FRotations = []
        self.FParaMatrix = []
        self.FOrtoMatrix = []
        self.FAxisDistances = []

    # Create initial axes and distance matrix, clears
    # BaseCount = Integer
    def InitializeAxes(self, BaseCount):
        self.FAxes = BaseSampleAxes(BaseCount)
        self.FParaMatrix = []
        self.FOrtoMatrix = []
        for f in range(len(self.FAxes)):
            self.FParaMatrix.append([])
            self.FOrtoMatrix.append([])
            for g in range(len(self.FPoints)):
                x, y = geomutils.OrthogonalCoords(self.FAxes[f], self.FPoints[g])
                self.FParaMatrix[f].append(x)
                self.FOrtoMatrix[f].append(y)

    def ComputeAxisDistances(self):
        self.FAxisDistances = []
        for f in range(len(self.FAxes)):
            self.FAxisDistances.append([])
            for g in range(len(self.FAxes)):
                self.FAxisDistances[f].append(0)
        for ax1 in range(len(self.FAxes) - 1):
            for ax2 in range(ax1 + 1, len(self.FAxes)):
                dist = self.AxisDistance(ax1, ax2)
                self.FAxisDistances[ax1][ax2] = dist
                self.FAxisDistances[ax2][ax1] = dist

    # Displacement = TFloat
    def SelectAxesByCoverage(self, Displacement):
        self.FSelectedAxes = []
        colcovercount = self.CoverByColumn(self.FAxisDistances, Displacement)
        rows = []
        columns = []
        for f in range(len(self.FAxes)):
            rows.append(f)
            columns.append(f)
        while columns is not None:
            self.PopMaxCoverageRow(self.FAxisDistances, Displacement, colcovercount, rows, columns, self.FSelectedAxes)

    # Does not use FAxialDistances matrix
    # MinDist = TFloat, MaxAxes = Integer
    def SelectAxesByMaxmin(self, MinDist, MaxAxes):
        self.FSelectedAxes = basetypes.FilledInts(1, len(self.FAxes))
        # This selects the last axis as te first point. The last axis is the Z axis
        rows = basetypes.FilledInts(1, 0)  # This is just a 0...N index to avoid using the FAxialDistances matrix
        distances = [self.AxisDistances(self.FSelectedAxes[0])]
        for f in range(1, MaxAxes):
            distances.append([])
        maxd = MinDist + 1
        while (maxd > MinDist) and (len(self.FSelectedAxes) < MaxAxes):
            mins = self.MinDistances(distances, rows)
            maxd, maxix = basetypes.MaxValIx(mins)
            self.FSelectedAxes.append(maxix)
            rows.append(len(self.FSelectedAxes) - 1)
            distances[len(self.FSelectedAxes) - 1] = self.AxisDistances(maxix)
        self.FAxialDistance = maxd

    # Monomers = Integer
    def ForceSingleRotation(self, Monomers):
        if Monomers > 0:
            self.FForcedSingleRotation = 2 * geomutils.PI / Monomers
        else:
            self.FForcedSingleRotation = -1

    # MinDist, Filter = TFloat
    def GenerateQuaternions(self, MinDist, Filter):
        self.FRotations = []
        self.FRotationSelectedAxis = []
        self.FRotationPoints = []
        self.FRotationsPerAxis = []
        for f in range(len(self.FSelectedAxes)):
            self.FRotationsPerAxis.append(self.AddQuaternions(self.FSelectedAxes[f], MinDist, Filter, f))

    # Generate axes and quaternions using the maximum distance greedy optimization; does not create the distance
    # matrix
    # BaseCount = Integer, Displacement = TFloat, Symmetry, MaxAxes = Integer
    def MaxDistQuaternions(self, BaseCount, Displacement, Symmetry = -1, MaxAxes = -1):
        if MaxAxes < 1:
            MaxAxes = BaseCount
        self.ForceSingleRotation(Symmetry)
        self.InitializeAxes(BaseCount)
        self.SelectAxesByMaxmin(Displacement, MaxAxes)
        self.GenerateQuaternions(Displacement * 2, Displacement)

    def GetSelectedAxes(self):
        Result = []
        for f in range(len(self.FSelectedAxes)):
            Result.append(self.FAxes[self.FSelectedAxes[f]])
        # Return TCoords
        return Result

    # Axis = Integer
    def MinDist(self, Axis):
        Result = 1e6
        for f in range(len(self.FSelectedAxes)):
            if (f is not Axis) and (self.FAxisDistances[self.FSelectedAxes[Axis]][self.FSelectedAxes[f]] < Result):
                Result = self.FAxisDistances[self.FSelectedAxes[Axis]][self.FSelectedAxes[f]]
        # Return TFloat
        return math.sqrt(Result)

    # Axis = Integer
    def RotationCount(self, Axis):
        # Return Integer
        return self.FRotationsPerAxis[Axis]


# Count axes spread over hemisphere, using the spiral algorithm
# Returns approximately count axis with z>=0
# Count = Integer
def BaseSampleAxes(Count):
    Result = surface.GoldenSpiralPoints(Count * 2)
    ix = 0
    for f in range(len(Result)):
        if Result[f][2] >= 0:
            Result[ix] = Result[f]
            ix = ix + 1
    # Return TCoords
    return Result


# These are deprecated (leave them?)


# Rotates around each axis by ZSteps. Axes are generated by points distributed around sphere (only for z>0, to
# eliminate symmetry redundancy)
# ZSteps = Integer, Axes = TCoords
def UniformSampleAngles(ZSteps, Axes):
    if ZSteps is 1:
        # Return TQuaternions
        return [geomutils.IdentityQuaternion]
    else:
        sphere = surface.GoldenSpiralPoints(ZSteps * ZSteps)
        Axes = []  # Maximum lengths, to be cut
        Result = [geomutils.IdentityQuaternion]
        ix = 1
        for f in range(len(sphere)):
            if sphere[f][2] >= 0:  # Only top hemisphere
                for g in range(1, ZSteps):
                    rot = 2 * geomutils.PI / ZSteps * g
                    Result.append(geomutils.RotationQuaternion(sphere[f], rot))
                    ix = ix + 1
        for f in range(len(Axes) - 1, ix, -1):
            del Axes[f]
        # Return TQuaternions (and TCoords in Python)
        return Result, Axes


# Rotates around Z axis ZSteps, for each rotates pole to one point in sphere
# ZSteps = Integer, ZRot = TFloat, Axes = TCoords, Margin = Integer
def FixedZSampleAngles(ZSteps, ZRot, Axes, Margin = 0):
    if ZSteps is 1:
        # Return TQuaternions
        return [geomutils.IdentityQuaternion]
    else:
        # Get ZSteps^2 points uniformly distributed around a sphere
        sphere = surface.GoldenSpiralPoints(ZSteps * ZSteps)
        Result = []
        ix = 0
        for f in range(len(sphere)):
            if sphere[f][2] >= 0:
                ix = PushRotation(sphere[f], ZRot, Result, Axes, ix)
                for g in range(1, Margin + 1):
                    ix = PushRotation(sphere[f], ZRot + g * 2 * geomutils.PI / ZSteps, Result, Axes, ix)
                    ix = PushRotation(sphere[f], ZRot - g * 2 * geomutils.PI / ZSteps, Result, Axes, ix)
        for f in range(len(Axes) - 1, ix, -1):
            del Axes[f]
        # Return TQuaternions (and TCoords in Python)
        return Result, Axes


# Axis = TCoord, Rot = TFloat, Result = TQuaternions, Axes = TCoords, ix = Integer
def PushRotation(Axis, Rot, Result, Axes, ix):
    Result.append(geomutils.RotationQuaternion(Axis, Rot))
    Axes[ix] = Axis
    ix = ix + 1
    # In Python return Integer
    return ix


# ZSteps = Integer
def EulerSampleZYX(ZSteps):
    # Get ZSteps^2 points uniformly distributed around a sphere
    Result = []
    for z in range(ZSteps):
        qz = geomutils.RotationQuaternion(basetypes.TCoord(0, 0, 1), 2 * geomutils.PI / ZSteps * z)
        for y in range(ZSteps):
            qy = geomutils.RotationQuaternion(basetypes.TCoord(0, 1, 0), 2 * geomutils.PI / ZSteps * y)
            for x in range(int(ZSteps / 2) - 1):
                qx = geomutils.RotationQuaternion(basetypes.TCoord(1, 0, 0), 2 * geomutils.PI / ZSteps * x)
                Result.append(geomutils.Multiply(geomutils.Multiply(qz, qy), qx))
    # Return TQuaternions
    return Result


# Return point with largest minimum distance from subset of points
# Points, Subset = const TCoords, LastPoint = Integer
def FarthestPoint(Points, Subset, LastPoint = -1):
    if LastPoint < 0:
        LastPoint = len(Subset) - 1
    Result = -1
    best = -1
    for f in range(len(Points)):
        tmp = geomutils.Distance(Points[f], Subset[0])
        for g in range(1, LastPoint):
            d = geomutils.Distance(Points[f], Subset[g])
            if d < tmp:
                tmp = d
            if tmp > best:
                best = tmp
                Result = f
    # Return Integer
    return Result


# Returns a set of NumPoints integers with indexes for the most widely spaced points
# Coords = TCoords, NumPoints = Integer
def SpacedPoints(Coords, NumPoints):
    if NumPoints > len(Coords):
        NumPoints = len(Coords)
    Result = []
    subset = [geomutils.MidPoint(Coords)]
    ix = FarthestPoint(Coords, subset, 0)
    subset[0] = Coords[ix]
    Result.append(ix)
    for f in range(1, NumPoints):
        Result.append(FarthestPoint(Coords, subset, f - 1))
        subset.append(Coords[Result[f]])
    # Return TIntegers
    return Result


# Returns an array of coords for the most widely spaced points
# Coords = TCoords, NumPoints = Integer
def SpacedCoords(Coords, NumPoints):
    selixs = SpacedPoints(Coords, NumPoints)
    Result = []
    for f in range(len(selixs)):
        Result.append(Coords[selixs[f]])
    # Return TCoords
    return Result
