import basetypes
import linegrids
import geomutils
import docktasks
import math


class TDockConstraintManager:
    # FProbeTransVec, FTargetTransVec = TCoord, FTargetResolution, FProbeResolution = TFloat,
    # FDomainBlock = TDomainBlock, FXDomain = TGridLine, FYDomainAtX = TLineArray, FZDomainAtXY = TGridPlane
    # Probe, Target = TDockingGrid
    def __init__(self, Probe, Target):
        self.FProbeTransVec = Probe.FTransVec
        self.FTargetTransVec = Target.FTransVec
        # Resolutions should be identical, but best not hard code that
        self.FTargetResolution = Target.FResolution
        self.FProbeResolution = Probe.FResolution
        # Base domain block
        self.FDomainBlock = linegrids.TDomainBlock()
        self.FDomainBlock.ProbeEndX = len(Probe.FSurf.Grid) - 1
        self.FDomainBlock.ProbeEndY = len(Probe.FSurf.Grid[0]) - 1
        self.FDomainBlock.ProbeEndZ = Probe.FSurf.ZMax
        self.FDomainBlock.TargetEndX = len(Target.FSurf.Grid) - 1
        self.FDomainBlock.TargetEndY = len(Target.FSurf.Grid[0]) - 1
        self.FDomainBlock.TargetEndZ = Target.FSurf.ZMax
        self.FDomainBlock.XOffset = - self.FDomainBlock.ProbeEndX
        self.FDomainBlock.YOffset = - self.FDomainBlock.ProbeEndY
        self.FDomainBlock.ZOffset = - self.FDomainBlock.ProbeEndZ
        self.FDomainBlock.DomainEndX = self.FDomainBlock.TargetEndX - self.FDomainBlock.XOffset
        self.FDomainBlock.DomainEndY = self.FDomainBlock.TargetEndY - self.FDomainBlock.YOffset
        self.FDomainBlock.DomainEndZ = self.FDomainBlock.TargetEndZ - self.FDomainBlock.ZOffset
        self.FXDomain = linegrids.NewLine(0, self.FDomainBlock.DomainEndX)
        self.FYDomainAtX = []
        self.FZDomainAtXY = []
        for x in range(self.FDomainBlock.DomainEndX + 1):
            self.FYDomainAtX.append(linegrids.NewLine(0, self.FDomainBlock.DomainEndY))
            self.FZDomainAtXY.append([])
            for y in range(self.FDomainBlock.DomainEndY + 1):
                self.FZDomainAtXY[x].append(linegrids.NewLine(0, self.FDomainBlock.DomainEndZ))

    # DomainX = Integer
    def YDomainAtX(self, DomainX):
        # Return TGridLine
        return self.FYDomainAtX[DomainX]

    # DomainX, DomainY = Integer
    def ZDomainAtXY(self, DomainX, DomainY):
        # Return TGridLine
        return self.FZDomainAtXY[DomainX][DomainY]

    # ATargetPoints, AProbePoints = const TCoords, Dist = TFloat
    def LinearDistanceConstraint(self, ATargetPoints, AProbePoints, Dist):
        targetpoints = geomutils.Add(self.FTargetTransVec, ATargetPoints)
        probepoints = geomutils.Add(self.FProbeTransVec, AProbePoints)
        tres = 1 / self.FTargetResolution
        pres = 1 / self.FProbeResolution

        mit = geomutils.Multiply(basetypes.Min(targetpoints), tres)
        mat = geomutils.Multiply(basetypes.Max(targetpoints), tres)
        mip = geomutils.Multiply(basetypes.Min(probepoints), tres)
        map = geomutils.Multiply(basetypes.Max(probepoints), tres)
        d = Dist / self.FTargetResolution

        lx1 = round(mit[0] - map[0] - self.FDomainBlock.XOffset - 0.5 - d)
        lx2 = round(mat[0] - mip[0] - self.FDomainBlock.XOffset + 0.5 + d)
        ly1 = round(mit[1] - map[1] - self.FDomainBlock.YOffset - 0.5 - d)
        ly2 = round(mat[1] - mip[1] - self.FDomainBlock.YOffset + 0.5 + d)
        lz1 = round(mit[2] - map[2] - self.FDomainBlock.ZOffset - 0.5 - d)
        lz2 = round(mat[2] - mip[2] - self.FDomainBlock.ZOffset + 0.5 + d)
        linegrids.Intersect(self.FXDomain, lx1, lx2)

        # Clear FYDomainAtX outside FXDomain
        oldix = 0
        for x in range(len(self.FXDomain)):
            for xx in range(oldix, self.FXDomain[x][0]):
                self.FYDomainAtX[xx] = []
            oldix = self.FXDomain[x][1] + 1
        for xx in range(oldix, len(self.FYDomainAtX)):
            self.FYDomainAtX[xx] = []

        # Set FYDomainAtX
        for x in range(len(self.FXDomain)):
            for xx in range(self.FXDomain[x][0], self.FXDomain[x][1] + 1):
                linegrids.Intersect(self.FYDomainAtX[xx], ly1, ly2)
                # Clear FZDomainAtXY outside FYDomainAtX
                oldix = 0
                for y in range(len(self.FYDomainAtX[xx])):
                    for yy in range(oldix, self.FYDomainAtX[xx][y][0]):
                        self.FZDomainAtXY[xx][yy] = []
                    oldix = self.FYDomainAtX[xx][y][1] + 1
                for yy in range(oldix, len(self.FYDomainAtX[xx])):
                    self.FZDomainAtXY[xx][yy] = []

                # Set FZDomainAtXY
                for y in range(len(self.FYDomainAtX[xx])):
                    for yy in range(self.FYDomainAtX[xx][y][0], self.FYDomainAtX[xx][y][1] + 1):
                        linegrids.Intersect(self.FZDomainAtXY[xx][yy], lz1, lz2)

    # ATargetPoints, AProbePoints = const TCoords, Dist = TFloat
    def EuclideanDistanceConstraint(self, ATargetPoints, AProbePoints, Dist):
        tres = 1 / self.FTargetResolution
        pres = 1 / self.FProbeResolution
        targetpoints = geomutils.Multiply(geomutils.Add(self.FTargetTransVec, ATargetPoints), tres)
        probepoints = geomutils.Add(basetypes.TCoord(self.FDomainBlock.XOffset, self.FDomainBlock.YOffset,
                                                     self.FDomainBlock.ZOffset), geomutils.Multiply(
            geomutils.Add(self.FProbeTransVec, AProbePoints), pres))
        mit = basetypes.Min(targetpoints)
        mat = basetypes.Max(targetpoints)
        mip = basetypes.Min(probepoints)
        map = basetypes.Max(probepoints)
        griddist = Dist / self.FTargetResolution
        distsquare = griddist ** 2

        lx1 = round(mit[0] - map[0] - 0.5 - griddist)
        lx2 = round(mat[0] - mip[0] + 0.5 + griddist)
        linegrids.Intersect(self.FXDomain, lx1, lx2)
        # Clear FYDomainAtX outside FXDomain
        oldix = 0
        for x in range(len(self.FXDomain)):
            for xx in range(oldix, self.FXDomain[x][0]):
                self.FYDomainAtX[xx] = []
            oldix = self.FXDomain[x][1] + 1
        for xx in range(oldix, len(self.FYDomainAtX)):
            self.FYDomainAtX[xx] = []

        # Set FYDomainAtX
        for x in range(len(self.FXDomain)):
            for xx in range(self.FXDomain[x][0], self.FXDomain[x][1] + 1):
                ly1, ly2 = self.SetYLimits(xx, targetpoints, probepoints, griddist, distsquare)
                linegrids.Intersect(self.FYDomainAtX[xx], ly1, ly2)

                # Clear FZDomainAtXY outside FYDomainAtX
                oldix = 0
                for y in range(len(self.FYDomainAtX[xx])):
                    for yy in range(oldix, self.FYDomainAtX[xx][y][0]):
                        self.FZDomainAtXY[xx][yy] = []
                    oldix = self.FYDomainAtX[xx][y][1] + 1
                for yy in range(oldix, len(self.FZDomainAtXY[xx])):
                    self.FZDomainAtXY[xx][yy] = []

                # Set FZDomainAtXY
                for y in range(len(self.FYDomainAtX[xx])):
                    for yy in range(self.FYDomainAtX[xx][y][0], self.FYDomainAtX[xx][y][1]):
                        lz1, lz2 = self.SetZLimits(xx, yy, targetpoints, probepoints, griddist, distsquare)
                        linegrids.Intersect(self.FZDomainAtXY[xx][yy], lz1, lz2)

    # XCoord = Integer (, targetpoints, probepoints = TCoords, griddist, distsquare = TFloat in Python)
    def SetYLimits(self, XCoord, targetpoints, probepoints, griddist, distsquare):
        ly1 = self.FDomainBlock.DomainEndY
        ly2 = 0
        for f in range(len(targetpoints)):
            for g in range(len(probepoints)):
                dif = abs(probepoints[g][0] + XCoord - targetpoints[f][0])
                if dif <= griddist:
                    dy = math.sqrt(distsquare - (dif ** 2))
                    dif = targetpoints[f][1] - probepoints[g][1]
                    ly1 = basetypes.Min(ly1, round(dif - 0.5 - dy))
                    ly2 = basetypes.Max(ly2, round(dif + 0.5 + dy))
        # In Python return Integer, Integer
        return ly1, ly2

    # XCoord, YCoord = Integer (, targetpoints, probepoints = TCoords, griddist, distsquare = TFloat in Python)
    def SetZLimits(self, XCoord, YCoord, targetpoints, probepoints, griddist, distsquare):
        lz1 = self.FDomainBlock.DomainEndZ
        lz2 = 0
        for f in range(len(targetpoints)):
            for g in range(len(probepoints)):
                dif = abs(probepoints[g][0] + XCoord - targetpoints[f][0])
                if dif <= griddist:
                    dif = (dif ** 2) - (probepoints[g][1] + YCoord - targetpoints[f][1]) ** 2
                    if dif <= distsquare:
                        dz = math.sqrt(distsquare - dif)
                        dif = targetpoints[f][2] - probepoints[g][2]
                        lz1 = basetypes.Min(lz1, round(dif - 0.5 - dz))
                        lz2 = basetypes.Max(lz2, round(dif + 0.5 + dz))
        # In Python return Integer, Integer
        return lz1, lz2

    # NOTE: Normal vector must be normalized
    # Point, Normal = const TCoord, Margin = const TFloat
    def PlaneConstraint(self, Point, Normal, Margin):
        # Scaling factors for the shapes
        tres = 1 / self.FTargetResolution
        pres = 1 / self.FProbeResolution
        # Plane and line variables
        planepoint = geomutils.Subtract(geomutils.Add(Point, geomutils.Multiply(self.FTargetTransVec, tres)),
                                        geomutils.Multiply(self.FProbeTransVec, pres))
        planepoint = geomutils.Subtract(planepoint, basetypes.TCoord(self.FDomainBlock.XOffset,
                                                                     self.FDomainBlock.YOffset,
                                                                     self.FDomainBlock.ZOffset))
        xflags = basetypes.FilledInts(self.FDomainBlock.DomainEndX + 1, -1)
        yflags = basetypes.FilledInts(self.FDomainBlock.DomainEndY + 1, -1)
        zflags = basetypes.FilledInts(self.FDomainBlock.DomainEndZ + 1, -1)
        for x in range(len(self.FXDomain)):
            for xx in range(self.FXDomain[x][0], self.FXDomain[x][1] + 1):
                self.SetYDomain(xx, yflags, zflags, planepoint, Normal, Margin)
                if self.FYDomainAtX[xx] is not None and (len(self.FYDomainAtX[xx]) != 0):
                    xflags[xx] = 1
        self.FXDomain = linegrids.IntegersToLine(xflags)

    # Computes distance from point to plane for each point. This seems necessary to give the proper margin...
    # xx, yy = Integer (, zflags = TIntegers, planepoint, Normal = TCoord, Margin = TFloat in Python)
    def SetZDomain(self, xx, yy, zflags, planepoint, Normal, Margin):
        linetoplane = basetypes.TCoord(planepoint[0] - xx, planepoint[1] - yy, planepoint[2])
        for z in range(len(self.FZDomainAtXY[xx][yy])):
            for zz in range(self.FZDomainAtXY[xx][yy][z][0], self.FZDomainAtXY[xx][yy][z][1] + 1):
                linetoplane[2] = planepoint[2] - zz
                dist = abs(geomutils.DotProduct(linetoplane, Normal))
                if dist <= Margin:
                    zflags[zz] = 1
        self.FZDomainAtXY[xx][yy] = linegrids.IntegersToLine(zflags)
        # Cleanup zflags
        linegrids.SetIntegersLine(self.FZDomainAtXY[xx][yy], zflags, -1)

    # xx = Integer (, yflags, zflags = TIntegers, planepoint, Normal = TCoord, Margin = TFloat in Python)
    def SetYDomain(self, xx, yflags, zflags, planepoint, Normal, Margin):
        for y in range(len(self.FYDomainAtX[xx])):
            for yy in range(self.FYDomainAtX[xx][y][0], self.FYDomainAtX[xx][y][1] + 1):
                self.SetZDomain(xx, yy, zflags, planepoint, Normal, Margin)
                if self.FZDomainAtXY[xx][yy] is not None and (len(self.FZDomainAtXY[xx][yy]) != 0):
                    yflags[yy] = 1
        self.FYDomainAtX[xx] = linegrids.IntegersToLine(yflags)
        # Cleanup yflags
        linegrids.SetIntegersLine(self.FYDomainAtX[xx], yflags, -1)

    # Constraints = TConstraintDefs, ProbeRot = TQuaternion, ProbeAxis = TCoord
    def ImportConstraints(self, Constraints, ProbeRot, ProbeAxis):
        for f in range(len(Constraints)):
            if Constraints[f].ConstraintType is docktasks.CEuclideanDistance:
                self.EuclideanDistanceConstraint(Constraints[f].TargetPoints,
                                                 geomutils.Rotate(Constraints[f].ProbePouints, ProbeRot),
                                                 Constraints[f].Distance)
            elif Constraints[f].ConstraintType is docktasks.CNormalPlane:
                self.PlaneConstraint(basetypes.TCoord(), ProbeAxis, Constraints[f].Distance)
