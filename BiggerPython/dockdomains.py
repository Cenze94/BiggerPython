import basetypes
import linegrids
import geomutils
import dockconstraints


class TDockDomain:
    # FTarget, FProbe = TDockingGrid, RemoveCores = Boolean, MinimumOverlap = Integer,
    # FConstraintManager = TDockConstraintManager, FAddModel = function (Score, X, Y, Z:Integer):Integer of object,
    # FDomainGrid = TDomainGrid, FCoreContacts = TLineArray, FCoarseDomain = TDockDomain, FCoarseTarget,
    # FCoarseProbe = TDockingGrid, FIntersect = TIntegers, FIntersectHigh = Integer, FScores, FOverlapixs = TIntegers,
    # CountXY, CountXYCore, CountX = Integer
    # ATarget, AProbe = TDockingGrid, MaxGrids = Integer
    def __init__(self, ATarget, AProbe, MaxGrids):
        # Target and Probe grids are NOT managed by this class
        self.FTarget = ATarget
        self.FProbe = AProbe
        self.RemoveCores = True
        self.MinimumOverlap = 0
        self.FConstraintManager = dockconstraints.TDockConstraintManager(self.FProbe, self.FTarget)
        self.FAddModel = None
        self.FDomainGrid = None
        # X oriented array of gridlines where core contacts can occur in the domain
        self.FCoreContacts = []
        # For multigrid speedup
        self.FCoarseDomain = None
        self.FCoarseTarget = None
        self.FCoarseProbe = None
        # Coarse grids and domain are created and managed by this class (frees coarse grids after freeing coarse domain)
        # For InPlaceIntersect
        self.FIntersect = []
        self.FIntersectHigh = -1
        # For scoring and overlaping without creating integer arrays
        self.FScores = []
        self.FOverlapIxs = []
        self.CountXY = 0
        self.CountXYCore = 0
        self.CountX = 0

    # Uses FIntersect and FIntersectHigh to avoid creating new lines. This removes about 10% in domain generation time
    # (7.55 to 6.7 s tested on 6k atoms)
    # Line1, Line2 = const TGridLine, Displace = const Integer
    def InPlaceIntersect(self, Line1, Line2, Displace):
        self.FIntersectHigh = -1
        if (Line1 is not None) and (len(Line1) != 0) and (Line2 is not None) and (len(Line2) != 0):
            ll1 = len(Line1) + len(Line2)
            if len(self.FIntersect) < ll1:
                for f in range(len(self.FIntersect), ll1):
                    self.FIntersect.append(linegrids.TLineSegment())
            i1 = 0
            i2 = 0
            ll1 = len(Line1)
            ll2 = len(Line2)
            while (i1 < ll1) and (i2 < ll2):
                top = basetypes.Min(Line1[i1][1] + Displace, Line2[i2][1])
                bot = basetypes.Max(Line1[i1][0] + Displace, Line2[i2][0])
                if top >= bot:
                    self.FIntersectHigh = self.FIntersectHigh + 1
                    self.FIntersect[self.FIntersectHigh][0] = bot
                    self.FIntersect[self.FIntersectHigh][1] = top
                if Line1[i1][1] + Displace >= Line2[i2][1]:
                    ni2 = i2 + 1
                else:
                    ni2 = i2
                if Line1[i1][1] + Displace <= Line2[i2][1]:
                    ni1 = i1 + 1
                else:
                    ni1 = i1
                i1 = ni1
                i2 = ni2

    def SetDomainYLines(self):
        for f in range(self.FDomainGrid.Block.DomainEndX):
            self.FDomainGrid.Shape.NonEmpty.append([])
            self.FCoreContacts.append([])
        for f in range(len(self.FDomainGrid.Block.DomainEndX) - 1, len(self.FDomainGrid.Shape.NonEmpty) - 1):
            del self.FDomainGrid.Shape.NonEmpty[f]
        for f in range(len(self.FDomainGrid.Block.DomainEndX) - 1, len(self.FCoreContacts) - 1):
            del self.FCoreContacts[f]
        xdomain = self.FConstraintManager.FXDomain
        for xx in range(len(xdomain)):
            for x in range(xdomain[xx][0], xdomain[xx][1]):
                self.CountX = self.CountX + 1
                limit1, limit2 = linegrids.GetLineExtremes(self.FConstraintManager.YDomainAtX(x))
                if limit1 > limit2:
                    self.FDomainGrid.Shape.NonEmpty[x] = []
                    self.FCoreContacts[x] = []
                else:
                    pix1, pix2, tix1, tix2 = GetIndexes(self.FDomainGrid.Block.ProbeEndX,
                                                        self.FDomainGrid.Block.TargetEndX,
                                                        self.FDomainGrid.Block.XOffset, x)
                    tmpmin = limit2 + 1
                    tmpmax = -1
                    coremin = limit2 + 1
                    coremax = -1
                    while tix1 <= tix2:
                        # Surface overlap Y range
                        if (self.FProbe.Surf.NonEmpty[pix1] is not None) and (len(self.FProbe.Surf.NonEmpty[pix1]) != 0) \
                                and (self.FTarget.Surf.NonEmpty[tix1] is not None) and \
                                (len(self.FTarget.Surf.NonEmpty[tix1]) != 0):
                            p1, p2 = linegrids.GetLineExtremes(self.FProbe.Surf.NonEmpty[pix1])
                            t1, t2 = linegrids.GetLineExtremes(self.FTarget.Surf.NonEmpty[tix1])
                            mi, ma = OverlapRegion(p1, p2, t1, t2, self.FDomainGrid.Block.YOffset, limit2)
                            tmpmin = basetypes.Min(mi, tmpmin)
                            tmpmax = basetypes.Max(ma, tmpmax)
                        # Core overlap Y range
                        if (self.FProbe.Core.NonEmpty[pix1] is not None) and (len(self.FProbe.Core.NonEmpty[pix1]) != 0) \
                                and (self.FTarget.Core.NonEmpty[tix1] is not None) and \
                                (len(self.FTarget.Core.NonEmpty[tix1]) != 0):
                            p1, p2 = linegrids.GetLineExtremes(self.FProbe.Core.NonEmpty[pix1])
                            t1, t2 = linegrids.GetLineExtremes(self.FTarget.Core.NonEmpty[tix1])
                            mi, ma = OverlapRegion(p1, p2, t1, t2, self.FDomainGrid.Block.YOffset, limit2)
                            coremin = basetypes.Min(mi, coremin)
                            coremax = basetypes.Max(ma, coremax)
                        tix1 = tix1 + 1
                        pix1 = pix1 + 1
                    tmpmin = basetypes.Max(limit1, tmpmin)
                    tmpmax = basetypes.Min(limit2, tmpmax)
                    coremin = basetypes.Max(limit1, coremin)
                    coremax = basetypes.Min(limit2, coremax)
                    if tmpmax < tmpmin:
                        self.FDomainGrid.Shape.NonEmpty[x] = []
                    else:
                        self.FDomainGrid.Shape.NonEmpty[x] = linegrids.NewLine(tmpmin, tmpmax)
                    if coremax < coremin:
                        self.FCoreContacts[x] = None
                    else:
                        self.FCoreContacts[x] = linegrids.NewLine(coremin, coremax)

    def SetDomainZLines(self):
        # Large buffer for intersections
        for f in range(len(self.FIntersect) - 1, 100):
            self.FIntersect.append(0)
        for f in range(len(self.FIntersect), len(self.FIntersect) - 1, -1):
            del self.FIntersect[f]
        self.FIntersectHigh = -1
        # Clear grid and set to nil to distinguish lines where core was processed
        self.FDomainGrid.Shape.Grid = []
        for f in range(self.FDomainGrid.Block.DomainEndX + 1):
            self.FDomainGrid.Shape.Grid.append([])
            for g in range(self.FDomainGrid.Block.DomainEndY + 1):
                self.FDomainGrid.Shape.Grid[f].append([])
        # Set lines with surface contacts
        self.FOverlapIxs = basetypes.FilledInts(2 * self.FDomainGrid.Block.ProbeEndZ +
                                                self.FDomainGrid.Block.TargetEndZ, 0)
        # FOverlapIxs used with accumulator, must span all range of surface, not actual domain
        for x in range(len(self.FDomainGrid.Shape.NonEmpty)):
            for y in range(len(self.FDomainGrid.Shape.NonEmpty[x])):
                for yy in range(self.FDomainGrid.Shape.NonEmpty[x][y][0], self.FDomainGrid.Shape.NonEmpty[x][y][1] +
                                                                          1):
                    self.SetZExtremes(x, yy)
        # Set lines with core overlaps
        if self.RemoveCores:
            self.FOverlapIxs = basetypes.FilledInts(self.FDomainGrid.Block.DomainEndZ + 1, 0)
            for x in range(len(self.FCoreContacts)):
                for y in range(len(self.FCoreContacts[x])):
                    for yy in range(self.FCoreContacts[x][y][0], self.FCoreContacts[x][y][1]):
                        if self.FDomainGrid.Shape.Grid[x][yy] is not None and \
                                len(self.FDomainGrid.Shape.Grid[x][yy]) != 0:
                            self.RemoveZCores(x, yy)

    # DomainX, DomainY = Integer
    def SetZExtremes(self, DomainX, DomainY):
        self.CountXY = self.CountXY + 1
        for z in range(len(self.FOverlapIxs)):
            self.FOverlapIxs[z] = 0
        limit1, limit2 = linegrids.GetLineExtremes(self.FConstraintManager.ZDomainAtXY(DomainX, DomainY))
        pix1, pix2, tix1, tix2 = GetIndexes(self.FDomainGrid.Block.ProbeEndX, self.FDomainGrid.Block.TargetEndX,
                                            self.FDomainGrid.Block.XOffset, DomainX)
        # tmpmin = limit2 + 1
        # tmpmax = -1
        displace = DomainY + self.FDomainGrid.Block.YOffset
        overlap = 0
        while tix1 <= tix2:
            self.InPlaceIntersect(self.FProbe.Surf.NonEmpty[pix1], self.FTarget.Surf.NonEmpty[tix1], displace)
            for y in range(self.FIntersectHigh + 1):
                for yy in range(self.FIntersect[y][0], self.FIntersect[y][1] + 1):
                    overlap = overlap + basetypes.Min(self.FProbe.Surf.CellCounts[pix1][yy - displace],
                                                      self.FTarget.Surf.CellCounts[tix1][yy])
                    p1, p2 = linegrids.GetLineExtremes(self.FProbe.Surf.Grid[pix1][yy - displace])
                    t1, t2 = linegrids.GetLineExtremes(self.FTarget.Surf.Grid[tix1][yy])
                    p1 = p1 - self.FDomainGrid.Block.ProbeEndZ
                    p2 = p2 - self.FDomainGrid.Block.ProbeEndZ
                    st = t1 - p2
                    en = t2 - p1
                    wid1 = p2 - p1
                    wid2 = t2 - t1
                    if wid1 > wid2:
                        z = wid1
                        wid1 = wid2
                        wid2 = z
                    wid1 = st + wid1
                    wid2 = st + wid2
                    self.FOverlapIxs[st] = self.FOverlapIxs[st] + 1
                    self.FOverlapIxs[wid1 + 1] = self.FOverlapIxs[wid1 + 1] - 1
                    self.FOverlapIxs[wid2 + 1] = self.FOverlapIxs[wid2 + 1] - 1
                    self.FOverlapIxs[en + 1] = self.FOverlapIxs[en + 1] + 1
            tix1 = tix1 + 1
            pix1 = pix1 + 1
        if overlap < self.MinimumOverlap:
            self.FDomainGrid.Shape.Grid[DomainX][DomainY] = []
        else:
            acc = 0
            dif = 0
            z = -1
            while (z < len(self.FOverlapIxs) - 1) and (acc <= self.MinimumOverlap):
                z = z + 1
                dif = dif + self.FOverlapIxs[z]
                acc = acc + dif
            tmpmin = z
            z = len(self.FOverlapIxs)
            dif = 0
            acc = 0
            while (z > 0) and (acc <= self.MinimumOverlap) and (z > tmpmin):
                z = z - 1
                dif = dif + self.FOverlapIxs[z]
                acc = acc + dif
            tmpmax = z
            # Convert from grid to domain
            tmpmin = tmpmin + self.FDomainGrid.Block.ProbeEndZ + self.FDomainGrid.Block.ZOffset
            tmpmax = tmpmax + self.FDomainGrid.Block.ProbeEndZ + self.FDomainGrid.Block.ZOffset
            tmpmin = basetypes.Max(tmpmin, limit1)
            tmpmax = basetypes.Min(tmpmax, limit2)
            if tmpmin <= tmpmax:
                self.FDomainGrid.Shape.Grid[DomainX][DomainY] = linegrids.NewLine(tmpmin, tmpmax)
            else:
                self.FDomainGrid.Shape.Grid[DomainX][DomainY] = []

    # Assumes a domain is already created
    # DomainX, DomainY = Integer
    def RemoveZCores(self, DomainX, DomainY):
        self.CountXYCore = self.CountXYCore + 1
        corefound = False
        pix1, pix2, tix1, tix2 = GetIndexes(self.FDomainGrid.Block.ProbeEndX, self.FDomainGrid.Block.TargetEndX,
                                            self.FDomainGrid.Block.XOffset, DomainX)
        displace = DomainY + self.FDomainGrid.Block.YOffset
        while tix1 <= tix2:
            if (self.FProbe.Core.NonEmpty[pix1] is not None) and (len(self.FProbe.Core.NonEmpty[pix1]) != 0) and \
                    (self.FTarget.Core.NonEmpty[tix1] is not None) and (len(self.FTarget.Core.NonEmpty[tix1]) != 0):
                self.InPlaceIntersect(self.FProbe.Core.NonEmpty[pix1], self.FTarget.Core.NonEmpty[tix1], displace)
                for y in range(self.FIntersectHigh + 1):
                    for yy in range(self.FIntersect[y][0], self.FIntersect[y][1] + 1):
                        corefound = True
                        pline = self.FProbe.Core.Grid[pix1][yy - displace]
                        tline = self.FTarget.Core.Grid[tix1][yy]
                        for pix in range(len(pline)):
                            for tix in range(len(tline)):
                                zmi = tline[tix][0] - (pline[pix][1] + self.FDomainGrid.Block.ZOffset)
                                zma = zmi + pline[pix][1] - pline[pix][0] + tline[tix][1] - tline[tix][0]
                                self.FOverlapIxs[zmi] = self.FOverlapIxs[zmi] - 1
                                self.FOverlapIxs[zma + 1] = self.FOverlapIxs[zma + 1] + 1
            tix1 = tix1 + 1
            pix1 = pix1 + 1
        if corefound:
            acc = 1
            for yy in range(len(self.FOverlapIxs)):
                acc = acc + self.FOverlapIxs[yy]
                self.FOverlapIxs[yy] = acc
            self.FDomainGrid.Shape.Grid[DomainX][DomainY] = \
                linegrids.Intersect(self.FDomainGrid.Shape.Grid[DomainX][DomainY],
                                    linegrids.IntegersToLine(self.FOverlapIxs))
            for yy in range(len(self.FOverlapIxs)):
                self.FOverlapIxs[yy] = 0

    # DomainX, DomainY = Integer
    def ScoreOverlap(self, DomainX, DomainY):
        zline = self.FDomainGrid.Shape.Grid[DomainX][DomainY]
        hizline = len(zline) - 1
        # Reset relevant scores (kept at FScores to avoid creating array at each iteration)
        for z in range(len(zline)):
            for zz in range(zline[z][0], zline[z][1] + 1):
                self.FScores[zz] = 0
        pix1, pix2, tix1, tix2 = GetIndexes(self.FDomainGrid.Block.ProbeEndX, self.FDomainGrid.Block.TargetEndX,
                                            self.FDomainGrid.Block.XOffset, DomainX)
        displace = DomainY + self.FDomainGrid.Block.YOffset
        while tix1 <= tix2:
            self.InPlaceIntersect(self.FProbe.Surf.NonEmpty[pix1], self.FTarget.Surf.NonEmpty[tix1], displace)
            for y in range(self.FIntersectHigh + 1):
                for yy in range(self.FIntersect[y][0], self.FIntersect[y][1] + 1):
                    self.CalcOverlaps(pix1, tix1, displace, yy, hizline, zline)
            tix1 = tix1 + 1
            pix1 = pix1 + 1
        for z in range(len(zline)):
            for zz in range(zline[z][0], zline[z][1] + 1):
                if self.FScores[zz] > self.MinimumOverlap:
                    self.MinimumOverlap = self.FAddModel(self.FScores[zz], DomainX + self.FDomainGrid.Block.XOffset,
                                                         DomainY + self.FDomainGrid.Block.YOffset,
                                                         zz + self.FDomainGrid.Block.ZOffset)

    # pix1, tix1, displace, yy, hizline = Integer, zline = TGridLine
    def CalcOverlaps(self, pix1, tix1, displace, yy, hizline, zline):
        pline = self.FProbe.Surf.Grid[pix1][yy - displace]
        tline = self.FTarget.Surf.Grid[tix1][yy]
        hi = len(self.FScores) - 1
        for pix in range(len(pline)):
            pz1 = pline[pix][0] + self.FDomainGrid.Block.ZOffset
            pz2 = pline[pix][1] + self.FDomainGrid.Block.ZOffset
            for tix in range(len(tline)):
                tz1 = tline[tix][0]
                tz2 = tline[tix][1]
                st = tz1 - pz2
                en = tz2 - pz1
                wid1 = -1
                wid2 = -1
                for z in range(hizline + 1):
                    if en < zline[z][0]:
                        break
                    elif (st <= zline[z][1]) and (en >= zline[z][0]):
                        if wid1 < 0:
                            wid1 = pz2 - pz1
                            wid2 = tz2 - tz1
                            if wid1 > wid2:
                                ad = wid1
                                wid1 = wid2
                                wid2 = ad
                            wid1 = st + wid1
                            wid2 = st + wid2
                        for zz in range(zline[z][0], zline[z][1] + 1):
                            if zz >= st:
                                if zz <= wid1:
                                    self.FScores[zz] = self.FScores[zz] + (zz - st + 1)
                                elif zz <= wid2:
                                    self.FScores[zz] = self.FScores[zz] + (wid1 - st + 1)
                                elif zz <= en:
                                    self.FScores[zz] = self.FScores[zz] + (en - st + 1)
                                else:
                                    break

    def TranslateToTarget(self):
        c = basetypes.TCoord((self.FDomainGrid.Block.XOffset + self.FDomainGrid.Block.ProbeEndX / 2) *
                             self.FProbe.FResolution,
                             (self.FDomainGrid.Block.YOffset + self.FDomainGrid.Block.ProbeEndY / 2) *
                             self.FProbe.FResolution,
                             (self.FDomainGrid.Block.ZOffset + self.FDomainGrid.Block.ProbeEndZ / 2) *
                             self.FProbe.FResolution)
        return geomutils.Subtract(c, self.FTarget.TransVec)

    # AddModel = TAddModel
    def AssignModelManager(self, AddModel):
        self.FAddModel = AddModel

    def BuildInitialModel(self):
        self.FDomainGrid.Block = self.FConstraintManager.FDomainBlock
        self.FDomainGrid.Shape.Grid = []
        self.FDomainGrid.Shape.NonEmpty = []
        self.SetDomainYLines()
        self.SetDomainZLines()

    def CalcDomainStats(self):
        linegrids.ComputeShapeStats(self.FDomainGrid.Shape)

    def Score(self):
        if self.FAddModel is not None:
            # Large buffer for intersections
            for f in range(len(self.FIntersect) - 1, 100):
                self.FIntersect.append(0)
            for f in range(len(self.FIntersect), len(self.FIntersect) - 1, -1):
                del self.FIntersect[f]
            self.FIntersectHigh = -1
            self.FScores = basetypes.FilledInts(self.FDomainGrid.Block.DomainEndZ + 1, 0)
            for x in range(len(self.FDomainGrid.Shape.NonEmpty)):
                for y in range(len(self.FDomainGrid.Shape.NonEmpty[x])):
                    for yy in range(self.FDomainGrid.Shape.NonEmpty[x][y][0], self.FDomainGrid.Shape.NonEmpty[x][y][1] +
                                                                              1):
                        if self.FDomainGrid.Shape.Grid[x][yy] is not None and \
                                len(self.FDomainGrid.Shape.Grid[x][yy]) != 0:
                            self.ScoreOverlap(x, yy)


# ProbeIx1, ProbeIx2, TargetIx1, TargetIx2, ProbeOffset, DomainEnd = const Integer
def OverlapRegion(ProbeIx1, ProbeIx2, TargetIx1, TargetIx2, ProbeOffset, DomainEnd):
    DomainIx1 = TargetIx1 - (ProbeIx2 + ProbeOffset)
    DomainIx2 = DomainIx1 + (ProbeIx2 - ProbeIx1) + (TargetIx2 - TargetIx1)
    if DomainIx1 < 0:
        DomainIx1 = 0
    if DomainIx2 > DomainEnd:
        DomainIx2 = DomainEnd
    # In Python return Integer, Integer
    return DomainIx1, DomainIx2


# Gets first and last index of target and probe grids for this domain coord
# ProbeEnd, TargetEnd, ProbeOffset, Coord = const Integer
def GetIndexes(ProbeEnd, TargetEnd, ProbeOffset, Coord):
    ProbeIx1 = -ProbeOffset - Coord
    ProbeIx2 = ProbeEnd
    TargetIx1 = 0
    TargetIx2 = TargetIx1 + ProbeIx2 - ProbeIx1
    if ProbeIx1 < 0:
        TargetIx1 = -ProbeIx1
        ProbeIx1 = 0
    if TargetIx2 > TargetEnd:
        ProbeIx2 = ProbeIx2 - TargetIx2 + TargetEnd
        TargetIx2 = TargetEnd
    # In Python return Integer, Integer, Integer, Integer
    return ProbeIx1, ProbeIx2, TargetIx1, TargetIx2
