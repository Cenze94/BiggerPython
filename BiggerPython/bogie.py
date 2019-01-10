import linegrids
import geomutils
import dockdomains
import docktasks
import basetypes


class TModelManager:
    # FModels = TDockModels, FLastModel, FMinOverlap = Integer, CurrentRotation = TQuaternion, GridScale = TFloat,
    # ProbeGridTransVec = TCoord
    # NumModels, MinOverlap = Integer, DockModels = TDockModels
    def __init__(self, NumModels, MinOverlap, DockModels):
        self.FModels = []
        for f in range(NumModels):
            if f < len(DockModels):
                self.FModels.append(DockModels[f])
            else:
                self.FModels.append(docktasks.TDockModel(basetypes.TCoord(), geomutils.IdentityQuaternion, -1))
        self.FLastModel = len(DockModels) - 1
        if self.FLastModel >= 0:
            self.FMinOverlap = basetypes.Min(self.FModels[self.FLastModel].OverlapScore, MinOverlap)
        else:
            self.FMinOverlap = MinOverlap
        self.CurrentRotation = geomutils.TQuaternion()
        self.GridScale = 0.0
        self.ProbeGridTransVec = basetypes.TCoord()

    # Score, X, Y, Z = Integer
    def AddModel(self, Score, X, Y, Z):
        if Score > self.FMinOverlap:
            ix1 = 0
            while self.FModels[ix1].OverlapScore >= Score:
                ix1 = ix1 + 1
                # This must stop if Score>FMinOverlap
            if self.FLastModel < len(self.FModels) - 1:
                self.FLastModel = self.FLastModel + 1
            ix2 = self.FLastModel
            while ix2 > ix1:
                self.FModels[ix2].OverlapScore = self.FModels[ix2 - 1].OverlapScore
                self.FModels[ix2].TransVec = self.FModels[ix2 - 1].TransVec
                self.FModels[ix2].Rotation = self.FModels[ix2 - 1].Rotation
                ix2 = ix2 - 1
            self.FModels[ix1].OverlapScore = Score
            self.FModels[ix1].TransVec = geomutils.Add(self.ProbeGridTransVec, basetypes.TCoord(X * self.GridScale,
                                                                                                Y * self.GridScale,
                                                                                                Z * self.GridScale))
            self.FModels[ix1].Rotation = self.CurrentRotation
            if self.FLastModel == len(self.FModels) - 1:
                self.FMinOverlap = self.FModels[self.FLastModel].OverlapScore
            # Return Integer
            return self.FMinOverlap

    # CSet = const TConstraintSet
    def ImportModels(self, CSet):
        for f in range(len(CSet.DockModels)):
            if f > len(self.FModels) - 1:
                break
            self.FModels[f] = CSet.DockModels[f]
        self.FMinOverlap = basetypes.Max(self.FMinOverlap, self.FModels[len(self.FModels) - 1].OverlapScore)

    # CSet = TConstraintSet
    def ExportModels(self, CSet):
        CSet.DockModels = []
        for f in range(len(self.FModels)):
            CSet.DockModels.append(self.FModels[f])

class TDockManager:
    # FTargetCoords = TCoords, FTargetRads = TFloats, FProbeCoords = TCoords, FProbeRads = TFloats,
    # FResolution = TFloat, FTargetGrid, FProbeGrid = TDockingGrid, FModelManagers = TModelManagers,
    # FRotations = TQuaternions, FAxes = TCoords, TCurrentRotation = Integer, FConstraintSets = TConstraintSets,
    # DigitizationTicks, ConstraintTicks, DomainTicks, ScoringTicks = Integer
    # TargCoords = TCoords, TargRads = TFloats, ProbCoords = TCoords, ProbRads = TFloats, AResolution = TFloat
    def __init__(self, TargCoords, TargRads, ProbCoords, ProbRads, AResolution):
        assert (len(TargCoords) == len(TargRads)) and (len(ProbCoords) == len(ProbRads)), \
            'Mismatching coordinate and radius arrays'
        # These are copies so they cannot be changed outside
        self.FTargetCoords = []
        self.FTargetRads = []
        for f in range(len(TargCoords)):
            self.FTargetCoords.append(TargCoords[f])
            self.FTargetRads.append(TargRads[f])
        self.FProbeCoords = []
        self.FProbeRads = []
        for f in range(len(ProbCoords)):
            self.FProbeCoords.append(ProbCoords[f])
            self.FProbeRads.append(ProbRads[f])
        self.FResolution = AResolution
        self.FTargetGrid = linegrids.TDockingGrid(self.FResolution)
        self.FProbeGrid = linegrids.TDockingGrid(self.FResolution)
        self.FModelManagers = []
        # Add coord and radius for soft docking
        self.FRotations = []
        self.FAxes = []
        self.FCurrentRotation = -1
        self.FConstraintSets = []
        # Statistics
        self.DigitizationTicks = 0
        self.ConstraintTicks = 0
        self.DomainTicks = 0
        self.ScoringTicks = 0

    def ClearModelManagers(self):
        self.FModelManagers = []

    def TotalRotations(self):
        # Return Integer
        return len(self.FRotations)

    # DockIx = Integer
    def Models(self, DockIx):
        # Return TDockModels
        return self.FModelManagers[DockIx].Models

    def ModelGroupsCount(self):
        # Return Integer
        return len(self.FModelManagers)

    def __del__(self):
        self.FTargetGrid = []
        self.FProbeGrid = []
        self.ClearModelManagers()

    # CSets = TConstraintSets
    def ImportConstraintSets(self, CSets):
        self.FConstraintSets = CSets
        self.ClearModelManagers()
        self.FModelManagers = []
        for f in range(len(self.FConstraintSets)):
            self.FModelManagers.append(TModelManager(self.FConstraintSets[f].NumModels,
                                                     self.FConstraintSets[f].MinOverlap,
                                                     self.FConstraintSets[f].DockModels))
            self.FModelManagers[f].GridScale = self.FResolution
            self.FModelManagers[f].ImportModels(CSets[f])

    # CSets = TConstraintSets
    def ExportResults(self, CSets):
        for f in range(len(self.FModelManagers)):
            self.FModelManagers[f].ExportModels(CSets[f])

    def BuildTargetGrid(self):
        self.FTargetGrid.BuildFromSpheres(self.FTargetCoords, self.FTargetRads)

    # Rotation = TQuaternion
    def BuildProbeGrid(self, Rotation):
        tmpcoords = geomutils.Rotate(self.FProbeCoords, Rotation)
        self.FProbeGrid.BuildFromSpheres(tmpcoords, self.FProbeRads)

    # Job = TDockRun
    def BuildRotations(self, Job):
        self.FRotations = Job.FRotations
        self.FAxes = []
        for f in range(len(Job.FAxisIndexes)):
            self.FAxes.append(Job.FAxes[Job.FAxisIndexes[f]])

    # Returns true if finished
    # NumSteps = Integer, CalcStats = Boolean
    def Dock(self, NumSteps = 1, CalcStats = False):
        while (self.FCurrentRotation < len(self.FRotations) - 1) and (NumSteps > 0):
            self.FCurrentRotation = self.FCurrentRotation + 1
            NumSteps = NumSteps - 1
            ticktime = 0
            if CalcStats:
                ticktime = basetypes.GetTickCount()
            self.BuildProbeGrid(self.FRotations[self.FCurrentRotation])
            if CalcStats:
                self.DigitizationTicks = basetypes.GetTimeInterval(ticktime)
                self.ConstraintTicks = 0
                self.DomainTicks = 0
                self.ScoringTicks = 0
            for f in range(len(self.FConstraintSets)):
                self.FModelManagers[f].FCurrentRotation = self.FRotations[self.FCurrentRotation]
                dockdomain = dockdomains.TDockDomain(self.FTargetGrid, self.FProbeGrid)
                dockdomain.FConstraintManager.ImportConstraints(self.FConstraintSets[f].Constraints,
                                                                self.FRotations[self.FCurrentRotation],
                                                                self.FAxes[self.FCurrentRotation])
                if CalcStats:
                    self.ConstraintTicks = self.ConstraintTicks + basetypes.GetTimeInterval(ticktime)
                dockdomain.AssignModelManager(self.FModelManagers[f].FAddModel)
                dockdomain.MinimumOverlap = self.FModelManagers[f].MinimumOverlap
                dockdomain.RemoveCores = True
                dockdomain.BuildInitialModel()
                if CalcStats:
                    self.DomainTicks = self.DomainTicks + basetypes.GetTimeInterval(ticktime)
                self.FModelManagers[f].ProbeGridTransVec = geomutils.Subtract(self.FProbeGrid.FTransVec,
                                                                              self.FTargetGrid.FTransVec)
                dockdomain.Score()
                if CalcStats:
                    self.ScoringTicks = self.ScoringTicks + basetypes.GetTimeInterval(ticktime)
        # Return Boolean
        return self.FCurrentRotation == len(self.FRotations) - 1
