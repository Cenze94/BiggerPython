import geomutils
import stringutils
import basetypes
import molfit
import quicksort
import molecules
import pdbmolecules
import molutils
import rotations
import oclconfiguration
from enum import Enum
import os.path
import xml.etree.cElementTree as ET
import random


# Current orders version
OrdersVersion = 1
# Constraint types
CNull = -1           # For errors in reading
CUnconstrained = 0
CLinearDistance = 1
CEuclideanDistance = 2
CContactCount = 3
CNormalPlane = 4     # Plane normal to the rotation axis
# Score types
ScoreNone = 0
ScoreRMSD = 1
ScoreContacts = 1


class TRMSDOptions(Enum):
    rmsdScoreTarget = 0
    rmsdScoreProbe = 1
    rmsdFitTarget = 2
    rmsdFitProbe = 3


class TDockModel:
    # TransVec = TCoord, Rotation = TQuaternion, OverlapScore = Integer
    def __init__(self, TransVec = basetypes.TCoord(), Rotation = geomutils.TQuaternion(), OverlapScore = 0):
        self.TransVec = TransVec
        self.Rotation = Rotation
        self.OverlapScore = OverlapScore


class TScoreResult:
    # ScoreID = string, ScoreVals = TFloats, ComputedCount = Integer
    def __init__(self, ScoreID = '', ScoreVals = None, ComputedCount = 0):
        self.ScoreID = ScoreID
        if ScoreVals is None:
            ScoreVals = []
        # Same index as corresponding models, the id for each is the index (0, ... )
        self.ScoreVals = ScoreVals
        self.ComputedCount = ComputedCount


class TConstraintDef:
    # Name = string, ProbePoints, TargetPoints = TCoords, Distance = TFloat, ConstraintType = Integer
    def __init__(self, Name = '', ProbePoints = None, TargetPoints = None, Distance = 0.0, ConstraintType=CContactCount):
        self.Name = Name
        if ProbePoints is None:
            ProbePoints = []
        self.ProbePoints = ProbePoints
        if TargetPoints is None:
            TargetPoints = []
        self.TargetPoints = TargetPoints
        self.Distance = Distance
        self.ConstraintType = ConstraintType
        # MinContacts, MaxContacts = Integer
        if ConstraintType == CContactCount:
            self.MinContacts = 0
            self.MaxContacts = 0


class TConstraintSet:
    # Name = string, Constraints = TConstraintsDefs, NumModels, MinOverlap = Integer, DockModels = TDockModels,
    # ScoreResults = TScoreResults
    def __init__(self, Name = '', Constraints = None, NumModels = 0, MinOverlap = 0, DockModels = None,
                 ScoreResults = None):
        self.Name = Name
        if Constraints is None:
            Constraints = []
        # Constraints may be empty for unconstrained docking
        self.Constraints = Constraints
        self.NumModels = NumModels
        self.MinOverlap = MinOverlap
        if DockModels is None:
            DockModels = []
        self.DockModels = DockModels
        if ScoreResults is None:
            ScoreResults = []
        self.ScoreResults = ScoreResults

# These constraints are added in series to the same constraint manager for conjunction of constraints

# Score definitions


class TScoreDef:
    # AName = string
    def __init__(self, AName):
        self.FName = AName

    def ScoreType(self):
        # Return Integer
        return ScoreNone


class TRMSDScoreDef(TScoreDef):
    def __init__(self, AName):
        # AName = string
        # FPredictedCoords, FActualCoords = TCoords, FFirstProbeCoord = Integer, FitTarget, FitProbe, ScoreTarget,
        # ScoreProbe = Boolean
        TScoreDef.__init__(self, AName)

        self.FPredictedCoords = []
        self.FActualCoords = []
        self.FFirstProbeCoord = 0
        self.FitTarget = True
        self.FitProbe = True
        self.ScoreTarget = True
        self.ScoreProbe = True

    # Preds, Actuals = TCoords, FirstProbe = Integer
    def SetCoords(self, Preds, Actuals, FirstProbe):
        for f in range(len(Preds)):
            self.FPredictedCoords.append(Preds[f])
        for f in range(len(Actuals)):
            self.FActualCoords.append(Actuals[f])
        self.FFirstProbeCoord = FirstProbe

    def ScoreType(self):
        # Return Integer
        return ScoreRMSD


class TContactScoreDef(TScoreDef):
    # AName = string
    # FTargetGroups, FProbeGroups = TCoordGroups, FTargetIndexes, FProbeIndexes = TIntegers, FContactScores = TFloats
    def __init__(self, AName):
        TScoreDef.__init__(self, AName)

        self.FTargetGroups = []
        self.FProbeGroups = []
        self.FTargetIndexes = []
        self.FProbeIndexes = []
        self.FContactScores = []

    # Group = TCoords
    def AddTargetGroup(self, Group):
        self.FTargetGroups.append(Group)

    # Group = TCoords
    def AddProbeGroup(self, Group):
        self.FProbeGroups.append(Group)

    # TargetIx, ProbeIx = Integer, Score = TFloat
    def AddContact(self, TargetIx, ProbeIx, Score):
        self.FTargetIndexes.append(TargetIx)
        self.FProbeIndexes.append(ProbeIx)
        self.FContactScores.append(Score)


class TDockStats:
    # DigitizationTickCount, ConstraintsTickCount, DomainTickCount, ScoringTickCount, TotalTickCount,
    # TestedModelsCount, InsertedModelsCount = Integer
    def __init__(self, DigitizationTickCount = 0, ConstraintsTickCount = 0, DomainTickCount = 0, ScoringTickCount = 0,
                 TotalTickCount = 0, TestedModelsCount = 0, InsertedModelsCount = 0):
        self.DigitizationTickCount = DigitizationTickCount
        self.ConstraintsTickCount = ConstraintsTickCount
        self.DomainTickCount = DomainTickCount
        self.ScoringTickCount = ScoringTickCount
        self.TotalTickCount = TotalTickCount
        self.TestedModelsCount = TestedModelsCount
        self.InsertedModelsCount = InsertedModelsCount


class TShapeInfo:
    # Coords = TCoords, TRads = TFloats
    def __init__(self, Coords = None, Rads = None):
        if Coords is None:
            Coords = []
        self.Coords = Coords
        if Rads is None:
            Rads = []
        self.Rads = Rads


class TDockRun:
    # TargetFile, ProbeFile = string, Target, Probe = TShapeInfo, TargetRads, ProbeRads = TFloats, Resolution,
    # AddedRadius = TFloat, Axes = TCoords, Rotations = TQuaternions, AxisIndexes = TIntegers, SecondBetweenSaves,
    # CompletedRotations = Integer, ConstraintSets = TConstraintSets, ScoreDefs = TScoreDefs, Stats = TDockStats
    def __init__(self, TargetFile = '', ProbeFile = '', Target = TShapeInfo(), Probe = TShapeInfo(), TargetRads = None,
                 ProbeRads = None, Resolution = 0, AddedRadius = 0, Axes = None, Rotations = None, AxisIndexes = None,
                 SecondsBetweenSaves = 0, CompletedRotations = 0, ConstraintSets = None, ScoreDefs = None,
                 Stats = None):
        self.TargetFile = TargetFile
        self.ProbeFile = ProbeFile
        self.Target = Target
        self.Probe = Probe
        if TargetRads is None:
            TargetRads = []
        self.TargetRads = TargetRads
        if ProbeRads is None:
            ProbeRads = []
        self.ProbeRads = ProbeRads
        self.Resolution = Resolution
        self.AddedRadius = AddedRadius
        if Axes is None:
            Axes = []
        self.Axes = Axes
        if Rotations is None:
            Rotations = []
        self.Rotations = Rotations
        if AxisIndexes is None:
            AxisIndexes = []
        # Axis rotations belong to
        self.AxisIndexes = AxisIndexes
        self.SecondsBetweenSaves = SecondsBetweenSaves
        self.CompletedRotations = CompletedRotations
        if ConstraintSets is None:
            ConstraintSets = []
        self.ConstraintSets = ConstraintSets
        if ScoreDefs is None:
            ScoreDefs = []
        self.ScoreDefs = ScoreDefs
        if Stats is None:
            Stats = []
        self.Stats = Stats


class TDockOrdersManager:
    # AJobFile = string
    # FTarget, FProbe = FMolecule, FModelMan = TPDBModelMan, FDockRuns = TDockRuns, FCurrentDock = Integer
    def __init__(self, AJobFile):
        self.FJobFile = AJobFile

        self.FTarget = molecules.TMolecule('', 0, None)
        self.FProbe = molecules.TMolecule('', 0, None)
        self.FDockRuns = []
        self.FCurrentDock = 0
        self.FModelMan = pdbmolecules.TPDBModelMan(oclconfiguration.Config.MonomersPath)

    # LayerName = string
    def GetLayer(self, LayerName):
        Result = self.FModelMan.LayerByFileName(LayerName)
        if Result is None:
            self.FModelMan.LoadLayer(LayerName)
            Result = self.FModelMan.LayerByFileName(LayerName)
        # Return TPDBModel
        return Result

    # Uses MagicFit to find the matches and returns the alpha carbon coordinates
    # Template = TMolecule, SubMat = TSubMatrix, MinSequenceMatch = TFloat
    def MagicFitToTemplate(self, Template, SubMat, MinSequenceMatch):
        TargetFit = molfit.MagicFitAlphaCoords(self.FTarget, Template, SubMat, None, MinSequenceMatch)
        exclusions = []
        # Prevent repeated chain assignments between target and probe
        for f in range(len(TargetFit.FitResult.Map)):
            if TargetFit.FitResult.Map[f] >= 0:
                exclusions.append(TargetFit.FitResult.Map[f])
        ProbeFit = molfit.MagicFitAlphaCoords(self.FProbe, Template, SubMat, exclusions, MinSequenceMatch)
        # In Python return TFitCoordsResult, TFitTCoordsResult
        return TargetFit, ProbeFit

    def __del__(self):
        FreeOrders(self.FDockRuns)

    def NewDockRun(self):
        run = TDockRun()
        run.Resolution = 1
        run.AddedRadius = 1.35
        run.SecondsBetweenSaves = 1000
        run.CompletedRotations = 0
        ZeroStats(run.Stats)
        self.FDockRuns.append(run)
        self.FCurrentDock = len(self.FDockRuns) - 1

    # Prepares the docking partners for that job
    # First method: TargetFile, ProbeFile, TargetChains, ProbeChains = string, Center, RandomizeProbe = Boolean
    # Second method: TargetFile = JobIndex = Integer
    def PreparePartners(self, TargetFile = 0, ProbeFile = None, TargetChains = None, ProbeChains = None, Center = None,
                        RandomizeProbe = None):
        if ProbeFile is None:
            # Second method
            JobIndex = TargetFile
            self.FCurrentDock = JobIndex
            self.PreparePartners(self.FDockRuns[self.FCurrentDock].TargetFile, self.FDockRuns[self.FCurrentDock].ProbeFile,
                            '', '', False, False)
        else:
            # First method
            tlayer = self.GetLayer(TargetFile)
            player = self.GetLayer(ProbeFile)
            # If no chains selected, then whole protein
            if TargetChains is '':
                tchains = tlayer.ListChains()
            else:
                tchains = stringutils.SplitChars(TargetChains)
            if ProbeChains is '':
                pchains = player.ListChains()
            else:
                pchains = stringutils.SplitChars(ProbeChains)
            self.FTarget = tlayer.CopyChains(tchains)
            self.FProbe = player.CopyChains(pchains)
            if Center:
                molutils.CenterMolecule(self.FTarget)
                molutils.CenterMolecule(self.FProbe)
            if RandomizeProbe:
                quat = geomutils.TQuaternion(random.random() - 0.5, random.random() - 0.5, random.random() - 0.5,
                                             random.random() - 0.5)
                geomutils.Normalize(quat)
                self.FProbe.Transform(quat)

    # Copies target and probe coordinates and radii, adding addedradius
    def BuildShapes(self):
        self.FDockRuns[self.FCurrentDock].Target.Coords = molutils.ListCoords(self.FTarget)
        self.FDockRuns[self.FCurrentDock].Probe.Coords = molutils.ListCoords(self.FProbe)
        self.FDockRuns[self.FCurrentDock].Target.Rads = geomutils.Add(molutils.ListRadii(self.FTarget),
                                                                      self.FDockRuns[self.FCurrentDock].AddedRadius)
        self.FDockRuns[self.FCurrentDock].Probe.Rads = geomutils.Add(molutils.ListRadii(self.FProbe),
                                                                     self.FDockRuns[self.FCurrentDock].AddedRadius)

    # TargetFile, ProbeFile = string
    def SetPDBFiles(self, TargetFile, ProbeFile):
        self.FDockRuns[self.FCurrentDock].TargetFile = TargetFile
        self.FDockRuns[self.FCurrentDock].ProbeFile = ProbeFile

    # Generates Axes, Rotations and AxisIndexes
    # ShapePoints, BaseCount = Integer, Displacement = TFloat, Simmetry, MaxAxes = Integer
    def BuildRotations(self, ShapePoints, BaseCount, Displacement, Symmetry, MaxAxes):
        coords = self.FProbe.AllCoords()
        selcoords = rotations.SpacedCoords(coords, 40)
        rotman = rotations.TRotationManager(selcoords)
        rotman.MaxDistQuaternions(BaseCount, Displacement, Symmetry, MaxAxes)
        self.FDockRuns[self.FCurrentDock].Rotations = rotman.Rotations
        self.FDockRuns[self.FCurrentDock].Axes = rotman.SelectedAxes
        self.FDockRuns[self.FCurrentDock].AxisIndexes = rotman.AxisIndexes

    # Text file containing one contact per line; each line has target chain space target res ID space probe chain
    # space probe res ID space distance; each line is an independent distance constraint between the atoms in those
    # residues
    # ContactFile = string, MaxContacts = Integer
    def AddResidueContacts(self, ContactFile, MaxContacts = 0):
        sl = []
        with open(ContactFile) as f:
            sl = f.read().splitlines()
        if MaxContacts == 0:
            MaxContacts = len(sl)
        else:
            MaxContacts = basetypes.Min(MaxContacts, len(sl))
        for f in range(MaxContacts):
            contact = stringutils.SplitString(sl[f], ' ')
            cset = TConstraintSet()
            cset.NumModels = int(contact[5])
            cset.MinOverlap = int(contact[6])
            cset.Name = sl[f]
            cdef = TConstraintDef()
            cdef.Name = 'Euclidean ' + contact[0] + contact[1] + ':' + contact[2] + contact[3]
            cdef.TargetPoints = molutils.ListCoords(self.FTarget.GetGroup(contact[0]).GetGroupById(int(contact[1])))
            cdef.ProbePoints = molutils.ListCoords(self.FProbe.GetGroup(contact[2]).GetGroupById(int(contact[3])))
            cdef.Distance = float(contact[4])
            cdef.ConstraintType = CEuclideanDistance
            cset.Constraints.append(cdef)
            self.FDockRuns[self.FCurrentDock].ConstraintSets.append(cset)


    # Adds a normal plane constraint, currently working only at 0,0,0
    # Margin = TFloat
    def AddSimmetryConstraint(self, Margin):
        cset = TConstraintSet()
        cset.NumModels = 5000
        cset.MinOverlap = 200
        cset.Name = 'Symmetry Constraints'
        cdef = TConstraintDef()
        cdef.Name = 'Symmetry Constraint'
        cdef.TargetPoints = []
        cdef.ProbePoints = []
        cdef.Distance = Margin
        cdef.ConstraintType = CNormalPlane
        cset.Constraints.append(cdef)
        self.FDockRuns[self.FCurrentDock].ConstraintSets.append(cset)

    # TemplateFile = string, SubMat = TSubMatrix, MinSequenceMatch = TFloat, Options = TRMSDOptions, Tag = string
    def AddRMSDScore(self, TemplateFile, SubMat, MinSequenceMatch, Options, Tag):
        layer = self.GetLayer(TemplateFile)
        targetfit, probefit = self.MagicFitToTemplate(layer.Molecule, SubMat, MinSequenceMatch)
        realcoords = basetypes.Concatenate(targetfit.TargetCoords, probefit.TargetCoords)
        predcoords = basetypes.Concatenate(targetfit.ProbeCoords, probefit.ProbeCoords)
        firstprobe = len(targetfit.ProbeCoords)
        rmsd = TRMSDScoreDef(Tag + ' to ' + os.path.basename(TemplateFile))
        rmsd.SetCoords(predcoords, realcoords, firstprobe)
        rmsd.FitTarget = Options.rmsdFitTarget
        rmsd.FitProbe = Options.rmsdFitProbe
        rmsd.ScoreTarget = Options.rmsdScoreTarget
        rmsd.ScoreProbe = Options.rmsdScoreProbe
        self.FDockRuns[self.FCurrentDock].ScoreDefs.append(rmsd)

    # TemplateFile = atring, Dist = TFloat, SubMat = TSubMatrix, MinSequenceMatch = TFloat
    def AddInterfaceRMSDSCore(self, TemplateFile, Dist, SubMat, MinSequenceMatch):
        layer = self.GetLayer(TemplateFile)
        targetfit, probefit = self.MagicFitToTemplate(layer.Molecule, SubMat, MinSequenceMatch)
        targpred, targtemp = molfit.GetMatchingResidues(self.FTarget, layer.Molecule, targetfit.FitResult)
        probepred, probetemp = molfit.GetMatchingResidues(self.FProbe, layer.Molecule, probefit.FitResult)
        targixs, probeixs = molutils.GroupsInContact(targtemp, probetemp, Dist)
        realcoords = []
        predcoords = []
        ix = 0
        for f in range(len(targixs)):
            ix = self.PushCoords(targtemp[targixs[f]].GetAtom('CA'), targpred[targixs[f]].GetAtom('CA'),
                                 realcoords, predcoords, ix)
        firstprobe = ix
        for f in range(len(probeixs)):
            ix = self.PushCoords(probetemp[probeixs[f]].GetAtom('CA'), probepred[probeixs[f]].GetAtom('CA'),
                                 realcoords, predcoords, ix)
        rmsd = TRMSDScoreDef('Interface RMSD to ' + os.path.basename(TemplateFile) + ' at ' + str(round(Dist, 3)) +
                             'A')
        rmsd.SetCoords(predcoords, realcoords, firstprobe)
        rmsd.FitTarget = True
        rmsd.FitProbe = True
        rmsd.ScoreTarget = True
        rmsd.ScoreProbe = True
        self.FDockRuns[self.FCurrentDock].ScoreDefs.append(rmsd)

    # atomr, atomp = TAtom, realcoords, predcoords = TCoords, ix = Integer
    def PushCoords(self, atomr, atomp, realcoords, predcoords, ix):
        if (atomr is not None) and (atomp is not None):
            realcoords.append(atomr.Coords)
            predcoords.append(atomp.Coords)
            return ix + 1

    # For unconstrained docking
    # NumModels, MinOverlap = Integer
    def AddNullConstraintsSet(self, NumModels, MinOverlap):
        cset = TConstraintSet()
        cset.NumModels = NumModels
        cset.MinOverlap = MinOverlap
        cset.Name = 'Unconstrained'
        self.FDockRuns[self.FCurrentDock].ConstraintSets.append(cset)

    def LoadJobFile(self):
        self.FDockRuns = LoadOrders(self.FJobFile)

    # Append = Boolean
    def SaveOrders(self, Append):
        SaveOrders(self.FDockRuns, self.FJobFile, Append)

    # Id = string
    def DeleteScores(self, Id = ''):
        for f in range(len(self.FDockRuns)):
            self.FDockRuns[f].ScoreDefs = []
            for g in range(len(self.FDockRuns[f].ConstraintSets)):
                self.FDockRuns[f].ConstraintSets[g].ScoreResults = []


# Orders = const TDockRuns, FileName = string, Append = Boolean
def SaveOrders(Orders, FileName, Append = False):
    if Append and os.path.exists(FileName):
        doc = ET.parse(FileName)
        root = doc.getroot()
    else:
        root = ET.Element("Orders")
        doc = ET.ElementTree(root)
    root.set('Version', str(OrdersVersion))
    for g in range(len(Orders)):
        DockRun = Orders[g]
        run = ET.Element('DockRun')
        root.append(run)
        AddDockStats(run, DockRun.Stats)
        AddDockParameters(run, DockRun)
        AddShapeInfo(run, 'Target', DockRun.Target)
        AddShapeInfo(run, 'Probe', DockRun.Probe)
        AddRotations(run, DockRun)
        for f in range(len(DockRun.ConstraintSets)):
            AddConstraintSet(run, DockRun.ConstraintSets[f])
        for f in range(len(DockRun.ScoreDefs)):
            if DockRun.ScoreDefs[f].ScoreType == ScoreRMSD:
                AddRMSDScore(run, TRMSDScoreDef(DockRun.ScoreDefs[f]))
    doc.write(FileName)


# Name, Val = string, Parent = TDOMNode
def AddTextNode(Name, Val, Parent):
    tmp = ET.Element(Name)
    tmp.text = Val
    Parent.append(tmp)


# Parent = TDOMNode (, DockRun = TDockRun in Python)
def AddDockParameters(Parent, DockRun):
    Result = ET.Element('DockParameters')
    AddTextNode('TargetFile', DockRun.TargetFile, Result)
    AddTextNode('ProbeFile', DockRun.ProbeFile, Result)
    AddTextNode('Resolution', DockRun.Resolution, Result)
    AddTextNode('AddedRadius', DockRun.AddedRadius, Result)
    AddTextNode('SecondsBetweenSaves', DockRun.SecondsBetweenSaves, Result)
    AddTextNode('CompletedRotations', DockRun.CompletedRotations, Result)
    AddTextNode('TotalRotations', DockRun.TotalRotations, Result)
    Parent.append(Result)
    # Return TDOMNode
    return Result


# Points = TCoords, PointSetName = string
def CreatePointSet(Points, PointSetName):
    Result = ET.Element(PointSetName)
    for f in range(len(Points)):
        point = ET.Element('Point')
        point.set('X', str(round(Points[f][0], 3)))
        point.set('Y', str(round(Points[f][1], 3)))
        point.set('Z', str(round(Points[f][2], 3)))
        Result.append(point)
    # Return TDOMNode
    return Result

'''
THIS FUNCTION IS NOT USED
# Parent = TDOMNode, Score = TContactScoreDef
def AddContactScore(Parent, Score):
    tmp = ET.Element('ContactScore')
    AddTextNode('ScoreID', Score.FName, tmp)
    for f in range(len(Score.FTargetGroups)):
        tmp.append(CreatePointSet(Score.FTargetGroups[f], 'TargetGroup'))
    for f in range(len(Score.FProbeGroups)):
        tmp.append(CreatePointSet(Score.FProbeGroups[f], 'ProbeGroup'))
    for f in range(len(Score.FTargetIndexes)):
        contact = ET.Element('Contact')
        contact.set('TargetIx', str(Score.FTargetIndexes[f]))
        contact.set('ProbeIx', str(Score.FProbeIndexes[f]))
        contact.set('ScoreVal', str(round(Score.FContactScores[f], 3)))
        tmp.append(contact)
    Parent.append(tmp)
'''


# Parent = TDOMNode, Score = TRMSDScoreDef
def AddRMSDScore(Parent, Score):
    tmp = ET.Element('RMSDScore')
    AddTextNode('ScoreID', Score.FName, tmp)
    tmp.append(CreatePointSet(Score.FPredictedCoords, 'Predicted'))
    tmp.append(CreatePointSet(Score.FActualCoords, 'Template'))
    tmp.set('ScoreTarget', str(Score.ScoreTarget))
    tmp.set('ScoreProbe', str(Score.ScoreProbe))
    tmp.set('FitTarget', str(Score.FitTarget))
    tmp.set('FitProbe', str(Score.FitProbe))
    AddTextNode('FirstProbePoint', str(Score.FFirstProbeCoord), tmp)
    Parent.append(tmp)


# Parent = TDOMNode, CoordName = string, Coor = TCoord
def AddCoordinate(Parent, CoordName, Coor):
    tmp = ET.Element(CoordName)
    tmp.set('X', str(round(Coor[0], 3)))
    tmp.set('Y', str(round(Coor[1], 3)))
    tmp.set('Z', str(round(Coor[2], 3)))
    Parent.append(tmp)


# Parent = TDOMNode, QuatName = string, Quat = TQuaternion, Index = Integer
def AddQuaternion(Parent, QuatName, Quat, Index = -1):
    tmp = ET.Element(QuatName)
    tmp.set('r', str(round(Quat[0], 3)))
    tmp.set('i', str(round(Quat[1], 3)))
    tmp.set('j', str(round(Quat[2], 3)))
    tmp.set('k', str(round(Quat[3], 3)))
    if Index >= 0:
        tmp.set('ix', str(Index))
    Parent.append(tmp)


# Parent = TDOMNode, ConstSet = TConstraintSet
def AddConstraintSet(Parent, ConstSet):
    setnode = ET.Element('PintConstraintSet')
    setnode.set('Name', ConstSet.Name)
    setnode.set('NumModels', str(ConstSet.NumModels))
    setnode.set('MinOverlap', str(ConstSet.MinOverlap))
    for f in range(len(ConstSet.Constraints)):
        if ConstSet.Constraints[f].ConstraintType == CEuclideanDistance:
            constnode = ET.Element('DistanceConstraint')
            constnode.set('Name', ConstSet.Constraints[f].Name)
            constnode.append(CreatePointSet(ConstSet.Constraints[f].TargetPoints, 'TargetPoints'))
            constnode.append(CreatePointSet(ConstSet.Constraints[f].ProbePoints, 'ProbePoints'))
            AddTextNode('Distance', str(round(ConstSet.Constraints[f].Distance, 3)), constnode)
            setnode.append(constnode)
        elif ConstSet.Constraints[f].ConstraintType == CNormalPlane:
            print('Added plane constraint')
            constnode = ET.Element('NormalPlaneConstraint')
            constnode.set('Name', ConstSet.Constraints[f].Name)
            AddTextNode('Margin', str(round(ConstSet.Constraints[f].Distance, 3)), constnode)
            setnode.append(constnode)
    if ConstSet.DockModels is not None:
        modelset = ET.Element('ComputedModels')
        for f in range(len(ConstSet.DockModels)):
            model = ET.Element('Model')
            AddTextNode('Contact', str(ConstSet.DockModels[f].OverlapScore), model)
            AddCoordinate(model, 'Translation', ConstSet.DockModels[f].TransVec)
            AddQuaternion(model, 'Rotation', ConstSet.DockModels[f].Rotation)
            modelset.append(model)
        setnode.append(modelset)
    for f in range(len(ConstSet.ScoreResults)):
        scoreset = ET.Element('ComputedScore')
        scoreset.set('Name', ConstSet.ScoreResults[f].ScoreID)
        for g in range(len(ConstSet.ScoreResults[f].ScoreVals)):
            score = ET.Element('ModelScore')
            score.set('Value', str(round(ConstSet.ScoreResults[f].ScoreVlas[g], 3)))
            scoreset.append(score)
        setnode.append(scoreset)
    Parent.append(setnode)


# Parent = TDOMNode, Stats = const TDockStats
def AddDockStats(Parent, Stats):
    statnode = ET.Element('Stats')
    AddTextNode('DigitizationTickCount', str(Stats.DigitizationTickCount), statnode)
    AddTextNode('ConstraintsTickCount', str(Stats.ConstraintsTickCount), statnode)
    AddTextNode('DomainTickCount', str(Stats.DomainTickCount), statnode)
    AddTextNode('ScoringTickCount', str(Stats.ScoringTickCount), statnode)
    AddTextNode('TotalTickCount', str(Stats.TotalTickCount), statnode)
    AddTextNode('TestedModelsCount', str(Stats.TestedModelsCount), statnode)
    AddTextNode('InsertedModelsCount', str(Stats.InsertedModelsCount), statnode)
    Parent.append(statnode)


# Parent = TDOMNode, NodeName = string, ShapeInfo = TShapeInfo
def AddShapeInfo(Parent, NodeName, ShapeInfo):
    shapenode = ET.Element(NodeName)
    for f in range(len(ShapeInfo.Coords)):
        atomnode = ET.Element('Atom')
        atomnode.set('X', str(round(ShapeInfo.Coords[f][0], 3)))
        atomnode.set('Y', str(round(ShapeInfo.Coords[f][1], 3)))
        atomnode.set('Z', str(round(ShapeInfo.Coords[f][2], 3)))
        atomnode.set('R', str(round(ShapeInfo.Rads[f], 3)))
        shapenode.append(atomnode)
    Parent.append(shapenode)


# Parent = TDOMNode, Run = TDockRun
def AddRotations(Parent, Run):
    axes = ET.Element('Axes')
    for f in range(len(Run.Axes)):
        AddCoordinate(axes, 'Axis', Run.Axes[f])
    Parent.append(axes)
    Rotations = ET.Element('Rotations')
    for f in range(len(Run.Rotations)):
        AddQuaternion(Rotations, 'Rotation', Run.Rotations[f], Run.AxisIndexes[f])
    Parent.append(Rotations)


# FileName = string
def LoadOrders(FileName):
    doc = ET.parse(FileName)
    orders = doc.getroot()
    if (orders is not None) and (orders.tag.upper() is 'ORDERS'):
        Result = []
        children = orders.xpath(".//*")
        for f in range(len(children)):
            Result.append(TDockRun())
            currdock = children[f]
            ReadDockStats(currdock, Result[f].Stats)
            try:
                ReadShapeInfo(currdock, 'Target', Result[f].Target)
                ReadShapeInfo(currdock, 'Probe', Result[f].Probe)
                ReadRotations(currdock, Result[f])
            except:
                exit()
            for g in range(len(currdock.xpath(".//*"))):
                node = currdock.xpath(".//*")[g]
                nodename = node.tag.upper()
                if nodename is 'DOCKPARAMETERS':
                    ReadDockParameters(f, node, Result)
                elif nodename is 'JOINTCONSTRAINTSET':
                    AddConstraintSetRead(f, node, Result)
                elif nodename is 'RMSDSCORE':
                    AddRMSDScoreRead(f, node, Result)
        # Return TDockRuns
        return Result
    return None


# Name = string, Parent = TDOMNode, Default = string
def ReadTextNode(Name, Parent, Default = ''):
    node = Parent.find(Name)
    if node is not None:
        # Return string
        return node.text
    else:
        # Return string
        return Default


# Node = TDOMNode, Name = string
def ReadBooleanAttribute(Node, Name):
    s = Node.attrib[Name].upper()
    # Return Boolean
    return (s is 'TRUE') or (s is '1')


# PointSet = TDOMNode, PointSetName = string
def ReadPointSet(PointSet, PointSetName = None):
    if PointSetName is None:
        Result = []
        for f in range(len(PointSet.xpath(".//*"))):
            point = PointSetName.xpath(".//*")[f]
            Result.append(basetypes.TCoord(float(point.attrib['X']), float(point.attrib['Y']),
                                           float(point.attrib['Z'])))
        # Return TCoord
        return Result
    else:
        ptset = PointSet.find(PointSetName)
        # Return TCoord
        return ReadPointSet(ptset)


# DockIx = Integer, Node = TDOMNode
def ReadDockParameters(DockIx, Node, Result):
    Result[DockIx].TargetFile = ReadTextNode('TargetFile', Node)
    Result[DockIx].ProbeFile = ReadTextNode('ProbeFile', Node)
    Result[DockIx].Resolution = ReadTextNode('Resolution', Node)
    Result[DockIx].AddedRadius = ReadTextNode('AddedRadius', Node)
    Result[DockIx].SecondsBetweenSaves = ReadTextNode('SecondsBetweenSaves', Node)
    Result[DockIx].CompletedRotations = ReadTextNode('CompletedRotations', Node)


# Parent = TDOMNode, CoordName = string
def ReadCoordinate(Parent, CoordName):
    tmp = Parent.find(CoordName)
    # Return TCoord
    return basetypes.TCoord(float(tmp.attrib['X']), float(tmp.attrib['Y']), float(tmp.attrib['Z']))


# Parent = TDOMNode, QuatName = string
def ReadQuaternion(Parent, QuatName):
    tmp = Parent.find(QuatName)
    # Return TQuaternion
    return geomutils.TQuaternion(float(tmp.attrib['r']), float(tmp.attrib['i']), float(tmp.attrib['j']),
                                 float(tmp.attrib['k']))


# DockIx = Integer, Node = TDOMNode
def AddConstraintSetRead(DockIx, Node, Result):
    constraint = TConstraintSet()
    constraint.Name = Node.attrib['Name']
    constraint.NumModels = int(Node.attrib['NumModels'])
    constraint.MinOverlap = int(Node.attrib['MinOverlap'])
    for f in range(len(Node.xpath(".//*"))):
        child = Node.xpath(".//*")[f]
        nodename = child.tag.upper()
        if nodename is 'DISTANCECONSTRAINT':
            constraint.Constraints.append(TConstraintDef(child.attrib['Name'],
                                                         ReadPointSet(child, 'ProbePoints'),
                                                         ReadPointSet(child, 'TargetPoints'),
                                                         float(ReadTextNode('Distance', child)),
                                                         CEuclideanDistance))
        elif nodename is 'NORMALPLANECONSTRAINT':
            constraint.Constraints.append(TConstraintDef(child.attrib['Name'],
                                                         [],
                                                         [],
                                                         float(ReadTextNode('Margin', child)),
                                                         CNormalPlane))
        elif nodename is 'COMPUTEDMODELS':
            for ix in range(len(constraint.DockModels)):
                model = child.xpath(".//*")[ix]
                constraint.DockModels[ix].OverlapScore = int(ReadTextNode('Contact', model))
                constraint.DockModels[ix].TransVec = ReadCoordinate(model, 'Translation')
                constraint.DockModels[ix].Rotation = ReadQuaternion(model, 'Rotation')
        elif nodename is 'COMPUTEDSCORE':
            score = TScoreResult(child.attrib['Name'])
            for g in range(len(constraint.ScoreResults)):
                constraint.ScoreResults[len(constraint.ScoreResults) - 1].ScoreVals[g] =\
                    float(child.xpath(".//*")[g].attrib['Value'])
            constraint.ScoreResults.append(score)
    Result[DockIx].ConstraintsSets.append(constraint)


'''
THIS FUNCTION IS NOT USED
# JobIx = Integer, Node = TDOMNode
def AddContactScoreRead(JobIx, Node, Result):
    score = TContactScoreDef(ReadTextNode('ScoreID', Node))
    for f in range(len(Node.xpath(".//*"))):
        child = Node.xpath(".//*")[f]
        childname = child.tag.upper()
        if childname is 'TARGETGROUP':
            score.AddTargetGroup(ReadPointSet(child))
        elif childname is 'PROBEGROUP':
            score.AddProbeGroup(ReadPointSet(child))
        elif childname is 'CONTACT':
            score.AddContact(int(child.attrib['TargetIx']), int(child.attrib['ProbeIx']),
                             int(child.attrib['ScoreVal']))
    Result[JobIx].ScoreDefs.append(score)
'''


# JobIx = Integer, Node = TDOMNode
def AddRMSDScoreRead(JobIx, Node, Result):
    score = TRMSDScoreDef(ReadTextNode('ScoreId', Node))
    score.SetCoords(ReadPointSet(Node, 'Predicted'), ReadPointSet(Node, 'Template'),
                    int(ReadTextNode('firstProbePoint', Node)))
    if 'FitTarget' in Node.attrib:
        score.FitTarget = ReadBooleanAttribute(Node, 'FitTarget')
    if 'FitProbe' in Node.attrib:
        score.FitTarget = ReadBooleanAttribute(Node, 'FitProbe')
    if 'ScoreTarget' in Node.attrib:
        score.FitTarget = ReadBooleanAttribute(Node, 'ScoreTarget')
    if 'ScoreProbe' in Node.attrib:
        score.FitTarget = ReadBooleanAttribute(Node, 'ScoreProbe')
    Result[JobIx].ScoreDefs.append(score)


# Parent = TDOMNode, Stats = TDockStats
def ReadDockStats(Parent, Stats):
    statnode = Parent.find('Stats')
    if statnode is None:
        ZeroStats(Stats)
    else:
        Stats.DigitizationTickCount = int(ReadTextNode('DigitizationTickCount', statnode))
        Stats.ConstraintsTickCount = int(ReadTextNode('ConstraintsTickCount', statnode))
        Stats.DomainTickCount = int(ReadTextNode('DomainTickCount', statnode))
        Stats.ScoringTickCount = int(ReadTextNode('ScoringTickCount', statnode))
        Stats.TotalTickCount = int(ReadTextNode('TotalTickCount', statnode))
        Stats.TestedModelsCount = int(ReadTextNode('TestedModelsCount', statnode))
        Stats.InsertedModelsCount = int(ReadTextNode('InsertedModelsCount', statnode))
    Parent.append(statnode)


# Parent = TDOMNode, NodeName = string, ShapeInfo = TShapeInfo
def ReadShapeInfo(Parent, NodeName, ShapeInfo):
    shapenode = Parent.find(NodeName)
    for f in range(len(shapenode.xpath(".//*"))):
        atomnode = shapenode.xpath(".//*")[f]
        ShapeInfo.Coords.append(basetypes.TCoord(float(atomnode.attrib['X']), float(atomnode.attrib['Y']),
                                                 float(atomnode.attrib['Z'])))
        ShapeInfo.Rads.append(int(atomnode.attrib['R']))


# Parent = TDOMNode, Run = TDockRun
def ReadRotations(Parent, Run):
    Run.Axes = ReadPointSet(Parent, 'Axes')
    rotations = Parent.find('Rotations')
    for f in range(len(rotations.xpath(".//*"))):
        rotation = rotations.xpath(".//*")[f]
        Run.Rotations.append(geomutils.Quaternion(float(rotation.attrib['r']),
                                                  float(rotation.attrib['i']),
                                                  float(rotation.attrib['j']),
                                                  float(rotation.attrib['k'])))
        Run.AxisIndexes.append(int(rotation.attrib['ix']))


# FileName = string
def ReadAsTable(FileName):
    jobs = LoadOrders(FileName)
    Result = [FileName]
    for f in range(len(jobs)):
        Result.append('')
        Result.append(jobs[f].TargetFile + ' to ' + jobs[f].ProbeFile)
        nummodels = 0

        # Headers
        s = ''
        s2 = ''
        for g in range(len(jobs[f].ConstraintSets)):
            s = s + jobs[f].ConstraintSets[g].Name + chr(9) + chr(9)
            s2 = s2 + 'ID' + chr(9) + 'Overlap' + chr(9)
            nummodels = basetypes.Max(nummodels, len(jobs[f].ConstraintSets[g].DockModels))
            for h in range(len(jobs[f].ConstraintSets[g].ScoreResults)):
                s = s + chr(9)
                s2 = s2 + jobs[f].ConstraintSets[g].ScoreResults[h].ScoreId + chr(9)
        Result.append(s)
        Result.append(s2)

        # Values
        for g in range(nummodels):
            s = ''
            for h in range(len(jobs[f].ConstraintSets)):
                if g < len(jobs[f].ConstraintSets[h].DockModels):
                    s = s + str(g) + chr(9) + str(jobs[f].ConstraintSets[h].DockModels[g].OverlapScore) + chr(9)
                else:
                    s = s + chr(9) + chr(9)
                for i in range(len(jobs[f].ConstraintSets[h].ScoreResults)):
                    if g < len(jobs[f].ConstraintSets[h].ScoreResults[i].ScoreVals):
                        s = s + str(round(jobs[f].ConstraintSets[h].ScoreResults[i].ScoreVals[g], 3)) + chr(9)
                    else:
                        s = s + chr(9)
            Result.append(s)
    # Return TSimpleStrings
    return Result


# Stats = TDockStats
def ZeroStats(Stats):
    Stats.DigitizationTickCount = 0
    Stats.ConstraintsTickCount = 0
    Stats.DomainTickCount = 0
    Stats.ScoringTickCount = 0
    Stats.TotalTickCount = 0
    Stats.TestedModelsCount = 0
    Stats.InsertedModelsCount = 0


# First function: Target, Probe = TMolecule, Run = const TDockRun, ConstSetIx, ModelId = Integer
# Second function: Run = const TDockRun, ConstSetIx, ModelId = Integer
def GetModel(Target, Probe, Run, ConstSetIx, ModelId):
    if isinstance(Target, molecules.TMolecule):
        # First function
        Result = Target.GetCopy(Target, None)
        rot = Run.ConstraintSets[ConstSetIx].DockModels[ModelId].Rotation
        transvec = Run.ConstraintSets[ConstSetIx].DockModels[ModelId].TransVec
        for f in range(len(Probe.Groups)):
            chain = Probe.GetCopy(Probe.Groups[f], None)
            chain.Transform(rot)
            chain.Transform(transvec)
            Result.AddGroup(chain)
        # Return TMolecule
        return Result
    else:
        # Second function
        ModelId = Run
        ConstSetIx = Probe
        Run = Target
        modelman = pdbmolecules.TPDBModelMan(oclconfiguration.Config.MonomersPath)
        target = modelman.LoadLayer(Run.TargetFile)
        probe = modelman.LoadLayer(Run.ProbeFile)
        # Return TMolecule
        return GetModel(target, probe, Run, ConstSetIx, ModelId)


# Orders = TDockRuns
def FreeOrders(Orders):
    # In Python return TDockRuns
    return []


# DockRun = const TDockRun
def SortedModelList(DockRun):
    ConstSetIxs = []
    ModelIxs = []
    cixs = []
    mixs = []
    overlaps = []

    # Flatten list and sort by geometric overlap
    for c in range(len(DockRun.ConstraintSets)):
        for m in range(len(DockRun.ConstraintSets[c].DockModels)):
            cixs.append(c)
            mixs.append(m)
            overlaps.append(DockRun.ConstraintSets[c].DockModels[m].OverlapScore)
    sortedixs = quicksort.QSAscendingIndex(overlaps)

    # Run through sorted list and check those with same overlap score to skip repeats
    checkfrom = len(sortedixs)
    count = 0
    lastoverlap = -1
    for m in range(len(sortedixs)):
        isrepeat = False
        ix = sortedixs[m]
        overlap = DockRun.ConstraintSets[cixs[ix]].DockModels[mixs[ix]].OverlapScore
        # Check all with same overlap value to test if repeated
        if overlap == lastoverlap:
            for f in range(checkfrom, count - 1, -1):
                if IsSame(cixs[ix], mixs[ix], ConstSetIxs[f], ModelIxs[f], DockRun):
                    isrepeat = True
                    break
        if not isrepeat:
            if overlap is not lastoverlap:
                checkfrom = count
                lastoverlap = overlap
            ConstSetIxs[count] = cixs[ix]
            ModelIxs[count] = mixs[ix]
            count = count + 1


# Cix1, Mix1, Cix2, Mix2 = Integer, DockRun = TDockRun
def IsSame(Cix1, Mix1, Cix2, Mix2, DockRun):
    Result = geomutils.Distance(DockRun.ConstraintSets[Cix1].DockModels[Mix1].Rotation,
                                DockRun.ConstraintSets[Cix2].DockModels[Mix2].Rotation) <= 1e-12
    if Result:
        Result = geomutils.Distance(DockRun.ConstraintSets[Cix1].DockModels[Mix1].TransVec,
                                    DockRun.ConstraintSets[Cix2].DockModels[Mix2].TransVec) <= 1e-12
    # Return Boolean
    return Result

'''
THIS FUNCTION IS NOT USED
# DockRun = const TDockRun
def CountModels(DockRun):
    Result = 0
    for f in range(len(DockRun.ConstraintSets)):
        Result = Result + len(DockRun.ConstraintSets[f].DockModels)
    # Return Integer
    return Result
'''


'''
THIS FUNCTION IS NOT USED
# ConstSet = const TConstraintSet
def ComputedScores(ConstSet):
    Result = []
    for f in range(len(ConstSet.ScoreResults)):
        Result.append(ConstSet.ScoreResults[f].ScoreId)
'''


# ConstSet = const TConstraintSet, ModelIx = Integer, ScoreId = string, Default = TFloat
def ScoreVal(ConstSet, ModelIx, ScoreId, Default = 0):
    scoreix = len(ConstSet.ScoreResults) - 1
    while (scoreix >= 0) and (ConstSet.ScoreResults[scoreix].ScoreId is not ScoreId):
        scoreix = scoreix - 1
    if scoreix >= 0:
        Result = ConstSet.ScoreResults[scoreix].ScoreVals[ModelIx]
    else:
        Result = Default
    # Return TFloat
    return Result
