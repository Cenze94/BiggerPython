import basetypes
import rmsd
import alignment
import pdbmolecules
import molecules
import geomutils
import sys


class TFitResult:
    # Rotation = TRotMatrix, Translation, Center = TCoord, Rmsd = Tfloat, AlignmentScore = TFloats, Map = TIntegers,
    # ResidueMaps = array of TIntegers
    def __init__(self, Rotation = geomutils.TRotMatrix(), Translation = basetypes.TCoord(), Center = basetypes.TCoord(),
                 Rmsd = 0, AlignmentScores = None, Map = None, ResidueMaps = None):
        self.Rotation = Rotation
        self.Translation = Translation
        self.Center = Center
        self.Rmsd = Rmsd
        if AlignmentScores is None:
            AlignmentScores = []
        self.AlignmentScores = AlignmentScores
        # Which chain of Probe corresponds to which of probe; nil if fit failed
        if Map is None:
            Map = []
        self.Map = Map
        # One map per probe chain indicating corresponding residues in target
        if ResidueMaps is None:
            ResidueMaps = []
        self.ResidueMaps = ResidueMaps


class TFitCoordsResult:
    # ProbeCoords, TargetCoords = TCoords, FitResult = TFitResult
    def __init__(self, ProbeCoords = None, TargetCoords = None, FitResult = TFitResult()):
        if ProbeCoords is None:
            ProbeCoords = []
        self.ProbeCoords = ProbeCoords
        if TargetCoords is None:
            TargetCoords = []
        self.TargetCoords = TargetCoords
        self. FitResult = FitResult


# Mol1, Mol2 = TMolecule, SubMat = TSubMatrix, MissingMarker = string
def AlignSequences(Mol1, Mol2, SubMat, MissingMarker = 'X'):
    seq1 = pdbmolecules.ChainSequence(Mol1, MissingMarker)
    seq2 = pdbmolecules.ChainSequence(Mol2, MissingMarker)
    # Return TPairwiseAlignment
    return alignment.NeedlemanWunschAlign(seq1, seq2, SubMat)


# Computes the fits from Probe to Target. First matches the probe chains to the available (not in TargetExclusion)
# target chains by sequence (best first). Then computes RMSD of matched chains and returns the fit
# Probe, Target = const TMolecule, SubMat = const TSubMatrix, TargetExclusion = TIntegers, MinSequenceMatch = TFloat,
# MinAlfaCarbons = Integer
def MagicFit(Probe, Target, SubMat, TargetExclusion = None, MinSequenceMatch = 0.7, MinAlfaCarbons = 20):
    Result = TFitResult()
    Result.Rmsd = -1
    alignments = BuildAlignmentMatrix(Probe, Target, SubMat, TargetExclusion)
    # Target ix for each probe chain
    map = basetypes.FilledInts(Probe.GroupCount, -1)
    FindBestMap(0, Result, Probe, Target, map, alignments, MinSequenceMatch, MinAlfaCarbons)
    # Return TFitResult
    return Result


# Probe, Target = const TMolecule, SubMat = const TSubMatrix, TargetExclusion = TIntegers
def BuildAlignmentMatrix(Probe, Target, SubMat, TargetExclusion):
    # Target chains in first dimension, Probe chains in second dimension, all alignments are Probe to Target
    alignments = []
    for f in range(Target.GroupCount):
        alignments.append([])
        for g in range(len(Probe.GroupCount)):
            alignments[f].append(alignment.TPairwiseAlignment())
    for ct in range(Target.GroupCount):
        if basetypes.IsInArray(ct, TargetExclusion):
            for f in range(len(alignments[ct])):
                alignments[ct][f].Score = -1
                alignments[ct][f].Map = []
        else:
            cht = Target.GetGroup(ct)
            for cp in range(Probe.GroupCount):
                chp = Probe.GetGroup(cp)
                alignments[ct][cp] = AlignSequences(chp, cht, SubMat, '')
                # Convert score to percent match. Empty missing marker to ignore missing residues
                score = 0
                if len(alignments[ct][cp]) != 0:
                    for f in range(len(alignments[ct][cp].Map)):
                        if alignments[ct][cp].Map[f] > 0:
                            score = score + 1
                    alignments[ct][cp].Score = score / (0.5 * (cht.GroupCount + chp.GroupCount))
    # In Python return array of array of TPairwiseAlignment
    return alignments


# Result = TFitResult, Probe, Target = const TMolecule, map = TIntegers,
# alignments = array of array of TPairwiseAlignment, MinAlfaCarbons = Integer
def TryFit(Result, Probe, Target, map, alignments, MinAlfaCarbons):
    tar = []
    prb = []
    currpos = 0
    for f in range(len(Map)):
        if map[f] >= 0:
            tarchain = Target.GetGroup(map[f])
            prbchain = Probe.GetGroup(f)
            for r in range(len(alignments[map[f]][f].Map)):
                if alignments[map[f]][f].Map[r] >= 0:
                    tca = tarchain.GetGroup(alignments[map[f]][f].Map[r]).GetAtom('CA')
                    pca = prbchain.GetGroup(r).GetAtom('CA')
                    if (tca is not None) and (pca is not None):
                        tar.append(tca.Coords)
                        prb.append(pca.Coords)
                        currpos = currpos + 1

    # Check if at least MinAlfaCarbons atoms were used
    if currpos < MinAlfaCarbons:
        sys.exit()

    rmsdcalc = rmsd.TRMSDCalculator()
    rmsdcalc.AddCoordinates(tar, prb)
    rmsdcalc.Minimise(1000, 0.01)

    tmp = ''
    for f in range(len(map)):
        tmp = tmp + str(map[f]) + ' '

    if (Result.Rmsd < 0) or (Result.Rmsd > rmsdcalc.FRmsd):
        Result.Rmsd = rmsdcalc.FRmsd
        Result.Center = rmsdcalc.FCenterVec
        Result.Rotation = rmsdcalc.FBaseRotation
        Result.Translation = rmsdcalc.FTranslation

        for f in range(len(map)):
            if map[f] > 0:
                Result.AlignmentScores[f] = alignments[map[f]][f].Score
                Result.ResidueMaps[f] = alignments[map[f]][f].Map
            else:
                Result.AlignmentScores[f] = -1
                Result.ResidueMaps[f] = []
            Result.Map[f] = map[f]

'''
THIS FUNCTION IS NOT USED
# alignments = array of array of TPairwiseAlignment
def HighestScore(alignments):
    Tix = 0
    Pix = 0
    Score = alignments[0][0].Score
    for f in range(len(alignments)):
        for g in range(len(alignments[0])):
            if alignments[f][g].Score > Score:
                Tix = f
                Pix = g
                Score = alignments[f][g].Score
    # In Python return Integer, Integer, TFloat
    return Tix, Pix, Score
'''


# Tix, Pix = Integer, alignments = array of array of TPairwiseAlignment
def WipeRowCol(Tix, Pix, alignments):
    for f in range(len(alignments)):
        alignments[f][Pix].Score = -1
    for f in range(len(alignments[0])):
        alignments[Tix][f].Score = -1


# Level = Integer, Result = TFitResult, Probe, Target = const TMolecule, map = TIntegers,
# alignments = array of array of TPairwiseAlignment, MinSequenceMatch = TFloat, MinAlfaCarbons = Integer
def FindBestMap(Level, Result, Probe, Target, map, alignments, MinSequenceMatch, MinAlfaCarbons):
    if Level > len(map) - 1:
        TryFit(Result, Probe, Target, map, alignments, MinAlfaCarbons)
    else:
        foundone = False
        for tix in range(len(alignments)):
            if alignments[tix][Level].Score >= MinSequenceMatch:
                cmap = []
                for f in range(Level):
                    cmap.append(map[f])
                if (Level == 0) or ((Level > 0) and (not basetypes.IsInArray(tix, cmap))):
                    foundone = True
                map[Level] = tix
                FindBestMap(Level + 1, map, alignments, MinSequenceMatch, MinAlfaCarbons)
        if not foundone:
            map[Level] = -1
            FindBestMap(Level + 1, map, alignments, MinSequenceMatch, MinAlfaCarbons)


# Fills ProbeCoords and TargetCoords with the alpha carbon coordinates of the matching residues after a MagicFit with
# the values given
# Probe, Target = const TMolecule, SubMat = const TSubMatrix, TargetExclusion = TIntegers, MinSequenceMatch = TFloat
def MagicFitAlphaCoords(Probe, Target, SubMat, TargetExclusion = None, MinSequenceMatch = 0.7):
    Result = TFitCoordsResult()
    Result.FitResult = MagicFit(Probe, Target, SubMat, TargetExclusion, MinSequenceMatch)
    currix = 0
    for f in range(len(Result.FitResult.Map)):
        if Result.FitResult.Map[f] >= 0:
            pchain = Probe.GetGroup(f)
            tchain = Target.GetGroup(Result.FitResult.Map[f])
            if (pchain is not None) and (tchain is not None):  # This should not be necessary
                for g in range(len(Result.FitResult.ResidueMaps[f])):
                    if Result.FitResult.ResidueMaps[f][g] >= 0:
                        pres = pchain.GetGroup(g)
                        tres = tchain.GetGroup(Result.FitResult.ResidueMaps[f][g])
                        if (pres is not None) and (tres is not None):
                            pca = pres.GetAtom('CA')
                            tca = tres.GetAtom('CA')
                            if (pca is not None) and (tca is not None):
                                Result.ProbeCoords[currix] = pca.Coords
                                Result.TargetCoords[currix] = tca.Coords
                                currix = currix + 1
    # Return TFitCoordsResult
    return Result


# Returns all residues from the two molecules that were matched in the fit result
# Probe, Target = const TMolecule, FitResult = const TFitResult
def GetMatchingResidues(Probe, Target, FitResult):
    ProbeRes = []
    TargetRes = []
    for f in range(len(FitResult.ResidueMaps)):
        if len(FitResult.Map[f]) > 0:
            targc = Target.GetGroup(FitResult.Map[f])
            probec = Probe.GetGroup(f)
            for g in range(len(FitResult.ResidueMaps[f])):
                if FitResult.ResidueMaps[f][g] >= 0:
                    ProbeRes.append(probec.GetGroup(g))
                    TargetRes.append(targc.GetGroup(FitResult.ResidueMaps[f][g]))
    # In Python return TMolecules, TMolecules
    return ProbeRes, TargetRes


# FitResult = const TFitResult
def CountResidueMatches(Fit):
    Result = 0
    for f in range(len(Fit.ResidueMaps)):
        for g in range(len(Fit.ResidueMaps[f])):
            if Fit.ResidueMaps[f][g] >= 0:
                Result = Result + 1
    # Return Integer
    return Result
