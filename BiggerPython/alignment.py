import fasta
import sequence
import stringutils
import basetypes

# In the original file there were also "progress" and "debugutils" as imported units, since they are used for debugging
# they could be ignored in Python. "progress" is used in "NeedlemanWunschalign" function. "debugutils" is used in
# "AlignmentToSequence"

# Default marker for sequence gaps
DefaultGapMarker = '-'
# Default marker used in substitution matrices. This is the index used in
DefaultSubstitutionGapMarker = '*'


# Record for returning pairwise alignment results (mapping and score)
class TPairwiseAlignment:
    # Map = TIntegers, Score = TFloat
    def __init__(self, Map = None, Score = 0):
        if Map is None:
            Map = []
        self.Map = Map
        self.Score = Score  # Indicates for each residue of molecule 1 which residue of 2 is a match. -1 if not matched


# Sequences stored in these data structures are meant to be the alignment results, not the original sequences. The
# SequenceID fields should be used to identify the original sequences
class TMSA:
    # This record holds multiple alignment matrixes; it may also hold a set of single alignments to one query sequence
    # SequenceIDs, Alignment = TSimpleStrings, GapMarker = Char
    def __init__(self, SequenceIDs = None, Alignment = None, GapMarker = ''):
        if SequenceIDs is None:
            SequenceIDs = []
        if Alignment is None:
            Alignment = []
        self.SequenceIDs = SequenceIDs
        self.Alignment = Alignment
        self.GapMarker = GapMarker


class TSingleAlignment:
    # QueryId, MatchID = string, Score, Bits, Expectation, Probability = TFloat, Identity, Positives, QueryStart,
    # QueryEnd, MatchStart, MatchEnd = Integer, AlignedQuery, AlignedMatch = string
    def __init__(self, QueryId = '', MatchID = '', Score = -1, Bits = -1, Expectation = -1, Probability = -1,
                 Identity = -1, Positives = -1, QueryStart = -1, QueryEnd = -1, MatchStart = -1, MatchEnd = -1,
                 AlignedQuery = '', AlignedMatch = ''):
        self.QueryId = QueryId
        self.MatchID = MatchID
        self.Score = Score
        self.Bits = Bits
        self.Expectation = Expectation
        self.Probability = Probability
        self.Identity = Identity
        self.Positives = Positives
        self.QueryStart = QueryStart
        self.QueryEnd = QueryEnd
        self.MatchStart = MatchStart
        self.MatchEnd = MatchEnd
        self.AlignedQuery = AlignedQuery
        self.AlignedMatch = AlignedMatch


class TSubMatrix:
    # Substitution matrices
    # MonomerIndex = string, Matrix = TMatrix, Comments = TSimpleStrings, GapPenalty, GapExtension = TFloat
    def __init__(self, MonomerIndex = '', Matrix = None, Comments = None, GapPenalty = -1, GapExtension = -1):
        if Matrix is None:
            Matrix = []
        if Comments is None:
            Comments = []
        self.MonomerIndex = MonomerIndex  # One letter code for monomers, same length as matrix
        self.Matrix = Matrix  # Substitution values
        # NOTE: MonomerIndex is 1 based. Must subtract 1 to obtain index in Matrix
        self.Comments = Comments
        self.GapPenalty = GapPenalty
        self.GapExtension = GapExtension


# Removes all gaps from the identified sequence and trims all others. Helps to find the residue variations from one
# sequence to all others. If the sequence is not found returns an empty MSA
# msa = TMSA, seqid = string
def TrimMSA(msa, seqid):
    Result = TMSA(None, None, msa.GapMarker)
    six = stringutils.LastIndexOf(seqid, msa.SequenceIDs)
    if six >= 0:
        positionlist = GetList(msa.Alignment[six], msa.GapMarker)
        for f in range(len(msa.Alignment)):
            Result.SequenceIDs.append(msa.SequenceIDs[f])
            Result.Alignment.append(FromList(msa.Alignment[f], positionlist))
    # Return TMSA
    return Result


# s = string, gm = char
def GetList(s, gm):
    positionlist = []
    for f in range(len(s)):
        if s[f] is not gm:
            positionlist.append(f)
    # Return TIntegers
    return positionlist


# s = string, positionlist = TIntegers
def FromList(s, positionlist):
    Result = ''
    for f in range(len(positionlist)):
        Result = Result + s[positionlist[f]]
    # Return string
    return Result


# Returns strings with the MSA columns
# MSA = const TMSA
def TransposeMSA(MSA):
    if MSA.Alignment is None:
        return None
    else:
        Result = []
        for f in range(len(MSA.Alignment[0])):
            Result.append([])
            for g in range(len(MSA.Alignment)):
                Result[f].append(MSA.Alignment[f][g])
    # Return TSimpleStrings
    return Result


# Builds a MSA with the query sequence as the first sequence and all matches in the array as aligned sequences. Gaps in
# the query sequence at each alignment are removed from the matches, if TrimToQuery is true. The gapmarker must match
# the marker used in the source sequences.
# Warning: this function doesn't check if all alignments correspond to the same query sequence.
# query = string, sas = TSingleAlignments, TrimToQuery = Boolean, gapmarker = char
def SinglesToMSA(query, sas, TrimToQuery = False, gapmarker = DefaultGapMarker):
    Result = TMSA()
    l = len(sas)
    if l > 1:
        for f in range(l):
            Result.Alignment.append(BuildMatch(f, sas, gapmarker, query))
            Result.SequenceIDs.append(sas[f].MatchID)
    # Return TMSA
    return Result


# ix = Integer, sas = TSingleAlignments, gapmarker = char, query = string
def BuildMatch(ix, sas, gapmarker, query):
    Result = ''
    qst = sas[ix].QueryStart
    sq = sas[ix].AlignedQuery
    sm = sas[ix].AlignedMatch
    while len(Result) < qst - 1:
        Result = Result + gapmarker
    for f in range(len(sq)):
        if sq[f] is not gapmarker:
            Result = Result + sm[f]
    while len(Result) < len(query):
        Result = Result + gapmarker


# al = TSingleAlignment, als = TSingleAlignments
def AddAlignment(al, als):
    als.append(al)


# Reads a substitution matrix in the BLAST format, see ftp://ftp.ncbi.nih.gov/blast/matrices for examples
# FileName = string
def ReadBLASTMatrix(FileName):
    Result = TSubMatrix('', None, None, -10, -1)
    with open(FileName, 'r') as f:
        buf = f.read().splitlines()
    for f in range(len(buf)):
        s = buf[f]
        # Snip comments
        ix = s.find('#')
        if ix > 0:
            Result.Comments.append(s[ix + 1:])
            s = s[:ix]
        if s is not '':
            if Result.MonomerIndex is '':  # First data line must be headings
                for ix in range(len(s)):
                    if ord(s[ix]) > ord(' '):
                        Result.MonomerIndex = Result.MonomerIndex + s[ix]
                        Result.Matrix.append([])
            else:  # Other lines are value lines
                ix = Result.MonomerIndex.find(s[0])
                if ix > 0:
                    ix = ix - 1
                    s = s[1:]  # Remove monomer code
                    Result.Matrix[ix] = basetypes.StringToFloats(s)
    # Return TSubMatrix
    return Result


# MSA = TMSA, FileName = string
def SaveMSAToFile(MSA, FileName):
    buf = ['**IDENTIFIERS**']
    for f in range(len(MSA.SequenceIDs)):
        buf.append(MSA.SequenceIDs[f])
    buf.append('**ALIGNMENT**')
    for f in range(len(MSA.Alignment)):
        buf.append(MSA.Alignment[f])
    with open(FileName, 'w') as f:
        for item in buf:
            f.write("%s\n" % item)


# MSA = const TMSA, SubMat = const TSubMatrix
def SumOfPairScores(MSA, SubMat):
    Result = []
    if MSA.Alignment is not None:
        for col in range(len(MSA.Alignment[0])):
            Result.append(0)
            for l1 in range(1, len(MSA.Alignment) - 1):
                for l2 in range(l1 + 1, len(MSA.Alignment)):
                    Result[col] = Result[col] + GetSubScore(SubMat, MSA.Alignment[l1, col + 1],
                                                            MSA.Alignment[l2, col + 1])
    # Return TFloats
    return Result


# If A1 or A2 are not found, uses SubGapMarker
# SubMat = const TSubMatrix, A1, A2, SubGapMarker = Char
def GetSubScore(SubMat, A1, A2, SubGapMarker = DefaultSubstitutionGapMarker):
    i1 = SubMat.MonomerIndex.find(A1) - 1
    if i1 < 0:
        i1 = SubMat.MonomerIndex.find(SubGapMarker) - 1
    i2 = SubMat.MonomerIndex.find(A2) - 1
    if i2 < 0:
        i2 = SubMat.MonomerIndex.find(SubGapMarker) - 1
    if (i1 >= 0) and (i2 >= 0):
        # Return TFloat
        return SubMat.Matrix[i1, i2]
    else:
        # Return TFloat
        return 0


# Seq1, Seq2 = const string OR const TOCCompressedSequence, SubMat = const TSubMatrix, SubGapMarker = Char
def ScoreSequencePair(Seq1, Seq2, SubMat, SubGapMarker = DefaultSubstitutionGapMarker):
    Result = 0
    if isinstance(Seq1[0], sequence.TOCCompressedSequenceRec):
        assert Seq1[len(Seq1) - 1].Last is Seq2[len(Seq2) - 1].Last, 'Error in scoring sequence pair: sequences must' \
                                                                     'have equal lengths'
        ix1 = 0
        ix2 = 0
        while ix1 < len(Seq1):
            st = basetypes.Max(Seq1[ix1].First, Seq2[ix2].First)
            en = basetypes.Min(Seq1[ix1].Last, Seq2[ix2].Last) + 1
            tmp = GetSubScore(SubMat, Seq1[ix1].Symbol, Seq2[ix2].Symbol, SubGapMarker) * (en - st)
            Result = Result + tmp
            if en > Seq1[ix1].Last:
                ix1 = ix1 + 1
            if en > Seq2[ix2].Last:
                ix2 = ix2 + 1
    else:
        assert len(Seq1) == len(Seq2), 'Error in scoring sequence pair: sequences must have equal lengths'
        for f in range(len(Seq1)):
            Result = Result + GetSubScore(SubMat, Seq1[f], Seq2[f], SubGapMarker)
    # Return TFloat
    return Result

# SubMat = const TSubMatrix, GapLen = Integer
def GapPenalty(SubMat, GapLen):
    if GapLen <= 0:
        # Return TFLoat
        return 0
    else:
        return SubMat.GapPenalty + GapLen * SubMat.GapExtension


# Sequences are the indexes in the SubMatrix scores. Result is the mapping from Sequence 1 to 2
# First function: Seq1Ix, Seq2Ix = TIntegers, SubMat = const TSubMatrix
# Converts sequences to indexes, calls NeedlemanWunschAlign(Seq1Ix, Seq2Ix...
# Second function: Seq1Ix, Seq2Ix = string, SubMat = const TSubMatrix
def NeedlemanWunschAlign(Seq1Ix, Seq2Ix, SubMat):
    if isinstance(Seq1Ix, str):
        # Second function
        scoremat = []
        Result = TPairwiseAlignment()
        for f in range(len(Seq1Ix)):
            Result.Map.append(-1)
        if (Seq1Ix is not None) and (Seq2Ix is not None):
            taskstep = 2 / len(Seq1Ix) / len(Seq2Ix) / (len(Seq1Ix) + len(Seq2Ix))
            ScoreMat = []
            BuildMat(ScoreMat, Seq1Ix, Seq2Ix, SubMat)
            Result.Score = ScoreMat[0][0]
            Align(Seq1Ix, Seq2Ix, Result, scoremat, SubMat)
        # Return TPairwiseAlignment
        return Result
    else:
        # First function
        seq1ix = IndexSequence(Seq1Ix, SubMat)
        seq2ix = IndexSequence(Seq2Ix, SubMat)
        # Return TPairwiseAlignment
        return NeedlemanWunschAlign(seq1ix, seq2ix, SubMat)


# Returns 0 based array with indexes for SubMat. -1 for not found
# Seq = string, SubMat = TSubMatrix
def IndexSequence(Seq, SubMat):
    Result = []
    for f in range(len(Seq)):
        Result.append(SubMat.MonomerIndex.find(Seq[f]) - 1)
    # Return TIntegers
    return Result


# S1, E1, S2, E2 = Integer, scoremat = TMatrix, SubMat = TSubMatrix
def TotalVal(S1, E1, S2, E2, scoremat, SubMat):
    Result = scoremat[E1][E2]
    if (S1 < E1) and (S1 >= 0):
        Result = Result + GapPenalty(SubMat, E1 - S1 - 1)
    if (S2 < E2) and (S2 >= 0):
        Result = Result + GapPenalty(SubMat, E2 - S2 - 1)
    # Return TFloat
    return Result


# I1, I2 = Integer, Seq1Ix, Seq2Ix = string, scoremat = TMatrix, SubMat = TSubMatrix
def EvalMat(I1, I2, Seq1Ix, Seq2Ix, scoremat, SubMat):
    # Indexes are for the matrix, starting at 0
    tot = 0
    if (Seq1Ix[I1] >= 0) and (Seq2Ix[I2] >= 0):
        tot = SubMat.Matrix[Seq1Ix[I1]][Seq2Ix[I2]]
    # If one residue is not present in the substitution matrix then the score starts at 0
    if (I1 < len(Seq1Ix) - 1) and (I2 < len(Seq2Ix) - 1):
        max = -10000
        for x in range(I1 + 1, len(Seq1Ix)):
            t = TotalVal(I1, x, I2 + 1, I2 + 1, scoremat, SubMat)
            if t > max:
                max = t
        for y in range(I2 + 1, len(Seq2Ix)):
            t = TotalVal(I1 + 1, I1 +1, I2, y, scoremat, SubMat)
            if t > max:
                max = t
        tot = tot + max
    scoremat[I1][I2] = tot


# I1, I2 = Integer, scoremat = TMatrix, Seq1Ix, Seq2Ix = string, SubMat = TSubMatrix
def CalcLines(I1, I2, scoremat, Seq1Ix, Seq2Ix, SubMat):
    for f in range(I1):
        EvalMat(f, I2, Seq1Ix, Seq2Ix, scoremat, SubMat)
    for f in range(I2 - 1):
        EvalMat(I1, f, Seq1Ix, Seq2Ix, scoremat, SubMat)


# scoremat = TMatrix, Seq1Ix, Seq2Ix = string, SubMat = TSubMatrix
def BuildMat(scoremat, Seq1Ix, Seq2Ix, SubMat):
    i1 = len(Seq1Ix)
    i2 = len(Seq2Ix)
    while True:
        CalcLines(i1, i2, scoremat, Seq1Ix, Seq2Ix, SubMat)
        i1 = i1 - 1
        i2 = i2 - 1
        if (i1 < 0) or (i2 < 0):
            break


# Seq1Ix, Seq2Ix = string, Result = TPairwiseAlignment, scoremat = TMatrix, SubMat = TSubMatrix
def Align(Seq1Ix, Seq2Ix, Result, scoremat, SubMat):
    NI1 = -1
    NI2 = -1
    while True:
        NI1, NI2 = NextLine(NI1, NI2, scoremat, SubMat, Result)
        if (NI1 <= len(Seq1Ix) - 1) and (NI2 <= len(Seq2Ix) - 1):
            Result.Map[NI1] = NI2
        if (NI1 >= len(Seq1Ix) - 1) or (NI2 >= len(Seq2Ix) - 1):
            break


# I1, I2 = Integer, scoremat = TMatrix, SubMat = TSubMatrix, Result = TPairwiseAlignment
def NextLine(I1, I2, scoremat, SubMat, Result):
    max = 0
    s1 = I1 + 1
    s2 = I2 + 1
    I1 = len(scoremat)
    I2 = len(scoremat[0])
    try:
        for x in range(s1, len(scoremat)):
            t = TotalVal(s1 - 1, x, s2, s2, scoremat, SubMat)
            if max < t:
                max = t
                I1 = x
                I2 = s2
        for y in range(s1, len(scoremat[0])):
            t = TotalVal(s1, s1, s2 - 1, y, scoremat, SubMat)
            if max < t:
                max = t
                I1 = s1
                I2 = y
    except:
        print(str(s1) + ' ' + str(s2) + ' ' + str(len(scoremat)) + ' ' + str(len(scoremat[0])))
    if max > Result.Score:
        Result.Score = max
    # In Python return Integer, Integer
    return I1, I2


# Indexes alignment to sequence, starting at index 1 (not index 0), for indexing strings
# Align, Seq = const string, GapMarker = Char
def AlignmentToSequence(Align, Seq, GapMarker):
    c = 0
    tmp = ''
    Result = []
    for f in range(len(Align)):
        if Align[f] is not GapMarker:
            c = c + 1
            Result.append(c)
            tmp = tmp + Align[f]
        else:
            Result.append(-1)
    # Return TIntegers
    return Result


# Seq = string, Filter = TIntegers
def SequenceFromIndex(Seq, Filter):
    Result = ''
    for f in range(len(Filter)):
        if Filter[f] > 0:
            Result = Result + Seq[Filter[f]]
    # Return string
    return Result


# FASTA only
# FileName = string
def ReadMSA(FileName):
    Result = TMSA()
    Result.GapMarker = DefaultGapMarker
    reader = fasta.TFastaReader(FileName)
    for f in range(len(reader.FSeqs)):
        Result.Alignment.append(reader.FSeqs[f].Sequence)
        Result.SequenceIDs.append(reader.FSeqs[f].ID)
    # Return TMSA
    return Result


# Seq, MSASeq = string, GapMarker = char
def MapSequenceToMSA(Seq, MSASeq, GapMarker):
    lm = len(MSASeq)
    ls = len(Seq)
    s = 0
    m = 0
    Result = []
    while (s < ls) and (m < lm):
        if Seq[s] is MSASeq[m]:
            Result.append(m)
            s = s + 1
            m = m + 1
        elif MSASeq[m] is GapMarker:
            m = m + 1
        else:
            # Return TIntegers
            return None
    # Return TIntegers
    return Result


# Seq = string, MSa = TMSA
def FindMatch(Seq, MSA):
    Map = []
    for f in range(len(MSA.Alignment)):
        Map = MapSequenceToMSA(Seq, MSA.Alignment[f], MSA.GapMarker)
        if Map is None:
            # Return Integer
            return f
    # Return Integer (and TIntegers in Python)
    return -1, Map
