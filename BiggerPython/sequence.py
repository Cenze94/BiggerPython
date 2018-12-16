import basetypes


class TOCSequence:
    # These fields are based on the EBI results
    # Sequence, Organism, Database, ID, Code, Evidence, Gene, Description = String, Score, Bits, Expectation, Identity,
    # Positives = TFloat
    def __init__(self, Sequence = '', Organism = '', Database = '', ID = '', Code = '', Evidence = '', Gene = '',
                 Description = '', Score = -1, Bits = -1, Expectation = -1, Identity = -1, Positives = -1):
        self.Sequence = Sequence
        self.Organism = Organism
        self.Database = Database  # E.g. uniprot
        self.ID = ID  # Protein identifier in the database. E.g. Q6AW43_9ANNE
        self.Code = Code  # Protein accession code, e.g. Q6AW43
        self.Evidence = Evidence  # UniProtKB, evidence level, can be a number 1-5; see http://www.uniprot.org/manual/protein_existence
        self.Gene = Gene
        self.Description = Description
        # These are used when the set of sequences results from database queries
        self.Score = Score
        self.Bits = Bits
        self.Expectation = Expectation
        self.Identity = Identity
        self.Positives = Positives


class TOCCompressedSequenceRec:
    # Symbol = Char, First, Last = Integer
    def __init__(self, Symbol = None, First = -1, Last = -1):
        self.Symbol = Symbol
        self.First = First
        self.Last = Last


# se = TOCSequence, ss = TOCSequences
def AddSequence(se, ss):
    ss.append(se)


def EmptySequence():
    #Return TOCSequence
    return TOCSequence()


# Lists all different organisms, by order of occurrence in the sequence array
# Sequences = const TOCSequences
def ListOrganisms(Sequences):
    Result = []
    for f in range(len(Sequences)):
        if not basetypes.IsInArray(Sequences[f].Organism, Result):
            Result.append(Sequences[f].Organism)
    # Return TSimpleStrings
    return Result


# The resulting set contains the same set of sequence arrays but with each array containing only thoses sequences from
# organisms present in all sequence arrays, and only the first of each.
# SequencesSet = TOCSequencesSet
def FilterByCommonOrganisms(SequencesSet):
    organisms = ListOrganisms(SequencesSet[0])
    inall = []
    indices = []
    for f in range(len(SequencesSet)):
        indices.append([])
    for f in range(len(organisms)):
        inall.append(True)
    for f in range(len(organisms)):
        g = 0
        while inall[f] and (g <= len(SequencesSet) - 1):
            indices[g].append(FirstIxByOrganism(SequencesSet[g], organisms[f]))
            inall[f] = (indices[g][f] >= 0)
            g = g + 1
    Result = []
    for f in range(len(SequencesSet)):
        Result.append([])
    for f in range(len(inall)):
        if inall[f]:
            for g in range(len(Result)):
                Result[g].append(SequencesSet[g][indices[g][f]])


# Compressed sequences store repeated letters as first..last segments. These are mostly useful for storing alignment
# columns, as these often have repeats
# Sequences = const TOCSequencesSet, Organism = string
def FirstIxByOrganism(Sequences, Organism):
    Result = 0
    while (Result <= len(Sequences) - 1) and (Organism is not Sequences[Result].Organism):
        Result = Result + 1
    if Result > len(Sequences) - 1:
        Result = -1
    # Return Integer
    return Result


# Sequence = string
def CompressSequence(Sequence):
    lastix = -1
    lastsymbol = ''
    Result = []
    for f in range(len(Sequence)):
        Result.append(TOCCompressedSequenceRec())
    for f in range(1, len(Sequence) + 1):
        if Sequence[f] is not lastsymbol:
            lastsymbol = Sequence[f]
            if lastix >= 0:
                Result[lastix].Last = f - 1
            lastix = lastix + 1
            Result[lastix].First = f
            Result[lastix].Symbol = Sequence[f]
    if lastix >= 0:
        Result[lastix].Last = len(Sequence)
    # Return TOCCompressedSequence
    return Result


# Sequence = TOCSequences OR TSimpleStrings
def CompressSequences(Sequences):
    Result = []
    if isinstance(Sequences[0], TOCSequence):
        for f in range(len(Sequences)):
            Result.append(CompressSequence(Sequences[f].Sequence))
    else:
        for f in range(len(Sequences)):
            Result.append(CompressSequence(Sequences[f]))
    # Return TOCCompressedSequences
    return Result
