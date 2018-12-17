import sequence


class TFastaData:
    # ID, Sequence = String
    def __init__(self, ID, Sequence):
        self.ID = ID
        self.Sequence = Sequence


class TFastaReader:
    # fromfile = string
    def __init__(self, fromfile):
        self.FSeqs = []
        self.Load(fromfile)

    #seq = TFastaData
    def AddSeq(self, seq):
        self.FSeqs.append(seq)

    # IdLine = string
    def ExtractID(self, IdLine):
        ix = IdLine.find(' ')
        if ix > 0:
            # Return string
            return IdLine[:ix]
        else:
            # Return string
            return IdLine

    # IdLine = string
    def ExtractOrganism(self, IdLine):
        ix = IdLine.upper().find('OS="')
        if ix > 0:
            end = IdLine.find('"', ix + 4)
            # Return string
            return IdLine[ix + 4:end]
        else:
            ix = IdLine.upper().find('[ORGANISM=')
            if ix > 0:
                end = IdLine.find(']', ix + 10)
                # Return string
                return IdLine[ix + 10:end]
            else:
                # Return string
                return ''

    # Destructor redefinition is not required in this case, because of garbage collector

    # fromfile = string
    def Load(self, fromfile):
        cur = TFastaData('', '')
        with open(fromfile, 'r') as f:
            buf = f.read().splitlines()
        for f in range(len(buf)):
            s = buf[f]
            if s.find('>') is 0:
                if cur.ID is not '':
                    self.AddSeq(cur)
                cur.ID = s[1:]
                cur.Sequence = ''
            else:
                cur.Sequence = cur.Sequence + s
        if cur.Sequence is not '':
            self.AddSeq(cur)

    def AsOCSequences(self):
        Result = []
        for f in range(len(self.FSeqs)):
            Result.append(sequence.TOCSequence(self.FSeqs[f].Sequence, self.ExtractOrganism(self.FSeqs[f].ID), '',
                                               self.ExtractID(self.FSeqs[f].ID)))

    # ID = string
    def SequenceByID(self, ID):
        for f in range(len(self.FSeqs)):
            if self.FSeqs[f].ID is ID:
                # Return string
                return self.FSeqs[f].Sequence


class TFastaWriter:
    def __init__(self):
        self.FSeqs = []

    # First method: Seq = TFastaData
    # Second method: Seq, IncludeOrganism = string
    # Third method: Seq = TOCSequence, IncludeOrganism = Boolean
    def AddSeq(self, Seq, IncludeOrganism = False):
        if isinstance(Seq, TFastaData):
            # First method
            self.FSeqs.append(Seq)
        elif isinstance(Seq, sequence.TOCSequence):
            # Third method
            self.FSeqs.append(TFastaData(Seq.ID, Seq.Sequence))
            if IncludeOrganism:
                self.FSeqs[len(self.FSeqs) - 1].ID = self.FSeqs[len(self.FSeqs) - 1].ID + '|os' + Seq.Organism
        else:
            # Second method
            self.FSeqs.append(TFastaData(Seq, IncludeOrganism))

    # Seqs = TOCSequences, IncludeOrganism = Boolean
    def AppendSeqs(self, Seqs, IncludeOrganism = False):
        for f in range(len(Seqs)):
            if IncludeOrganism:
                self.FSeqs.append(TFastaData(Seqs[f].ID + '|os' + Seqs[f].Organism, Seqs[f].Sequence))
            else:
                self.FSeqs.append(TFastaData(Seqs[f].ID, Seqs[f].Sequence))

    # FileName = string, MaxCharsPerLine = Integer
    def SaveToFile(self, FileName, MaxCharsPerLine = 70):
        sl = []
        for f in range(len(self.FSeqs)):
            limit = MaxCharsPerLine - 2
            if limit > len(self.FSeqs[f].ID):
                limit = len(self.FSeqs[f].ID) - 1
            sl.append('>' + self.FSeqs[f].ID[:limit])
            tmp = self.FSeqs[f].Sequence
            while tmp is not '':
                if len(tmp) < MaxCharsPerLine - 1:
                    sl.append(tmp)
                    tmp = ''
                else:
                    sl.append(tmp[:MaxCharsPerLine - 1])
                    tmp = tmp[MaxCharsPerLine:]
        with open(FileName, 'w') as f:
            for item in sl:
                f.write("%s\n" % item)

    # Destructor redefinition is not required in this case, because of garbage collector
