from enum import Enum


"""Enumeration, I don't know what is its purpose"""
class PDBChargeOrigin(Enum):
    pdbNone = 1,
    pdbOccTemp = 2
    pdbOccupancy = 3
    pdbCharge = 4


class PDB:
    def __init__(self, file):
        self.LoadLayer(file)

    def LoadLayer(self, file):
        print("banana")
