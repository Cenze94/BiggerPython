from guizero import App, PushButton
from tkinter import Frame
from tkinter import filedialog
import Tooltip
import pdbmolecules
import oclconfiguration
import geomutils


def loadFile():
    openFile = OpenFile(app.tk)
    pdbName = openFile.openFile()
    if pdbName != "":
        mol = FMolecules.LoadLayer(pdbName)
        # FMolecules.LayerByIx(0).DeleteWater()
        # mol.Transform(geomutils.Simmetric(FindCenter(mol)))
        print('ChainIDs:')
        print(len(mol.FChainIDs))
        for f in range(len(mol.FChainIDs)):
            print(mol.FChainIDs[f])

        print('')
        print('Atoms:')
        print(len(mol.FAtoms))
        if len(mol.FAtoms) > 0:
            atom = mol.FAtoms[0]
            print('First Atom:')
            print('IsHet = ' + str(atom.IsHet))
            print('Serial = ' + str(atom.Serial))
            print('AtomName = ' + atom.AtomName)
            print('AltLoc = ' + atom.AltLoc)
            print('ResName = ' + atom.ResName)
            print('ChainID = ' + atom.ChainID)
            print('ResSeq = ' + str(atom.ResSeq))
            print('ICode = ' + atom.ICode)
            print('Coords = ' + str(atom.Coords[0]) + ' ' + str(atom.Coords[1]) + ' ' + str(atom.Coords[2]))
            print('Occupancy = ' + str(atom.Occupancy))
            print('OccTemp = ' + str(atom.OccTemp))
            print('Temp = ' + str(atom.Temp))
            print('Element = ' + atom.Element)
            print('Charge = ' + atom.Charge)
            print('ModelNum = ' + str(atom.ModelNum))
            print('ChainNum = ' + str(atom.ChainNum))

        print('')
        print('Models:')
        print(len(mol.FConnections))
        if len(mol.FConnections) > 0:
            conn = mol.FConnections[0]
            print('First Connection:')
            print('AtomID: ' + str(conn.AtomID))
            print('Connects:')
            for f in range(len(conn.Connects)):
                print(conn.Connects[f])

        print('')
        print('AtomCount: ' + str(mol.FAtomCount))
        print('ChainCount: ' + str(mol.FChainCount))
        print('ModelCount: ' + str(mol.FModelCount))


class OpenFile(Frame):

    def __init__(self, parent):
        Frame.__init__(self, parent)

        self.parent = parent

    def openFile(self):
        return self.onOpen()

    def onOpen(self):
        ftypes = [('PDB files', '*.pdb'), ('All files', '*')]
        dlg = filedialog.Open(self, filetypes = ftypes)
        fl = dlg.show()

        if fl != '':
            return self.readFile(fl)

    def readFile(self, filename):
        f = open(filename, "r")
        text = f.read()
        return text


app = App(title="BiggerPython")
app.tk.iconbitmap('chemera.ico')

loadFileButton = PushButton(app, command=loadFile, text="Load PDB file")
loadFileButton_ttp = Tooltip.Tooltip(loadFileButton.tk, text="Load a PDB file from PDBeChem.")

# Load atom and config data
oclconfiguration.DefaultConfig()
oclconfiguration.LoadAtomData()
oclconfiguration.LoadAAData()
FMolecules = pdbmolecules.TPDBModelMan(oclconfiguration.Config.MonomersPath)

app.display()
