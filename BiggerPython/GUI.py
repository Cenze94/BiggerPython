from guizero import App, PushButton
from tkinter import Frame
from tkinter import filedialog
import Tooltip
import Openfile
import pdbmolecules
import oclconfiguration


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


def loadFile():
    openFile = Openfile.OpenFile(app.tk)
    pdbName = openFile.openFile()
    if pdbName != "":
        mol = FMolecules.LoadLayer(pdbName)
        FMolecules.LayerByIx(0).DeleteWater()
        mol.Transform(Simmetric(FindCenter(mol)))


class OpenFile(Frame):
    def __init__(self, parent):
        Frame.__init__(self, parent)
        self.parent = parent

    def openFile(self):
        return self.onOpen()

    # Define and show open file dialog, return selected file name
    def onOpen(self):
        ftypes = [('PDB files', '*.pdb'), ('All files', '*')]
        dlg = filedialog.Open(self, filetypes=ftypes)
        fl = dlg.show()

        if fl != '':
            return fl
