from guizero import App, PushButton
from tkinter import Frame
from tkinter import filedialog
import Tooltip
import pdbmolecules
import oclconfiguration


def loadFileCommon():
    openFile = OpenFile(app.tk)
    return openFile.openFile()


def loadFile1():
    global mol1
    pdbName = loadFileCommon()
    if pdbName != "":
        print('Loading file...')
        mol1 = FMolecules.LoadLayer(pdbName)
        print('File loaded.')


def loadFile2():
    global mol2
    pdbName = loadFileCommon()
    if pdbName != "":
        print('Loading file...')
        mol2 = FMolecules.LoadLayer(pdbName)
        print('File loaded.')


def bigger():
    if mol1 is None:
        print('First molecule not loaded.')
    elif mol2 is None:
        print('Second molecule not loaded')
    else:
        print('Bigger')


class OpenFile(Frame):

    def __init__(self, parent):
        Frame.__init__(self, parent)

        self.parent = parent

    def openFile(self):
        return self.onOpen()

    def onOpen(self):
        ftypes = [('PDB files', '*.pdb'), ('All files', '*')]
        dlg = filedialog.Open(self, filetypes=ftypes)
        fl = dlg.show()

        if fl != '':
            return self.readFile(fl)

    def readFile(self, filename):
        f = open(filename, "r")
        return f.name


mol1 = None
mol2 = None
app = App(title="BiggerPython")
app.tk.iconbitmap('chemera.ico')

loadFileButton1 = PushButton(app, command=loadFile1, text="Load the first PDB file")
loadFileButton_ttp = Tooltip.Tooltip(loadFileButton1.tk, text="Load the first PDB file")
loadFileButton2 = PushButton(app, command=loadFile2, text="Load the second PDB file")
BiggerButton = PushButton(app, command=bigger, text='Start Bigger')

# Load atom and config data
oclconfiguration.DefaultConfig()
oclconfiguration.LoadAtomData()
oclconfiguration.LoadAAData()
FMolecules = pdbmolecules.TPDBModelMan(oclconfiguration.Config.MonomersPath)

app.display()
