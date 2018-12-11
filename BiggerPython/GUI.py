from guizero import App, PushButton, Text
from tkinter import Frame
from tkinter import filedialog
import pdbmolecules
import oclconfiguration
import geomutils
import molutils


def loadFileCommon():
    openFile = OpenFile(app.tk)
    return openFile.openFile()


def loadFile1():
    global target
    pdbName = loadFileCommon()
    if pdbName != "":
        textLoadButton1.text_color = 'black'
        textLoadButton1.value = 'Loading file...'
        target = FMolecules.LoadLayer(pdbName)
        textLoadButton1.text_color = 'green'
        textLoadButton1.value = 'File loaded.'


def loadFile2():
    global probe
    pdbName = loadFileCommon()
    if pdbName != "":
        textLoadButton2.text_color = 'black'
        textLoadButton2.value = 'Loading file...'
        probe = FMolecules.LoadLayer(pdbName)
        textLoadButton2.text_color = 'green'
        textLoadButton2.value = 'File loaded.'


def bigger():
    global textBiggerButton
    if target is None:
        textBiggerButton.text_color = 'red'
        textBiggerButton.value = 'First molecule not loaded.'
    elif probe is None:
        textBiggerButton.text_color = 'red'
        textBiggerButton.value = 'Second molecule not loaded.'
    else:
        textBiggerButton.text_color = 'black'
        textBiggerButton.value = 'Starting Bigger...'

    target.Transform(geomutils.Simmetric(molutils.FindCenter(target)))
    probe.Transform(geomutils.Simmetric(molutils.FindCenter(probe)))
    targetrads = geomutils.Add(molutils.ListRadii(target), 1.4)
    targetcoords = molutils.ListCoords(target)
    proberads = geomutils.Add(molutils.ListRadii(probe), 1.4)
    probecoords = molutils.ListCoords(probe)


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


target = None
probe = None
app = App(title="BiggerPython")
app.tk.iconbitmap('chemera.ico')

loadFileButton1 = PushButton(app, command=loadFile1, text="Load the first PDB file", width=19)
textLoadButton1 = Text(app, text='Missing file.', color='red', height=2)
loadFileButton2 = PushButton(app, command=loadFile2, text="Load the second PDB file", width=19)
textLoadButton2 = Text(app, text='Missing file.', color='red', height=2)
biggerButton = PushButton(app, command=bigger, text='Start Bigger', width=19)
textBiggerButton = Text(app, text='', height=2)

# Load atom and config data
oclconfiguration.DefaultConfig()
oclconfiguration.LoadAtomData()
oclconfiguration.LoadAAData()
FMolecules = pdbmolecules.TPDBModelMan(oclconfiguration.Config.MonomersPath)

app.display()
