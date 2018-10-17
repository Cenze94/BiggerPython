from guizero import App, PushButton
from tkinter import Frame
from tkinter import filedialog
import Tooltip
import Openfile
import pdbmolecules


def loadFile():
    openFile = Openfile.OpenFile(app.tk)
    pdb = openFile.openFile()
    if pdb != "":
        pdbmolecules.TPDBModels(pdb)


app = App(title="BiggerPython")
app.tk.iconbitmap('chemera.ico')

loadFileButton = PushButton(app, command=loadFile, text="Load PDB file")
loadFileButton_ttp = Tooltip.Tooltip(loadFileButton.tk, text="Load a PDB file from PDBeChem.")

app.display()


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
