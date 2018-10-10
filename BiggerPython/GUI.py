from guizero import App, PushButton
import Tooltip
import Openfile
import LoadPDB


def loadFile():
    openFile = Openfile.OpenFile(app.tk)
    pdb = openFile.openFile()
    if pdb != "":
        LoadPDB.PDB(pdb)


app = App(title="BiggerPython")
app.tk.iconbitmap('chemera.ico')

loadFileButton = PushButton(app, command=loadFile, text="Load PDB file")
loadFileButton_ttp = Tooltip.Tooltip(loadFileButton.tk, text="Load a PDB file from PDBeChem.")

app.display()
