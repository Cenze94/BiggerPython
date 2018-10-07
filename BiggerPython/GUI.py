from guizero import App, PushButton
import Tooltip


def loadFile():
    file = "banana"


app = App(title="BiggerPython")
app.tk.iconbitmap('chemera.ico')

loadFileButton = PushButton(app, command=loadFile, text="Load PDB file")
loadFileButton_ttp = Tooltip.Tooltip(loadFileButton.tk, text="Load a PDB file from PDBeChem.")

app.display()
