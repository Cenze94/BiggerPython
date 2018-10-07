from tkinter import Frame
from tkinter import filedialog


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
