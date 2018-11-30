import pdbparser


def TPDBReaderTest():
    r = pdbparser.TPDBReader('../PDB/3f6u.pdb')
    print('ChainIDs:')
    print(len(r.FChainIDs))
    for f in range(len(r.FChainIDs)):
        print(r.FChainIDs[f])

    print('')
    print('Atoms:')
    print(len(r.FAtoms))
    if len(r.FAtoms) > 0:
        atom = r.FAtoms[0]
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
    print(len(r.FConnections))
    if len(r.FConnections) > 0:
        conn = r.FConnections[0]
        print('First Connection:')
        print('AtomID: ' + str(conn.AtomID))
        print('Connects:')
        for f in range(len(conn.Connects)):
            print(conn.Connects[f])

    print('')
    print('AtomCount: ' + str(r.FAtomCount))
    print('ChainCount: ' + str(r.FChainCount))
    print('ModelCount: ' + str(r.FModelCount))


TPDBReaderTest()
