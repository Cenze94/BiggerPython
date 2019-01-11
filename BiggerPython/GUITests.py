import pdbmolecules
import oclconfiguration
import geomutils
import molutils
import bogie
import linegrids
import basetypes
import dockdomains

# Tests about loading file and Bigger execution


def LoadTest():
    mol = FMolecules.LoadLayer('../PDB/4a0q.pdb')
    print('FName: ' + mol.FName)
    print('FID: ' + str(mol.FID))
    if mol.FParent is not None:
        print('FParent: ' + mol.FParent.FName)
    else:
        print('FParent: None')

    print('')
    print('Atoms:')
    print(len(mol.FAtoms))
    if len(mol.FAtoms) > 0:
        atom = mol.FAtoms[0]
        print('First Atom:')
        print('AtomName = ' + atom.AtomName)
        print('FID = ' + str(atom.FID))
        if atom.FParent is not None:
            print('FParent = ' + atom.FParent.FName)
        else:
            print('FParent = None')
        print('FAtomicNumber = ' + str(atom.FAtomicNumber))
        print('FCoord = ' + str(atom.FCoord[0]) + ' ' + str(atom.FCoord[1]) + ' ' + str(atom.FCoord[2]))
        print('FRadius = ' + str(atom.FRadius))
        print('FCharge = ' + str(atom.FCharge))
        print('FMass = ' + str(atom.FMass))
        print('Tag = ' + str(atom.Tag))

    print('')
    print('FGroups:')
    print(len(mol.FGroups))
    for f in range(len(mol.FGroups)):
        print('   ' + mol.FGroups[f].FName)

    print('')
    print('FBondsTable')
    print(len(mol.FBondsTable))
    if len(mol.FBondsTable) > 0:
        bond = mol.FBondsTable[0]
        print('First Bond:')
        print('Atom1: ' + bond.Atom1.FName)
        print('Atom2: ' + bond.Atom2.FName)
        print('BondType: ' + str(bond.BondType))
        print('Tag: ' + str(bond.Tag))

    print('')
    print('FType: ' + mol.FType)


def BiggerTest():
    target = FMolecules.LoadLayer('../PDB/3f6u.pdb')
    probe = FMolecules.LoadLayer('../PDB/4a0q.pdb')
    target.Transform(geomutils.Simmetric(molutils.FindCenter(target)))
    probe.Transform(geomutils.Simmetric(molutils.FindCenter(probe)))
    targetrads = geomutils.Add(molutils.ListRadii(target), 1.4)
    targetcoords = molutils.ListCoords(target)
    proberads = geomutils.Add(molutils.ListRadii(probe), 1.4)
    probecoords = molutils.ListCoords(probe)

    models = bogie.TModelManager(100, 300, [])
    models.GridScale = 1
    targetgrid = linegrids.TDockingGrid(1)
    targetgrid.BuildFromSpheres(targetcoords, targetrads)

    domain = None
    tick1 = 0
    MaxIters = 1
    for f in range(1, MaxIters + 1):
        tick1 = basetypes.GetTickCount()
        probegrid = linegrids.TDockingGrid(1)
        probegrid.BuildFromSpheres(probecoords, proberads)

        domain = dockdomains.TDockDomain(targetgrid, probegrid, 0)
        domain.MinimumOverlap = models.FMinOverlap
        domain.AssignModelManager(models.AddModel)
        domain.RemoveCores = True
        domain.BuildInitialDomain()
        domain.Score()
        print(str(models.FModels[0].OverlapScore) + ' (' + str(models.FModels[0].TransVec[0]) + ',' +
              str(models.FModels[0].TransVec[1]) + ',' + str(models.FModels[0].TransVec[2]) + ')')
    tick2 = basetypes.GetTickCount()

    domain.CalcDomainStats()
    print((tick2 - tick1) / 1000)
    print(str(domain.FDomainGrid.Shape.TotalCount) + ' cells')
    print(str(len(targetcoords)) + ' atoms')
    for f in range(len(models.FModels)):
        print(str(models.FModels[f].OverlapScore) + ' (' + str(models.FModels[f].TransVec[0]) + ', ' +
              str(models.FModels[f].TransVec[1]) + ', ' + str(models.FModels[f].TransVec[2]) + ')')
    print('')


oclconfiguration.DefaultConfig()
oclconfiguration.LoadAtomData()
oclconfiguration.LoadAAData()
FMolecules = pdbmolecules.TPDBModelMan(oclconfiguration.Config.MonomersPath)

# LoadTest()
BiggerTest()
