unit guitests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, oclconfiguration, chemeramain, pdbmolecules, molecules,
  basetypes, geomutils, bogie, linegrids, dockdomains, molutils;

procedure StartGUITest();
procedure LoadTest();
procedure BiggerTest();

var
  FMolecules:TPdbModelMan;

implementation

procedure StartGUITest();
begin
  LoadAtomData();
  LoadAAData();
  FMolecules:=TPdbModelMan.Create(Config.MonomersPath);

  // LoadTest();
  BiggerTest();
end;

// Replico le istruzioni all'interno del codice eseguito dal pulsante che mi
// interessano
procedure LoadTest();
var mol:TMolecule; atom:TAtom; bond:TAtomBond; f:integer;

begin
  mol:=FMolecules.LoadLayer('../PDB/4a0q.pdb');
  WriteLn('FName: ' + mol.Name);
  WriteLn('FID: ' + IntToStr(mol.ID));
  if mol.Parent <> Nil then
    WriteLn('FParent: ' + mol.Parent.Name)
  else
    WriteLn('FParent: None');

  WriteLn('');
  WriteLn('Atoms:');
  WriteLn(Length(mol.GroupAtoms));
  if Length(mol.GroupAtoms) > 0 then
  begin
    atom:=mol.GroupAtoms[0];
    WriteLn('First Atom:');
    WriteLn('AtomName = ' + atom.Name);
    WriteLn('FID = ' + IntToStr(atom.ID));
    if atom.Parent <> Nil then
      WriteLn('FParent = ' + atom.Parent.Name)
    else
      WriteLn('FParent = None');
    WriteLn('FAtomicNumber = ' + IntToStr(atom.AtomicNumber));
    WriteLn('FCoord = ' + FloatToStr(atom.Coords[0]) + ' ' + FloatToStr(atom.Coords[1]) + ' ' + FloatToStr(atom.Coords[2]));
    WriteLn('FRadius = ' + FloatToStr(atom.Radius));
    WriteLn('FCharge = ' + FloatToStr(atom.Charge));
    WriteLn('FMass = ');
    WriteLn('Tag = ' + IntToStr(atom.Tag));
  end;

  WriteLn('');
  WriteLn('FGroups:');
  WriteLn(Length(mol.Groups));
  for f:=0 to High(mol.Groups) do
  begin
    WriteLn('   ' + mol.Groups[f].Name);
  end;

  WriteLn('');
  WriteLn('FType: ' + mol.MolType);
end;

procedure BiggerTest();
var targetrads,proberads:TFloats;
  targetcoords,probecoords:TCoords;
  targetgrid,probegrid:TDockingGrid;
  domain:TDockDomain;
  tick1,tick2:DWORD;
  models:TModelManager;
  f:Integer;
  target,probe:TMolecule;
  MaxIters:Integer;

begin
  target:=FMolecules.LoadLayer('..\PDB\3f6u.pdb');
  probe:=FMolecules.LoadLayer('..\PDB\4a0q.pdb');

  target.Transform(Simmetric(FindCenter(target)));
  probe.Transform(Simmetric(FindCenter(probe)));

  targetrads:=Add(ListRadii(target),1.4);
  targetcoords:=ListCoords(target);

  proberads:=Add(ListRadii(probe),1.4);
  probecoords:=ListCoords(probe);

  models:=TModelManager.Create(100,300, nil);
  models.GridScale:=1;
  Writeln('Workin...');
  targetgrid:=TDockingGrid.Create(1);
  targetgrid.BuildFromSpheres(targetcoords,targetrads);
  WriteLn(IntToStr(Length(targetgrid.Surf.NonEmpty)));
  WriteLn(IntToStr(Length(targetgrid.Surf.NonEmpty[1])));
  WriteLn(IntToStr(Length(targetgrid.Surf.Grid)));
  WriteLn(IntToStr(Length(targetgrid.Surf.Grid[0])));
  WriteLn(IntToStr(Length(targetgrid.Surf.Grid[0, 0])));
  WriteLn(IntToStr(targetgrid.Surf.Grid[1, 54, 0, 1]));
  WriteLn(IntToStr(Length(targetgrid.Surf.CellCounts)));
  WriteLn(IntToStr(Length(targetgrid.Surf.CellCounts[0])));
  WriteLn(IntToStr(targetgrid.Base.TotalCount));
  WriteLn(IntToStr(targetgrid.Surf.ZMax));
end;

end.

