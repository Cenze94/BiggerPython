unit guitests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, oclconfiguration, chemeramain, pdbmolecules,
  molecules;

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

  LoadTest();
  // BiggerTest();
end;

// Replico le istruzioni all'interno del codice eseguito dal pulsante che mi
// interessano
procedure LoadTest();
var mol:TMolecule; atom:TAtom; bond:TAtomBond; f:integer;

begin
  mol:=FMolecules.LoadLayer('../PDB/3f6u.pdb');
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

  {WriteLn('');
  WriteLn('FBondsTable');
  WriteLn(Length(mol.BondsTable));
  if Length(mol.BondsTable) > 0 then
  begin
    bond:=mol.BondsTable[0];
    WriteLn('First Bond:');
    WriteLn('Atom1: ' + bond.Atom1.Name);
    WriteLn('Atom2: ' + bond.Atom2.Name);
    WriteLn('BondType: ' + IntToStr(bond.BondType));
    WriteLn('Tag: ' + IntToStr(bond.Tag));
  end;}

  WriteLn('');
  WriteLn('FType: ' + mol.MolType);
end;

// Provo ad eseguire direttamente il codice del pulsante, dato che altrimenti il
// codice risulterebbe lungo da riscrivere
procedure BiggerTest();
var main:TCmMainForm;

begin
  main.MenuItem2Click(Nil);
end;

end.

