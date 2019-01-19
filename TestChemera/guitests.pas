unit guitests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, oclconfiguration, chemeramain, pdbmolecules, molecules,
  basetypes, geomutils, bogie, linegrids, dockdomains, molutils, LCLProc;

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

procedure LoadTest();
var mol:TMolecule; atom:TAtom; bond:TAtomBond; f:integer; startTick,endTick:DWORD;

begin
  startTick:=GetTickCount;
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

  endTick:=GetTickCount;
  WriteLn('Execution time: ' + FloatToStr((endTick - startTick) / 1000) + ' seconds');
end;

procedure BiggerTest();
var targetrads,proberads:TFloats;
  targetcoords,probecoords:TCoords;
  targetgrid,probegrid:TDockingGrid;
  domain:TDockDomain;
  tick1,tick2,startTick,endTick:DWORD;
  models:TModelManager;
  f:Integer;
  target,probe:TMolecule;
  MaxIters:Integer;

begin
  target:=FMolecules.LoadLayer('..\PDB\3f6u.pdb');
  probe:=FMolecules.LoadLayer('..\PDB\4a0q.pdb');
  startTick:=GetTickCount;

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

  MaxIters:=1;

  for f:=1 to MaxIters do
    begin
    tick1:=GetTickCount;
    probegrid:=TDockingGrid.Create(1);
    probegrid.BuildFromSpheres(probecoords,proberads);

    domain:=TDockDomain.Create(targetgrid,probegrid,0);
    domain.MinimumOverlap:=models.MinimumOverlap;
    domain.AssignModelManager(@models.AddModel);
    domain.RemoveCores:=True;
    domain.BuildInitialDomain;
    domain.Score;
      writeLn(models.Models[0].OverlapScore,' (',models.Models[0].TransVec[0],',',
              models.Models[0].TransVec[1],',',models.Models[0].TransVec[2],')');
    if f<MaxIters then
      begin
      domain.free;
      probegrid.free;
      end;
    end;
  tick2:=GetTickCount;

  domain.CalcDomainStats;

  WriteLn(FloatToStrF((tick2-tick1)/1000,ffFixed,4,3));
  WriteLn(domain.Size,' cells');
  DebugLn(IntToStr(Length(targetcoords)),' atoms');
  for f:=0 to 20 do
    with models.Models[f] do
      writeLn(OverlapScore,' (',TransVec[0],',',TransVec[1],',',TransVec[2],')');

  endTick:=GetTickCount;
  WriteLn('Execution time: ' + FloatToStr((endTick - startTick) / 1000) + ' seconds');
end;

end.

