unit pdbparsertests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pdbparser;

procedure StartPdbparserTest();
procedure TPDBReaderTest();

implementation

procedure StartPdbparserTest();
begin
  TPDBReaderTest();
end;

procedure TPDBReaderTest();
var r:TPDBReader; f:integer; atom:TPDBAtom;

begin
  r := TPDBReader.Create('../PDB/3f6u.pdb');

  // Le catene sono poche, quindi posso elencarle
  WriteLn('ChainIDs:');
  WriteLn(Length(r.ChainIDs));
  for f:=0 to High(r.ChainIDs) do
  begin
    WriteLn(r.ChainIDs[f]);
  end;

  // Gli atomi sono molti, quindi stampo la lunghezza dell'array e il contenuto
  // della prima cella
  WriteLn('');
  WriteLn('Atoms:');
  WriteLn(Length(r.Atoms));
  atom := r.Atoms[0];
  WriteLn('First Atom:');
  WriteLn('IsHet = ' + BoolToStr(atom.IsHet));
  WriteLn('Serial = ' + IntToStr(atom.Serial));
  WriteLn('AtomName = ' + atom.AtomName);
  WriteLn('AltLoc = ' + atom.AltLoc);
  WriteLn('ResName = ' + atom.ResName);
  WriteLn('ChainID = ' + atom.ChainID);
  WriteLn('ResSeq = ' + IntToStr(atom.ResSeq));
  WriteLn('ICode = ' + atom.ICode);
  WriteLn('Coords = ' + FloatToStr(atom.Coords[0]) + ' ' +
  FloatToStr(atom.Coords[1]) + ' ' + FloatToStr(atom.Coords[2]));
  WriteLn('Occupancy = ' + FloatToStr(atom.Occupancy));
  WriteLn('OccTemp = ' + FloatToStr(atom.OccTemp));
  WriteLn('Temp = ' + FloatToStr(atom.Temp));
  WriteLn('Element = ' + atom.Element);
  WriteLn('Charge = ' + atom.Charge);
  WriteLn('ModelNum = ' + IntToStr(atom.ModelNum));
  WriteLn('ChainNum = ' + IntToStr(atom.ChainNum));

  WriteLn('');
  WriteLn('Connections:');
  WriteLn(Length(r.Connections));

  WriteLn('');
  WriteLn('AtomCount: ' + IntToStr(r.AtomCount));
  WriteLn('ChainCount: ' + IntToStr(r.ChainCount));
  WriteLn('ModelCount: ' + IntToStr(r.ModelCount));
  // Info non viene impostato direttamente da questa classe, quindi non è
  // possibile stamparne i valori
end;

end.

