program TestChemera;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, basetypestests, quicksorttests, stringutilstests, pdbparsertests;

begin
  // StartBasetypesTest();
  // StartQuicksortTest();
  // StartStringutilsTest();
  StartPdbparserTest();

  Readln;
end.

