program TestChemera;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, basetypestests, quicksorttests, stringutilstests, pdbparsertests,
  geomutilstests, guitests, lazopenglcontext;

begin
  // StartBasetypesTest();
  // StartQuicksortTest();
  // StartStringutilsTest();
  // StartPdbparserTest();
  StartGeomutilsTest();
  // StartGUITest();

  Readln;
end.

