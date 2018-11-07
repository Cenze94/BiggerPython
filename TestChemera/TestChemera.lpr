program TestChemera;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, basetypestests, quicksorttests, stringutilstests;

begin
  // StartBasetypesTest();
  // StartQuicksortTest();
  StartStringutilsTest();

  Readln;
end.

