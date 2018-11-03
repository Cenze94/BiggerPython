program TestChemera;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, basetypestests;

begin
  StartBasetypesTest();

  Readln;
end.

