unit quicksorttests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, quicksort, basetypes;

  procedure StartQuicksortTest();
  procedure QSIndexAsRankingTest();
  procedure QSIntAsValsTest();
  procedure QSAscendingIndexTest();
  procedure QSZeroBasedIndexTest();
  procedure QSSortedTest();

implementation

procedure StartQuicksortTest();
begin
  // QSIndexAsRankingTest();
  // QSIntAsValsTest();
  QSAscendingIndexTest();
  // QSZeroBasedIndexTest();
  // QSSortedTest();
end;

procedure QSIndexAsRankingTest();
begin

end;

procedure QSIntAsValsTest();
begin

end;

// Qui viene verificata anche la funzione Quick, che sarebbe interna a
// QSAscendingIndex. Il test riporta che il metodo non è corretto con array
// che non sono già ordinati, non so se in pratica una caso del genere possa
// capitare all'interno dell'applicazione
procedure QSAscendingIndexTest();
var Ixs1, Ixs2, Ixs3:TIntegers; x:integer;

begin
  AddToArray(5, Ixs1);
  AddToArray(6, Ixs1);
  AddToArray(7, Ixs1);
  AddToArray(2, Ixs1);
  AddToArray(11, Ixs1);
  Ixs1 := QSAscendingIndex(Ixs1);
  for x:=0 to High(Ixs1) do
  begin
    WriteLn(Ixs1[x]);
  end;
  WriteLn('');

  AddToArray(9, Ixs2);
  AddToArray(8, Ixs2);
  AddToArray(5, Ixs2);
  AddToArray(2, Ixs2);
  AddToArray(1, Ixs2);
  Ixs2 := QSAscendingIndex(Ixs2);
  for x:=0 to High(Ixs2) do
  begin
    WriteLn(Ixs2[x]);
  end;
  WriteLn('');

  AddToArray(5, Ixs3);
  AddToArray(6, Ixs3);
  AddToArray(7, Ixs3);
  AddToArray(9, Ixs3);
  AddToArray(11, Ixs3);
  Ixs3 := QSAscendingIndex(Ixs3);
  for x:=0 to High(Ixs3) do
  begin
    WriteLn(Ixs3[x]);
  end;
end;

procedure QSZeroBasedIndexTest();
begin

end;

procedure QSSortedTest();
begin

end;

end.

