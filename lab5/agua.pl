%“Hacer Aguas”: disponemos de un grifo de agua, un cubo de 5 litros y otro de 8 litros. Se
%puede verter el contenido de un cubo en otro, llenar un cubo, o vaciar un cubo del todo, y queremos
%saber la secuencia mı́nima de operaciones para obtener exactamente 4 litros de agua en el cubo de 8
%litros.


%unPaso([cubo5, cubo8], [cubo5'], [cubo8']).
unPaso([_,M],[5,M]).
unPaso([N,_],[N,8]).
unPaso([_,M],[0,M]).
unPaso([N,_],[N,0]).
unPaso([N,M],[N2,M2]):-
	AUX is min(N, 8-M), %la cantidad que vamos a verter en el cubo de 8
	M2 is M+AUX, 
	M2 =< 8, %llenar el cubo de 8
	N2 is N-AUX, 
	N2 >= 0. %vaciar cubo de 5
	
unPaso([N,M],[N2,M2]):-
	AUX is min(M, 5-N), %la cantidad que vamos a verter en el cubo de 8
	N2 is N+AUX, 
	N2 =< 5, %llenar el cubo de 8
	M2 is M-AUX, 
	M2 >= 0. %vaciar cubo de 5

%camino( Estado Actual, Estado Final, CaminoHastaAhora, CaminoTotal).
camino(E,E,C,C).
camino(EstadoActual,EstadoFinal,CaminoHastaAhora,CaminoTotal):-
unPaso(EstadoActual,EstSiguiente),
\+member(EstSiguiente,CaminoHastaAhora),
camino(EstSiguiente,EstadoFinal,[EstSiguiente|CaminoHastaAhora],
	CaminoTotal).


nat(0).
nat(N):- nat(N1), N is N1 + 1.

solucionOptima:-
	nat(N),
	camino([0,0],[0,4],[[0,0]],C),
	length(C,N),
	write(C).