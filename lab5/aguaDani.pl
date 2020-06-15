%unPaso([cubo5, cubo8], [cubo5', cubo8']).
unPaso([_,M],[5,M]). %omplo el cub de 5 litres
unPaso([N,_],[N,8]). %omplo el cub de 8 litres.
unPaso([_,M],[0,M]). %vaciar el cubo de 5 litros.
unPaso([N,_],[N,0]). %vaciar el cubo de 8 litros.
unPaso([N,M],[J,K]) :- %vaciar el cubo de 5 en el de 8
                A is min(N, 8-M),%lo que le falta al cubo de 8 para llenarse
                K is M+A, %llenamos el cubo
                J is N - A, %queda el restante en el cubo de 5
                K =< 8,
                J >= 0.
unPaso([N,M], [J,K]) :- %vaciar el cubo de 8 en el de 5
                A is min(5,5-N),
                J is N+A,
                K is M-A,
                J =< 5,
                K >= 0.
                

%camino(EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal).
camino( E,E, C,C ).
camino( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):- 
                                unPaso( EstadoActual, EstSiguiente ), 
                                \+member(EstSiguiente,CaminoHastaAhora), 
                                camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal ).

nat(0).
nat(N) :- nat(N1), N is N1 +1.

solucionOptima:-
                nat(N), % Buscamos soluci´on de "coste" 0; si no, de 1, etc.
                camino([0,0],[0,4],[[0,0]],C), % En "hacer aguas": -un estado es [cubo5,cubo8], y
                length(C,N), % -el coste es la longitud de C.
                reverse(C,CR),
                write(CR).

 