programa([]):-write("yes").
programa([begin|L]):-append(LS, [end], L), instruccion(LS).


variable(x).
variable(y).
variable(z).

comp([X,=,Y]):- variable(X), variable(Y).

instruccion([]):-write("yes").
instruccion([X,=,Y,+,Z]):- variable(X), variable(Y), variable(Z).
instruccion(L):-append(LS, [;|LX], L), instruccion(LS), instruccion(LX).
instruccion([if|L]):- append(L1, [then|L2], L), comp(L1),
						append(L3, [else|L4], L2), instruccion(L3),
						append(L5, [endif], L4), instruccion(L5),!.
