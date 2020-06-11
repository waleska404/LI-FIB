:-include(entradaLigas).
symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.


%%%%%% Some helpful definitions to make the code cleaner:
equipo(E) :- numEquipos(N), between(1,N,E).
partido(E1,E2):-equipo(E1), equipo(E2), E1 =\= E2.
numJornadas(M) :- numEquipos(N), M is (N-1)*2.
jornada(J):- numJornadas(M), between(1,M,J).
jornadaIda(J):- jornada(J), numEquipos(N), J < N.
jornadaVuelta(J):- jornada(J), not(jornadaIda(J)).

%%%%%%  1. SAT Variables:
%x(E1,E2,J) equipo E1 y equipo E2 juegan en la jornaad J en casa de E1

satVariable( x(E1,E2,J)):-partido(E1,E2), jornada(J).


%%%%%%  2. Clause generation:

writeClauses:- 
    cadaEquipoJuegaCadaJornada,%todos los equipos deben jugar todas las jornadas
    paridoSolo1Jornada, %no se repiten partidos
    dosVueltas, %partidos de ida i vuelta en el mismo orden
    equipoNoFuera, %el equipo no quiere jugar fuera en una jornada concreta
    equipoNoCasa, %el equipo no quiere jugar en casa en una jornada concreta
    noPartidoEnJornada,
    siPartidoEnJornada,
    noKRepeticiones,
    true,!.                    % this way you can comment out ANY previous line of writeClauses
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.


cadaEquipoJuegaCadaJornada :- equipo(E), jornada(J), findall(x(E,E1,J), partido(E,E1), Lits1), findall(x(E1,E,J),partido(E1,E), Lits2), append(Lits1,Lits2,Lits), exactly(1,Lits), fail.
cadaEquipoJuegaCadaJornada.

paridoSolo1Jornada:- partido(E1,E2), findall(x(E1,E2,J), jornada(J), Lits), atMost(1,Lits), fail.

paridoSolo1Jornada.


dosVueltas:- jornadaIda(J1), numEquipos(N), J2 is J1+N-1, jornadaVuelta(J2), partido(E1,E2), writeClause([x(E2,E1,J2), -x(E1,E2,J1)]), fail.

dosVueltas.


equipoNoFuera:- equipo(E1),jornada(J), nofuera(E1,J), findall(x(E1,E2,J),(equipo(E2), E1 =\= E2), Lits), atLeast(1,Lits), false.

equipoNoFuera.


%restriccionNoFuera :- nofuera(E, J), jornada(J), findall(x(E, E2, J), partido(E, E2), Lits), atLeast(1, Lits), fail.
%restriccionNoFuera.

equipoNoCasa:- nocasa(E,J), jornada(J), findall(x(E1,E,J), partido(E1,E), Lits), atLeast(1,Lits),fail.
equipoNoCasa.

noPartidoEnJornada :- partido(E1,E2), nopartido(E1,E2,J),findall(x(E1,E2,J2),(jornada(J2), J2 \= J), Lits), atLeast(1,Lits),fail.
noPartidoEnJornada.

siPartidoEnJornada :- partido(E1,E2), jornada(J), sipartido(E1,E2,J),writeClause([x(E1,E2,J)]), fail.
siPartidoEnJornada.

kRepeticiones(3).

noKRepeticiones :- kRepeticiones(K), K1 is K-1, equipo(E1), jornada(J1), J2 is J1+K1, jornada(J2),
	findall(x(E1,E2,J3), (between(J1,J2,J3), partido(E1,E2)), Lits),
	atMost(K1, Lits), fail.
noKRepeticiones. 

%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:


displaySol(M):- nl, jornada(J), nl, write('Jornada '), write(J), write(': '),nl, member(x(E1, E2, J), M),
	write(E1), write(' vs. '), write(E2),nl,
	fail.
displaySol(_):-nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything below is given as a standard library, reusable for solving 
%    with SAT many different problems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> or('), write(Lits), write(')'), nl, !. 
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.

% Express that Var is equivalent to the conjunction of Lits:
expressAnd( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> and('), write(Lits), write(')'), nl, !. 
expressAnd( Var, Lits):- member(Lit,Lits), negate(Var,NVar), writeClause([ NVar, Lit ]), fail.
expressAnd( Var, Lits):- findall(NLit, (member(Lit,Lits), negate(Lit,NLit)), NLits), writeClause([ Var | NLits]), !.


%%%%%% Cardinality constraints on arbitrary sets of literals Lits:

exactly(K,Lits):- symbolicOutput(1), write( exactly(K,Lits) ), nl, !.
exactly(K,Lits):- atLeast(K,Lits), atMost(K,Lits),!.

atMost(K,Lits):- symbolicOutput(1), write( atMost(K,Lits) ), nl, !.
atMost(K,Lits):-   % l1+...+ln <= k:  in all subsets of size k+1, at least one is false:
	negateAll(Lits,NLits),
	K1 is K+1,    subsetOfSize(K1,NLits,Clause), writeClause(Clause),fail.
atMost(_,_).

atLeast(K,Lits):- symbolicOutput(1), write( atLeast(K,Lits) ), nl, !.
atLeast(K,Lits):-  % l1+...+ln >= k: in all subsets of size n-k+1, at least one is true:
	length(Lits,N),
	K1 is N-K+1,  subsetOfSize(K1, Lits,Clause), writeClause(Clause),fail.
atLeast(_,_).

negateAll( [], [] ).
negateAll( [Lit|Lits], [NLit|NLits] ):- negate(Lit,NLit), negateAll( Lits, NLits ),!.

negate( -Var,  Var):-!.
negate(  Var, -Var):-!.

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).


%%%%%% main:

main:-  symbolicOutput(1), !, writeClauses, halt.   % print the clauses in symbolic form and halt
main:-  initClauseGeneration,
tell(clauses), writeClauses, told,          % generate the (numeric) SAT clauses and call the solver
tell(header),  writeHeader,  told,
numVars(N), numClauses(C),
write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
shell('cat header clauses > infile.cnf',_),
write('Calling solver....'), nl,
shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
	treatResult(Result),!.

treatResult(20):- write('Unsatisfiable'), nl, halt.
treatResult(10):- write('Solution found: '), nl, see(model), symbolicModel(M), seen, displaySol(M), nl,nl,halt.
treatResult( _):- write('cnf input error. Wrote anything strange in your cnf?'), nl,nl, halt.
    

initClauseGeneration:-  %initialize all info about variables and clauses:
	retractall(numClauses(   _)),
	retractall(numVars(      _)),
	retractall(varNumber(_,_,_)),
	assert(numClauses( 0 )),
	assert(numVars(    0 )),     !.

writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w(-Var):- symbolicOutput(1), satVariable(Var), write(-Var), write(' '),!. 
w( Var):- symbolicOutput(1), satVariable(Var), write( Var), write(' '),!. 
w(-Var):- satVariable(Var),  var2num(Var,N),   write(-), write(N), write(' '),!.
w( Var):- satVariable(Var),  var2num(Var,N),             write(N), write(' '),!.
w( Lit):- told, write('ERROR: generating clause with undeclared variable in literal '), write(Lit), nl,nl, halt.


% given the symbolic variable V, find its variable number N in the SAT solver:
:-dynamic(varNumber / 3).
var2num(V,N):- hash_term(V,Key), existsOrCreate(V,Key,N),!.
existsOrCreate(V,Key,N):- varNumber(Key,V,N),!.                            % V already existed with num N
existsOrCreate(V,Key,N):- newVarNumber(N), assert(varNumber(Key,V,N)), !.  % otherwise, introduce new N for V

writeHeader:- numVars(N),numClauses(C), write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-     retract( numClauses(N0) ), N is N0+1, assert( numClauses(N) ),!.
newVarNumber(N):- retract( numVars(   N0) ), N is N0+1, assert(    numVars(N) ),!.

% Getting the symbolic model M from the output file:
symbolicModel(M):- get_code(Char), readWord(Char,W), symbolicModel(M1), addIfPositiveInt(W,M1,M),!.
symbolicModel([]).
addIfPositiveInt(W,L,[Var|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, varNumber(_,Var,N),!.
addIfPositiveInt(_,L,L).
readWord( 99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ c
readWord(115,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ s
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================
