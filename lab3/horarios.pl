:-include(entradaHoraris1).
symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.

%% Example input:
%numCursos(4).
%numAssignatures(23).
%numAules(3).
%numProfes(5).

%%%%%% Some helpful definitions to make the code cleaner:
day(D)					:- between(1,5,D). %un dia de la semana
hour(H)					:- between(8,20,H).
hour1(H)				:- between(1,12,H).
year(Y)					:- numCursos(N), between(1,N,Y). %un año = 2ndo de batch

slot(S)              	:- between(1,60,S). %horas en formato semnal: Monday slots [1,..,12], Tuesday [13,..24]
slotOfDay(D,S)			:- hour1(H), S is (D-1)*12 + H. %dado un dia calcula su slot
lastSlotOfDay(D,S)		:- day(D), S is D*12.
room(R)              	:- numAules(N), between(1,N,R). %aula

teacher(T)           	:- numProfes(N), between(1,N,T). %profe
teacherForbHours(T,HP)		:- horesProhibides(T, HP).
teacherForbSingleHour(T,HP) :- teacherForbHours(T, HPS), member(HP, HPS).

subject(C)				:- numAssignatures(N), between(1,N,C). %asignatura
lectureOfSubject(C,L) 	:- assig(_,C,N,_,_), between(1,N,L). %sesion de una asignatura
subjectYear(C,Y)		:- assig(Y,C,_,_,_), year(Y). %el año (curso) de un asignatura
subjectHours(C,H)		:- assig(_,C,H,_,_). %las horas semanales que tiene que dar una asignatura
subjectRooms(C,R)		:- assig(_,C,_,R,_). % las posibles aulas para la asignatura
subjectSingleRoom(C,R)	:- subjectRooms(C, RS), member(R, RS).
subjectTeachers(C,T)	:- assig(_,C,_,_,T). % los posiles prifesores para la asignatura
subjectSingleTeacher(C,T):- subjectTeachers(C, TS), member(T, TS).








%%%%%%  1. SAT Variables:

% cls(C,L,S) la classe(sesion) número L de l'assignatura C s'imparte a l'slot S
satVariable( cls(C,L,S) ):- subject(C), lectureOfSubject(C,L), slot(S).

% cdh(C,D,H) meaning "subject C taught on day D, hour H"
%satVariable( cdh(C,D,H) ):- subject(C), day(D), hour(H).

% cr(C,R) l'assignatura C és impartida en l'aula R
satVariable( cr(C,R) ):- subject(C), room(R).

% ys(Y,S) el año y tiene clase en el slot S.	
satVariable( ys(Y,S) ):- year(Y), slot(S).

% cs(C,S) l'assignatura C és impartida en el slot S
satVariable( cs(C,S) ):- subject(C), slot(S).

% ct(C,T) l'assignatura C és impartida pel professor T
satVariable( ct(C,T) ):- subject(C), teacher(T).

% ct(C,T) l'assignatura C és impartida pel professor T
satVariable( ts(T, S) ):- teacher(T), slot(S).

% ct(C,D) l'assignatura C és impartida el dia Dasignatura
%satVariable( cd(C,D) ):- subject(C), day(D).

%cdhp(C,D,HTP) meaning "subject C taught on day D, hour H and professor P"
%satVariable( cdhp(C,D,H,T) ):- subject(C), day(D), hour(H), teacher(T).

%cdhr(C,D,H,R) meaning "subject C taught on day D, hour H and room R"
%satVariable( cdhr(C,D,H,R) ):- subject(C), day(D), hour(H), room(R). 

%%%%%%  2. Clause generation:

writeClauses:-
	
	oneSlotPerLecture, %cada sesion de cada asig correspondera a un solo numero del 1 al 60
	oneLecturePerSlot,
	oneLecturePerDay, %cada asignatura solo puede hacer una clase al dia
	subjSameTeacher, %todas las sesiones de una asig tienen el mismo profe
	subjSameRoom, %todas las sesiones de una asig se imparten en la misma aula
	roomOneLecture, %no pueden darse dos clases a la vez en la misma aula
	teacherOneLecture, %un profe no puede impartir dos clases a la vez
	sixHperDay, %un curso no puede dar mas de 6h de clase al dia
	noSolaping, %dos clases de un mismo curso no pueden solaparse
	profeHlibres, %un profe no puede dar clase en sus horas libres
	defineTs,
	defineCs,
	defineYs,
	horasCompactas, %en un curso no puede haber horas libres entre clases

    true,!.                    % this way you can comment out ANY previous line of writeClauses
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Definitions of variables that we used in our program:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

defineCs:- subject(C), slot(S),findall(cls(C, L, S), lectureOfSubject(C,L), List), expressOr(cs(C, S), List), fail.
defineCs.

defineYs:- year(Y), slot(S), findall(cls(C, L, S),(subjectYear(C, Y),lectureOfSubject(C,L)),List), expressOr(ys(Y,S), List), fail.
defineYs.

defineTs:- teacher(T), slot(S), findall(cs(C, S), subjectSingleTeacher(C, T), List), expressOr(ts(T,S), List), fail.
defineTs.



oneSlotPerLecture:- subject(C), lectureOfSubject(C, L), findall( cls(C, L, S), slot(S), List), exactly(1, List), fail.
oneSlotPerLecture.

oneLecturePerSlot:- year(Y),slot(S), findall( cls(C, L, S),(lectureOfSubject(C, L),subjectYear(C, Y)), List), atMost(1, List), fail.
oneLecturePerSlot.

oneLecturePerDay:- subject(C), day(D), findall( cls(C,L,S), (slotOfDay(D,S), lectureOfSubject(C,L)), List), atMost(1, List), fail.
oneLecturePerDay.

subjSameTeacher:- subject(C), subjectTeachers(C, T), findall(ct(C, Teacher), member(Teacher, T), List), exactly(1, List), fail.
subjSameTeacher.

subjSameRoom:- subject(C), subjectRooms(C, R), findall(cr(C, Room), member(Room, R), List), exactly(1, List), fail.
subjSameRoom.

roomOneLecture:- room(R),slot(S), findall( cls(C, L, S),(lectureOfSubject(C, L),subjectSingleRoom(C, R)), List), atMost(1, List), fail.
roomOneLecture.


teacherOneLecture:- teacher(T), slot(S), subject(C1), subject(C2), subjectTeachers(C1, T1), subjectTeachers(C2, T2),
C1 \= C2, member(T, T1), member(T, T2), atMost(3, [cs(C1, S), cs(C2, S), ct(C1, T), ct(C2, T)]), fail.
teacherOneLecture.

sixHperDay:- year(Y), day(D), findall(ys(Y, S), slotOfDay(D, S), List), atMost(6, List), fail.
sixHperDay.

noSolaping:- year(Y), day(D), slotOfDay(D,S), findall( cls(C, L, S), subjectYear(C, Y), 
lectureOfSubject(C, L), List), atMost(1, List), fail.
noSolaping.

profeHlibres:- teacher(T), slot(S), teacherForbSingleHour(T, S), writeClause([-ts(T,S)]), fail.
profeHlibres.

horasCompactas:- year(Y), slot(S), day(D), slotOfDay(D, S), lastSlotOfDay(D, LS), slot(S1), between(S, LS, S1), slot(S2), 
between(S1,LS,S2), slot(K), between(S1,S2,K), writeClause([-ys(Y,S1), -ys(Y,S2), ys(Y,K)]), fail.
horasCompactas.

%writeClause([-ys(Y,S1), -ys(Y,S2), ys(Y,H)])
%horarioCompacto :- curso(C), slot(S), dia(D), diaSesion(D,S), ultimoSlotDia(SM,D),slot(S1), between(S,SM,S1), slot(S2), 
%between(S1,SM, S2),slot(H), between(S1,S2,H), writeClause([-cs(C,S1), -cs(C,S2), cs(C,H)]),fail.


%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:

%% -------- ADAPTA les SAT Variables cls(C,L,S), cr(C,R), ct(C,T)

extraBlank(N):- 
    N < 10, !, write(' ').
extraBlank(_).

drawTail(Y, Hour):-
    Hour > 48, 
    write('  Curs: '), write(Y), nl.
drawTail(_, _).

drawCell(Y, S, M):-
    member(cls(C,L,S), M),                   %% -------- ADAPTA la SAT variable cls(C,L,S)
    assig(Y, C, _, _, _), !,
    write(' '), extraBlank(C), write(C), write(' - '),
    extraBlank(L), write(L), 
    write('  ['), member(cr(C,R), M),        %% -------  ADAPTA la SAT variable cr(C,R)
    write('A:'), extraBlank(R), write(' '), write(R), write(']'),
    write('  ['), member(ct(C,T), M),        %% -------  ADAPTA la SAT variable ct(C,T)
    write('P:'), extraBlank(T), write(' '), write(T), write(']'),
    write(' ').
drawCell(_, _, _):- 
    write('                           ').    

drawRow(Row, _):-
    1 is Row mod 2,
    H is Row // 2 + 8, 
    extraBlank(H), 
    write(' '), write(H), write(':00 '), 
    between(1, 141, _), write('='), 
    fail.
drawRow(Row, _):-
    1 is Row mod 2, !, nl.

drawRow(Row, M):-
    year(Y),
    write('       |'),
    between(0, 4, Day), 
    Hour is Row // 2 + Day * 12,
    drawCell(Y, Hour, M), 
    write('|'), 
    drawTail(Y, Hour), 
    fail.
drawRow(_, _).

drawHeader:-
    nl, nl, 
    write(' Format de sortida: Assignatura - Hora [A: Aula] [P: Professor]'), 
    nl, nl, 
    write('                 Dilluns                     Dimarts                     dimecres                     Dijous                    Divendres').

displaySchedule(M):-
    drawHeader, nl,
    between(1, 25, Row), 
    drawRow(Row, M), 
    fail.

drawHeaderYear(Y):-
    nl, nl, 
    write('----------------------------------------------------------------------------------------------------------------------------------------------------'),
    nl,
    write(' Horari del curs '), write(Y),
    nl,
    write(' Format de sortida: Assignatura - Hora [A: Aula] [P: Professor]'), 
    nl, nl, 
    write('                 Dilluns                     Dimarts                     dimecres                     Dijous                    Divendres').

drawTailYear(Hour):-
    Hour > 48, nl.
drawTailYear(_, _).

drawRowYear(Row, _, _):-
    1 is Row mod 2,
    H is Row // 2 + 8, 
    extraBlank(H), 
    write(' '), write(H), write(':00 '), 
    between(1, 141, _), write('='), 
    fail.
drawRowYear(Row, _, _):-
    1 is Row mod 2, !, nl.
drawRowYear(Row, Y, M):-
    write('       |'),
    between(0, 4, Day), 
    Hour is Row // 2 + Day * 12,
    drawCell(Y, Hour, M), 
    write('|'),
    drawTailYear(Hour), 
    fail.
drawRowYear(_, _, _).

displayScheduleYear(Y,M):-
    drawHeaderYear(Y), nl,
    between(1, 25, Row), 
    drawRowYear(Row, Y, M), 
    fail.

displaySol(M):- displaySchedule(M), fail.
displaySol(M):- year(Y), displayScheduleYear(Y,M), fail.
displaySol(_).


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
