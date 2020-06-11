
%%%%%% Some helpful definitions to make the code cleaner:

year(Y)    :- numCursos(N), between(1,N,Y).       %% AQUESTA ÉS OBLIGATÒRIA

%% -------- ADAPTA aquestes definicions
%%
%% course(C)            :- numAssignatures(N), between(1,N,C).
%% lectureOfCourse(C,L) :- assig(_,C,N,_,_), between(1,N,L).
%% slot(S)              :- between(1,60,S).
%% room(R)              :- numAules(N), between(1,N,R).
%% teacher(T)           :- numProfes(N), between(1,N,T).


%%%%%%  1. SAT Variables:

%% -------- ADAPTA aquestes SAT Variables
%%
%% % cls(C,L,S) la classe número L de l'assignatura C s'imparte a l'slot S
%% satVariable( cls(C,L,S) ):- course(C), lectureOfCourse(C,L), slot(S).
%% % cr(C,R) l'assignatura C és impartida en l'aula R
%% satVariable( cr(C,R) ):- course(C), room(R).
%% % ct(C,T) l'assignatura C és impartida pel professor T
%% satVariable( ct(C,T) ):- course(C), teacher(T).


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
