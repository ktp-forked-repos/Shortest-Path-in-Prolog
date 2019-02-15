%BOMPA REMUS 325CB

%Negare
not(G) :- G,!,fail.
not(_).

%predicatul getColor/3 (Graph,Node,Color), instantiaza al treilea parametru primit: Color, la
%culoarea nodului Node
getColor([V|_],N,C) :- getColor2(V,N,C),!.
getColor2([[N,C]|_],N,C) :- !.
getColor2([_|T],N,C) :- getColor2(T,N,C).

%predicatul contains/2 cu parametrii (E,L), verifica daca elelmentul E se afla in lista L
contains(E, [E|_]).
contains(E, [_|T]) :- contains(E, T).

%predicatul edge/5 cauta muchia [X,Y] in graful G
edge(X, Y, [_,E],LastPath, Visited) :- append(LastPath,Visited,R), contains([X,Y], E),not(contains(Y,R)).

%predicatul lastTerm/3 cu parametrii (L,B,M) intoarce ultimul element al listei M in L si restul listei in B
lastTerm(H,[],[H]).
lastTerm(L,B,[H|T]) :- lastTerm(L,NewB,T),B = [H|NewB],!.

%predicatul intoarce in Path calea cea mai scurta de la nodul From la To din Graph, care respecta Formula 
getPath(From, To, Graph, Formula, Path) :-bfs(Graph,[[From]],To,Paths),search(Graph,Formula,Paths,Path),! .

%predicatul search/4 cu parametrii (Graph,Formula,Paths,Path) instantiaza ultimul parametru: Path la 
%prima (deci cea mai scurta) cale din Paths care respecta Formula
search(G,F,[Path|_],P) :- verify(G,F,Path),P=Path,!.
search(G,F,[_|T],P) :- search(G,F,T,P).

%verificarea formulelor

%formule simple: valid, culoare, future, global, until
verify(_,valid,_).
verify(G,Color,[H|_]) :- getColor(G,H,Color),!.
verify(G,future(C),P) :- containsC(G,C,P).
verify(G,global(C),P) :- containsGlobal(G,C,P).
verify(G,until(C1,C2),P) :- containsUntil(G,C1,C2,P).

%formule complexe: next, and, or, not
verify(G,next(F),[_|T]) :- verify(G,F,T),!.
verify(G,and(F1,F2),P) :- verify(G,F1,P),verify(G,F2,P),!.
verify(G,or(F1,F2),P) :- verify(G,F1,P),!;verify(G,F2,P),!.
verify(G,not(F),P) :- not(verify(G,F,P)),!.

%predicate care ajuta la verificarea formulelor

%predicatul containsC/3 (Graph,Culoare,Path) verifica daca exista un nod de culoare Culoare in 
%calea Path
containsC(G, C, [N|_]) :- getColor(G,N,C), !.
containsC(G, C, [_|T]) :- containsC(G,C, T).

%predicatul containsGlobal/3 (Graph,Culoare,Path) verifica daca toate nodurile din calea Path
%au culoarea Culoare
containsGlobal(G,C,[E]) :- getColor(G,E,C),!.
containsGlobal(G,C,[N|T]) :- getColor(G,N,C),containsGlobal(G,C,T).

%predicatul containsUntil/4 (Graph,Color1,Color2,Path) verifica daca calea Path are toate
%nodurile colorate cu Color1, pana la intalnirea unui nod colorat cu Color2
containsUntil(G,C1,_,[N]) :- getColor(G,N,C1),!.
containsUntil(G,_,C2,[N|_]) :- getColor(G,N,C2),!.
containsUntil(G,C1,C2,[N|T]) :- getColor(G,N,C1),containsUntil(G,C1,C2,T).

%BFS

%predicatul bfs/4 (Graph,Queue,To,Paths) se termina cu succes daca coada Queue este goala (in 
%lista Paths se afla toate caile posibile de la sursa la To), iiar daca Queue este nevida, se
%extrage ultimul drum LastPath din ea pentru a se completa cu vecinii ultimului nod V din drum 
bfs(_,[],_,[]).
bfs(G,Q,To,Paths) :- lastTerm(LastPath,NewQ,Q),lastTerm(V,_,LastPath), gasireSol(G,V,LastPath,NewQ,To,Paths),!.

%predicatul gasireSol/6 (Graph,V,LastPath,Queue,To,Paths) verifica daca calea LastPath este solutie
%(daca se termina cu destinatia To), caz in care se insereaza solutia in Paths si se trece direct
%la extragerea urmatoarei cai din coada prin bfs/4 fara a mai vizita vecinii nodului V de la 
%sfarsitul drumului. In cazul in care calea nu e solutie se viziteaza vecinii nodului V.
gasireSol(G,To,LastPath,Q,To,Paths) :- Paths=[LastPath|NewPath] , bfs(G,Q,To,NewPath),!.
gasireSol(G,V,LastPath,Q,To,Paths) :- visit(G,LastPath,Q,To,[V],Paths),!.

%predicatul visit/6 (Graph,LastPath,Queue,To,Visited,Paths) cauta vecini ai ultimului nod al
%drumului LastPath: V care sa nu fi fost  deja vizitati ca vecini ai lui V (Visited) si care
%sa nu fie in LastPath(am avea noduri duplicate). In cazul in care se gasesc vecini, se continua
%vizitarea iar in caz contrar se extrage urmatorul drum, apelandu-se bfs/4
visit(G,LastPath,Q,To,Visited,Paths) :- lastTerm(V,_,LastPath),edge(V,U,G,LastPath,Visited),append(LastPath,[U],NewPath),
                                        visit(G,LastPath,[NewPath|Q],To,[U|Visited],Paths),!.
visit(G,_,Q,To,_,Paths) :- bfs(G,Q,To,Paths),!.
