:-use_module(library('semweb/rdf_db')).
:-use_module(library('semweb/rdfs')).



:- rdf_meta
        trp_base(?,?,?),triple(?,?,?),
		trp_inference(?,?,?),
		class(?),
		property(?),
		individual(?).

dynamic_pred:-
      dynamic trp_store/3,end/0.

%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%


  


%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD
%%%%%%%%%%%%%%%%%%%%%%%

load(File):-rdf_register_ns(ex,'http://www.example.org/example#',[]),rdf_load(File),bottom_up_main.

%%%%%%%%%%%%%%%%%%%%%%%%
% TRIPLE
%%%%%%%%%%%%%%%%%%%%%%%%

triple(X,Y,Z):-trp_store(X,Y,Z).

%%%%%%%%%%%%%%%%%%
% NAMEID
%%%%%%%%%%%%%%%%%%


nameid(X,X):-var(X),!.
nameid(S,S):-S=..[':','rdf'|_],!.
nameid(S,S):-S=..[':','rdfs'|_],!.
nameid(S,S):-S=..[':','owl'|_],!.
nameid(S,NS):-S=..[':'|_],!,rdf_global_id(S,NS).
nameid(S,NS):-S=..[Op|Args],name_list(Args,NArgs),NS=..[Op|NArgs].

%%%%%%%%%%%%%%%%
% NAMELIST
%%%%%%%%%%%%%%%%

name_list([],[]).
name_list([S|RS],[NS|NRS]):-nameid(S,NS),name_list(RS,NRS).



 
%%%%%%%%%%%%%%%%%%
% VALID
%%%%%%%%%%%%%%%%%%%

valid('http://www.w3.org/2002/07/owl#Thing'):-!.
valid(E):-atomic(E),class(E),!.
valid(E):-atomic(E),property(E),!.
valid(E):-atomic(E),individual(E),!.
valid(E):-compound(E),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
class(C):-rdf(C,rdf:type,'http://www.w3.org/2002/07/owl#Class'),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% property
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


property(P):-rdf(P,rdf:type,'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% individual
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


individual(I):-rdf(I,rdf:type,'http://www.w3.org/2002/07/owl#Thing'),!.
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% trp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trp_base(X,rdfs:subClassOf,Y):-
				rdf(X,rdfs:subClassOf,Y).
				 
trp_base('http://www.w3.org/2002/07/owl#Thing',rdfs:subClassOf,forall(inv(X),Y)):-
				rdf(X,rdfs:domain,Y).
				 
trp_base('http://www.w3.org/2002/07/owl#Thing',rdfs:subClassOf,forall(X,Y)):-
				rdf(X,rdfs:range,Y).
				 
trp_base(inter(NewL),rdfs:subClassOf,Z):-
				rdf(X,owl:intersectionOf,List),
				retrieve_list(List,L),
				trp_list(L,NewL),
				rdf(X,rdfs:subClassOf,Z).

trp_base(union(NewL),rdfs:subClassOf,Z):-
				rdf(X,owl:unionOf,List),
				retrieve_list(List,L),
				trp_list(L,NewL),
				rdf(X,rdfs:subClassOf,Z).

trp_base(X,rdfs:subClassOf,Z):-
			 rdf(X,rdfs:subClassOf,D),
			 rdf(D,rdf:type,'http://www.w3.org/2002/07/owl#Restriction'), 
			 new_trp_base(D,Z).

trp_base(A,owl:equivalentClass,B):-rdf(A,owl:equivalentClass,B).

trp_base(X,rdfs:subPropertyOf,Y):-rdf(X,rdfs:subPropertyOf,Y).

trp_base(X,owl:equivalentProperty,Y):-rdf(X,owl:equivalentProperty,Y).

trp_base(X,owl:equivalentProperty,inv(Y)):-rdf(X,owl:inverseOf,Y).

trp_base(X,owl:equivalentProperty,inv(X)):-rdf(X,rdf:type,'http://www.w3.org/2002/07/owl#SymmetricProperty').

trp_base(trans(X),rdfs:subPropertyOf,X):-rdf(X,rdf:type,'http://www.w3.org/2002/07/owl#TransitiveProperty').

trp_base(X,rdf:type,Y):-rdf(X,rdf:type,Y).

trp_base(X,P,Y):-rdf(X,Q,Y),P=Q,rdf_global_id(P,NP),rdf(NP,rdf:type,'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BOTTOM-UP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bottom_up:-retractall(trp_store(_,_,_)),fail.

bottom_up:-trp_base(X,Y,Z),valid(X),valid(Y),valid(Z),assert(trp_store(X,Y,Z)),fail.

bottom_up.

bottom_up_loop:-clause(trp_inference(X,Y,Z),C),call_condition(C),\+trp_store(X,Y,Z),assert(end),assert(trp_store(X,Y,Z)),fail.

bottom_up_loop.

bottom_up_main:-bottom_up,fail.

bottom_up_main:-bottom_up_loop,end,!,retractall(end),bottom_up_loop.

bottom_up_main.

%%%%%%%%%%%%%%%%%%%%%%%%%%
% CALL_CONDITION
%%%%%%%%%%%%%%%%%%%%%%%%%%%


call_condition(C):-C=..[','|Conditions],!,call_list(Conditions).

call_condition(C):-C=trp_base(X,Y,Z),!,trp_store(X,Y,Z).

call_condition(C):-C=trp_inference(X,Y,Z),!,trp_store(X,Y,Z).

call_condition(C):-call(C).

call_list([]).

call_list([C|RC]):-call_condition(C),call_list(RC).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REASONING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trp_inference(E,owl:equivalentClass,F):-trp_base(E,owl:equivalentClass,F).

%Eq1

trp_inference(E,owl:equivalentClass,E):-class(E).

%Eq2



trp_inference(E,owl:equivalentClass,G):- trp_inference(E,owl:equivalentClass,F),E\==F,
					trp_inference(F,owl:equivalentClass,G),F\==G. 

%Eq3

trp_inference(E,owl:equivalentClass,F):- trp_inference(F,owl:equivalentClass,E).

%Eq4

trp_inference(E,owl:equivalentClass,F):-
	trp_inference(E,rdfs:subClassOf,F),
	trp_inference(F,rdfs:subClassOf,E).

%--------------

trp_inference(C,rdfs:subClassOf,D):-trp_base(C,rdfs:subClassOf,D),atomic(D),class(D),compound(C).

trp_inference(C,rdfs:subClassOf,D):-trp_base(C,rdfs:subClassOf,D),atomic(C),class(C),compound(D).

trp_inference(C,rdfs:subClassOf,D):-trp_base(C,rdfs:subClassOf,D),compound(C),compound(D).

trp_inference(C,rdfs:subClassOf,D):-trp_base(C,rdfs:subClassOf,D),atomic(C),atomic(D),class(C),class(D).

%Sub1

trp_inference(C,rdfs:subClassOf,D):- trp_inference(C,owl:equivalentClass,D),C\==D.

%Sub2

trp_inference(C,rdfs:subClassOf,E):- trp_inference(C,rdfs:subClassOf,D),C\==D,trp_inference(D,rdfs:subClassOf,E),D\==E.  

%Sub3

trp_inference(D,rdfs:subClassOf,E):- trp_inference(A,rdfs:subClassOf,E),A=union(L),member(D,L).

%Sub4

trp_inference(E,rdfs:subClassOf,C):- trp_inference(E,rdfs:subClassOf,A),A=inter(L),member(C,L).

%Sub5

trp_inference(inter(LE),rdfs:subClassOf,D):-trp_inference(A,rdfs:subClassOf,D),A=inter(L),member(C1,L),trp_inference(E,rdfs:subClassOf,C1),E\==C1,replace(C1,E,L,LE).

%Sub6

trp_inference(union(LE),rdfs:subClassOf,D):-trp_inference(A,rdfs:subClassOf,D),A=union(L),member(C1,L),trp_inference(E,rdfs:subClassOf,C1),E\==C1,replace(C1,E,L,LE). 

%Sub7

trp_inference(C,rdfs:subClassOf,inter(LE)):- trp_inference(C,rdfs:subClassOf,A),A=inter(L),member(D1,L),trp_inference(D1,rdfs:subClassOf,E),D1\==E,replace(D1,E,L,LE).

%Sub8

trp_inference(hasvalue(Q,O),rdfs:subclassOf,D):- trp_inference(Q,rdfs:subPropertyOf,P),Q\==P,trp_inference(T,rdfs:subClassOf,D),T=hasvalue(P,O).

%Sub9

trp_inference(exists(Q,C),rdfs:subClassOf,D):- trp_inference(Q,rdfs:subPropertyOf,P),Q\==P,trp_inference(T,rdfs:subClassOf,D),T=exists(P,C).

%Sub10

trp_inference(exists(P,E),rdfs:subClassOf,D):- trp_inference(E,rdfs:subClassOf,C),E\==C,trp_inference(T,rdfs:subClassOf,D),
T=exists(P,C).  

%Sub11

trp_inference(C,rdfs:subClassOf,hasvalue(Q,O)):- trp_inference(P,rdfs:subPropertyOf,Q),P\==Q,trp_inference(C,rdfs:subClassOf,T),T=hasvalue(P,O).

%Sub12

trp_inference(C,rdfs:subClassOf,forall(Q,D)):- trp_inference(Q,rdfs:subPropertyOf,P),P\==Q,trp_inference(C,rdfs:subClassOf,T),T=forall(P,D).

%Sub13

trp_inference(C,rdfs:subClassOf,forall(P,E)):- trp_inference(D,rdfs:subClassOf,E),D\==E,atomic(E),trp_inference(C,rdfs:subClassOf,T),T=forall(P,D).  

%----------------------------

trp_inference(A,rdf:type,C):-trp_base(A,rdf:type,C).

%Type1

trp_inference(A,rdf:type,D):-  trp_inference(C,rdfs:subClassOf,D),C\==D,trp_inference(A,rdf:type,C).  					
%Type2
	
trp_inference(A,rdf:type,D):-trp_inference(I,rdfs:subClassOf,D),I=inter(Int),trp_cond(A,Int).

%Type3

trp_inference(A,rdf:type,D):- trp_inference(B,rdfs:subClassOf,D),B=exists(P,C),trp_inference(A,P,B), trp_inference(B,rdf:type,C).

%Type4

trp_inference(A,rdf:type,D):- trp_inference(B,rdfs:subClassOf,D),B=hasvalue(P,O),trp_inference(A,P,O).

%Type5

trp_inference(B,rdf:type,D):- trp_inference(C,rdfs:subClassOf,B),B=forall(P,D),trp_inference(A,P,B),trp_inference(A,rdf:type,C).  

%Type6

trp_inference(A,rdf:type,D):-trp_inference(C,rdfs:subClassOf,B),B=forall(inv(P),D),
				trp_inference(A,P,B),trp_inference(A,rdf:type,C).  

%Type7

trp_inference(A,rdf:type,'http://www.w3.org/2002/07/owl#Thing'):-individual(A).

%---------------------

trp_inference(P,rdfs:subPropertyOf,Q):- trp_base(P,rdfs:subPropertyOf,Q).

%Prop1

trp_inference(P,owl:equivalentProperty,P):-property(P).

trp_inference(inv(P),owl:equivalentProperty,inv(P)):-property(P).

trp_inference(trans(P),owl:equivalentProperty,trans(P)):-property(P).

%Prop2

trp_inference(P,owl:equivalentProperty,R):-trp_inference(P,owl:equivalentProperty,Q),P\==Q,
			trp_inference(Q,owl:equivalentProperty,R),Q\==R. 

%Prop3

trp_inference(P,owl:equivalentProperty,Q):-trp_inference(Q,owl:equivalentProperty,P).

%Prop4

trp_inference(P,owl:equivalentProperty,Q):-trp_inference(P,rdfs:subPropertyOf,Q),trp_inference(Q,rdfs:subPropertyOf,P).

%Prop5

trp_inference(P,rdfs:subPropertyOf,Q):- trp_inference(P,owl:equivalentProperty,Q),P\==Q.

%Prop6

trp_inference(P,rdfs:subPropertyOf,R):-trp_inference(P,rdfs:subPropertyOf,Q),P\==Q,
			trp_inference(Q,rdfs:subPropertyOf,R),Q\==R. 

%Prop7


trp_inference(A,Q,B):- trp_inference(P,rdfs:subPropertyOf,Q),P\==Q,trp_inference(A,P,B).

%Prop8

trp_inference(B,Q,A):- trp_inference(P,rdfs:subPropertyOf,inv(Q)),trp_inference(A,P,B).

%Prop9

trp_inference(B,P,A):- trp_inference(inv(Q),rdfs:subPropertyOf,P),trp_inference(A,Q,B).

%Prop10

trp_inference(A,P,C):- trp_inference(trans(P),rdfs:subPropertyOf,P),trp_inference(A,P,B),trp_inference(B,P,C).

%Prop11

trp_inference(A,P,O):- trp_inference(C,rdfs:subClassOf,H),H=hasvalue(P,O),trp_inference(A,rdf:type,C).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% trp_cond
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trp_cond(_,[]):-!. 

trp_cond(A,[E|RE]):-atomic(E),!,trp_store(A,rdf:type,E),trp_cond(A,RE).

trp_cond(A,[exists(P,C)|RE]):-!,trp_store(A,P,B),trp_store(B,rdf:type,C),trp_cond(A,RE).

trp_cond(A,[hasvalue(P,O)|RE]):-!,trp_store(A,P,O),trp_cond(A,RE).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REPLACE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

replace(X,Y,[Z|L],[Y|L]):-X=Z,!.
replace(X,Y,[Z|L],[Z|L2]):-replace(X,Y,L,L2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMPLEX CLASS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



complex_class(X):-rdf(X,owl:unionOf,_).
complex_class(X):-rdf(X,owl:intersectionOf,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEW trp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


new_trp_base(X,exists(P,D)):-rdf(X,owl:onProperty,P),rdf(X,owl:someValuesFrom,D).

new_trp_base(X,hasvalue(P,D)):-rdf(X,owl:onProperty,P),rdf(X,owl:hasValue,D).

new_trp_base(X,forall(P,D)):-rdf(X,owl:onProperty,P),rdf(X,owl:allValuesFrom,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% trp LIST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


trp_list([],[]).

trp_list([X|L],[X|L2]):-rdf(X,rdf:type,'http://www.w3.org/2002/07/owl#Class'),!,
			trp_list(L,L2).

trp_list([X|L],[NX|L2]):-rdf(X,rdf:type,'http://www.w3.org/2002/07/owl#Restriction'),!,
			new_trp_base(X,NX),
			trp_list(L,L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RETRIEVE LIST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

retrieve_list(List,[X]):-rdf(List,rdf:first,X),rdf(List,rdf:rest,rdf:nil),!.

retrieve_list(List,[X|L]):-			
				rdf(List,rdf:first,X),rdf(List,rdf:rest,List2),retrieve_list(List2,L).


