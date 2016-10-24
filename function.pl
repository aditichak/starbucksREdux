
noun_phrase(T0,T5,Ind,C0,C5,L0,L2) :-
    prep(T0,T1,Ind,C0,C1),
    det(T1,T2,Ind,C1,C2),
    adjectives(T2,T3,Ind,C2,C3,L0,L1),
    noun(T3,T4,Ind,C3,C4),
    mp(T4,T5,Ind,C4,C5,L1,L2).


% Determiners do not provide any extra constaints.
det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det(T,T,_,C,C).

% deals with preposition 'in'
prep([in | T],T,_,C,C).
prep(T,T,_,C,C).

% Adjectives 
adjectives(T0,T2,Ind,C0,C2,L0,L2) :-
    adj(T0,T1,C0,C1,L0,L1),
    adjectives(T1,T2,Ind,C1,C2,L1,L2).
adjectives(T,T,_,C,C,L,L).

% mp for '...that' and 'how many ... are'
mp(T0,T2,I1,C0,C2,L0,L2) :-
    reln(T0,T1,I1,I2,C0,C1,L0,L1),
    noun_phrase(T1,T2,I2,C1,C2,L1,L2).
mp([that|T0],T2,I1,C0,C2,L0,L2) :-
    reln(T0,T1,I1,I2,C0,C1,L0,L1),
    noun_phrase(T1,T2,I2,C1,C2,L1,L2).
mp(T,T,_,C,C,L,L).

% reln(T0,T1,I1,I2,R0,R1,L0,L2) 
%reln([calories | T],T,I1,I2,C,[calories_in(I1,I2,I3,I4,I5)|C],_,_).
reln(T,T,_,_,R,R,L,L).

% DICTIONARY

% adj(T0,T1,Ind,C0,C1)
adj([Adj| T],T,C,[size(Adj)|C],L,[Adj|L]):-size(Adj). 
adj([Adj| T],T,C,[milk(Adj)|C],L,[Adj|L]):-milk(Adj).
adj([Adj| T],T,C,[whip(Adj)|C],L,[Adj|L]) :- whip(Adj).
adj([Adj| T],T,C,C,L,L):-drink(Adj). 


% noun(T0,T1,Ind,C0,C1) is true if T0-T1 is a noun that provides properties C1-C0 to Ind
noun([drink | T],T,Ind,R,[drink(Ind)|R]).
noun([nutrition | T],T,Ind,R,[nutrition(Ind)|R]).

% The following are for proper nouns:
noun([Ind | T],T,Ind,C,C) :- drink(Ind).
noun([Ind | T],T,Ind,C,C) :- nutrition(Ind).


% calories query
calories(Ind,[A1,A2,A3],Cal):-calories_in(Ind, A1, A2, A3, Cal).
calories(Ind,[A1,A2,A3],Cal):-calories_in(Ind, A1, A3, A2, Cal).
calories(Ind,[A1,A2,A3],Cal):-calories_in(Ind, A2, A1, A3, Cal).
calories(Ind,[A1,A2,A3],Cal):-calories_in(Ind, A2, A3, A1, Cal).
calories(Ind,[A1,A2,A3],Cal):-calories_in(Ind, A3, A2, A1, Cal).
calories(Ind,[A1,A2,A3],Cal):-calories_in(Ind, A3, A1, A2, Cal).


% "how much sugar/fat/protein/sodium is in a pumpkin spice latte"

question([how, much | T0],T2,Ind,C0,C2,L0,L2) :-      
    noun_phrase(T0,[is|T1],Ind,C0,C1,L0,L1),
    mp(T1,T2,Ind,C1,C2,L1,L2).

% "how many calories are in a tall pumpkin spice latte"
question([how, many, calories| T0],T2,Ind,C0,C2,L0,L2) :-  
    noun_phrase(T0,T1,Ind,C0,C1,L0,L1),
    mp(T1,T2,Ind,C1,C2,L1,L2).

question([what | T0],T2,Ind,C0,C2,L0,L2) :-
    noun_phrase(T0,T1,Ind,C0,C1,L0,L1),
    mp(T1,T2,Ind,C1,C2,L1,L2).


% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],Ind,[],C,[],L),
    prove_all(C),
    calories(Ind,L,A).

% prove_all(L) proves all elements of L against the database
prove_all([]).
prove_all([H|T]) :-
     H,
    prove_all(T).

%  The Database of Facts to be Queried

drink(psl).

whip(whip).
whip(nowhip).

ingredient(whip).
ingredient(sugar).

coffee(psl).

size(short).
size(tall).
size(grande).
size(venti).

milk(whole).
milk(partial).
milk(skim).
milk(soy).

nutrition(calories).
nutrition(sugar).
nutrition(protein).
nutrition(fat).

calories_in(psl, short, whole, whip, 230).
calories_in(psl, short, partial, whip, 210).
calories_in(psl, short, skim, whip, 180).
calories_in(psl, short, soy, whip, 190).

calories_in(psl, short, whole, nowhip, 170).
calories_in(psl, short, partial, nowhip, 160).
calories_in(psl, short, skim, nowhip, 130).
calories_in(psl, short, soy, nowhip, 140).

calories_in(psl, tall, whole, whip, 330).
calories_in(psl, tall, partial, whip, 300).
calories_in(psl, tall, skim, whip, 260).
calories_in(psl, tall, soy, whip, 270).

calories_in(psl, tall, whole, nowhip, 270).
calories_in(psl, tall, partial, nowhip, 240).
calories_in(psl, tall, skim, nowhip, 200).
calories_in(psl, tall, soy, nowhip, 210).

calories_in(psl, grande, whole, whip, 420).
calories_in(psl, grande, partial, whip, 380).
calories_in(psl, grande, skim, whip, 330).
calories_in(psl, grande, soy, whip, 350).

calories_in(psl, grande, whole, nowhip, 350).
calories_in(psl, grande, partial, nowhip, 310).
calories_in(psl, grande, skim, nowhip, 260).
calories_in(psl, grande, soy, nowhip, 280).

calories_in(psl, venti, whole, whip, 520).
calories_in(psl, venti, partial, whip, 470).
calories_in(psl, venti, skim, whip, 400).
calories_in(psl, venti, soy, whip, 420).

calories_in(psl, venti, whole, nowhip, 440).
calories_in(psl, venti, partial, nowhip, 400).
calories_in(psl, venti, skim, nowhip, 330).
calories_in(psl, venti, soy, nowhip, 350).