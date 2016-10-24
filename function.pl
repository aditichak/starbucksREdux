% PART1: ask(Q,A) gives answer A to question related to
% calories of Starbucks drinks. returns false if drink 
% cannot be found.

ask(Q,A) :-
    question(Q,[],Ind,[],C,[],L),
    prove_all(C),
    calories(Ind,L,A).



% PART2: questions (Question,QR,Indect,Q0,Query)

% is true if Query-Q0 provides an answer about Indect to 
% Question-QR. L0-L2 are lists that store possible 
% specifications on the drink (size, milk type, and whip 
% cream)

% Question Type a: 
% "how much sugar/fat/protein in a pumpkin spice latte"
question([how, much | T0],T2,Ind,C0,C2,L0,L2) :-      
    noun_phrase(T0,[is|T1],Ind,C0,C1,L0,L1),
    mp(T1,T2,Ind,C1,C2,L1,L2).

% Question Type b: 
% "how many calories in a tall pumpkin spice latte"
question([how, many, calories| T0],T2,Ind,C0,C2,L0,L2) :-  
    noun_phrase(T0,T1,Ind,C0,C1,L0,L1),
    mp(T1,T2,Ind,C1,C2,L1,L2).



% PART3: noun phrases(T0,T5,Ind,C0,C5,L0,L2).

% is true if words in T0 before T4 form a noun phrase. T4 % is an ending of T0 (difference list.)
% Ind is the type of drink the noun phrase is referring 
% to C0, C5 are relations to be proven. C5-C0 gives
% constrainst on Ind implied by the noun phrase on the 
% drink. L0,L2 are adjective constrainst that are implied
% by the noun phrase on the drink.
% a noun phrase in our case is composed of preposition-
% determiner-adjectives-nouns-modifying clauses. 

noun_phrase(T0,T5,Ind,C0,C5,L0,L2) :-
    prep(T0,T1,Ind,C0,C1),
    det(T1,T2,Ind,C1,C2),
    adjectives(T2,T3,Ind,C2,C3,L0,L1),
    noun(T3,T4,Ind,C3,C4),
    mp(T4,T5,Ind,C4,C5,L1,L2).




% PART4: Prepositions ('in') are ignored in our case

prep([in | T],T,_,C,C).
prep(T,T,_,C,C).



% PART5: Determiners. We specifies determiner to not give
% us constrains either. In other word we only allow users
% to query info of 1 serving of drink. 

det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det(T,T,_,C,C).



% PART6: Adjectives consist of a sequence of adjectives.
% While the argument meanings are the same as noun
% phrases, list of adjectives are properly parsed and
% filled in here. 

adjectives(T0,T2,Ind,C0,C2,L0,L2) :-
    adj(T0,T1,C0,C1,L0,L1),
    adjectives(T1,T2,Ind,C1,C2,L1,L2).
adjectives(T,T,_,C,C,L,L).



% PART7: adj(T0,T1,Ind,C0,C1,L1,L2) 
% arugments have same meanings as Ajectives and noun 
% phrases. is true if the parsed word is a valid adj 
% in the knowledge base.  

adj([Adj| T],T,C,[size(Adj)|C],L,[Adj|L]):-size(Adj). 
adj([Adj| T],T,C,[milk(Adj)|C],L,[Adj|L]):-milk(Adj).
adj([Adj| T],T,C,[whip(Adj)|C],L,[Adj|L]) :- whip(Adj).




% PART8: noun(T0,T1,Ind,C0,C1) is true if T0-T1 is a noun 
% that provides properties C1-C0 to Ind

noun([drink | T],T,Ind,R,[drink(Ind)|R]).
noun([nutrition | T],T,Ind,R,[nutrition(Ind)|R]).

% The following are for proper nouns:

noun([Ind | T],T,Ind,C,C) :- drink(Ind).
noun([Ind | T],T,Ind,C,C) :- nutrition(Ind).



% PART9: modifying phrase is a relation (verb or preposition) followed by a noun_phrase

mp(T0,T2,I1,C0,C2,L0,L2) :-
    reln(T0,T1,I1,I2,C0,C1,L0,L1),
    noun_phrase(T1,T2,I2,C1,C2,L1,L2).
mp(T,T,_,C,C,L,L).



% PART10: reln(T0,T1,I1,I2,R0,R1,L0,L2)  is true if T0-T1 
% is a relation that provides relations R1-R0 and drink 
% specifications L2-L0 on individuals I1 and I2

reln(T,T,_,_,R,R,L,L).



% PART11: calorie queries.

% one drink specification. 
% calories(Ind, [A1], [Y,Z|Cal]) gives back calories of a drink with 1 speicification A1 (whip/size/milk type) in Cal. Answer includes multiple drinks.

calories(Ind,[A1],[Y,Z|Cal]):-calories_in(Ind, A1, Y, Z, Cal).
calories(Ind,[A1],[Y,Z|Cal]):-calories_in(Ind, Y, A1, Z, Cal).
calories(Ind,[A1],[Y,Z|Cal]):-calories_in(Ind, Y, Z, A1, Cal).

% two drink specifications 
% calories(Ind, [A1,A2], [Z|Cal]) gives back calories of a drink with 2 speicification[A1,A2](whip/size/milk type) in Cal. Answers include multiple drinks.

calories(Ind,[A1,A2],[Z|Cal]):-calories_in(Ind, A2, A1, Z, Cal).
calories(Ind,[A1,A2],[Z|Cal]):-calories_in(Ind, A2, Z, A1, Cal).
calories(Ind,[A1,A2],[Z|Cal]):-calories_in(Ind, Z, A2, A1, Cal).
calories(Ind,[A1,A2],[Z|Cal]):-calories_in(Ind, A1, A2, Z, Cal).
calories(Ind,[A1,A2],[Z|Cal]):-calories_in(Ind, A1, Z, A2, Cal).
calories(Ind,[A1,A2],[Z|Cal]):-calories_in(Ind, Z, A1, A2, Cal).


% three drink specifications 
% calories(Ind, [A1,A2], [Z|Cal]) gives back calories of a drink with all speicification[A1,A2,A3] in Cal. Answers include one drink.
calories(Ind,[A1,A2,A3],Cal):-calories_in(Ind, A1, A2, A3, Cal).
calories(Ind,[A1,A2,A3],Cal):-calories_in(Ind, A1, A3, A2, Cal).
calories(Ind,[A1,A2,A3],Cal):-calories_in(Ind, A2, A1, A3, Cal).
calories(Ind,[A1,A2,A3],Cal):-calories_in(Ind, A2, A3, A1, Cal).
calories(Ind,[A1,A2,A3],Cal):-calories_in(Ind, A3, A2, A1, Cal).
calories(Ind,[A1,A2,A3],Cal):-calories_in(Ind, A3, A1, A2, Cal).




% PART12: prove_all(L) proves all elements of L against 
% the database

prove_all([]).
prove_all([H|T]) :-
     H,
    prove_all(T).

% ++++++++++ DATABASE FOR STARBUCKS DRINKS +++++++++++

drink(psl).
drink(cm).

whip(whip).
whip(nowhip).

coffee(psl).
coffee(psl).
coffee(cm).

% coffee sizes
size(short).
size(tall).
size(grande).
size(venti).

% milk type
milk(whole).
milk(partial).
milk(skim).
milk(soy).

% diferrent ingredients
nutrition(calories).
nutrition(sugar).
nutrition(protein).
nutrition(fat).

% calories info
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


calories_in(cm, short, whole, whip, 200).
calories_in(cm, short, partial, whip, 180).
calories_in(cm, short, skim, whip, 150).
calories_in(cm, short, soy, whip, 180).

calories_in(cm, short, whole, nowhip, 140).
calories_in(cm, short, partial, nowhip, 130).
calories_in(cm, short, skim, nowhip, 100).
calories_in(cm, short, soy, nowhip, 130).

calories_in(cm, tall, whole, whip, 300).
calories_in(cm, tall, partial, whip, 270).
calories_in(cm, tall, skim, whip, 230).
calories_in(cm, tall, soy, whip, 270).

calories_in(cm, tall, whole, nowhip, 240).
calories_in(cm, tall, partial, nowhip, 210).
calories_in(cm, tall, skim, nowhip, 170).
calories_in(cm, tall, soy, nowhip, 210).

calories_in(cm, grande, whole, whip, 390).
calories_in(cm, grande, partial, whip, 350).
calories_in(cm, grande, skim, whip, 300).
calories_in(cm, grande, soy, whip, 350).

calories_in(cm, grande, whole, nowhip, 320).
calories_in(cm, grande, partial, nowhip, 280).
calories_in(cm, grande, skim, nowhip, 230).
calories_in(cm, grande, soy, nowhip, 280).

calories_in(cm, venti, whole, whip, 490).
calories_in(cm, venti, partial, whip, 440).
calories_in(cm, venti, skim, whip, 370).
calories_in(cm, venti, soy, whip, 440).

calories_in(cm, venti, whole, nowhip, 420).
calories_in(cm, venti, partial, nowhip, 370).
calories_in(cm, venti, skim, nowhip, 300).
calories_in(cm, venti, soy, nowhip, 370).