
% noun_phrase(T0,T4,Ind,C0,C4) is true if
%  T0 and T4 are list of words, such that
%        T4 is an ending of T0
%        the words in T0 before T4 (written T0-T4) form a noun phrase
%  Ind is the individual that the noun phrase is referring to
%  C0 and C4 are lists of relations such that
%        C0 is an ending of C4 and
%        the relations in C4-C0 give the constraints on Ind implied by the noun phrase
% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(T0,T5,Ind,C0,C5) :-
    prep(T0,T1,Ind,C0,C1),
    det(T1,T2,Ind,C1,C2),
    adjectives(T2,T3,Ind,C2,C3),
    noun(T3,T4,Ind,C3,C4),
    mp(T4,T5,Ind,C4,C5).

% Try:
%?- noun_phrase([a,tall,student],T1,I1,[],C1).
%?- noun_phrase([a,math,course],T2,I2,[],C2).
%?- noun_phrase([a,tall,student,enrolled,in,a,math,course],T3,I3,[],C3).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constaints.
det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det(T,T,_,C,C).

% deals with preposition 'in'
prep([in | T],T,_,C,C).
prep(T,T,_,C,C).

% Adjectives consist of a sequence of adjectives.
% The meaning of the arguments is the same as for noun_phrase
adjectives(T0,T2,Ind,C0,C2) :-
    adj(T0,T1,C0,C1),
    adjectives(T1,T2,Ind,C1,C2).
adjectives(T,T,_,C,C).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(T0,T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp([that|T0],T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp(T,T,_,C,C).

% DICTIONARY

% adj(T0,T1,Ind,C0,C1) is true if T0-T1 is an adjective that provides properties C1-C0 to Ind
adj([Adj| T],T,C,[size(Adj)|C]).
adj([Adj| T],T,C,[milk(Adj)|C]).

% noun(T0,T1,Ind,C0,C1) is true if T0-T1 is a noun that provides properties C1-C0 to Ind
noun([drink | T],T,Ind,R,[drink(Ind)|R]).
% The following are for proper nouns:
noun([Ind | T],T,Ind,C,C) :- drink(Ind).

% reln(T0,T1,I1,I2,R0,R1) is true if T0-T1 is a relation
%   that provides relations R1-R0 on individuals I1 and I2
reln([nutritional, value | T],T,I1,I2,C,[nutritional_value(I1,I2)|C]).

% Some Example Queries
% ask noun_phrase([a,computer,science,course],R,Ind,[],C).
% ask noun_phrase([a,tall,student,enrolled,in,a,computer,science,course],R,Ind,[],C).

% question(Question,QR,Indect,Q0,Query) is true if Query-Q0 provides an answer about Indect to Question-QR
question([is | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).
question([who,is | T0],T1,Ind,C0,C1) :-
    mp(T0,T1,Ind,C0,C1).
question([who,is | T0],T1,Ind,C0,C1) :-
    noun_phrase(T0,T1,Ind,C0,C1).
question([how,many | T0],T1,Ind,C0,C1) :-
    noun_phrase(T0,T1,Ind,C0,C1).
question([what,is | T0],T1,Ind,C0,C1) :-
    noun_phrase(T0,T1,Ind,C0,C1).
question([who,is | T0],T1,Ind,C0,C1) :-
    adjectives(T0,T1,Ind,C0,C1).
question([what | T0],T2,Ind,C0,C2) :-      % allows for a "what ... is ..."
    noun_phrase(T0,[is|T1],Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).
question([what | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).


% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A,[],C),
    prove_all(C).

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

drink(psl).

size(short).
size(tall).
size(grande).
size(venti).

milk(whole).
milk(partial).
milk(skim).
milk(soy).

calories_in(psl, short, whole, true, 230).
calories_in(psl, short, partial, true, 210).
calories_in(psl, short, skim, true, 180).
calories_in(psl, short, soy, true, 190).

calories_in(psl, short, whole, false, 170).
calories_in(psl, short, partial, false, 160).
calories_in(psl, short, skim, false, 130).
calories_in(psl, short, soy, false, 140).

calories_in(psl, tall, whole, true, 330).
calories_in(psl, tall, partial, true, 300).
calories_in(psl, tall, skim, true, 260).
calories_in(psl, tall, soy, true, 270).

calories_in(psl, tall, whole, false, 270).
calories_in(psl, tall, partial, false, 240).
calories_in(psl, tall, skim, false, 200).
calories_in(psl, tall, soy, false, 210).

calories_in(psl, grande, whole, true, 420).
calories_in(psl, grande, partial, true, 380).
calories_in(psl, grande, skim, true, 330).
calories_in(psl, grande, soy, true, 350).

calories_in(psl, grande, whole, false, 350).
calories_in(psl, grande, partial, false, 310).
calories_in(psl, grande, skim, false, 260).
calories_in(psl, grande, soy, false, 280).

calories_in(psl, venti, whole, true, 520).
calories_in(psl, venti, partial, true, 470).
calories_in(psl, venti, skim, true, 400).
calories_in(psl, venti, soy, true, 420).

calories_in(psl, venti, whole, false, 440).
calories_in(psl, venti, partial, false, 400).
calories_in(psl, venti, skim, false, 330).
calories_in(psl, venti, soy, false, 350).