% Onur Sasmaz - CS381 HW5
% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parent(Y,X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- female(X), parent(X,_).
isFather(X) :- male(X), parent(X,_).

% 3. Define a predicate `grandparent/2`.
grandparent(X,Z) :- parent(X,Y), parent(Y,Z).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
% There is Dublicate of bart!!
% Source:https://www.csee.umbc.edu/~finin/prolog/sibling/siblings.html
% (1) generate all results, with duplicates;
% (2) remove duplicates;
% (3) generate as answers those remaining pairs for which there is not also
%   a similar pair whose args aren't in standard order.
% @< which is true when one term comes before the other in 'standard order'.
% setof(+Template, +Goal, -Set)
% Solution for non-dublicate answers
% sibling(X,Y) :- setof((X,Y), P^(parent(P,X),parent(P,Y), \+X=Y), Sibs),
%             member((X,Y), Sibs),
%             \+ (Y@<X, member((Y,X), Sibs)).

sibling(X,Y) :- parent(P,X), parent(P,Y), X\=Y.


% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- male(X), sibling(X,Y).
sister(X,Y) :- female(X), sibling(X,Y).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
% ?- siblingInLaw(selma,X).
% X = homer .
siblingInLaw(X,Y) :- sibling(X,Z), married(Z,Y).
siblingInLaw(X,Y) :- sibling(Y,Z), married(Z,X).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
%  Aunt
% 1. The sister of one's father or mother.
% 2. The wife of one's uncle.
% uncle
% 1. a brother of one's father or mother.
% 2. an aunt's husband.
aunt(X,Y) :- female(X), sibling(X,Z), parent(Z,Y).
aunt(X,Y) :- female(X), married(X,Z), sibling(Z,T), parent(T,Y).
uncle(X,Y) :- male(X), sibling(X,Z), parent(Z,Y).
%homer->married with Marge -> Marge sibling with Patty and Selma->Selma kid ling
uncle(X,Y) :- male(X), married(X,Z), sibling(Z,T), parent(T,Y).

% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- parent(Z,X), parent(S,Y), sibling(Z,S).
%cousin(X,Y) :- uncle(U,X), father(U,Y).


% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(Z,Y), ancestor(X,Z).


% Extra credit: Define the predicate `related/2`.
%related(X,Y) :- ancestor(E,X), ancestor(E,Y).
%related(X,Y) :- sibling(X,Y).
%related(X,Y) :- siblingInLaw(X,Y).
%related(X,Y) :- siblingInLaw(Y,X).
%related(X,Y) :- cousin(X,Y).
%related(X,Y) :- cousin(Y,X).
%related(X,Y) :- aunt(X,Y).
%related(X,Y) :- aunt(Y,X).
%related(X,Y) :- uncle(X,Y).
%related(X,Y) :- uncle(Y,X).
%related(X,Y) :- parent(Y,X).
%related(X,Y) :- grandparent(X,Y).
%  enumerates all members of the tree and doesn’t infinitely loop.
% solution commented out will give infinite loop. but this solution will print
% out the  every other person in the tree.
related(X,Y) :- male(Y), X \= Y.
related(X,Y) :- female(Y), X \= Y.


%%
% Part 2. Language implementation (see course web page)
%%
% reduce(add(L,R),Result) :- reduce(L,M), reduce(R,N), Result is M+N.
% cmd/3 which describes the effect of a command on the stack
% predicate cmd(C,S1,S2) means that executing command C with
% stack S1 produces stack S2.
fun(X) :- number(X).
fun(X) :- string(X).

cmd(X,Y,S2) :- fun(X), S2 = [X|Y].
cmd(add,[H,S|T],S2) :- X is H+S, S2 = [X|T].
cmd(lte,[H,S|T],S2) :- H =< S, S2 = [t|T].
cmd(lte,[H,S|T],S2) :- H > S, S2 = [f|T].
cmd(if(T,_),[t|Y],Z) :- prog(T,Y,Z).
cmd(if(_,F),[f|Y],Z) :- prog(F,Y,Z).

% prog/3, which describes the effect of a program on the stack
% the predicate prog(P,S1,S2) means that executing program P
% with stack S1 produces stack S2.
%base case - prog([],X,Y) :- Y = X.
prog([X],Y,Z) :- cmd(X, Y, Z).
prog([X|Y],Z,T) :- cmd(X,Z,U), prog(Y,U,T).
