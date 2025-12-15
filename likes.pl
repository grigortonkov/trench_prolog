%% Demo coming from http://clwww.essex.ac.uk/course/LG519/2-facts/index_18.html
%%
%% Please load this file into SWI-Prolog
%%
%% Sam's likes and dislikes in food
%%
%% Considering the following will give some practice
%% in thinking about backtracking.
%%
%% You can also run this demo online at
%% http://swish.swi-prolog.org/?code=https://github.com/SWI-Prolog/swipl-devel/raw/master/demo/likes.pl&q=likes(sam,Food).

/** <examples>
?- likes(sam,dahl).
?- likes(sam,chop_suey).
?- likes(sam,pizza).
?- likes(sam,chips).
?- likes(sam,curry).
*/

likes(sam,Food) :-
    indian(Food),
    mild(Food).
likes(sam,Food) :-
    chinese(Food).
likes(sam,Food) :-
    italian(Food).

likes(sam,Food) :-
    bulgarian(Food).

likes(sam,chips).

indian(curry).
indian(dahl).
indian(tandoori).
indian(kurma).

mild(dahl).
mild(tandoori).
mild(kurma).

chinese(chow_mein).
chinese(chop_suey).
chinese(sweet_and_sour).

italian(pizza).
italian(spaghetti).

bulgarian(moussaka).
bulgarian(sarmi).
bulgarian(kebapche).
bulgarian(lyutenitsa).
bulgarian(banitsa).


man(sam).
man(john).
man(bob).
man(tom).

woman(merry).
woman(dori).

pretty(dori).

age(tom,45).
age(dori,22).

hasMoney(bob, 10001).
hasMoney(sam, 10000).
hasMoney(tom, 11000).

canHaveChilderen(M,W) :-
    man(M), woman(W), age(W,A), A < 40.
    
canHaveDate(M,W) :- man(M), woman(W), pretty(W), age(W,A), A < 25.

willBuyCar(M) :- man(M), hasMoney(M, Amount), Amount > 5000.

happy(X) :- canHaveChilderen(X,_), hasMoney(X, Money), Money>9000, Money<10999.


%% ORDER examples 

machine(machine1).
machine(machine2).
machine(machine3).  

allmachines([order1, order2, order3]).

order(order1).
order(order2).
order(order3).

% some lists 
allorders([order1, order2, order3]).

neworders([order3]).
doneorders([order1, order2]).

finishedorders :- doneorders(Done), neworders(New), append(Done, New, Finished), allorders(All), sort(All, Finished).

revenue(order1, 10000).
revenue(order2, 11000).
revenue(order3, 12000).

deadline(order1, "2025-12-31").
deadline(order2, "2025-01-31").
deadline(order3, "2025-01-31").

notBefore(order1, "2025-12-30").
notBefore(order2, "2024-01-31").
notBefore(order3, "2024-01-31").

sortOrders(Order1, Order2) :- revenue(Order1, R1), revenue(Order2, R2), R1 > R2.

takeNext(Order, Date) :- 
    order(Order), 
    deadline(Order, DeadlineDate), 
    DeadlineDate @>= Date,
    notBefore(Order, BeforeDate),
     Date @>= BeforeDate.

    
% ? takeNext(Order, "2025-01-01").