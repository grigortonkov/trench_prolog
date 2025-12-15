%% PLANTAFEL mit KI Prototyp 
%%

machine(machine1).
machine(machine2).
machine(machine3).  

allmachines([order1, order2, order3]).

order(order1, 200000).
order(order2, 100000).
order(order3, 300000).

% some lists 
allorders([order1, order2, order3]).

% Collect all persons into a list of person(Name, Age) structures
get_all_orders(Orders) :-
    findall(order(Id, Revenue), order(Id, Revenue), Orders).


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


% Display sorted orders
display_orders([]).
display_orders([order(Id, Revenue)|Rest]) :-
    format('~w: ~w revenue ~n', [Id, Revenue]),
    display_orders(Rest).


% Main predicate to show sorted results

% Sort orders by revenue (ascending order)
sort_orders_by_revenue(SortedOrders) :-
    get_all_orders(Orders),
    sort_by_revenue(Orders, SortedOrders).

% Helper predicate to sort by age
sort_by_revenue(Orders, Sorted) :-
    predsort(compare_revenue, Orders, Sorted).

% Compare two orders by age
compare_revenue(Order, order(_, Revenue1), order(_, Revenue2)) :-
    compare(Order, Revenue1, Revenue2).


show_sorted_ascending :-
    write('Orders sorted by revenue (ascending):'), nl,
    sort_orders_by_revenue(Sorted),
    display_orders(Sorted).

% ? takeNext(Order, "2025-01-01").