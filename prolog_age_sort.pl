% Define persons with their names and ages
% person(Name, Age)
person(alice, 25).
person(bob, 30).
person(charlie, 20).
person(diana, 35).
person(eve, 28).

% Collect all persons into a list of person(Name, Age) structures
get_all_persons(Persons) :-
    findall(person(Name, Age), person(Name, Age), Persons).

% Sort persons by age (ascending order)
sort_persons_by_age(SortedPersons) :-
    get_all_persons(Persons),
    sort_by_age(Persons, SortedPersons).

% Helper predicate to sort by age
sort_by_age(Persons, Sorted) :-
    predsort(compare_age, Persons, Sorted).

% Compare two persons by age
compare_age(Order, person(_, Age1), person(_, Age2)) :-
    compare(Order, Age1, Age2).

% Sort in descending order (oldest first)
sort_persons_by_age_desc(SortedPersons) :-
    get_all_persons(Persons),
    predsort(compare_age_desc, Persons, SortedPersons).

compare_age_desc(Order, person(_, Age1), person(_, Age2)) :-
    compare(TempOrder, Age2, Age1),  % Reversed comparison
    Order = TempOrder.

% Display sorted persons
display_persons([]).
display_persons([person(Name, Age)|Rest]) :-
    format('~w: ~w years old~n', [Name, Age]),
    display_persons(Rest).

% Main predicate to show sorted results
show_sorted_ascending :-
    write('Persons sorted by age (ascending):'), nl,
    sort_persons_by_age(Sorted),
    display_persons(Sorted).

show_sorted_descending :-
    write('Persons sorted by age (descending):'), nl,
    sort_persons_by_age_desc(Sorted),
    display_persons(Sorted).

% Example queries:
% ?- show_sorted_ascending.
% ?- show_sorted_descending.
% ?- sort_persons_by_age(Result).