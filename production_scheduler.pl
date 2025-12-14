% ============================================================================
% PRODUCTION ORDER SCHEDULING SYSTEM IN PROLOG
% ============================================================================
% This system schedules production orders with multiple steps across different
% machines, considering resources, materials, delivery dates, and shift systems.
% ============================================================================

:- dynamic scheduled_order/6.
:- dynamic scheduled_step/8.
:- dynamic order_counter/1.

% Initialize order counter
order_counter(1000).

% ============================================================================
% DATABASE FACTS
% ============================================================================

% Products: product(ProductID, Name, MaterialNeeded, Quantity)
product(p1, aIR, rope, 100).
product(p2, aSC, cable, 200).
product(p3, wire_bundle, wire, 150).

% Production Steps: production_step(ProductID, StepNumber, MachineID, BaseTime)
% BaseTime is in hours for 1-shift system
production_step(p1, 1, m1, 24).
production_step(p1, 2, m2, 20).
production_step(p1, 3, m3, 16).
production_step(p1, 4, m4, 16).
production_step(p1, 5, m5, 16).

production_step(p2, 1, m1, 18).
production_step(p2, 2, m2, 22).
production_step(p2, 3, m3, 22).

production_step(p3, 1, m2, 12).
production_step(p3, 2, m3, 14).
production_step(p3, 3, m1, 10).

% Machines: machine(MachineID, Name)
machine(m1, cutting_machine).
machine(m2, winding).
machine(m3, lack).
machine(m4, oven).
machine(m5, finishing_station).

% Resources: resource_needed(ProductID, ResourceID, Quantity)
resource_needed(p1, lattenkorb24, 2).
resource_needed(p2, lattenkorb24, 1).
resource_needed(p3, lattenkorb42, 3).

% Resources: resource(ResourceID, Name, Available)
resource(lattenkorb24, 'Lattenkorb 24 Latten', 5).
resource(lattenkorb42, 'Lattenkorb 42 Latten', 10).

% Shifts: shift_system(SystemID, Name, Multiplier)
shift_system(1, one_shift, 1.0).
shift_system(2, two_shift, 0.67).  % ~16h instead of 24h
shift_system(3, three_shift, 0.33). % ~8h instead of 24h

% Calendar: working_day(Date)
% Date format: date(Year, Month, Day)
working_day(date(2025, 12, 15)).
working_day(date(2025, 12, 16)).
working_day(date(2025, 12, 17)).
working_day(date(2025, 12, 18)).
working_day(date(2025, 12, 19)).
working_day(date(2025, 12, 22)).
working_day(date(2025, 12, 23)).
working_day(date(2026, 1, 5)).
working_day(date(2026, 1, 6)).
working_day(date(2026, 1, 7)).
working_day(date(2026, 1, 8)).
working_day(date(2026, 1, 9)).
working_day(date(2026, 1, 10)).
working_day(date(2026, 1, 11)).
working_day(date(2026, 1, 12)).
working_day(date(2026, 1, 13)).
working_day(date(2026, 1, 14)).
working_day(date(2026, 1, 15)).
working_day(date(2026, 1, 16)).
working_day(date(2026, 1, 17)).
working_day(date(2026, 1, 18)).
working_day(date(2026, 1, 19)).
working_day(date(2026, 1, 20)).
working_day(date(2026, 1, 21)).
working_day(date(2026, 1, 22)).
working_day(date(2026, 1, 23)).
working_day(date(2026, 1, 24)).
working_day(date(2026, 1, 25)).
working_day(date(2026, 1, 26)).
working_day(date(2026, 1, 27)).
working_day(date(2026, 1, 28)).
working_day(date(2026, 1, 29)).
working_day(date(2026, 1, 30)).
working_day(date(2026, 1, 31)).


% ============================================================================
% UTILITY PREDICATES
% ============================================================================

% Calculate production time based on shift system
calculate_step_time(BaseTime, ShiftSystem, ActualTime) :-
    shift_system(ShiftSystem, _, Multiplier),
    ActualTime is ceiling(BaseTime * Multiplier).

% Date arithmetic - add hours to a datetime
% datetime(Date, Hour)
add_hours_to_datetime(datetime(Date, StartHour), Hours, datetime(EndDate, EndHour)) :-
    TotalHours is StartHour + Hours,
    calculate_end_date(Date, TotalHours, EndDate, EndHour).

calculate_end_date(Date, TotalHours, EndDate, EndHour) :-
    TotalHours < 24,
    EndDate = Date,
    EndHour is TotalHours.

calculate_end_date(date(Y, M, D), TotalHours, EndDate, EndHour) :-
    TotalHours >= 24,
    Days is floor(TotalHours / 24),
    EndHour is floor(TotalHours) mod 24,
    add_days_to_date(date(Y, M, D), Days, EndDate).

% Simple date addition (simplified version)
add_days_to_date(date(Y, M, D), Days, date(Y2, M2, D2)) :-
    D1 is D + Days,
    days_in_month(M, Y, MaxDays),
    (D1 =< MaxDays ->
        Y2 = Y, M2 = M, D2 = D1
    ;
        D2 is D1 - MaxDays,
        M1 is M + 1,
        (M1 > 12 ->
            Y2 is Y + 1, M2 = 1
        ;
            Y2 = Y, M2 = M1
        )
    ).

days_in_month(1, _, 31). days_in_month(2, _, 28). days_in_month(3, _, 31).
days_in_month(4, _, 30). days_in_month(5, _, 31). days_in_month(6, _, 30).
days_in_month(7, _, 31). days_in_month(8, _, 31). days_in_month(9, _, 30).
days_in_month(10, _, 31). days_in_month(11, _, 30). days_in_month(12, _, 31).

% Compare dates (before)
date_before(date(Y1, M1, D1), date(Y2, M2, D2)) :-
    (Y1 < Y2 ; 
     (Y1 =:= Y2, M1 < M2) ; 
     (Y1 =:= Y2, M1 =:= M2, D1 < D2)).

datetime_before(datetime(D1, H1), datetime(D2, H2)) :-
    (date_before(D1, D2) ; (D1 = D2, H1 < H2)).

% ============================================================================
% SCHEDULING PREDICATES
% ============================================================================

% Main predicate to schedule a new order
schedule_new_order(ProductID, Quantity, DeliveryDate, ShiftSystem) :-
    % Generate new order ID
    order_counter(CurrentID),
    OrderID = CurrentID,
    NewID is CurrentID + 1,
    retract(order_counter(CurrentID)),
    assert(order_counter(NewID)),
    
    % Get product info
    product(ProductID, ProductName, Material, UnitMaterial),
    TotalMaterial is Quantity * UnitMaterial,
    
    % Check material availability
    write('Scheduling Order '), write(OrderID), nl,
    write('Product: '), write(ProductName), nl,
    write('Quantity: '), write(Quantity), nl,
    write('Material needed: '), write(TotalMaterial), write('m of '), write(Material), nl,
    write('Delivery date: '), write(DeliveryDate), nl,
    
    % Calculate backward from delivery date
    findall(StepNum, production_step(ProductID, StepNum, _, _), Steps),
    length(Steps, NumSteps),
    
    % Schedule each step backward from delivery date
    schedule_steps_backward(OrderID, ProductID, Steps, DeliveryDate, ShiftSystem),
    
    % Store the order
    assert(scheduled_order(OrderID, ProductID, Quantity, DeliveryDate, ShiftSystem, Material)),
    
    write('Order '), write(OrderID), write(' scheduled successfully!'), nl.

% Schedule production steps backward from delivery date
schedule_steps_backward(OrderID, ProductID, Steps, DeliveryDate, ShiftSystem) :-
    reverse(Steps, ReversedSteps),
    schedule_steps_backward_helper(OrderID, ProductID, ReversedSteps, datetime(DeliveryDate, 0), ShiftSystem).

schedule_steps_backward_helper(_, _, [], _, _).
schedule_steps_backward_helper(OrderID, ProductID, [StepNum|Rest], EndDateTime, ShiftSystem) :-
    % Get step info
    production_step(ProductID, StepNum, MachineID, BaseTime),
    calculate_step_time(BaseTime, ShiftSystem, ActualTime),
    
    % Calculate start time by subtracting ActualTime from EndDateTime
    subtract_hours_from_datetime(EndDateTime, ActualTime, StartDateTime),
    
    % Find available machine slot (simplified - checks for conflicts)
    find_available_slot(MachineID, StartDateTime, EndDateTime, FinalStart, FinalEnd),
    
    % Store the scheduled step
    assert(scheduled_step(OrderID, ProductID, StepNum, MachineID, FinalStart, FinalEnd, ShiftSystem, ActualTime)),
    
    % Continue with next step, using current start as next end
    schedule_steps_backward_helper(OrderID, ProductID, Rest, FinalStart, ShiftSystem).

% Subtract hours from datetime
subtract_hours_from_datetime(datetime(Date, Hour), Hours, datetime(StartDate, StartHour)) :-
    NewHour is Hour - Hours,
    (NewHour >= 0 ->
        StartDate = Date,
        StartHour is NewHour
    ;
        DaysBack is ceiling(abs(NewHour) / 24),
        subtract_days_from_date(Date, DaysBack, TempDate),
        RemainingHours is NewHour + (DaysBack * 24),
        StartDate = TempDate,
        StartHour is RemainingHours
    ).

subtract_days_from_date(date(Y, M, D), Days, date(Y2, M2, D2)) :-
    D1 is D - Days,
    (D1 > 0 ->
        Y2 = Y, M2 = M, D2 = D1
    ;
        M1 is M - 1,
        (M1 < 1 ->
            Y2 is Y - 1, M2 = 12
        ;
            Y2 = Y, M2 = M1
        ),
        days_in_month(M2, Y2, MaxDays),
        D2 is MaxDays + D1
    ).

% Find available slot for machine with conflict checking
find_available_slot(MachineID, DesiredStart, DesiredEnd, FinalStart, FinalEnd) :-
    % Get all scheduled steps for this machine
    findall(
        slot(StepStart, StepEnd),
        scheduled_step(_, _, _, MachineID, StepStart, StepEnd, _, _),
        ExistingSlots
    ),
    % Try to fit the slot, checking for conflicts
    find_free_slot(DesiredStart, DesiredEnd, ExistingSlots, FinalStart, FinalEnd).

% Find a free slot that doesnt conflict with existing slots
find_free_slot(Start, End, ExistingSlots, FinalStart, FinalEnd) :-
    % Check if the desired slot conflicts with any existing slot
    (has_conflict(Start, End, ExistingSlots) ->
        % If conflict exists, move the slot earlier
        calculate_duration(Start, End, Duration),
        move_slot_earlier(Start, ExistingSlots, Duration, NewStart, NewEnd),
        % Recursively check the new slot
        find_free_slot(NewStart, NewEnd, ExistingSlots, FinalStart, FinalEnd)
    ;
        % No conflict, use this slot
        FinalStart = Start,
        FinalEnd = End
    ).

% Check if a time slot conflicts with any existing slot
has_conflict(Start, End, ExistingSlots) :-
    member(slot(ExistingStart, ExistingEnd), ExistingSlots),
    slots_overlap(Start, End, ExistingStart, ExistingEnd).

% Check if two time slots overlap
slots_overlap(Start1, End1, Start2, End2) :-
    % Slots overlap if one starts before the other ends
    % and ends after the other starts
    (datetime_before(Start1, End2) ; Start1 = End2),
    (datetime_before(Start2, End1) ; Start2 = End1),
    \+ (Start1 = End2),  % Exact end-to-start is not a conflict
    \+ (Start2 = End1).

% Calculate duration between two datetimes in hours
calculate_duration(datetime(D1, H1), datetime(D2, H2), Duration) :-
    (D1 = D2 ->
        Duration is H2 - H1
    ;
        % Simplified: assume dates are close
        days_between(D1, D2, Days),
        Duration is (Days * 24) + (H2 - H1)
    ).

days_between(date(Y, M, D1), date(Y, M, D2), Days) :-
    Days is D2 - D1.
days_between(date(Y, M1, _), date(Y, M2, _), Days) :-
    M1 \= M2,
    Days is (M2 - M1) * 30.  % Simplified
days_between(date(Y1, _, _), date(Y2, _, _), Days) :-
    Y1 \= Y2,
    Days is (Y2 - Y1) * 365.  % Simplified

% Move a slot earlier to avoid conflicts
move_slot_earlier(datetime(Date, Hour), ExistingSlots, Duration, NewStart, NewEnd) :-
    % Find the latest conflicting slot that ends before our desired start
    findall(
        EndTime,
        (member(slot(_, EndTime), ExistingSlots),
         datetime_before(EndTime, datetime(Date, Hour))),
        ConflictingEnds
    ),
    (ConflictingEnds \= [] ->
        % Find the latest end time
        find_latest_datetime(ConflictingEnds, LatestEnd),
        % Start right after the latest conflicting slot
        NewEnd = LatestEnd,
        subtract_hours_from_datetime(NewEnd, Duration, NewStart)
    ;
        % No earlier conflicts, move back by duration + buffer
        Buffer is Duration * 0.1,  % 10% buffer
        TotalShift is ceiling(Duration + Buffer),
        subtract_hours_from_datetime(datetime(Date, Hour), TotalShift, NewStart),
        add_hours_to_datetime(NewStart, Duration, NewEnd)
    ).

% Find the latest datetime from a list
find_latest_datetime([DT], DT).
find_latest_datetime([DT1, DT2|Rest], Latest) :-
    (datetime_before(DT1, DT2) ->
        find_latest_datetime([DT2|Rest], Latest)
    ;
        find_latest_datetime([DT1|Rest], Latest)
    ).

% ============================================================================
% QUERY PREDICATES
% ============================================================================

% Query schedule for a specific order
query_order_schedule(OrderID) :-
    scheduled_order(OrderID, ProductID, Quantity, DeliveryDate, ShiftSystem, Material),
    product(ProductID, ProductName, _, _),
    write('========================================'), nl,
    write('SCHEDULE FOR ORDER '), write(OrderID), nl,
    write('========================================'), nl,
    write('Product: '), write(ProductName), write(' ('), write(ProductID), write(')'), nl,
    write('Quantity: '), write(Quantity), nl,
    write('Material: '), write(Material), nl,
    write('Delivery Date: '), write(DeliveryDate), nl,
    shift_system(ShiftSystem, ShiftName, _),
    write('Shift System: '), write(ShiftName), nl,
    write('----------------------------------------'), nl,
    write('PRODUCTION STEPS:'), nl,
    write('----------------------------------------'), nl,
    findall(StepNum, scheduled_step(OrderID, _, StepNum, _, _, _, _, _), StepNums),
    sort(StepNums, SortedSteps),
    print_steps(OrderID, SortedSteps),
    write('========================================'), nl.

print_steps(_, []).
print_steps(OrderID, [StepNum|Rest]) :-
    scheduled_step(OrderID, _, StepNum, MachineID, Start, End, _, Duration),
    machine(MachineID, MachineName),
    write('Step '), write(StepNum), write(': '), write(MachineName), write(' ('), write(MachineID), write(')'), nl,
    write('  Start: '), write(Start), nl,
    write('  End:   '), write(End), nl,
    write('  Duration: '), format('~2f', [Duration]), write(' hours'), nl,
    print_steps(OrderID, Rest).

% List all scheduled orders
list_all_orders :-
    write('========================================'), nl,
    write('ALL SCHEDULED ORDERS'), nl,
    write('========================================'), nl,
    findall(OrderID, scheduled_order(OrderID, _, _, _, _, _), Orders),
    sort(Orders, SortedOrders),
    print_order_summary(SortedOrders).

print_order_summary([]).
print_order_summary([OrderID|Rest]) :-
    scheduled_order(OrderID, ProductID, Quantity, DeliveryDate, _, _),
    product(ProductID, ProductName, _, _),
    write('Order '), write(OrderID), write(': '), 
    write(Quantity), write('x '), write(ProductName),
    write(' (Due: '), write(DeliveryDate), write(')'), nl,
    print_order_summary(Rest).

% ============================================================================
% EXAMPLE USAGE
% ============================================================================

% Example queries to run:
% 
% 1. Schedule a new order:
%    ?- schedule_new_order(p1, 1, date(2026, 1, 1), 3).
%
% 2. Query order schedule:
%    ?- query_order_schedule(1000).
%
% 3. List all orders:
%    ?- list_all_orders.
%
% 4. Schedule multiple orders with different shift systems:
%    ?- schedule_new_order(p1, 2, date(2026, 1, 5), 1).
%    ?- schedule_new_order(p2, 1, date(2026, 1, 8), 2).
%    ?- schedule_new_order(p3, 3, date(2026, 1, 10), 3).