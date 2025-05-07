% Definizione numeri reali
extended_real(X) :- number(X).
extended_real(pos_infinity).
extended_real(neg_infinity).


not_neg_infinity(X) :-
    X \= neg_infinity.

not_infinity(X) :-
    X \= pos_infinity.


%Somma
plus_e(0).

plus_e(X, Result) :-
    extended_real(X),
    Result = X, !.

plus_e(X, Result) :-
    extended_real(Result),
    X = Result.

plus_e(X, Y, Result) :-
    sum_extended_reals(X, Y, Result).

%Predicato di somma
%Casi con infinito
sum_extended_reals(pos_infinity, neg_infinity, _) :- !, fail.
sum_extended_reals(neg_infinity, pos_infinity, _) :- !, fail.
sum_extended_reals(pos_infinity, X, pos_infinity) :- extended_real(X), !.
sum_extended_reals(X, pos_infinity, pos_infinity) :- extended_real(X), !.
sum_extended_reals(neg_infinity, X, neg_infinity) :- extended_real(X), !.
sum_extended_reals(X, neg_infinity, neg_infinity) :- extended_real(X), !.

sum_extended_reals(X, Y, Result) :-
    number(X),
    number(Y),
    Result is X + Y, !.

% Gestisce i casi in cui una delle variabili è sconosciuta
sum_extended_reals(X, Y, Result) :-
    var(X),
    number(Y),
    number(Result),
    sum_extended_reals(0, Y, Result1),
    X is Result - Result1.

sum_extended_reals(X, Y, Result) :-
    number(X),
    var(Y),
    number(Result),
    sum_extended_reals(X, 0, Result1),
    Y is Result - Result1.

sum_extended_reals(X, Y, Result) :-
    var(X),
    var(Y),
    number(Result),
    X = 0,
    Y = Result.

%Differenza
minus_e(pos_infinity, neg_infinity).
minus_e(neg_infinity, pos_infinity).
minus_e(X, Result) :-
    number(X),
    Result is -X, !.

minus_e(X, Result) :-
    number(Result),
    X is -Result.
%Casi con infinito
minus_e(pos_infinity, pos_infinity, _) :- !, fail.
minus_e(neg_infinity, neg_infinity, _) :- !, fail.
minus_e(pos_infinity, neg_infinity, pos_infinity) :- !.
minus_e(neg_infinity, pos_infinity, neg_infinity) :- !.
minus_e(pos_infinity, X, pos_infinity) :- extended_real(X), !.
minus_e(neg_infinity, X, neg_infinity) :- extended_real(X), !.
minus_e(X, pos_infinity, neg_infinity) :- extended_real(X), !.
minus_e(X, neg_infinity, pos_infinity) :- extended_real(X), !.

%Caso "base"
minus_e(X, Y, Result) :-
    number(X),
    number(Y),
    Result is X - Y, !.
minus_e(X, Y, Result) :-
    number(X),
    number(Result),
    Y is X - Result, !.
minus_e(X, Y, Result) :-
    number(Y),
    number(Result),
    X is Result + Y, !.


%Prodotto
times_e(1).
times_e(X, X) :- extended_real(X).

%Casi con infinito
times_e(pos_infinity, 0, _) :- !, fail.
times_e(0, pos_infinity, _) :- !, fail.
times_e(neg_infinity, 0, _) :- !, fail.
times_e(0, neg_infinity, _) :- !, fail.
times_e(pos_infinity, pos_infinity, pos_infinity) :- !.
times_e(neg_infinity, neg_infinity, pos_infinity) :- !.
times_e(pos_infinity, neg_infinity, neg_infinity) :- !.
times_e(neg_infinity, pos_infinity, neg_infinity) :- !.
times_e(pos_infinity, X, pos_infinity) :- number(X), X > 0, !.
times_e(pos_infinity, X, neg_infinity) :- number(X), X < 0, !.
times_e(neg_infinity, X, neg_infinity) :- number(X), X > 0, !.
times_e(neg_infinity, X, pos_infinity) :- number(X), X < 0, !.
times_e(X, pos_infinity, pos_infinity) :- number(X), X > 0, !.
times_e(X, pos_infinity, neg_infinity) :- number(X), X < 0, !.
times_e(X, neg_infinity, neg_infinity) :- number(X), X > 0, !.
times_e(X, neg_infinity, pos_infinity) :- number(X), X < 0, !.

times_e(X, Y, Result) :-
    number(X), number(Y), !,
    Result is X * Y.

times_e(X, Y, Result) :-
    number(X), number(Result), X =\= 0, !,
    Y is Result / X.

times_e(X, Y, Result) :-
    number(Y), number(Result), Y =\= 0, !,
    X is Result / Y.


%Divisione
div_e(X, Result) :-
    var(X), number(Result), Result \= 0, !,
    X is 1 / Result.

div_e(X, Result) :-
    number(X), X \= 0, !,  % non c'è divisione con 0
    Result is 1 / X.

%reciprochi del infinito
div_e(pos_infinity, 0) :- !.
div_e(neg_infinity, 0) :- !.
div_e(0, pos_infinity) :- !.


div_e(X, Y, Result) :-
    number(X), number(Y), Y \= 0, !,
    Result is X / Y.

div_e(X, Y, Result) :-
    number(X), number(Result), X \= 0, !,
    Y is X / Result.

div_e(X, Y, Result) :-
    number(Y), number(Result), Y \= 0, !,
    X is Result * Y.

%Casi con infinito
div_e(X, pos_infinity, 0) :- number(X), !.
div_e(X, neg_infinity, 0) :- number(X), !.

div_e(pos_infinity, pos_infinity, _) :- !, fail.
div_e(neg_infinity, neg_infinity, _) :- !, fail.
div_e(pos_infinity, neg_infinity, _) :- !, fail.
div_e(neg_infinity, pos_infinity, _) :- !, fail.

div_e(pos_infinity, Y, pos_infinity) :- number(Y), Y > 0, !.
div_e(pos_infinity, Y, neg_infinity) :- number(Y), Y < 0, !.
div_e(neg_infinity, Y, neg_infinity) :- number(Y), Y > 0, !.
div_e(neg_infinity, Y, pos_infinity) :- number(Y), Y < 0, !.

%divisione con 0 fallisce sempre
div_e(X, 0, _) :- !, number(X), fail.

empty_interval([]).

%creazione intervalli
interval([]).


interval(X, [X, X]) :-
    extended_real(X).


interval(X, [L, L]) :-
    var(X),                      % X deve essere una variabile
    extended_real(L),            % L deve essere un numero reale esteso
    X = L.

interval(pos_infinity, pos_infinity, [pos_infinity, pos_infinity]).
interval(neg_infinity, neg_infinity, [neg_infinity, neg_infinity]).
interval(pos_infinity, X, _) :- !, extended_real(X), fail. %Non valido
interval(X, neg_infinity, _) :- !, extended_real(X), fail. %Non valido

interval(neg_infinity, H, [neg_infinity, H]) :-
    extended_real(H).

interval(L, pos_infinity, [L, pos_infinity]) :-
    extended_real(L).



interval(L, H, [L, H]) :-
    extended_real(L),
    extended_real(H),
    L < H.   %Controllo per rendere l'intervallo valido


interval(L, H, [L, H]) :-
    extended_real(L),
    extended_real(H),
    L = H,
    interval(L, _Lista).

interval(L, H, []) :-
    extended_real(L),
    extended_real(H),
    L > H.  %Intervallo vuoto

%controllo validità di un intervallo
%Intervalli sempre veri
is_interval([neg_infinity, neg_infinity]).
is_interval([pos_infinity, pos_infinity]).

%Intervalli illogici
is_interval([X, neg_infinity]) :-
    extended_real(X),
    !, fail.

is_interval([pos_infinity, X]) :-
    extended_real(X),
    !, fail.


is_interval([X, pos_infinity]) :-
    extended_real(X), !.

is_interval([neg_infinity, X]) :-
    extended_real(X), !.


is_interval([]).

is_interval([X, X]) :-
    extended_real(X).


is_interval([L, H]) :-
    extended_real(L),
    extended_real(H),
    L =< H.

is_interval([Interval]) :-
    is_interval(Interval).

%Controllo intervalli disgiunti
is_interval([Interval1 | Rest]) :-
    is_interval(Interval1),
    is_interval(Rest).

%L'intervallo rappresenta il whole interval
whole_interval([neg_infinity, pos_infinity]) :-
    is_interval([neg_infinity, pos_infinity]).

%INtervalli disgiunti
whole_interval(R) :-
    is_interval(R),
    R = [[neg_infinity, _] | _],
    last(R, [_, pos_infinity]).

is_singleton(S) :-
    is_interval(S),
    S = [X, X].

is_empty(I) :-
    is_interval(I),
    I = [].

not_empty_interval(I) :-
    is_interval(I),
    I \= [].
%Estermo inferiore di un intervallo
iinf([L, H], L) :-
    not_empty_interval([L, H]).

%Gestione intervalli disgiunti
iinf([FirstInterval | _Rest], L) :-
    not_empty_interval(FirstInterval),
    FirstInterval = [L, _].

%Estermo superiore di un intervallo
isup([L, H], H) :-
    not_empty_interval([L, H]).

%Gestione disgiunti
isup(Intervals, H) :-
    last(Intervals, LastInterval),
    not_empty_interval(LastInterval),
    LastInterval = [_, H].

%Verifica se un intervallo è contenuto in un altro intervallo
icontains(I, X) :-
    not_empty_interval(I),
    I = [L, H],
    extended_real(X),
    compare_bounds(L, X), %controlla se gli intervalli hanno comune un estermo
    compare_bounds(X, H).


icontains(I, X) :-
    not_empty_interval(I),
    I = [L, H],
    is_interval(X),
    X = [LX, HX],
    compare_bounds(L, LX),
    compare_bounds(HX, H).

%Intervalli disgiunti
icontains(I, X) :-
    not_empty_interval(I),
    is_interval(I),
    member(Interval, I),
    icontains(Interval, X).
%Predicato compare bound per il controllo degli estremi
compare_bounds(neg_infinity, neg_infinity).
compare_bounds(pos_infinity, pos_infinity).
compare_bounds(_, neg_infinity) :- !, fail.
compare_bounds(pos_infinity, _) :- !, fail.
compare_bounds(neg_infinity, _) :- !.
compare_bounds(_, pos_infinity) :- !.
compare_bounds([L1, H1], Y) :-
    number(Y),
    compare_bounds(L1, Y),
    compare_bounds(Y, H1).
compare_bounds(X, [L2, H2]) :-
    number(X),
    compare_bounds(L2, X),
    compare_bounds(X, H2).
compare_bounds(X, Y) :-
    number(X),
    number(Y),
    X =< Y.

%overlap
ioverlap(I1, I2) :-
    is_interval(I1),
    is_interval(I2),
    I1 = [L1, H1],
    I2 = [L2, H2],
    compare_bounds(L1, H2),
    compare_bounds(L2, H1).


%overlap casi disgiunti
ioverlap(I1, I2) :-
    is_interval(I1),
    is_interval(I2),
    member(SubInterval, I1),
    ioverlap(SubInterval, I2).


ioverlap(I1, I2) :-
    is_interval(I1),
    is_interval(I2),
    member(SubInterval, I2),
    ioverlap(I1, SubInterval).


ioverlap(I1, I2) :-
    is_interval(I1),
    is_interval(I2),
    member(SubInterval1, I1),
    member(SubInterval2, I2),
    ioverlap(SubInterval1, SubInterval2).

% Somma con intervalli
iplus([Lower, Upper]) :-
    is_interval([Lower, Upper]).

iplus(X, [X, X]) :-
    extended_real(X), !.

iplus([], []).

iplus([Lower, Upper], [Lower, Upper]) :-
    iplus([Lower, Upper]).

iplus([], Y, Y1) :-
    int_convert(Y, Y1).

iplus(X, [], X1) :-
    int_convert(X, X1).

%  somma due intervalli.
iplus(X, Y, R) :-
    int_convert(X, X1),  % Converti X in intervallo se necessario
    int_convert(Y, Y1),  % Converti Y in intervallo se necessario
    som_intervals(X1, Y1, R).

% Predicato di supporto per convertire un numero reale esteso
%o infinito in un intervallo
int_convert(X, [X, X]) :-
    extended_real(X), !.

int_convert([Lower, Upper], [Lower, Upper]) :-
    is_interval([Lower, Upper]).  % X è già un intervallo non vuoto

% Predicato di supporto per sommare due intervalli.
som_intervals([L1, U1], [L2, U2], [L3, U3]) :-
    is_interval([L1, U1]),
    is_interval([L2, U2]),
    plus_e(L1, L2, L3),  % Somma i limiti inferiori
    plus_e(U1, U2, U3),  % Somma i limiti superiori
    is_interval([L3, U3]).

%Sottrazione con intervalli
iminus([Lower, Upper]) :-
    is_interval([Lower, Upper]).

iminus([], []).

iminus(X, [X, X]) :-
    extended_real(X), !.

iminus([Lower, Upper], [Lower, Upper]) :-
    iminus([Lower, Upper]).

%casi con insieme vuoto
iminus([], Y, []) :-
    int_convert(Y, Y1),
    is_interval(Y1).

iminus(X, [], X) :-
    int_convert(X, X1),
    is_interval(X1).

iminus(X, Y, R) :-
    int_convert(X, X1),
    int_convert(Y, Y1),
    sot_intervals(X1, Y1, R).

sot_intervals([L1, U1], [L2, U2], [L3, U3]) :-
    minus_e(L1, U2, L3),   % Differenza limiti inferiori
    minus_e(U1, L2, U3),   % Differenza limiti superiori
    is_interval([L3, U3]).

% Moltiplicazione con intervalli

itimes([A, B]) :-
    is_interval([A, B]).

itimes([], []).

itimes(X, [X, X]) :-
    extended_real(X),
    is_interval([X, X]).

itimes([A, B], [A, B]) :-
    itimes([A, B]),
    is_interval([A, B]).

itimes([], Y, []) :-
    int_convert(Y, Y1),
    is_interval(Y1).

itimes(X, [], []) :-
    int_convert(X, X1),
    is_interval(X1).

itimes(X, Y, R) :-
    extended_real(X),
    extended_real(Y),
    itimes([X, X], [Y, Y], R).


itimes(X, Y, [R, R]) :-
    extended_real(X),
    extended_real(Y),
    times_e(X, Y, R),  % Moltiplicazione diretta dei numeri estesi
    is_interval([R, R]).

%prodotto tra numero e intervallo
itimes(X, [A, B], [R1, R2]) :-
    extended_real(X),
    times_e(X, A, R1),
    times_e(X, B, R2),
    is_interval([R1, R2]).

itimes([A, B], Y, [R1, R2]) :-
    extended_real(Y),
    times_e(A, Y, R1),
    times_e(B, Y, R2),
    is_interval([R1, R2]).
%prodotto intervallo x intervallo
itimes([A1, B1], [A2, B2], [R1, R2]) :-
    times_e(A1, A2, R11),
    times_e(A1, B2, R12),
    times_e(B1, A2, R21),
    times_e(B1, B2, R22),
    minimum(R11, R12, R21, R22, R1), %scelgo min
    maximum(R11, R12, R21, R22, R2), %scelgo max
    is_interval([R1, R2]).


%check del minimo
minimum(neg_infinity, _, _, _, neg_infinity) :- !.
minimum(_, neg_infinity, _, _, neg_infinity) :- !.
minimum(_, _, neg_infinity, _, neg_infinity) :- !.
minimum(_, _, _, neg_infinity, neg_infinity) :- !.

%gestione del + infinito
minimum(pos_infinity, pos_infinity, pos_infinity, pos_infinity, pos_infinity) :-
    !.
minimum(pos_infinity, pos_infinity, pos_infinity, X, X) :- !.
minimum(pos_infinity, pos_infinity, X, pos_infinity, X) :- !.
minimum(pos_infinity, X, pos_infinity, pos_infinity, X) :- !.
minimum(X, pos_infinity, pos_infinity, pos_infinity, X) :- !.
minimum(X, Y, pos_infinity, pos_infinity, Min) :-
    Min is min(X, Y), !.
minimum(X, pos_infinity, Y, pos_infinity, Min) :-
    Min is min(X, Y), !.
minimum(X, pos_infinity, pos_infinity, Y, Min) :-
    Min is min(X, Y), !.
minimum(pos_infinity, X, Y, pos_infinity, Min) :-
    Min is min(X, Y), !.
minimum(pos_infinity, X, pos_infinity, Y, Min) :-
    Min is min(X, Y), !.
minimum(pos_infinity, pos_infinity, X, Y, Min) :-
    Min is min(X, Y), !.

minimum(pos_infinity, A, B, C, Min) :-
    Min is min(A, min(B, C)), !.

minimum(A, pos_infinity, B, C, Min) :-
    Min is min(A, min(B, C)), !.

minimum(A, B, pos_infinity, C, Min) :-
    Min is min(A, min(B, C)), !.

minimum(A, B, C, pos_infinity, Min) :-
    Min is min(A, min(B, C)), !.

%Caso base
minimum(A, B, C, D, Min) :-
    Min1 is min(A, B), %minimo primo intervallo
    Min2 is min(C, D), %minimo dei secondo intervallo
    Min is min(Min1, Min2). %minimo totale


%scelta massimo
maximum(pos_infinity, _, _, _, pos_infinity) :- !.
maximum(_, pos_infinity, _, _, pos_infinity) :- !.
maximum(_, _, pos_infinity, _, pos_infinity) :- !.
maximum(_, _, _, pos_infinity, pos_infinity) :- !.

%gestione - infinito
maximum(neg_infinity, neg_infinity, neg_infinity, neg_infinity, neg_infinity) :-
    !.
maximum(neg_infinity, neg_infinity, neg_infinity, X, X) :- !.
maximum(neg_infinity, neg_infinity, X, neg_infinity, X) :- !.
maximum(neg_infinity, X, neg_infinity, neg_infinity, X) :- !.
maximum(X, neg_infinity, neg_infinity, neg_infinity, X) :- !.
maximum(X, Y, neg_infinity, neg_infinity, Min) :-
    Min is min(X, Y), !.
maximum(X, neg_infinity, Y, neg_infinity, Min) :-
    Min is min(X, Y), !.
maximum(X, neg_infinity, neg_infinity, Y, Min) :-
    Min is min(X, Y), !.
maximum(neg_infinity, X, Y, neg_infinity, Min) :-
    Min is min(X, Y), !.
maximum(neg_infinity, X, neg_infinity, Y, Min) :-
    Min is min(X, Y), !.
maximum(neg_infinity, neg_infinity, X, Y, Min) :-
    Min is min(X, Y), !.

maximum(neg_infinity, A, B, C, Max) :-
    Max is max(A, max(B, C)), !.

maximum(A, neg_infinity, B, C, Max) :-
    Max is max(A, max(B, C)), !.

maximum(A, B, neg_infinity, C, Max) :-
    Max is max(A, max(B, C)), !.

maximum(A, B, C, neg_infinity, Max) :-
    Max is max(A, max(B, C)), !.

%caso base
maximum(A, B, C, D, Max) :-
    Max1 is max(A, B),
    Max2 is max(C, D),
    Max is max(Max1, Max2).


%Divisione tra intervalli
idiv(0, [0, 0]).
idiv(X, [R, R]) :-
    extended_real(X),
    div_e(1, X, R).

idiv([A, B], [R1, R2]) :-
    is_interval([A, B]),
    div_e(1, A, R1),
    div_e(1, B, R2),
    is_interval([R2, R1]).

% Verifica se l'intervallo è positivo e senza zero
positive_interval_without_zero([pos_infinity, pos_infinity]).
positive_interval_without_zero([A, pos_infinity]) :-
    number(A), A > 0,
    is_interval([A, pos_infinity]), !.

positive_interval_without_zero([A, B]) :-
    number(A), A > 0,
    number(B), B > 0,
    is_interval([A, B]).

%Verifica che l'intervallo è postivo
positive_interval([pos_infinity, pos_infinity]).
positive_interval([A, pos_infinity]) :-
    number(A), A >= 0,
    is_interval([A, pos_infinity]), !.

positive_interval([A, B]) :-
    number(A), A >= 0,
    number(B), B >= 0,
    is_interval([A, B]).
%Verifica che l'intervallo è negativo
negative_interval([neg_infinity, neg_infinity]).
negative_interval([neg_infinity, B]) :-
    number(B), B =< 0,
    is_interval([neg_infinity, B]), !.

negative_interval([A, B]) :-
    number(A), A =< 0,
    number(B), B =< 0,
    is_interval([A, B]).
%Verifica che l'intervallo è misto
mixed_interval([neg_infinity, pos_infinity]).

mixed_interval([A, pos_infinity]) :-
    number(A), A < 0,
    is_interval([A, pos_infinity]).

mixed_interval([neg_infinity, B]) :-
    number(B), B > 0,
    is_interval([neg_infinity, B]).

mixed_interval([A, B]) :-
    number(A), A < 0,
    number(B), B > 0,
    is_interval([A, B]).

% Verifica se l'intervallo contiene zero (per il caso P0)
interval_contains_zero([A, B]) :-
    number(A),
    A =:= 0,
    extended_real(B),
    is_interval([A, B]).

%Verifica che l'intervallo contiene 0 (N0)
negative_interval_with_zero([A, 0]) :-
    number(A),
    A < 0,
    is_interval([A, 0]).

negative_interval_with_zero([neg_infinity, 0]).

negative_interval_without_zero([neg_infinity, neg_infinity]).
negative_interval_without_zero([A, B]) :-
    number(A),
    number(B),
    A < 0,
    B < 0,
    is_interval([A, B]).

negative_interval_without_zero([neg_infinity, B]) :-
    number(B), B < 0,
    is_interval([neg_infinity, B]).


%casi della divione tra insiemi
idiv([0, 0], IntervalDenominator, [0, 0]) :-
    is_interval(IntervalDenominator).

idiv(IntervalNumerator, [0, 0], _) :-
    !, is_interval(IntervalNumerator), fail.

% Predicato principale per gestire gli intervalli
idiv(IntervalNumerator, IntervalDenominator, Result) :-
    positive_interval_without_zero(IntervalNumerator),
    positive_interval(IntervalDenominator),
    idiv_do1(IntervalNumerator, IntervalDenominator, Result).

idiv(IntervalNumerator, IntervalDenominator, Result) :-
    interval_contains_zero(IntervalNumerator),
    positive_interval(IntervalDenominator),
    idiv_do2(IntervalNumerator, IntervalDenominator, Result).

idiv(IntervalNumerator, IntervalDenominator, Result) :-
    mixed_interval(IntervalNumerator),
    positive_interval(IntervalDenominator),
    idiv_do3(IntervalNumerator, IntervalDenominator, Result).

idiv(IntervalNumerator, IntervalDenominator, Result) :-
    negative_interval_with_zero(IntervalNumerator),
    positive_interval(IntervalDenominator),
    idiv_do4(IntervalNumerator, IntervalDenominator, Result).

idiv(IntervalNumerator, IntervalDenominator, Result) :-
    negative_interval_without_zero(IntervalNumerator),
    positive_interval(IntervalDenominator),
    idiv_do5(IntervalNumerator, IntervalDenominator, Result).

idiv(IntervalNumerator, IntervalDenominator, [neg_infinity, pos_infinity]) :-
    interval_contains_zero(IntervalNumerator),
    mixed_interval(IntervalDenominator).

idiv(IntervalNumerator, IntervalDenominator, [neg_infinity, pos_infinity]) :-
    mixed_interval(IntervalNumerator),
    mixed_interval(IntervalDenominator).

idiv(IntervalNumerator, IntervalDenominator, [neg_infinity, pos_infinity]) :-
    negative_interval_with_zero(IntervalNumerator),
    mixed_interval(IntervalDenominator).

idiv(IntervalNumerator, IntervalDenominator, Result) :-
    positive_interval_without_zero(IntervalNumerator),
    negative_interval(IntervalDenominator),
    idiv_do6(IntervalNumerator, IntervalDenominator, Result).

idiv(IntervalNumerator, IntervalDenominator, Result) :-
    interval_contains_zero(IntervalNumerator),
    negative_interval(IntervalDenominator),
    idiv_do7(IntervalNumerator, IntervalDenominator, Result).

idiv(IntervalNumerator, IntervalDenominator, Result) :-
    mixed_interval(IntervalNumerator),
    negative_interval(IntervalDenominator),
    idiv_do8(IntervalNumerator, IntervalDenominator, Result).

idiv(IntervalNumerator, IntervalDenominator, Result) :-
    negative_interval_with_zero(IntervalNumerator),
    negative_interval(IntervalDenominator),
    idiv_do9(IntervalNumerator, IntervalDenominator, Result).

idiv(IntervalNumerator, IntervalDenominator, Result) :-
    negative_interval_without_zero(IntervalNumerator),
    negative_interval(IntervalDenominator),
    idiv_do10(IntervalNumerator, IntervalDenominator, Result).

idiv(IntervalNumerator, IntervalDenominator, Result) :-
    negative_interval_without_zero(IntervalNumerator),
    mixed_interval(IntervalDenominator),
    idiv_do11(IntervalNumerator, IntervalDenominator, Result).

idiv(IntervalNumerator, IntervalDenominator, Result) :-
    positive_interval_without_zero(IntervalNumerator),
    mixed_interval(IntervalDenominator),
    idiv_do12(IntervalNumerator, IntervalDenominator, Result).


%Caso eccezionale: quando il denominatore ha 0 come estremo inferiore
idiv_do2([0, B], [0, D], [0, pos_infinity]) :-
    extended_real(D),
    extended_real(B).

%caso base
idiv_do2([0, B], [C, D], [0, High]) :-
    extended_real(B),
    extended_real(C), C \= 0,
    extended_real(D),
    div_e(B, C, High),
    is_interval([0, High]).

idiv_do1([A, pos_infinity], [C, D], [Low, High]) :-
    extended_real(A), extended_real(B),
    extended_real(C), extended_real(D),
    C \= 0,
    div_e(A, D, Low),
    div_e(B, C, High),
    is_interval([Low, High]), !.

idiv_do1([A, B], [C, pos_infinity], [Low, High]) :-
    extended_real(A), extended_real(B),
    extended_real(C), extended_real(D),
    C \= 0,
    div_e(A, D, Low),
    div_e(B, C, High),
    is_interval([Low, High]), !.

idiv_do1([A, pos_infinity], [C, pos_infinity], [Low, High]) :-
    extended_real(A), extended_real(B),
    extended_real(C), extended_real(D),
    C \= 0,
    div_e(A, D, Low),
    div_e(B, C, High),
    is_interval([Low, High]), !.


idiv_do1([A, B], [C, D], [Low, High]) :-
    extended_real(A), extended_real(B),
    extended_real(C), extended_real(D),
    C \= 0,
    div_e(A, D, Low),
    div_e(B, C, High),
    is_interval([Low, High]).

% Caso eccezionale: quando il denominatore ha 0 come estremo inferiore
idiv_do1([A, B], [0, D], [Low, pos_infinity]) :-
    extended_real(D),
    extended_real(A), extended_real(B),
    div_e(A, D, Low),
    is_interval([Low, pos_infinity]).

%se il numeratore è un intervallo con minimo negativo e massimo positivo
idiv_do3([A, B], [C, D], [Low, High]) :-
    extended_real(D),
    div_e(A, C, Low),
    div_e(B, C, High),
    is_interval([Low, High]).

% Caso eccezionale: quando il denominatore ha 0 come estremo inferiore
idiv_do3([A, B], [0, D], [neg_infinity, pos_infinity]) :-
    extended_real(D),
    extended_real(A),
    extended_real(B).

%caso base
idiv_do4([A, 0], [C, D], [Low, 0]) :-
    extended_real(C),
    extended_real(D),
    div_e(A, C, Low),
    is_interval([Low, 0]).
%Caso eccezionale: quando il denominatore ha 0 come estremo inferiore
idiv_do4([A, 0], [0, D], [neg_infinity, 0]) :-
    extended_real(A),
    extended_real(D).

%Caso base
idiv_do5([A, B], [C, D], [Low, High]) :-
    extended_real(C),
    extended_real(D),
    extended_real(A),
    extended_real(B),
    div_e(A, C, Low),
    div_e(B, D, High),
    is_interval([Low, High]).

%Caso eccezionale: quando il denominatore ha 0 come estremo inferiore
idiv_do5([A, B], [0, D], [neg_infinity, High]) :-
    extended_real(D),
    extended_real(A),
    extended_real(B),
    div_e(B, D, High),
    is_interval([neg_infinity, High]).

%Caso base
idiv_do6([A, B], [C, D], [Low, High]) :-
    extended_real(C),
    extended_real(D),
    extended_real(A),
    extended_real(B),
    div_e(B, D, Low),
    div_e(A, C, High),
    is_interval([Low, High]).

% Caso eccezionale: quando il denominatore ha 0 come estremo superiore
idiv_do6([A, B], [C, 0], [neg_infinity, High]) :-
    extended_real(C),
    extended_real(B),
    extended_real(A),
    div_e(A, C, High),
    is_interval([neg_infinity, High]).

idiv_do7([A, B], [C, D], [Low, 0]) :-
    extended_real(C),
    extended_real(D),
    extended_real(A),
    extended_real(B),
    div_e(B, D, Low),
    is_interval([Low, 0]).

% Caso eccezionale: quando il denominatore ha 0 come estremo superiore
idiv_do7([A, B], [C, 0], [neg_infinity, 0]) :-
    extended_real(C),
    extended_real(B),
    extended_real(A).

idiv_do8([A, B], [C, D], [Low, High]) :-
    extended_real(D),
    extended_real(C),
    extended_real(B),
    extended_real(A),
    div_e(B, D, Low),
    div_e(A, D, High),
    is_interval([Low, High]).

% Caso eccezionale: quando il denominatore ha 0 come estremo superiore
idiv_do8([A, B], [C, 0], [neg_infinity, pos_infinity]) :-
    extended_real(B),
    extended_real(A),
    extended_real(C).

idiv_do9([A, 0], [C, D], [0, High]) :-
    extended_real(C),
    extended_real(A),
    extended_real(D),
    div_e(A, D, High),
    is_interval([0, High]).

% Caso eccezionale: quando il denominatore ha 0 come estremo superiore
idiv_do9([A, 0], [C, 0], [0, pos_infinity]) :-
    extended_real(A),
    extended_real(C).

idiv_do10([A, B], [C, D], [Low, High]) :-
    div_e(B, C, Low),
    div_e(A, D, High),
    is_interval([Low, High]).

% Caso eccezionale: quando il denominatore ha 0 come estremo superiore
idiv_do10([A, B], [C, 0], [Low, pos_infinity]) :-
    extended_real(C),
    extended_real(B),
    extended_real(A),
    div_e(B, C, Low),
    is_interval([Low, pos_infinity]).

%casi in cui il risultato è disgiunto
idiv_do11([_A, B], [C, D], [[neg_infinity, Low1], [Low2, pos_infinity]]) :-
    div_e(B, D, Low1),
    div_e(B, C, Low2),
    is_interval([[neg_infinity, Low1], [Low2, pos_infinity]]).

idiv_do12([A, _B], [C, D], [[neg_infinity, Low1], [Low2, pos_infinity]]) :-
    div_e(A, C, Low1),
    div_e(A, D, Low2),
    is_interval([[neg_infinity, Low1], [Low2, pos_infinity]]).
