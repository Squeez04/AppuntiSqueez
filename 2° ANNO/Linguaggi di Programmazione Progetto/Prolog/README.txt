README
September 2024 Project E5P
Interval Arithmetic

Il progetto consisteva nel creare una libreria per la gestione di intervalli.
Gli intervalli sono stati trattati come liste con estremi inclusi,
gli infiniti sono stati rappresentati con le diciture:
"pos_infinity" e "neg_infinity".

Questa libreria permette l'utilizzo delle queattro operazioni
e diversi operazioni per la creazione e gestione degli intervalli.

extended_real/1:
Controlla che la variabile in ingresso sia un numero reale

plus_e:
Funzione di somma tra numeri reali

Esempio:
?- plus_e(3, 5, Result).

>Result = 8.

minus_e:
Funzione di differenza tra numeri reali

Esempio:
?- minus_e(3, 5, Result).

>Result = -2.

times_e:
Funzione che esegue il prodotto di numeri reali

Esempio:
?- times_e(3, 4, Result).

>Result = 12.

div_e:
Funzione che esegue la divisione di numeri reali

Esempio:
?- div_e(10, 2, Result).

>Result = 5.

Tutti i predicati funzionano su ogni numero reale, infiniti compresi.

?- times_e(neg_infinity, 3, Result).

>Result = neg_infinity.

interval/2:
Dato un numero restituisce un singoletto

Esempio:
?- interval(5, X).
X = [5, 5]

interval/3:
Dati i due esetremi genera un intervallo

Esempio:
?- interval(neg_infinity, 4, X).
X = [neg_infinity, 4]

is_interval/1:
Predicato che controlla la validità dell'intervallo dato in ingresso.

Esempio:
?- is_interval([neg_infinity, 5]).
true.

?- is_interval([pos_infinity, 3]).
false.

?- is_interval([[neg_infinity, 0], [5, pos_infinity]]).
true.

whole_interval/1:
Il predicato verifica se un intervallo
copre l'intero insieme dei numeri reali estesi
considerando sia intervalli singoli che disgiunti.

Esempi:
?- whole_interval([neg_infinity, pos_infinity]).
true.

?- whole_interval([[neg_infinity, 0], [1, pos_infinity]]).
true.

?- whole_interval([neg_infinity, 42]).
false.

is_singleton/1:
Verifica che l'intervallo è un singoletto.

Esempi:

?- is_singleton([5, 5]).
true.

?- is_singleton([5, 6]).
false.

is_empty/1:
Il predicato verifica se l'intervallo è vuoto.

?- is_empty([]).
true.

?- is_empty([1, 2]).
false.

not_emptyinterval/1:
Il predicato verifica se l'intervallo non è vuoto.

Esempi:
?- not_empty_interval([1, 2]).
true.

?- not_empty_interval([]).
false.

iinf/2:
Estrae il limite inferiore dell'intervallo (anche disgiunto).

Esempi:
?- iinf([4, 12], L).
L = 4.

?- iinf([[-1, 5], [6, 42]], L).
L = -1.

isup/2:
Estrae il limite superiore dell'intervallo (anche disgiunto).

Esempi:
?- isup([1, 5], H).
H = 5.

?- isup([[1, 5], [6, 42]], H).
H = 42.

icontains/2:
Verifica se un valore o un intervallo
è contenuto in un altro intervallo (anche disgiunto).

Esempi:
?- icontains([1, 5], 6).
false.

?- icontains([[1, 3], [5, 8]], [6, 7]).
true.

compare_bounds/2:
Confronta due numeri o estremi di intervalli,
inclusi infinito positivo e negativo,
per verificare se il primo è minore o uguale al secondo.

Esempi:
?- compare_bounds(10, pos_infinity).
true.

ioverlap/2:
Verifica se due intervalli, anche disgiunti, si sovrappongono.

Esempi:
?- ioverlap([1, 2], [5, 6]).
false.

?- ioverlap([[1, 3], [5, 7]], [[2, 4], [6, 42]]).
true.

iplus/2:
Il predicato permette di trattare
un valore singolo come un intervallo con un singoletto.

Esempi:
?- iplus(3, Interval).
Interval = [3, 3].

iplus/3:
Somma di intervalli

Esempi:
?- iplus([1, 3], [2, 4], R).
R = [3, 7].

int_converter/2:
Converte un numero reale esteso in un singoletto.

Esempio:
?- int_convert(5, Interval).
Interval = [5, 5].

som_intervals/3:
Somma due intervalli sommando i rispettivi limiti inferiori e superiori,
verifica se il risultato è un intervallo valido.

iminus/3:
Differenza tra intervalli

Esempio:
?- iminus([5, 8], [2, 3], R).
R = [2, 5].

sot_intervals/3:
Sottrae due intervalli sottraendo
il limite superiore del secondo dal limite inferiore del primo,
il limite inferiore del secondo dal limite superiore del primo.
Verifica se il risultato è un intervallo valido.

itimes/2:
Il predicato verifica o converte un numero
o un intervallo in un intervallo valido.

Esempi:
?- itimes(4, Interval).
Interval = [4, 4].

?- itimes([], Interval).
Interval = [].

itimes/3:
Esegue la moltipliazione tra intervalli.

Esempi:
?- itimes(3, [2, 5], R).
R = [6, 15].

?- itimes([1, 3], [2, 4], R).
R = [2, 12].

minimum/5:
Il predicato calcola il minimo tra quattro numeri reali estesi,
gestendo casi speciali come infinito positivo e negativo.
Si utilizza per trovare il limite inferiore
risultante dalla moltiplicazione tra intervalli.

Esempi:
?- minimum(neg_infinity, 3, 7, 10, Min).
Min = neg_infinity.

?- minimum(3, pos_infinity, 7, 10, Min).
Min = 3.

maximum/5:
Il predicato calcola il massimo tra quattro numeri reali estesi,
gestendo casi speciali come infinito positivo e negativo.
Si utilizza per trovare
il limite superiore risultante dalla moltiplicazione tra intervalli.

Esempi:
?- maximum(neg_infinity, 4, 6, 3, Max).
Max = 6.

?- maximum(3, 7, 42, 9, Max).
Max = 42.

idiv/2:
Il predicato calcola l'inverso (reciproco)
dei limiti dell'intervallo o del numero singolo.

Esempio:
?- idiv([2, 4], R).
R = [0.25, 0.5].

idiv/3:
Il predicato esegue la divisione tra intervalli controllando
a che tipo di intervallo siano
il numeratore e il denominatore (positivo, negativo o misto),
per poi eseguire la divsione applicando al div_doX adeguata.

Esempi:
?- idiv([2, 5], [1, 4], R).
R = [0.5, 5]

?- idiv([-3 , 3], [1, 8], R).
R = [-3, 3]

idiv([-2, 4], [-12, -1], R).
R = [-4, 2]
