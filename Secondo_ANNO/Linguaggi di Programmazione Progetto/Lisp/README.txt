# README
September 2024 Project E5P
Interval Arithmetic

Questo progetto si concentra sulla gestione e sull'operatività sugli intervalli
e sugli intervalli disgiunti. Un intervallo può essere rappresentato come una
coppia di valori (inf, sup) che indicano il limite inferiore e superiore.
Gli intervalli disgiunti sono rappresentati come liste di intervalli
non sovrapposti. Il codice permette di effettuare varie operazioni su
intervalli e intervalli disgiunti, come somma, sottrazione, moltiplicazione
e divisione.

## Costanti

+pos-infinity+: Rappresenta il valore di più infinito.
+neg-infinity+: Rappresenta il valore di meno infinito.
+empty-interval+: Rappresenta un intervallo vuoto.

## Funzioni di confronto

### maggiore(x1, x2):
Verifica se x1 è maggiore o uguale a x2, considerando infiniti.
### minore(x1, x2):
Verifica se x1 è minore o uguale a x2, considerando infiniti.

## Funzioni operazioni su numeri reali

### e+(x, y):
Somma due numeri reali o infiniti, con gestione dei valori infiniti.
### e-(x, y):
Sottrae y da x, con gestione degli infiniti.
### e*(x, y):
Moltiplica due numeri reali o infiniti, considerando infiniti.
### e/(x, y):
Divide x per y, gestendo divisioni per infinito.

## Funzioni sugli intervalli (i+, i-, i, i/)*

### i+(x, y):
Somma due intervalli.
### i-(x, y):
Sottrae due intervalli.
### i*(x, y):
Moltiplica due intervalli.
### i/(x, y):
Divide x per y (intervalli).
In questa implementazione la traccia diceva che la gestione dei risultati
doveva eliminare gli 0 in caso apparissero nel risultato. Nella nostra
implementazione sappiamo riconoscere se è presente uno zero ma non siamo
sicuri di come "eliminare" questo 0.
Abbiamo pensato di creare un intervallo disgiunto che non comprende lo zero,
però non è comunque una soluzione perfetta. Infatti se faccio contains con
l’intervallo creatosi risulterebbe presente lo 0. Per esempio noi abbiamo 
pensato che se dobbiamo escludere lo 0 in un intervallo (-2, 2) allora si 
potrebbe riscrivere l’intervallo in: (-2, 0) U (0, 2), in questa scrittura 
lo 0 non sarebbe compreso. Questa scrittura però implica l’esistenza di 
parentesi rotonde e quadrate che dicono se un numero è compreso oppure no.

L’altro modo sarebbe semplicemente aggiungere una stringa "\{0}". Dunque
verebbe un qualcosa come: (-2, 2) \{0}. Però lo 0 sarebbe comunque 
presente nell’intervallo.
Noi non abbiamo usato questa implementazione alternativa nel codice LISP.


## Funzioni sugli intervalli disgiunti

### divide-disjoint-intervals(x, y):
Divide due liste di intervalli disgiunti.
### multiply-disjoint-intervals(x, y):
Moltiplica due liste di intervalli disgiunti.
### sub-disjoint-intervals(x, y):
Sottrae due liste di intervalli disgiunti.
### sum-disjoint-intervals(x, y):
Somma due liste di intervalli disgiunti.

### divide-interval-with-disjoint(int-x, y):
Divide un singolo intervallo int-x con una lista di intervalli y.
### multiply-interval-with-disjoint(int-x, y):
Moltiplica un singolo intervallo int-x con una lista di intervalli y.
### sub-interval-with-disjoint(int-x, y):
Sottrae una lista di intervalli y da un singolo intervallo int-x.
### sum-interval-with-disjoint(int-x, y):
Somma un singolo intervallo int-x con una lista di intervalli y.

## Funzioni di gestione intervallo

### inf(i):
Restituisce l'infimo (limite inferiore) di un intervallo i.
### sup(i):
Restituisce il supremo (limite superiore) di un intervallo i.
### is-interval(i):
Verifica se i è un intervallo valido.
### is-empty(i):
Verifica se un intervallo i è vuoto.
### extended-real-p(x):
Verifica se x è un numero reale esteso (include infiniti).
### interval(l h):
Crea un intervallo con l come limite inferiore  e h come limite superiore.
### whole-interval():
Restituisce l'intervallo che rappresenta tutti i numeri reali,
ovvero l'intervallo che va da -∞ a +∞.
### is-singleton(i):
Verifica se un intervallo i è un "singleton",
cioè se l'infimo e il supremo dell'intervallo coincidono.
### contains(i, x):
Verifica se un intervallo i contiene un elemento x.
Il valore x può essere un numero reale esteso o un intervallo.
### overlap(i1, i2):
Verifica se due intervalli (o liste di intervalli) si
sovrappongono. Gestisce anche gli infiniti.
