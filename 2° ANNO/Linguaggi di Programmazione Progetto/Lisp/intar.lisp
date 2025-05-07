(defconstant +pos-infinity+ '+pos-infinity+)
(defconstant +neg-infinity+ '+neg-infinity+)
;; Intervallo vuoto
(defconstant +empty-interval+ nil)


;; Funzione intervallo totale
(defun whole-interval () (cons '+neg-infinity+ '+pos-infinity+))

;; Funzione per restituire l'intervallo vuoto
(defun empty-interval ()
  "Restituisce l'intervallo vuoto."
  +empty-interval+)

(defun +e (&optional (x 0) (y 0))
  (cond
    ;; Caso dove x o y sono +inf -inf
    ((and (eq x +pos-infinity+) (eq y +neg-infinity+))
     (error "Undefined operation: +inf + -inf"))
    ((and (eq x +neg-infinity+) (eq y +pos-infinity+))
     (error "Undefined operation: -inf + +inf"))

    ((or (eq x +pos-infinity+) (eq y +pos-infinity+)) +pos-infinity+)
    ((or (eq x +neg-infinity+) (eq y +neg-infinity+)) +neg-infinity+)
    ((and (eq x +neg-infinity+) (eq y 0)) +neg-infinity+)
    ((and (eq x 0) (eq y +neg-infinity+)) +neg-infinity+)
    ((and (eq x 0) (eq y +pos-infinity+)) +pos-infinity+)
    ((and (eq x +pos-infinity+) (eq y 0)) +pos-infinity+)
    ((and (eq x +neg-infinity+) (eq y 0)) +neg-infinity+)
    ((and (eq x 0) (eq y +neg-infinity+)) +neg-infinity+)
    ;; Caso normale
    (t (+ x y))
    ))


(defun -e (x &optional y)
  (cond
    ;; Un solo argomento, ritorna l'opposto di x
    ((null y)
     (cond
       ((eq x '+pos-infinity+) '+neg-infinity+)
       ((eq x '+neg-infinity+) '+pos-infinity+)
       ((realp x) (- x))
       (t (error "Indefinito per il valore ~A" x))))

    ;; Entrambi gli argomenti sono reali
    ((and (realp x) (realp y)) (- x y))

    ;; Sottrazione con +pos-infinity+
    ((and (eq x '+pos-infinity+) (eq y '+pos-infinity+))
     (error "Indefinito: +pos-infinity+ - +pos-infinity+"))
    ((eq x '+pos-infinity+) '+pos-infinity+)
    ((eq y '+pos-infinity+) '+neg-infinity+)

    ;; Sottrazione con +neg-infinity+
    ((and (eq x '+neg-infinity+) (eq y '+neg-infinity+))
     (error "Indefinito: +neg-infinity+ - +neg-infinity+"))
    ((eq x '+neg-infinity+) '+neg-infinity+)
    ((eq y '+neg-infinity+) '+pos-infinity+)

    ;; Qualsiasi altro caso e' indefinito
    (t (error "Indefinito per i valori ~A e ~A" x y))))


(defun *e (&optional x y)
  (cond
    ;; Nessun argomento, ritorna l'elemento neutro per la moltiplicazione
    ((and (null x) (null y)) 1)

    ;; con 0  e infinito
    ((or (and (eq x 0) (eq y +neg-infinity+))
	 (and (eq y 0) (eq x +neg-infinity+)))
     (error "Indefinito: 0 * - infinito"))

    ((or (and (eq x 0) (eq y '+pos-infinity+))
	 (and (eq y 0) (eq x '+pos-infinity+)))
     (error "Indefinito: 0 * + infinito"))


    ;; Un solo argomento, ritorna quel valore
    ((and x (null y)) x)
    ((and y (null x)) y)
    ((and (realp x) (realp y)) (* x y))

    ;; gestione infiniti * infiniti
    ((or (and (eq x +pos-infinity+) (eq y +pos-infinity+))
	 (and (eq x +neg-infinity+) (eq y +neg-infinity+)))
     +pos-infinity+
     )

    ((or (and (eq x +pos-infinity+) (eq y +neg-infinity+))
         (and (eq x +neg-infinity+) (eq y +pos-infinity+))
         )
     +neg-infinity+
     )

    ;; Moltiplicazione con +pos-infinity+
    ((or (and (eq x +pos-infinity+) (plusp y))
         (and (eq y +pos-infinity+) (plusp x))
         ) +pos-infinity+)

    ;; Moltiplicazione con +neg-infinity+
    ((or (and (eq x +pos-infinity+) (minusp y) )
         (and (eq y +pos-infinity+) (minusp x) )
	 )
     +neg-infinity+
     )

    ;; Moltiplicazione con -pos-infinity+
    ((or (and (eq x +neg-infinity+) (plusp y))
         (and (eq y +neg-infinity+) (plusp x))
	 )
     +neg-infinity+
     )
    ;; Moltiplicazione con -neg-infinity+
    ((or (and (eq x +neg-infinity+) (minusp y))
         (and (eq y +neg-infinity+) (minusp x))
	 )
     +pos-infinity+
     )

    ;; Qualsiasi altro caso e' indefinito
    (t (error "Indefinito per i valori ~A e ~A" x y))
    ))


(defun /e (x &optional y)
  (cond
    ;; Caso: nessun argomento fornito
    ((not x)
     (error "Serve un argomento"))

    ;; Caso: divisione unaria, restituisce il reciproco di x
    ((or (not y) (null y))
     (cond
       ;; Divisione unaria con infinito positivo
       ((eq x '+pos-infinity+)
        0)  ; Reciproco di +infinito e' 0

       ;; Divisione unaria con infinito negativo
       ((eq x '+neg-infinity+)
        0)  ; Reciproco di -infinito e' 0

       ;; Divisione unaria con 0
       ((eq x 0)
        (error "Indeterminate form: division by zero."))

       ;; Divisione unaria con numero reale diverso da zero
       ((realp x)
        (/ 1 x))  ; Reciproco di un numero reale

       ;; Caso non gestito
       (t
        (error "Invalid argument type."))))

    ;; Caso: divisione binaria, x e y sono forniti
    (t
     (cond
       ;; Divisione per zero
       ((eq y 0)
        (error "Indefinito: divisione con 0"))

       ;; 0 diviso un qualsiasi numero non zero
       ((eq x 0)
        0)

       ;; infinito positivo diviso infinito positivo o negativo
       ((and (eq x '+pos-infinity+)
             (or (eq y '+pos-infinity+) (eq y '+neg-infinity+)))
        (error "Indefinito: infinito diviso infinito"))

       ;; infinito negativo diviso infinito positivo o negativo
       ((and (eq x '+neg-infinity+)
             (or (eq y '+pos-infinity+) (eq y '+neg-infinity+)))
        (error "Indefinito: infinito diviso infinito"))

       ;; infinito positivo diviso un numero positivo
       ((and (eq x '+pos-infinity+) (> y 0))
        '+pos-infinity+)

       ;; infinito positivo diviso un numero negativo
       ((and (eq x '+pos-infinity+) (< y 0))
        '+neg-infinity+)

       ;; infinito negativo diviso un numero positivo
       ((and (eq x '+neg-infinity+) (> y 0))
        '+neg-infinity+)

       ;; infinito negativo diviso un numero negativo
       ((and (eq x '+neg-infinity+) (< y 0))
        '+pos-infinity+)

       ;;numero diviso infinito
       ((and (numberp x) (or (eq y '+pos-infinity+) (eq y '+neg-infinity+)))
        0)

       ;; Divisione normale per numeri reali
       ((and (numberp x) (numberp y))
        (/ x y))

       ;; Caso non gestito
       (t
        (error "Invalid arguments for division.")
        )
       ))))

;; Funzione intervallo
(defun interval (&optional l h)
  (cond
    ;; nessun argomento fornito, restituisce l'intervallo vuoto
    ((and (null l) (null h))
     +empty-interval+)
    ;; solo l  fornito, restituisce un intervallo singleton [l, l]
    ((and l (null h))
     (cond ((extended-real-p l) (cons l l))
           (t (error "l invalido, deve essere un numero reale o infinito"))
           )
     )
    ((and l h)
     (cond
       ;; entrambi i numeri, creazione dell'intervallo se l<=h
       ((and (numberp l) (numberp h))
        (cond ((<= l h) (cons l h))
              (t (empty-interval)))
        )
       ;; intervallo con infinito positivo
       ((and (extended-real-p l) (eq h '+pos-infinity+))
        (cons l h))
       ;; intervallo con infinito negativo
       ((and (eq l '+neg-infinity+) (extended-real-p h))
        (cons l h))
       ;; intervallo completo
       ((and (eq l '+neg-infinity+) (eq h '+pos-infinity+))
        (whole-interval))
       (t (error "L e/o H input invalidi"))
       )
     )
    (t (error "Caso inaspettato nella funzione intervallo"))
    )
  )


(defun is-interval (x)
  "Restituisce T se x e'  un intervallo, altrimenti NIL."
  (cond
    ;; Verifica che x sia un intervallo vuoto
    ((eq x '+empty-interval+) T)
    ((eq x (empty-interval)) T)

    ;; Caso: Intervallo non vuoto rappresentato come cons
    ((and (consp x)                   ; Verifica che x sia una coppia cons
          (realp (car x))   ; Verifica che l sia un numero reale esteso
          (realp (cdr x))   ; Verifica che h sia un numero reale esteso
          (<= (car x) (cdr x)))       ; Verifica che l <= h
     T)

    ;; -inf come limite inferiore e un numero reale come limite superiore
    ((and (consp x)                   ; Verifica che x sia una coppia cons
          (eq (car x) '+neg-infinity+) ; Verifica che l sia -inf
          (extended-real-p (cdr x))    ; Verifica h = numero reale esteso
	  ) ; Verifica che h non sia -inf
     T)

    ;; +inf come limite superiore e un numero reale come limite inferiore
    ((and (consp x)                   ; Verifica che x sia una coppia cons
          (eq (cdr x) '+pos-infinity+) ; Verifica che h sia +inf
          (extended-real-p (car x))    ; Verifica l = numero reale esteso
	  )
     T)

    ;; Caso: Intervallo completo da -infinity a +infinity
    ((and (consp x)
          (eq (car x) '+neg-infinity+)  ; Verifica che l sia -inf
          (eq (cdr x) '+pos-infinity+)) ; Verifica che h sia +inf
     T)

    ;; Verifica intervall disgiunto

    ((and (listp x)
          (is-interval (car x))
          (is-interval (cdr x))
	  )
     T)

    ;; Caso: Non e'  un intervallo
    (t NIL)))

(defun extended-real-p (x)
  ;; Verifica se x e' un numero reale esteso.
  (or (numberp x)
      (eq x '+pos-infinity+)
      (eq x '+neg-infinity+)))

(defun is-empty (x)
  (cond
    ;; Caso: Se x non e' un intervallo, genera un errore
    ((not (is-interval x))
     (error "L'argomento fornito non e' un intervallo valido."))

    ;; Caso: Verifica se x e' uguale a +empty-interval+
    ((eq x +empty-interval+) T)

    ;; Caso: Non e' un intervallo vuoto
    (t NIL)))

(defun is-singleton (x)
  (cond
    ;; Caso: Se x non e' un intervallo, genera un errore
    ((not (is-interval x))
     (error "L'argomento fornito non e' un intervallo valido."))

    ((and (consp x)                ; Verifica che x sia una coppia cons
          (= (car x) (cdr x)))    ; Verifica che l = h
     T)

    ;; Caso: Non e' un intervallo singleton
    (t NIL)))


(defun inf (i)
  (cond
    ((and (is-interval i) (not(is-empty i)) (extended-real-p (car i)))
     (car i))
    ((and (listp i) (every 'is-interval i))
     (reduce 'min(mapcar 'inf i))) ;; inf ad ogni intervallo
    (t (error "L'argomento fornito non e' valido."))))


(defun sup (i)
  (cond
    ((and (is-interval i) (not (is-empty i))(extended-real-p (car i)))
     (cdr i))

    ;;Se e' una lista di intervalli,`sup` a ciascun intervallo e trova il max
    ((and (listp i) (every 'is-interval i))
     (reduce 'max (mapcar 'sup i)))

    ;; Caso generale: errore
    (t (error "L'argomento fornito non e' valido."))))


(defun contains (i x)
  (cond
    ;; generazione errore per i
    ((or (not (is-interval i)) (is-empty i))
     (error "L'argomento fornito non e'  valido."))

    ;; Caso in cui X e' un numero e i e' intervallo normale
    ((and (numberp x) (extended-real-p (car i)))
     (let ((l (inf i))
           (h (sup i)))
       (cond
         ;; Caso: l e h sono entrambi -inf e +inf, x e'  sicuramente contenuto
         ((and (eq l '+neg-infinity+) (eq h '+pos-infinity+))
          T)
         ;; Caso: l e'  -inf, x deve essere <= h
         ((eq l '+neg-infinity+)
          (<= x h))
         ;; Caso: h e' +inf, x deve essere >= l
         ((eq h '+pos-infinity+)
          (<= l x))
         ;; Caso Generale: x deve essere tra l e h
         (t (and (<= l x) (<= x h))))
       ))

    ;; caso in cui x e'  un infinito e i intervallo
    ((and (or (eq x '+pos-infinity+) (eq x '+neg-infinity+))
	  (extended-real-p (car i)))
     (let ((l (inf i))
           (h (sup i)))
       (cond
         ;; Caso: x e'  +inf e h e'  +inf
         ((and (eq x '+pos-infinity+) (eq h '+pos-infinity+))
          T)
         ;; Caso: x e' -inf e l e' -inf
         ((and (eq x '+neg-infinity+) (eq l '+neg-infinity+))
          T)
         ;; Caso generale: x non e'  contenuto
         (t NIL))))

    ;; Caso: Se x e' un intervallo singolo
    ((is-interval x)
     (let ((l2 (inf x))
           (h2 (sup x)))
       (and (contains i l2) (contains i h2))))

    ;; Caso in cui i e' un intervallo disgiunto (lista di intervalli)
    ((and (listp i) (every 'is-interval i))
     ;; Controlla se x e' contenuto in uno degli intervalli di i
     (some (lambda (int) (contains int x)) i))

    ;; Caso in cui x e' un intervallo disgiunto (lista di intervalli)
    ((and (listp x) (every 'is-interval x))
     ;; Verifica che ogni intervallo in x sia contenuto in uno degli int
     (every (lambda (sub-x) (contains i sub-x)) x))

    (t nil)))





(defun overlap (i1 i2)
  (cond
    ;; Controlla che entrambi siano intervalli validi
    ((or (not (is-interval i1)) (not (is-interval i2)))
     (error "Entrambi gli argomenti devono essere intervalli validi."))

    ;; Se uno degli intervalli e' vuoto, non possono sovrapporsi
    ((or (is-empty i1) (is-empty i2))
     nil)

    ;; i1 si sovrappone con qualsiasi intervallo
    ((and (eq (inf i1) +neg-infinity+)
          (eq (sup i1) +pos-infinity+))
     t)
    ;; i2 si sovrappone con qualsiasi intervallo
    ((and (eq (inf i2) +neg-infinity+)
          (eq (sup i2) +pos-infinity+))
     t)


    ;; Caso: Se i1 e' una lista di intervalli (intervallo disgiunto)
    ((and (listp i1) (every 'is-interval i1))
     ;; Verifica la sovrapposizione con uno degli intervalli di i1
     (some (lambda (sub-i1) (overlap sub-i1 i2)) i1))

    ;; Caso: Se i2 e' una lista di intervalli (intervallo disgiunto)
    ((and (listp i2) (every 'is-interval i2))
     ;; Verifica la sovrapposizione con uno degli intervalli di i2
     (some (lambda (sub-i2) (overlap i1 sub-i2)) i2))


    ;; Caso generale: i1 e i2 sono singoli intervalli
    ;; controlla la sovrapposizione gestendo gli infiniti
    (t (let ((l1 (inf i1))
             (h1 (sup i1))
             (l2 (inf i2))
             (h2 (sup i2)))
         (and (minore l1 h2) (minore l2 h1))))))






(defun i+ (&optional x y)
  (cond

    ;; Caso: Nessun argomento fornito, ritorna il singleton [0, 0]
    ((and (null x) (null y))
     (interval 0 0))

    ;; Torna lo stesso intervallo se non e' presente un argomento
    ((and (null y) (is-interval x)) x)
    ((and (null x) (is-interval y)) y)

    ;; Caso: Due argomenti forniti
    ((and x y)
     (let ((ix (cond
		 ((realp x) (interval x))
		 ((is-interval x) x)
		 (t
		  (error "il primo argomento non e' valido"))))
           (iy (cond
		 ((realp y) (interval y))
		 ((is-interval y) y)
		 (t
		  (error "il secondo argomento non e' valido")))))
       (cond
         ;; Se uno degli intervalli e' vuoto, il risultato e' vuoto
         ((or (is-empty ix) (is-empty iy))
          +empty-interval+)

         ;; intervallo normale
         ((and (extended-real-p (car ix)) (extended-real-p (car iy)))
          (interval (+e (inf ix) (inf iy)) (+e (sup ix) (sup iy)))
          )

         ;; gestione intervallo disgiunto
         ((and (listp ix) (extended-real-p (car iy)))
          (mapcar (lambda (int)
		    (interval (+e (inf int) (inf iy)) (+e (sup int) (sup iy))))
		  ix)
          )

         ((and (listp iy) (extended-real-p (car ix)))
          (mapcar (lambda (int)
		    (interval (+e (inf int) (inf ix)) (+e (sup int) (sup ix))))
		  iy)
          )

         ;; intervallo disgiunto sommato a intervallo disgiunto
         ((and (listp ix) (is-interval ix) (not (extended-real-p (car ix)))
               (listp iy) (is-interval iy) (not (extended-real-p (car iy))))
          (sum-disjoint-intervals ix iy))

         (t (error "errore"))
         )
       )
     )

    ;; Caso inatteso
    (t (error "Caso inatteso nella funzione i+."))))


(defun i- (x &optional y)
  (cond
    ;; Caso: Nessun argomento fornito, ritorna errore
    ((and (null x) (null y)) (interval 0 0))

    ;; Caso: Un argomento fornito, nega gli argomenti dell'intervallo
    ((and x (null y))
     (cond
       ;; Reciproco
       ((numberp x) (interval (- x) (- x)))
       ((and (eq +pos-infinity+ (sup x)) (numberp (inf x)))
        (interval +neg-infinity+ (- (inf x))))
       ((and (eq +neg-infinity+ (inf x)) (numberp (sup x)))
        (interval (- (sup x)) +pos-infinity+))
       ((and (eq +neg-infinity+ (inf x)) (eq +pos-infinity+ (sup x)))
        (interval +neg-infinity+ +pos-infinity+))
       ((and (listp x) (null y))
        (mapcar #'(lambda (interval) (i/ interval)) x))
       ;; Se x intervallo, creo intervallo con limiti negati
       ((and (extended-real-p (car x)) (is-interval x))
        (interval (- (sup x)) (- (inf x))))
       ((and (is-interval x) (listp x))
        (mapcar #'(lambda (interval) (i- interval)) x))
       (t (error "L'argomento fornito non e' un numero o intervallo"))))



    ;; Caso: Due argomenti forniti
    ((and x y)
     (let ((ix
            (cond
              ((realp x) (interval x))
              ((is-interval x) x)
              (t (error "il primo argomento non e' valido"))))
           (iy
            (cond
              ((realp y) (interval y))
              ((is-interval y) y)
              (t (error "il secondo argomento non e' valido")))))
       (cond
         ;; Se uno degli intervalli e' vuoto, il risultato e' vuoto
         ((or (is-empty ix) (is-empty iy))
          +empty-interval+)

         ;; intervallo normale
         ((and (extended-real-p (car ix)) (extended-real-p (car iy)))
          (interval (-e (inf ix) (sup iy)) (-e (sup ix) (inf iy)))
          )

         ;; gestione intervallo disgiunto
         ((and (listp ix) (extended-real-p (car iy)))
          (mapcar (lambda (int)
		    (interval (-e (inf int) (sup iy)) (-e (sup int) (inf iy))))
		  ix)
          )

         ((and (listp iy) (extended-real-p (car ix)))
          (mapcar (lambda (int)
		    (interval (-e (inf int) (sup ix)) (-e (sup int) (inf ix))))
		  iy)
          )

         ;; Case 7: Both x and y are disjoint intervals
         ((and (listp ix) (is-interval ix) (not (extended-real-p (car ix)))
               (listp y) (is-interval iy) (not (extended-real-p (car iy))))
	  ;; Compute all combinations of intervals from x and y using recursion
          (sub-disjoint-intervals ix iy))

         (t (error "errore"))
         )))

    ;; Caso inatteso
    (t (error "Caso inatteso nella funzione i-."))))

(defun i* (&optional x y)
  (cond
    ;;  Nessun argomento, ritorna [1, 1]
    ((and (null x) (null y)) (interval 1 1))

    ;; Un argomento e' un numero reale, trasformalo in un intervallo singleton
    ((and (numberp x) (numberp y)) (i* (interval x x) (interval y y)))
    ((and (numberp x) (null y)) (interval x x))
    ((and (null x) (numberp y)) (interval y y))

    ;; Se e' un solo intervallo allora lo moltiplico per 1
    ((and (is-interval x) (null y)) x)
    ((and (is-interval y) (null x)) y)

    ;; Entrambi gli argomenti sono intervalli rappresentati come cons
    ((and (is-interval x) (is-interval y)
          (extended-real-p (car x)) (extended-real-p (car y)))
     (let* ((x1 (*e (inf x) (inf y)))
            (x2 (*e (inf x) (sup y)))
            (x3 (*e (sup x) (inf y)))
            (x4 (*e (sup x) (sup y))))
       (cond
         ((or (eq x1 +neg-infinity+) (eq x2 +neg-infinity+)
              (eq x3 +neg-infinity+) (eq x4 +neg-infinity+))
          (cond
            ((or (eq x1 +pos-infinity+) (eq x2 +pos-infinity+)
                 (eq x3 +pos-infinity+) (eq x4 +pos-infinity+))
             (cons +neg-infinity+ +pos-infinity+)
             )
            (t  (cons +neg-infinity+ (maximum x1 x2 x3 x4)))
            )
          )
         ((or (eq x1 +pos-infinity+) (eq x2 +pos-infinity+)
              (eq x3 +pos-infinity+) (eq x4 +pos-infinity+))
          (cons (minimum x1 x2 x3 x4) +pos-infinity+)
          )
         (t (cons (minimum x1 x2 x3 x4) (maximum x1 x2 x3 x4)))
         )
       )
     )

    ;; gestione intervallo disgiunto
    ((and (listp x) (extended-real-p (car y)))
     (mapcar (lambda (int)
               (interval (*e (inf int) (inf y)) (*e (sup int) (sup y))))
             x)
     )

    ((and (listp y) (extended-real-p (car x)))
     (mapcar (lambda (int)
               (interval (*e (inf int) (inf x)) (*e (sup int) (sup x))))
             y)
     )

    ((and (listp x) (is-interval x) (not (extended-real-p (car x)))
          (listp y) (is-interval y) (not (extended-real-p (car y))))
     (multiply-disjoint-intervals x y))

    ;; Input non valido, genera un errore
    (t (error "Errore: x o y non e' un numero reale o un intervallo"))))


(defun maximum (x1 x2 x3 x4)
  (cond
    ;; Caso in cui uno dei valori e' +pos-infinity+.
    ((or (eq x1 +pos-infinity+) (eq x2 +pos-infinity+)
         (eq x3 +pos-infinity+) (eq x4 +pos-infinity+))
     +pos-infinity+)

    ;; tutti i valori sono +neg-infinity+, restituiamo +neg-infinity+.
    ((and (eq x1 +neg-infinity+) (eq x2 +neg-infinity+)
          (eq x3 +neg-infinity+) (eq x4 +neg-infinity+))
     +neg-infinity+)

    ;; ignora +neg-infinity+ e calcola il massimo tra i numeri validi.
    (t
     (let ((valid-numbers (list
                           (cond ((eq x1 +neg-infinity+) nil) (t x1))
                           (cond ((eq x2 +neg-infinity+) nil) (t x2))
                           (cond ((eq x3 +neg-infinity+) nil) (t x3))
                           (cond ((eq x4 +neg-infinity+) nil) (t x4)))))
       ;; Applica max solo ai numeri validi (non +neg-infinity+)
       (apply #'max (remove nil valid-numbers))))))


(defun minimum (x1 x2 x3 x4)
  (cond
    ;; Caso in cui uno dei valori e' +neg-infinity+.
    ((or (eq x1 +neg-infinity+) (eq x2 +neg-infinity+)
         (eq x3 +neg-infinity+) (eq x4 +neg-infinity+))
     +neg-infinity+)

    ;; tutti i valori sono +pos-infinity+, restituiamo +pos-infinity+.
    ((and (eq x1 +pos-infinity+) (eq x2 +pos-infinity+)
          (eq x3 +pos-infinity+) (eq x4 +pos-infinity+))
     +pos-infinity+)

    ;; ignora +pos-infinity+ e calcola il minimo tra i numeri validi.
    (t
     (let ((valid-numbers (list
                           (cond ((eq x1 +pos-infinity+) nil) (t x1))
                           (cond ((eq x2 +pos-infinity+) nil) (t x2))
                           (cond ((eq x3 +pos-infinity+) nil) (t x3))
                           (cond ((eq x4 +pos-infinity+) nil) (t x4)))))
       ;; Applica min solo ai numeri validi (non +pos-infinity+)
       (apply #'min (remove nil valid-numbers))))))

(defun i/ (x &optional y)
  (cond
    ;; Caso base
    ((null x) '())
    ;; Un argomento e' un numero reale, trasformalo in un intervallo singleton
    ((and (numberp x) (null y)) (interval (/e x) (/e x)))
    ((and (null x) (numberp y)) (interval (/e y) (/e y)))
    ((and (numberp x) (numberp y)) (i/ (interval x x) (interval y y)))

    ((and (extended-real-p (car x)) (is-interval x) (null y))
     (if (or (and (eq +neg-infinity+ (inf x))
                  (eq +pos-infinity+ (sup x)))
             (and (maggiore -1 (inf x)) (minore 1 (sup x))))
         (list (interval +neg-infinity+ (/e (sup x))) (interval (/e (inf x))
								+pos-infinity+))
         (interval (/e (sup x)) (/e (inf x)))
	 ))
    ;; gestione intervallo disgiunto
    ((and (listp x) (is-interval x) (null y) (not (extended-real-p (car x))))
     ;; x e' disgiunto
     (cons (i/ (first x))
           (i/ (rest x))))

    ;; x e' disgiunto, y e' normale
    ((and (listp x) (is-interval x) (not (extended-real-p (car x)))
          (extended-real-p (car y)) (is-interval y))
     (cons (i/ (first x) y)
           (i/ (rest x) y)))

    ;; y e' disgiunto, x e' normale
    ((and (extended-real-p (car x)) (is-interval x)
          (listp y) (is-interval y) (not (extended-real-p (car y))))
     (cons (i/ x (first y))
           (i/ x (rest y))))

    ;; Entrambi disgiunti
    ((and (listp x) (is-interval x) (not (extended-real-p (car x)))
          (listp y) (is-interval y) (not (extended-real-p (car y))))
     (divide-disjoint-intervals x y))

    ;; Entrambi gli argomenti sono intervalli normali
    ((and (is-interval x) (is-interval y))
     (let* ((a (inf x))
            (b (sup x))
            (c (inf y))
            (d (sup y)))
       (div-handler a b c d)
       )
     )

    ;; Input non valido, genera un errore
    (t (error "Errore: x o y non e' un numero reale o un intervallo"))))




(defun div-handler (a b c d)
  (cond
    ;; Controllo se e' M
    ((or (and (eq +neg-infinity+ a) (eq +pos-infinity+ b))
         (and (maggiore -1 a) (minore 1 b)))
     (cond
       ;; Controllo che sia N
       ((and (minore d 0) (or (eq +neg-infinity+ c) (minore c d)))
        (cond
          ;; Controllo che d = 0
          ((eql d 0) (interval +neg-infinity+ +pos-infinity+))
          (t (interval (/e b d) (/e a d)))
          )
        )
       ;; Controllo che sia M
       ((or (and (eq +neg-infinity+ c) (eq +pos-infinity+ d))
            (and (maggiore 0 c) (minore 0 d)))
        (interval +neg-infinity+ +pos-infinity+)
        )

       ;; Controllo se e' P
       ((and (maggiore c 0) (or (eq +pos-infinity+ d) (maggiore d c)))
        (cond
          ;; Controllo che c = 0
          ((eql c 0) (interval +neg-infinity+ +pos-infinity+))
          (t (interval (/e a c) (/e b c)))
          )
        ))

     )

    ;; Controllo se e' N0
    ((and (eq b 0) (or (eq +neg-infinity+ a) (minore a b)))
     ;; Ora controllare la C e la D
     (cond
       ;;Opzione P
       ((and (maggiore c 0) (or (eq +pos-infinity+ d) (maggiore d c)))
        (cond
          ;; Controllo che c = 0
          ((eql c 0) (interval +neg-infinity+ 0))
          (t (interval (/e a c) 0))
          )
        )
       ;;opzione M
       ((or (and (eq +neg-infinity+ c) (eq +pos-infinity+ d))
            (and (maggiore 0 d) (minore 0 c))
            ) (whole-interval)
        )
       ;;opzione N
       ((and (minore d 0) (or (eq +neg-infinity+ c) (minore c d)))
        (cond
          ((eql d 0) (interval 0 +pos-infinity+)) ;; Controllo che d = 0
          (t (interval 0 (/e a d)))
          )
        )
       )
     )
					;)

    ;; Controllo se e' N1
    ((and (minore b -1) (or (eq +neg-infinity+ a) (minore a b)))

     (cond
       ;;opzione P
       ;; Ora controllare la C e la D
       ((and (maggiore c 0) (or (eq +pos-infinity+ d) (maggiore d c)))
  	(cond
          ;; Se c = 0
          ((eql c 0) (let ((risultato (interval +neg-infinity+ (/e b d))))
                       (if (contains risultato 0)
			   (list (interval +neg-infinity+ 0)
				 (interval 0 (/e b d)))
			   risultato)))
          ;; Se c != 0
          (t (let ((risultato (interval (/e a c) (/e b d))))
               (if (contains risultato 0)
                   (list (interval (/e a c) 0) (interval 0 (/e b d)))
                   risultato))))
        )
       ;;opzione M ;;
       ((or (and (eq +neg-infinity+ c) (eq +pos-infinity+ d))
            (and (minore 0 d) (maggiore 0 c)))
        (list (interval +neg-infinity+ (/e b d))
              (interval (/e b c) +pos-infinity+))
        )

       ;;opzione N
       ((and (minore d 0) (or (eq +neg-infinity+ c) (minore c d)))
        (cond
          ;; Se d = 0
          ((eql d 0) (let ((risultato (interval (/e b c) +pos-infinity+)))
                       (if (contains risultato 0)
			   (list (interval (/e b c) 0)
				 (interval 0 +pos-infinity+))
			   risultato)))
          ;; Se d != 0
          (t (let ((risultato (interval (/e b c) (/e a d))))
               (if (contains risultato 0)
                   (list (interval (/e b c) 0) (interval 0 (/e a d)))
                   risultato))))
        )
       )
     )

    ;; Controllo se e' P0
    ((and (eq a 0) (or (maggiore b a) (eq +pos-infinity+ b)))
     ;; Ora controllare la C e la D
     (cond
       ;;opzione P
       ((and (maggiore c 0) (or (eq +pos-infinity+ d) (maggiore d c)))
        (cond
          ;; Controllo che c = 0
          ((eql c 0) (interval 0 +pos-infinity+))
          (t (interval 0 (/e b c)))
          ))

       ;;opzione M
       ((or (and (eq +neg-infinity+ c) (eq +pos-infinity+ d))
            (and (maggiore 0 d) (minore 0 c)))
        (whole-interval))

       ;;Opzione N
       ((and (minore d 0) (or (eq +neg-infinity+ c) (minore c d)))
        (cond
          ;; Controllo che d = 0
          ((eql d 0) (interval +neg-infinity+ 0))
          (t (interval (/e b d) 0))
          )
        )
       ))
					;)

    ;; Controllo se e' P1
    ((and (maggiore a 1) (or (eq +pos-infinity+ b) (maggiore b a)))
     (cond
       ;; Controllo che sia N
       ((and (minore d 0) (or (eq +neg-infinity+ c) (minore c d)))
        (cond
          ;; Se d = 0
          ((eql d 0) (let ((risultato (interval +neg-infinity+ (/e a c))))
                       (if (contains risultato 0)
			   (list (interval +neg-infinity+ 0)
				 (interval 0 (/e a c)))
			   risultato)))
          ;; Se d != 0
          (t (let ((risultato (interval (/e b d) (/e a c))))
               (if (contains risultato 0)
                   (list (interval (/e b d) 0) (interval 0 (/e a c)))
                   risultato))))
        )
       ;;opzione M ;;
       ((or (and (eq +neg-infinity+ c) (eq +pos-infinity+ d))
            (and (minore 0 d) (maggiore 0 c)))
        (list (interval +neg-infinity+ (/e a c)) (interval (/e a d)
							   +pos-infinity+))
        )

       ;; Controllo se e' P
       ((and (maggiore c 0) (or (eq +pos-infinity+ d) (maggiore d c)))
        (cond
          ;; Se c = 0
          ((eql c 0) (let ((risultato (interval (/e a d) +pos-infinity+)))
                       (if (contains risultato 0)
			   (list (interval (/e a d) 0)
				 (interval 0 +pos-infinity+))
			   risultato)))
          ;; Se c != 0
          (t (let ((risultato (interval (/e a d) (/e b c))))
               (if (contains risultato 0)
                   (list (interval (/e a d) 0) (interval 0 (/e b c)))
                   risultato))))
        )
       )
     )
    (t (error "errore"))
    ))


(defun maggiore (x1 x2)
  (cond
    ;; Caso in cui x1 meno infinito
    ((eq x1 +neg-infinity+) nil)
    ;; Caso piu infinito
    ((eq x1 +pos-infinity+) t)
    ((eq x2 +pos-infinity+) nil)
    ;; Caso in cui x2 piu infinito
    ((eq x2 +neg-infinity+) t)
    ((>= x1 x2) t)
    (t nil)
    )
  )


(defun minore (x1 x2)
  (cond
    ;; Caso in cui x1 meno infinito
    ((eq x1 +neg-infinity+) t)
    ;; Caso piu infinito
    ((eq x1 +pos-infinity+) nil)
    ((eq x2 +pos-infinity+) t)
    ;; Caso in cui x2 piu infinito
    ((eq x2 +neg-infinity+) nil)
    ((<= x1 x2) t)
    (t nil)
    )
  )


(defun divide-disjoint-intervals (x y)
  (if (null x)
      '()
      (append (divide-interval-with-disjoint (first x) y)
              (divide-disjoint-intervals (rest x) y))))

(defun divide-interval-with-disjoint (int-x y)
  (if (null y)
      '()
      (cons (i/ int-x (first y))
            (divide-interval-with-disjoint int-x (rest y)))))

(defun sub-disjoint-intervals (x y)
  (if (null x)
      '()
      (append (sub-interval-with-disjoint (first x) y)
              (sub-disjoint-intervals (rest x) y)))
  )

(defun sub-interval-with-disjoint (int-x y)
  (if (null y)
      '()
      (cons (i- int-x (first y))
            (sub-interval-with-disjoint int-x (rest y))))
  )

(defun multiply-disjoint-intervals (x y)
  (if (null x)
      '()
      (append (multiply-interval-with-disjoint (first x) y)
              (multiply-disjoint-intervals (rest x) y))))

(defun multiply-interval-with-disjoint (int-x y)
  (if (null y)
      '()
      (cons (i* int-x (first y))
            (multiply-interval-with-disjoint int-x (rest y)))))

(defun sum-disjoint-intervals (x y)
  (if (null x)
      '()
      (append (sum-interval-with-disjoint (first x) y)
              (sum-disjoint-intervals (rest x) y)))
  )

(defun sum-interval-with-disjoint (int-x y)
  (if (null y)
      '()
      (cons (i+ int-x (first y))
            (sum-interval-with-disjoint int-x (rest y)))))
