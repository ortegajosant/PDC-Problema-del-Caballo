;__________________________________________________________
(define (get matriz fil colum)
  (cond
    ((null? matriz)
     '())
    (else
     (getAux matriz fil colum 0))))

(define (getAux matriz fil colum filAux)
  (cond
    ((null? matriz)
     '())
    ((equal? fil filAux)
     (getAuxColum (car matriz) colum 0))
    (else
     (getAux (cdr matriz) fil colum (+ filAux 1)))))

(define (getAuxColum vec colum columAux)
  (cond
    ((null? vec)
     '())
    ((equal? colum columAux)
     (car vec))
    (else
     (getAuxColum (cdr vec) colum (+ columAux 1)))))

;----------------------------------------------------------

(define (esValido fila columna tamano matriz)
  (cond
    ((or (> fila (- tamano 1)) (< fila 0) (< columna 0) (> columna (- tamano 1))) #f)
    ((not (equal? (get matriz fila columna) 0)) #f)
    (else #t)
  )
)
; Elimina los elementos nulos de la lista de posibles 
(define (EliminarNulos lista)
  (cond ((null? lista) '())
        ((null? (car lista)) (EliminarNulos (cdr lista)))
        (else (cons (car lista) (EliminarNulos (cdr lista))))
   )
)
(define (buscarPosible fila columna tamano matriz)
  (EliminarNulos (buscarPosible_aux fila columna tamano matriz))
)


(define (buscarPosible_aux fila columna tamano matriz)
     (list
     (cond ((esValido (- fila 2) (- columna 1) tamano matriz) (list (- fila 2) (- columna 1)))
           (else '()))
     (cond ((esValido (- fila 2) (+ columna 1) tamano matriz) (list (- fila 2) (+ columna 1)))
           (else '()))
     (cond ((esValido (- fila 1) (- columna 2) tamano matriz) (list (- fila 1) (- columna 2)))
           (else '()))
     (cond ((esValido (- fila 1) (+ columna 2) tamano matriz) (list (- fila 1) (+ columna 2)))
           (else '()))
     (cond ((esValido (+ fila 2) (- columna 1) tamano matriz) (list (+ fila 2) (- columna 1)))
           (else '()))
     (cond ((esValido (+ fila 2) (+ columna 1) tamano matriz) (list (+ fila 2) (+ columna 1)))
           (else '()))
     (cond ((esValido (+ fila 1) (- columna 2) tamano matriz) (list (+ fila 1) (- columna 2)))
           (else '()))
     (cond ((esValido (+ fila 1) (+ columna 2) tamano matriz) (list (+ fila 1) (+ columna 2)))
           (else '()))
     )
)

;Ordenaiento de la lista por numeros de posibles movimientos (menor-mayor)
;-----------------------------------------------------------------------------
(define (Quicksort lista tamano matriz)
  (cond ((null? lista) (list))
  (else
   (quicksort_aux (cdr lista) (car lista) '() '() tamano matriz)
  )))

(define (quicksort_aux list pivote menores mayores tamano matriz)
  (cond
    ((null? list) (append (Quicksort menores tamano matriz) (cons pivote (Quicksort mayores))))
    ((<= (numPosibles (car list) tamano matriz) (numPosibles pivote tamano matriz)) (quicksort_aux (cdr list) pivote (cons (car list) menores) mayores))
    (else
       (quicksort_aux (cdr list) pivote menores (cons (car list) mayores))
     )
  )
)
;-----------------------------------------------------------------------------------------------
; Da el numero de posibles movimientos que se pueden hacer desde un punto en el tablero

(define (numPosibles lista tamano matriz)
  (numPosibles_aux (buscarPosibles (car lista) (cadr lista) tamano matriz))
)

(define (numPosibles_aux lista)
  (cond ((null? lista) 0)
        (else (+ 1 (numPosibles_aux (cdr lista))))
   )
)
;__________________________________________________________

;Función principal para solucionar el problema
;tamaño = tamaño de la matriz
;pos = posición inicial del caballo
(define (PDC-Sol tamano pos)
 (buscarPosible (car pos) (cadr pos) tamano '((0 0 0) (0 0 0) (0 0 0)))
;; ordernar la lista posibles 

)

(PDC-Sol 3 '(0 0))