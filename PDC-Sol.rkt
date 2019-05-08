#lang racket
(require "logica_matriz.rkt")

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
    ((null? list) (append (Quicksort menores tamano matriz) (cons pivote (Quicksort mayores tamano matriz))))
    ((<= (numPosibles (car list) tamano matriz) (numPosibles pivote tamano matriz)) (quicksort_aux (cdr list) pivote (cons (car list) menores) mayores tamano matriz))
    (else
       (quicksort_aux (cdr list) pivote menores (cons (car list) mayores) tamano matriz)
     )
  )
)
;-----------------------------------------------------------------------------------------------
; Da el numero de posibles movimientos que se pueden hacer desde un punto en el tablero

(define (numPosibles lista tamano matriz)
  (numPosibles_aux (buscarPosible (car lista) (cadr lista) tamano matriz))
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
(define matriz3 '((0 0 0) (0 0 0) (0 0 0)))
(define matriz '((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)))

(define (PDC-Sol tamano pos)
  (resolverProblema (cons pos (Quicksort (buscarPosible (car pos) (cadr pos) tamano matriz) tamano matriz)) tamano (insertar matriz 1 (car pos) (cadr pos)))
)

;; Funcion para llamar a resolver el problema recursivamente
(define (resolverProblema listaBuscar tamano matriz)
  (cond
    ((null? listaBuscar) (list))
  (else
   (cons (list (car listaBuscar) (resolverProblema (Quicksort (buscarPosible (caar listaBuscar) (cadar listaBuscar) tamano matriz) tamano matriz) tamano (insertar matriz 1 (caar listaBuscar) (cadar listaBuscar))))
         (resolverProblema (cdr listaBuscar) tamano matriz))   
   )
  )
)

(PDC-Sol 5 '(0 0))