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
(define matriz '((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)))

(define (PDC-Sol tamano pos)
  (resolverProblema pos tamano matriz)
)

;; Funcion para llamar a resolver el problema recursivamente
(define (resolverProblema pos tamano matriz)
  (resolver_aux (buscarPosible (car pos) (cadr pos) tamano matriz) tamano (insertar matriz 1 (car pos) (cadr pos)) pos)
)

;; Auxiliar para obtener una solucion 
(define (resolver_aux lista tamano matriz pos_anterior)
  (cond ((null? lista) (list))
        (else (cons pos_anterior (resolverProblema (car (Quicksort lista tamano matriz)) tamano matriz)))       
  )
)

;; Auxiliar para obtener todas las solciones
(define (resolver_aux_todas lista tamano matriz pos_anterior)
  (cond ((null? lista) (list))
        (else (list (cons pos_anterior (resolverProblema (car (Quicksort lista tamano matriz)) tamano matriz)))
              
              )       
  )
)

(PDC-Sol 6 '(6 6))