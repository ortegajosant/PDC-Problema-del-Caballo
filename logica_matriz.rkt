#lang racket
(provide (all-defined-out))

;; Elimina un dato de la matriz en una posición específica
;; matriz: matriz en trabajo
;; fil: fila de la matriz
;; colum: columna de la matriz
(define (eliminar_pos matriz fil colum)
  (cond
    ((null? matriz)
     '())
    (else
     (insertar_aux matriz '() fil colum 0))))

;; Inserta el dato en una posición específica de la matriz
;; elem : elemento a insertar
;; matriz : matriz en trabajo
;; fil : fila de la matriz
;; colum : columna de la matriz 
(define (insertar matriz elem fil colum)
  (cond
    ((null? matriz)
     '())
    (else
     (insertar_aux matriz elem fil colum 0))))

;; Función auxiliar para insertar (busca la fila)
;; elem : elemento a insertar
;; matriz : matriz en trabajo
;; fil : fila de la matriz
;; colum : columna de la matriz
;; filaAux : conteo de la fila actual
(define (insertar_aux matriz elem fil colum filAux)
  (cond
    ((null? matriz)
     '())
    ((equal? fil filAux)
     (cons (insertar_colum elem (car matriz) colum 0) (cdr matriz)))
    (else
     (cons (car matriz) (insertar_aux (cdr matriz) elem fil colum (+ filAux 1))))))

;; Busca la columna a sustituir
;; elem : elemento a insertar
;; vec : vector para sustituir elemento
;; colum : columna de la matriz
;; columAux : conteo de columna actual 
(define (insertar_colum elem vec colum columAux)
  (cond
    ((null? vec)
     '())
    ((equal? colum columAux)
     (cons elem (cdr vec)))
    (else
     (cons (car vec) (insertar_colum elem (cdr vec) colum (+ columAux 1))))))

;; Obtiene un dato de una posición específica para una matriz
;; matriz : matriz de donde se obtiene el elemento buscado
;; fil : fila del elemento
;; colum : columna del elemento
(define (get matriz fil colum)
  (cond
    ((null? matriz)
     '())
    (else
     (getAux matriz fil colum 0))))

;; Función auxiliar para encontrar la fila
;; matriz : matriz de donde se obtiene el elemento buscado
;; fil : fila del elemento
;; colum : columna del elemento
;; filAux : conteo de la fila actual
(define (getAux matriz fil colum filAux)
  (cond
    ((null? matriz)
     '())
    ((equal? fil filAux)
     (getAuxColum (car matriz) colum 0))
    (else
     (getAux (cdr matriz) fil colum (+ filAux 1)))))

;; Función auxiliar para encintrar la columna
;; vec : fila de la matriz donde se busca el elemento
;; colum : columna donde puede estar el elemento buscado
;; columAux : conteo de la columna actual
(define (getAuxColum vec colum columAux)
  (cond
    ((null? vec)
     '())
    ((equal? colum columAux)
     (car vec))
    (else
     (getAuxColum (cdr vec) colum (+ columAux 1)))))
