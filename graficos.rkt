#lang racket

(require racket/gui)
(define dec '())
(define n 0)
(define largo 600)
(define ancho 600)
(define movimientos '())
(define solucion '())
(define frame (new frame%
                   [label "PDC-PAINT"]
                   [width (+ 17 ancho)]
                   [height (+ 34 largo)]
                   [stretchable-width (+ 34 largo)]
                   [stretchable-height (+ 34 largo)]
                   ))

;; Función principal que dibuja todos los detalles del tablero
(define (pintando canvas dc)
  (set! dec dc)
  (send dc set-scale 1 1)
  (colorear_tablero dc 0 0 (/ ancho n) #f)
  (thread pintar_solucion))

(new canvas% [parent frame]
             [paint-callback pintando])

;------------------------------------------------------------------------------------------------------

;; Se pinta el patrón de la solucion
(define (pintar_solucion)
  (cond
    ((null? solucion)
     (send dec set-text-foreground "red")
     (send dec draw-text "FINALIZADO!" 290 290))
    (else
     (pintar_movimiento dec (caar solucion) (cadar solucion) (/ ancho n))
     (set! solucion (cdr solucion))
     (sleep 1)
     (pintar_solucion))))

;; Función auxiliar que pinta un casilla específica
(define (pintar_movimiento dc fila colum grosor)
  (send dc set-brush "green" 'solid)
  (send dc draw-rectangle (* colum grosor) (* fila grosor) grosor grosor))

;------------------------------------------------------------------------------------------------------
;; Se colorea el tablero de juego
(define (colorear_tablero dc fila colum grosor current)
  (cond
    ((and (equal? (- n 1) fila) (equal? colum (- n 1)))
     (colorear dc fila colum grosor current))
    ((equal? colum n)
     (cond
       ((even? n)
        (colorear_tablero dc (+ fila 1) 0 grosor (not current)))
       (else
        (colorear_tablero dc (+ fila 1) 0 grosor current)))
     )
    (else
     (colorear dc fila colum grosor current)
     (colorear_tablero dc fila (+ colum 1) grosor (not current)))))

;; Función auxiliar que colorea el tablero de juego de blanco y negro
(define (colorear dc fila colum grosor current)
  (cond
    ((equal? current #t)
     (send dc set-brush "white" 'solid)
     (send dc draw-rectangle (* fila grosor) (* colum grosor) (* (+ 1 fila) grosor) (* (+ 1 colum) grosor)))
    (else
     (send dc set-brush "black" 'solid)
     (send dc draw-rectangle (* fila grosor) (* colum grosor) (* (+ 1 fila) grosor) (* (+ 1 colum) grosor)))))

;; Dibuja lineas verticales
(define (lineas_v dc num_lineas grosor)
  (cond
    ((equal? 1 num_lineas)
     (send dc draw-line (* num_lineas grosor) 0 (* num_lineas grosor) largo))
    (else
     (send dc draw-line (* num_lineas grosor) 0 (* num_lineas grosor) largo)
     (lineas_v dc (- num_lineas 1) grosor))))

;; Dibuja lineas horizontales
(define (lineas_h dc num_lineas grosor)
  (cond
    ((equal? 1 num_lineas)
     (send dc draw-line 0 (* num_lineas grosor) ancho (* num_lineas grosor)))
    (else
     (send dc draw-line 0 (* num_lineas grosor) ancho (* num_lineas grosor))
     (lineas_h dc (- num_lineas 1) grosor))))

;; Funcion que inicia el recorrido de la solución gráfica
;; num : tamaño del tablero
;; soluc : una solución para el problema del caballo
(define (PDC-Paint num soluc)
  (set! n num)
  (set! solucion soluc)
  (send frame show #t))

;; ----------------------------------------------------------
;; PRUEBA
(PDC-Paint 5   '((0 0) (1 2) (2 4) (4 3) (3 1) (1 0) (2 2) (0 3) (1 1) (3 0) (4 2) (3 4) (1 3) (0 1) (2 0) (4 1) (3 3) (1 4) (0 2) (2 1) (4 0) (3 2) (4 4) (2 3) (0 4)))
;; ----------------------------------------------------------
