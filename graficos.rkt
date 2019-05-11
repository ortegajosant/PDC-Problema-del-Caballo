#lang racket

(require racket/gui)
(define dec '())
(define n 0)
(define largo 600)
(define ancho 600)
(define movimientos '())
(define solucion '())
(define trazo (new dc-path%))
(define frame (new frame%
                   [label "PDC-PAINT"]
                   [width (+ 17 ancho)]
                   [height (+ 39 largo)]))

(define knight (make-object bitmap% "knight.png"))
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
     (pintar_movimiento dec (caar movimientos) (cadar movimientos) (/ ancho n))
     (send dec draw-bitmap knight (+ (* (cadar movimientos) (/ ancho n)) (/ (- (/ ancho n) (/ 512 n)) 2))
           (+ (* (caar movimientos) (/ ancho n)) (/ (- (/ ancho n) (/ 512 n)) 2)))
     (send dec set-text-foreground "red")
     (send dec set-pen "red" 2 'solid)
     (send dec set-brush "white" 'solid)
     (send dec draw-rectangle 246 260 110 22)
     (send dec draw-text "¡FINALIZADO!" 250 261))
    (else
     (cond
       ((not (null? movimientos))
        (pintar_movimiento dec (caar movimientos) (cadar movimientos) (/ ancho n))))
     (cond
       ((not (null? solucion))
        (send dec draw-bitmap knight (+ (* (cadar solucion) (/ ancho n)) (/ (- (/ ancho n) (/ 512 n)) 2))
              (+ (* (caar solucion) (/ ancho n)) (/ (- (/ ancho n) (/ 512 n)) 2)))))
     (set! movimientos (cons (car solucion) movimientos))
     (set! solucion (cdr solucion))
     (sleep 1)
     (pintar_solucion))))

;; Función auxiliar que pinta un casilla específica
(define (pintar_movimiento dc fila colum grosor)
  (send dc set-brush "gray" 'solid)
  (cond
    ((null? (cdr movimientos))
     (send dc set-pen "yellow" 3 'solid))
    (else
     (send dc set-pen "green" 3 'solid)))
  (send dc draw-rectangle (* colum grosor) (* fila grosor) grosor grosor)
  (trazar_flecha grosor dc))
  

(define (trazar_flecha grosor dc)
  (cond
    ((null? movimientos)
     (send dc draw-point 0 0))
    ((null? (cdr movimientos))
     (send dc set-pen "blue" 10 'solid)
     (send dc draw-point (+ (* (cadar movimientos) grosor) (/ grosor 2)) (+ (* (caar movimientos) grosor) (/ grosor 2)))
     (send dc set-pen "blue" 3 'solid))
    (else
     (send dc set-pen "blue" 10 'solid)
     (send dc draw-point (+ (* (cadar movimientos) grosor) (/ grosor 2)) (+ (* (caar movimientos) grosor) (/ grosor 2)))
     (send dc set-pen "blue" 3 'solid)
     (send dc draw-line (+ ( * (cadadr movimientos) grosor) (/ grosor 2)) (+ ( * (caadr movimientos) grosor) (/ grosor 2))
           (+ (* (cadar movimientos) grosor) (/ grosor 2)) (+ (* (caar movimientos) grosor) (/ grosor 2))))))


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
     (send dc draw-rectangle (* fila grosor) (* colum grosor) grosor grosor))
    (else
     (send dc set-brush "black" 'solid)
     (send dc draw-rectangle (* fila grosor) (* colum grosor) grosor grosor))))

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
  (set! knight (make-object bitmap% "knight.png" 'png #f #f num))
  (set! n num)
  (set! solucion soluc)
  (send frame show #t))

;; ---------------------------------------------------------------------------------
;; PRUEBA
(PDC-Paint 40 '((0 2) (2 1) (1 3) (3 4) (4 2) (6 3) (7 5) (5 6) (3 7) (4 5) (2 6) (0 7) (1 5) (2 3) (1 1) (3 0) (5 2) (7 1) (5 0)))