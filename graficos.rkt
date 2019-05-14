#lang racket

(provide PDC-Paint-Graph)
(require racket/gui)
(define dec '()) ; Variable requerida para pintar en el canvas
(define n 0) ; Tamaño del tablero, necesario para escalar los elementos
(define largo 600) ; Largo del tablero
(define ancho 600) ; Ancho del tablero
(define movimientos '()) ; Movimientos realizados por el caballo
(define solucion '()) ; Pila con la solución del caballo
(define frame (new frame%
                   [label "PDC-PAINT"]
                   [width (+ 17 ancho)]
                   [height (+ 39 largo)])) ; Canvas para dibujar todo

(define knight (make-object bitmap% "knight.png"))


;; Función principal que dibuja todos los detalles del tablero
;; Es requerida por el canvas
(define (pintando canvas dc)
  (set! dec dc)
  (send dc set-scale 1 1)
  (colorear_tablero dc 0 0 (/ ancho n) #f)
  (thread pintar_solucion))

(define canvas1 (new canvas% [parent frame]
             [paint-callback pintando]))

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
     (sleep 0.5)
     (pintar_solucion))))

;; Función auxiliar que pinta un casilla específica
;; dc : se recibe dec, necesario para pintar
;; fila : fila de la matriz a pintar
;; colum : columna de la matriz a pintar
;; grosor : se refiere al tamaño de las casillas
;; retorna : ejecuta las acciones gráficas
(define (pintar_movimiento dc fila colum grosor)
  (send dc set-brush "gray" 'solid)
  (cond
    ((null? (cdr movimientos))
     (send dc set-pen "yellow" 3 'solid))
    (else
     (send dc set-pen "green" 1.5 'solid)))
  (send dc draw-rectangle (* colum grosor) (* fila grosor) grosor grosor)
  (cond
    ((null? solucion)
     (trazar_flecha grosor dc movimientos))
    (else
     (trazar_flecha grosor dc (cons (car solucion) movimientos))
  )))
  
;; Dibuja el patrón que seguido el caballo en la matriz
;; grosor : tamaño de las casillas en la matriz
;; movimientos : movimientos realizados por el caballo
;; retorna : ejecuta acciones gráficas
(define (trazar_flecha grosor dc movimientos)
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
           (+ (* (cadar movimientos) grosor) (/ grosor 2)) (+ (* (caar movimientos) grosor) (/ grosor 2)))
     (trazar_flecha grosor dc (cdr movimientos)))))


;------------------------------------------------------------------------------------------------------
;; Se colorea el tablero de juego
;; dc : se recibe dec, necesario para pintar
;; fila : fila de la matriz a pintar
;; colum : columna de la matriz a pintar
;; grosor : se refiere al tamaño de las casillas
;; current : #t o #f, true si es una casilla par, #f si es una casilla impar
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
;; dc : se recibe dec, necesario para pintar
;; fila : fila de la matriz a pintar
;; colum : columna de la matriz a pintar
;; grosor : se refiere al tamaño de las casillas
;; current : #t o #f, true si es una casilla par, #f si es una casilla impar
(define (colorear dc fila colum grosor current)
  (cond
    ((equal? current #t)
     (send dc set-brush "white" 'solid)
     (send dc draw-rectangle (* fila grosor) (* colum grosor) grosor grosor))
    (else
     (send dc set-brush "black" 'solid)
     (send dc draw-rectangle (* fila grosor) (* colum grosor) grosor grosor))))

;; Funcion que inicia el recorrido de la solución gráfica
;; num : tamaño del tablero
;; soluc : una solución para el problema del caballo
(define (PDC-Paint-Graph num soluc)
  (set! knight (make-object bitmap% "knight.png" 'png #f #f num))
  (set! n num)
  (set! solucion soluc)
  (set! movimientos '())
  (send frame show #t))

;; ---------------------------------------------------------------------------------
