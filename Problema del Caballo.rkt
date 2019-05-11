#lang racket
(require "logica_matriz.rkt")
(require "graficos.rkt")

;________________________________________PDC-Sol___________________________________________________

;Ordenaiento de la lista por numero de posibles movimientos (menor a mayor)
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
; Da el numero de posibles movimientos que se pueden hacer desde un punto en el tablero
(define (numPosibles lista tamano matriz)
  (numPosibles_aux (buscarPosible (car lista) (cadr lista) tamano matriz #f))
)

(define (numPosibles_aux lista)
  (cond ((null? lista) 0)
        (else (+ 1 (numPosibles_aux (cdr lista))))
   )
)

;Obtiene el ultimo elemento de una lista
(define (obtenerUltimo lista)
  (cond ((null? lista) lista)
        ((null? (cdr lista)) (car lista))
        (else (obtenerUltimo (cdr lista))
         )
   )
)
; Eliminar el utlimo de una lista
(define (eliminarUltimo lista)
  (cond ((null? (cdr lista)) '())
        (else (cons (car lista) (eliminarUltimo (cdr lista))))
   )
)
; Primero de la ruta
(define (primeroRuta ruta)
  (cond ((null? ruta) '())
        (else (car ruta))
   )
)
; Comprueba que la ruta a tomar no se haya porbado todavia.
; return Punto no probado para tomar una ruta
(define (buscarCamino posiblesCaminos noCamino)
  (cond ((null? posiblesCaminos) '())
        ((buscar (car posiblesCaminos) noCamino) (buscarCamino (cdr posiblesCaminos) noCamino))
        (else (car posiblesCaminos))                                      
   )
)
;Comprueba que un elemento exista en una lista
(define (buscar ele lista)
  (cond ((null? lista) #f)
        ((equal? ele (car lista)) #t)
        (else (buscar ele (cdr lista)))
   )
)

; Funcion para devolverse hasta un punto valido
(define (puntoValido tamano matriz ruta noCamino)
  (cond  ((equal? (contarEle ruta) 1) '())
         ((null? (buscarCamino (buscarPosible (car (obtenerUltimo ruta)) (cadr (obtenerUltimo ruta)) tamano matriz #f) noCamino))
               (puntoValido tamano (insertar matriz 0 (car (obtenerUltimo ruta)) (cadr (obtenerUltimo ruta))) (eliminarUltimo ruta) (cons (obtenerUltimo ruta) noCamino))
         )
        (else (resolverProblema1 (buscarCamino (buscarPosible (car (obtenerUltimo ruta)) (cadr (obtenerUltimo ruta)) tamano matriz #f) noCamino) tamano matriz ruta noCamino )
         )
   )  
)

(define (PDC-Sol tamano pos)
  (resolverProblema1 pos tamano (creaMatriz tamano 0) '() '())

)

(define (resolverProblema1 pos tamano matriz ruta noCamino)
  (cond ((equal? (* tamano tamano) (contarEle ruta)) ruta)
        (else (resolverProblema_aux1 pos (buscarPosible (car pos) (cadr pos) tamano matriz #f) tamano (insertar matriz 1 (car pos) (cadr pos)) (append ruta (list pos)) noCamino)
         )
  )
)

(define (resolverProblema_aux1 pos listaPosibles tamano matriz ruta noCamino)
  (cond ((null? listaPosibles) (cond ((equal? (* tamano tamano) (contarEle ruta)) ruta )
                                     (else (puntoValido tamano (insertar matriz 0 (car pos) (cdr pos)) (eliminarUltimo ruta) (cons (obtenerUltimo ruta) noCamino)))
                               )
         )
        (else (resolverProblema1 (car (Quicksort listaPosibles tamano matriz)) tamano matriz ruta noCamino)
        )
   )
)

;________________________________________PDC-Todas________________________________________________________________________________________________________________

;Función principal para solucionar el problema
;tamaño: tamaño de la matriz
;pos: posición inicial del caballo
;retorna: una lista con todos los path los cuales llegan a dar una solución.
(define (PDC-Todas tamano pos)
  (unirSoluciones tamano (resolverProblema pos tamano (creaMatriz tamano 0)))
)

;Llama la función para buscar posibles caminos y elimina los que son nulos.
;fila: fila donde se encuentra el caballo.
;columna: columna donde se encuentra el caballo.
;tamano: tamaño de la matriz.
;matriz: matriz lógica encargada de llevar el conteo donde ya ha estado el caballo.
;flag: se utiliza para saber de que manera se quiere revisar la matriz (al encontrar 0's sea t o f)
;retorna: una lista con los posibles caminos reales del caballo.
(define (buscarPosible fila columna tamano matriz flag)
  (EliminarNulos (buscarPosible_aux fila columna tamano matriz flag))
)

;A partir de una posición busca los posibles caminos que puede tomar el caballo.
;fila: fila donde se encuentra el caballo.
;columna: columna donde se encuentra el caballo.
;tamano: tamaño de la matriz.
;matriz: matriz lógica encargada de llevar el conteo donde ya ha estado el caballo.
;flag: se utiliza para saber de que manera se quiere revisar la matriz (al encontrar 0's sea t o f)
;retorna: una lista con los posibles lugares que el caballo pueda ir.
(define (buscarPosible_aux fila columna tamano matriz flag)
     (list
     (cond ((esValido (- fila 2) (- columna 1) tamano matriz flag) (list (- fila 2) (- columna 1)))
           (else '()))
     (cond ((esValido (- fila 2) (+ columna 1) tamano matriz flag) (list (- fila 2) (+ columna 1)))
           (else '()))
     (cond ((esValido (- fila 1) (- columna 2) tamano matriz flag) (list (- fila 1) (- columna 2)))
           (else '()))
     (cond ((esValido (- fila 1) (+ columna 2) tamano matriz flag) (list (- fila 1) (+ columna 2)))
           (else '()))
     (cond ((esValido (+ fila 2) (- columna 1) tamano matriz flag) (list (+ fila 2) (- columna 1)))
           (else '()))
     (cond ((esValido (+ fila 2) (+ columna 1) tamano matriz flag) (list (+ fila 2) (+ columna 1)))
           (else '()))
     (cond ((esValido (+ fila 1) (- columna 2) tamano matriz flag) (list (+ fila 1) (- columna 2)))
           (else '()))
     (cond ((esValido (+ fila 1) (+ columna 2) tamano matriz flag) (list (+ fila 1) (+ columna 2)))
           (else '()))
     )
)

;Revisa si la posición dada se encuentra dentro del rango de la matriz.
;fila: fila a revisar.
;columna: columna a revisar.
;tamano: tamaño de la matriz.
;matriz: matriz lógica encargada de llevar el conteo donde ya ha estado el caballo.
;flag: se utiliza para saber de que manera se quiere revisar la matriz (al encontrar 0's sea t o f)
;retorna: true o false
(define (esValido fila columna tamano matriz flag)
  (cond
    ((or (> fila (- tamano 1)) (< fila 0) (< columna 0) (> columna (- tamano 1))) #f)
    ((not (equal? (get matriz fila columna) 0)) #f)
    ((and (equal? (get matriz fila columna) 0) flag) #t)
    (else #t)
  )
)


;Función encargada de recorrer el tablero (matriz) para buscar todos los caminos posibles.
;pos: posición en la que se encuentra el caballo actualmente.
;tamano: tamaño de la matriz.
;matriz: matriz lógica encargada de llevar el conteo donde ya ha estado el caballo.
(define (resolverProblema pos tamano matriz)
  (resolverProblema_aux pos (buscarPosible (car pos) (cadr pos) tamano matriz #f) tamano (insertar matriz 1 (car pos) (cadr pos)))
 )

;Funcion auxiliar que va creando el path por donde el caballo va pasando y sigue buscando por todas las posiciones.
;pos: posición en la que se encuentra el caballo actualmente.
;tamano: tamaño de la matriz.
;matriz: matriz lógica encargada de llevar el conteo donde ya ha estado el caballo.
(define (resolverProblema_aux pos listaBuscar tamano matriz)
  (cond
    ((null? listaBuscar)
     (cond
       ((revisaMatriz matriz) pos)
     (else '())
     )
    )
  (else
   (append (creaLista (listar pos (creaLista (resolverProblema (car listaBuscar) tamano matriz)) #f))
         (creaLista (creaLista(resolverProblema_aux pos (cdr listaBuscar) tamano matriz))))
   )
  ) 
)

;Une todos los posibles caminos encontrados por el caballo (solo los que sean soluciones).
;tamano: tamaño de la matriz, se utiliza para ver si la cantidad de elementos de la lista
;        es igual al producto con el mismo.
;lista: lista con todas las rutas tomadas para encontrar las soluciones.
(define (unirSoluciones tamano lista)
  (cond
    ((null? lista) '())
    ((= (contarEle (car lista)) (* tamano tamano))
     (cons (car lista) (unirSoluciones tamano (cdr lista))))
    (else
     (unirSoluciones tamano (cdr lista)))
    )
  )

;;Ejecutar 
;(PDC-Todas 5 '(0 0))

;____________________________________PDC-Test____________________________________________________________;

;Función principal
;tamano: tamaño del tablero.
;solucion: posible solución.
;retorna: el recorrido sobre la matriz, si la solución es correcta.
(define (PDC-Test tamano solucion)
  (revisaSolucion tamano solucion (creaMatriz tamano 0)))

;Valida si el tamaño de la solución es congruente con el tamaño del tablero,
;si lo es procede a verificarla.
;tamano: tamaño del tablero.
;solucion: posible solucion.
;matriz: matriz del tamaño indicado, será en la que se irá marcando el camino.
;retorna: de ser la solucion, la matriz con la ruta.
(define (revisaSolucion tamano solucion matriz)
  (cond
    ((not (= (contarEle solucion) (* tamano tamano)))
     (write "La ruta dada no recorre o recorre de más todas las casillas del tablero") #f)
    (else
     (revisaSol_aux tamano solucion (creaMatriz tamano 0) 1)
    )
   )
 )

;Revisa que el path dado en la solucion sea correcto, y va editando la matriz.
;tamano: tamaño del tablero.
;solucion: posible solucion.
;matriz: será en la que se irá marcando el camino.
;cont: es el encargado de ir marcando el orden de las casillas de la matriz.
;retorna: matriz con la ruta definida.
(define (revisaSol_aux tamano solucion matriz cont)
  (cond
    ((revisaMatriz matriz) matriz)
    ((seEncuentra (buscarPosible (caar solucion) (cadar solucion) tamano matriz #t) (cdr solucion))
     (revisaSol_aux tamano (cdr solucion) (insertar matriz cont (caar solucion) (cadar solucion)) (+ cont 1)))
    (else (write "La solución dada no es correcta") #f)
    )
  )

;Revisa si el siguiente par ordenado de la solucion es un posible y válido movimiento del caballo.
;listaPosibles: lista con los posibles movimientos del caballo.
;solucion: este debe estar en la lista de posibles movimientos para que sea cierto.
;retorna: true o false.
(define (seEncuentra listaPosibles solucion)
  (cond
    ((null? solucion) #t)
    ((null? listaPosibles) #f)
    ((and (= (caar listaPosibles) (caar solucion)) (= (cadar listaPosibles) (cadar solucion)) #t))
    (else
     (seEncuentra (cdr listaPosibles) solucion))
    )
  )
     
;Ejecutar 
;(PDC-Test 5 '((0 0) (2 1) (0 2) (1 0) (3 1) (4 3) (2 2) (1 4) (3 3) (4 1) (2 0) (0 1) (1 3) (3 4) (4 2) (3 0) (1 1) (0 3) (2 4) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0)))

;______________________________________PDC-Paint_________________________________________________

;(PDC-Paint 5 (PDC-Sol 5 '(1 0)))













