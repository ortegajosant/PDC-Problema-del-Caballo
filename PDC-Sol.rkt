#lang racket
(require "logica_matriz.rkt")

;Revisa si la posición dada se encuentra dentro del rango de la matriz.
;fila: fila a revisar.
;columna: columna a revisar.
;tamano: tamaño de la matriz.
;matriz: matriz lógica encargada de llevar el conteo donde ya ha estado el caballo.
;retorna: true o false
(define (esValido fila columna tamano matriz)
  (cond
    ((or (> fila (- tamano 1)) (< fila 0) (< columna 0) (> columna (- tamano 1))) #f)
    ((not (equal? (get matriz fila columna) 0)) #f)
    (else #t)
  )
)
; Elimina los elementos nulos de la lista de posibles.
;lista: lista con varios elementos para eliminar solo los que están nulos.
;retorna: lista sin elementos nulos en ella.
(define (EliminarNulos lista)
  (cond ((null? lista) '())
        ((null? (car lista)) (EliminarNulos (cdr lista)))
        (else (cons (car lista) (EliminarNulos (cdr lista))))
   )
)

;Llama la función para buscar posibles caminos y elimina los que son nulos.
;fila: fila donde se encuentra el caballo.
;columna: columna donde se encuentra el caballo.
;tamano: tamaño de la matriz.
;matriz: matriz lógica encargada de llevar el conteo donde ya ha estado el caballo.
;retorna: una lista con los posibles caminos reales del caballo.
(define (buscarPosible fila columna tamano matriz)
  (EliminarNulos (buscarPosible_aux fila columna tamano matriz))
)

;A partir de una posición busca los posibles caminos que puede tomar el caballo.
;fila: fila donde se encuentra el caballo.
;columna: columna donde se encuentra el caballo.
;tamano: tamaño de la matriz.
;matriz: matriz lógica encargada de llevar el conteo donde ya ha estado el caballo.
;retorna: una lista con los posibles lugares que el caballo pueda ir.
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


;Da el numero de posibles movimientos que se pueden hacer desde un punto en el tablero.
;lista: lista de los pares ordenados.
;tamano: tamaño de la matriz.
;matriz: matriz lógica encargada de llevar el conteo donde ya ha estado el caballo.
(define (numPosibles lista tamano matriz)
  (numPosibles_aux (buscarPosible (car lista) (cadr lista) tamano matriz))
)

;Funcion auxiliar para llevar el conteo de los posibles caminos.
;lista: lista de los pares ordenados.
(define (numPosibles_aux lista)
  (cond ((null? lista) 0)
        (else (+ 1 (numPosibles_aux (cdr lista))))
   )
)

;Función encargada de recorrer el tablero (matriz) para buscar todos los caminos posibles.
;pos: posición en la que se encuentra el caballo actualmente.
;tamano: tamaño de la matriz.
;matriz: matriz lógica encargada de llevar el conteo donde ya ha estado el caballo.
(define (resolverProblema pos tamano matriz)
  (resolverProblema_aux pos (buscarPosible (car pos) (cadr pos) tamano matriz) tamano (insertar matriz 1 (car pos) (cadr pos)))
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

;Crea la lista de listas que van a contener los path.
;ele: elemento que se desea agregar a la lista.
;lista: lista donde se agregará el elemento.
;retorna: la lista con el nuevo elemento.
(define (listar ele lista flag)
  (cond
    ((and (null? lista) flag) '())
    ((null? lista) ele)
    ((null? ele) lista)
    (else
     (cons (cons ele (creaLista(car lista))) (listar ele (cdr lista) #t))
     )
    )
  )

;Revisa si es una lista y si no, lo convierte en una.
;lista: elemento a revisar
;retorna: la lista si ya era list, sino el elemento en una lista.
(define (creaLista lista)
  (cond
    ((null? lista) lista)
    ((list? (car lista)) lista)
    (else
     (list lista)
     )
    )
  )

;Revisa si la matriz ha sido completamente recorrida por el caballo.
;matriz: matriz a revisar.
;retorna: true o false.
(define (revisaMatriz matriz)
  (cond
    ((null? matriz) #t)
    ((revisaMatriz_aux (car matriz)) (revisaMatriz (cdr matriz)))
    (else #f)
   )
)

;Auxiliar que recorre fila por fila revisando si ya han sido visitados.
;fila: fila a revisar.
;retorna: true o false.
(define (revisaMatriz_aux fila)
  (cond
    ((null? fila) #t)
    ((= (car fila) 1) (revisaMatriz_aux (cdr fila)))
    (else #f)
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

;Cuenta la cantidad de elemento de una lista.
(define (contarEle lista)
  (cond
    ((null? lista) 0)
    (else
     (+ 1 (contarEle (cdr lista))))
    )
  )

;Crea una matriz cuadrada del tamano deseado.
;tamano: tamaño de la matriz.
;cont: contador para detener la cantidad de filas.
;retorna: matriz cuadrada del tamaño requerido.
(define (creaMatriz tamano cont)
  (cond
    ((= tamano cont) '())
  (else
   (cons (creaFila tamano 0) (creaMatriz tamano (+ cont 1)))
    )
  )
 )

;Crea las filas para la matriz.
;tamano: tamaño de cada fila.
;cont: contador para la cantidad de columnas.
;retorna: una lista que representa la fila.
(define (creaFila tamano cont)
  (cond
    ((= cont tamano) '())
    (else
     (cons 0 (creaFila tamano (+ cont 1)))
     )
    )
  )

;Función principal para solucionar el problema
;tamaño: tamaño de la matriz
;pos: posición inicial del caballo
;retorna: una lista con todos los path los cuales llegan a dar una solución.
(define (PDC-Todas tamano pos)
  (unirSoluciones tamano (resolverProblema pos tamano (creaMatriz tamano 0)))
)

;;Ejecutar 
(PDC-Todas 5 '(0 0))