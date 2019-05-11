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

;Cuenta la cantidad de elemento de una lista.
;lista: lista a la que se quiere obtener la cantidad de elementos.
;retorna:. cantidad de elementos.
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
    ((not (= (car fila) 0)) (revisaMatriz_aux (cdr fila)))
    (else #f)
    )
)

;Elimina los elementos nulos de la lista de posibles.
;lista: lista con varios elementos para eliminar solo los que están nulos.
;retorna: lista sin elementos nulos en ella.
(define (EliminarNulos lista)
  (cond ((null? lista) '())
        ((null? (car lista)) (EliminarNulos (cdr lista)))
        (else (cons (car lista) (EliminarNulos (cdr lista))))
   )
)

