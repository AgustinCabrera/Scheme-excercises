(define (cuadrado x)(* x x ))
(define (cubo x)(* x x x ))
(define (suma1 x) (+ x 1))
(define funcs(list suma1 cubo cuadrado))

( define miArbol '(1 (2 ( 4 () ())
                        (5  () ()))
                     (3 ()
                        ( 6 () ()))))
;representacion del arbol vacio
(define arbol-vacio '())
(define (vacio? arbol) (eq? arbol arbol-vacio) )
(define (raiz arbol) (car arbol))

;selecotres
(define (raiz arbol) (car arbol))
(define (izq arbol) (cadr arbol))
(define (der arbol) (caddr arbol))

;constructores
(define (crear-arbol raiz izq der) (list raiz izq der))
(define (crear-hoja raiz )(crear-arbol raiz arbol-vaico arbol-vacio))

(define (mapear-niveles LF AB )
  (if (null? AB)
      AB
      (if (vacio? AB)
          arbol-vacio
          (crear-arbol ((car LF ) (raiz AB ))
                      (mapear-niveles (cdr LF) (der AB))
                      (mapear-niveles (cdr LF) (izq AB))
           )
       )
    )
 )

(define personas '(("Cervantes" "Literatura" 1547 1616) 
 ("Velazquez" "Pintura" 1599 1660) 
 ("Picasso" "Pintura" 1881 1973) 
 ("Beethoven" "Musica" 1770 1823) 
 ("Poincare" "Ciencia" 1854 1912) 
 ("Quevedo" "Literatura" 1580 1654) 
 ("Goya" "Pintura" 1746 1828) 
 ("Einstein" "Ciencia" 1879 1955) 
 ("Mozart" "Musica" 1756 1791) 
 ("Botticelli" "Pintura" 1445 1510) 
 ("Borromini" "Arquitectura" 1599 1667) 
 ("Bach" "Musica" 1685 1750) 
 ))

;(define nombres 
 ;  (lambda (lista)
  ;   (if (null? lista)
   ;      '()
    ;     (cons (caar lista) (nombres (cdr lista))))))

(define (nombres lista)
  (map car lista))

(define es-pintor?
     (lambda (reg)
   (equal? (cadr reg) "Pintura")))

(define (filter f lista )
  (if (null? lista)
      '()
      (if (f (car lista))
          (cons (car lista)(filter f (cdr lista)))
          (filter f (cdr lista)))))

 (define (pintores bd)
 (map car(filter es-pintor? personas)))

 (define (selecciona bd)
   (lambda(actividad)
   (map car (filter (lambda (reg) (equal? (cadr reg) actividad)) bd))))
             