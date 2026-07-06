#lang eopl



;; =========================================================================
;; Steven Fernando Aragon - 2418804
;; Manuela Martinez Moncada - 2375458
;; Andrés Gerardo González  - 2416541
;; link github: https://github.com/CiberCarpincho/Proyecto-Mathflow.git
;; =========================================================================

;***********************************************************************************************************************
;***********************************************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos, 
;;;;; procedimientos recursivos y type checker

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {identifier = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<optional-type-exp> <identificador>}*(,)) <expression>
;;                      <proc-exp (arg-texps ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {<optional-type-exp> identifier ({<optional-type-exp> identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp result-texps proc-names arg-texpss idss bodies bodyletrec>
;;  <primitive>     ::= + | - | * | add1 | sub1 

;***********************************************************************************************************************
;***********************************************************************************************************************

;***********************************************************************************************************************
;**********************************************    Especificación Léxica   *********************************************
;***********************************************************************************************************************

(define scanner-spec-simple-interpreter
  '((white-sp
     (whitespace) skip)

    (comment
     ("#" (arbno (not #\newline))) skip)

    (identifier
     (letter (arbno (or letter digit))) symbol)

    (number
     (digit (arbno digit)) number)

    (number
     ("-" digit (arbno digit)) number)

    (number
     (digit (arbno digit) "." digit (arbno digit)) number)

    (number
     ("-" digit (arbno digit) "." digit (arbno digit)) number)
    
    (string
     ("\"" (arbno (not #\")) "\"")
     string)))



;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (string) string-exp)
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
    (expression
     ("if" expression "then" expression "else" expression "end")
     if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression
     ("(" expression (arbno expression) ")")
     app-exp)
    
    
    (expression
     ("var" "{" declaraciones "}")
     var-exp-definition)

    (expression
     ("const" "{" declaraciones "}")
     const-exp-definition)

    (expression
     ("switch" expression "{" (arbno caso-switch) "default" ":" expression "}")
     switch-exp)

    (expression
     ("while" expression "do" expression "done")
     while-exp)

    (expression
     ("for" identifier "in" expression "do" expression "done")
     for-exp)

    (expression
     ("return" expression)
     return-exp)

    (expression
     ("symbol" identifier)
     symbol-exp)

    (expression
     ("func" identifier
             "(" (separated-list identifier ",") ")"
             "{"
             (separated-list expression ";")
             "}")
     func-exp)

    (caso-switch
     ("case" expression ":" expression)
     un-caso-switch)

    (declaraciones
     (identifier "=" "(" expression ")" declaraciones-tail)
     declaraciones-exp)

    (declaraciones-tail
     ()
     fin-declaraciones)

    (declaraciones-tail
     ("," declaraciones)
     mas-declaraciones)
    
    (expression
     (identifier identifier-tail)
     identifier-exp)

    (identifier-tail
     ()
     lectura-id-tail)

    (identifier-tail
     ("=" expression)
     asignacion-id-tail)

    
    (expression
     ("begin" (separated-list expression ";") "end")
     begin-exp)
   
    ;;;;;;

    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("/") division-prim)
    (primitive ("%") modulo-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive (">") mayor-prim)
    (primitive ("<") menor-prim)
    (primitive (">=") mayor-igual-prim)
    (primitive ("<=") menor-igual-prim)
    (primitive ("==") igual-prim)
    (primitive ("<>") diferente-prim)
    ;añadidas:
    (primitive ("zero?") zero-test-prim)
    (primitive ("crear-lista") crear-lista-prim)
    (primitive ("vacio?") vacioq-prim)
    (primitive ("lista?") listaq-prim)
    (primitive ("cabeza") cabeza-prim)
    (primitive ("cola") cola-prim)
    (primitive ("ref-list") ref-list-prim)
    (primitive ("and") and-prim)
    (primitive ("or") or-prim)
    (primitive ("not") not-prim)
    (primitive ("longitud") longitud-prim)
    (primitive ("concatenar") concatenar-prim)
    (primitive ("print") print-prim)

    (primitive ("append") append-prim)
    (primitive ("set-list") set-list-prim)

    ;; Punto 5 — Adendo Diccionarios
    (primitive ("crear-diccionario") crear-diccionario-prim)
    (primitive ("diccionario?") diccionarioq-prim)
    (primitive ("ref-diccionario") ref-diccionario-prim)
    (primitive ("set-diccionario") set-diccionario-prim)
    (primitive ("claves") claves-prim)
    (primitive ("valores") valores-prim)

    (type-exp ("int") int-type-exp)
    (type-exp ("bool") bool-type-exp)
    (type-exp ("(" (separated-list type-exp "*") "->" type-exp ")")
              proc-type-exp)
    (optional-type-exp ("?")
      no-type-exp)
    (optional-type-exp (type-exp)
      a-type-exp)

    ; características adicionales
(expression ("false") false-exp)
(expression ("true") true-exp)
(expression ("null") null-exp)
(expression ("vacio") vacio-exp)
    (expression ("proc" "(" (separated-list optional-type-exp identifier ",") ")" expression)
                proc-exp)
    (expression ("letrec" (arbno optional-type-exp identifier
                                 "(" (separated-list optional-type-exp identifier ",") ")"
                                 "=" expression) "in" expression) 
                letrec-exp)
    ;;;;;;

    ))

;***********************************************************************************************************************
;***********************************************************************************************************************

;***********************************************************************************************************************
;************************       Tipos de datos para la sintaxis abstracta de la gramática      *************************
;***********************************************************************************************************************

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;***********************************************************************************************************************
;***********************************************************************************************************************


;***********************************************************************************************************************
;*******************************************    Parser, Scanner, Interfaz     ******************************************
;***********************************************************************************************************************

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

(provide scan&parse eval-program)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

(define interpretador-tipos
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (aux-interpretador  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

(define aux-interpretador
  (lambda (x)
    (if (type? (type-of-program x)) (eval-program  x) 'error) ))

(define interfaz-checker
  (sllgen:make-rep-loop  "-->" 
                         (lambda (pgm) (type-to-external-form (type-of-program pgm)))
                         (sllgen:make-stream-parser
                                  scanner-spec-simple-interpreter 
                                  grammar-simple-interpreter)))
;; Representacion de declaraciones var y const de MathFlow

(define crear-binding-mathflow
  (lambda (valor clase)
    (list 'binding-mathflow clase valor)))

(define binding-mathflow?
  (lambda (v)
    (and (pair? v)
         (eqv? (car v) 'binding-mathflow))))

(define crear-simbolo-mathflow
  (lambda (id)
    (list 'simbolo-mathflow id)))


(define simbolo-mathflow?
  (lambda (v)
    (and (pair? v)
         (eqv? (car v) 'simbolo-mathflow))))

(define simbolo-mathflow-id
  (lambda (v)
    (cadr v)))

(define crear-expresion-simbolica
  (lambda (operador izquierda derecha)
    (list 'expresion-simbolica
          operador
          izquierda
          derecha)))

(define expresion-simbolica?
  (lambda (v)
    (and (pair? v)
         (eqv? (car v) 'expresion-simbolica))))

(define expresion-simbolica-operador
  (lambda (v)
    (cadr v)))

(define expresion-simbolica-izquierda
  (lambda (v)
    (caddr v)))

(define expresion-simbolica-derecha
  (lambda (v)
    (cadddr v)))

(define valor-simbolico?
  (lambda (v)
    (or (simbolo-mathflow? v)
        (expresion-simbolica? v))))

(define verificar-no-simbolicos
  (lambda (operador args)
    (if (or (valor-simbolico? (car args))
            (valor-simbolico? (cadr args)))
        (eopl:error
         'operacion-simbolica
         "El operador ~s no puede aplicarse a simbolos o expresiones simbolicas"
         operador)
        #t)))

(define aplicar-operacion-aritmetica
  (lambda (operador operacion izquierda derecha)
    (if (or (valor-simbolico? izquierda)
            (valor-simbolico? derecha))
        (crear-expresion-simbolica
         operador
         izquierda
         derecha)
        (operacion izquierda derecha))))

(define binding-mathflow-clase
  (lambda (binding)
    (cadr binding)))

(define binding-mathflow-valor
  (lambda (binding)
    (caddr binding)))

(define actualizar-binding-mathflow
  (lambda (id nuevo-valor env)
    (let ((binding (apply-env env id)))
      (if (binding-mathflow? binding)
          (if (eqv? (binding-mathflow-clase binding) 'const)
              (eopl:error 'assign-exp
                          "No se puede modificar una constante: ~s"
                          id)
              (extend-env
               (list id)
               (list (crear-binding-mathflow nuevo-valor 'var))
               env))
          (eopl:error 'assign-exp
                      "El identificador no fue declarado con var: ~s"
                      id)))))





;***********************************************************************************************************************
;************************************************    El Interprete      ************************************************
;***********************************************************************************************************************

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

(define init-env
  (lambda ()
    (extend-env
     '(x y z f)
     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (identifier-exp 'y (lectura-id-tail)) (cons (primapp-exp (decr-prim) (cons (identifier-exp 'y (lectura-id-tail)) '())) '())))
                      (empty-env)))
     (empty-env))))

(define crear-resultado-eval
  (lambda (valor env)
    (list valor env)))

(define resultado-eval-valor
  (lambda (resultado)
    (car resultado)))

(define resultado-eval-env
  (lambda (resultado)
    (cadr resultado)))

(define eval-begin
  (lambda (exps env)
    (if (null? exps)
        'null
        (eval-begin-expressions exps env))))

(define eval-declaraciones
  (lambda (decls clase env)
    (cases declaraciones decls

      (declaraciones-exp (id value-exp tail)

        (let ((binding-existente
               (buscar-binding-opcional env id)))

         
          (if (and binding-existente
                   (simbolo-mathflow?
                    (cadr binding-existente)))

              (eopl:error
               'eval-declaraciones
               "El identificador ~s ya fue declarado como simbolo algebraico"
               id)

              (let ((valor
                     (eval-expression value-exp env)))

                (let ((nuevo-env
                       (extend-env
                        (list id)
                        (list
                         (crear-binding-mathflow
                          valor
                          clase))
                        env)))

                  (cases declaraciones-tail tail

                    (fin-declaraciones ()
                      nuevo-env)

                    (mas-declaraciones
                     (resto-decls)
                     (eval-declaraciones
                      resto-decls
                      clase
                      nuevo-env)))))))))))

(define eval-begin-expressions
  (lambda (exps env)
    (if (null? exps)
        'null
        (let* ((exp-actual (car exps))
               (resto (cdr exps))
               (resultado
                (eval-expression-con-env exp-actual env))
               (valor
                (resultado-eval-valor resultado)))

          (cond
            ;; Si encontramos return, dejamos de evaluar
            ;; inmediatamente y propagamos la señal.
            ((resultado-return? valor)
             valor)

            ;; Si era la última expresión, devolvemos su valor.
            ((null? resto)
             valor)

            ;; Si no, continuamos con el ambiente actualizado.
            (else
             (eval-begin-expressions
              resto
              (resultado-eval-env resultado))))))))

(define eval-switch-cases
  (lambda (valor casos default-exp env)
    (if (null? casos)
        (eval-expression default-exp env)
        (cases caso-switch (car casos)

          (un-caso-switch (case-exp result-exp)
            (if (equal? valor
                        (eval-expression case-exp env))
                (eval-expression result-exp env)
                (eval-switch-cases
                 valor
                 (cdr casos)
                 default-exp
                 env)))))))

;; Variante que propaga el ambiente (misma razón que eval-while-con-env):
;; una asignación dentro de un case debe verse después del switch.
(define eval-switch-cases-con-env
  (lambda (valor casos default-exp env)
    (if (null? casos)
        (eval-expression-con-env default-exp env)
        (cases caso-switch (car casos)
          (un-caso-switch (case-exp result-exp)
            (if (equal? valor
                        (eval-expression case-exp env))
                (eval-expression-con-env result-exp env)
                (eval-switch-cases-con-env
                 valor
                 (cdr casos)
                 default-exp
                 env)))))))

;; FIX propagación de mutaciones: antes eval-while devolvía solo 'null al
;; terminar, así que cualquier mutación hecha dentro del cuerpo (p. ej.
;; `contador = contador + 1;`) se perdía para el código que viene después
;; del while. eval-while-con-env sí retorna (valor . ambiente-final);
;; eval-while sigue existiendo para los contextos donde solo se necesita
;; el valor (p. ej. dentro de una sub-expresión aritmética).
(define eval-while-con-env
  (lambda (test-exp body-exp env)
    (if (true-value? (eval-expression test-exp env))
        (let* ((resultado
                (eval-expression-con-env body-exp env))
               (valor
                (resultado-eval-valor resultado)))

          (if (resultado-return? valor)

              ;; Detenemos el while y propagamos el return.
              resultado

              ;; Si no hubo return, continuamos.
              (eval-while-con-env
               test-exp
               body-exp
               (resultado-eval-env resultado))))

        (crear-resultado-eval 'null env))))

(define eval-while
  (lambda (test-exp body-exp env)
    (resultado-eval-valor (eval-while-con-env test-exp body-exp env))))

(define eval-begin-con-env
  (lambda (exps env)
    (if (null? exps)
        (crear-resultado-eval 'null env)
        (let ((resultado
               (eval-expression-con-env (car exps) env)))
          (if (null? (cdr exps))
              resultado
              (eval-begin-con-env
               (cdr exps)
               (resultado-eval-env resultado)))))))

(define eval-expression-con-env
  (lambda (exp env)
    (cases expression exp

      (begin-exp (exps)
        (eval-begin-con-env exps env))

      ;; --- Los siguientes casos antes caían en el "else" de abajo, que
      ;; llama a eval-expression (sin threading de ambiente) y por lo
      ;; tanto ROMPÍA var/const/func/if/switch/while/for cuando aparecían
      ;; como sentencias dentro de un begin, o como cuerpo de un
      ;; while/for/if. Ahora cada uno tiene su propia variante que
      ;; retorna (valor . ambiente-actualizado). ---

      (var-exp-definition (decls)
        (crear-resultado-eval 'null (eval-declaraciones decls 'var env)))

      (const-exp-definition (decls)
        (crear-resultado-eval 'null (eval-declaraciones decls 'const env)))

      (symbol-exp (id)
        (let ((binding-existente
         (buscar-binding-opcional env id)))

          (if (and binding-existente
             (binding-mathflow?
              (cadr binding-existente)))

              (eopl:error
               'symbol-exp
               "El identificador ~s ya fue declarado como variable o constante"
               id)

              (crear-resultado-eval
               'null
               (extend-env
                (list id)
                (list
                 (crear-simbolo-mathflow id))
                env)))))

      (func-exp (nombre ids body-exps)
        (crear-resultado-eval
         'null
         (extend-env-recursively
          (list nombre)
          (list ids)
          (list (begin-exp body-exps))
          env)))

      (if-exp (test-exp true-exp false-exp)
        (if (true-value? (eval-expression test-exp env))
            (eval-expression-con-env true-exp env)
            (eval-expression-con-env false-exp env)))

      (switch-exp (value-exp casos default-exp)
        (let ((valor (eval-expression value-exp env)))
          (eval-switch-cases-con-env valor casos default-exp env)))

      (while-exp (test-exp body-exp)
        (eval-while-con-env test-exp body-exp env))

      (for-exp (id iterable-exp body-exp)
        (let ((elementos (eval-expression iterable-exp env)))
          (eval-for-con-env id elementos body-exp env)))

      (identifier-exp (id tail)
        (cases identifier-tail tail

          (asignacion-id-tail (value-exp)
            (let ((nuevo-valor
                   (eval-expression value-exp env)))
              (crear-resultado-eval
               nuevo-valor
               (actualizar-binding-mathflow
                id
                nuevo-valor
                env))))

          (lectura-id-tail ()
            (crear-resultado-eval
             (eval-expression exp env)
             env))))

      (else
        (crear-resultado-eval
         (eval-expression exp env)
         env)))))

;; FIX acumulación en for: antes cada iteración partía siempre del MISMO
;; `env` original, así que algo como `suma = suma + i;` dentro del for no
;; se acumulaba (cada vuelta "olvidaba" lo que hizo la anterior). Ahora se
;; encadena el ambiente resultante de una iteración hacia la siguiente,
;; igual que ya se hacía correctamente en eval-while-con-env.
;; Nota de diseño (documentar en el repo): el identificador del for queda
;; con una nueva ligadura en cada iteración; si el mismo nombre se usa
;; después del for para otra variable, verá el último valor de iteración
;; en vez de su valor previo. Es una limitación conocida de esta
;; representación de ambientes (no exigida por el enunciado a resolver).
(define eval-for-con-env
  (lambda (id elementos body-exp env)
    (if (vacio-mathflow? elementos)
        (crear-resultado-eval 'null env)
        (let* ((env-iteracion
                (extend-env
                 (list id)
                 (list (cabeza-mathflow elementos))
                 env))
               (resultado
                (eval-expression-con-env
                 body-exp
                 env-iteracion))
               (valor
                (resultado-eval-valor resultado)))

          (if (resultado-return? valor)

              ;; Detenemos el for y propagamos el return.
              resultado

              ;; Si no hubo return, continuamos.
              (eval-for-con-env
               id
               (cola-mathflow elementos)
               body-exp
               (resultado-eval-env resultado)))))))

(define eval-for
  (lambda (id elementos body-exp env)
    (resultado-eval-valor (eval-for-con-env id elementos body-exp env))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (string-exp (text)
  (substring text 1 (- (string-length text) 1)))
      
      (identifier-exp (id tail)
        (cases identifier-tail tail

          (lectura-id-tail ()
            (let ((valor (apply-env env id)))
              (if (binding-mathflow? valor)
                  (binding-mathflow-valor valor)
                  valor)))

          (asignacion-id-tail (value-exp)
            (eopl:error 'eval-expression
                        "La asignacion debe evaluarse dentro de begin"))))
      
      
      (var-exp-definition (decl)
                          (eopl:error 'eval-expression
                                      "Semantica de var pendiente de implementar"))

      (const-exp-definition (decl)
                            (eopl:error 'eval-expression
                                        "Semantica de const pendiente de implementar"))

      
      (begin-exp (exps)
                 (eval-begin exps env))
      
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (switch-exp (value-exp cases default-exp)
                  (let ((valor
                         (eval-expression value-exp env)))
                    (eval-switch-cases
                     valor
                     cases
                     default-exp
                     env)))

      (while-exp (test-exp body-exp)
        (eval-while test-exp body-exp env))

      (for-exp (id iterable-exp body-exp)
               (let ((elementos
                      (eval-expression iterable-exp env)))
                 (eval-for id elementos body-exp env)))
      
      (return-exp (value-exp)
        (valor-return
         (eval-expression value-exp env)))


      (symbol-exp (id)
        (eopl:error 'eval-expression
                    "La declaracion symbol debe evaluarse dentro de begin"))

      (func-exp (nombre ids body-exps)
        (eopl:error 'eval-expression
                    "La definicion func debe evaluarse dentro de begin"))

      
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (proc-exp (args-texps ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (result-texps proc-names arg-texpss idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      
      (true-exp ()
                #t)

      (false-exp ()
                 #f)

      (null-exp ()
                'null)

      (vacio-exp ()
                 vacio-mathflow))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim ()
        (aplicar-operacion-aritmetica
         '+
         +
         (car args)
         (cadr args)))

      (substract-prim ()
        (aplicar-operacion-aritmetica
         '-
         -
         (car args)
         (cadr args)))

      (mult-prim ()
        (aplicar-operacion-aritmetica
         '*
         *
         (car args)
         (cadr args)))

      (division-prim ()
        (aplicar-operacion-aritmetica
         '/
         /
         (car args)
         (cadr args)))
      
      (modulo-prim ()
        (remainder (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (zero-test-prim () (zero? (car args)))
      (menor-prim ()
        (begin
          (verificar-no-simbolicos '< args)
          (< (car args) (cadr args))))

      (mayor-prim ()
        (begin
          (verificar-no-simbolicos '> args)
          (> (car args) (cadr args))))

      (mayor-igual-prim ()
        (begin
          (verificar-no-simbolicos '>= args)
          (>= (car args) (cadr args))))

      (menor-igual-prim ()
        (begin
          (verificar-no-simbolicos '<= args)
          (<= (car args) (cadr args))))

      (igual-prim ()
        (begin
          (verificar-no-simbolicos '== args)
          (equal? (car args) (cadr args))))

      (diferente-prim ()
        (begin
          (verificar-no-simbolicos '<> args)
          (not (equal? (car args) (cadr args)))))
      
      (and-prim ()
        (begin
          (verificar-no-simbolicos 'and args)
          (and (true-value? (car args))
               (true-value? (cadr args)))))

      (or-prim ()
        (begin
          (verificar-no-simbolicos 'or args)
          (or (true-value? (car args))
              (true-value? (cadr args)))))

      (not-prim ()
        (if (valor-simbolico? (car args))
            (eopl:error
             'operacion-simbolica
             "El operador not no puede aplicarse a simbolos o expresiones simbolicas")
            (not (true-value? (car args)))))

      (longitud-prim ()
        (string-length (car args)))

      (concatenar-prim ()
        (string-append (car args) (cadr args)))

      (print-prim ()
        ;; FIX: antes hacía (display (car args)) directo — para números,
        ;; strings y booleanos "colaba" por accidente parecido al
        ;; formato esperado, pero para listas y (ahora) diccionarios
        ;; mostraba la representación cruda de Racket en vez de
        ;; [1, 2, 3] o {"clave": valor}. valor-mathflow->string ya
        ;; existía para esto (se usaba dentro de lista-mathflow->string)
        ;; pero nunca se conectó con print.
        (begin
          (display (valor-mathflow->string (car args)))
          (newline)
          'null))

      (crear-lista-prim () (crear-lista-mathflow (car args) (cadr args)))
      (vacioq-prim () (vacio-mathflow? (car args)))
      (listaq-prim () (lista-mathflow? (car args)))


      (cabeza-prim () (cabeza-mathflow (car args)))
      (cola-prim () (cola-mathflow (car args)))
      (ref-list-prim () (ref-list-mathflow (car args) (cadr args)))
      (append-prim () (append-mathflow (car args) (cadr args)))
      (set-list-prim () (set-list-mathflow (car args) (cadr args) (caddr args)))

      ;; Punto 5 — Adendo Diccionarios
      ;; crear-diccionario recibe una cantidad variable de argumentos
      ;; (0, o pares clave,valor,clave,valor,...), por eso usa `args`
      ;; completo en vez de (car args)/(cadr args) como las demás.
      (crear-diccionario-prim () (crear-diccionario-mathflow args))
      (diccionarioq-prim () (diccionario-mathflow? (car args)))
      (ref-diccionario-prim () (ref-diccionario-mathflow (car args) (cadr args)))
      (set-diccionario-prim () (set-diccionario-mathflow (car args) (cadr args) (caddr args)))
      (claves-prim () (claves-diccionario-mathflow (car args)))
      (valores-prim () (valores-diccionario-mathflow (car args)))
     )
    )
  )

(define true-value?
  (lambda (valor)
    (cond
      ((eqv? valor #f) #f)
      ((and (number? valor) (zero? valor)) #f)
      ((and (string? valor) (string=? valor "")) #f)
      ((eqv? valor 'null) #f)
      (else #t))))

;***********************************************************************************************************************
;*******************************************  Punto 4 — Adendo Listas  **************************************************
;***********************************************************************************************************************
;; Representación interna elegida: una lista MathFlow es un par de Racket
;; (cons), y `vacio` es la lista vacía nativa '(). Esto corresponde
;; exactamente con la nota semántica del enunciado:
;;   crear-lista(x, xs) ≡ (x.xs)

;; -----------------------------------------------------------------------
;; secciones 4.0.1 a 4.0.4 del enunciado
;; -----------------------------------------------------------------------

;; Una lista MathFlow no vacía es un vector mutable:
;; #(lista-mathflow elemento resto)

(define vacio-mathflow '())

(define crear-lista-mathflow
  (lambda (elem lst)
    (vector 'lista-mathflow elem lst)))

(define vacio-mathflow?
  (lambda (lst)
    (null? lst)))

(define lista-mathflow?
  (lambda (x)
    (or
     (vacio-mathflow? x)
     (and (vector? x)
          (= (vector-length x) 3)
          (eqv? (vector-ref x 0) 'lista-mathflow)))))

;; -----------------------------------------------------------------------
;; secciones 4.0.6 y 4.0.8 del enunciado
;; -----------------------------------------------------------------------

(define cabeza-mathflow
  (lambda (lst)
    (if (vacio-mathflow? lst)
        (eopl:error 'cabeza
                    "No se puede obtener la cabeza de una lista vacia")
        (vector-ref lst 1))))

(define cola-mathflow
  (lambda (lst)
    (if (vacio-mathflow? lst)
        (eopl:error 'cola
                    "No se puede obtener la cola de una lista vacia")
        (vector-ref lst 2))))

;;NOTA PARA MANUELAAA!!!!!!
;; ref-list-mathflow devuelve vacio-mathflow como "no encontrado" cuando el
;; índice está fuera de rango. unificar esto con el
;; valor `null` propio de la sección 2.1 cuando esté definido.
(define ref-list-mathflow
  (lambda (lst i)
    (cond
      ((vacio-mathflow? lst) 'null)
      ((zero? i) (cabeza-mathflow lst))
      (else
       (ref-list-mathflow
        (cola-mathflow lst)
        (- i 1))))))


(define append-mathflow
  (lambda (lst1 lst2)
    (if (vacio-mathflow? lst1)
        lst2
        (crear-lista-mathflow
         (cabeza-mathflow lst1)
         (append-mathflow (cola-mathflow lst1) lst2)))))

(define set-list-mathflow
  (lambda (lst i valor)
    (cond
      ((vacio-mathflow? lst)
       (eopl:error 'set-list
                   "Indice fuera de rango"))

      ((zero? i)
       (begin
         (vector-set! lst 1 valor)
         lst))

      (else
       (begin
         (set-list-mathflow
          (cola-mathflow lst)
          (- i 1)
          valor)
         lst)))))


(define lista-mathflow->string
  (lambda (lst)
    (string-append "[" (lista-elems->string lst) "]")))

(define lista-elems->string
  (lambda (lst)
    (cond
      ((vacio-mathflow? lst) "")
      ((vacio-mathflow? (cola-mathflow lst)) (valor-mathflow->string (cabeza-mathflow lst)))
      (else (string-append
             (valor-mathflow->string (cabeza-mathflow lst))
             ", "
             (lista-elems->string (cola-mathflow lst)))))))

(define valor-mathflow->string
  (lambda (v)
    (cond
      ((eqv? v 'null)
       "null")
      ((diccionario-mathflow? v)
       (diccionario-mathflow->string v))
      ((simbolo-mathflow? v)
       (symbol->string
        (simbolo-mathflow-id v)))

      ((expresion-simbolica? v)
       (expresion-simbolica->string v))
      ((lista-mathflow? v)
       (lista-mathflow->string v))
      ((string? v)
       v)
      ((boolean? v)
       (if v "true" "false"))
      ((number? v)
       (number->string v))
      (else
       "valor-no-representable"))))
(define expresion-simbolica->string
  (lambda (expr)
    (string-append
     "("
     (valor-mathflow->string
      (expresion-simbolica-izquierda expr))
     " "
     (symbol->string
      (expresion-simbolica-operador expr))
     " "
     (valor-mathflow->string
      (expresion-simbolica-derecha expr))
     ")")))

;***********************************************************************************************************************
;*****************************************  Punto 5 — Adendo Diccionarios  **********************************************
;***********************************************************************************************************************
;; Representación interna elegida: un diccionario MathFlow es una lista de
;; Racket ETIQUETADA — (list 'diccionario-mathflow (list (clave . valor) ...))
;; — siguiendo el mismo estilo que ya usaron para `binding-mathflow` más
;; arriba en el archivo. La etiqueta es necesaria porque, sin ella, un
;; diccionario vacío `(list)` sería indistinguible de `vacio-mathflow`
;; (la lista vacía), y diccionario?/lista? necesitan poder diferenciarlos.

(define etiqueta-diccionario-mathflow 'diccionario-mathflow)

;; -----------------------------------------------------------------------
;; sección 5.0.1 del enunciado — crear-diccionario()
;; -----------------------------------------------------------------------

(define pares-desde-argumentos-planos
  ;; Convierte (clave1 valor1 clave2 valor2 ...) en ((clave1 . valor1) ...)
  (lambda (args)
    (if (null? args)
        '()
        (cons (cons (car args) (cadr args))
              (pares-desde-argumentos-planos (cddr args))))))

(define crear-diccionario-mathflow
  (lambda (args-planos)
    (vector
     etiqueta-diccionario-mathflow
     (pares-desde-argumentos-planos args-planos))))

(define diccionario-mathflow?
  (lambda (v)
    (and (vector? v)
         (= (vector-length v) 2)
         (eqv? (vector-ref v 0)
               etiqueta-diccionario-mathflow))))

(define diccionario-mathflow-pares
  (lambda (d)
    (vector-ref d 1)))

;; -----------------------------------------------------------------------
;; sección 5.0.3 del enunciado — ref-diccionario(dic, clave)
;; -----------------------------------------------------------------------

(define buscar-en-pares-mathflow
  (lambda (clave pares)
    (cond
      ((null? pares) 'null)
      ((equal? (caar pares) clave) (cdar pares))
      (else (buscar-en-pares-mathflow clave (cdr pares))))))

(define ref-diccionario-mathflow
  (lambda (d clave)
    (buscar-en-pares-mathflow clave (diccionario-mathflow-pares d))))

;; -----------------------------------------------------------------------
;; sección 5.0.4 del enunciado — set-diccionario(dic, clave, valor)
;; -----------------------------------------------------------------------

(define set-en-pares-mathflow
  (lambda (clave valor pares)
    (cond
      ((null? pares) (list (cons clave valor)))
      ((equal? (caar pares) clave) (cons (cons clave valor) (cdr pares)))
      (else (cons (car pares)
                  (set-en-pares-mathflow clave valor (cdr pares)))))))

(define set-diccionario-mathflow
  (lambda (d clave valor)
    (begin
      (vector-set!
       d
       1
       (set-en-pares-mathflow
        clave
        valor
        (diccionario-mathflow-pares d)))
      d)))

;; -----------------------------------------------------------------------
;; secciones 5.0.5 y 5.0.6 del enunciado — claves / valores
;; -----------------------------------------------------------------------
;; Nota: el resultado es una lista MathFlow. Como una lista MathFlow ya es
;; literalmente un par/lista de Racket (ver sección de Listas más arriba),
;; `map car`/`map cdr` sobre los pares produce directamente una lista con
;; la representación correcta, sin necesidad de convertir nada.

(define lista-racket->lista-mathflow
  (lambda (lst)
    (if (null? lst)
        vacio-mathflow
        (crear-lista-mathflow
         (car lst)
         (lista-racket->lista-mathflow (cdr lst))))))

(define claves-diccionario-mathflow
  (lambda (d)
    (lista-racket->lista-mathflow
     (map car (diccionario-mathflow-pares d)))))

(define valores-diccionario-mathflow
  (lambda (d)
    (lista-racket->lista-mathflow
     (map cdr (diccionario-mathflow-pares d)))))

;; -----------------------------------------------------------------------
;; Representación legible para `print` (análoga a lista-mathflow->string)
;; -----------------------------------------------------------------------

(define diccionario-mathflow->string
  (lambda (d)
    (string-append "{" (pares-diccionario->string (diccionario-mathflow-pares d)) "}")))

(define pares-diccionario->string
  (lambda (pares)
    (cond
      ((null? pares) "")
      ((null? (cdr pares)) (par-diccionario->string (car pares)))
      (else (string-append
             (par-diccionario->string (car pares))
             ", "
             (pares-diccionario->string (cdr pares)))))))

(define par-diccionario->string
  ;; Las claves siempre se muestran entre comillas (son cadenas en los
  ;; ejemplos del enunciado); los valores usan la misma representación
  ;; que ya se usa para listas, salvo que las cadenas también se citan
  ;; aquí para que coincida con el formato mostrado en la sección 5
  ;; (ej. {"nombre": "Ana María", "edad": 34}).
  (lambda (p)
    (string-append
     "\"" (valor-mathflow->string (car p)) "\": "
     (valor-diccionario-elemento->string (cdr p)))))

(define valor-diccionario-elemento->string
  (lambda (v)
    (if (string? v)
        (string-append "\"" v "\"")
        (valor-mathflow->string v))))

;***********************************************************************************************************************
;***********************************************************************************************************************

;***********************************************************************************************************************
;*********************************************   Definición tipos     **************************************************
;***********************************************************************************************************************

(define-datatype type type?
  (atomic-type (name symbol?))
  (proc-type
    (arg-types (list-of type?))
    (result-type type?))
  (tvar-type
    (serial-number integer?)
    (container vector?)))

;***********************************************************************************************************************
;*************************************************   Type Checker     **************************************************
;***********************************************************************************************************************

(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp) (type-of-expression exp (empty-tenv))))))

(define type-of-expression
  (lambda (exp tenv)
    (cases expression exp
      (lit-exp (number)
               int-type)
      (string-exp (text)
                  (eopl:error 'type-of-expression
                              "Las cadenas no usan el sistema de tipos heredado"))

      (true-exp ()
                bool-type)

      (false-exp ()
                 bool-type)

      (null-exp ()
        (eopl:error 'type-of-expression
                    "El valor null no usa el sistema de tipos heredado"))

      (vacio-exp ()
        (eopl:error 'type-of-expression
                    "El valor vacio no usa el sistema de tipos heredado"))

      (identifier-exp (id tail)
        (cases identifier-tail tail

          (lectura-id-tail ()
            (apply-tenv tenv id))

          (asignacion-id-tail (value-exp)
            (eopl:error 'type-of-expression
                        "La asignacion no usa el sistema de tipos heredado"))))
      
      (var-exp-definition (decl)
        (eopl:error 'type-of-expression
                    "var no usa el sistema de tipos heredado"))

      (const-exp-definition (decl)
        (eopl:error 'type-of-expression
                    "const no usa el sistema de tipos heredado"))

      
      (begin-exp (exps)
                 (eopl:error 'type-of-expression
                             "begin no usa el sistema de tipos heredado"))
      
      (if-exp (test-exp true-exp false-exp)
              (let ((test-type (type-of-expression test-exp tenv))
                    (false-type (type-of-expression false-exp tenv))
                    (true-type (type-of-expression true-exp tenv)))
                (check-equal-type! test-type bool-type test-exp)
                (check-equal-type! true-type false-type exp)
                true-type))

      (switch-exp (value-exp cases default-exp)
        (eopl:error 'type-of-expression
              "switch no usa el sistema de tipos heredado"))

      (while-exp (test-exp body-exp)
        (eopl:error 'type-of-expression
              "while no usa el sistema de tipos heredado"))

      (for-exp (id iterable-exp body-exp)
        (eopl:error 'type-of-expression
              "for no usa el sistema de tipos heredado"))
      (return-exp (value-exp)
        (eopl:error 'type-of-expression
              "return no usa el sistema de tipos heredado"))

      (symbol-exp (id)
        (eopl:error 'type-of-expression
                    "symbol no usa el sistema de tipos heredado"))

      (func-exp (nombre ids body-exps)
        (eopl:error 'type-of-expression
                    "func no usa el sistema de tipos heredado"))
      
      (proc-exp (texps ids body)
                (type-of-proc-exp texps ids body tenv))
      (primapp-exp (prim rands)
                   (type-of-application
                    (type-of-primitive prim)
                    (types-of-expressions rands tenv)
                    prim rands exp))
      (app-exp (rator rands)
               (type-of-application
                (type-of-expression rator tenv)
                (types-of-expressions rands tenv)
                rator rands exp))
      (let-exp (ids rands body)
               (type-of-let-exp ids rands body tenv))
      (letrec-exp (result-texps proc-names texpss idss bodies letrec-body)
                  (type-of-letrec-exp result-texps proc-names texpss idss bodies
                                      letrec-body tenv)))))

(define check-equal-type!            
  (lambda (t1 t2 exp)
    (cond
      ((eqv? t1 t2)  )  
      ((tvar-type? t1) (check-tvar-equal-type! t1 t2 exp))
      ((tvar-type? t2) (check-tvar-equal-type! t2 t1 exp))
      ((and (atomic-type? t1) (atomic-type? t2))
       (if (not
             (eqv?
               (atomic-type->name t1)
               (atomic-type->name t2)))
         (raise-type-error t1 t2 exp)
         #t))
      ((and (proc-type? t1) (proc-type? t2))
       (let ((arg-types1 (proc-type->arg-types t1))
             (arg-types2 (proc-type->arg-types t2))
             (result-type1 (proc-type->result-type t1))
             (result-type2 (proc-type->result-type t2)))
         (if (not
               (= (length arg-types1) (length arg-types2)))
           (raise-wrong-number-of-arguments t1 t2 exp)
           (begin
             (for-each
               (lambda (t1 t2)
                 (check-equal-type! t1 t2 exp))
               arg-types1 arg-types2)
             (check-equal-type!
               result-type1 result-type2 exp)))))
      (else (raise-type-error t1 t2 exp)))))

(define check-tvar-equal-type!
  (lambda (tvar ty exp)
    (if (tvar-non-empty? tvar)
      (check-equal-type! (tvar->contents tvar) ty exp)
      (begin
        (check-no-occurrence! tvar ty exp)
        (tvar-set-contents! tvar ty)))))

(define check-no-occurrence!
  (lambda (tvar ty exp)
    (letrec
      ((loop
         (lambda (ty1)
           (cases type ty1
             (atomic-type (name) #t) 
             (proc-type (arg-types result-type)
               (begin
                 (for-each loop arg-types)
                 (loop result-type)))
             (tvar-type (num vec)
               (if (tvar-non-empty? ty1)
                 (loop (tvar->contents ty1))
                 (if (eqv? tvar ty1)
                   (begin  
                    (display "me salgo") 
                   (raise-occurrence-check tvar ty exp))
                   #t)))))))
      (loop ty))))

(define raise-type-error
  (lambda (t1 t2 exp)
    (eopl:error 'check-equal-type!
      "Type mismatch: ~s doesn't match ~s in ~s~%"
      (type-to-external-form t1)
      (type-to-external-form t2)
      exp)))

(define raise-wrong-number-of-arguments
  (lambda (t1 t2 exp)
    (eopl:error 'check-equal-type!
      "Different numbers of arguments ~s and ~s in ~s~%"
      (type-to-external-form t1)
      (type-to-external-form t2)
      exp)))

(define raise-occurrence-check
  (lambda (tvnum t2 exp)
    (eopl:error 'check-equal-type!
      "Can't unify: ~s occurs in type ~s in expression ~s~%" 
      (type-to-external-form tvnum)
      (type-to-external-form t2)
      exp)))

(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (atomic-type (name) name)
      (proc-type (arg-types result-type)
                 (append
                  (arg-types-to-external-form arg-types)
                  '(->)
                  (list (type-to-external-form result-type))))
      (tvar-type (serial-number container) ;;; NUEVO
        (if (tvar-non-empty? ty)
          (type-to-external-form (tvar->contents ty))
          (string->symbol
            (string-append
              "tvar"
              (number->string serial-number))))))))

(define arg-types-to-external-form
  (lambda (types)
    (if (null? types)
        '()
        (if (null? (cdr types))
            (list (type-to-external-form (car types)))
            (cons
             (type-to-external-form (car types))
             (cons '*
                   (arg-types-to-external-form (cdr types))))))))

(define type-of-proc-exp
  (lambda (texps ids body tenv)
    (let ((arg-types (expand-optional-type-expressions texps tenv)))
      (let ((result-type
             (type-of-expression body
                                 (extend-tenv ids arg-types tenv))))
        (proc-type arg-types result-type)))))

(define type-of-application
  (lambda (rator-type actual-types rator rands exp)
    (let ((result-type (fresh-tvar)))
      (check-equal-type!
        rator-type
        (proc-type actual-types result-type)
        exp)
      result-type)))

(define type-of-primitive
  (lambda (prim)
    (cases primitive prim
      (add-prim ()
                (proc-type (list int-type int-type) int-type))
      (substract-prim ()
                      (proc-type (list int-type int-type) int-type))
      (mult-prim ()
                 (proc-type (list int-type int-type) int-type))
      (division-prim ()
        (proc-type (list int-type int-type) int-type))

      (modulo-prim ()
        (proc-type (list int-type int-type) int-type))
      
      (incr-prim ()
                 (proc-type (list int-type) int-type))
      (decr-prim ()
                 (proc-type (list int-type) int-type))
      (zero-test-prim ()
                      (proc-type (list int-type) bool-type))
      (menor-prim () (proc-type (list int-type int-type) bool-type))
      (mayor-prim () (proc-type (list int-type int-type) bool-type))
      (mayor-igual-prim ()
        (proc-type (list int-type int-type) bool-type))

      (menor-igual-prim ()
        (proc-type (list int-type int-type) bool-type))

      (igual-prim ()
        (proc-type (list int-type int-type) bool-type))

      (diferente-prim ()
        (proc-type (list int-type int-type) bool-type))

      (and-prim ()
        (proc-type (list bool-type bool-type) bool-type))

      (or-prim ()
        (proc-type (list bool-type bool-type) bool-type))

      (not-prim ()
        (proc-type (list bool-type) bool-type))
      
      (longitud-prim ()
        (eopl:error 'type-of-primitive
              "La primitiva longitud no usa el sistema de tipos heredado"))

      (concatenar-prim ()
        (eopl:error 'type-of-primitive
              "La primitiva concatenar no usa el sistema de tipos heredado"))

      (print-prim ()
        (eopl:error 'type-of-primitive
              "La primitiva print no usa el sistema de tipos heredado"))
      
      (crear-lista-prim ()
                        (eopl:error 'type-of-primitive
                                    "La primitiva crear-lista no usa el sistema de tipos heredado"))

      (vacioq-prim ()
                   (eopl:error 'type-of-primitive
                               "La primitiva vacio? no usa el sistema de tipos heredado"))

      (listaq-prim ()
                   (eopl:error 'type-of-primitive
                               "La primitiva lista? no usa el sistema de tipos heredado"))

      (cabeza-prim ()
                   (eopl:error 'type-of-primitive
                               "La primitiva cabeza no usa el sistema de tipos heredado"))

      (cola-prim ()
                 (eopl:error 'type-of-primitive
                             "La primitiva cola no usa el sistema de tipos heredado"))

      (ref-list-prim ()
                     (eopl:error 'type-of-primitive
                                 "La primitiva ref-list no usa el sistema de tipos heredado"))

      (append-prim ()
                   (eopl:error 'type-of-primitive
                               "La primitiva append no usa el sistema de tipos heredado"))

      (set-list-prim ()
                     (eopl:error 'type-of-primitive
              "La primitiva set-list no usa el sistema de tipos heredado"))

      (crear-diccionario-prim ()
        (eopl:error 'type-of-primitive
                    "La primitiva crear-diccionario no usa el sistema de tipos heredado"))

      (diccionarioq-prim ()
        (eopl:error 'type-of-primitive
                    "La primitiva diccionario? no usa el sistema de tipos heredado"))

      (ref-diccionario-prim ()
        (eopl:error 'type-of-primitive
                    "La primitiva ref-diccionario no usa el sistema de tipos heredado"))

      (set-diccionario-prim ()
        (eopl:error 'type-of-primitive
                    "La primitiva set-diccionario no usa el sistema de tipos heredado"))

      (claves-prim ()
        (eopl:error 'type-of-primitive
                    "La primitiva claves no usa el sistema de tipos heredado"))

      (valores-prim ()
        (eopl:error 'type-of-primitive
                    "La primitiva valores no usa el sistema de tipos heredado"))
      )))

(define types-of-expressions
  (lambda (rands tenv)
    (map (lambda (exp) (type-of-expression exp tenv)) rands)))

(define type-of-let-exp
  (lambda (ids rands body tenv)
    (let ((tenv-for-body
           (extend-tenv
            ids
            (types-of-expressions rands tenv)
            tenv)))
      (type-of-expression body tenv-for-body))))

(define type-of-letrec-exp
  (lambda (result-texps proc-names arg-optional-texpss idss bodies letrec-body tenv)
    (let ((arg-typess (map (lambda (texps)
                             (expand-optional-type-expressions texps tenv))
                           arg-optional-texpss))
          (result-types (expand-optional-type-expressions result-texps tenv)))
      (let ((the-proc-types
             (map proc-type arg-typess result-types)))
        (let ((tenv-for-body
               (extend-tenv proc-names the-proc-types tenv)))
          (for-each
           (lambda (ids arg-types body result-type)
             (check-equal-type!
              (type-of-expression
               body
               (extend-tenv ids arg-types tenv-for-body))
              result-type
              body))
           idss arg-typess bodies result-types)
          (type-of-expression letrec-body tenv-for-body))))))

;***********************************************************************************************************************
;*********************************************     Procedimientos     **************************************************
;***********************************************************************************************************************

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

(define-datatype resultado-return resultado-return?
  (valor-return
   (valor (lambda (x) #t))))

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
        (let ((resultado
               (eval-expression
                body
                (extend-env
                 ids
                 (map
                  (lambda (arg)
                    (crear-binding-mathflow arg 'var))
                  args)
                 env))))
          (if (resultado-return? resultado)
              (cases resultado-return resultado
                (valor-return (valor)
                  valor))
              resultado))))))

;***********************************************************************************************************************
;***********************************************     Ambientes     *****************************************************
;***********************************************************************************************************************

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

(define empty-env  
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))
(define buscar-binding-opcional
  (lambda (env sym)
    (cases environment env

      (empty-env-record ()
        #f)

      (extended-env-record (syms vals old-env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
              (list 'encontrado (list-ref vals pos))
              (buscar-binding-opcional old-env sym))))

      (recursively-extended-env-record
       (proc-names idss bodies old-env)
       (let ((pos
              (list-find-position sym proc-names)))
         (if (number? pos)
             (list 'encontrado
                   (closure
                    (list-ref idss pos)
                    (list-ref bodies pos)
                    env))
             (buscar-binding-opcional
              old-env
              sym)))))))

;***********************************************************************************************************************
;********************************************  Ambientes de tipos  *****************************************************
;***********************************************************************************************************************

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
    (syms (list-of symbol?))
    (vals (list-of type?))
    (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
        (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (list-ref vals pos)
            (apply-tenv env sym)))))))

;***********************************************************************************************************************
;****************************************************  Tipos  **********************************************************
;***********************************************************************************************************************

(define int-type
  (atomic-type 'int))
(define bool-type
  (atomic-type 'bool))

(define expand-type-expression
  (lambda (texp)
    (cases type-exp texp
      (int-type-exp () int-type)
      (bool-type-exp () bool-type)
      (proc-type-exp (arg-texps result-texp)
                     (proc-type
                      (expand-type-expressions arg-texps)
                      (expand-type-expression result-texp))))))

(define expand-type-expressions
  (lambda (texps)
    (map expand-type-expression texps)))

(define fresh-tvar
  (let ((serial-number 0))
    (lambda ()
      (set! serial-number (+ 1 serial-number))
      (tvar-type serial-number (vector '())))))

(define tvar-non-empty?
  (lambda (ty)
    (not (null? (vector-ref (tvar-type->container ty) 0)))))

(define expand-optional-type-expressions
  (lambda (otexps tenv)
    (map
      (lambda (otexp)
        (expand-optional-type-expression otexp tenv))
      otexps)))

(define expand-optional-type-expression
  (lambda (otexp tenv)
    (cases optional-type-exp otexp
      (no-type-exp () (fresh-tvar))
      (a-type-exp (texp) (expand-type-expression texp)))))

(define tvar->contents
  (lambda (ty)
    (vector-ref (tvar-type->container ty) 0)))

(define tvar-set-contents!
  (lambda (ty val)
    (vector-set! (tvar-type->container ty) 0 val)))

(define atomic-type?
  (lambda (ty)
    (cases type ty
      (atomic-type (name) #t)
      (else #f))))

(define proc-type?
  (lambda (ty)
    (cases type ty
      (proc-type (arg-types result-type) #t)
      (else #f))))

(define tvar-type?
  (lambda (ty)
    (cases type ty
      (tvar-type (sn cont) #t)
      (else #f))))

(define atomic-type->name
  (lambda (ty)
    (cases type ty
      (atomic-type (name) name)
      (else (eopl:error 'atomic-type->name
              "Not an atomic type: ~s" ty)))))

(define proc-type->arg-types
  (lambda (ty)
    (cases type ty
      (proc-type (arg-types result-type) arg-types)
      (else (eopl:error 'proc-type->arg-types
              "Not a proc type: ~s" ty)))))

(define proc-type->result-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-types result-type) result-type)
      (else (eopl:error 'proc-type->arg-types
              "Not a proc type: ~s" ty)))))

(define tvar-type->serial-number
  (lambda (ty)
    (cases type ty
      (tvar-type (sn c) sn)
      (else (eopl:error 'tvar-type->serial-number
              "Not a tvar-type: ~s" ty)))))

(define tvar-type->container
  (lambda (ty)
    (cases type ty
      (tvar-type (sn vec) vec)
      (else (eopl:error 'tvar-type->container
              "Not a tvar-type: ~s" ty)))))

;***********************************************************************************************************************
;************************************************    Funciones Auxiliares    ̈*******************************************
;***********************************************************************************************************************

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;***********************************************************************************************************************
;***************************************************    Pruebas    *****************************************************
;***********************************************************************************************************************

(show-the-datatypes)
just-scan
scan&parse
;(interpretador-tipos)
;***************************************************  Pruebas punto 4   *****************************************************
(scan&parse "vacio")
(scan&parse "crear-lista(3, vacio)")
(scan&parse "vacio?(vacio)")
(scan&parse "lista?(crear-lista(1, vacio))")
(scan&parse "cabeza(crear-lista(1, crear-lista(2, vacio)))")
(scan&parse "cola(crear-lista(1, crear-lista(2, vacio)))")
(scan&parse "append(crear-lista(1, vacio), crear-lista(2, vacio))")
(scan&parse "ref-list(crear-lista(1, crear-lista(2, vacio)), 1)")
(scan&parse "set-list(crear-lista(1, crear-lista(2, vacio)), 1, 99)")

;***************************************************  Pruebas punto 5   *****************************************************
;; scan&parse de cada primitiva nueva (requisito del enunciado: ejemplos
;; de cada producción usando scan&parse — sección 1, "Importante").
(scan&parse "crear-diccionario()")
(scan&parse "crear-diccionario(\"nombre\", \"Ana\", \"edad\", 34)")
(scan&parse "diccionario?(crear-diccionario())")
(scan&parse "ref-diccionario(crear-diccionario(\"nombre\", \"Ana\"), \"nombre\")")
(scan&parse "set-diccionario(crear-diccionario(\"nombre\", \"Ana\"), \"edad\", 34)")
(scan&parse "claves(crear-diccionario(\"id\", 101, \"nombre\", \"Carlos\"))")
(scan&parse "valores(crear-diccionario(\"id\", 101, \"nombre\", \"Carlos\"))")

;; eval-program de los ejemplos exactos del PDF, sección 5:
;; crear-diccionario, diccionario?, ref-diccionario, set-diccionario,
;; claves, valores — todos con begin ... end porque son varias
;; sentencias en secuencia.
;; NOTA: para ejecutar de a una estas pruebas en DrRacket, seleccioná la
;; expresión y usá "Run" o evaluá en el REPL — no hace falta correrlas
;; todas juntas.
(eval-program (scan&parse
  "begin
     var { d = (crear-diccionario(\"nombre\", \"Ana\", \"edad\", 34)) };
     print(d)
   end"))

(eval-program (scan&parse
  "begin
     var { d = (crear-diccionario(\"nombre\", \"Ana\")) };
     d = set-diccionario(d, \"edad\", 34);
     d = set-diccionario(d, \"nombre\", \"Ana Maria\");
     print(d)
   end"))

(eval-program (scan&parse
  "begin
     var { pacientes = (crear-diccionario(\"id\", 101, \"nombre\", \"Carlos\", \"diagnostico\", \"Hipertension\")) };
     print(claves(pacientes));
     print(valores(pacientes))
   end"))
;; Esperado: ["id", "nombre", "diagnostico"]
;;           [101, "Carlos", "Hipertension"]

;***************************************************  Pruebas punto 6   *****************************************************
;; Casos de control de la sección 6, incluyendo los dos que antes
;; fallaban: recursión en func, y mutaciones dentro de while/for que
;; deben verse DESPUÉS del bloque.
;; IMPORTANTE: las primitivas en esta gramática son PREFIJAS —
;; <=(n, 1), no "n <= 1" — porque así se definieron los tokens de
;; primitive en grammar-simple-interpreter (primitive "(" args ")").
#|
;; 6.0.5/6.0.6 — función simple y factorial recursivo (el ejemplo
;; central del enunciado: antes de este arreglo, esto lanzaba
;; "no binding for factorial")
(eval-program (scan&parse
  "begin
     func factorial (n) {
       if <=(n, 1) then return 1 else return *(n, factorial(-(n, 1))) end
     };
     print(factorial(5));
   end"))
;; Esperado: 120

(eval-program (scan&parse
  "begin
     func fib (n) {
       if <=(n, 1) then return n else return +(fib(-(n, 1)), fib(-(n, 2))) end
     };
     print(fib(6));
   end"))
;; Esperado: 8

;; 6.0.3 — while: antes de este arreglo, print imprimía 0 en vez de 5
;; porque la mutación de `contador` dentro del while se perdía al salir.
(eval-program (scan&parse
  "begin
     var { contador = (0) };
     while <(contador, 5) do
       contador = +(contador, 1)
     done;
     print(contador);
   end"))
;; Esperado: 5

;; 6.0.4 — for: acumulación dentro del for (antes se perdía en cada
;; vuelta; ahora se encadena).
(eval-program (scan&parse
  "begin
     var { suma = (0) };
     for n in crear-lista(1, crear-lista(2, crear-lista(3, vacio))) do
       suma = +(suma, n)
     done;
     print(suma);
   end"))
;; Esperado: 6

;; 6.0.2 — switch
(eval-program (scan&parse
  "begin
     var { color = (\"rojo\") };
     switch color {
       case \"rojo\" : print(\"Detente\")
       case \"amarillo\" : print(\"Precaucion\")
       default : print(\"Color desconocido\")
     };
   end"))
;; Esperado: Detente
|#