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
   ("%" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "?" "-"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)))


;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ( "(" expression (arbno expression) ")")
                app-exp)
    
    ; características adicionales
    (expression ("false") false-exp)
    (expression ("true") true-exp)
    (expression ("proc" "(" (separated-list optional-type-exp identifier ",") ")" expression)
                proc-exp)
    (expression ("letrec" (arbno optional-type-exp identifier
                                 "(" (separated-list optional-type-exp identifier ",") ")"
                                 "=" expression) "in" expression) 
                letrec-exp)
    ;;;;;;

    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive (">") mayor-prim)
    (primitive ("<") menor-prim)
    ;añadidas:
    (primitive ("zero?") zero-test-prim)
    (primitive ("crear-lista") crear-lista-prim)
    (primitive ("vacio?") vacioq-prim)
    (primitive ("lista?") listaq-prim)
    (primitive ("cabeza") cabeza-prim)
    (primitive ("cola") cola-prim)
    (primitive ("ref-list") ref-list-prim)

    ; características adicionales
    (expression ("false") false-exp)
    (expression ("true") true-exp)
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

;***********************************************************************************************************************
;***********************************************************************************************************************


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
     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (decr-prim) (cons (var-exp 'y) '())) '())))
                      (empty-env)))
     (empty-env))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (eval-expression test-exp env)
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
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
                 #f))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (zero-test-prim () (zero? (car args)))
      (menor-prim () (< (car args) (cadr args)))
      (mayor-prim () (> (car args) (cadr args)))
     )
    )
  )

(define true-value?
  (lambda (x)
    (not (zero? x))))

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
      (true-exp ()
                bool-type)
      (false-exp ()
                 bool-type)
      (var-exp (id)
               (apply-tenv tenv id))
      (if-exp (test-exp true-exp false-exp)
              (let ((test-type (type-of-expression test-exp tenv))
                    (false-type (type-of-expression false-exp tenv))
                    (true-type (type-of-expression true-exp tenv)))
                (check-equal-type! test-type bool-type test-exp)
                (check-equal-type! true-type false-type exp)
                true-type))
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

(define check-equal-type!         ;;; NUEVO      
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
      (incr-prim ()
                 (proc-type (list int-type) int-type))
      (decr-prim ()
                 (proc-type (list int-type) int-type))
      (zero-test-prim ()
                      (proc-type (list int-type) bool-type))
      (menor-prim () (proc-type (list int-type int-type) bool-type))
      (mayor-prim () (proc-type (list int-type int-type) bool-type))
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

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

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
(interpretador-tipos)
