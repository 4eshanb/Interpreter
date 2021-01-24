#! /Applications/Racket_v6.1/bin/mzscheme -qr
;#!/usr/local/bin/mzscheme -qr


;; $Id: sbi.scm,v 1.5 2019-01-04 17:04:42-08 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;
;;
;;
(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))


;; instantiates label-table
; Taken from sybmol.scm
(define *label-table* (make-hash))

;; function to get  value for a key
; Taken from sybmol.scm
(define (label-get key)
    (hash-ref *label-table* key))

;; function to insert a key value in the label-table
; Taken from sybmol.scm
(define (label-put! key value)
    (hash-set! *label-table* key value))

(define (print-label-table)
    (hash-for-each *label-table*
         (lambda (key value)
        (printf "key: ~s value: ~s ~n" key value))))

; Taken from sybmol.scm
(define *function-table* (make-hash))

(define (function-get key)
        (hash-ref *function-table* key '(no such key in
                                         function-table)))
;----------
; Taken from sybmol.scm
(define *variable-table* (make-hash))

; Taken from sybmol.scm
(define (variable-get! key)
        (hash-ref *variable-table* key))

; Taken from sybmol.scm
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

(define (print-variable-table)  
    (hash-for-each *variable-table*
        (lambda (name value) 
          (printf "~n -------~n ~s = ~s ~n--------~n" name value)
        )
    )
)

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))



(define (parse-line-with-label line)
    (when (not (null? line))
      (when (and (not (null? (car line))) (not (null? (cdr line))))
   ;     (printf "QQQ car line ~s cdr ~s ~n" (car line) (cadr line))
            (let ( (value (car line))
                   (key (car (cdr line)))
                 )
                 (when (symbol? key)
                    (label-put! key value )
                 )
            )
      )
    )
   ; (print-label-table)
)
(define (store-label-statement program)
  (when (not (null? program))
    (let ((line (car program)))
;      (printf "line ~s ~n" line)
      (parse-line-with-label line)
    )
    (store-label-statement (cdr program))
  )
)
   
(define (interpret-log x)
    (if (equal? x 0) 0.0 (log x))
)

(define (interpret-divide x y )
    (if (equal? y 0) 0.0 (/ x y))
)

(define (interpret-print-strings x)
   ; (printf "print-strings ~s ~s ~n" x (length x))
    (when (and (not (null? (car x))) (string? (car x)))
        (display (car x))
        (when (= (length x) 1)
          (newline)
        )
    )
)

(define (debug-print xx) 
    ( printf "~n MAP= " ) 
    ( display xx  ) 
    ( printf "~n" ) 
)

; taken from examples evaluate-exxpr
(define (evaluate-expr x )
    (if (pair? x)
      (begin 
        (let ((func-name (function-get (car x))) 
              (args (map evaluate-expr (cdr x)))
        )
          (apply (car func-name) args)
        )
      )
      (begin
        (if (number? x) 
          x
          (variable-get! x)
        )
      )
    )
)


(define (interpret-print-expressions x)
;   (printf "print-expr x= ~s ~n" x)
   (if (and (list? (car x) ) (= (length (car x)) 3))
     (begin
       (if  (and (= (length (car x)) 3) (eqv? (car (car x)) 'asub) )
        (let ( (array-var (evaluate-expr(car (cdr (car x)))))
               (array-index (evaluate-expr(car (cdr (cdr (car x))))))
             )
           (display (vector-ref array-var ( - array-index 1) ))
           (newline)
         )
         (when (not(string? (car x)))
           (display (evaluate-expr (car x)))
           (newline)
         )
       )
     )
     (when (not(string? (car x)))
      (display (evaluate-expr (car x)))
      (newline)
     )
   )
)
(define (interpret-print x ) 
    (when (not (null? x))
      (interpret-print-strings x)
      (interpret-print-expressions x)
      (interpret-print (cdr x))
    )
)

(define (interpret-let-expressions x) 
   (when (not(null? x ))
      (evaluate-expr (car x))
   )
)
(define (interpret-let x) 
 (when (not (null? x))
;      (printf "let x=~s car=~s cdr ~s ~n" x (car x) (cdr x))
   ( let ((variable-name (car x))
          (expr-value ( interpret-let-expressions (cdr x)))
         )
;       (printf "let value ~s name ~s x=~s ~n"  
;         expr-value 
;          variable-name x)
        ( store-let-variable variable-name expr-value)
   )
  )
;  (print-variable-table)
)



(define ( store-let-variable variable-name value )
 (if (and (list? variable-name) )
    (begin
      (let ( (array-var (evaluate-expr(car (cdr variable-name))))
         (array-index (evaluate-expr(car (cdr (cdr variable-name))))))
    ; ( printf "store got a asub ~s ~s ~s ~n" 
    ; array-var array-index value)
           (vector-set! array-var ( - array-index 1) value )
      )
    )
    (when (and (not (null? variable-name)) (not(null? value)))
      ( variable-put! variable-name value)
    )
  )
)

(define (interpret-trunc x)
   (printf "interpert-trunc ~s ~n" x)
   (void)
)
(define (interpret-lessthan-equalto x )
   (printf "interpert-<=  ~n" )
   (void)
)

(define ( execute-if program prog line-number expression)
   (if (or 
            (equal? (evaluate-expr expression) #f)
             (not ( <= line-number (length program))) 
       )
      (exit)
      (let ( (line (list-ref program (- line-number 1))))
        (when ( = (length line) 3)
          (let ((stmt (cdr (cdr (list-ref program (- line-number 1))))))
            (execute-statement1 line-number  (car stmt) prog program ) 
          ) 
        )
        (when ( = (length line) 2)
          (let ( (stmt (cdr (list-ref program (- line-number 1)))))
            (execute-statement1 line-number (car stmt) prog program) 
          )
        )
        (execute-if program prog (+ line-number 1) expression)
      )
    )
)

(define (interpret-if line program prog linenum )
   (let ( (label (cdr line)) (expression (car line)) )
      ( let ( (line-number (label-get (car label))) )
              (execute-if program prog line-number expression)
      )
   )
)



(define (interpret-input var-name)
   (let ((var-value (read)))
     ( variable-put! (car var-name) var-value)
   )
)
(define (interpret-goto line program prog linenum )

   ( let ( (line-number (label-get (car line))))
        (execute-goto program prog line-number )
   )
)


(define ( execute-goto program prog line-number)
   (if (not ( <= line-number (length program)))
      (exit)
      (let ((line (list-ref program (- line-number 1))))
        (when ( = (length line) 3)
          (let ((stmt (cdr (cdr (list-ref program (- line-number 1))))))
             (execute-statement1 line-number  (car stmt) prog program ) 
          ;   (newline)
          )
        )
        (when ( = (length line) 2)
           (let ( (stmt (cdr (list-ref program (- line-number 1)))))
              (execute-statement1 line-number (car stmt) prog program) 
          ;    (newline)
           )
        )
        (execute-goto program prog (+ line-number 1))
      )
    )
)






(define (interpret-dim-expression x )
  (when (not (null? x ))
    (evaluate-expr (car (cdr (cdr(car x)))))
  )
)
(define (interpret-dim x )
  (when (not (null? x))
   ; (printf "dim x=~s car=~s cdr=~s   ~n" x  (caar x) (cdr(car x)) ) 
    (let ( (func-name (caar x))
           (variable-name (car(cdr(car x))))
           (array-len (interpret-dim-expression x ))
         )
   ;    ( printf "dim ~s ~s ~s ~n" func-name variable-name  array-len)
       (variable-put! variable-name (make-vector array-len))
    )
   ; (print-variable-table)
  )
)

(for-each 
  (lambda(keyval) 
     ( hash-set! *function-table* (car keyval) (cdr keyval)))
   `(
     (print ,interpret-print)
     (let ,interpret-let)
     (dim ,interpret-dim)
     (if  ,interpret-if)
     (input ,interpret-input)
     (goto , interpret-goto)
     (/ ,interpret-divide)
     (log ,interpret-log)
     (+ ,+)
     (<= ,<=)
     (= ,=)
     (< ,<)
     (> ,>)
     (>= ,>=)
     (<> , (lambda (x y) ( (not (equal? x y))) ))
     (- ,-)
     (* ,*)
     (exp ,exp)
     (sqrt ,sqrt(x))
     (^ ,expt)
     (abs ,abs)
     (cos ,cos)
     (sin ,sin)
     (tan ,tan)
     (asin ,asin)
     (atan ,atan)
     (acos ,acos)
     (trunc ,interpret-trunc)
     (floor ,floor)
     (ceil ,ceiling)
     (round ,round)
    ) 
)

(for-each 
  (lambda(keyval) 
    ( hash-set! *variable-table* (car keyval) (cdr keyval)))
   `(
     (eof 0.0)
     (e  (exp 1.0))
     (pi (acos -1.0))

    ) 
)
(define (interpret-program program complete-program)
    (when (not (null? program))
      (let ((line (car program)))
   ;     (printf "INTERPRET-program line ~s ~n" line)
        (interpret-line line program complete-program)
      )
      (interpret-program (cdr program) complete-program)
    )
)

(define (skip-stmts program label line complete-program) 
  (when (not (null? line))
     (when (list? (car (cdr line)))
        (map (lambda (x) 
              (when (equal? x label)
                 (interpret-line line program complete-program)
              )
             ) (flatten line)
        )
     )
    ) 
    (when (not (null? program))
      (skip-stmts (cdr program)  
        label (car program ) complete-program )
    )
)

(define (interpret-line line program program-complete)
    (when (not (null? line))
      (let ((linenum (car line)) (remaining (cdr line)))
        (when (not (null? remaining))
          (when (and 
                    (symbol? (car remaining)) 
                    (not(null?(cdr remaining))) 
                )
                (execute-statement1 linenum (car (cdr remaining)) 
                        program program-complete)

                (skip-stmts (cdr program) 
                            (car remaining) 
                            (car (cdr program)) 
                            program-complete)
          )
          (when (and 
                  (not (null? (car remaining))) 
                  (null? (cdr remaining))
                )
            (execute-statement1 linenum 
                 (car remaining) program program-complete)
          )
        )
      )
    )
)
(define (execute-statement1  linenum line program program-complete) 
   (when (not (null? line)) 
    ;;     (printf "             execute-statement1 x=~s ~n" line )
     (when (not (symbol? line))
       ( let  ( (keyword (car line)) (expression (cdr line))
              )
      ;   (printf "keyword ~s expression ~s ~n" keyword expression)
         (if (or (eqv? keyword 'goto) (eqv? keyword 'if ))
            ((car (function-get keyword)) 
                expression  program-complete program linenum)
            ((car (function-get keyword)) expression)
         )
       )
     )
    )
)
(define (execute-statement line) 
   (when (not (null? line)) 
     (printf "execute-statement x=~s ~n" line )
     (when (not (symbol? line))
       ( let  ( (keyword (car line)) (expression (cdr line))
              )
         ((car (function-get keyword)) expression)
       )
     )
    )
)


(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n")
    (dump-stdin))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile))
              )
        ;;      (write-program-by-line sbprogfile program)
               (store-label-statement program)
               (interpret-program program program)
         )))

(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments))))

