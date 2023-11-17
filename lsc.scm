(define read-list-of-forms
  (lambda (port)
    (letrec ((iter
              (lambda (acc)
                (let ((cur (read port)))
                  (if (eof-object? cur)
                      (reverse acc)
                      (iter (cons cur acc)))))))
      (iter '()))))

(define read-forms-from-file
  (lambda (file)
    (with-input-from-file file
      (lambda ()
        (read-list-of-forms (current-input-port))))))

;; macro expander

;; convert splices

;; convert internal definitions

;; convert assignments

;; cps

;; convert closures

;; register allocation

;; generate vm code

(define vm-codegen
  (lambda (funcs entrypoint)
    #f))

;; assembly

;; asm ::= stmt ...
;; stmt ::= label | op
;; op ::= nop
;;      | sys rd
;;      | sdiv/smod/add/sub/mul/udiv/umod/fadd/fsub/fmul/fdiv/
;;        and/or/xor/not/lsh/rsh rd, ra, rb
;;      | fti/itf/ld/st/mov rd, ra
;;      | ldi rd, imm
;;      | jnz ra, rb

(define regs
  (letrec ((iter
            (lambda (n acc)
              (if (< n 0)
                  acc
                  (iter (- n 1)
                        (cons (cons (string->symbol (string-append "r" (number->string n)))
                                    n)
                              acc))))))
    (iter 31 '())))

(define ston
  (lambda (rname)
    (cdr (assq rname regs))))

(define <<
  (lambda (x y)
    (if (= y 0)
        x
        (<< (* x 2) (- y 1)))))

(define >>
  (lambda (x y)
    (if (= y 0)
        x
        (>> (div x 2) (- y 1)))))

(define make-encoder-none
  (lambda (opcode)
    (lambda (stmt)
      (<< opcode 27))))

(define make-encoder-r
  (lambda (opcode)
    (lambda (stmt)
      (+ (<< opcode 27)
         (<< (ston (cadr stmt)) 22)))))

(define make-encoder-rr
  (lambda (opcode)
    (lambda (stmt)
      (+ (<< opcode 27)
         (<< (ston (cadr stmt)) 22)
         (<< (ston (caddr stmt)) 17)))))

(define make-encoder-rrr
  (lambda (opcode)
    (lambda (stmt)
      (+ (<< opcode 27)
         (<< (ston (cadr stmt)) 22)
         (<< (ston (caddr stmt)) 17)
         (<< (ston (cadddr stmt)) 12)))))

(define make-encoder-ri
  (lambda (opcode)
    (lambda (stmt)
      (+ (<< opcode 27)
         (<< (ston (cadr stmt)) 22)
         (caddr stmt)))))

(define asm-stmts
  (list (cons 'nop (make-encoder-none 0))
        (cons 'sys (make-encoder-ri 1))
        (cons 'sdiv (make-encoder-rrr 2))
        (cons 'smod (make-encoder-rrr 3))
        (cons 'add (make-encoder-rrr 4))
        (cons 'sub (make-encoder-rrr 5))
        (cons 'mul (make-encoder-rrr 6))
        (cons 'udiv (make-encoder-rrr 7))
        (cons 'umod (make-encoder-rrr 8))
        (cons 'fadd (make-encoder-rrr 9))
        (cons 'fsub (make-encoder-rrr 10))
        (cons 'fmul (make-encoder-rrr 11))
        (cons 'fdiv (make-encoder-rrr 12))
        (cons 'fti (make-encoder-rr 13))
        (cons 'itf (make-encoder-rr 14))
        (cons 'and (make-encoder-rrr 15))
        (cons 'or (make-encoder-rrr 16))
        (cons 'xor (make-encoder-rrr 17))
        (cons 'not (make-encoder-rr 18))
        (cons 'lsh (make-encoder-rrr 19))
        (cons 'rsh (make-encoder-rrr 20))
        (cons 'ld (make-encoder-rr 21))
        (cons 'st (make-encoder-rr 22))
        (cons 'ldi (make-encoder-ri 23))
        (cons 'mov (make-encoder-rr 24))
        (cons 'jnz (make-encoder-rr 25))))

(define asm
  (lambda (code)
    (letrec ((resolve-labels
              (lambda (code addr acc)
                (if (null? code)
                    acc
                    (let ((next (+ addr 4))
                          (stmt (car code))
                          (rest (cdr code)))
                      (if (symbol? stmt)
                          (resolve-labels rest addr (cons (cons stmt addr) acc))
                          (resolve-labels rest next acc))))))
             (assemble
              (lambda (code labels acc)
                (if (null? code)
                    (reverse acc)
                    (let ((stmt (car code))
                          (rest (cdr code)))
                      (cond ((symbol? stmt)
                             (assemble rest labels acc))
                            (else
                             (let ((stmt (map (lambda (x)
                                                (let ((res (assq x labels)))
                                                  (if res
                                                      (cdr res)
                                                      x)))
                                              stmt))
                                   (handler (cdr (assq (car stmt) asm-stmts))))
                               (assemble rest labels (cons (handler stmt) acc))))))))))
      (let* ((labels (resolve-labels code 0 '()))
             (assembled (assemble code labels '())))
        assembled))))

(define write-asm
  (lambda (code port)
    (write (* (length code) 4) port)
    (let loop ((code code))
      (if (null? code)
          #f
          (let ((op (car code)))
            (display " " port)
            (write (mod (>> op 0) 256) port)
            (display " " port)
            (write (mod (>> op 8) 256) port)
            (display " " port)
            (write (mod (>> op 16) 256) port)
            (display " " port)
            (write (mod (>> op 24) 256) port)
            (loop (cdr code)))))))

(define write-prg-file
  (lambda (code file)
    (with-output-to-file file
      (lambda ()
        (write-asm code (current-output-port))))))
