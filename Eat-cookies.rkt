#lang racket
;input  
 (define n 0)  
 ;is a symbolic list?  
 (define (slst? l) (if (null? l) #t (if (number? (car l)) #f (slst? (cdr l)))))  
 ;visualization
 (define (view l) (begin (display "The grid now is: \n")  
              (display (cons (car l) (cons (cadr l) (cons (caddr l) (cons (cadddr l)  (list (cadddr (cdr l)))))))) (newline)  
              (display (cons (cadddr (cddr l)) (cons (cadddr (cdddr l)) (cons (cadddr (cddddr l)) (cons (cadddr (cddddr (cdr l))) (list(cadddr (cddddr (cddr l))))))))) (newline)
              (display (cons (cadddr (cddddr (cdddr l))) (cons (cadddr (cddddr (cddddr l))) (cons (cadddr (cddddr (cddddr (cdr l)))) (cons (cadddr (cddddr (cddddr (cddr l)))) (list(cadddr (cddddr (cddddr (cdddr l)))))))))) (newline)
              (display (cddddr (cddddr (cddddr (cdddr l))))) (newline)))  
 (define (free? l e) (if (null? l) #f (if (not (equal? (car l) e)) (free? (cdr l) e) #t)))  
 ;find & replace

 (define (tr l e s)  
   (if (equal? (car l) e) ( replace l e e s )  
     (cons (car l) (tr (cdr l) e s))))  



(define  (replace l temp number s)
  (cond ( ( null? l ) l )
        ( (not( = (remainder number 5 ) 0)) (cons s (replace (cdr l) temp (+ 1 number) s ) ) )
        ( else (cons s  (replace (space (cdr l) (remainder temp 5) number 0) temp (+ number (remainder temp 5)) s )))))  

(define (space l mod number counter)
  (cond ((= counter mod) l )
        (cons (car l) (space (cdr l) mod (+ 1 number) (+ 1 counter)))))




  
;(cons s (replace (cdr l) (+ number 1))

;win position?
 (define (win l wpos) (cond ((null? l) #f)  
               ((equal? (car (reverse l)) (car wpos)) #t)  
               (else #f)))  

;list of win positions
 (define (win? l)(win l '(0)))

; contains the element

 (define (cc l e) (if (null? l) #f (if (equal? (car l) e) #t (cc (cdr l) e)))) ;

 (define (wplay genlst plslst mnslst turn)  
 (view genlst)  
  (cond ((win? plslst) (display " the winner is user"))  
     ((win? mnslst)  (display "the winner  computer"))
     ((slst? genlst) (display "no winner"))  
     (else (begin (display "Insert a number between 0 and 19, is your turn ") (display turn) (newline)  
         (set! n (read)) (display "you insert: ") (display n) (newline)  
         (if (number? n)  
           (if (< n 20)  
             (if (free? genlst n)  
              [if (equal? turn 'user) (wplay (tr genlst n '%) (append plslst (list n)) mnslst 'computer)  
                        (wplay (tr genlst n '%) plslst (append mnslst (list n)) 'user)]  
             (begin (display "this is a busy place\n") (wplay genlst plslst mnslst turn)))  
            (begin (display "not valid place\n")(wplay genlst plslst mnslst turn)))  
          (begin (display "A valid place is a number between 0 and 19\n")(wplay genlst plslst mnslst turn))))))) 
 ;play function 
 (define play (begin (display "Game started is the turn of user .\n")(wplay (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) (list ) (list ) 'user)))  