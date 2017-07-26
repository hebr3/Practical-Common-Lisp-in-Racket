#lang racket

(define getf hash-ref)

(define (y-or-n?)
  (let ([i (read-line)])
    (cond
      [(or (equal? i "y") (equal? i "Y")) #t]
      [(or (equal? i "n") (equal? i "N")) #f]
      [else (displayln "ERROR: [y/n]")
            (y-or-n?)])))

;;

(define (make-cd title artist rating ripped)
  (hash 'TITLE title
        'ARTIST artist
        'RATING rating
        'RIPPED ripped))

(define *db* '())

(define (add-record cd)
  (set! *db* (cons cd *db*)))

(add-record (make-cd "Roses" "Kathy Mattea" 7 #t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 #t))
(add-record (make-cd "Home" "Dixie Chicks" 9 #t))

;(define (dump-db)
;  (let ([l (map list->alist *db*)])
;    (for* ([j l]
;           [i j])
;      (let* ([key (~a (symbol->string (car i))
;                      ":"
;                      #:min-width 10)]
;             [kvl (~a key (cdr i))])
;        (displayln kvl)))))

(define (prompt-read prompt)
  (display (~a prompt ": "))
  (read-line))

(define (prompt-for-cd)
  (define (Ripped?)
    (display "Ripped [y/n]:")
    (y-or-n?))
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (prompt-read "Rating")
   (Ripped?)))

(define (add-cds)
  (add-record (prompt-for-cd))
  (display "Another: ")
  (if (y-or-n?)
      (add-cds)
      '()))

(define (save-db filename)
  (call-with-output-file filename #:exists 'replace
    (λ (out) (display *db* out))))

(define (load-db filename)
  (set! *db*
        (read (open-input-string 
               (call-with-input-file filename ;#:mode 'text
                 (λ (in) (read-line in)))))))

(define (select-by-artist artist)
  (filter (λ (x) (equal? artist (getf x 'ARTIST)))
          *db*))

(define (select selector-fn)
  (filter selector-fn *db*))

(define (artist-selector artist)
  (λ (cd) (equal? (getf cd 'ARTIST) artist)))

(define (where #:title [title #f] #:artist [artist #f]
               #:rating [rating #f] #:ripped [ripped #\f])
  (λ (cd)
    (and (if title
             (equal? (getf cd 'TITLE) title)
             #t)
         (if artist
             (equal? (getf cd 'ARTIST) artist)
             #t)
         (if rating
             (equal? (getf cd 'RATING) rating)
             #t)
         (if (boolean? ripped)
             (equal? (getf cd 'RIPPED) ripped)
             #t))))

;(define (update selector-fn
;                #:title [title #f] #:artist [artist #f]
;                #:rating [rating #f] #:ripped [ripped #\f])
;  (set! *db*
;        (map (λ (row)
;               (for ([i row])
;                 (if title (
                 