;;; Copyright (C) 2023 Joseph Donaldson
;;; Bigloo port of reference srfi196 implementation
;;;
;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
(module srfi196
   (library srfi1 srfi133)
   (export
      (final-class <range>
         start-index::long
         length::long
         indexer::procedure 
         complexity)
      (inline range? v)
      (inline range-start-index::long r::<range>)
      (inline range-length::long r::<range>)
      (inline range-indexer::procedure r::<range>)
      (inline range-complexity::long r::<range>)
      (range length indexer)
      numeric-range
      iota-range
      (vector-range vec)
      (string-range s)
      (range-ref r index)
      (range-first r)
      (range-last r)
      range=?
      (range-split-at r index)
      (subrange r start end)
      (range-segment r k)
      (range-take r count)
      (range-take-right r count)
      (range-drop r count)
      (range-drop-right r count)
      (range-count pred r . rs)
      (range-any pred r . rs)
      (range-every pred r . rs)
      (range-map proc . rs)
      (range-map->list proc r . rs)
      (range-filter-map->list proc r . rs)
      (range-map->vector proc r . rs)
      (range-for-each proc r . rs)
      range-fold
      range-fold-right
      (range-filter-map proc . rs)
      (range-filter->list pred r)
      (range-remove pred r)
      (range-remove->list pred r)
      (range-reverse r)
      range-append
      (range-index pred r . rs)
      (range-index-right pred r . rs)
      (range-take-while pred r)
      (range-take-while-right pred r)
      (range-drop-while pred r)
      (range-drop-while-right pred r)
      (range->list r)
      (range->vector r)
      (range->string r)
      (vector->range vec)
      (range->generator r)
      (range-filter pred r))
   
   (option
      (set! *arithmetic-overflow* #f)
      (set! *optim-cfa-fixnum-arithmetic?* #t)
      (set! *optim-cfa-flonum-arithmetic?* #t)))


; ;;; vector-unfold and vector-unfold! from srfi-133 

; ;;; (VECTOR-UNFOLD <f> <length> <initial-seed> ...) -> vector
; ;;;     (F <index> <seed> ...) -> [elt seed' ...]
; ;;;   The fundamental vector constructor.  Creates a vector whose
; ;;;   length is LENGTH and iterates across each index K between 0 and
; ;;;   LENGTH, applying F at each iteration to the current index and the
; ;;;   current seeds to receive N+1 values: first, the element to put in
; ;;;   the Kth slot and then N new seeds for the next iteration.
; (define (vector-unfold f::procedure length::long . initial-seeds)
;   (define vec (make-vector length))
;   (apply vector-unfold! f vec 0 length initial-seeds)
;   vec)

; ;;; (VECTOR-UNFOLD! <vec> <start> <end> <f> <initial-seed> ...) -> vector
; ;;;     (F <index> <seed> ...) -> [elt seed' ...]
; ;;;   Like VECTOR-UNFOLD, but unfolds onto an existing vector starting
; ;;;   at <start> up to but not including <end>.
; (define (vector-unfold! f::procedure vec::vector start::long  end::long  . initial-seeds)
;   (letrec ((tabulate!                   ; Special zero-seed case.
;             (lambda (f vec i len)
;               (cond ((< i len)
;                      (vector-set! vec i (f i))
;                      (tabulate! f vec (+ i 1) len)))))
;            (unfold1!                    ; Fast path for one seed.
;             (lambda (f vec i len seed)
;               (if (< i len)
;                   (receive (elt new-seed)
;                            (f i seed)
;                     (vector-set! vec i elt)
;                     (unfold1! f vec (+ i 1) len new-seed)))))
;            (unfold2+!                   ; Slower variant for N seeds.
;             (lambda (f vec i len seeds)
;               (if (< i len)
;                   (receive (elt . new-seeds)
;                            (apply f i seeds)
;                     (vector-set! vec i elt)
;                     (unfold2+! f vec (+ i 1) len new-seeds))))))
;      (let ()
;         (cond ((null? initial-seeds)
;                (tabulate! f vec start end))
;               ((null? (cdr initial-seeds))
;                (unfold1! f vec start end (car initial-seeds)))
;               (else
;                (unfold2+! f vec start end initial-seeds))))))

(define exact inexact->exact)

;; simple assume implementation 
(define-syntax assume
   (syntax-rules ()
      ((assume exp)
       (assume exp ""))
      ((assume exp msg)
       (cond-expand
          (bigloo-debug
           (unless exp
              (error "assume failure" msg exp)))))))

(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda)
     (lambda args
       (error "case-lambda" "case-lambda without any clauses." #unspecified)))
    ((case-lambda 
      (?a1 ?e1 ...) 
      ?clause1 ...)
     (lambda args
       (let ((l (length args)))
         (case-lambda "CLAUSE" args l 
           (?a1 ?e1 ...)
           ?clause1 ...))))
    ((case-lambda "CLAUSE" ?args ?l 
      ((?a1 ...) ?e1 ...) 
      ?clause1 ...)
     (if (= ?l (length '(?a1 ...)))
         (apply (lambda (?a1 ...) ?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l 
           ?clause1 ...)))
    ((case-lambda "CLAUSE" ?args ?l
      ((?a1 . ?ar) ?e1 ...) 
      ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l 1 (?a1 . ?ar) (?ar ?e1 ...) 
       ?clause1 ...))
    ((case-lambda "CLAUSE" ?args ?l 
      (?a1 ?e1 ...)
      ?clause1 ...)
     (let ((?a1 ?args))
       ?e1 ...))
    ((case-lambda "CLAUSE" ?args ?l)
     (error "case-lambda" "Wrong number of arguments to CASE-LAMBDA." #unspecified))
    ((case-lambda "IMPROPER" ?args ?l ?k ?al ((?a1 . ?ar) ?e1 ...)
      ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l (+ ?k 1) ?al (?ar ?e1 ...) 
      ?clause1 ...))
    ((case-lambda "IMPROPER" ?args ?l ?k ?al (?ar ?e1 ...) 
      ?clause1 ...)
     (if (>= ?l ?k)
         (apply (lambda ?al ?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l 
           ?clause1 ...)))))

(define exact-integer? integer?)

(define (exact-natural? x)
  (and (exact-integer? x) (not (negative? x))))

;; Find the least element of a list non-empty of naturals. If an element
;; is zero, returns it immediately.
(define (short-minimum ns)
  (bind-exit (return)
     (reduce (lambda (n s)
                (if (zero? n) (return n) (min n s)))
        0
        ns)))

(define (sum ns) (reduce + 0 ns))

(define (raw-range::<range> start-index::long length::long indexer complexity)
   (instantiate::<range> (start-index start-index) (length length)
                         (indexer indexer) (complexity complexity)))

(define-inline (range? v)
   (isa? v <range>))

(define-inline (range-start-index::long r::<range>)
   (-> r start-index))

(define-inline (range-length::long r::<range>)
   (-> r length))

(define-inline (range-indexer::procedure r::<range>)
   (-> r indexer))

(define-inline (range-complexity::long r::<range>)
   (-> r complexity))

; (define-record-type <range>
;   (raw-range start-index length indexer complexity)
;   range?
;   (start-index range-start-index)
;   (length range-length)
;   (indexer range-indexer)
;   (complexity range-complexity))

;; Maximum number of indexers to compose with range-reverse and
;; range-append before a range is expanded with vector-range.
;; This may need adjustment.
(define %range-maximum-complexity 16)

;; Returns an empty range which is otherwise identical to r.
(define (%empty-range-from r)
  (raw-range (range-start-index r) 0 (range-indexer r) (range-complexity r)))

(define (threshold? k)
  (> k %range-maximum-complexity))

(define (%range-valid-index? r index)
  (and (exact-natural? index)
       (< index (range-length r))))

;; As the previous check, but bound is assumed to be exclusive.
(define (%range-valid-bound? r bound)
  (and (exact-natural? bound)
       (<= bound (range-length r))))

;;;; Constructors

;; The primary range constructor does some extra consistency checking.
(define (range length indexer)
  (assume (exact-natural? length))
  (assume (procedure? indexer))
  (raw-range 0 length indexer 0))

(define numeric-range
  (case-lambda
    ((start end) (numeric-range start end 1))
    ((start end step)
     (assume (real? start))
     (assume (real? end))
     (assume (not (zero? step)) "numeric-range: zero-valued step")
     (let ((len (exact (ceiling (max 0 (/ (- end start) step))))))
       ;; Try to ensure that we can compute a correct range from the
       ;; given parameters, i.e. one not plagued by roundoff errors.
       (assume (cond ((and (positive? step) (< start end))
                      (and (> (+ start step) start)
                           (< (+ start (* (- len 1) step)) end)))
                     ((and (negative? step) (> start end))
                      (and (< (+ start step) start)
                           (> (+ start (* (- len 1) step)) end)))
                     (else #t))
               "numeric-range: invalid parameters")
       (raw-range 0 len (lambda (n) (+ start (* n step))) 0)))))

;; TODO: Consider possible round-off bugs.
(define iota-range
  (case-lambda
    ((len) (iota-range len 0 1))
    ((len start) (iota-range len start 1))
    ((len start step)
     (assume (exact-natural? len))
     (assume (real? start))
     (assume (real? step))
     (raw-range 0
                len
                (cond ((and (zero? start) (= step 1)) (lambda (i) i))
                      ((= step 1) (lambda (i) (+ start i)))
                      ((zero? start) (lambda (i) (* step i)))
                      (else (lambda (i) (+ start (* step i)))))
                0))))

(define (vector-range vec)
  (assume (vector? vec))
  (raw-range 0 (vector-length vec) (lambda (i) (vector-ref vec i)) 0))

;; This implementation assumes that string-ref is O(n), as would be
;; the case with UTF-8.  If an implementation has an O(1) string-ref,
;; the following version is preferable:
;;
;; (raw-range 0 (string-length s) (lambda (i) (string-ref s i))))
;;
(define (string-range s)
  (assume (string? s))
  ;(vector-range (string->vector s))
  (raw-range 0 (string-length s) (lambda (i) (string-ref s i)) 0))

(define (%range-maybe-vectorize r)
  (if (threshold? (range-complexity r))
      (vector-range (range->vector r))
      r))

;;;; Accessors

(define (range-ref r index)
  (assume (range? r))
  (assume (%range-valid-index? r index) "range-ref: invalid index")
  ((range-indexer r) (+ index (range-start-index r))))

;; A portable implementation can't rely on inlining, but it
;; can rely on macros.
(define-syntax %range-ref-no-check
  (syntax-rules ()
    ((_ r index)
     ((range-indexer r) (+ index (range-start-index r))))))

(define (range-first r) (%range-ref-no-check r (range-start-index r)))

(define (range-last r) (%range-ref-no-check r (- (range-length r) 1)))

;;;; Predicates

(define range=?
  (case-lambda
    ((equal ra rb)                      ; two-range fast path
     (assume (procedure? equal))
     (assume (range? ra))
     (%range=?-2 equal ra rb))
    ((equal . rs)                       ; variadic path
     (assume (procedure? equal))
     (assume (pair? rs))
     (let ((ra (car rs)))
       (assume (range? ra))
       (every (lambda (rb) (%range=?-2 equal ra rb)) (cdr rs))))))

(define (%range=?-2 equal ra rb)
  (assume (range? rb))
  (or (eqv? ra rb)                      ; quick check
      (let ((la (range-length ra)))
        (and (= la (range-length rb))
             (if (zero? la)
                 #t                     ; all empty ranges are equal
                 (let lp ((i 0))
                   (cond ((= i la) #t)
                         ((not (equal (%range-ref-no-check ra i)
                                      (%range-ref-no-check rb i)))
                          #f)
                         (else (lp (+ i 1))))))))))

;;;; Iteration

(define (range-split-at r index)
  (assume (range? r))
  (assume (%range-valid-bound? r index))
  (cond ((= index 0) (values (%empty-range-from r) r))
        ((= index (range-length r)) (values r (%empty-range-from r)))
        (else
         (let ((indexer (range-indexer r)) (k (range-complexity r)))
           (values (raw-range (range-start-index r) index indexer k)
                   (raw-range index (- (range-length r) index) indexer k))))))

(define (subrange r start end)
  (assume (range? r))
  (assume (%range-valid-index? r start) "subrange: invalid start index")
  (assume (%range-valid-bound? r end) "subrange: invalid end index")
  (assume (not (negative? (- end start))) "subrange: invalid subrange")
  (if (and (zero? start) (= end (range-length r)))
      r
      (raw-range (+ (range-start-index r) start)
                 (- end start)
                 (range-indexer r)
                 (range-complexity r))))

(define (range-segment r k)
  (assume (range? r))
  (assume (and (exact-integer? k) (positive? k)))
  (let ((len (range-length r))
        (%subrange-no-check
         (lambda (s e)
           (raw-range (+ (range-start-index r) s)
                      (- e s)
                      (range-indexer r)
                      (range-complexity r)))))
    (unfold (lambda (i) (>= i len))
            (lambda (i) (%subrange-no-check i (min len (+ i k))))
            (lambda (i) (+ i k))
            0)))

(define (range-take r count)
  (assume (range? r))
  (assume (%range-valid-bound? r count) "range-take: invalid count")
  (cond ((zero? count) (%empty-range-from r))
        ((= count (range-length r)) r)
        (else (raw-range (range-start-index r)
                         count
                         (range-indexer r)
                         (range-complexity r)))))

(define (range-take-right r count)
  (assume (range? r))
  (assume (%range-valid-bound? r count)
          "range-take-right: invalid count")
  (cond ((zero? count) (%empty-range-from r))
        ((= count (range-length r)) r)
        (else
         (raw-range (+ (range-start-index r) (- (range-length r) count))
                    count
                    (range-indexer r)
                    (range-complexity r)))))

(define (range-drop r count)
  (assume (range? r))
  (assume (%range-valid-bound? r count) "range-drop: invalid count")
  (if (zero? count)
      r
      (raw-range (+ (range-start-index r) count)
                 (- (range-length r) count)
                 (range-indexer r)
                 (range-complexity r))))

(define (range-drop-right r count)
  (assume (range? r))
  (assume (%range-valid-bound? r count) "range-drop: invalid count")
  (if (zero? count)
      r
      (raw-range (range-start-index r)
                 (- (range-length r) count)
                 (range-indexer r)
                 (range-complexity r))))

(define (range-count pred r . rs)
  (assume (procedure? pred))
  (assume (range? r))
  (if (null? rs)                        ; one-range fast path
      (%range-fold-1 (lambda (c x) (if (pred x) (+ c 1) c)) 0 r)
      (apply range-fold                 ; variadic path
             (lambda (c . xs)
               (if (apply pred xs) (+ c 1) c))
             0
             r
             rs)))

(define (range-any pred r . rs)
  (assume (procedure? pred))
  (assume (range? r))
  (bind-exit (return)
     (if (null? rs)                        ; one-range fast path
         (%range-fold-1 (lambda (_last x)
                           (cond ((pred x) => return)
                                 (else #f)))
            #f
            r)
         (apply range-fold                 ; variadic path
            (lambda (_last . xs)
               (cond ((apply pred xs) => return)
                     (else #f)))
            #f
            r
            rs))))

(define (range-every pred r . rs)
  (assume (procedure? pred))
  (assume (range? r))
  (bind-exit (return)
     (if (null? rs)                     ; one-range fast path
         (%range-fold-1 (lambda (_ x) (or (pred x) (return #f))) #t r)
         (apply range-fold              ; variadic path
            (lambda (_ . xs) (or (apply pred xs) (return #f)))
            #t
            r
            rs))))

(define (range-map proc . rs)
  (assume (pair? rs))
  (vector-range (apply range-map->vector proc rs)))

(define (range-filter-map proc . rs)
  (assume (pair? rs))
  (vector-range (list->vector (apply range-filter-map->list proc rs))))

(define (range-map->list proc r . rs)
  (assume (procedure? proc))
  (if (null? rs)                        ; one-range fast path
      (%range-fold-right-1 (lambda (res x) (cons (proc x) res)) '() r)
      (apply range-fold-right           ; variadic path
             (lambda (res . xs) (cons (apply proc xs) res))
             '()
             r
             rs)))

(define (range-filter-map->list proc r . rs)
  (if (null? rs)                        ; one-range fast path
      (%range-fold-right-1 (lambda (res x)
                             (cond ((proc x) =>
                                    (lambda (elt) (cons elt res)))
                                   (else res)))
                           '()
                           r)
      (apply range-fold-right           ; variadic path
             (lambda (res . xs)
               (cond ((apply proc xs) => (lambda (elt) (cons elt res)))
                     (else res)))
             '()
             r
             rs)))

(define (range-map->vector proc r . rs)
  (assume (procedure? proc))
  (assume (range? r))
  (if (null? rs)                        ; one-range fast path
      (vector-unfold (lambda (i) (proc (%range-ref-no-check r i)))
                     (range-length r))
      (let ((rs* (cons r rs)))          ; variadic path
        (vector-unfold (lambda (i)
                         (apply proc (map (lambda (r)
                                            (%range-ref-no-check r i))
                                          rs*)))
                       (short-minimum (map range-length rs*))))))

(define (range-for-each proc r . rs)
  (assume (procedure? proc))
  (assume (range? r))
  (if (null? rs)                        ; one-range fast path
      (let ((len (range-length r)))
        (let lp ((i 0))
          (cond ((= i len) (if #f #f))
                (else (proc (%range-ref-no-check r i))
                      (lp (+ i 1))))))
      (let* ((rs* (cons r rs))          ; variadic path
             (len (short-minimum (map range-length rs*))))
        (let lp ((i 0))
          (cond ((= i len) (if #f #f))
                (else
                 (apply proc (map (lambda (r)
                                    (%range-ref-no-check r i))
                                  rs*))
                 (lp (+ i 1))))))))

(define (%range-fold-1 proc nil r)
  (assume (procedure? proc))
  (assume (range? r))
  (let ((len (range-length r)))
    (let lp ((i 0) (acc nil))
      (if (= i len)
          acc
          (lp (+ i 1) (proc acc (%range-ref-no-check r i)))))))

(define range-fold
  (case-lambda
    ((proc nil r)                       ; one-range fast path
     (%range-fold-1 proc nil r))
    ((proc nil . rs)                    ; variadic path
     (assume (procedure? proc))
     (assume (pair? rs))
     (let ((len (short-minimum (map range-length rs))))
       (let lp ((i 0) (acc nil))
         (if (= i len)
             acc
             (lp (+ i 1)
                 (apply proc acc (map (lambda (r)
                                        (%range-ref-no-check r i))
                                      rs)))))))))

(define (%range-fold-right-1 proc nil r)
  (assume (procedure? proc))
  (assume (range? r))
  (let ((len (range-length r)))
    (let rec ((i 0))
      (if (= i len)
          nil
          (proc (rec (+ i 1)) (%range-ref-no-check r i))))))

(define range-fold-right
  (case-lambda
    ((proc nil r)                       ; one-range fast path
     (%range-fold-right-1 proc nil r))
    ((proc nil . rs)                    ; variadic path
     (assume (procedure? proc))
     (assume (pair? rs))
     (let ((len (short-minimum (map range-length rs))))
       (let rec ((i 0))
         (if (= i len)
             nil
             (apply proc
                    (rec (+ i 1))
                    (map (lambda (r) (%range-ref-no-check r i)) rs))))))))

(define (range-filter pred r)
  (vector-range (list->vector (range-filter->list pred r))))

(define (range-filter->list pred r)
  (assume (procedure? pred))
  (assume (range? r))
  (range-fold-right (lambda (xs x)
                      (if (pred x) (cons x xs) xs))
                    '()
                    r))

(define (range-remove pred r)
  (vector-range (list->vector (range-remove->list pred r))))

(define (range-remove->list pred r)
  (assume (procedure? pred))
  (assume (range? r))
  (range-fold-right (lambda (xs x)
                      (if (pred x) xs (cons x xs)))
                    '()
                    r))

(define (range-reverse r)
  (assume (range? r))
  (%range-maybe-vectorize
   (raw-range (range-start-index r)
              (range-length r)
              (lambda (n)
                ((range-indexer r) (- (range-length r) 1 n)))
              (+ 1 (range-complexity r)))))

(define dummy-indexer
   (lambda (n) (error "range:dummy-indexer" "this indexer should not have been called" n)))

(define range-append
  (case-lambda
    (() (raw-range 0 0 dummy-indexer 0))
    ((r) r)                             ; one-range fast path
    ((ra rb)                            ; two-range fast path
     (let ((la (range-length ra))
           (lb (range-length rb)))
       (%range-maybe-vectorize          ; FIXME: should be lazy.
        (raw-range 0
                   (+ la lb)
                   (lambda (i)
                     (if (< i la)
                         (%range-ref-no-check ra i)
                         (%range-ref-no-check rb (- i la))))
                   (+ 2 (range-complexity ra) (range-complexity rb))))))
    (rs                                 ; variadic path
     (let ((lens (map range-length rs)))
       (%range-maybe-vectorize          ; FIXME: should be lazy.
        (raw-range 0
                   (sum lens)
                   (lambda (i)
                     (let lp ((i i) (rs rs) (lens lens))
                       (if (< i (car lens))
                           (%range-ref-no-check (car rs) i)
                           (lp (- i (car lens)) (cdr rs) (cdr lens)))))
                   (+ (length rs) (sum (map range-complexity rs)))))))))

;;;; Searching

(define (range-index pred r . rs)
  (assume (procedure? pred))
  (assume (range? r))
  (if (null? rs)                        ; one-range fast path
      (let ((len (range-length r)))
        (let lp ((i 0))
          (cond ((= i len) #f)
                ((pred (%range-ref-no-check r i)) i)
                (else (lp (+ i 1))))))
      (let* ((rs* (cons r rs))          ; variadic path
             (len (short-minimum (map range-length rs*))))
        (let lp ((i 0))
          (cond ((= i len) #f)
                ((apply pred
                        (map (lambda (s) (%range-ref-no-check s i))
                             rs*))
                 i)
                (else (lp (+ i 1))))))))

(define (range-index-right pred r . rs)
  (assume (procedure? pred))
  (assume (range? r))
  (if (null? rs)                        ; one-range fast path
      (let lp ((i (- (range-length r) 1)))
        (cond ((< i 0) #f)
              ((pred (%range-ref-no-check r i)) i)
              (else (lp (- i 1)))))
      (let ((len (range-length r))      ; variadic path
            (rs* (cons r rs)))
        (assume (every (lambda (s) (= len (range-length s))) rs)
                "range-index-right: ranges must be of the same length")
        (let lp ((i (- len 1)))
          (cond ((< i 0) #f)
                ((apply pred
                        (map (lambda (s) (%range-ref-no-check s i))
                             rs*))
                 i)
                (else (lp (- i 1))))))))

(define (range-take-while pred r)
  (cond ((range-index (lambda (x) (not (pred x))) r) =>
         (lambda (i) (range-take r i)))
        (else r)))

(define (range-take-while-right pred r)
  (cond ((range-index-right (lambda (x) (not (pred x))) r) =>
         (lambda (i) (range-take-right r (- (range-length r) 1 i))))
        (else r)))

(define (range-drop-while pred r)
  (cond ((range-index (lambda (x) (not (pred x))) r) =>
         (lambda (i) (range-drop r i)))
        (else (%empty-range-from r))))

(define (range-drop-while-right pred r)
  (cond ((range-index-right (lambda (x) (not (pred x))) r) =>
         (lambda (i) (range-drop-right r (- (range-length r) 1 i))))
        (else (%empty-range-from r))))

;;;; Conversion

(define (range->list r)
  (range-fold-right xcons '() r))

(define (range->vector r)
  (assume (range? r))
  (vector-unfold (lambda (i) (%range-ref-no-check r i))
                 (range-length r)))

(define (range->string r)
  (assume (range? r))
  (let ((res (make-string (range-length r))))
    (range-fold (lambda (i c) (string-set! res i c) (+ i 1)) 0 r)
    res))

(define (vector->range vec)
  (assume (vector? vec))
  (vector-range (vector-copy vec)))

(define (range->generator r)
  (assume (range? r))
  (let ((i 0) (len (range-length r)))
    (lambda ()
      (if (>= i len)
          (eof-object)
          (begin
           (let ((v (%range-ref-no-check r i)))
             (set! i (+ i 1))
             v))))))