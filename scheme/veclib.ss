;; Utilitaires
;; ===========
(require (lib "defmacro.ss"))

(define pi 3.14159265358979324)
(define 2pi (* 2.0 pi))
(define pi/3 (/ pi 3.0))
(define 2pi/3 (* 2.0 pi/3))
(define (sqr x) (* x x))

;;; {\sf L'intervalle g-bénérique : 0, 1, 2, \dots{} n-1}
(define (interval0 n)
  (let boucle ((n (- n 1)) (l ()))
    (if (< n 0) l
        (boucle (- n 1) (cons n l)))))

;;; {\sf Interv. entre a et b, avec ntrm pts. interm.}
(define (interval a b ntrm)
  (let* ((n (+ ntrm 1))
         (i (interval0 n))
         (dlt (/ (- b a) ntrm)))
    (map (lambda (x) (+ a (* x dlt))) i)))

;; Fonctionnelles: zipWith et folding
(define (zipWith op l1 l2)
  (if (null? l1) ()
      (cons (op (car l1) (car l2))
            (zipWith op (cdr l1) (cdr l2)))))

(define (foldr op ini l)
  (if (null? l) ini
      (op (car l) (foldr op ini (cdr l)))))
;; Version récursive terminale
(define (foldl op buf l)
  (if (null? l) buf
      (foldl op (op (car l) buf) (cdr l))))
(define (sum l)
  (foldr + 0 l))



;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; Vecteurs : paires (x . y). Un peu d'algèbre

(define-macro (x vec) `(car ,vec))
(define-macro (y vec) `(cdr ,vec))

(define (v+ v1 v2)
  (cons (+ (x v1) (x v2)) (+ (y v1) (y v2))))
(define (v- v1 v2)
  (cons (- (x v1) (x v2)) (- (y v1) (y v2))))
(define (s*v s v) (cons (* s (x v)) (* s (y v))))

(define (rotate ang v)
  (let ((cs (cos ang))
        (sn (sin ang)))
    (rotcs cs sn (x v) (y v))))
(define (rotcs c s x y)
  (cons (- (* c x) (* s y))
        (+ (* s x) (* c y))))
(define pi 3.141592653589793)
(define 2pi (* 2 pi))

(define dgr (/ pi 180))

(define (scalpr u v)
  (+ (* (x u) (x v)) (* (y u) (y v))))
(define (vecpr u v)
  (- (* (x u) (y v)) (* (y u) (x v))))

;; ===========================
;; 3D

(define-macro (x3 vec) `(car ,vec))
(define-macro (y3 vec) `(cadr ,vec))
(define-macro (z3 vec) `(caddr ,vec))
(define (v3+ v1 v2)
  (zipWith + v1 v2))
(define (v3- v1 v2)
  (zipWith - v1 v2))
(define (s*v3 s v) (map (lambda (x) (* s x)) v))

(define (scalpr3 v1 v2)
  (sum (zipWith * v1 v2)))
(define (norm2 v)
  (scalpr3 v v))
(define (norm v) (sqrt (norm2 v)))
(define (normalize v)
  (s*v3 (/ 1.0 (norm v)) v))

(define (vecpr3 u v)
  (list (- (* (y3 u) (z3 v)) (* (z3 u) (y3 v)))
        (- (* (z3 u) (x3 v)) (* (x3 u) (z3 v)))
        (- (* (x3 u) (y3 v)) (* (y3 u) (x3 v)))))

;; PROJECTIONS
(define (paral n v)
  (s*v3 (scalpr3 v n) n))
(define (perp n v)
  (v3- v (paral n v)))

;; ROTATION, Formule de Rodrigues
(define (rot3cs n ca sa v)
  (let* ((vpar (paral n v))
         (vperp (v3- v vpar))
         (v3 (vecpr3 n vperp))
         (vpp (v3+ (s*v3 ca vperp) (s*v3 sa v3))))
    (v3+ vpar vpp)))
(define (rotate3 n ang v)
  (rot3cs n (cos ang) (sin ang) v))
