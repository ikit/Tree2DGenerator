; =====================================================================================================================
;                                           APPLICATIONS GRAPHIQUES
; =====================================================================================================================

(define decor? #t)
; =============================
; Méthodes pour dessiner les branches
; colorié, fil de fer, couleurs progressives etc
; =============================
; Couleurs pour le dessin
(define C-NOIR (make-object color% 0 0 0))
(define C-BLANC (make-object color% 255 255 255))
; les branches / troncs
(define C-BRUN (make-object color% 255 200 100))
(define C-NOIR (make-object color% 0 0 0))
(define C-BLANC (make-object color% 255 255 255))
; les sols
(define C-NEIGE (make-object color% 239 249 255))
(define C-HERBE (make-object color% 93 186 80))
(define C-PAILLASSE (make-object color% 158 108 49))
; le ciel 
(define C-CIEL-BEAU1 '(9 90 238))
(define C-CIEL-BEAU2 '(192 200 255))
(define C-CIEL-COUVERT1 '(158 170 198))
(define C-CIEL-COUVERT2 '(163 163 163))

; les feuilles
(define C-PRINTEMP1 (make-object color% 11 220 80))
(define C-PRINTEMP2 (make-object color% 94 185 124))
(define C-ETE1 (make-object color% 23 135 23))
(define C-ETE2 (make-object color% 9 97 9))
(define C-AUTOMNE1 (make-object color% 255 238 90))
(define C-AUTOMNE2 (make-object color% 255 172 90))
(define C-AUTOMNE3 (make-object color% 232 77 52))
(define C-AUTOMNE4 (make-object color% 148 88 17)) ; contour feuille



; dessine un quadrilatère contours et bordures même couleur
(define (quad-plein quad coul)
  (fen: brush-color coul)
  (fen: pen-color coul)
  (fen: polygon (map (lambda (p) (make-object point% (x p) (y p))) quad) 0 0 'odd-even)
)

; Mode 1: dessine les branches façon dessin "réaliste"
(define (trace p1 p2 p3 p4)
  (quad-plein (list p1 p2 p3 p4)  C-BRUN)
  (fen: pen-color C-NOIR)
  (fen: line (x p1) (y p1) (x p4) (y p4))
  (fen: line (x p2) (y p2) (x p3) (y p3))
  (fen: line (x p3) (y p3) (x p4) (y p4))
  )

; Mode 2: dessine les branches en fils de fer
(define (fdfer p1 p2 p3 p4)
  (fen: pen-color C-NOIR)
  (fen: line (x p1) (y p1) (x p4) (y p4))
  (fen: line (x p2) (y p2) (x p3) (y p3))
  (fen: line (x p3) (y p3) (x p4) (y p4))
  (fen: line (x p1) (y p1) (x p2) (y p2))
  )





; =============================
; Fonction qui trace une branche avec:
; trace_quad_seg   = nbr de quad par branche
; trace_quad_delta = variabilité du positionement des quad les uns par rapports au autres (en %)
; trace_quad_meth  = méthode pour dessiner les branches
; =============================
(define trace_quad_seg 10)
(define trace_quad_delta 20) 
(define trace_quad_meth (lambda (x y z t) (trace x y z t)))

(define (trace-branche A B C D )
  (let* ((AD (cons (- (x D) (x A))
                   (- (y D) (y A))))
         (BC (cons (- (x C) (x B))
                   (- (y C) (y B))))
         (ADu (s*v (/ 1 trace_quad_seg) AD))
         (BCu (s*v (/ 1 trace_quad_seg) BC))
                  (ADp (cons (* -1 (y ADu)) ; vecteur perpendiculaire à ADu ( /!\ même norme)
                    (x ADu)))
         (BCp (cons (* -1 (y BCu)) ; vecteur perpendiculaire à BCu ( /!\ même norme)
                    (x BCu)))
         )
    (let aux ((i (- trace_quad_seg 1)) (A A) (B B))
      (cond ((<= i 0) (trace_quad_meth A B C D))
            (else
             (let* ((variation (/ (- (random (* 2 trace_quad_delta)) trace_quad_delta) 100))
                    (Cprim (v+ B BCu))
                    (Dprim (v+ A ADu))
                    (C (v+ Cprim (s*v variation BCp)))
                    (D (v+ Dprim (s*v variation ADp))))
               (trace_quad_meth A B C D)
               (aux (- i 1) D C)
               ))))))

; donne la longueur du quadrillatere
(define (long_quad A B C D)
  (let* ((AB (cons (- (x B) (x A))
                  (- (y B) (y A))))
        (DC (cons (- (x C) (x D))
                  (- (y C) (y D))))
        (mAB (v+ A (s*v 0.5 AB)))
        (mDC (v+ D (s*v 0.5 DC)))
        (mm (cons (- (x mDC) (x mAB))
                  (- (y mDC) (y mAB)))))
    (norme mm)
    ))

; quand on dessine l'arbre, on trace en premier les branches les plus grosses
(define (plq quad1 quad2)
  (> (long_quad (car quad1) (cadr quad1)
                      (caddr quad1) (cadddr quad1))
     (long_quad (car quad2) (cadr quad2)
                      (caddr quad2) (cadddr quad2))))
(define (tri sqlt)
  (mergesort sqlt plq))

; =============================
; Fonction qui trace le decor
; ciel + montagne (fond d'ecran)
; =============================
; les variables globales de couleurs pour le decors
(define COUL-CIEL1 C-CIEL-BEAU1)
(define COUL-CIEL2 C-CIEL-BEAU2)
(define COUL-SOL C-HERBE)


; le ciel (dégradé de coul1 vers coul2
(define (ciel etape xmin xmax ymin ymax)
  (let* ((delta_cR (- (car COUL-CIEL1) (car COUL-CIEL2))) ; variation de la composante Rouge
         (delta_cG (- (cadr COUL-CIEL1) (cadr COUL-CIEL2)))
         (delta_cB (- (caddr COUL-CIEL1) (caddr COUL-CIEL2)))
         (d_cR (- (/ delta_cR etape))); incrementation de la composante rouge pour aller de c1 à c2
         (d_cG (- (/ delta_cG etape)))
         (d_cB (- (/ delta_cB etape)))
         (dy (exact->inexact (/ (- ymax ymin) etape))))
    (let aux ((i etape) (y ymax) (cr (car COUL-CIEL1)) (cg (cadr COUL-CIEL1)) (cb (caddr COUL-CIEL1)))
      (let* ((yprim (- y dy))
              (R (truncate (floor (+ cr d_cr))))
              (G (truncate (floor (+ cg d_cg))))
              (B (truncate (floor (+ cb d_cb))))
              (coul (make-object color% cr cg cb)))
        (quad-plein (list (cons xmin y) (cons xmax y)
                          (cons xmax yprim)(cons xmin yprim))
                    coul)
        (if (> i 0)
            (aux (- i 1) yprim R G B))))))


; la montagne (aleatoire: méthode mid)
(define (getvar m)
  (let aux ((res 0) (n m))
    (if (= 0 n) 
        (- res (/ m 2))
        (aux (+ res (/ (random 1000) 1000.0)) (- n 1)))))
 (define (milieu p1 p2)
   (cons (/ (+ (x p1) (x p2)) 2.0)
         (/ (+ (y p1) (y p2)) 2.0)))

(define (gener_montagne seg delta p1 p2)
  (let* ((pM_init (milieu p1 p2))
         (pM (cons  (x pM_init) (+ (y pM_init) (getvar delta)))))
    (cond ((= 0 seg) ; alors on trace une partie de la montagne
           (quad-plein (list (cons (x p1) -1)
                             (cons (x p2) -1)
                             p2 p1 )
                       COUL-SOL)
           (fen: pen-color C-NOIR)
           (fen: line (x p1) (y p1) (x p2) (y p2)))
          (else (gener_montagne (- seg 1) delta p1 pM)
                (gener_montagne (- seg 1) delta  pM p2)))))



; =============================
; Fonctions pour dessiner les feuilles
; =============================
; variables globales pour dessiner les feuilles
(define COUL-FEUILL1 C-PRINTEMP1) ; feuille
(define COUL-FEUILL2 C-PRINTEMP2) ; bordure

(define meth_feuilles (lambda (x y z t) (display "feuilles  ")))
; ne dessine aucune feuille, seulement la branche
(define meth_feuilles_hiver (lambda (x y z t) (trace-branche x y z t)))
; dessine des petites feuilles
(define meth_feuilles_rondes (lambda (A B C D) 
                               (trace-branche A B C D)
                               (fen: brush-color COUL-FEUILL1)
                               (fen: pen-color COUL-FEUILL2)
                               (let* ((AB (cons (- (x B) (x A))
                                                (- (y B) (y A))))
                                      (AD (cons (- (x D) (x A))
                                                (- (y D) (y A))))
                                      (coeff (* 0.1 ordre))
                                      (coeff2 (+ 1  coeff))
                                      (Aprim (v- (v- A (s*v coeff AD))
                                                 (s*v coeff AB)))
                                      (ABprim (s*v coeff2 AB))
                                      (ADprim (s*v coeff2 AD)))
                                 (let aux ((nbr 20))
                                   (if (> nbr 0)
                                       (let* ((c1 (v+ Aprim (s*v (/ (random 100) 100.0) ABprim)))
                                              (c2 (v+ c1 (s*v (/ (random 100) 100.0) ADprim)))
                                              (r (/(+ 50 (random 100)) 1000.0)))
                                         (fen: centered-ellipse (x c2) (y c2) r r)
                                         (aux (- nbr 1) ))
                                       )))))
; dessine de gros paquet de feuilles
(define meth_feuilles_dru (lambda (A B C D) 
                            (meth_feuilles_rondes A B C D)))
; dessine des feuilles aux couleurs de l'automne
(define (poly_aleat imax poly)
  (let aux ((i imax) (res poly))
    (let* ((x (- (/ (random 250) 1000.0) 0.125))
           (y (- (/ (random 250) 1000.0) 0.125))
           (v (cons x y)))
      (if (> i 1)
          (aux (- i 1) (cons (v+ (car res) v) res))
          res))))
(define meth_feuilles_automne (lambda (A B C D) 
                                (trace-branche A B C D)
                                (fen: pen-color COUL-FEUILL2)
                                (let* ((AB (cons (- (x B) (x A))
                                                 (- (y B) (y A))))
                                       (AD (cons (- (x D) (x A))
                                                 (- (y D) (y A))))
                                       (coeff (* 0.2 ordre))
                                       (2coeff (+ 1  coeff))
                                       (Aprim (v- (v- A (s*v coeff AD))
                                                  (s*v coeff AB)))
                                       (ABprim (s*v 2coeff  AB))
                                       (ADprim (s*v 2coeff AD)))
                                  (let aux ((nbr 11))
                                    (if(> nbr 1)
                                       (let* ((p0 (v+ Aprim (s*v (/ (random 100) 100.0) ABprim)))
                                              (p1 (v+ p0 (s*v (/ (random 100) 100.0) ADprim)))
                                              (cotes (+ 3 (random 3)))
                                              (pol (poly_aleat cotes (list p1))))
                                         (let ((a (random 3))) ; couleur des feuilles aleatoire
                                           (cond ((= a 0) (set! COUL-FEUILL1 C-AUTOMNE1))
                                                 ((= a 1) (set! COUL-FEUILL1 C-AUTOMNE2))
                                                 ((= a 2) (set! COUL-FEUILL1 C-AUTOMNE3))))
                                         (fen: brush-color COUL-FEUILL1)
                                         (fen: polygon (map (lambda (p) (make-object point% (x p) (y p))) pol) 0 0 'odd-even)
                                         (aux (- nbr 1) ))
                                       ; si nbr > 1 , on met une feuille par terre
                                       (if (< (random (+ 1 ordre)) 2) ; sinon pour ordre = 10 trop de feuille apparaissent
                                           (let* ((p0 (v+ '(-4 . -2) (s*v (/ (random 100) 100.0) '(8 . 0))))
                                                  (p1 (v+ p0 (s*v (/ (random 100) 100.0) '(0 . 5))))
                                                  (cotes (+ 3 (random 3)))
                                                  (pol (poly_aleat cotes (list p1))))
                                             (let ((a (random 3))) ; couleur des feuilles aleatoire
                                               (cond ((= a 0) (set! COUL-FEUILL1 C-AUTOMNE1))
                                                     ((= a 1) (set! COUL-FEUILL1 C-AUTOMNE2))
                                                     ((= a 2) (set! COUL-FEUILL1 C-AUTOMNE3))))
                                             (fen: brush-color COUL-FEUILL1)
                                             (fen: polygon (map (lambda (p) (make-object point% (x p) (y p))) pol) 0 0 'odd-even))
                                        ))))))
; dessine les feuilles des coniferes
(define meth_feuilles_conifere (lambda (A B C D) 
                                (trace-branche A B C D)
                                (fen: pen-color COUL-FEUILL2)
                                (let* ((AB (cons (- (x B) (x A))
                                                 (- (y B) (y A))))
                                       (3AB (s*v 3 AB))
                                       (DC (cons (- (x C) (x D))
                                                 (- (y C) (y D))))
                                       (mAB (v+ A (s*v 0.5 AB)))
                                       (mDC (v+ D (s*v 0.5 DC)))
                                       (mm (cons (- (x mDC) (x mAB))
                                                 (- (y mDC) (y mAB))))
                                       (AD (cons (- (x D) (x A))
                                                 (- (y D) (y A))))
                                       (BC (cons (- (x C) (x B))
                                                 (- (y C) (y B))))
                                       (1/10AD (s*v 0.1 AD))
                                       (1/10BC (s*v 0.1 AD))
                                       (1/10mm (s*v 0.1 AD)))
                                  (let aux ((i 10) (p1 (v+ (v- A 3AB) 1/10AD))
                                                   (p2 mAB)
                                                   (p3 (v+ (v+ B 3AB) 1/10BC)))
                                    (if (> i 0)
                                        (begin0 (fen: line (x p1) (y p1) (x p2) (y p2))
                                                (fen: line (x p2) (y p2) (x p3) (y p3))
                                                (aux (- i 1) (v+ p1 1/10AD) (v+ p2 1/10mm) (v+ p3 1/10BC))))
                                    ))))



; =============================
; Fonction qui gere les variables dessin
; =============================
(define arbre 'dessin)

; pour modifier la représentation de l'arbre :
(define (dessin type)
  (cond ((eq? type 'fdfer)
         (set! trace_quad_meth (lambda (x y z t) (fdfer x y z t)))
         (set! arbre 'fdfer)
         ; il faut reconstruire l'arbre à partir du squelette
         (set! arb (sqlt->coord_fdfer sqlt '(0 . 0))))
        ((eq? type 'dessin)
         (set! trace_quad_meth   (lambda (x y z t) (trace x y z t)))
         (set! arbre 'dessin)
         ; il faut reconstruire l'arbre à partir du squelette
         (set! arb (sqlt->coord_Vdir sqlt '(0 . 0)))
         (set! arb2  (sqlt->coord_quad arb 0.2))
         (set! arb2 (tri arb2)))
	((eq? type 'dessfdfer)
	 (set! trace_quad_meth   (lambda (x y z t) (fdfer x y z t)))
         (set! arbre 'dessfdfer)
         ; il faut reconstruire l'arbre à partir du squelette
         (set! arb (sqlt->coord_Vdir sqlt '(0 . 0)))
         (set! arb2  (sqlt->coord_quad arb 0.2))
         (set! arb2 (tri arb2)))
        ))

; pour modifier les couleurs du dessin
(define (saison typ_lieu typ_temps typ_feuille) 
  (let ((lieu (cond ((eq? typ_lieu 'neige) 0)
                    ((eq? typ_lieu 'herbe) 1)
                    ((eq? typ_lieu 'friche) 2)
                    (else -1)))
        (tmps (cond ((eq? typ_temps 'beau) 0)
                    ((eq? typ_temps 'couvert) 1)
                    (else  -1)))
        (flle (cond ((eq? typ_feuille 'hiver) 0)
                    ((eq? typ_feuille 'printp) 1)
                    ((eq? typ_feuille 'ete) 2)
                    ((eq? typ_feuille 'automne) 3)
                    ((eq? typ_feuille 'conifere) 4)
                    (else -1))))
    (case lieu
      ((0) (set! COUL-SOL C-NEIGE))
      ((1) (set! COUL-SOL C-HERBE))
      ((2) (set! COUL-SOL C-PAILLASSE)))
    (case tmps
      ((0) (set! COUL-CIEL1 C-CIEL-BEAU1)
           (set! COUL-CIEL2 C-CIEL-BEAU2))
      ((1) (set! COUL-CIEL1 C-CIEL-COUVERT1)
           (set! COUL-CIEL2 C-CIEL-COUVERT1)))
    (case flle
      ((0) (set! meth_feuilles meth_feuilles_hiver))
      ((1) (set! meth_feuilles meth_feuilles_rondes)
           (set! COUL-FEUILL1 C-PRINTEMP1)
           (set! COUL-FEUILL2 C-PRINTEMP2))
      ((2) (set! meth_feuilles meth_feuilles_dru)
           (set! COUL-FEUILL1 C-ETE1)
           (set! COUL-FEUILL2 C-ETE2))
      ((3) (set! meth_feuilles meth_feuilles_automne)
           (set! COUL-FEUILL2 C-AUTOMNE4))
      ((4) (set! meth_feuilles meth_feuilles_conifere)
           (set! COUL-FEUILL2 C-ETE2))
    )
    ))
