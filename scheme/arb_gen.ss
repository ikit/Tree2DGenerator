
; =====================================================================================================================
;                                             ALGORYTHMES CREANT L ARBRE
; =====================================================================================================================
; Variables globales
(define sqlt '()) ; squelette de l'arbre (contient uniquement les vecteurs des branches : direction & norme)
(define arb '())  ; une fois les vecteurs convertient en segments
(define arb2 '()) ; une fois les segements convertient en quadrilatère

(define meth_avorte  null) ; ces variables <methode> seront initialisées plus tard
(define meth_basse   null) ; car elles designeront des méthodes qui vont être maintenant
(define meth_normale null) ; defini
(define meth_haute   null)

;-------------------------
; Definition des differentes methode de generation d'angle aleatoire "controle"
;-------------------------
; aleatoire simple
(define (meth_rand_aleat a_rand)
  (deg->rad (random a_rand)))

; conifere
(define (meth_rand_conifere a_rand)
  (deg->rad (/ a_rand 2)))


;------------------------
; Definition des differentes methode pour normer les branches n+1
;------------------------
; ne change rien (donne des 'arbres buissons')
(define (meth_brch_normal vect_brch)
  vect_brch)

; Simuler 'la gravite': cree des branches longues ou avortees
; cette methode utilise 3 rapports (constantes)
;                       3 autres methodes pour faire varier alpha
;                       1 methode pour faire avorter les branches deffectueuses
; -------
(define rap_brch_basse   0.7) ; -> 70% de la taille initiale
(define rap_brch_normale 1.0)
(define rap_brch_haute   1.3)
(define rap_brch_avorte  0.3)
(define rap_ang_basse    0.5)  ; -> l'angle initiale se rapproche de 50% de l'horizontale
(define rap_ang_normale  1.0)
(define rap_ang_haute    0.5)
(define rap_ang_avorte   0.0)

(define (aprox_ang v1 v2 %ang) ; rapproche v2 de v1 de x% de l'angle qu'ils forment tout les deux (renvoie v2' )
  (let* ((theta (angle v1))
         (alpha (angle v2))
         (alpha_prim (- theta (* (- theta alpha) (- 1 %ang)))))
    (rotate (- alpha_prim alpha) v2)))
         
         
  
; methode angle branches
; - BASSES
(define (ang_basse_rien vect) vect)
(define (ang_basse_descent vect)
  (if (> (angle_norme vect) pi/2)
      (aprox_ang '(-1 . 0) vect rap_ang_basse)
      (aprox_ang '(1 . 0) vect rap_ang_basse)))
(define (ang_basse_chute vect)
  (aprox_ang '(0 . -1) vect rap_ang_basse))
(define (ang_basse_monte vect)
  (aprox_ang '(0 . 1) vect rap_ang_basse))

; - NORMALES
(define (ang_normale_rien vect) vect)
(define (ang_normale_monte vect) 
  (aprox_ang '(0 . 1) vect rap_ang_normale))
(define (ang_normale_descent vect)
  (if (> (angle_norme vect) pi/2)
      (aprox_ang '(-1 . 0) vect rap_ang_normale)
      (aprox_ang '(1 . 0) vect rap_ang_normale)))
(define (ang_normale_chute vect)
  (aprox_ang '(0 . -1) vect rap_ang_normale))
  
; - HAUTES
(define (ang_haute_rien vect) vect)
(define (ang_haute_monte vect)
  (aprox_ang '(0 . 1) vect rap_ang_haute))
(define (ang_haute_descent vect)
  (if (> (angle_norme vect) pi/2)
      (aprox_ang '(-1 . 0) vect rap_ang_haute)
      (aprox_ang '(1 . 0) vect rap_ang_haute)))
      
; - AVORTEES
(define (avorte_brch_reduit vect) (s*v rap_brch_avorte vect))
(define (avorte_brch_rien vect) vect )
                              
; -------
; fonction qui applique à toutes les branches les méthodes séléctionnées precécedement
(define (meth_brch_gravite vect_brc)
  (let ((a_brch (angle_norme vect_brc)))               ; angle du vecteur branche (norme 0<a<2pi )
    (cond ((> a_brch pi) (meth_avorte vect_brc))
          ((or (<= a_brch pi/4) (>= a_brch 3pi/4))     ; branche basse
           (meth_basse (s*v rap_brch_basse vect_brc)))
          ((or (<= a_brch pi/3) (>= a_brch 2pi/3))     ; branche qcq
           (meth_normale (s*v rap_brch_normale vect_brc)))
          (else                                        ; branche haute
           (meth_haute (s*v rap_brch_haute vect_brc)))
   )))


  

;------------------------
; Definition des fonctions principales de generation de branche
;------------------------
; les variables globales utilisées
(define embrchmt-min 3)
(define embrchmt-max 1)
(define tropisme (/ (* 2 pi) 3))
(define rap_brch 0.7)
(define meth_rand meth_rand_aleat)
(define meth_brch meth_brch_normal)

; génére l'embranchement n+1
(define (gener_embrch_n+1  brch_n)
  (let* ((nbr_embrchmt (+ (random embrchmt-max) ;  = nombre de branche qui pousse
                          embrchmt-min))
         (demi_tropi (/ tropisme 2))                 ; radian
         (alpha_delta (/ tropisme nbr_embrchmt))     ; radian
         (alpha_depar (- (angle brch_n) demi_tropi)) ; radian
         (vect_base (rotate (- demi_tropi) brch_n))
         (alpha_random (+ 1 (inexact->exact (round (rad->deg alpha_delta)))))) ; en degre car random d'entier
    (let aux ((i 0) (res '() ))
      (if (= i nbr_embrchmt)
          (reverse res)
          (let* ((alpha-prim (meth_rand alpha_random)) ; en radian
                 (alpha (+  (* i alpha_delta) alpha-prim))
                 (vect-prim (s*v rap_brch (rotate alpha vect_base))) ; on positionne la branche n+1 et on regle sa taille
                 (vect (meth_brch vect-prim))) ; /!\ on modifie le vect grace a la methode meth_brch
            (aux (+ i 1) (cons (list vect) res))))
      )))
  
; genere l'arbre d'ordre n en utilisant gener_embrch_n+1
(define (gener_squelette ordre sqlt)
  (if (= 0 ordre) sqlt
      (let* ((embrchmt (gener_embrch_n+1  (car sqlt)))
             (arbre
              (let aux ((lst embrchmt) (res '()))
                (if (null? lst)
                    res
                    (aux (cdr lst)
                         (append res (list (gener_squelette (- ordre 1)  (car lst)))))))))
             (append sqlt arbre)))
      )

    



; =====================================================================================================================
;                                     CONVERSION DU SQUELETTE EN COORDONNEES
; =====================================================================================================================
; convertit le sqelette pour tracer l'arbre en fil de fer
(define (sqlt->coord_fdfer squelette point)
  (if (null? squelette) '()
      (let* ((point-suivant (v+ point (car squelette)))
             (suite-sqlt (cdr squelette))
             (segment (list point point-suivant)))
        (let aux ((lst suite-sqlt) (res (list segment)))
          (if (null? lst)
              res
              (aux (cdr lst)
                   (append res (sqlt->coord_fdfer (car lst) point-suivant))))))))
; trace un arbre fil de fer
(define (trace-fdfer sql)
  (if (null? sql) 'fini
      (let* ((ligne (car sql))
            (p1 (car ligne))
            (p2 (cadr ligne)))
        (fen: line (x p1) (y p1) (x p2) (y p2))
        (trace-fdfer (cdr sql)))))

; convertit le sqelette pour tracer l'arbre avec des quadrilateres /!\ necessite de le faire d'abord en fil de fer
; utilise une constante pour le rapport entre les 2 bases
(define rap_amincissement 0.5)

(define (sqlt->coord_Vdir squelette point)
  (if (null? squelette) '()
      (let* ((point-suivant (v+ point (car squelette)))
             (suite-sqlt (cdr squelette))
             (segment (list point point-suivant)))
        (cons segment  (map (lambda (x) (sqlt->coord_Vdir x point-suivant))
                                   suite-sqlt)))))


(define (sqlt->coord_quad sqlt_fdfer 1/2taille_base)
  (if (null? sqlt_fdfer) '()
      (let* ((ligne (car sqlt_fdfer))
             (mAB (car ligne))
             (mDC (cadr ligne))
             (Vdir (cons (- (x mDC) (x mAB))
                         (- (y mDC) (y mAB))))            ; vecteur directeur de la branche
             (Vdir_p (cons (* -1 (y Vdir))                ; vecteur perpendiculaire à Vdir ( /!\ même norme)
                           (x Vdir)))
             (Vdir_u_p (s*v (/ 1 (norme Vdir_p)) Vdir_p)) ; vecteur unitaire perpend à Vdir
             (1/2AB (s*v 1/2taille_base Vdir_u_p))
             (1/2taille_base_suivante (* 1/2taille_base rap_amincissement))
             (1/2DC (s*v 1/2taille_base_suivante Vdir_u_p))
             (A (v- mAB 1/2AB))
             (B (v+ mAB 1/2AB))
             (C (v+ mDC 1/2DC))
             (D (v- mDC 1/2DC))
             (sqlt-suite (cdr sqlt_fdfer))
             (quad (if (null? sqlt-suite) ; alors derniere branche , donc on met des feuilles
                       (list A B C D #t) ; afin d'indiquer qu'il faut tracer les feuilles
                       (list A B C D))))
        (let aux ((lst sqlt-suite) (res (list quad)))
          (if (null? lst)
              res
              (aux (cdr lst)
                   (append res (sqlt->coord_quad (car lst) 1/2taille_base_suivante))))))))

  
  
; trace un arbre "epaix"
(define (trace-quad sqlt)
  (if (null? sqlt) 'fini
      (let* ((quad (car sqlt))
             (p1 (car quad))
             (p2 (cadr quad))
             (p3 (caddr quad))
             (p4 (cadddr quad))
             (feuille? (if (= 5 (length quad))
                           #t
                           #f)))
        (if feuille?
            (meth_feuilles p1 p2 p3 p4)
            (trace-branche p1 p2 p3 p4))
        (trace-quad (cdr sqlt)))))

