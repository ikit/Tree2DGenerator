#!python
# coding: utf-8

import os
import sys
import operator
import math
import random
import pygame

from math import acos
from math import sqrt
from math import pi
from pygame.locals import *

def meth_brch_normal(vect_brch):
    return vect_brch
def meth_rand_aleat(a_rand):
    return deg2rad(random.randrange(a_rand))


# les variables globales utilisées
X_MAX = 800
Y_MAX = 800

tree_xmin = X_MAX
tree_xmax = 0
tree_ymin = Y_MAX
tree_ymax = 0

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PARAMETRISATION DE L'ARBRE
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# taille de la première branche (en pixel)
taille_branche_initiale = 100

embrchmt_min = 1
embrchmt_max = 4
tropisme     = (2 * pi) / 3.0
rap_brch     = 0.7
meth_rand    = meth_rand_aleat
meth_brch    = meth_brch_normal

# lors du dessin quadrilatère des branche, le rapport de taille entre la base [AB] et le sommet [DC]
rap_amincissement = 0.5


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# TOOLS
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
def vect(p1, p2):
    return (p2[0] - p1[0], p2[1]-p1[1])
def length(v):
    return sqrt(v[0]**2+v[1]**2)
def dot_product(v,w):
   return v[0]*w[0]+v[1]*w[1]
def determinant(v,w):
   return v[0]*w[1]-v[1]*w[0]
def inner_angle(v,w):
    # print("\ninner_angle(",v, ", ", w, ")", end="")
    cosx=dot_product(v,w)/(length(v)*length(w))
    rad=acos(cosx) # in radians
    return rad*180/pi # returns degrees
def angle_clockwise(A, B):
    inner=inner_angle(A,B)
    det = determinant(A,B)
    if det<0: #this is a property of the det. If the det < 0 then B is clockwise of A
        return inner
    else: # if the det > 0 then A is immediately clockwise of B
        return 360-inner
def angle(vecteur):
    return angle_clockwise(vecteur, (1,0))
    
def rad2deg(angle):
    return angle * 180 / pi

def deg2rad(angle):
    return angle * pi / 180


def s_v (scalaire, vecteur) :
    return (vecteur[0] * scalaire, vecteur[1] * scalaire)

def v_add(v1, v2):
    return (v1[0] + v2[0], v1[1] + v2[1])
def v_minus(v1, v2):
    return (v1[0] - v2[0], v1[1] - v2[1])

def rotate(angle, vecteur):
    cs = math.cos(angle)
    sn = math.sin(angle)
    return rotcs(cs, sn, vecteur[0], vecteur[1])
     
def rotcs(cos, sin, x, y):
    return ( (cos*x) - (sin * y), (sin*x) + (cos*y))




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# GENERATION DE L'ARBRE
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# génére l'embranchement n+1
def gener_embrch(brch_n):
    # print("\ngener_embrch start : ", brch_n, end="")
    nbr_embrchmt = random.randrange(embrchmt_min, embrchmt_max)
    demi_tropi   = tropisme / 2 # radian
    alpha_delta  = tropisme / nbr_embrchmt # radian
    alpha_depar  = angle(brch_n) - demi_tropi # radian
    vect_base    = rotate(-demi_tropi, brch_n)
    alpha_random = 1 + round( rad2deg(alpha_delta))  # en degre car random d'entier

    result = []
    for i in range(0, nbr_embrchmt):
        alpha_prim = meth_rand(alpha_random) # en radian
        alpha      = (i * alpha_delta) + alpha_prim
        vect_prim  = s_v(rap_brch, rotate(alpha, vect_base)) # on positionne la branche n+1 et on regle sa taille
        vect       = meth_brch(vect_prim) # /!\ on modifie le vect grace a la methode meth_brch
        result.append(vect)
    # print("\ngener_embrch result : ", result, end="")
    return result



# genere l'arbre d'ordre n en utilisant gener_embrch_n+1
def gener_squelette(ordre, root):
    # generation of the skeleton
    if ordre == 0:
        return (root, [])
    embrchmt = gener_embrch (root)

    arbre = [gener_squelette(ordre-1, b) for b in embrchmt]
    return (root, arbre)


def update_tree_bounds(branch):
    global tree_xmin, tree_xmax, tree_ymin, tree_ymax
    if branch[0] < tree_xmin :
        tree_xmin = branch[0]
    if branch[0] > tree_xmax:
        tree_xmax = branch[0]
    if branch[1] < tree_ymin:
        tree_ymin = branch[1]
    if branch[1] > tree_ymax:
        tree_ymax = branch[1]





# convertit le sqelette pour tracer l'arbre en fil de fer
def sqlt2coord_fdfer(squelette, point):
    if len(squelette) == 0: 
        return None

    point_suivant = v_minus(point, s_v(taille_branche_initiale, squelette[0]))
    return ((point, point_suivant), [sqlt2coord_fdfer(b, point_suivant) for b in squelette[1]])



# convertit le sqelette pour tracer l'arbre avec des quadrilateres /!\ necessite de le faire d'abord en fil de fer


def sqlt2coord_quad(sqlt_fdfer, demi_taille_base):
    ligne = sqlt_fdfer[0]
    mAB      = ligne[0]
    mDC      = ligne[1]
    Vdir     = (mDC[0] - mAB[0], mDC[1] - mAB[1]) # vecteur directeur de la branche
    Vdir_p   = (Vdir[1] * -1, Vdir[0])            # vecteur perpendiculaire à Vdir ( /!\ même norme)
    Vdir_u_p = s_v(1/length(Vdir_p), Vdir_p)       # vecteur unitaire perpend à Vdir

    demi_AB = s_v(demi_taille_base, Vdir_u_p)
    demi_taille_base_suivante = demi_taille_base * rap_amincissement
    demi_DC = s_v(demi_taille_base_suivante, Vdir_u_p)
    A = v_minus(mAB, demi_AB)
    B = v_add(mAB, demi_AB)
    C = v_add(mDC, demi_DC)
    D = v_minus(mDC, demi_DC)

    return ((A, B, C, D), [sqlt2coord_quad(s, demi_taille_base_suivante) for s in sqlt_fdfer[1]])




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# METHODES POUR LE DESSIN
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# draw gradient 
def fill_gradient(surface, color, gradient, rect=None, vertical=True, forward=True):
    """fill a surface with a gradient pattern
    Parameters:
    color    -> starting color
    gradient -> final color
    rect     -> area to fill; default is surface's rect
    vertical -> True=vertical; False=horizontal
    forward  -> True=forward; False=reverse
    
    Pygame recipe: http://www.pygame.org/wiki/GradientCode
    """
    if rect is None: rect = surface.get_rect()
    x1,x2 = rect.left, rect.right
    y1,y2 = rect.top, rect.bottom
    if vertical: h = y2-y1
    else:        h = x2-x1
    if forward: a, b = color, gradient
    else:       b, a = color, gradient
    rate = (
        float(b[0]-a[0])/h,
        float(b[1]-a[1])/h,
        float(b[2]-a[2])/h
    )
    fn_line = pygame.draw.line
    if vertical:
        for line in range(y1,y2):
            color = (
                min(max(a[0]+(rate[0]*(line-y1)),0),255),
                min(max(a[1]+(rate[1]*(line-y1)),0),255),
                min(max(a[2]+(rate[2]*(line-y1)),0),255)
            )
            fn_line(surface, color, (x1,line), (x2,line))
    else:
        for col in range(x1,x2):
            color = (
                min(max(a[0]+(rate[0]*(col-x1)),0),255),
                min(max(a[1]+(rate[1]*(col-x1)),0),255),
                min(max(a[2]+(rate[2]*(col-x1)),0),255)
            )
            fn_line(surface, color, (col,y1), (col,y2))


# la montagne (aleatoire: méthode mid)
#(define (getvar m)
#  (let aux ((res 0) (n m))
#    (if (= 0 n) 
#        (- res (/ m 2))
#        (aux (+ res (/ (random 1000) 1000.0)) (- n 1)))))


def getvar(n):
    if n <= 0 : 
        return 0
    return random.randrange(2 * n) - n;


def milieu(p1, p2):
    return ((p1[0]+p2[0])/2, (p1[1]+p2[1])/2)

# Dessine montagne (par dicotomie + récursif)
def gen_mountain(color, seg, delta, p1, p2):


    pM_init = milieu(p1, p2)
    pM = (pM_init[0], pM_init[1]+getvar(delta))

    if seg == 0:
        pygame.draw.polygon(DISPLAYSURF, color, [(p1[0], Y_MAX), (p2[0], Y_MAX), p2, p1])
        pygame.draw.line(DISPLAYSURF, BLACK, p1, p2)
    else:
        d = round(delta/2)
        gen_mountain(color, seg-1, d, p1, pM)
        gen_mountain(color, seg-1, d, pM, p2)




pygame.init()
  
# set up the window


DISPLAYSURF = pygame.display.set_mode((X_MAX, Y_MAX), 0, 32)
pygame.display.set_caption('2D tree generator')
  
# set up the colors
# Bases
BLACK = (  0,   0,   0)
WHITE = (255, 255, 255)
RED   = (255,   0,   0)
GREEN = (  0, 255,   0)
BLUE  = (  0,   0, 255)

# les branches / troncs
C_BRUN = (255, 200, 100)
C_NOIR  = (0, 0, 0)
C_BLANC = (255, 255, 255)
# les sols
C_NEIGE = (239, 249, 255)
C_HERBE = (93, 186, 80)
C_PAILLASSE = (158, 108, 49)
# le ciel 
C_CIEL_BEAU1 = (9, 90, 238)
C_CIEL_BEAU2 = (192, 200, 255)
C_CIEL_COUVERT1 = (158, 170, 198)
C_CIEL_COUVERT2 = (163, 163, 163)

# les feuilles
C_PRINTEMP1 = (11, 220, 80)
C_PRINTEMP2 = (94, 185, 124)
C_ETE1 = (23, 135, 23)
C_ETE2 = (9, 97, 9)
C_AUTOMNE1 = (255, 238, 90)
C_AUTOMNE2 = (255, 172, 90)
C_AUTOMNE3 = (232, 77, 52)
C_AUTOMNE4 = (148, 88, 17) # contour feuille
  




# Dessine en mode file de fer
def draw_line(line):
    pygame.draw.line(DISPLAYSURF, BLUE, line[0], line[1])

# Dessine polygone en mode file de fer
def draw_quad(quad):
    pygame.draw.polygon(DISPLAYSURF, WHITE, quad, 1)

def draw_quad2(quad):
    pygame.draw.polygon(DISPLAYSURF, C_BRUN, quad)
    pygame.draw.line(DISPLAYSURF, C_NOIR, quad[0], quad[3])
    pygame.draw.line(DISPLAYSURF, C_NOIR, quad[1], quad[2])


# Fonction qui trace une branche avec:
# trace_quad_seg   = nbr de quad par branche
# trace_quad_delta = variabilité du positionement des quad les uns par rapports au autres (en %)
# trace_quad_meth  = méthode pour dessiner les branches
trace_quad_seg = 10
trace_quad_delta = 20
trace_quad_meth = draw_quad2
def draw_multi_quad(quad):
    AD = v_minus(quad[3], quad[0])
    BC = v_minus(quad[2], quad[1])

    ADu = s_v(1/trace_quad_seg, AD)
    BCu = s_v(1/trace_quad_seg, BC)
    ADp = (-1 * ADu[1], ADu[0]) # vecteur perpendiculaire à ADu ( /!\ même norme)
    BCp = (-1 * BCu[1], BCu[0]) # vecteur perpendiculaire à BCu ( /!\ même norme)


    A = quad[0]
    B = quad[2]
    M = milieu(A, B)
    M = (round(M[0]), round(M[1]))
    pygame.draw.circle(DISPLAYSURF, RED, M, round(length(vect(A,B))), 1)

    def recursive_draw(i, p1, p2):
        if i <= 0 :
            trace_quad_meth((p1, p2, quad[2], quad[3]))
        else:
            variation = (random.randrange(2 * trace_quad_delta) - trace_quad_delta) / 100.0
            p3_prim = v_add(p2, BCu)
            p4_prim = v_add(p1, ADu)
            p3 = v_add(p3_prim, s_v(variation, BCp))
            p4 = v_add(p4_prim, s_v(variation, ADp))
            trace_quad_meth((p1, p2, p3, p4))
            recursive_draw(i-1, p4, p3)

    recursive_draw(trace_quad_seg, quad[0], quad[1])





# Dessinbe l'arbre en appliquand les méthodes de dessin fournies
def draw_tree(skeleton, branch_meth, sheet_meth):
    if len(skeleton) == 0 :
        return


    if branch_meth is not None:
        branch_meth(skeleton[0])
    if len(skeleton[1]) == 0 and sheet_meth is not None:
        sheet_meth(skeleton[0])

    for sqlt in skeleton[1]:
        draw_tree(sqlt, branch_meth, sheet_meth)



ordre = 6

def draw_sheet_t1(branch):
    A = branch[0]
    C = branch[2]
    AB = vect(A, branch[1])
    AD = vect(A, branch[3])
    coeff = 0.1 * ordre
    coeff2 = coeff + 1
    Aprim = v_minus(v_minus(A, s_v(coeff, AD)), s_v(coeff, AB))
    ABprim = s_v(coeff2, AB)
    ADprim = s_v(coeff2, AD)

    for i in range(0,20):
        c1 = v_add(Aprim, s_v(random.randrange(0, 100)/100, ABprim))
        c2 = v_add(c1,    s_v(random.randrange(0, 100)/100, ADprim))
        r  = (50 + random.randrange(0, 100))/1000
        #pygame.draw.circle(DISPLAYSURF, C_PRINTEMP2, (round(abs(c2[0])), round(abs(c2[1]))), 5, 0)
        M = milieu(A, C)
        M = (round(M[0]), round(M[1]))
        pygame.draw.circle(DISPLAYSURF, RED, M, round(length(vect(A,C))), 1)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# MAIN 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

arbre = gener_squelette(4, (0,1))
# print(arbre)
segments = sqlt2coord_fdfer(arbre, (400, 600))
# print(segments)
quads = sqlt2coord_quad(segments, 20)


# Draw decor
fill_gradient(DISPLAYSURF, C_CIEL_BEAU1, C_CIEL_BEAU2)

y2 = Y_MAX/2
y4 = Y_MAX/4
gen_mountain(C_HERBE, 7, y4, (0, y2 + getvar(y4)), (X_MAX, y2 + getvar(y4)))

    
# Draw quad
draw_tree(quads, draw_multi_quad, draw_sheet_t1)

# Draw skeleton
draw_tree(segments, draw_line, None)

# run the game loop
while True:
    for event in pygame.event.get():
        if event.type == QUIT:
            pygame.quit()
            sys.exit()
    pygame.display.update()


