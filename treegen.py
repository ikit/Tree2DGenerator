import os
import operator
import math
from math import acos
from math import sqrt
from math import pi
import random
##import pygame
##from pygame.locals import *

def meth_brch_normal(vect_brch):
    return vect_brch
def meth_rand_aleat(a_rand):
    return deg2rad(random.randrange(a_rand))


# les variables globales utilisées
embrchmt_min = 1
embrchmt_max = 3
tropisme     = (2 * pi) / 3.0
rap_brch     = 0.7
meth_rand    = meth_rand_aleat
meth_brch    = meth_brch_normal



# tools
def length(v):
    return sqrt(v[0]**2+v[1]**2)
def dot_product(v,w):
   return v[0]*w[0]+v[1]*w[1]
def determinant(v,w):
   return v[0]*w[1]-v[1]*w[0]
def inner_angle(v,w):
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
def angle(v):
    return angle_clockwise(v, (1,0))
    
def rad2deg(angle):
    return angle * 180 / pi

def deg2rad(angle):
    return angle * pi / 180


def s_v (s, v) :
    return (v[0] * s, v[1] * s)

def rotate(angle, vecteur):
    cs = math.cos(angle)
    sn = math.sin(angle)
    return rotcs(cs, sn, vecteur[0], vecteur[1])
     
def rotcs(c, s, x, y):
    return ( (c*x) - (s * y), (s*x) + (c*y))





# génére l'embranchement n+1
def gener_embrch(brch_n):
    nbr_embrchmt = random.randrange(embrchmt_min, embrchmt_max)
    demi_tropi   = tropisme / 2 # radian
    alpha_delta  = tropisme / nbr_embrchmt # radian
    alpha_depar  = angle(brch_n) - demi_tropi # radian
    vect_base    = rotate(-demi_tropi, brch_n)
    alpha_random = 1 + round( rad2deg(alpha_delta))  # en degre car random d'entier

    def aux(i, res):
        if i == nbr_embrchmt :
            return res
        alpha_prim = meth_rand(alpha_random) # en radian
        alpha      = (i * alpha_delta) + alpha_prim
        vect_prim  = s_v(rap_brch, rotate(alpha, vect_base)) # on positionne la branche n+1 et on regle sa taille
        vect       = meth_brch(vect_prim) # /!\ on modifie le vect grace a la methode meth_brch
        return aux(i+1, (list(vect), res))
    
    return aux(0, [])



# genere l'arbre d'ordre n en utilisant gener_embrch_n+1
def gener_squelette(ordre, sqlt):
    if ordre == 0:
        return sqlt
    embrchmt = gener_embrch (sqlt[0])
    arbre = [gener_squelette(ordre-1, b) for b in embrchmt]
    return sqlt.append(arbre)







gener_squelette(2, [(0,1)])




















##pygame.init()
##
###Ouverture de la fenêtre Pygame
##fenetre = pygame.display.set_mode((640, 480))
##
###Chargement et collage du fond
##fond = pygame.image.load("C:/Users/Olive/Desktop/background.jpg").convert()
##fenetre.blit(fond, (0,0))
##
###Chargement et collage du personnage
##perso = pygame.image.load("C:/Users/Olive/Desktop/perso.png").convert_alpha()
##fenetre.blit(perso, (200,300))
##
###Rafraîchissement de l'écran
##pygame.display.flip()
##
###BOUCLE INFINIE
##continuer = 1
##while continuer:
##	for event in pygame.event.get():
##		if event.type == QUIT:
##			continuer = 0
