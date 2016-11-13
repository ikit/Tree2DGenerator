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
embrchmt_min = 2
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

def rotate(angle, vecteur):
    cs = math.cos(angle)
    sn = math.sin(angle)
    return rotcs(cs, sn, vecteur[0], vecteur[1])
     
def rotcs(cos, sin, x, y):
    return ( (cos*x) - (sin * y), (sin*x) + (cos*y))





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
    if ordre == 0:
        return (root, [])
    embrchmt = gener_embrch (root)

    arbre = [gener_squelette(ordre-1, b) for b in embrchmt]
    return (root, arbre)





# print( gener_squelette(0, (0,1)) )
# print( gener_squelette(1, (0,1)) )
# print( gener_squelette(2, (0,1)) )
# print( gener_squelette(3, (0,1)) )





# convertit le sqelette pour tracer l'arbre en fil de fer
def sqlt2coord_fdfer(squelette, point):
    if len(squelette) == 0: 
        return []


    point_suivant = v_add(point, squelette[0])
        
    res = [(point, point_suivant)]
    for b in squelette[1]:
        res += sqlt2coord_fdfer(b, point_suivant)
    return res


  
pygame.init()
  
# set up the window
DISPLAYSURF = pygame.display.set_mode((800, 800), 0, 32)
pygame.display.set_caption('Drawing')
  
# set up the colors
BLACK = (  0,   0,   0)
WHITE = (255, 255, 255)
RED   = (255,   0,   0)
GREEN = (  0, 255,   0)
BLUE  = (  0,   0, 255)
  
# draw on the surface object
DISPLAYSURF.fill(BLACK)
#pygame.draw.polygon(DISPLAYSURF, GREEN, ((146, 0), (291, 106), (236, 277), (56, 277), (0, 106)))
#pygame.draw.line(DISPLAYSURF, BLUE, (60, 60), (120, 60), 4)
#pygame.draw.line(DISPLAYSURF, BLUE, (120, 60), (60, 120))
#pygame.draw.line(DISPLAYSURF, BLUE, (60, 120), (120, 120), 4)
#pygame.draw.circle(DISPLAYSURF, BLUE, (300, 50), 20, 0)
#pygame.draw.ellipse(DISPLAYSURF, RED, (300, 200, 40, 80), 1)
#pygame.draw.rect(DISPLAYSURF, RED, (200, 150, 100, 50))
  
#pixObj = pygame.PixelArray(DISPLAYSURF)
#pixObj[380][280] = BLACK
#pixObj[382][282] = BLACK
#pixObj[384][284] = BLACK
#pixObj[386][286] = BLACK
#pixObj[388][288] = BLACK
#del pixObj



arbre = gener_squelette(4, (0,100))
print(arbre)
segments = sqlt2coord_fdfer(arbre, (400, 400))
print(segments)

for s in segments:
    pygame.draw.line(DISPLAYSURF, WHITE, s[0], s[1])

# run the game loop
while True:
    for event in pygame.event.get():
        if event.type == QUIT:
            pygame.quit()
            sys.exit()
    pygame.display.update()


