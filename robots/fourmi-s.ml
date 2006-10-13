#!/usr/bin/env srobot

avance;
gauche;
droite;
tant que rien devant faire
  avance
fait;
tant que regarde = Vide faire
  avance
fait;
si regarde = Mur alors
  gauche
sinon
  avance
fin si;
gauche;
