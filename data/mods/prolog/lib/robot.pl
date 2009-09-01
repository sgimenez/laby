laby_name_left:- write('left\n'),read_atom(_).

laby_name_right:- write('right\n'),read_atom(_).

laby_name_forward:- write('forward\n'),read_atom(_).

laby_name_take:- write('take\n'),read_atom(_).

laby_name_drop:- write('drop\n'),read_atom(_).

laby_name_escape:- write('escape\n'),read_atom(_).

laby_name_look(X):- write('look\n'),read_atom(X).

laby_name_say(X):- write('say '),write(X),write('\n'),read_atom(_).

start:- write('start\n'),read_atom(_),laby_name_ant.

:-initialization(start).
