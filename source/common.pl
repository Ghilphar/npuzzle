
% this declare itself as a module and export 2 predicates (grid_size and dir)
:- module( common, [ grid_size/2, dir/2 ] ).

% Grid size determines the size L of a grid G
grid_size(G, L):- length(G, L), maplist( {L}/[E]>>length(E, L), G ).

% dir describe the possible directions of movement on a grid
% right
dir((X, Y), (X1, Y)):- X1 is X + 1.
% down
dir((X, Y), (X, Y1)):- Y1 is Y + 1.
% left
dir((X, Y), (X1, Y)):- X1 is X - 1.
% up
dir((X, Y), (X, Y1)):- Y1 is Y - 1.
