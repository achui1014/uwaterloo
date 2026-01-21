##
## *******************************************************************
## Ashley Chui (21003047)
## CS 231 Spring 2024
## Assignment 1, P2
## *******************************************************************
##

from graphs import *
from equiv import *
from trees import *


def ancestor_colours(tree, choice):
    '''
    ancestor_colours(tree, choice) produces a list of strings of 
    colours of ancestors of the node with ID choice in tree.
    
    ancestor_colours: Tree Str => (listof Str)
    Requires:
      * len(choice) = 1
      * choice is a valid ID in of a node in tree
    '''
    colours = []
    curr_id = choice
    while tree.parent(curr_id) != None:
        colours.append(tree.node_colour(tree.parent(curr_id)))
        curr_id = tree.parent(curr_id)
    return colours