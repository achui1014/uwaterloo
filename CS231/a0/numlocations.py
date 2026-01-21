##
## *******************************************************************
## Ashley Chui (21003047)
## CS 231 Spring 2024
## Assignment 1, P1
## *******************************************************************
##

from grids import *

def num_locations(grid, item):
    '''
    num_locations(grid, item) produces the number of entries in grid 
    that contain item.

    num_locations: Grid Str => Nat
    '''
    num_rows = grid.row_num()
    num_col = grid.col_num()
    locs = 0
    for i in range(num_rows):
        for j in range(num_col):
            if grid.access(i,j) == item:
                locs += 1
    return locs