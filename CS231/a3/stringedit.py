##
## ******************************************************************
## Ashley Chui (21003047)
## CS 231, Spring 2024
## Assignment 03, P1
## ******************************************************************
##

from grids import *

def string_edit(source, target, add_cost, delete_cost, sub_cost):
    '''
    string_edit(source, target, add_cost, delete_cost, sub_cost)
    produces the lowest cost of any editing sequence that can be
    used to edit source into target.
    
    string_edit: Str Str Int Int Int => Int
    '''
    # Intializing the table for DP
    len_s = len(source)
    len_t = len(target)
    table = Grid(len_s+1, len_t+1)

    for i in range(len_s+1):
        table.enter(i,0, i*delete_cost)
    for j in range(1, len_t+1):
        table.enter(0,j, j*add_cost)

    # Looping over the table to fill in entries
    for i in range(1, len_s+1):
        for j in range(1, len_t+1):
            if source[i-1]==target[j-1]:
                table.enter(i,j,table.access(i-1,j-1))
            else:
                table.enter(i,j, min(
                    table.access(i-1,j-1)+sub_cost,
                    table.access(i-1,j)+delete_cost,
                    table.access(i,j-1)+add_cost)
              )
    return table.access(i,j)
