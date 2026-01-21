##
## *******************************************************************
## Ashley Chui (21003047)
## CS 231 Spring 2024
## Assignment 2, P2
## *******************************************************************
##

def align_helper(loi, first, last):
    '''
    align_helper produces a Boolean value determined by whether a
    a position p exists between the first and last position in loi,
    where the value of position p is p.

    align_helper: (listof Int) Int Int => Bool
    '''
    if first > last:
        return False
    mid = (first+last)//2
    if loi[mid] == mid:
        return True
    elif loi[mid] > mid:
        return align_helper(loi, first, mid-1)
    else:
        return align_helper(loi, mid+1, last)

def align(loi):
    '''
    align produces a Boolean value determined by whether a positon p
    exists in the list of integers loi, where the value of position p
    is p.

    align: (listof Int) => Bool
    '''
    return align_helper(loi, 0, len(loi) - 1)