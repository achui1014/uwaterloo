## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##   
##   CS 231 code download
##   Version date: February 2022
##   File name: equiv.py
##   
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##
def equiv(one, two):
    """
    equiv(one, two) produces True if one and two contain the same 
        data items (in any order) and False otherwise.
    equiv: (listof Any) (listof Any) -> Bool

    """
    if len(one) != len(two):
        return False
    else:
        for item in one:
            if one.count(item) != two.count(item):
                return False
        return True

def equiv_count(one, group):
    """
    equiv_count(one, group) produces the number of lists in group
        equivalent (as used in equiv) to one.
    equiv_count: (listof Any) (listof (listof Any)) -> Bool

    """
    result = 0
    for item in group:
        if equiv(one, item):
            result = result + 1
    return result

def equiv_lol(one, two):
    """
    equiv_lol(one, two) produces True if one and two contain the same
        lists (in any order), where lists are the same if they contain
        the same items in any order,  and False otherwise.
    equiv_lol: (listof (listof Any)) (listof (listof Any)) -> Bool

    """
    if len(one) != len(two):
        return False
    else:
        for item in one:
            if equiv_count(item, one) != equiv_count(item, two):
                return False
        return True

def in_equiv(one, two):
    """
    in_equiv(one, two) produces True if two contains a list
        equivalent to one and False otherwise.
    in_equiv: (listof Any) (listof (listof Any)) -> Bool

    """
    for list in two:
        if equiv(one, list):
            return True
    return False
    


