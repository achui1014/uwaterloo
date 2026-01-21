def filter_out(elements, ind, m, acc):
    if ind == len(elements):
        return acc
    elif elements[ind] < m:  # Include elements less than or equal to m
        el_within = [elements[ind]]
        return filter_out(elements, ind + 1, m, acc + el_within)
    else:
        return filter_out(elements, ind + 1, m, acc)

def comb_element(current_element, combs, ind):
    if ind == len(combs):
        return []
    return [[current_element] + combs[ind]] + comb_element(current_element, combs, ind+1)

def generate_combinations(elements, ind):
    if ind == len(elements):
        return [[]]
    rest_combs = generate_combinations(elements, ind+1)
    current_element = elements[ind]
    combine_all = comb_element(current_element, rest_combs, 0)
    return rest_combs + combine_all

def combinations(elements): 
    return generate_combinations(elements, 0)

def sum_list(combs, i, acc):
    if i == len(combs):
        return acc
    else:
        i_sum = sum(combs[i])
        return sum_list(combs, i + 1, acc + [i_sum])

def num_of_solutions(elements, m):
    '''
    Returns the number of unique combinations of elements that sum up to m.
    '''
    # Filter out elements less than or equal to m
    first_filt = filter_out(elements, 0, m, [])
    
    # Generate combinations
    combs = combinations(first_filt)
    
    # Sum the combinations
    comb_sums = sum_list(combs, 0, [])
    
    # Count the occurrences of m in filtered list and combination sums
    m_count = elements.count(m) + comb_sums.count(m)
    
    return m_count

# Test cases
print(num_of_solutions([3, 34, 4, 12, 5, 2], 9))  # Output: 2
print(num_of_solutions([5, 5, 5, 5, 5, 5], 5))   # Output: 6
print(num_of_solutions([8], 8))                  # Output: 1
print(num_of_solutions([8], 17))                 # Output: 0
