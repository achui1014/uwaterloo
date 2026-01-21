def filter_out(elements, ind, m, acc):
    '''
    Returns the value of acc with the values in elements which are less than
    or equal to m, starting from list index ind.
    
    filter_out: (listof Nat) Nat Nat (listof Nat) => (listof Nat)
    Requires:
    ind < len(elements)
    '''
    if ind == len(elements):
      return acc
    elif elements[ind] < m:
      el_within = [elements[ind]]
      return filter_out(elements, ind+1, m, acc+ el_within)
    else:
      return filter_out(elements, ind+1, m, acc)

def combinations(elements):
    def generate_combinations(elements):
        if elements == []:
            return [[]]  # Base case: return an empty combination
        rest_combinations = generate_combinations(elements[1:])
        current_element = elements[0]
        
        # Helper function to accumulate combinations with the current element
        def accumulate_combinations(current_combination, combinations):
            if current_combination == []:
                return combinations
            return accumulate_combinations(current_combination[1:], combinations + [[current_element] + current_combination[0]])
        
        # Call the helper function to accumulate combinations with the current element
        return accumulate_combinations(rest_combinations, [])
    
    return generate_combinations(elements)

def sum_list(combs, i, acc):
    if i == len(combs):
       return acc
    else:
       i_sum = sum(combs[i])
       return sum_list(combs, i+1, acc+[i_sum])

def num_of_solutions(elements, m):
    '''
    Returns the number of unique combinations of elements that sum up to m.
    
    num_of_solutions: (listof Nat) Nat => Nat
    
    Examples:
    num_of_solutions([3,34,4,12,5,2],9) => 2
    num_of_solutions([5,5,5,5,5,5],5) => 6
    num_of_solutions([8],8) => 1
    num_of_solutions([8],17) => 0
    '''
    m_count = elements.count(m)
    first_filt = filter_out(elements, 0, m, [])
    comb_sums = sum_list(combinations(first_filt), 0, [])
    m_count = m_count + comb_sums.count(m)
    return m_count

print(combinations([1, 2, 3]))