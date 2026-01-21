
def set_of_sums(single, num, i, acc):
  '''
  Returns the value of acc, with the a set of sums appended to it. The sums
  are found by adding each integer in the string num, with the value of single.

  set_of_sums: Nat Str (listof Nat) => (listof Nat)
  '''
  if i == len(num):
    return acc
  else:
    pair_sum = [single + int(num[i])]
    return set_of_sums(single, num, i+1, acc+pair_sum)

def full_set(num, i, acc):
  '''
  Returns the value of acc, with a full set of all the possible sums in num,
  made by uniquely pairing each number with each other.
  
  full_set: Str (listof Nat) => (listof Nat)
  '''
  if i == len(num)-1:
    return acc
  else:
    sum_set = set_of_sums(int(num[i]), num[i+1:], 0, [])
    return full_set(num, i+1, acc+sum_set)

def pairs(num, total):
  '''
  Returns the number of pairs that exist in num, such that the sum of those two
  numbers equals to the value of total.
  
  pairs: Nat Nat => Nat
  
  Examples:
    pairs(444, 8) => 3
    pairs(4343461, 7) => 7
    pairs(111, 1) => 0
    pairs(0, 10) => 0
  '''
  return full_set(str(num), 0, []).count(total)
  
print(pairs(444, 8))
