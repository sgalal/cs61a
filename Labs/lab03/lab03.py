""" Lab 3: Recursion and Python Lists """

def skip_add(n):
    """ Takes a number x and returns x + x-2 + x-4 + x-6 + ... + 0.

    >>> skip_add(5)  # 5 + 3 + 1 + 0
    9
    >>> skip_add(10) # 10 + 8 + 6 + 4 + 2 + 0
    30
    >>> # Do not use while/for loops!
    >>> from construct_check import check
    >>> check('lab03.py', 'skip_add',
    ...       ['While', 'For'])
    True
    """
    helper = lambda n, acc: acc if n < 0 else helper(n - 2, acc + n)
    return helper(n, 0)


this_file = __file__

def hailstone(n):
    """Print out the hailstone sequence starting at n, and return the
    number of elements in the sequence.

    >>> a = hailstone(10)
    10
    5
    16
    8
    4
    2
    1
    >>> a
    7
    >>> # Do not use while/for loops!
    >>> from construct_check import check
    >>> check(this_file, 'hailstone',
    ...       ['While', 'For'])
    True
    """
    gen_next_value = lambda x: x // 2 if x % 2 == 0 else x * 3 + 1
    helper = lambda n, acc: print(n) or (acc + 1 if n == 1 else helper(gen_next_value(n), acc + 1))
    return helper(n, 0)

def summation(n, term):

    """Return the sum of the first n terms in the sequence defined by term.
    Implement using recursion!

    >>> summation(5, lambda x: x * x * x) # 1^3 + 2^3 + 3^3 + 4^3 + 5^3
    225
    >>> summation(9, lambda x: x + 1) # 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10
    54
    >>> summation(5, lambda x: 2**x) # 2^1 + 2^2 + 2^3 + 2^4 + 2^5
    62
    >>> # Do not use while/for loops!
    >>> from construct_check import check
    >>> check(this_file, 'summation',
    ...       ['While', 'For'])
    True
    """
    assert n >= 1
    helper = lambda n, acc: acc if n == 0 else helper(n - 1, acc + term(n))
    return helper(n, 0)
