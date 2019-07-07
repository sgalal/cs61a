""" Optional problems for Lab 3 """

def is_prime(n):
    """Returns True if n is a prime number and False otherwise.

    >>> is_prime(2)
    True
    >>> is_prime(16)
    False
    >>> is_prime(521)
    True
    """
    if n == 1:
        return False  # 1 is not considered to be a prime number
    helper = lambda i: True if i == n else False if n % i == 0 else helper(i + 1)
    return helper(2)

def gcd(a, b):
    """Returns the greatest common divisor of a and b.
    Should be implemented using recursion.

    >>> gcd(34, 19)
    1
    >>> gcd(39, 91)
    13
    >>> gcd(20, 30)
    10
    >>> gcd(40, 40)
    40
    """
    min_val, max_val = min(a, b), max(a, b)
    return min_val if max_val % min_val == 0 else gcd(min_val, max_val % min_val)

def ten_pairs(n):
    """Return the number of ten-pairs within positive integer n.

    >>> ten_pairs(7823952)
    3
    >>> ten_pairs(55055)
    6
    >>> ten_pairs(9641469)
    6
    """
    def count_targets(n, tar, acc):
        return acc if n == 0 else count_targets(n // 10, tar, acc + 1 if n % 10 == tar else acc)
    def iterate_digits(n, acc):
        return acc if n == 0 else iterate_digits(n // 10, acc + count_targets(n // 10, 10 - n % 10, 0))
    return iterate_digits(n, 0)
