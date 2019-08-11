import math

def isPrime(n):
    for i in range(2, int(math.sqrt(n))+1):
        if n % i == 0:
            return False
    return True

import random

def is_prime(n, test_count=3):
    for a in random.sample(range(n), test_count):
        if not pow(a, n, n) == a:
            return False
    return True

def fib(n):
    pass
