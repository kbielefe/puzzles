#!/usr/bin/python2
from math import sqrt, floor

# memoize from https://wiki.python.org/moin/PythonDecoratorLibrary#Memoize
class memoize(dict):
    def __init__(self, func):
        self.func = func
   
    def __call__(self, *args):
        return self[args]
    
    def __missing__(self, key):
        result = self[key] = self.func(*key)
        return result

def int_sqrt(x):
    return int(floor(sqrt(x)))

def perfect_square(x):
    return int_sqrt(x)**2 == x

@memoize
def answer(n):
    if perfect_square(n):
        return 1
    return min(1 + answer(n - i**2) for i in range(int_sqrt(n),0,-1))

if __name__ == "__main__":
    print max(answer(i) for i in range(1,10001))
    assert answer(10000) == 1
    assert answer(2) == 2
    assert answer(24) == 3
    assert answer(160) == 2
