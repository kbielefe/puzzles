#!/usr/bin/python2

# memoize from https://wiki.python.org/moin/PythonDecoratorLibrary#Memoize
class memoize(dict):
    def __init__(self, func):
        self.func = func
   
    def __call__(self, *args):
        return self[args]
    
    def __missing__(self, key):
        result = self[key] = self.func(*key)
        return result

@memoize
def factorial(n):
    if n == 0:
        return 1
    return n * factorial(n - 1)

class Node:

    def __init__(self, value):
        self.value = value
        self.children = [None, None]

    def insert(self, element):
        child = 0 if element < self.value else 1
        if self.children[child]:
            self.children[child].insert(element)
        else:
            self.children[child] = Node(element)

    def sorts(self):
        child_sorts = [child.sorts() for child in self.children if child]
        count = sum(x[0] for x in child_sorts)
        sorts = factorial(count)
        for (child_count, child_sort) in child_sorts:
            sorts = sorts * child_sort / factorial(child_count)

        return (count + 1, sorts)

def answer(xs):
    tree = Node(xs.pop(0))
    for x in xs:
        tree.insert(x)
    return str(tree.sorts()[1])

if __name__ == "__main__":
    assert answer([5,9,8,2,1]) == "6"
    assert answer([1,2,3,4,5,6,7,8,9,10]) == "1"
