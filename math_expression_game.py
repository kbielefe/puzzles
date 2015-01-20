#!/usr/bin/python

def answer(numbers=6, stack=0):
    if numbers == 0 and stack == 1:
        return 1
    count = 0
    if stack > 1:
        count += answer(numbers, stack-1)
        count += answer(numbers, stack-1)
        count += answer(numbers, stack-1)
        count += answer(numbers, stack-1)
    for i in range(numbers):
        count += answer(numbers-1, stack+1)
    return count

if __name__ == "__main__":
    print(answer())
