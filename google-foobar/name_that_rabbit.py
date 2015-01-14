#!/usr/bin/python2

def answer(names):
    def key(name):
        return (sum(ord(char) - 96 for char in name), name)

    return sorted(names, key=key, reverse=True)

if __name__ == "__main__":
    print answer(["annie", "bonnie", "liz"])
    print answer(["abcdefg", "vi"])
