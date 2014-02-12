#(define SIZE 10000)
SIZE = 10000

vec = [SIZE - i for i in range(SIZE)]

def bubble_sort(l):
    swapped = True
    while swapped:
        swapped = False
        i = 0
        while i < len(l) - 1:
            a, b = l[i], l[i + 1]
            if a > b:
                l[i] = b
                l[i + 1] = a
                swapped = True
            i += 1

bubble_sort(vec)
#assert vec == sorted(vec)
