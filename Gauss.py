import math

def print_matrix(matrix):
    for row in matrix:
        print(" ".join(f"{elem: .2f}" for elem in row))

def elimination(A, b):
    n = len(b)
    
    for k in range(n):
        # Find the pivot row and swap if necessary
        max_row = max(range(k, n), key=lambda i: abs(A[i][k]))
        if k != max_row:
            A[k], A[max_row] = A[max_row], A[k]
            b[k], b[max_row] = b[max_row], b[k]
        
        # Eliminate the column entries below the pivot
        for i in range(k + 1, n):
            if A[k][k] == 0:
                raise ZeroDivisionError("Zero pivot element encountered.")
            m = A[i][k] / A[k][k]
            A[i][k:] = [A[i][j] - m * A[k][j] for j in range(k, n)]
            b[i] -= m * b[k]

    return A, b

def solveUpperTriangular(U, b):
    n = len(b)
    x = [0] * n
    x[-1] = b[-1] / U[-1][-1]

    for i in range(n - 2, -1, -1):
        s = sum(U[i][j] * x[j] for j in range(i + 1, n))
        x[i] = (b[i] - s) / U[i][i]

    return x

def limiar(matriz):
    if isinstance(matriz[0], list):  
        for row in matriz:
            for i in range(len(row)):
                if abs(row[i]) < 1e-10:
                    row[i] = 0
    else:  # It's a vector
        for i in range(len(matriz)):
            if abs(matriz[i]) < 1e-10:
                matriz[i] = 0

'''
Ai = [[3, 2, 4],
      [1, 1, 2],
      [4, 3, 2]]

bi = [1, 2, 3]
'''
Ai = [[10, 1, 1, 0, 0, 0, 0, 0, 0, 0],
    [1, 10, 1, 1, 0, 0, 0, 0, 0, 0],
    [1, 1, 10, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 1, 10, 1, 1, 0, 0, 0, 0],
    [0, 0, 1, 1, 10, 1, 1, 0, 0, 0],
    [0, 0, 0, 1, 1, 10, 1, 1, 0, 0],
    [0, 0, 0, 0, 1, 1, 10, 1, 1, 0],
    [0, 0, 0, 0, 0, 1, 1, 10, 1, 1],
    [0, 0, 0, 0, 0, 0, 1, 1, 10, 1],
    [0, 0, 0, 0, 0, 0, 0, 1, 1, 10]]

bi = [15, 25, 35, 45, 55, 65, 75, 85, 95, 105]

U, b = elimination(Ai, bi)

limiar(U)
print("U =")
print_matrix(U)

#limiar(b)
#print("b =", b)

x = solveUpperTriangular(U, b)
limiar(x)
print("x =", x)

