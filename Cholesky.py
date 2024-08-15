import math
def print_matrix(matrix):
    for row in matrix:
        print(" ".join(f"{elem: .2f}" for elem in row))

def LU_decomposition(A):
    n = len(A)
    L = [[0]*n for _ in range(n)]
    U = [row[:] for row in A]  

    for k in range(n):
        L[k][k] = 1
        for i in range(k+1, n):
            if U[k][k] == 0:
                raise ZeroDivisionError("Zero pivot encountered in LU decomposition.")
            m = U[i][k] / U[k][k]
            L[i][k] = m
            for j in range(k, n):
                U[i][j] -= m * U[k][j]

    return L, U

def solve_lower_triangular(L, b):
    n = len(b)
    y = [0] * n
    for i in range(n):
        s = sum(L[i][j] * y[j] for j in range(i))
        y[i] = b[i] - s
    return y

def solve_upper_triangular(U, b):
    n = len(b)
    x = [0] * n
    x[-1] = b[-1] / U[-1][-1]
    for i in range(n-2, -1, -1):
        s = sum(U[i][j] * x[j] for j in range(i+1, n))
        x[i] = (b[i] - s) / U[i][i]
    return x

def limiar(matrix, limiar=1e-10):
    if isinstance(matrix[0], list):  
        for row in matrix:
            for i in range(len(row)):
                if abs(row[i]) < limiar:
                    row[i] = 0
    else:  # It's a vector
        for i in range(len(matrix)):
            if abs(matrix[i]) < limiar:
                matrix[i] = 0

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
L, U = LU_decomposition(Ai)
print("L =")
print_matrix(L)
print("U =")
print_matrix(U)

limiar(L)
limiar(U)

# Solve the systems
y = solve_lower_triangular(L, bi)
limiar(y)
print("y =", y)

x = solve_upper_triangular(U, y)
limiar(x)
print("x =", x)
