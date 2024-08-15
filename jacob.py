import math

def comparar(x, xk, eps):
    soma = sum(math.fabs(list1_i - list2_i) for list1_i, list2_i in zip(x, xk))
    return soma < eps

def gaussJacobi(A, b, maxiter, eps):
    n = len(b)
    x = [0] * n
    for i in range(n):
        if math.fabs(A[i][i]) > 1e-10:
            x[i] = b[i] / A[i][i]
        else:
            raise ValueError("Zero or near-zero diagonal element found, Gauss-Jacobi method may fail.")
    
    print("Iteração 0")
    print("x = ", x)
    
    iter = 0
    while iter < maxiter:
        iter += 1
        xk = [0] * n
        for i in range(n):
            s = sum(A[i][j] * x[j] for j in range(n) if i != j)
            xk[i] = (b[i] - s) / A[i][i]

        print("Iteração:", iter)
        print("xk = ", xk)
        
        if comparar(x, xk, eps):
            break
        
        x = xk
    
    return x
'''
A = [[3, 2, 4],
      [1, 1, 2],
      [4, 3, 2]]

b = [1, 2, 3]
'''

A = [[10, 1, 1, 0, 0, 0, 0, 0, 0, 0],
    [1, 10, 1, 1, 0, 0, 0, 0, 0, 0],
    [1, 1, 10, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 1, 10, 1, 1, 0, 0, 0, 0],
    [0, 0, 1, 1, 10, 1, 1, 0, 0, 0],
    [0, 0, 0, 1, 1, 10, 1, 1, 0, 0],
    [0, 0, 0, 0, 1, 1, 10, 1, 1, 0],
    [0, 0, 0, 0, 0, 1, 1, 10, 1, 1],
    [0, 0, 0, 0, 0, 0, 1, 1, 10, 1],
    [0, 0, 0, 0, 0, 0, 0, 1, 1, 10]]

b = [15, 25, 35, 45, 55, 65, 75, 85, 95, 105]


x = gaussJacobi(A, b, 10, 0.01)
print("x =", x)
