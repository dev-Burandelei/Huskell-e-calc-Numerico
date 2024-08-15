import math

def comparar(x, xk, eps):
    soma = sum(math.fabs(x_i - xk_i) for x_i, xk_i in zip(x, xk))
    return soma < eps

def gaussSeidel(A, b, maxiter, eps):
    n = len(b)
    x = [0] * n  
    
    for i in range(n):
        if math.fabs(A[i][i]) > 1e-10:
            x[i] = b[i] / A[i][i]
        else:
            raise ValueError("Zero or near-zero diagonal element found, Gauss-Seidel method may fail.")
    
    print("Iteração 0")
    print("x = ", x)
    
    for iter in range(1, maxiter + 1):
        xk = x.copy()
        for i in range(n):
            s1 = sum(A[i][j] * xk[j] for j in range(i))  
            s2 = sum(A[i][j] * x[j] for j in range(i + 1, n))  
            xk[i] = (b[i] - s1 - s2) / A[i][i]
        
        print("Iteração:", iter)
        print("xk = ", xk)
        
        if comparar(x, xk, eps):
            return xk
        
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
x = gaussSeidel(A, b, 10, 0.01)
print("x =", x)
