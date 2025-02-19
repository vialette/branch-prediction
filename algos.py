def kmp_table(X):
    m = len(X)
    tab =[-1] * (1+m)
    i, j = 0, -1
    while i<m:
        while j>-1 and X[i] != X[j]:
            j = tab[j]
        i, j = i+1, j+1
        if i<m and X[i] == X[j]:
            tab[i] = tab[j]
        else:
            tab[i] = j
    return tab

def mp_table(X):
    m = len(X)
    tab =[-1] * (1+m)
    i, j = 0, -1
    while i<m:
        while j>-1 and X[i] != X[j]:
            j = tab[j]
        i, j = i+1, j+1
        tab[i] = j
    return tab

def search_predict(X, T, table_func):
    m,n = len(X), len(T)
    tab = table_func(X)
    i,j = 0, 0
    taken_or_not, res = [], []
    while j<n:
        s = ""
        while i>-1 and X[i] != T[j]: # T^+ N  ou T^+
            i = tab[i]
            s += '1'
        if i>-1: s += '0'
        taken_or_not.append(s)
        i,j = i+1, j+1
        if i>=m:
            res.append(j-i)
            i = tab[i]
    return "".join(taken_or_not), res