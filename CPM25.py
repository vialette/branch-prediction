# This is some companion Python Code to the article
# "Theoretical Analysis of Branch Prediction Algorithms 
#  Morris-Pratt and Knuth-Morris-Pratt"
# By C. Nicaud, C. Pivoteau & S. Vialette
# Submitted to CPM 25

from sympy import *

def kmp_table(X):
    """ compute the table for KMP """
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
    """ compute the table for MP """
    m = len(X)
    tab =[-1] * (1+m)
    i, j = 0, -1
    while i<m:
        while j>-1 and X[i] != X[j]:
            j = tab[j]
        i, j = i+1, j+1
        tab[i] = j
    return tab


def transducer_TX(X, alphabet, table_func):
    """
        Compute the transducer TX
        
        Input
        - X: the pattern
        - alphabet: an iterable of letters, size >=2
        - table_func: the function that precompute the table 
                    (mp_table or kmp_table)

        Output
        - transitions: the transition system where a transition u-a->v
                     is encoded by transitions[len(u),a]=len(v)
        - output: the sequence of N/T for the letter comparisons
    """
    m = len(X)
    B = table_func(X)
    
    transitions = {(0, a): 0 for a in alphabet}
    output = {(0, a): 'T' for a in alphabet}
    
    for i in range(len(X)-1):
        a = X[i]
        p = transitions[i,a]
        
        transitions[i,a] = i+1
        output[i,a] = 'N'
        for b in alphabet:
            transitions[i+1,b] = transitions[p,b]
            if B[i+1] == -1:
                output[i+1,b] = 'T'
            else:
                output[i+1,b] = 'T' + output[B[i+1],b]
    output[len(X)-1,X[-1]] = 'N' # special case when the pattern is found
            
    return transitions, output


def automaton_AX(X, alphabet, table_func):      
    """
        Compute the automaton AX, as a transition system
    """     
    return transducer_TX(X, alphabet, table_func)[0]


def stationary_vector(transitions, alphabet):
    """
        Compute the stationary vector of the states of an automaton
        
        transitions: is a dict (source,letter) -> destination, 
                    where source & destination are states
        alphabet: is an iterable with the alphabet symbols, size >=2
        
        Output: a dict state->probability of state as a sympy formula
    """
    probas = {a:symbols('p'+a) for a in alphabet}
    last_letter = alphabet[-1]
    # changing the probability of the last letter to sum to 1
    probas[last_letter] = 1
    for k in range(len(alphabet)-1):
        a = alphabet[k]
        probas[last_letter] -= probas[a]
        
    states = list(set(s for s,a in transitions.keys()))
    variables = symbols(" ".join('x'+str(i) for i in range(len(states))))
    
    # Computing M^t-Id as a system of equation
    equations = [-variables[i] for i in range(len(states))] # -Id
    for state in states:
        for k in range(len(alphabet)):
            a = alphabet[k]
            i = states.index(state)
            j = states.index(transitions[state,a])
            equations[j] += probas[a] * variables[i]
    # adding that the sum of the coordinate is 1 to normalize the eigenvector
    equations.append(sum(variables)-1)

    # solving the system
    sol = solve(tuple(equations), tuple(variables))

    return {states[i]:sol[variables[i]] for i in range(len(states))}
 

def stationary_AX(X, alphabet, table_func):
    """
        Compute the stationary distribution of the states of AX
    """
    return stationary_vector(automaton_AX(X, alphabet, table_func), alphabet)


def stationary_AX_uniform(X, alphabet, table_func):
    """
        Compute the stationary distribution of the states of AX, for pi(a)=1/|alphabet|
    """
    v = stationary_AX(X, alphabet, table_func)
    uniform = {symbols('p'+a):Rational(1,len(alphabet)) for a in alphabet}
    return {s:simplify(v[s].subs(uniform)) for s in v}


def update_predictor(s, w):
    """ update the predictor state and count mispredictions
        Input: 
            - s: initial state of the predictor
            - w: sequence of N/T
        Output:
            - s: final state of the predictor
            - mp: number of mispredictions
    """
    mp = 0 # number of mispredictions in the process
    for x in w:
        if x == 'N':
            if s >= 2: mp += 1
            s = max(0,s-1) 
        else: 
            if s <= 1: mp += 1
            s = min(3,s+1)
    return s, mp


def transducer_PX(X, alphabet, table_func):
    """
        Compute the transducer PX
        
        Input
        - X: the pattern
        - alphabet: an iterable of letters, size >=2
        - table_func: the function that precompute the table 
                    (mp_table or kmp_table)

        Output
        - transitions: the transition system where a transition u-a->v
                     is encoded by transitions[len(u),a]=len(v)
        - output: the number of mispredictions for the letter comparisons
                    for each transition
    """    
    transitions, output = transducer_TX(X, alphabet, table_func)
    states = set([(0,3)])
    trans, mispred = {}, {} #transition and output of the final transducer
    todo = [(0,3)] # todo list for the traversal
    while len(todo)>0:
        s, p = todo.pop()
        for a in alphabet:
            t = transitions[s,a] # state image in TX
            w = output[s,a] # sequence of test produced while reading a
            q, misses = update_predictor(p, w) # predictor state after reading w
            trans[(s,p),a] = (t,q)
            mispred[(s,p),a] = misses
            if (t,q) not in states:
                states.add((t,q))
                todo.append((t,q))
    return trans, mispred

##############

def dico_variables(X):
    """
        Return the dictionnary a->pa where 
        a letter of X and pa associated sympy symbol
    """
    d = {}
    for a in set(X):
        d[a] = symbols("p"+a)
    return d

def dico_variables_normalized(alphabet):
    """
        Return the dictionnary a->pa where 
        a letter of alphabet and pa associated sympy symbol
        the last letter is set to 1 - sum_{other letters b} pb
    """
    d_variables = dico_variables(alphabet)
    last_letter = alphabet[-1]
    d_variables[last_letter] = 1
    for i in range(len(alphabet)-1):
        d_variables[last_letter] -= d_variables[alphabet[i]]
    return d_variables


def proba_word(X,alphabet):
    """
        return pi(X) as a product of sympy variables
    """
    d_variables = dico_variables_normalized(alphabet)
    r = 1
    for a in X: r *= d_variables[a]
    return r

def mispred_counter(X, alphabet):
    """
        return the expected number of misprediction for the counter update,
        the branch Line 7, according to Proposition 6.
    """
    if len(set(X)) == 1: # all the letter are identical
        p = symbols("pa")
        if len(X) == 1:
            return p*(1-p)/(1-2*p*(1-p))
        if len(X) == 2:
            return (1-p)*(p**2)*(1+2*p+p**2-p**3) / (1-p**3 + p**4)
        return (p**len(X)) *(1-p)*(1+p)**2
    return proba_word(X, alphabet)

def mispred_counter_uniform(X, alphabet):
    """
        return the expected number of misprediction for the counter update,
        the branch Line 7, according to Proposition 6, for the uniform 
        distribution on alphabet
    """
    uniform = {symbols('p'+a):Rational(1,len(alphabet)) for a in alphabet}
    return mispred_counter(X,alphabet).subs(uniform)


def mispred_comparisons(X, alphabet, table_func):
    """
        Compute the expected number of mispredictions of X[i]!=W[j],
        asymptotically, for each character of the text
        
        Input
            - X: the pattern
            - alphabet: an iterable of letters, size >=2
            - table_func: the function that precompute the table 
                    (mp_table or kmp_table)

        Output
            - sympy formula
    """
    transitions, output = transducer_PX(X, alphabet, table_func)
    v = stationary_vector(transitions, alphabet)
    
    d_variables = dico_variables(alphabet)
    last_letter = alphabet[-1]
    d_variables[last_letter] = 1
    for i in range(len(alphabet)-1):
        d_variables[last_letter] -= d_variables[alphabet[i]]
    
    r = 0
    for (state,letter),mispred in output.items():
        r += v[state] * d_variables[letter] * mispred
    return simplify(r)

def mispred_comparisons_uniform(X, alphabet, table_func):
    """
        Compute the expected number of mispredictions of X[i]!=W[j],
        asymptotically, for each character of the text, for the uniform
        distribution on alphabet
        
        Input
            - X: the pattern
            - alphabet: an iterable of letters, size >=2
            - table_func: the function that precompute the table 
                    (mp_table or kmp_table)

        Output
            - rational number
    """
    uniform = {symbols('p'+a):Rational(1,len(alphabet)) for a in alphabet}
    return mispred_comparisons(X, alphabet, table_func).subs(uniform)

def mispred_nonnegative_i(X, alphabet, table_func):
    """
        Compute the expected number of mispredictions of i>=0,
        asymptotically, for each character of the text
        
        Input
            - X: the pattern
            - alphabet: an iterable of letters, size >=2
            - table_func: the function that precompute the table 
                    (mp_table or kmp_table)

        Output
            - sympy formula
    """
    AX = automaton_AX(X, alphabet, table_func)
    v = stationary_AX(X, alphabet, table_func)
    
    d_variables = dico_variables_normalized(alphabet)

    mp = 0
    for state in range(len(X)):
        for a in alphabet:
            if AX[state,a] == 0 and (state,a) != (len(X)-1,X[-1]):
                mp += v[state] * d_variables[a]
    return simplify(mp)

def mispred_nonnegative_i_uniform(X, alphabet, table_func):
    """
        Compute the expected number of mispredictions of i>=0,
        asymptotically, for each character of the text, for the uniform
        distribution on alphabet
        
        Input
            - X: the pattern
            - alphabet: an iterable of letters, size >=2
            - table_func: the function that precompute the table 
                    (mp_table or kmp_table)

        Output
            - rational number
    """
    uniform = {symbols('p'+a):Rational(1,len(alphabet)) for a in alphabet}
    return mispred_nonnegative_i(X, alphabet, table_func).subs(uniform)


if __name__ == "__main__":
    alphabet = "ab"
    print("Alphabet size = ", len(alphabet))
    for X in ['aa', 'ab', 'aaa', 'aab', 'aba', 'abb']:
        print('-'*10+' pattern = '+X+' '+'-'*10)
        for algo in [mp_table, kmp_table]:
            if algo == mp_table: print('MP')
            else: print('KMP')

            mc = mispred_counter(X, alphabet)
            mcu = mispred_counter_uniform(X, alphabet)
            print("\ti=m       :  ", mc, ", latex=", latex(mc))
            print("\tuniform   :  ", mcu, ", float=", N(mcu, 3))
            print()

            mi = mispred_nonnegative_i(X, alphabet, algo)
            miu = mispred_nonnegative_i_uniform(X, alphabet, algo)
            print("\ti>=0      :  ", mi, ", latex=", latex(mi))
            print("\tuniform   :  ", miu, ", float=", N(miu, 3))
            print()

            ml = mispred_comparisons(X, alphabet, algo)
            mlu = mispred_comparisons_uniform(X, alphabet, algo)
            print("\tX[i]!=W[j]:  ", ml, ", latex=", latex(ml))
            print("\tuniform   :  ", mlu, ", float=", N(mlu, 3))
            print()



