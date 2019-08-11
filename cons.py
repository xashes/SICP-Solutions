def cons(x, y):
    def dispatch(m):
        if m == 0:
            return x
        else:
            return y
    return dispatch

def car(p):
    return p(0)

def cdr(p):
    return p(1)

def lcons(x, y):
    return lambda f: f(x, y)

def lcar(p):
    return p(lambda x, y: x)

def lcdr(p):
    return p(lambda x, y: y)
