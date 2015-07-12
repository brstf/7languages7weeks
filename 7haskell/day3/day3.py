#!/usr/bin/python
# Write a monad in a non-functional language

class ListMonad(object):
    # List Return
    # in Haskell: return x = [x]
    @staticmethod
    def listReturn(x):
        return [x]

    # List Bind
    # in Haskell: m >>= f = concatMap f m
    @staticmethod
    def listBind(f, l):
        concatenated = []
        for el in map(f, l):
            concatenated += el
        return concatenated


if __name__ == "__main__":
    # Law One: 
    print "Law 1: return x >>= f is the same as f x"
    r1 = ListMonad.listBind(lambda x: [x + 1], ListMonad.listReturn(12))
    r2 = (lambda x: [x + 1])(12)
    print r1, "==?", r2, r1 == r2

    # Law Two:
    print "Law 2: m >>= return is just m"
    print ListMonad.listBind(lambda x: ListMonad.listReturn(x),[[1,2],[3,4]])

    # Law Three:
    print "Law 3: (m >>= f) >>= g is the same as m >>= (\\x -> f x >>= g)"
    f = lambda x: [x+1]
    g = lambda x: [x-2]
    print ListMonad.listBind(g, ListMonad.listBind(f, [1,2,3,4]))
    print ListMonad.listBind(lambda x: ListMonad.listBind(g, f(x)),[1,2,3,4])