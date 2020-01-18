import std
import testing
import bign

// Knuth divide is about 20% faster than Hansen (15.7s vs. 19.8s for 200000 TRIALs of 12 digit bigns)


// xorshift64*
let xorshift64star_state = [97531, 987654321] /* The state must be seeded with a nonzero value. */

// i is the index of the random state so we can have multiple independent streams
//
def xorshift64star_next(i:int):
    var x = xorshift64star_state[i]
    x = x ^ (x >> 12) // a
    x = x ^ (x << 25) // b
    x = x ^ (x >> 27) // c
    xorshift64star_state[i] = x
    return x * 2685821657736338717

def xorshift64star_rand(i: int, n: int):
    let x = xorshift64star_next(i)
    return abs(x) % n


def bign_rand_len(len):
    let d = map(len): xorshift64star_rand(0, base)
    let b = Bign{ digits: d, digitsUsed: len }
    b.adjustDigitsUsed()
    return b

let MAXBIGNLEN = 4

def bign_rand():
    return bign_rand_len(2 + xorshift64star_rand(1, MAXBIGNLEN))

let bign_zero = Bign { digits: [0], digitsUsed: 0 }

run_test("Bign div no r"):
    let TRIALS = 2000
    for(TRIALS):
        let x = bign_rand()
        let y = bign_rand()
        let p = bign_multiply(x, y)
        var q, r = bign_divide(p, y)
        assert 0 == bign_compare(x, q) or 0 == bign_compare(y, bign_zero)
        assert 0 == bign_compare(r, bign_zero)
        //print "x: " + bign2str(x)
        //print "y: " + bign2str(y)
        //print "p: " + bign2str(p)
        //print "q: " + bign2str(q)
        //print "r: " + bign2str(r)
        q, r = bign_divide(p, x)
        //print "q: " + bign2str(q)
        //print "r: " + bign2str(r)
        assert 0 == bign_compare(y, q) or 0 == bign_compare(x, bign_zero)
        assert 0 == bign_compare(r, bign_zero)

run_test("Bign div with r"):
    let TRIALS = 3000
    for(TRIALS) i:
        let x = bign_rand()
        let y = bign_rand()
        let z = bign_rand()
        let p = bign_multiply(x, y)
        let q, r = bign_divide(p, z)
        let s = bign_add(r, bign_multiply(q, z))
        let c = bign_compare(s, p)
        if c != 0:
            print "x: " + bign2str(x)
            print "y: " + bign2str(y)
            print "z: " + bign2str(z)
            print "p: " + bign2str(p)
            print "q: " + bign2str(q)
            print "r: " + bign2str(r)
            print "s: " + bign2str(s)
        assert 0 == bign_compare(s, p) or 0 == bign_compare(z, bign_zero)
    // print "corrections: " + corrections

run_test("Bign sub"):
    let TRIALS = 300000
    for(TRIALS) i:
        let x = bign_rand()
        let y = bign_rand()
        let p = bign_multiply(x, y)
        let pb, pd = bign_subtract(p,x)
        assert 0 == pb
        assert 0 == bign_compare(p, bign_add(x, pd))
        if bign_compare(x, y) < 0:
            let yb, yd = bign_subtract(y, x)
            assert 0 == yb
            assert 0 == bign_compare(y, bign_add(x, yd))
            let xb, xd = bign_subtract(x, y)
            assert 0 != xb
            //print "b: " + bign2str(x) + " - " + bign2str(y) + " = " + bign2str(xd)
            let zb, zd = bign_subtract(x, xd)
            //print "z: " + bign2str(x) + " - " + bign2str(xd) + " = " + bign2str(zd)
            assert 0 != zb
            assert 0 == bign_compare(y, zd)
        else:
            let yb, yd = bign_subtract(y, x)
            assert 0 != yb
            let zb, zd = bign_subtract(y, yd)
            assert 0 != zb
            assert 0 == bign_compare(x, zd)
            let xb, xd = bign_subtract(x, y)
            assert 0 == xb
            assert 0 == bign_compare(x, bign_add(y, xd))

