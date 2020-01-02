
import testing
import RobinHoodHashmap

class RHHM_s_i = RobinHoodHashmap<string,int>
class RHHE_s_i = RobinHoodEntry<string,int>

let ht = newRobinHoodHashmap<string,int>(3, "", 0)

assert 1 == ht.put("one", 1)
assert 2 == ht.put("two", 2)
assert 6 == ht.put("six", 6)
assert 10 == ht.put("ten", 10)
assert ht.qentries == 4
assert ht.qbuckets == 8
assert 1 == ht.put("one", -1)
assert 2 == ht.put("two", -2)
assert ht.qentries == 4
assert ht.qbuckets == 8
assert 3 == ht.put("three", 3)
assert 4 == ht.put("four", 4)
assert 5 == ht.put("five", 5)
assert 7 == ht.put("seven", 7)
assert 8 == ht.put("eight", 8)
assert 9 == ht.put("nine", 9)
assert ht.qentries == 10
assert ht.qbuckets == 16
assert -1 == ht.get("one")
assert -2 == ht.get("two")
assert  3 == ht.get("three")
assert  4 == ht.get("four")
assert  5 == ht.get("five")
assert  6 == ht.get("six")
assert  7 == ht.get("seven")
assert  8 == ht.get("eight")
assert  9 == ht.get("nine")
assert  0 == ht.get("nonesuch key")
assert 10 == ht.get("ten")
assert ht.qentries == 10
assert ht.qbuckets == 16



// random stress test of RobinHoodHashmap

let STRESS_TEST_N =  5000
let BB_ITERATIONS = 20000 // 10000 ok
let MAXINT = 9223372036854775807
let MARKER = (-MAXINT) - 1

let chs = "0123456789_/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

assert 64 == chs.length

var qinrhht = 0
var q_ins = 0
var q_del = 0
var q_get = 0

let inrhht = vector_reserve(typeof return, STRESS_TEST_N)

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

def init_random_rhht():
    for (STRESS_TEST_N):
        inrhht.push(MARKER)
    qinrhht = 0
    q_ins = 0
    q_del = 0
    q_get = 0

def make_random_key_val(k: int): // -> string, int
    assert(k > 0)
    //xorshift64star_state[0] = k
    //let size = xorshift64star_rand(0, 16) + 8
    //let svec = vector_reserve(typeof return, size)
    //for(size):
    //    let idx = xorshift64star_rand(0, 64)
    //    svec.push(chs.substring(idx, 1))
    //return svec.concat_string(""), -k
    return number_to_string(k,36,1), -k

let rhht = newRobinHoodHashmap<string,int>(5, "", 0)

def tstput(k: int):
    let key, val = make_random_key_val(k)
    rhht.put(key, val)

def tstget(k: int) -> int:
    let key, val = make_random_key_val(k)
    //discard val
    return rhht.get(key)

def tstrem(k: int):
    let key, val = make_random_key_val(k)
    //discard val
    rhht.del(key)

def do_random_rhht_op():
    let x = xorshift64star_rand(1, STRESS_TEST_N)
    let n = inrhht[x]
    if n == MARKER:
        // insert
        let k = 1 + xorshift64star_rand(1, MAXINT-1)
        //print("x " + x + " k " + k)
        tstput(k)
        inrhht[x] = k
        qinrhht += 1
        q_ins += 1
    else:
        // delete
        tstrem(n)
        inrhht[x] = MARKER
        qinrhht -= 1
        q_del += 1

def find_and_wipe_duplicate() -> int:
    for(STRESS_TEST_N) i:
        let n = inrhht[i]
        if n != MARKER:
            // in std: for_range(i + 1, STRESS_TEST_N) j:
            var j = i + 1
            while j < STRESS_TEST_N:
                if n == inrhht[j]:
                    // duplicate
                    inrhht[j] = MARKER
                    qinrhht -= 1
                    q_ins -= 1
                    return j // > 0
                j += 1
    return 0

def innerlp() -> int:
    //print(rhht)
    //print concat_string([string(q_ins), "-", string(q_del)], "")
    //for_each_ht(rhht): print concat_string([string(_a), "/", string(_b)], "")
    for(STRESS_TEST_N) j:
        let n = inrhht[j]
        if n != MARKER:
            let v = tstget(n)
            q_get += 1
            assert(v <= 0)
            //if v != -n:
            //    print concat_string([string(v), "≉", string(-n)], "")
            //    return true // break
            assert(v == -n)
            if v == 1:
                return true // break
    return false

def outerlp():
    var stop = false
    // do():
    for(BB_ITERATIONS) i:
        if stop: return true
        do_random_rhht_op()
        if innerlp(): return true
        //if innerlp(): return true from do
        let sz = rhht.len
        if sz != q_ins - q_del:
            let dup = find_and_wipe_duplicate()
            if dup != 0:
                // whew!
                print "dup!"
        assert(sz == (q_ins - q_del))
        if stop or (sz != (q_ins - q_del)):
            //fprintf(stderr, "random err: %u nodes, %ld final size\n", sz, q_ins - q_del);
            assert(not stop)
            stop = true
        //if ((i+1) % 1024) == 0:
            //write(stderr, ".")

def for2(xs, ys, fun): return for xs.length: fun(xs[_], ys[_])

run_test("timed random rhht iterations"):
    let start = seconds_elapsed()
    //write(stderr, "        ")
    init_random_rhht()
    outerlp()
    let finish = seconds_elapsed()
    let q_ops = q_get + q_ins - q_del
    print("")
    print("  CPU time " + (finish - start)
          + " s for " + BB_ITERATIONS + " loops, averaging "
          + ((finish - start) / BB_ITERATIONS) + " s per loop")
    print("  CPU time " + (finish - start)
          + " s for " + q_ops + " ops, averaging "
          + (1000 * (finish - start) / q_ops) + " ms per op")
    print("  random q: " + q_ins + " inserts, " + q_del + " deletes, "
          + qinrhht + " qinrhht, " + (q_ins - q_del) + " final size")
