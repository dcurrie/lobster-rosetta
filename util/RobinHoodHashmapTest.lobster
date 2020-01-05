
import std
import testing
import RobinHoodHashmap

class RHHM_s_i = RobinHoodHashmap<string,int>
class RHHE_s_i = RobinHoodEntry<string,int>

class RHHM_i_i = RobinHoodHashmap<int,int>
class RHHE_i_i = RobinHoodEntry<int,int>

class RHHM_i_s = RobinHoodHashmap<int,string>
class RHHE_i_s = RobinHoodEntry<int,string>

class RHHM_f_s = RobinHoodHashmap<float,string>
class RHHE_f_s = RobinHoodEntry<float,string>

let MAXINT = 9223372036854775807
let MARKER = (-MAXINT) - 1

run_test("Basic hashmap<string,int> operation"):
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

run_test("Basic hashmap<int,int> operation"):
    let ht = newRobinHoodHashmap<int,int>(3, 0, MARKER)
    //assert hash(1) is int
    assert 101 == ht.put(1, 101)
    assert 102 == ht.put(2, 102)
    assert 106 == ht.put(6, 106)
    assert 110 == ht.put(10, 110)
    assert ht.qentries == 4
    assert ht.qbuckets == 8
    assert 101 == ht.put(1, 201)
    assert 102 == ht.put(2, 202)
    assert ht.qentries == 4
    assert ht.qbuckets == 8
    assert 201 == ht.get(1)
    assert 202 == ht.get(2)
    assert 106 == ht.get(6)
    assert MARKER == ht.get(45)
    assert 110 == ht.get(10)
    assert ht.qentries == 4
    assert ht.qbuckets == 8

run_test("Basic hashmap<int,string> operation"):
    let ht = newRobinHoodHashmap<int,string>(3, 0, "")
    //assert hash(1) is int
    assert "one" == ht.put(1, "one")
    assert "two" == ht.put(2, "two")
    assert "six" == ht.put(6, "six")
    assert "ten" == ht.put(10, "ten")
    assert ht.qentries == 4
    assert ht.qbuckets == 8
    assert "one" == ht.put(1, "One")
    assert "two" == ht.put(2, "Two")
    assert ht.qentries == 4
    assert ht.qbuckets == 8
    assert "One" == ht.get(1)
    assert "Two" == ht.get(2)
    assert "six" == ht.get(6)
    assert  ""   == ht.get(45)
    assert "ten" == ht.get(10)
    assert ht.qentries == 4
    assert ht.qbuckets == 8

run_test("Basic hashmap<float,string> operation"):
    let ht = newRobinHoodHashmap<float,string>(3, -0.0, "")
    //assert hash(1.0) is int
    assert "one" == ht.put(1.0, "one")
    assert "two" == ht.put(2.0, "two")
    assert "six" == ht.put(6.0, "six")
    assert "ten" == ht.put(10.0, "ten")
    assert ht.qentries == 4
    assert ht.qbuckets == 8
    assert "one" == ht.put(1.0, "One")
    assert "two" == ht.put(2.0, "Two")
    assert ht.qentries == 4
    assert ht.qbuckets == 8
    assert "One" == ht.get(1.0)
    assert "Two" == ht.get(2.0)
    assert "six" == ht.get(6.0)
    assert  ""   == ht.get(45.0)
    assert "ten" == ht.get(10.0)
    assert ht.qentries == 4
    assert ht.qbuckets == 8


// random stress test of RobinHoodHashmap

// utility classes and functions...

def for2(xs, ys, fun): return for xs.length: fun(xs[_], ys[_])

class Stats:
    M = 0.0
    S = 0.0
    n = 0
    bins = [0,0,0,0,0,0,0,0,0,0]
    def incl(x):
        n += 1
        if n == 1:
            M = x
        else:
            let mm = (x - M)
            M += mm / n
            S += mm * (x - M)
        if x >= 0 and x < bins.length:
            bins[x] += 1
        else:
            bins[bins.length-1] += 1
    def mean(): return M
    //def variance(): return (if n > 1.0: S / (n - 1.0) else: 0.0) // Bessel's correction
    def variance(): return (if n > 0.0: S / n else: 0.0)
    def stddev(): return sqrt(variance())
    def count(): return n
    def makebins(q): bins = map(q): 0

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

// the ht test configuration and data model

var STRESS_TEST_N = 1000 // 10000 ok
var BB_ITERATIONS = 2000 // 20000 ok

var qinrhht = 0
var q_ins = 0
var q_del = 0
var q_get = 0
var inrhht = [0]
var rhht = newRobinHoodHashmap<string,int>(5, "", 0)

def init_random_rhht():
    qinrhht = 0
    q_ins = 0
    q_del = 0
    q_get = 0
    inrhht = map(STRESS_TEST_N): MARKER
    rhht = newRobinHoodHashmap<string,int>(5, "", 0)

def make_random_key_val(k: int): // -> string, int
    assert(k > 0)
    return number_to_string(k,36,1), -k

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

run_test("timed random checked rhht iterations"):
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
    let start = seconds_elapsed()
    //write(stderr, "        ")
    init_random_rhht()
    outerlp()
    let finish = seconds_elapsed()
    let q_ops = q_get + q_ins + q_del
    let s = Stats {}
    s.makebins(20)
    for_each_ht_psl(rhht): _a; s.incl(_b)
    print("")
    print("  CPU time " + (finish - start)
          + " s for " + BB_ITERATIONS + " loops, averaging "
          + (1000 * (finish - start) / BB_ITERATIONS) + " ms per loop")
    print("  CPU time " + (finish - start)
          + " s for " + q_ops + " ops, averaging "
          + (1000000 * (finish - start) / q_ops) + " µs per op")
    print("  random q: " + q_ins + " inserts, " + q_del + " deletes, "
          + qinrhht + " qinrhht, " + (q_ins - q_del) + " final size")
    print("  Probe sequence length " + s.mean + " average, "
          + s.variance + " variance,\n    bins: " + s.bins)
    // STRESS_TEST_N = 10000
    // BB_ITERATIONS = 20000
    //  CPU time 105.275488 s for 20000 loops, averaging 5.2637744 ms per loop
    //  CPU time 105.275488 s for 75566686 ops, averaging 1.393146779173 µs per op
    //  random q: 12452 inserts, 7548 deletes, 4904 qinrhht, 4904 final size
    //  Probe sequence length 0.69310766721 average, 0.98881057079 variance,
    //    bins: [2742, 1351, 539, 172, 66, 23, 5, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

run_test("timed random rhht iterations"):
    STRESS_TEST_N = 100000
    BB_ITERATIONS = 400000
    init_random_rhht()
    let start = seconds_elapsed()
    for(BB_ITERATIONS) i:
        do_random_rhht_op()
        let sz = rhht.len
        if sz != q_ins - q_del:
            let dup = find_and_wipe_duplicate()
            if dup != 0:
                // whew!
                print "dup!"
        assert(sz == (q_ins - q_del))
    for(STRESS_TEST_N) j:
        let n = inrhht[j]
        if n != MARKER:
            let v = tstget(n)
            q_get += 1
            assert(v <= 0)
            assert(v == -n)
    let finish = seconds_elapsed()
    let q_ops = q_get + q_ins + q_del
    let s = Stats {}
    s.makebins(20)
    for_each_ht_psl(rhht): _a; s.incl(_b)
    print("")
    print("  CPU time " + (finish - start)
          + " s for " + BB_ITERATIONS + " loops, averaging "
          + (1000 * (finish - start) / BB_ITERATIONS) + " ms per loop")
    print("  CPU time " + (finish - start)
          + " s for " + q_ops + " ops, averaging "
          + (1000000 * (finish - start) / q_ops) + " µs per op")
    print("  random q: " + q_ins + " inserts, " + q_del + " deletes, "
          + qinrhht + " qinrhht, " + (q_ins - q_del) + " final size")
    print("  Probe sequence length " + s.mean + " average, "
          + s.variance + " variance,\n    bins: " + s.bins)
    // STRESS_TEST_N = 100000
    // BB_ITERATIONS = 200000
    //  CPU time 1.225349 s for 200000 loops, averaging 0.006126745 ms per loop
    //  CPU time 1.225349 s for 249056 ops, averaging 4.919973821149 µs per op
    //  random q: 124528 inserts, 75472 deletes, 49056 qinrhht, 49056 final size
    //  Probe sequence length 1.491356816699 average, 3.369747539348 variance,
    //    bins: [18282, 12691, 7709, 4561, 2497, 1375, 788, 461, 289, 170, 88, 54, 31, 24, 9, 11, 13, 3, 0, 0]
    // STRESS_TEST_N = 100000
    // BB_ITERATIONS = 400000
    //  CPU time 2.56038 s for 400000 loops, averaging 0.00640095 ms per loop
    //  CPU time 2.56038 s for 450020 ops, averaging 5.689480467535 µs per op
    //  random q: 225010 inserts, 174990 deletes, 50020 qinrhht, 50020 final size
    //  Probe sequence length 1.542522990804 average, 3.521842335037 variance,
    //    bins: [18085, 12952, 8016, 4556, 2736, 1575, 898, 499, 274, 166, 98, 58, 31, 18, 22, 11, 8, 12, 5, 0]

