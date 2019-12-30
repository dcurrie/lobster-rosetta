// Robin Hood hashmap for Lobster

// See:
// Pedro Celis, 1986, Robin Hood Hashing, University of Waterloo
// https://cs.uwaterloo.ca/research/tr/1986/CS-86-14.pdf
//
// Also:
// https://www.sebastiansylvan.com/post/robin-hood-hashing-should-be-your-default-hash-table-implementation/
// https://www.sebastiansylvan.com/post/more-on-robin-hood-hashing-2/
// https://probablydance.com/2018/06/16/fibonacci-hashing-the-optimization-that-the-world-forgot-or-a-better-alternative-to-integer-modulo/
// http://codecapsule.com/2013/11/11/robin-hood-hashing/
// http://codecapsule.com/2013/11/17/robin-hood-hashing-backward-shift-deletion/


class RobinHoodEntry<K,V>:
    key: K
    val: V
    psl: int // the probe sequence length, i.e., the distance of this entry from the ideal location
             // if negative, the Entry is empty

class RobinHoodHashmap<K,V>:
    qentries: int
    qbuckets: int
    log2qbuc: int
    the_nil_K: K
    the_nil_V: V
    buckets: [RobinHoodEntry]

def initBuckets<K,V>(qbuckets: int, nK: K, nV: V) -> [RobinHoodEntry<K, V>]:
    let b = vector_reserve(typeof return, qbuckets)
    for (qbuckets):
        b.push(RobinHoodEntry { key: nK, val: nV, psl: -1 })
    return b

def newRobinHoodHashmap<K,V>(log2_initial_size, nK: K, nV: V) -> RobinHoodHashmap<K,V>:
    assert log2_initial_size < 48
    let qbuckets = 1 << log2_initial_size
    assert qbuckets > 0
    return RobinHoodHashmap { qentries: 0,
                              qbuckets: qbuckets,
                              log2qbuc: log2_initial_size,
                              the_nil_K: nK,
                              the_nil_V: nV,
                              buckets:  initBuckets(qbuckets, nK, nV) }

def keyToIndex<K>(m: RobinHoodHashmap, key: K) -> int:
    let h = hash(key) * 11400714819323198485 // fibonacci hashing
    return h & ((1 << m.log2qbuc) - 1)


class RHHM_s_i = RobinHoodHashmap<string,int>
class RHHE_s_i = RobinHoodEntry<string,int>
// with the above I get:
// RobinHoodHashmap.lobster(44): error: "constructor" (6th argument) requires type: [RobinHoodEntry<K, V>], got: [RHHE_s_i]
// without the above I get:
// RobinHoodHashmap.lobster(32): error: no specialization of RobinHoodEntry matches these types: string int int

let ht = newRobinHoodHashmap<string,int>(4, "", 0)
