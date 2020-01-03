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

// TODO: the code assumes type K supports operator `==`

// TODO: Paul Khuong observes that with the appropriate hash&index function, hash values can be
// made to resolve in-order and can be used in place of PSL
// https://www.pvk.ca/Blog/more_numerical_experiments_in_hashing.html

class RobinHoodEntry<K,V>:
    key: K
    val: V
    hsh: int
    psl: int // the probe sequence length, i.e., the distance of this entry from the ideal location
             // if negative, the Entry is empty

class RobinHoodHashmap<K,V>:
    qentries: int
    qbuckets: int
    log2qbuc: int
    the_nil_K: K
    the_nil_V: V
    buckets: [RobinHoodEntry<K,V>]

private let minsize = 16 // entries -- won't shrink below this size
private let minload = 40 // percent -- otherwise, shrink after delete to below this load factor
private let maxload = 85 // percent -- grow before insert if above this load factor

private let validate = false

private def initBuckets<K,V>(qbuckets: int, nK: K, nV: V) -> [RobinHoodEntry<K, V>]:
    let b = vector_reserve(typeof return, qbuckets)
    for (qbuckets):
        b.push(RobinHoodEntry { key: nK, val: nV, hsh: 0, psl: -1 })
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

def len<K,V>(m: RobinHoodHashmap<K,V>) -> int:
    return m.qentries

private def keyToIndex<K,V>(m: RobinHoodHashmap<K,V>, key: K) -> int, int: // hash, index
    //let h = if key is int: key else: if key is float: hash(string(key)) else: hash(key)
    let h = hash(key)
    let f = h * 11400714819323198485 // fibonacci hashing
    return h, f & ((1 << m.log2qbuc) - 1)

private def validate_psl<K,V>(m: RobinHoodHashmap<K,V>, idx: int) -> int:
    let h, i = keyToIndex(m, m.buckets[idx].key)
    let psl = m.buckets[idx].psl
    let dif = if idx - i < 0: m.qbuckets + idx - i else: idx - i
    return psl == dif

private def put_<K,V>(m: RobinHoodHashmap<K,V>, k: K, v: V) -> V:
    let h, i = keyToIndex(m, k)
    // From the paper: "when inserting, if a record probes a location that is already occupied,
    // the record that has traveled longer in its probe sequence keeps the location, and the
    // other one continues on its probe sequence" (page 12).
    // So, if the probe sequence length (PSL) of the element being inserted is greater than PSL
    // of the element in the bucket, then swap them and continue.
    var pk = k
    var pv = v
    var ph = h
    var ps = 0
    var idx = i
    while true:
        if m.buckets[idx].psl < 0: // empty, so put it here!
            m.buckets[idx].key = pk
            m.buckets[idx].val = pv
            m.buckets[idx].hsh = ph
            m.buckets[idx].psl = ps
            m.qentries += 1
            if validate: assert validate_psl(m, idx)
            return v
        if validate: assert validate_psl(m, idx)
        if m.buckets[idx].hsh == h and m.buckets[idx].key == k: // key k already in table, replace
            if validate: assert ph == h and pv == v // we didn't find a rich bucket yet
            pv = m.buckets[idx].val
            m.buckets[idx].val = v
            if validate: assert m.buckets[idx].psl == ps
            return pv // the old value
        if ps > m.buckets[idx].psl: // rich bucket found, swap
            let tk = pk
            let tv = pv
            let th = ph
            let ts = ps
            pk = m.buckets[idx].key
            pv = m.buckets[idx].val
            ph = m.buckets[idx].hsh
            ps = m.buckets[idx].psl
            m.buckets[idx].key = tk
            m.buckets[idx].val = tv
            m.buckets[idx].hsh = th
            m.buckets[idx].psl = ts
            // continue looking
        ps += 1
        idx += 1
        if idx >= m.qbuckets: idx = 0
    return v // never happens, but compiler typechecker wants it

private def resize<K,V>(m: RobinHoodHashmap<K,V>, log2_new_size: int):
    let oldsize = m.qbuckets
    let oldents = m.qentries
    let oldbuckets = m.buckets
    if validate: assert oldsize == oldbuckets.length
    let newsize = 1 << log2_new_size
    assert newsize > 0
    m.buckets = initBuckets(newsize, m.the_nil_K, m.the_nil_V)
    m.qbuckets = newsize
    m.log2qbuc = log2_new_size
    m.qentries = 0
    for(oldsize) i:
        if oldbuckets[i].psl >= 0:
            m.put_(oldbuckets[i].key, oldbuckets[i].val)
    if validate: assert m.qentries == oldents

def put<K,V>(m: RobinHoodHashmap<K,V>, k: K, v: V) -> V:
    // qe/qb > 85/100 == 100 * qe > 85 * qb
    if 100 * m.qentries > maxload * m.qbuckets:
        if validate: print concat_string(["Resizing up: ", string(m.qentries), "/", string(m.qbuckets)], "")
        m.resize(m.log2qbuc + 1)
        if validate: print concat_string(["Resized up:  ", string(m.qentries), "/", string(m.qbuckets)], "")
    return m.put_(k, v)

def get<K,V>(m: RobinHoodHashmap<K,V>, k: K) -> V:
    let h, i = keyToIndex(m, k)
    var ps = 0
    var idx = i
    while true:
        if m.buckets[idx].psl < ps: // empty or rich
            // Stop probing if we hit an empty bucket, or if we hit a bucket with PSL lower than
            // the distance from the base location, then it means that we found the rich bucket
            // which should have been captured, if the key was inserted
            // -- see the central point of the algorithm in the insertion function.
            return m.the_nil_V
        if validate: assert validate_psl(m, idx)
        if m.buckets[idx].hsh == h and m.buckets[idx].key == k:
            return m.buckets[idx].val
        ps += 1
        idx += 1
        if idx >= m.qbuckets: idx = 0
    return m.the_nil_V // never happens, but compiler typechecker wants it

def del<K,V>(m: RobinHoodHashmap<K,V>, k: K) -> V:
    let h, i = keyToIndex(m, k)
    var ps = 0
    var result = m.the_nil_V
    var idx = i
    var probing = true
    while probing:
        if m.buckets[idx].psl < ps: // empty or rich
            return m.the_nil_V
        if validate: assert validate_psl(m, idx)
        if m.buckets[idx].hsh == h and m.buckets[idx].key == k:
            result = m.buckets[idx].val
            probing = false
        else:
            ps += 1
            idx += 1
            if idx >= m.qbuckets: idx = 0
    // result found at idx
    m.qentries -= 1
    // shift following entries backward to maintain variance and avoid need for tombstones
    probing = true
    while probing:
        m.buckets[idx].psl = -1 // empty
        m.buckets[idx].key = m.the_nil_K // for gc
        m.buckets[idx].val = m.the_nil_V // for gc
        var nidx = idx + 1
        if nidx >= m.qbuckets: nidx = 0
        if m.buckets[nidx].psl <= 0: // -1: free, or 0: in its base location
            probing = false
        else:
            m.buckets[idx].key = m.buckets[nidx].key
            m.buckets[idx].val = m.buckets[nidx].val
            m.buckets[idx].hsh = m.buckets[nidx].hsh
            m.buckets[idx].psl = m.buckets[nidx].psl - 1
            idx = nidx
            if validate: assert validate_psl(m, idx)
    // see if we need to shrink // qe/qb < 40/100 == 100 * qe > 40 * qb
    if m.qbuckets > minsize and 100 * m.qentries < minload * m.qbuckets:
        if validate: print concat_string(["Resizing dn: ", string(m.qentries), "/", string(m.qbuckets)], "")
        m.resize(m.log2qbuc - 1)
        if validate: print concat_string(["Resized dn:  ", string(m.qentries), "/", string(m.qbuckets)], "")
    return result

def for_each_ht<K,V>(m: RobinHoodHashmap<K,V>, fun):
    for(m.qbuckets) idx:
        if m.buckets[idx].psl >= 0:
            fun(m.buckets[idx].key, m.buckets[idx].val)

// for testing...
def for_each_ht_psl<K,V>(m: RobinHoodHashmap<K,V>, fun):
    for(m.qbuckets) idx:
        if m.buckets[idx].psl >= 0:
            fun(m.buckets[idx].key, m.buckets[idx].psl)
