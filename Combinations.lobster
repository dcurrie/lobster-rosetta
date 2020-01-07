import std

// combi is an itertor that solves the Combinations problem for iota arrays as stated
// It is based on the Nim version http://rosettacode.org/wiki/Combinations#Nim

def combi(m, n, f):
    let c = map(n): _

    while true:
        f(c)
        var i = n-1
        c[i] = c[i] + 1
        if c[i] > m - 1:
            while c[i] >= m - n + i:
                i -= 1
                if i < 0: return
            c[i] = c[i] + 1
            while i < n-1:
                c[i+1] = c[i] + 1
                i += 1

combi(5, 3): print(_)

// comba solves the general problem for any values in an input array
// It is based on the JavaScript version http://rosettacode.org/wiki/Combinations#JavaScript
// which is based on the Python version, based on the Haskell version.

def comba<T>(arr: [T], k) -> [[T]]:
    let ret = []
    for(arr.length) i:
        if k == 1:
            ret.push([arr[i]])
        else:
            let sub = comba(arr.slice(i+1, -1), k-1)
            for(sub) next:
                next.insert(0, arr[i])
                ret.push(next)
    return ret

print comba([0,1,2,3,4], 3)
print comba(["Crosby", "Stills", "Nash", "Young"], 3)
// Of course once could use combi to index the input array instead
var s = ""
combi(4, 3): s += (map(_) i: ["Crosby", "Stills", "Nash", "Young"][i]) + " "
print s