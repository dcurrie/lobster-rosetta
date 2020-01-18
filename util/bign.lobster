
import std

// Very basic arbitrary-precision integers
// - only non-negative numbers

let base = 1000000000

class Bign:
    digits: [int] // little endian, of base base
    digitsUsed: int

    def clone():
        return Bign { digits: copy(digits), digitsUsed: digitsUsed }

    def resize(newLength):
        while digits.length < newLength:
            digits.push(0)

    def adjustDigitsUsed():
        while digitsUsed > 0 and digits[digitsUsed - 1] == 0:
                digitsUsed -= 1

    // multiplication by one digit; used to convert string to Bign
    def muldigit(multiplier : int):
        if (multiplier < 0):
            return // "Signed arithmetic isn't supported"
        resize(digitsUsed + 1)
        var temp = 0
        for(digitsUsed) i:
            temp += digits[i] * multiplier
            digits[i] = temp % base
            temp /= base
        digits[digitsUsed] = temp
        digitsUsed += 1
        adjustDigitsUsed()

    // addition of one digit; used to convert string to Bign
    def adddigit(addend: int):
        if (addend < 0):
            return // "Signed arithmetic isn't supported"
        var temp = addend
        var i = 0
        while i < digitsUsed and temp != 0:
            temp += digits[i]
            digits[i] = temp % base
            temp /= base
            i += 1
        if temp != 0:
            resize(digitsUsed + 1)
            digits[digitsUsed] = temp
            digitsUsed += 1

    // division by one digit
    def divdigit(divisor: int) -> int: // returns remainder
        if (divisor < 0):
            return // "Signed arithmetic isn't supported"
        var remainder = 0
        var i = digitsUsed - 1
        while i >= 0:
            let twoDigits = remainder * base + digits[i]
            remainder = twoDigits % divisor
            digits[i] = twoDigits / divisor
            i -= 1
        adjustDigitsUsed()
        return remainder

    def bign2str():
        var i = digitsUsed
        if i == 0:
            return "0"
        i -= 1
        var s = string(digits[i])
        while i > 0:
            i -= 1
            s += number_to_string(digits[i], 10, 9)
        return s

def str2bign(value):
    // each of our Bign digits can store 9 decimal digits
    let this = Bign { digits: map(value.length() / 9 + 1): 0, digitsUsed: 0 }
    // handle nine digits at a time
    var i = 0
    while i < value.length:
        var multiplier = 1
        var addend = 0
        for(min(9, value.length() - i)) j:
            let c = value[i+j]
            //if (c < '0' or c > '9') -- what!?
            multiplier *= 10
            addend *= 10
            addend += c - '0'
        this.muldigit(multiplier)
        this.adddigit(addend)
        i += 9
    return this

// Long addition

def bign_add(this, addend):
    // Make sure the shorter number is on the right side to make things a bit more efficient
    let left  = if (this.digitsUsed > addend.digitsUsed): this else: addend
    let right = if (this.digitsUsed > addend.digitsUsed): addend else: this
    let newDigits = map(left.digitsUsed): 0
    var temp = 0
    var i = 0
    while i < right.digitsUsed:
        temp += left.digits[i] + right.digits[i]
        newDigits[i] = temp % base
        temp /= base
        i += 1
    while i < left.digitsUsed:
        temp += left.digits[i]
        newDigits[i] = temp % base
        temp /= base
        i += 1
    if temp != 0:
        newDigits.push(temp)
    let bign = Bign { digits: newDigits, digitsUsed: newDigits.length }
    bign.adjustDigitsUsed()
    return bign

// Long subtraction

def bign_subtract(this, subtrahend) -> int, Bign: // overflow, result
    var borrow = 0
    var i = 0
    if (this.digitsUsed < subtrahend.digitsUsed):
        // overflow; compute 9s complement anyway
        let newDigits = map(subtrahend.digitsUsed): 0
        while i < this.digitsUsed:
            let diff = this.digits[i] - subtrahend.digits[i] - borrow + base
            newDigits[i] = diff % base
            borrow = 1 - diff / base
            i += 1
        while i < subtrahend.digitsUsed:
            let diff = base - subtrahend.digits[i] - borrow
            newDigits[i] = diff % base
            borrow = 1 - diff / base
            i += 1
        let bign = Bign { digits: newDigits, digitsUsed: i }
        bign.adjustDigitsUsed()
        assert borrow != 0
        return borrow, bign
    let newDigits = map(this.digitsUsed): 0
    while i < subtrahend.digitsUsed:
        let diff = this.digits[i] - subtrahend.digits[i] - borrow + base
        newDigits[i] = diff % base
        borrow = 1 - diff / base
        i += 1
    while i < this.digitsUsed:
        let diff = this.digits[i] - borrow + base
        newDigits[i] = diff % base
        borrow = 1 - diff / base
        i += 1
    let bign = Bign { digits: newDigits, digitsUsed: i }
    bign.adjustDigitsUsed()
    return borrow, bign

// Long multiplication

def bign_multiply(this, multiplier):
    // Make sure the shorter number is on the right side to make things a bit more efficient
    let left  = if (this.digitsUsed > multiplier.digitsUsed): this else: multiplier
    let right = if (this.digitsUsed > multiplier.digitsUsed): multiplier else: this
    let newDigits = map(left.digitsUsed + right.digitsUsed): 0
    for(right.digitsUsed) rightPos:
        let rightDigit = right.digits[rightPos]
        var temp = 0
        for(left.digitsUsed) leftPos:
            temp += newDigits[leftPos + rightPos]
            temp += rightDigit * left.digits[leftPos]
            newDigits[leftPos + rightPos] = temp % base
            temp /= base
        // Roll forward any carry we may have
        let destPos = rightPos + left.digitsUsed
        while temp != 0:
            temp += newDigits[destPos]
            newDigits[destPos] = temp % base
            temp /= base
            destPos +- 1
    let bign = Bign { digits: newDigits, digitsUsed: newDigits.length }
    bign.adjustDigitsUsed()
    return bign

// <0, 0, >0 lt, eq, gt

def bign_compare(x, y):
    var result = x.digitsUsed - y.digitsUsed
    var i = x.digitsUsed
    while i > 0 and result == 0:
        i -= 1
        result = x.digits[i] - y.digits[i]
    return result

// Knuth's Algorithm D, for base 1000000000

// var corrections = 0 // used for validation of tet code coverage; force by reducing base to 1000, or 10

def bign_divide(u, v) -> Bign, Bign: // q, r
    let m = u.digitsUsed
    let n = v.digitsUsed

    if m < n or n <= 0:
        return Bign { digits: [0], digitsUsed: 0 }, u.clone() // q = 0, r = u

    let q = Bign { digits: map(m - n + 1): 0, digitsUsed: 0 }

    if n == 1:
        // single-digit divisor
        var k = 0
        var j = m - 1
        q.digitsUsed = m
        let w = v.digits[0]
        while j >= 0:
            let d = k * base + u.digits[j]
            q.digits[j] = d / w
            k = d - q.digits[j] * w
            j -= 1
        q.adjustDigitsUsed()
        let r = Bign { digits: [k], digitsUsed: k != 0 }
        return q, r

    // Normalize
    let vn = v.clone()
    let un = u.clone()
    let scale = base / (v.digits[n-1] + 1)
    vn.muldigit(scale)
    un.muldigit(scale)

    // Main loop
    var j = m - n
    q.digitsUsed = j + 1
    while j >= 0:
        // Compute estimate qhat of q[j]
        let d = un.digits[j+n] * base + un.digits[j+n-1]
        var qhat = d / vn.digits[n-1]
        var rhat = d - qhat * vn.digits[n-1]
        var trying = true
        while trying:
            if qhat >= base or qhat * vn.digits[n-2] > base * rhat + un.digits[j+n-2]:
                qhat -= 1
                rhat += vn.digits[n-1]
                trying = rhat < base
            else:
                trying = false
        // Multiply and subtract
        var k = 0
        for(n) i:
            let p = qhat * vn.digits[i]
            let t = un.digits[i+j] - k - (p % base) + base + base // add base twice to avoid negative t
            un.digits[i+j] = t % base
            k = p / base + 2 - t / base // the 2 removes the "+ base + base" above
        k = un.digits[n+j] - k
        un.digits[n+j] = k
        q.digits[j] = qhat
        if k < 0:
            // corrections += 1
            q.digits[j] = qhat - 1
            k = 0
            for(n) i:
                let t = un.digits[i+j] + vn.digits[i] + k
                un.digits[i+j] = t % base
                k = t / base
            un.digits[j+n] = un.digits[j+n] + k
        j -= 1
    un.divdigit(scale)
    q.adjustDigitsUsed()
    return q, un


// let one = str2bign("18446744073709551616")
// let two = one.clone()
// var pro = one.bign_multiply(two)
// print(bign2str(pro))
// pro = pro.bign_multiply(pro)
// print(bign2str(pro))
// var q, r = pro.bign_divide(two)
// print(bign2str(q))
// print q
// print(bign2str(r))
// print r
// print ""
// q, r = one.bign_divide(one)
// print one
// print q
// print r
