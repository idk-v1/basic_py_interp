print "primes from "
input i
print "to "
input max
while i <= max:
    if i == 2:
        print i
        print " "
    elif i > 2:
        ii = 2
        found = False
        sq = sqrt i
        while ii <= sq and not found:
            if i % ii == 0:
                found = True
            else:
                ii += 1
        if not found:
            print i
            print " "
    i += 1
println ""
