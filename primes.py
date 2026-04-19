print "primes from "
input i
print "to "
input max
if i <= 2 and 2 <= max:
	print 2
	print " "
if i < 3:
	i = 3
while i <= max:
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
	i += 2
println ""
