_00 = 0
_01 = 0
_02 = 0
_10 = 0
_11 = 0
_12 = 0
_20 = 0
_21 = 0
_22 = 0

count = 0
won = False
playerTurn = True

while count <= 9:
	if _00 == 0:
		print " |"
	elif _00 == 1:
		print "x|"
	else:
		print "o|"
	if _01 == 0:
		print " |"
	elif _01 == 1:
		print "x|"
	else:
		print "o|"
	if _02 == 0:
		print " \n"
	elif _02 == 1:
		print "x\n"
	else:
		print "o\n"

	if _10 == 0:
		print " |"
	elif _10 == 1:
		print "x|"
	else:
		print "o|"
	if _11 == 0:
		print " |"
	elif _11 == 1:
		print "x|"
	else:
		print "o|"
	if _12 == 0:
		print " \n"
	elif _12 == 1:
		print "x\n"
	else:
		print "o\n"

	if _20 == 0:
		print " |"
	elif _20 == 1:
		print "x|"
	else:
		print "o|"
	if _21 == 0:
		print " |"
	elif _21 == 1:
		print "x|"
	else:
		print "o|"
	if _22 == 0:
		print " \n"
	elif _22 == 1:
		print "x\n"
	else:
		print "o\n"

	if won:
		if playerTurn:
			println "you lose!"
		else:
			println "you win!"
		count = 10
	elif count < 9:
		if playerTurn:
			print "x:"
			input x
			print "y:"
			input y
			found = False
			if x == 0 and y == 0:
				if _00 == 0:
					_00 = 1
					found = True
			elif x == 1 and y == 0:
				if _01 == 0:
					_01 = 1
					found = True
			elif x == 2 and y == 0:
				if _02 == 0:
					_02 = 1
					found = True
			elif x == 0 and y == 1:
				if _10 == 0:
					_10 = 1
					found = True
			elif x == 1 and y == 1:
				if _11 == 0:
					_11 = 1
					found = True
			elif x == 2 and y == 1:
				if _12 == 0:
					_12 = 1
					found = True
			elif x == 0 and y == 2:
				if _20 == 0:
					_20 = 1
					found = True
			elif x == 1 and y == 2:
				if _21 == 0:
					_21 = 1
					found = True
			elif x == 2 and y == 2:
				if _22 == 0:
					_22 = 1
					found = True
			if found:
				count += 1
				playerTurn = False
			else:
				println "Try again"
		else:
			if _00 + _01 + _02 == -2:
				if _00 == 0:
					_00 = -1
					println "x:0\ny:0"
				elif _01 == 0:
					_01 = -1
					println "x:1\ny:0"
				else:
					_02 = -1
					println "x:2\ny:0"
			elif _10 + _11 + _12 == -2:
				if _10 == 0:
					_10 = -1
					println "x:0\ny:1"
				elif _11 == 0:
					_11 = -1
					println "x:1\ny:1"
				else:
					_12 = -1
					println "x:2\ny:1"
			elif _20 + _21 + _22 == -2:  
				if _20 == 0:
					_20 = -1
					println "x:0\ny:2"
				elif _21 == 0:
					_21 = -1
					println "x:1\ny:2"
				else:
					_22 = -1
					println "x:2\ny:2"
			elif _00 + _10 + _20 == -2:
				if _00 == 0:
					_00 = -1
					println "x:0\ny:0"
				elif _10 == 0:
					_10 = -1
					println "x:0\ny:1"
				else:
					_20 = -1
					println "x:0\ny:2"
			elif _01 + _11 + _21 == -2:
				if _01 == 0:
					_01 = -1
					println "x:1\ny:0"
				elif _11 == 0:
					_11 = -1
					println "x:1\ny:1"
				else:
					_21 = -1
					println "x:1\ny:2"
			elif _02 + _12 + _22 == -2:
				if _02 == 0:
					_02 = -1
					println "x:2\ny:0"
				elif _12 == 0:
					_12 = -1
					println "x:2\ny:1"
				else:
					_22 = -1
					println "x:2\ny:2"
			elif _00 + _11 + _22 == -2:
				if _00 == 0:
					_00 = -1
					println "x:0\ny:0"
				elif _11 == 0:
					_11 = -1
					println "x:1\ny:1"
				else:
					_22 = -1
					println "x:2\ny:2"
			elif _20 + _11 + _02 == -2:
				if _20 == 0:
					_20 = -1
					println "x:0\ny:2"
				elif _11 == 0:
					_11 = -1
					println "x:1\ny:1"
				else:
					_02 = -1
					println "x:2\ny:0"

			elif _00 + _01 + _02 == 2:
				if _00 == 0:
					_00 = -1
					println "x:0\ny:0"
				elif _01 == 0:
					_01 = -1
					println "x:1\ny:0"
				else:
					_02 = -1
					println "x:2\ny:0"
			elif _10 + _11 + _12 == 2:
				if _10 == 0:
					_10 = -1
					println "x:0\ny:1"
				elif _11 == 0:
					_11 = -1
					println "x:1\ny:1"
				else:
					_12 = -1
					println "x:2\ny:1"
			elif _20 + _21 + _22 == 2:
				if _20 == 0:
					_20 = -1
					println "x:0\ny:2"
				elif _01 == 0:
					_21 = -1
					println "x:1\ny:2"
				else:
					_22 = -1
					println "x:2\ny:2"
			elif _00 + _10 + _20 == 2:
				if _00 == 0:
					_00 = -1
					println "x:0\ny:0"
				elif _10 == 0:
					_10 = -1
					println "x:0\ny:1"
				else:
					_20 = -1
					println "x:0\ny:2"
			elif _01 + _11 + _21 == 2:
				if _01 == 0:
					_01 = -1
					println "x:1\ny:0"
				elif _11 == 0:
					_11 = -1
					println "x:1\ny:1"
				else:
					_21 = -1
					println "x:1\ny:2"
			elif _02 + _12 + _22 == 2:
				if _02 == 0:
					_02 = -1
					println "x:2\ny:0"
				elif _12 == 0:
					_12 = -1
					println "x:2\ny:1"
				else:
					_22 = -1
					println "x:2\ny:2"
			elif _00 + _11 + _22 == 2:
				if _00 == 0:
					_00 = -1
					println "x:0\ny:0"
				elif _11 == 0:
					_11 = -1
					println "x:1\ny:1"
				else:
					_22 = -1
					println "x:2\ny:2"
			elif _20 + _11 + _02 == 2:
				if _20 == 0:
					_20 = -1
					println "x:0\ny:2"
				elif _11 == 0:
					_11 = -1
					println "x:1\ny:1"
				else:
					_02 = -1
					println "x:2\ny:0"

			elif _11 == 0:
				_11 = -1
				println "x:1\ny:1"
			elif _00 == 0:
				_00 = -1
				println "x:0\ny:0"
			elif _02 == 0:
				_02 = -1
				println "x:2\ny:0"
			elif _20 == 0:
				_20 = -1
				println "x:0\ny:2"
			elif _22 == 0:
				_22 = -1
				println "x:2\ny:2"
			elif _01 == 0:
				_01 = -1
				println "x:1\ny:0"
			elif _10 == 0:
				_10 = -1
				println "x:0\ny:1"
			elif _12 == 0:
				_12 = -1
				println "x:2\ny:1"
			elif _21 == 0:
				_21 = -1
				println "x:1\ny:2"
			count += 1
			playerTurn = True

		if _00 + _01 + _02 == 3:
			won = True
		elif _10 + _11 + _12 == 3:
			won = True
		elif _20 + _21 + _22 == 3:
			won = True
		elif _00 + _10 + _20 == 3:
			won = True
		elif _01 + _11 + _21 == 3:
			won = True
		elif _02 + _12 + _22 == 3:
			won = True
		elif _00 + _11 + _22 == 3:
			won = True
		elif _20 + _11 + _02 == 3:
			won = True

		elif _00 + _01 + _02 == -3:
			won = True
		elif _10 + _11 + _12 == -3:
			won = True
		elif _20 + _21 + _22 == -3:
			won = True
		elif _00 + _10 + _20 == -3:
			won = True
		elif _01 + _11 + _21 == -3:
			won = True
		elif _02 + _12 + _22 == -3:
			won = True
		elif _00 + _11 + _22 == -3:
			won = True
		elif _20 + _11 + _02 == -3:
			won = True
	else:
		println "tie"
		count = 10
