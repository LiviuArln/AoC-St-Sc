accessing
day01p1
	^ (self day01_puzzle select: [ :v | (v = #+) not ] ) sum