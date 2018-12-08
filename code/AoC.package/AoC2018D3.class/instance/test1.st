tests
test1
	| q1 |
	q1 := (Character cr split: self) puzzle collect: [ :l | $@ split: l ].
	self assert: q1