accessing
day01p2
	"comment stating purpose of message"

	| currentFrq resultingFrqSet |
	currentFrq := 0.
	resultingFrqSet := Set new.
	resultingFrqSet add: currentFrq.
	[ self day01_puzzle select: [ :v | (v = #+) not ]
		thenCollect: [ :v | 
			currentFrq := currentFrq + v.
			(resultingFrqSet includes: currentFrq)
				ifFalse: [ resultingFrqSet add: currentFrq ]
				ifTrue: [ ^ currentFrq ] ] ] repeat