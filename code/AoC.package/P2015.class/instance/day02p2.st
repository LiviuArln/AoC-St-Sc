accessing
day02p2
	"comment stating purpose of message"

	| currentFrq resultingFrqSet |
	currentFrq := 0.
	resultingFrqSet := Set new.
	resultingFrqSet add: currentFrq.
	[ self day02_puzzle second
		collect: [ :v | 
			currentFrq := currentFrq + v.
			(resultingFrqSet includes: currentFrq)
				ifFalse: [ resultingFrqSet add: currentFrq ]
				ifTrue: [ ^ currentFrq ] ] ] repeat