as yet unclassified
day11p
	| serial maxValue maxCoordinates lastCalculations |
	serial := 4455.
	lastCalculations := Dictionary new.
	maxValue := 0.
	0 to: 299 do: [ :n | 
		| bleedigMargin |
		bleedigMargin := self day11_bleedigMargin: n.
		1 to: 300 - n do: [ :x | 
			1 to: 300 - n do: [ :y | 
				| smallerSquareValue value |
				smallerSquareValue := lastCalculations at: x @ y ifAbsent: [ 0 ].
				value := smallerSquareValue + (bleedigMargin collect: [ :p | self day11_powerLevel: serial at: x @ y + p ]) sum.
				lastCalculations at: x @ y put: value.
				value > maxValue
					ifTrue: [ maxValue := value.
						maxCoordinates := x @ y -> (n + 1) ] ] ] ].
	^ maxCoordinates