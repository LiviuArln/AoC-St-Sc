as yet unclassified
day11p
	| serial maxValue maxCoordinates lastCalculations |
	serial := 4455.
	lastCalculations := Dictionary new.
	maxValue := 0.
	0 to: 299 do: [ :n | 
		| bleedigMargin |
		bleedigMargin := (self day11_bleedigMargin: n) asOrderedCollection.
		1 to: 100 - n do: [ :x | 
			1 to: 100 - n do: [ :y | 
				| smallerSquareValue value actualBleedingMargin bleedingMarginValues |
				smallerSquareValue := lastCalculations at: x @ y ifAbsent: [ 0 ].
				actualBleedingMargin := bleedigMargin collect: [ :p | x @ y + p ].
				bleedingMarginValues := actualBleedingMargin collect: [ :p | self day11_powerLevel: serial x: p x y: p y ].
				Transcript
					show: x @ y;
					cr;
					show: smallerSquareValue;
					cr;
					show: actualBleedingMargin;
					cr;
					cr.
				value := smallerSquareValue + bleedingMarginValues sum.
				lastCalculations at: x @ y put: value.
				value > maxValue
					ifTrue: [ maxValue := value.
						maxCoordinates := x @ y -> (n + 1) ] ] ] ].
	^ maxCoordinates