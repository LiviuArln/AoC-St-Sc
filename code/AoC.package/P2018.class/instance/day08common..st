as yet unclassified
day08common: sumType
	| firstNode data nodeStack |
	data := (self day08_puzzles second splitOn: $ ) collect: #asNumber.
	nodeStack := Stack new.
	firstNode := D08Node new.
	nodeStack push: firstNode.
	data
		do: [ :d | 
			nodeStack top consumeData: d.
			[ nodeStack isNotEmpty
				and: [ nodeStack top expectingChild | nodeStack top complete ] ]
				whileTrue: [ nodeStack top expectingChild
						ifTrue: [ | newNode |
							newNode := D08Node new.
							nodeStack top nodeCreated: newNode.
							nodeStack push: newNode ]
						ifFalse: [ nodeStack pop ] ] ].
	^ firstNode perform: sumType