as yet unclassified
day8p2
	| data nodeStack completedNodes firstNode |
	data := (self day8puzzles second splitOn: $ ) collect: #asNumber.
	nodeStack := Stack new.
	firstNode := P2018D8Node new.
	nodeStack push: firstNode.
	completedNodes := OrderedCollection new.
	data
		do: [ :d | 
			nodeStack top consumeData: d.
			[ nodeStack isNotEmpty and: [ nodeStack top expectingChild | nodeStack top complete ] ]
				whileTrue: [ nodeStack top expectingChild
						ifTrue: [ | newNode |
							newNode := P2018D8Node new.
							nodeStack top nodeCreated: newNode.
							nodeStack push: newNode ]
						ifFalse: [ completedNodes add: nodeStack pop ] ] ].
	^ firstNode nodeSum