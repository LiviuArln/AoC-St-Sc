as yet unclassified
day8p1
	| data nodeStack completedNodes |
	data := (self day8puzzles second splitOn: $ ) collect: #asNumber.
	nodeStack := Stack new.
	nodeStack push: P2018D8Node new.
	completedNodes := OrderedCollection new.
	data
		do: [ :d | 
			nodeStack top consumeData: d.
			[ nodeStack isNotEmpty and: [ nodeStack top expectingChild | nodeStack top complete ] ]
				whileTrue: [ nodeStack top expectingChild
						ifTrue: [ nodeStack top nodeCreated.
							nodeStack push: P2018D8Node new ]
						ifFalse: [ completedNodes add: nodeStack pop ] ] ].
	^ (completedNodes collect: #metaSum) sum