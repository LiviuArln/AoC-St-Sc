as yet unclassified
day07p1
	| pattern inverseLinks nodes dependencies toVisit visited |
	pattern := RxMatcher forString: 'Step (.*) must be finished before step (.*) can begin.'.
	inverseLinks := self day07_puzzles second lines
		collect: [ :l | 
			(pattern matchesStream: l readStream)
				ifTrue: [ (pattern subexpression: 3) -> (pattern subexpression: 2) ] ].
	nodes := (inverseLinks
		flatCollect: [ :a | 
			{a key.
			a value} ]) asSet.
	dependencies := nodes collect: [ :n | n -> (inverseLinks select: [ :l | l key = n ] thenCollect: #value) ].
	toVisit := (dependencies select: [ :a | a value isEmpty  ] thenCollect: #key) asSortedCollection.
	visited := OrderedCollection new.
	[ toVisit isEmpty ]
		whileFalse: [ | next |
			dependencies := dependencies reject: [ :a | toVisit includes: a key ].
			next := toVisit removeFirst.
			visited addLast: next.
			dependencies := dependencies collect: [ :a | a key -> (a value reject: [ :n | n = next ]) ].
			(dependencies select: [ :a | a value isEmpty ] thenCollect: #key) do: [ :n | toVisit add: n ] ].
	^ visited joinUsing: ''