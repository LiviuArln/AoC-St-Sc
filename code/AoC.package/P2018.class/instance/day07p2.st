as yet unclassified
day07p2
	| pattern inverseLinks nodes ealiestFinish dependencies dependenciesCopy toVisit visited stepDurations workers currentDependenciesTimes |
	pattern := RxMatcher forString: 'Step (.*) must be finished before step (.*) can begin.'.
	inverseLinks := self day07_puzzles second lines
		collect: [ :l | 
			(pattern matchesStream: l readStream)
				ifTrue: [ (pattern subexpression: 3) -> (pattern subexpression: 2) ] ].
	nodes := (inverseLinks
		flatCollect: [ :a | 
			{a key.
			a value} ]) asSet.
	ealiestFinish := (nodes collect: [ :s | s -> (s first asciiValue - $A asciiValue + 61) ]) asDictionary.
	dependencies := nodes collect: [ :n | n -> (inverseLinks select: [ :l | l key = n ] thenCollect: #value) ].
	dependenciesCopy := dependencies asDictionary.
	toVisit := (dependencies select: [ :a | a value size = 0 ] thenCollect: #key)
		asSortedCollection: [ :x :y | 
			| xdts ydts mx my |
			xdts := (dependenciesCopy at: x) collect: [ :d | ealiestFinish at: d ].
			ydts := (dependenciesCopy at: y) collect: [ :d | ealiestFinish at: d ].
			mx := xdts isEmpty
				ifTrue: [ 0 ]
				ifFalse: [ xdts max ].
			my := ydts isEmpty
				ifTrue: [ 0 ]
				ifFalse: [ ydts max ].
			mx = my
				ifTrue: [ x < y ]
				ifFalse: [ mx > my ] ].
	visited := OrderedCollection new.
	stepDurations := nodes collect: [ :s | s -> (s first asciiValue - $A asciiValue + 1) ].
	workers := D07Workers new.
	[ toVisit isEmpty ]
		whileFalse: [ | next |
			dependencies := dependencies reject: [ :a | toVisit includes: a key ].
			next := toVisit removeFirst.
			visited addLast: next.
			dependencies := dependencies collect: [ :a | a key -> (a value reject: [ :n | n = next ]) ].
			(dependencies select: [ :a | a value size = 0 ] thenCollect: #key) do: [ :n | toVisit add: n ].
			currentDependenciesTimes := ((dependenciesCopy at: next) collect: [ :d | d -> (ealiestFinish at: d) ]) asDictionary.
			ealiestFinish
				at: next
				put:
					(currentDependenciesTimes
						ifEmpty: [ workers work: (ealiestFinish at: next) ]
						ifNotEmpty: [ | baseTime |
							baseTime := {currentDependenciesTimes max.
							workers min} max.
							workers at: baseTime work: (ealiestFinish at: next) ]) ].^workers max