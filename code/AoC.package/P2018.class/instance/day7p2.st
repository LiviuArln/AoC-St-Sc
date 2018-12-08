as yet unclassified
day7p2
	| puzzleSmall puzzle pattern inverseLinks nodes ealiestFinish dependencies dependenciesCopy toVisit visited stepDurations workers currentDependenciesTimes |
	puzzleSmall := 'Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.'.
	puzzle := 'Step Y must be finished before step L can begin.
Step N must be finished before step D can begin.
Step Z must be finished before step A can begin.
Step F must be finished before step L can begin.
Step H must be finished before step G can begin.
Step I must be finished before step S can begin.
Step M must be finished before step U can begin.
Step R must be finished before step J can begin.
Step T must be finished before step D can begin.
Step U must be finished before step D can begin.
Step O must be finished before step X can begin.
Step B must be finished before step D can begin.
Step X must be finished before step V can begin.
Step J must be finished before step V can begin.
Step D must be finished before step A can begin.
Step K must be finished before step P can begin.
Step Q must be finished before step C can begin.
Step S must be finished before step E can begin.
Step A must be finished before step V can begin.
Step G must be finished before step L can begin.
Step C must be finished before step W can begin.
Step P must be finished before step W can begin.
Step V must be finished before step W can begin.
Step E must be finished before step W can begin.
Step W must be finished before step L can begin.
Step P must be finished before step E can begin.
Step T must be finished before step K can begin.
Step A must be finished before step G can begin.
Step G must be finished before step P can begin.
Step N must be finished before step S can begin.
Step R must be finished before step D can begin.
Step M must be finished before step G can begin.
Step Z must be finished before step L can begin.
Step M must be finished before step T can begin.
Step S must be finished before step L can begin.
Step S must be finished before step W can begin.
Step O must be finished before step J can begin.
Step Z must be finished before step D can begin.
Step A must be finished before step C can begin.
Step P must be finished before step V can begin.
Step A must be finished before step P can begin.
Step B must be finished before step C can begin.
Step R must be finished before step S can begin.
Step X must be finished before step S can begin.
Step T must be finished before step P can begin.
Step Y must be finished before step E can begin.
Step G must be finished before step E can begin.
Step Y must be finished before step K can begin.
Step J must be finished before step P can begin.
Step I must be finished before step Q can begin.
Step E must be finished before step L can begin.
Step X must be finished before step J can begin.
Step T must be finished before step X can begin.
Step M must be finished before step O can begin.
Step K must be finished before step A can begin.
Step D must be finished before step W can begin.
Step H must be finished before step C can begin.
Step F must be finished before step R can begin.
Step B must be finished before step Q can begin.
Step M must be finished before step Q can begin.
Step D must be finished before step S can begin.
Step Y must be finished before step I can begin.
Step M must be finished before step K can begin.
Step S must be finished before step G can begin.
Step X must be finished before step L can begin.
Step D must be finished before step V can begin.
Step B must be finished before step X can begin.
Step C must be finished before step L can begin.
Step V must be finished before step L can begin.
Step Z must be finished before step Q can begin.
Step Z must be finished before step H can begin.
Step M must be finished before step S can begin.
Step O must be finished before step C can begin.
Step B must be finished before step A can begin.
Step U must be finished before step V can begin.
Step U must be finished before step A can begin.
Step X must be finished before step G can begin.
Step K must be finished before step C can begin.
Step T must be finished before step S can begin.
Step K must be finished before step G can begin.
Step U must be finished before step B can begin.
Step A must be finished before step E can begin.
Step F must be finished before step V can begin.
Step Q must be finished before step A can begin.
Step F must be finished before step Q can begin.
Step J must be finished before step L can begin.
Step O must be finished before step E can begin.
Step O must be finished before step Q can begin.
Step I must be finished before step K can begin.
Step I must be finished before step P can begin.
Step J must be finished before step D can begin.
Step Q must be finished before step P can begin.
Step S must be finished before step C can begin.
Step U must be finished before step P can begin.
Step S must be finished before step P can begin.
Step O must be finished before step B can begin.
Step Z must be finished before step F can begin.
Step R must be finished before step V can begin.
Step D must be finished before step L can begin.
Step Y must be finished before step T can begin.
Step G must be finished before step C can begin.'.
	pattern := RxMatcher forString: 'Step (.*) must be finished before step (.*) can begin.'.
	inverseLinks := puzzle lines
		collect: [ :l | 
			(pattern matchesStream: l readStream)
				ifTrue: [ (pattern subexpression: 3) -> (pattern subexpression: 2) ] ].
	nodes := (inverseLinks
		flatCollect: [ :a | 
			{a head.
			a value} ]) asSet.
	ealiestFinish := (nodes collect: [ :s | s -> (s first asciiValue - $A asciiValue + 61) ]) asDictionary.
	dependencies := nodes collect: [ :n | n -> (inverseLinks select: [ :l | l head = n ] thenCollect: #value) ].
	dependenciesCopy := dependencies asDictionary.
	toVisit := (dependencies select: [ :a | a value size = 0 ] thenCollect: #head)
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
	workers := D7Workers new.
	[ toVisit isEmpty ]
		whileFalse: [ | next |
			dependencies := dependencies reject: [ :a | toVisit includes: a head ].
			next := toVisit removeFirst.
			visited addLast: next.
			dependencies := dependencies collect: [ :a | a head -> (a value reject: [ :n | n = next ]) ].
			(dependencies select: [ :a | a value size = 0 ] thenCollect: #head) do: [ :n | toVisit add: n ].
			currentDependenciesTimes := ((dependenciesCopy at: next) collect: [ :d | d -> (ealiestFinish at: d) ]) asDictionary.
			ealiestFinish
				at: next
				put:
					(currentDependenciesTimes
						ifEmpty: [ workers work: (ealiestFinish at: next) ]
						ifNotEmpty: [ | baseTime |
							baseTime := {currentDependenciesTimes max.
							workers min} max.
							workers at: baseTime work: (ealiestFinish at: next) ]).
			Transcript
				show: toVisit;
				cr;
				show: visited;
				cr;
				show: ealiestFinish;
				cr;
				show: workers ws;
				cr;
				cr ].
	visited.
	workers max
