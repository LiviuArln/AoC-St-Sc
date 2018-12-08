as yet unclassified
day06p1
	| coordinates distances allXY grid infinity xx |
	coordinates := self day06_puzzles first lines
		collect: [ :l | 
			| s |
			s := $, split: l.
			s first asNumber @ s second trim asNumber ].
	allXY := (coordinates collect: [ :p | p x ]) -> (coordinates collect: [ :p | p y ]).
	grid := allXY head min @ allXY value min corner: allXY head max @ allXY value max.
	distances := Dictionary new.
	coordinates
		do: [ :c | 
			grid top to: grid bottom do: [ :x | 
				grid left to: grid right do: [ :y | 
					| distance currentMin |
					distance := (c x - x) abs + (c y - y) abs.
					currentMin := distances at: x @ y ifAbsentPut: [ SmallInteger maxVal -> OrderedCollection new ].
					currentMin head = distance
						ifTrue: [ currentMin value add: c ].
					currentMin head > distance
						ifTrue: [ currentMin := distance -> {c} asOrderedCollection ].
					distances at: x @ y put: currentMin ] ] ].
	infinity := Set new.
	(grid top to: grid bottom)
		collect: [ :x | 
			infinity addAll: (distances at: x @ grid left) value.
			infinity addAll: (distances at: x @ grid right) value ].
	(grid left to: grid right)
		collect: [ :y | 
			infinity addAll: (distances at: grid top @ y) value.
			infinity addAll: (distances at: grid bottom @ y) value ].
	xx := (distances associations select: [ :a | a tail value size = 1 ] thenCollect: [ :a | a head -> a tail value anyOne ])
		reject: [ :a | 
			infinity includes: a value ].
	^ infinity includes: 1 @ 1