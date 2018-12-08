as yet unclassified
solve
	| toCompare compareResults |
	toCompare := (puzzle associations flatCollect: [ :c1 | puzzle associations collect: [ :c2 | c1 -> c2 ] ]) reject: [ :a | a head = a tail tail head ].
	toCompare := toCompare collect: [ :a | a tail head -> a tail tail tail head ].
	compareResults := toCompare collect: [ :a | a head intersection: a value ].
	^ (compareResults inject: Set new into: [ :s1 :s2 | s1 union: s2 ]) size