as yet unclassified
day11_bleedigMargin: aNumber
	| interval margin |
	interval := (0 to: aNumber-1) asOrderedCollection.
	margin := interval collect: [ :n | 0 @ (n +1) ].
	margin addAll: (interval collect: [ :n | (n+1) @ 0 ]).
	margin add: aNumber @ aNumber.
	^ margin asSet