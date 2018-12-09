as yet unclassified
day09p1: nPlayers marble: lastMarble
	| currentLink scores scoresByPlayer |
	scores := OrderedCollection new.
	currentLink := DoubleLink new.
	currentLink value: 0.
	currentLink nextLink: currentLink.
	currentLink previousLink: currentLink.
	1 to: lastMarble do: [ :m | 
		m % 23 = 0
			ifTrue: [ "self halt."
				currentLink := currentLink previousLink previousLink previousLink previousLink previousLink previousLink previousLink.
				currentLink previousLink nextLink: currentLink nextLink.
				currentLink nextLink previousLink: currentLink previousLink.
				scores add: m + currentLink value.
				currentLink := currentLink nextLink ]
			ifFalse: [ | newLink |
				currentLink := currentLink nextLink.
				newLink := DoubleLink new.
				newLink value: m.
				newLink nextLink: currentLink nextLink.
				newLink previousLink: currentLink.
				currentLink nextLink previousLink: newLink.
				currentLink nextLink: newLink.
				currentLink := newLink ] ].
	scoresByPlayer := ((scores withIndexCollect: [ :s :i | (i * 23 - 1) % nPlayers + 1 -> s ]) groupedBy: #key) collect: [ :g | (g collect: #value) sum ].
	^ scoresByPlayer max