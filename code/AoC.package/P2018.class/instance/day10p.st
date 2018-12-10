as yet unclassified
day10p
	""

	| floatingLights matcher bounds lastLights |
	matcher := RxMatcher forString: 'position=<(.*),(.*)> velocity=<(.*),(.*)>'.
	floatingLights := self day10_puzzles second lines
		collect: [ :l | 
			(matcher matches: l)
				ifTrue: [ (matcher subexpression: 2) trim asNumber @ (matcher subexpression: 3) trim asNumber -> ((matcher subexpression: 4) trim asNumber @ (matcher subexpression: 5) trim asNumber) ] ].
	bounds := OrderedCollection new.
	bounds addFirst: (self day10_findBounds: floatingLights).
	[ 
	
	
	lastLights := floatingLights.
	floatingLights := floatingLights collect: [ :a | a key + a value -> a value ].
	bounds addFirst: (self day10_findBounds: floatingLights).
	bounds first area < bounds second area ] whileTrue.
	^ (self day10_view: (lastLights collect: #key) inBounds: bounds second) -> (bounds size - 2)