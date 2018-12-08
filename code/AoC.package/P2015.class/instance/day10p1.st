accessing
day10p1
	| puzzle counter |
	puzzle := '1'.
	counter := OrderedCollection new.
	puzzle do: [ :c|  
		
		Transcript show: counter last; cr.
	]