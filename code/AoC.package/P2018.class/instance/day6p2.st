as yet unclassified
day6p2
	| coordinates gridTopLeft gridBottomRight distances |
	coordinates := self day6puzzles first lines
		collect: [ :l | 
			| s |
			s := $, split: l.
			s first asNumber @ s second trim asNumber ].
	gridTopLeft := (coordinates collect: [ :p | p x ]) min @ (coordinates collect: [ :p | p y ]) min.
	gridBottomRight := (coordinates collect: [ :p | p x ]) max @ (coordinates collect: [ :p | p y ]) max.
	distances := Dictionary new.
	gridTopLeft x to: gridBottomRight x do: [ :x | gridTopLeft y to: gridBottomRight y do: [ :y | distances at: x @ y put: (coordinates collect: [ :c | (c x - x) abs + (c y - y) abs ]) sum ] ].
	(distances select: [ :d | d < 10000 ]) size