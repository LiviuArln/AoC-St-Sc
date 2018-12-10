as yet unclassified
day10_findBounds: lights
	| ys xs |
	xs := lights collect: [ :a | a key x ].
	ys := lights collect: [ :a | a key y ].
	^ xs min @ ys min corner: xs max @ ys max