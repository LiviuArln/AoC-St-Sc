as yet unclassified
initiazilePuzzle: aPuzzle
	| lines |
	lines := Character cr split: aPuzzle.
	puzzle := (lines collect: [ :line | self claimFrom: line ])asDictionary