as yet unclassified
claimFrom: aLine
	| splited |
	splited := $@ split: aLine.
	^ ($# split: splited first trim) second -> (self rectagleFrom: splited second trim)