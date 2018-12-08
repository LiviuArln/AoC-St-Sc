as yet unclassified
topLeftCornerFrom: aString
	| coordinates |
	coordinates := $, split: aString.
	^ Point x: coordinates first asNumber y:  coordinates second asNumber 