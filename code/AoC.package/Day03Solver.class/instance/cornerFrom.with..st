as yet unclassified
cornerFrom: aString with: aChar
	| coordinates |
	coordinates := aChar split: aString.
	^ Point x: coordinates first asNumber y: coordinates second asNumber