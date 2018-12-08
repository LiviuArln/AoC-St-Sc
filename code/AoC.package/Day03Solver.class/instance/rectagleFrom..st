as yet unclassified
rectagleFrom: aString
	| postionAndDimensions topLeft dimensions |
	postionAndDimensions := $: split: aString.
	topLeft := self cornerFrom: postionAndDimensions first with: $,.
	dimensions := self cornerFrom: postionAndDimensions second trim with: $x.
	^ self allSquares:  (topLeft extent: dimensions)