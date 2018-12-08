as yet unclassified
allSquares: aRectangle
	| allSquares |
	allSquares := Set new.
	(aRectangle topLeft x to: aRectangle bottomRight x - 1) do: [ :x |
		(aRectangle topLeft y to: aRectangle bottomRight y - 1) do: [ :y| 
			allSquares add: (Point x: x y: y) ] ].
	^ allSquares