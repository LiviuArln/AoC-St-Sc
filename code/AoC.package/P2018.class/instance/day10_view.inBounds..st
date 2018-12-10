as yet unclassified
day10_view: data inBounds: bounds
| v ss trachelShape |
v := RTView new.
	ss := RTBitmap element.
	v add: ss.
	trachelShape := ss trachelShape.
	trachelShape extent: (bounds right - bounds left + 1) @ (bounds bottom - bounds top + 1).
	trachelShape fillColor: Color blue.
	data do: [ :fl | trachelShape at: (fl  x - bounds left) @ (fl  y - bounds top) put: Color green ].
	^v