stream in
work: aTime
	| min worker |
	min := ws min.
	worker := ws keyAtValue: min.
	^ ws at: worker put: min + aTime
	