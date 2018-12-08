stream in
at: baseTime work: aTime
	| nextWorker |
	nextWorker := self nextAvailable: baseTime.
	^ ws at: nextWorker put: baseTime + aTime