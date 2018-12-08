as yet unclassified
metaSum
	^ metaData sum + (nodes ifEmpty:  [ 0 ] ifNotEmpty: [ (nodes collect: #metaSum) sum ])