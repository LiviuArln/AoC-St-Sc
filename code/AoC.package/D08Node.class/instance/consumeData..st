as yet unclassified
consumeData: aNumber
	self headerComplete
		ifTrue: [ metaData add: aNumber ]
		ifFalse: [ header key = -1
				ifTrue: [ header := aNumber -> -1 ]
				ifFalse: [ header := header key -> aNumber ] ]