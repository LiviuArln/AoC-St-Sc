as yet unclassified
consumeData: aNumber
	self headerComplete
		ifTrue: [ metaData add: aNumber ]
		ifFalse: [ header head = -1
				ifTrue: [ header := aNumber -> -1 ]
				ifFalse: [ header := header head -> aNumber ] ]