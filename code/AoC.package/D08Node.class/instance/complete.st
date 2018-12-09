as yet unclassified
complete
	^ self headerComplete & (nodesCount = header key) & (metaData size = header value)