as yet unclassified
complete
	^ self headerComplete & (nodesCount = header head) & (metaData size = header value)