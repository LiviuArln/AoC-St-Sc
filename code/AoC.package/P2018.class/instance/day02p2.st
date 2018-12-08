accessing
day02p2
	| comparedIds idSize onlyOneDiff |
	idSize := self  first size.
	comparedIds := self day02_puzzle first
		flatCollect: [ :id1 | 
			self day02_puzzle first
				collect: [ :id2 | 
					| matches |
					matches := id1 asOrderedCollection with: id2 asOrderedCollection collect: [ :c1 :c2 | c1 -> (c1 = c2) ].
					matches select: [ :assoc | assoc value ] ] ].
	onlyOneDiff := comparedIds select: [ :c | c isNotEmpty & (c size = (idSize - 1)) ].
	^ (onlyOneDiff collect: [ :c | c collect: [ :a | a head ] ]) first inject: '' into:  [ :v1 :v2 | v1 , (String with: v2) ]