accessing
day02p1
	| count2Letters count3Letters |
	count2Letters := 0.
	count3Letters := 0.
	self  day02_puzzle first
		do: [ :id | 
			| bagOfLetters letterOccurrncies |
			bagOfLetters := Bag new.
			id do: [ :c | bagOfLetters add: c ].
			letterOccurrncies := (bagOfLetters asSet collect: [ :k | k -> (bagOfLetters occurrencesOf: k) ]) asDictionary.
			(letterOccurrncies contains: [ :asoc | asoc value = 2 ])
				ifTrue: [ count2Letters := count2Letters + 1 ].
			(letterOccurrncies contains: [ :asoc | asoc value = 3 ])
				ifTrue: [ count3Letters := count3Letters + 1 ] ].
	^ count2Letters * count3Letters