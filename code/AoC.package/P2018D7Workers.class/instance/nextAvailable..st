stream in
nextAvailable: baseTime
	
ws associations do: [ :a | a value <= baseTime ifTrue:[^ a head]]