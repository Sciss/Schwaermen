# steps

 - in Mellite - in workspace `Schwaermen170615.mllt`, execute action 'ExtractWords' with attributes
   `timeline` and `output` (of type `Artifact`), e.g. gives `text1words.bin`.
 - verify text using `YieldText`
 - build first binary file using `BuildSimilarities` with two text pairs, e.g. gives `edges12.bin`
 - build final binary file using `BinarySimilarities` with two text pairs, e.g. gives `edges12opt.bin`.
