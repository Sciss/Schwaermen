# steps

 - in Mellite - in workspace `Schwaermen170615.mllt`, execute action 'ExtractWords' with attributes
   `timeline` and `output` (of type `Artifact`), e.g. gives `text1words.bin`.
 - verify text using `YieldText`
 - build first binary file using `BuildSimilarities` with two text pairs, e.g. gives `edges12.bin`
 - build final binary file using `BinarySimilarities` with two text pairs, e.g. gives `edges12opt.bin`.

# catalog

The catalog text _Eine Null schiebt sich zwischen Eins und Acht_ can be rendered by 
running `Catalog` first, followed by `CatalogPaths`. This will render all the text fragments,
then learn the topology of the paper space, then develop the paths of the 'edges' sentences, finally
render the compound document. Directories are hardcoded as `dir` and `dirTmp` in `Catalog`. Unix
programs `pdflatex`, `inkscape`, and `pdftk` must be installed. Installation of TeX live extra 
fonts might be needed (we use 'Alegreya').