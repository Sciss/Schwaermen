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

## cover

The cover is generated through a GNG process based on a gray-scale "probability map" of one of the
photos from the exhibition.

Run `CatalogCover` with `--selected`.

We assume a middle bar of 5mm (for 80 pages catalog), which means the image is 405 x 200 mm plus
3 mm bleed, thus in total 411 x 206 mm. The photo will have around 250 dpi at this resolution, the
GNG will be rendered at higher resolution, 450 dpi or 7282 x 3650 pixels.

The final cover is created by running the result through this process:

- scale original colour photo to 7282 x 3650
- apply gaussian blur of radius 48
- normalise colours (stretches contrast)
- adjust colour balance, magenta-green +40 (this makes the NEF correspond more to the JPG)
- add gray GNG as a second layer
- add noise "pick" filter to GNG layer, maximum (100) randomisation
- add HSV noise to GNG layer: hue 0, saturation 0, value 64
- set GNG layer composition mode to 'Value'
- duplicate GNG layer and set now top-most layer to composition mode 'Grain Merge'

Export flattened image at 450 dpi to PDF.