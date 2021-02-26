# XNAString: Efficient manipulation of modified oligonucleotide sequences
------------------------------------------------------------------------
<!-- badges: start -->
[![R build status](https://github.com/plucinskam/XNAString/workflows/R-CMD-check/badge.svg)](https://github.com/plucinskam/XNAString/actions)
[![R-CMD-check-bioc](https://github.com/plucinskam/XNAString/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/plucinskam/XNAString/actions)
<!-- badges: end -->

### **Description**
The XNAString package allows for description of base sequences and associated chemical modifications in a single object. XNAString is able to capture single stranded, as well as double stranded molecules. Chemical modifications are represented as independent strings associated with different features of the molecules (base sequence, sugar sequence, backbone sequence, modifications) and can be read or written to a HELM notation. It also enables secondary structure prediction using RNAfold from ViennaRNA.
XNAString is designed to be efficient representation of nucleic-acid based therapeutics, therefore it stores information about target sequences and provides interface for matching and alignment functions from Biostrings package. 


### **Usage**

See [vignette](vignettes/XNAString_vignette.html) for usage examples


### **Dependencies**
1) [Biostrings](https://github.com/Bioconductor/Biostrings) - Matching & alignment methods (matchPattern, vmatchPattern, pairwiseAlignment and matchPDict) are inherited from Biostrings R package. Check [Biostrings repository](https://github.com/Bioconductor/Biostrings) for detailed information.


2) [ViennaRNA](https://github.com/ViennaRNA/ViennaRNA) - Source code of ViennaRNA is included in XNAString package and used for secondary structure prediction. Please note that ViennaRNA is distributed under its own [licence](https://github.com/ViennaRNA/ViennaRNA/blob/master/COPYING).

    Citation:
      * Lorenz, R., Bernhart, S.H., Höner zu Siederdissen, C. et al. ViennaRNA Package 2.0. Algorithms Mol Biol 6, 26 (2011). https://doi.org/10.1186/1748-7188-6-26
      * Hofacker, I.L., Fontana, W., Stadler, P.F. et al. Fast folding and comparison of RNA secondary structures. Monatsh Chem 125, 167–188 (1994). https://doi.org/10.1007/BF0081816
  

------------------------------------------------------------------------

### **Authors**

Lykke Pedersen (1)

Łukasz J. Kiełpiński (1)

Anna Górska (2)

Marianna Plucinska (2)

Disa Tehler (1)

Peter H. Hagedorn (1)

(1) Roche Pharma Research and Early Development, Roche Innovation Center Copenhagen, DK-2970 Hørsholm, Denmark.

(2) Roche Global IT Solution Centre, Roche Polska Sp. z o.o.
