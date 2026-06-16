# MethScope

**MethScope** identifies Most Recurrent Methylation Patterns (MRMPs) as the basis to encode latent representations for interpretatble analysis of DNA mehtylation data, in particular single cell and spatial methylome. The MRMPs embeddings will support automatic **cell annotation**, **bulk deconvolution**, **unsupervisded clustering**, and **cancer cell of origin prediction**. 

## Installation

MethScope requires R >= 4.0 and the zlib system library.

Install the released version of usethis from CRAN:

``` r
install.packages("MethScope")
```

or install from github

```r
devtools::install_github("zhou-lab/MethScope")
```

## Usage 

**MethScope** performs rapid cell annotation, deconvolution and other tasks for single cell and spatial DNA methylome. Please check out our documentation for more details: 

[Explore the MethScope Website](https://zhou-lab.github.io/MethScope/)

## Example Data

The CRAN package includes a small toy dataset for quick installation checks and examples:

```r
qry <- system.file("extdata", "toy.cg", package = "MethScope")
msk <- system.file("extdata", "toy.cm", package = "MethScope")
res <- GenerateInput(qry, msk)
```

The GitHub repository also provides a larger `inst/extdata/example.cg` file for fuller end-to-end testing after cloning the repository. This larger file is excluded from CRAN builds to keep the package size appropriate for CRAN.

```r
qry <- "inst/extdata/example.cg"
msk <- system.file("extdata", "toy.cm", package = "MethScope")
res <- GenerateInput(qry, msk)
```

## Citation

If you use MethScope, kindly cite (coming soon):

Hongxiang Fu, Chin Nien Lee, Cameron Cloud, Hao Xu, Yanxiang Deng, Wanding Zhou, MethScope: Ultra-fast Analysis of Sparse DNA Methylome via Recurrent Pattern Encoding.
