## ----include = FALSE----------------------------------------------------------
options(device = "png")
knitr::opts_chunk$set(
  fig.ext = "png",
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(MethScope)

## ----eval=FALSE---------------------------------------------------------------
#  #path to your .cg and .cm files
#  example_file <- "example.cg"
#  reference_pattern <- "Liu2021_MouseBrain.cm"
#  input_pattern <- GenerateInput(example_file, reference_pattern)

## ----eval=FALSE---------------------------------------------------------------
#  prediction_result <- PredictCellType(MethScope:::Liu2021_MouseBrain_P1000,input_pattern)

## ----eval=FALSE---------------------------------------------------------------
#  trained_model <- Input_training(input_pattern,cell_type_label)

## ----eval=FALSE---------------------------------------------------------------
#  umap_plot <- PlotUMAP(input_pattern,prediction_result)
#  ### cell_type_label is the true cell type label
#  PlotConfusion(prediction_result,cell_type_label)
#  PlotF1(prediction_result,cell_type_label)

## ----eval=FALSE---------------------------------------------------------------
#  reference_pattern <- "Liu2021_MouseBrain.cm"
#  reference_input <- readRDS("2021Liu_reference_pattern.rds")
#  cell_proportion <- nnls_deconv(reference_input,input_pattern)

## ----eval=FALSE---------------------------------------------------------------
#  Pattern.obj <- CreateSeuratObject(counts = t(input_pattern), assay = "DNAm")
#  VariableFeatures(Pattern.obj) <- rownames(Pattern.obj[['DNAm']])
#  DefaultAssay(Pattern.obj) <- "DNAm"
#  Pattern.obj <- NormalizeData(Pattern.obj, assay = "DNAm", verbose = FALSE)
#  Pattern.obj <- ScaleData(Pattern.obj, assay = "DNAm", verbose = FALSE)
#  ### Can directly use the initial counts matrix
#  Pattern.obj@assays$DNAm@layers$scale.data <- as.matrix(Pattern.obj@assays$DNAm@layers$counts)
#  Pattern.obj <- RunPCA(Pattern.obj,assay="DNAm",reduction.name = 'mpca', verbose = FALSE)
#  Pattern.obj <- FindNeighbors(Pattern.obj, reduction = "mpca", dims = 1:30)
#  Pattern.obj <- FindClusters(Pattern.obj, verbose = FALSE, resolution = 0.7)
#  Pattern.obj <- RunUMAP(Pattern.obj, reduction = "mpca",  reduction.name = "meth.umap", dims = 1:30)

