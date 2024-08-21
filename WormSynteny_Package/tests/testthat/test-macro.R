# Case 1 : macro to macro : both genes data frame should be equal
test_that("macro function doesn't return the right data frame", {
  raw_df <- data.frame(
    seq_id = c("bovis_CBOVI.ctg00001_chrV", "becei_CSP29.scaffold30920_cov176", "panamensis_CSP28.scaffold4_cov88", "inopinata_Sp34_Chr5", "elegans_V", "tropicalis_Scaffold491", "remanei_V", "remanei_V", "latens_scaffold_178", "tribulationis_CSP40.scaffold03291", "briggsae_V", "nigoni_CM008513.1"),
    seqnames = c("CBOVI.ctg00001_chrV", "CSP29.scaffold30920_cov176", "CSP28.scaffold4_cov88", "Sp34_Chr5", "V", "Scaffold491", "V", "V", "scaffold_178", "CSP40.scaffold03291", "V", "CM008513.1"),
    start = c(5255821, 599969, 1722125, 11088229, 6017970, 39297, 7047651, 7048039, 106681, 125335, 11804562, 13053812),
    end = c(5256415, 600593, 1722423, 11089012, 6018886, 39928, 7048320, 7048456, 107352, 126152, 11805741, 13054519),
    length = c(595, 625, 299, 784, 917, 632, 670, 418, 672, 818, 1180, 708),
    strand = c("-", "+", "-", "+", "+", "-", "+", "-", "-", "-", "+", "+"),
    types = c("gene", "gene", "gene", "gene", "gene", "gene", "gene", "gene", "gene", "gene", "gene", "gene"),
    gene_id = c("CBOVI.g1081", "CSP29.g17806", "CSP28.g6317", "Sp34_50205400", "WBGene00000159", "Csp11.Scaffold491.g2076", "GCK72_017854", "GCK72_017855", "FL83_17155", "CSP40.g24546", "WBGene00030881", "Cni-aps-1"),
    gene_biotype = c("protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding"),
    Orthogroup = c("OG0007017", "OG0007017", "OG0007017", "OG0007017", "OG0007017", "OG0007017", "OG0007017", NA, "OG0007017", "OG0007017", "OG0007017", "OG0007017")
  )
  seqs <- data.frame(
    bin_id = c("C. tropicalis", "C. tribulationis", "C. remanei", "C. panamensis", "C. nigoni", "C. latens", "C. inopinata", "C. elegans", "C. briggsae", "C. bovis", "C. becei"),
    seq_id = c("tropicalis_Scaffold491", "tribulationis_CSP40.scaffold03291", "remanei_V", "panamensis_CSP28.scaffold4_cov88", "nigoni_CM008513.1", "latens_scaffold_178", "inopinata_Sp34_Chr5", "elegans_V", "briggsae_V", "bovis_CBOVI.ctg00001_chrV", "becei_CSP29.scaffold30920_cov176"),
    seqnames = c("Scaffold491", "CSP40.scaffold03291", "V", "CSP28.scaffold4_cov88", "CM008513.1", "scaffold_178", "Sp34_Chr5", "V", "V", "CBOVI.ctg00001_chrV", "CSP29.scaffold30920_cov176"),
    start = c(39164, 125207, 7047645, 1721983, 13053806, 106554, 11088223, 6017970, 11804561, 5255703, 599963),
    end = c(39933, 126157, 7048456, 1722423, 13054656, 107352, 11089012, 6018886, 11805744, 5256432, 600728),
    length = c(769, 950, 811, 440, 850, 798, 789, 917, 1183, 729, 765)
  )

  result <- macro(raw_df, seqs, species_macro)

  result$seqnames <- as.character(result$seqnames)
  result$strand <- as.character(result$strand)
  result$start <- as.double(result$start)
  result$end <- as.double(result$end)

  expect_identical(result, raw_df)

})

# Case 2 : macro to micro to macro : both genes data frame should be equal
test_that("macro function doesn't return the right data frame", {
  genes <- data.frame(
    seq_id = c("bovis_CBOVI.ctg00001_chrV", "becei_CSP29.scaffold30920_cov176", "panamensis_CSP28.scaffold4_cov88", "inopinata_Sp34_Chr5", "elegans_V", "tropicalis_Scaffold491", "remanei_V", "remanei_V", "latens_scaffold_178", "tribulationis_CSP40.scaffold03291", "briggsae_V", "nigoni_CM008513.1"),
    seqnames = c("CBOVI.ctg00001_chrV", "CSP29.scaffold30920_cov176", "CSP28.scaffold4_cov88", "Sp34_Chr5", "V", "Scaffold491", "V", "V", "scaffold_178", "CSP40.scaffold03291", "V", "CM008513.1"),
    start = c(5255821, 599969, 1722125, 11088229, 6017970, 39297, 7047651, 7048039, 106681, 125335, 11804562, 13053812),
    end = c(5256415, 600593, 1722423, 11089012, 6018886, 39928, 7048320, 7048456, 107352, 126152, 11805741, 13054519),
    length = c(595, 625, 299, 784, 917, 632, 670, 418, 672, 818, 1180, 708),
    strand = c("-", "+", "-", "+", "+", "-", "+", "-", "-", "-", "+", "+"),
    types = c("gene", "gene", "gene", "gene", "gene", "gene", "gene", "gene", "gene", "gene", "gene", "gene"),
    gene_id = c("CBOVI.g1081", "CSP29.g17806", "CSP28.g6317", "Sp34_50205400", "WBGene00000159", "Csp11.Scaffold491.g2076", "GCK72_017854", "GCK72_017855", "FL83_17155", "CSP40.g24546", "WBGene00030881", "Cni-aps-1"),
    gene_biotype = c("protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding", "protein_coding"),
    Orthogroup = c("OG0007017", "OG0007017", "OG0007017", "OG0007017", "OG0007017", "OG0007017", "OG0007017", NA, "OG0007017", "OG0007017", "OG0007017", "OG0007017")
  )

  seqs <- data.frame(
    bin_id = c("C. tropicalis", "C. tribulationis", "C. remanei", "C. panamensis", "C. nigoni", "C. latens", "C. inopinata", "C. elegans", "C. briggsae", "C. bovis", "C. becei"),
    seq_id = c("tropicalis_Scaffold491", "tribulationis_CSP40.scaffold03291", "remanei_V", "panamensis_CSP28.scaffold4_cov88", "nigoni_CM008513.1", "latens_scaffold_178", "inopinata_Sp34_Chr5", "elegans_V", "briggsae_V", "bovis_CBOVI.ctg00001_chrV", "becei_CSP29.scaffold30920_cov176"),
    seqnames = c("Scaffold491", "CSP40.scaffold03291", "V", "CSP28.scaffold4_cov88", "CM008513.1", "scaffold_178", "Sp34_Chr5", "V", "V", "CBOVI.ctg00001_chrV", "CSP29.scaffold30920_cov176"),
    start = c(39164, 125207, 7047645, 1721983, 13053806, 106554, 11088223, 6017970, 11804561, 5255703, 599963),
    end = c(39933, 126157, 7048456, 1722423, 13054656, 107352, 11089012, 6018886, 11805744, 5256432, 600728),
    length = c(769, 950, 811, 440, 850, 798, 789, 917, 1183, 729, 765)
  )

  genes_micro <- micro(genes, species_micro)
  result <- macro(genes_micro, seqs, species_macro)

  result$seqnames <- as.character(result$seqnames)
  result$strand <- as.character(result$strand)
  result$start <- as.double(result$start)
  result$end <- as.double(result$end)

  expect_identical(result, genes)

})

