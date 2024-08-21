#' Pipeline function
#'
#' Generate three types of tables to be used in gggenomes: aligned_sequences used in gggenomes, aligned_genes, and links along with an aligned sequences table to be display
#'
#' Belong to the server function in every section
#'
#' @param region The inquiry region, a data frame generated with create_region function
#'
#' @return A list of data frames : sequences, aligned_genes, links and aligned_sequences
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom tidyr separate
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_c
#'
#'
#' @examples
#' # Generate the necessary three tables for gggenomes: aligned_genes, aligned_sequences and links
#' # data <- pipeline_fct(region)
#'
pipeline_fct <- function(region){

  #Get the path
  folder_ubuntu <- get_ubuntu_path()

  ## Step 1 : Retrieve the homologous fragments of the C. elegans sequence of inquiry

  #Species names
  species <- c("bovis", "becei", "panamensis", "inopinata","tropicalis", "remanei", "latens", "tribulationis", "briggsae", "nigoni")

  #HalLiftover command
  list_df <- system_linux(species)

  #Get data frames from PSL files obtained with halLiftover command
  elegans_becei <- pslToDataFrame(list_df$becei,"becei")
  elegans_bovis <- pslToDataFrame(list_df$bovis,"bovis")
  elegans_panamensis <- pslToDataFrame(list_df$panamensis,"panamensis")
  elegans_inopinata <- pslToDataFrame(list_df$inopinata,"inopinata")
  elegans_tropicalis <- pslToDataFrame(list_df$tropicalis,"tropicalis")
  elegans_remanei <- pslToDataFrame(list_df$remanei,"remanei")
  elegans_latens <- pslToDataFrame(list_df$latens,"latens")
  elegans_tribulationis <- pslToDataFrame(list_df$tribulationis,"tribulationis")
  elegans_briggsae <- pslToDataFrame(list_df$briggsae,"briggsae")
  elegans_nigoni <- pslToDataFrame(list_df$nigoni,"nigoni")

  #Build the aligned regions (blocks)
  sorted_bovis <- sort_haliftover_data(elegans_bovis)
  sorted_becei <- sort_haliftover_data(elegans_becei)
  sorted_panamensis <- sort_haliftover_data(elegans_panamensis)
  sorted_inopinata <- sort_haliftover_data(elegans_inopinata)
  sorted_tropicalis <-  sort_haliftover_data(elegans_tropicalis)
  sorted_remanei <-  sort_haliftover_data(elegans_remanei)
  sorted_latens <-  sort_haliftover_data(elegans_latens)
  sorted_tribulationis <- sort_haliftover_data(elegans_tribulationis)
  sorted_briggsae <-  sort_haliftover_data(elegans_briggsae)
  sorted_nigoni <-  sort_haliftover_data(elegans_nigoni)
  sorted_elegans <- region %>% select(seqnames, start, end, length)

  ## Step 2 : Find pairwise alignment between phylogenetically adjacent species using the homologous fragments (halLiftover)

  #Remove precedent stored files
  system(paste0("rm ", folder_ubuntu, "sorted_*"))
  system(paste0("rm bovis_becei.psl becei_panamensis.psl panamensis_inopinata.psl inopinata_elegans.psl elegans_tropicalis.psl tropicalis_remanei.psl remanei_latens.psl latens_tribulationis.psl tribulationis_briggsae.psl briggsae_nigoni.psl"))

  #Export reconfigured files
  export_fct(sorted_bovis, "bovis")
  export_fct(sorted_becei, "becei")
  export_fct(sorted_panamensis, "panamensis")
  export_fct(sorted_inopinata, "inopinata")
  export_fct(sorted_tropicalis, "tropicalis")
  export_fct(sorted_remanei, "remanei")
  export_fct(sorted_latens, "latens")
  export_fct(sorted_tribulationis, "tribulationis")
  export_fct(sorted_briggsae, "briggsae")
  export_fct(sorted_nigoni, "nigoni")
  export_fct(sorted_elegans, "elegans")

  #Species names list
  species <- c("bovis", "becei", "panamensis", "inopinata","elegans","tropicalis", "remanei", "latens", "tribulationis", "briggsae", "nigoni")

  #HalLiftover command
  list_df <- system_linux_2(species)

  #Create raw data frames from PSL files
  bovis_becei <- pslToDataFrame2(list_df,"bovis_becei")
  becei_panamensis <- pslToDataFrame2(list_df,"becei_panamensis")
  panamensis_inopinata <- pslToDataFrame2(list_df,"panamensis_inopinata")
  inopinata_elegans <- pslToDataFrame2(list_df,"inopinata_elegans")
  elegans_tropicalis <- pslToDataFrame2(list_df,"elegans_tropicalis")
  tropicalis_remanei <- pslToDataFrame2(list_df,"tropicalis_remanei")
  remanei_latens <- pslToDataFrame2(list_df,"remanei_latens")
  latens_tribulationis <- pslToDataFrame2(list_df,"latens_tribulationis")
  tribulationis_briggsae <- pslToDataFrame2(list_df,"tribulationis_briggsae")
  briggsae_nigoni <- pslToDataFrame2(list_df,"briggsae_nigoni")

  #Links_fct : select only interesting columns : start, end, seq_id, start2, end2,  seq_id2
  links_bovis_becei <- links_fct(bovis_becei)
  links_becei_panamensis <- links_fct(becei_panamensis)
  links_panamensis_inopinata <- links_fct(panamensis_inopinata)
  links_inopinata_elegans <- links_fct(inopinata_elegans)
  links_elegans_tropicalis <- links_fct(elegans_tropicalis)
  links_tropicalis_remanei <- links_fct(tropicalis_remanei)
  links_remanei_latens <- links_fct(remanei_latens)
  links_latens_tribulationis <- links_fct(latens_tribulationis)
  links_tribulationis_briggsae <- links_fct(tribulationis_briggsae)
  links_briggsae_nigoni <- links_fct(briggsae_nigoni)

  ## Step 3 : Assemble adjacent fragments according to a gap value

  #Sticth function
  stitch_bovis <- stitch(sorted_bovis, region$gap)
  stitch_becei <- stitch(sorted_becei, region$gap)
  stitch_panamensis <- stitch(sorted_panamensis, region$gap)
  stitch_inopinata <- stitch(sorted_inopinata, region$gap)
  stitch_tropicalis <- stitch(sorted_tropicalis, region$gap)
  stitch_remanei <- stitch(sorted_remanei, region$gap)
  stitch_latens <- stitch(sorted_latens, region$gap)
  stitch_tribulationis <- stitch(sorted_tribulationis, region$gap)
  stitch_briggsae <- stitch(sorted_briggsae, region$gap)
  stitch_nigoni <- stitch(sorted_nigoni, region$gap)

  ## Step 4 : Filter assembled homologous fragments through a filtering percentage of the sequence of inquiry length

  #Filter function
  filtered_bovis <- filtering(stitch_bovis, region)
  filtered_becei <- filtering(stitch_becei, region)
  filtered_panamensis <- filtering(stitch_panamensis, region)
  filtered_inopinata <- filtering(stitch_inopinata, region)
  filtered_tropicalis <- filtering(stitch_tropicalis, region)
  filtered_remanei <- filtering(stitch_remanei, region)
  filtered_latens <- filtering(stitch_latens, region)
  filtered_tribulationis <- filtering(stitch_tribulationis, region)
  filtered_briggsae <- filtering(stitch_briggsae, region)
  filtered_nigoni <- filtering(stitch_nigoni, region)

  ## Step 5 : Separate the longest homologous fragments from the others

  #Final function : takes max region
  final_bovis <- final(filtered_bovis)
  final_becei <- final(filtered_becei)
  final_panamensis <- final(filtered_panamensis)
  final_inopinata <- final(filtered_inopinata)
  final_tropicalis <- final(filtered_tropicalis)
  final_remanei <- final(filtered_remanei)
  final_latens <- final(filtered_latens)
  final_tribulationis <- final(filtered_tribulationis)
  final_briggsae <- final(filtered_briggsae)
  final_nigoni <- final(filtered_nigoni)

  #For elegans : select the correct columns
  final_elegans  <- region %>% select(seqnames, start, end, length)

  #Other regions function : stores the other reads into data frames
  extras_fragments_bovis <- extras_fragments(filtered_bovis, final_bovis)
  extras_fragments_becei <- extras_fragments(filtered_becei, final_becei)
  extras_fragments_panamensis <- extras_fragments(filtered_panamensis, final_panamensis)
  extras_fragments_inopinata <- extras_fragments(filtered_inopinata, final_inopinata)
  extras_fragments_tropicalis <- extras_fragments(filtered_tropicalis, final_tropicalis)
  extras_fragments_remanei <- extras_fragments(filtered_remanei, final_remanei)
  extras_fragments_latens <- extras_fragments(filtered_latens, final_latens)
  extras_fragments_tribulationis <- extras_fragments(filtered_tribulationis, final_tribulationis)
  extras_fragments_briggsae <- extras_fragments(filtered_briggsae, final_briggsae)
  extras_fragments_nigoni <- extras_fragments(filtered_nigoni, final_nigoni)

  ## Step 6 : Find genes/piece of genes in the aligned regions (subsetByIntersect)

  #Genes function : uses subsetByIntersect to find overlapping genes (only coordinates)
  genes_bovis <- genes_fct(bovis, final_bovis)
  genes_becei <- genes_fct(becei, final_becei)
  genes_panamensis <- genes_fct(panamensis, final_panamensis)
  genes_inopinata <- genes_fct(inopinata, final_inopinata)
  genes_elegans <- genes_fct(elegans, region)
  genes_tropicalis <- genes_fct(tropicalis, final_tropicalis)
  genes_remanei <- genes_fct(remanei, final_remanei)
  genes_latens <- genes_fct(latens, final_latens)
  genes_tribulationis <- genes_fct(tribulationis, final_tribulationis)
  genes_briggsae <- genes_fct(briggsae, final_briggsae)
  genes_nigoni <- genes_fct(nigoni, final_nigoni)

  ## Step 7 : Add metadata to genes/piece of genes in the aligned regions (subsetByIntersect) (Orthogroup)

  #Find metadata
  genes_bovis <- metadata_fct(bovis, genes_bovis)
  genes_becei <- metadata_fct(becei, genes_becei)
  genes_panamensis <- metadata_fct(panamensis, genes_panamensis)
  genes_inopinata<- metadata_fct(inopinata, genes_inopinata)
  genes_elegans <- metadata_fct(elegans, genes_elegans)
  genes_tropicalis<- metadata_fct(tropicalis, genes_tropicalis)
  genes_remanei <- metadata_fct(remanei, genes_remanei)
  genes_latens <- metadata_fct(latens, genes_latens)
  genes_tribulationis <- metadata_fct(tribulationis, genes_tribulationis)
  genes_briggsae <- metadata_fct(briggsae, genes_briggsae)
  genes_nigoni <- metadata_fct(nigoni, genes_nigoni)

  ## Step 8 : Check if there is some "hidden genes" inside the intersected genes found

  genes_elegans <- check_overlapping(elegans, genes_elegans)
  genes_becei <- check_overlapping(becei, genes_becei)
  genes_bovis <- check_overlapping(bovis, genes_bovis)
  genes_panamensis <- check_overlapping(panamensis, genes_panamensis)
  genes_inopinata <- check_overlapping(inopinata, genes_inopinata)
  genes_tropicalis <- check_overlapping(tropicalis, genes_tropicalis)
  genes_remanei <- check_overlapping(remanei, genes_remanei)
  genes_latens <- check_overlapping(latens, genes_latens)
  genes_tribulationis <- check_overlapping(tribulationis, genes_tribulationis)
  genes_briggsae <- check_overlapping(briggsae, genes_briggsae)
  genes_nigoni <- check_overlapping(nigoni, genes_nigoni)

  ## Step 9 : Find genes/piece of genes in the removed regions too

  #Genes function : uses subsetByIntersect to find overlapping genes (filter rows for now)
  extras_genes_bovis <- genes_fct(bovis, extras_fragments_bovis)
  extras_genes_becei <- genes_fct(becei, extras_fragments_becei)
  extras_genes_panamensis <- genes_fct(panamensis, extras_fragments_panamensis)
  extras_genes_inopinata <- genes_fct(inopinata, extras_fragments_inopinata)
  extras_genes_tropicalis <- genes_fct(tropicalis, extras_fragments_tropicalis)
  extras_genes_remanei <- genes_fct(remanei, extras_fragments_remanei)
  extras_genes_latens <- genes_fct(latens,extras_fragments_latens)
  extras_genes_tribulationis <- genes_fct(tribulationis,extras_fragments_tribulationis)
  extras_genes_briggsae <- genes_fct(briggsae, extras_fragments_briggsae)
  extras_genes_nigoni <- genes_fct(nigoni, extras_fragments_nigoni)

  #Find metadata
  extras_genes_bovis <- metadata_fct(bovis, extras_genes_bovis)
  extras_genes_becei <- metadata_fct(becei, extras_genes_becei)
  extras_genes_panamensis <- metadata_fct(panamensis, extras_genes_panamensis)
  extras_genes_inopinata <- metadata_fct(inopinata, extras_genes_inopinata)
  extras_genes_tropicalis <- metadata_fct(tropicalis, extras_genes_tropicalis)
  extras_genes_remanei <- metadata_fct(remanei, extras_genes_remanei)
  extras_genes_latens <- metadata_fct(latens, extras_genes_latens)
  extras_genes_tribulationis <- metadata_fct(tribulationis, extras_genes_tribulationis)
  extras_genes_briggsae <- metadata_fct(briggsae, extras_genes_briggsae)
  extras_genes_nigoni <- metadata_fct(nigoni, extras_genes_nigoni)

  ## Step 10 : control of repetition of genes in top and bottom tables and remove them from bottom tables

  retrieved_genes_bovis <- retrieve_same_genes(genes_bovis, extras_genes_bovis)
  retrieved_genes_becei <- retrieve_same_genes(genes_becei, extras_genes_becei)
  retrieved_genes_panamensis <- retrieve_same_genes(genes_panamensis, extras_genes_panamensis)
  retrieved_genes_inopinata <- retrieve_same_genes(genes_inopinata, extras_genes_inopinata)
  retrieved_genes_tropicalis <- retrieve_same_genes(genes_tropicalis, extras_genes_tropicalis)
  retrieved_genes_remanei <- retrieve_same_genes(genes_remanei, extras_genes_remanei)
  retrieved_genes_latens <- retrieve_same_genes(genes_latens, extras_genes_latens)
  retrieved_genes_tribulationis <- retrieve_same_genes( genes_tribulationis, extras_genes_tribulationis)
  retrieved_genes_briggsae <- retrieve_same_genes(genes_briggsae, extras_genes_briggsae)
  retrieved_genes_nigoni <- retrieve_same_genes(genes_nigoni, extras_genes_nigoni)

  extras_genes_bovis <- control(genes_bovis, extras_genes_bovis)
  extras_genes_becei <- control(genes_becei, extras_genes_becei)
  extras_genes_panamensis <- control(genes_panamensis, extras_genes_panamensis)
  extras_genes_inopinata <- control(genes_inopinata, extras_genes_inopinata)
  extras_genes_tropicalis <- control(genes_tropicalis, extras_genes_tropicalis)
  extras_genes_remanei <- control(genes_remanei, extras_genes_remanei)
  extras_genes_latens <- control(genes_latens, extras_genes_latens)
  extras_genes_tribulationis <- control( genes_tribulationis, extras_genes_tribulationis)
  extras_genes_briggsae <- control(genes_briggsae, extras_genes_briggsae)
  extras_genes_nigoni <- control(genes_nigoni, extras_genes_nigoni)

  #Add seq_id in retrieved genes
  retrieved_seqs_bovis <- transform_retrieved(retrieved_genes_bovis, "bovis")
  retrieved_seqs_becei <- transform_retrieved(retrieved_genes_becei, "becei")
  retrieved_seqs_panamensis <- transform_retrieved(retrieved_genes_panamensis, "panamensis")
  retrieved_seqs_inopinata <- transform_retrieved(retrieved_genes_inopinata, "inopinata")
  retrieved_seqs_tropicalis <- transform_retrieved(retrieved_genes_tropicalis, "tropicalis")
  retrieved_seqs_remanei <- transform_retrieved(retrieved_genes_remanei, "remanei")
  retrieved_seqs_latens <- transform_retrieved(retrieved_genes_latens, "latens")
  retrieved_seqs_tribulationis <- transform_retrieved(retrieved_genes_tribulationis, "tribulationis")
  retrieved_seqs_briggsae <- transform_retrieved(retrieved_genes_briggsae, "briggsae")
  retrieved_seqs_nigoni <- transform_retrieved(retrieved_genes_nigoni, "nigoni")

  ## Step 11 : Create aligned seqeunces, aligned genes and links tables for gggenomes

  #Add seq_id in genes
  genes_bovis <- add_seq_id(genes_bovis, "bovis")
  genes_becei <- add_seq_id(genes_becei, "becei")
  genes_panamensis <- add_seq_id(genes_panamensis, "panamensis")
  genes_inopinata <- add_seq_id(genes_inopinata, "inopinata")
  genes_elegans <- add_seq_id(genes_elegans, "elegans")
  genes_tropicalis <- add_seq_id(genes_tropicalis, "tropicalis")
  genes_remanei <- add_seq_id(genes_remanei, "remanei")
  genes_latens <- add_seq_id(genes_latens, "latens")
  genes_tribulationis <- add_seq_id(genes_tribulationis, "tribulationis")
  genes_briggsae <- add_seq_id(genes_briggsae, "briggsae")
  genes_nigoni <- add_seq_id(genes_nigoni, "nigoni")

  #Add seq_id in genes noise
  extras_genes_bovis <- add_seq_id_extras(extras_genes_bovis, "bovis")
  extras_genes_becei <- add_seq_id_extras(extras_genes_becei, "becei")
  extras_genes_panamensis <- add_seq_id_extras(extras_genes_panamensis, "panamensis")
  extras_genes_inopinata <- add_seq_id_extras(extras_genes_inopinata, "inopinata")
  extras_genes_tropicalis <- add_seq_id_extras(extras_genes_tropicalis, "tropicalis")
  extras_genes_remanei <- add_seq_id_extras(extras_genes_remanei, "remanei")
  extras_genes_latens <- add_seq_id_extras(extras_genes_latens, "latens")
  extras_genes_tribulationis <- add_seq_id_extras(extras_genes_tribulationis, "tribulationis")
  extras_genes_briggsae <- add_seq_id_extras(extras_genes_briggsae, "briggsae")
  extras_genes_nigoni <- add_seq_id_extras(extras_genes_nigoni, "nigoni")

  #Add seq_id in genes
  retrieved_genes_bovis <- add_seq_id(retrieved_genes_bovis, "bovis")
  retrieved_genes_becei <- add_seq_id(retrieved_genes_becei, "becei")
  retrieved_genes_panamensis <- add_seq_id(retrieved_genes_panamensis, "panamensis")
  retrieved_genes_inopinata <- add_seq_id(retrieved_genes_inopinata, "inopinata")
  retrieved_genes_tropicalis <- add_seq_id(retrieved_genes_tropicalis, "tropicalis")
  retrieved_genes_remanei <- add_seq_id(retrieved_genes_remanei, "remanei")
  retrieved_genes_latens <- add_seq_id(retrieved_genes_latens, "latens")
  retrieved_genes_tribulationis <- add_seq_id(retrieved_genes_tribulationis, "tribulationis")
  retrieved_genes_briggsae <- add_seq_id(retrieved_genes_briggsae, "briggsae")
  retrieved_genes_nigoni <- add_seq_id(retrieved_genes_nigoni, "nigoni")

  #Create links file
  links <- rbind(links_bovis_becei, links_becei_panamensis, links_panamensis_inopinata, links_inopinata_elegans,links_elegans_tropicalis, links_tropicalis_remanei,links_remanei_latens,  links_latens_tribulationis,links_tribulationis_briggsae, links_briggsae_nigoni)

  #Get the strand and strand2 columns
  if(!(is.null(links))){
    links <- links %>% separate(strand, into = c("strand", "strand2"), sep = "(?<=[+|-])(?=[+|-])")
  }else{
    links <- NULL
  }

  #Create genes file
  genes <- rbind(genes_bovis,genes_becei,genes_panamensis,genes_inopinata,genes_elegans,genes_tropicalis,genes_remanei,genes_latens,genes_tribulationis,genes_briggsae,genes_nigoni)

  extras_genes <- rbind(extras_genes_bovis,extras_genes_becei,extras_genes_panamensis,extras_genes_inopinata,extras_genes_tropicalis,extras_genes_remanei,extras_genes_latens,extras_genes_tribulationis,extras_genes_briggsae,extras_genes_nigoni)

  retrieved_genes <- rbind(retrieved_genes_bovis, retrieved_genes_becei, retrieved_genes_panamensis, retrieved_genes_inopinata, retrieved_genes_tropicalis, retrieved_genes_remanei, retrieved_genes_latens, retrieved_genes_tribulationis, retrieved_genes_briggsae, retrieved_genes_nigoni)

  #Combined genes and genes noise
  genes <- rbind(genes, retrieved_genes, extras_genes)

  #Generate seqs files function : genes present -> length_seqs == length_genes else -> length_seqs == length_max_stitched_regions
  seqs_bovis <- seqs_fct(genes_bovis, final_bovis, "bovis")
  seqs_becei <- seqs_fct(genes_becei, final_becei, "becei")
  seqs_panamensis <- seqs_fct(genes_panamensis, final_panamensis, "panamensis")
  seqs_inopinata <- seqs_fct(genes_inopinata, final_inopinata, "inopinata")
  seqs_elegans <- seqs_fct(genes_elegans, final_elegans, "elegans")
  seqs_tropicalis <- seqs_fct(genes_tropicalis, final_tropicalis, "tropicalis")
  seqs_remanei <- seqs_fct(genes_remanei, final_remanei, "remanei")
  seqs_latens <- seqs_fct(genes_latens, final_latens, "latens")
  seqs_tribulationis <- seqs_fct(genes_tribulationis, final_tribulationis, "tribulationis")
  seqs_briggsae <- seqs_fct(genes_briggsae, final_briggsae, "briggsae")
  seqs_nigoni <- seqs_fct(genes_nigoni, final_nigoni, "nigoni")

  #Create seqs file
  seqs <- rbind(seqs_bovis, seqs_becei, seqs_panamensis, seqs_inopinata, seqs_elegans, seqs_tropicalis, seqs_remanei, seqs_latens, seqs_tribulationis, seqs_briggsae, seqs_nigoni)
  seqnames_order <- unique(seqs$bin_id)

  retrieved_seqs <- rbind(retrieved_seqs_bovis , retrieved_seqs_becei, retrieved_seqs_panamensis, retrieved_seqs_inopinata, retrieved_seqs_tropicalis, retrieved_seqs_remanei, retrieved_seqs_latens, retrieved_seqs_tribulationis, retrieved_seqs_briggsae, retrieved_seqs_nigoni)

  seqs <- rbind(seqs, retrieved_seqs)

  seqs <- seqs %>%
    arrange(match(bin_id, seqnames_order), start)

  #### Sequences that will appear in aligned sequences table
  extras_seqs_bovis <- extras_seqs_fct(extras_genes_bovis, extras_fragments_bovis, "bovis")
  extras_seqs_becei <- extras_seqs_fct(extras_genes_becei, extras_fragments_becei, "becei")
  extras_seqs_panamensis <- extras_seqs_fct(extras_genes_panamensis, extras_fragments_panamensis, "panamensis")
  extras_seqs_inopinata <- extras_seqs_fct(extras_genes_inopinata, extras_fragments_inopinata, "inopinata")
  extras_seqs_tropicalis <- extras_seqs_fct(extras_genes_tropicalis, extras_fragments_tropicalis, "tropicalis")
  extras_seqs_remanei <- extras_seqs_fct(extras_genes_remanei, extras_fragments_remanei, "remanei")
  extras_seqs_latens <- extras_seqs_fct(extras_genes_latens, extras_fragments_latens, "latens")
  extras_seqs_tribulationis <- extras_seqs_fct(extras_genes_tribulationis, extras_fragments_tribulationis, "tribulationis")
  extras_seqs_briggsae <- extras_seqs_fct(extras_genes_briggsae, extras_fragments_briggsae, "briggsae")
  extras_seqs_nigoni <- extras_seqs_fct(extras_genes_nigoni, extras_fragments_nigoni, "nigoni")

  extras_seqs <- rbind(extras_seqs_bovis,extras_seqs_becei,extras_seqs_panamensis, extras_seqs_inopinata, extras_seqs_tropicalis, extras_seqs_remanei, extras_seqs_latens,  extras_seqs_tribulationis,  extras_seqs_briggsae, extras_seqs_nigoni)

  aligned_sequences <- rbind(seqs, extras_seqs)

  #Create seqs noise
  if(!(is.null(extras_genes))){
    extras_seqs <- extras_genes %>% select(seqnames, start, end, length, seq_id)
    extras_seqs <- extras_seqs %>% mutate(bin_id = str_c(str_c("C. ", str_extract(seq_id, "[:alpha:]+")), "(filtered)", sep = " "))

  }else{
    extras_seqs <-  NULL
  }

  #Combined seqs and seqs noise
  seqs <- rbind(seqs, extras_seqs)

  #Put the columns in order
  if(!is.null(genes)){
    genes <- genes %>% rename(types = "type") %>% select(seq_id, seqnames, start, end, length, strand, types, gene_id, gene_biotype, Orthogroup)
  }
  seqs <- seqs %>% select(bin_id, seq_id, seqnames, start, end, length)

  return(list(seqs = seqs, genes = genes, links = links, aligned_sequences = aligned_sequences))

}
