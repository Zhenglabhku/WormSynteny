library(ggplot2)
library(ggridges)
library(ggthemes)
library(stringr)
library(treeio)
library(ggtree)
library(tidyverse)
library(reshape2)
library(GenomicRanges)
library(wig)
library(readxl)

AlignmentTree = read.tree(text = "(Caenorhabditisbovis:0.204595,((Caenorhabditispanamensis:0.08681,Caenorhabditis becei:0.0826785)0.910146:0.0677077,((Caenorhabditis inopinata:0.139818,Caenorhabditis elegans:0.102282)0.326161:0.025392,(Caenorhabditis tropicalis:0.148186,((Caenorhabditis tribulationis:0.0941337,(Caenorhabditis nigoni:0.0418542,Caenorhabditis briggsae:0.0301551)0.852374:0.0721951)0.540949:0.029186,(Caenorhabditis remanei:0.0354268,Caenorhabditis latens:0.0784396)0.839071:0.0760313)0.352374:0.0202954)0.364371:0.0192056)0.552947:0.0417637)1:0.204595)Anc00;")

#' Mutation summarized by running following command in linux
#' halSummarizeMutations /lustre1/g/sbs_cgz/cactus_synteny/evolver_Caenorhabditis.hal >halSummarizeMutations.txt


#' Fig01.A species tree
Fig01.A = ggtree(AlignmentTree) + geom_tiplab(size = 5, color = "black")+ xlim(0, 1)
ggsave(file = "tree_havelength.pdf", width = 10, height = 5)



#' Fig01.B duplication
#' duplication statistics bed 
#' the file (C.all.par.bed) is concatenate all bed file from halBranchMutations function.

bed_col_name = c("Chr", "Start", "End", "Type", "Parent", "Child")
All_par = read.table(file = "./raw_data/04.analysis/C.all.par.bed",    
                     skip = "#", col.names = bed_col_name) %>% 
  filter(Type == "U") %>% 
  mutate(Length = abs(Start - End) + 1) %>%
  filter(Length >= 100 & Length<= 2000)

branch_elegans_par = read.table(file = "./raw_data/04.analysis/caenorhabditis_elegans.par.bed", 
                                skip = "#", col.names = bed_col_name)
branch_elegans_par = branch_elegans_par %>% filter(Type == "U") %>% mutate(length = abs(Start - End) + 1)
branch_elegans_par_frequence = All_par %>% mutate(interval = floor(Length/20)) %>% group_by(Child, interval, Parent) %>%
  summarise(n = n()) %>% mutate(n2 = log10(n)) %>% mutate(bin_position = interval*20)
branch_elegans_par_frequence = table()

part_branch =branch_elegans_par_frequence %>% filter(Parent %in% c("0.326161", "0.364371", "0.852374", "0.839071")) %>%
  mutate(Clade = case_when(Parent == "0.326161" ~ "Clade1",
                           Parent == "0.852374" ~ "Clade2",
                           Parent %in% c("0.364371", "0.839071") ~ "Clade3")) %>% filter(Child != "0.352374") %>%
  mutate(SexDetermination = if_else(Child %in% c("caenorhabditis_elegans", "caenorhabditis_briggsae", "caenorhabditis_tropicalis"), "hermaphroditic", "male female")) %>%
  mutate(Sexfill = if_else(Child %in% c("caenorhabditis_elegans", "caenorhabditis_briggsae", "caenorhabditis_tropicalis"),"#FFFED3", "#973131")) %>%
  mutate(Sexfill2 = if_else(Child == "caenorhabditis_latens", "#E0A75E", Sexfill))

part_branch$Child = factor(part_branch$Child, levels = c("caenorhabditis_inopinata", "caenorhabditis_elegans",
                                                         "caenorhabditis_nigoni","caenorhabditis_briggsae",
                                                         "caenorhabditis_remanei","caenorhabditis_latens",
                                                         "caenorhabditis_tropicalis"))
Fig01.B = ggplot(data = part_branch) + geom_area(aes(x = bin_position, y = n2, fill = Sexfill2), position = "identity", alpha = 0.8) + 
  facet_wrap(facets = "Clade", nrow = 1) + scale_fill_identity()+ theme_few(base_size = 16, base_family = "Arial")+ labs(x = "Position", y = "Log10(count)")

#' t-test to compare the duplication size.
dcast_bin_table = dcast(part_branch[ , c("Child", "interval", "n")], formula = interval ~ Child, fill =0)
Clade1 = t.test(x = dcast_bin_table$caenorhabditis_elegans, dcast_bin_table$caenorhabditis_inopinata, paired = T, alternative = "less")
Clade2 = t.test(x = dcast_bin_table$caenorhabditis_briggsae, dcast_bin_table$caenorhabditis_nigoni, paired = T, alternative = "less")
Clade3 = t.test(x = dcast_bin_table$caenorhabditis_tropicalis, dcast_bin_table$caenorhabditis_latens, paired = T, alternative = "less")
Clade3_2 = t.test(x = dcast_bin_table$caenorhabditis_tropicalis, dcast_bin_table$caenorhabditis_remanei, paired = T, alternative = "less")


#' Fig01.C
#' Bin size is 20000bp
#' smooth function is deafult by ggplot2.

C_elegans_wig = import_wig("./raw_data/wig/halAlignmentDepth_caenorhabditis_elegans.wig")
C_elegans_wig_window = C_elegans_wig %>% filter(chr != "MtDNA") %>% mutate(pos2 = ceiling(pos/20000)) %>% 
  group_by(chr, pos2) %>% mutate(overall_depth = sum(val), pos3 = mean(pos)) %>% 
  select(chr, overall_depth, pos3) %>% distinct() 
C_elegans_wig_window_Chr1 = C_elegans_wig_window %>% filter(chr == "I")

C_elegans_wig_lm = lm(overall_depth ~ pos3 + I(pos3^2), data = C_elegans_wig_window_Chr1)
ChrI_new_pos = seq(min(C_elegans_wig_window_Chr1$pos3), max(C_elegans_wig_window_Chr1$pos3), 10)
C_elegans_wig_window_Chr1_predict = data.frame(predict(C_elegans_wig_lm, newdata = data.frame(pos3 = ChrI_new_pos), 
                                                       interval = "confidence"), pos3 = ChrI_new_pos)

Fig01.C = ggplot(data = C_elegans_wig_window, aes(x = pos3, y=overall_depth)) +
  geom_point(size = 0.2)  + geom_smooth(span = 0.01) + facet_wrap(facets = "chr", nrow = 1, ncol = 7,scales = "free_x")  + 
  scale_x_continuous(labels = function(bin_position)
  {x= format(bin_position, scientific = FALSE, trim = TRUE, nsmall = 0)
  y= str_extract(x, ".+(?=000000)")
  y[which(is.na(c(NA,5,6)))] = 0
  return(y)}) + 
  scale_y_continuous(labels = function(bin_position)
  {x= format(bin_position, scientific = FALSE, trim = TRUE, nsmall = 0)
  y= str_extract(x, ".+(?=00000)")
  y[which(is.na(c(NA,5,6)))] = 0
  return(y)}) + 
  theme(panel.spacing.y = unit(0, "mm")) + 
  theme_few(base_size = 16, base_family = "Arial") + labs(x = "Position", y = "Alignment Depth 10e5")

ggsave(Fig01C, file = "Fig1.B.5.pdf", width = 10, height = 5) #' use this one

#' average alignment depth
#' data for Fig.01.D

becei = import_wig("./raw_data/wig/halAlignmentDepth_caenorhabditis_becei.wig") %>% summarise(mean = mean(val))
bovis= import_wig("./raw_data/wig/halAlignmentDepth_caenorhabditis_bovis.wig") %>% summarise(mean = mean(val))
briggsae = import_wig("./raw_data/wig/halAlignmentDepth_caenorhabditis_briggsae.wig") %>% summarise(mean = mean(val))
elegans = import_wig("./raw_data/wig/halAlignmentDepth_caenorhabditis_elegans.wig") %>% summarise(mean = mean(val))
inopinata = import_wig("./raw_data/wig/halAlignmentDepth_caenorhabditis_inopinata.wig") %>% summarise(mean = mean(val))
latens = import_wig("./raw_data/wig/halAlignmentDepth_caenorhabditis_latens.wig") %>% summarise(mean = mean(val))
nigoni = import_wig("./raw_data/wig/halAlignmentDepth_caenorhabditis_nigoni.wig") %>% summarise(mean = mean(val))
panamensis = import_wig("./raw_data/wig/halAlignmentDepth_caenorhabditis_panamensis.wig") %>% summarise(mean = mean(val))
remanei = import_wig("./raw_data/wig/halAlignmentDepth_caenorhabditis_remanei.wig") %>% summarise(mean = mean(val)) 
tribulationis = import_wig("./raw_data/wig/halAlignmentDepth_caenorhabditis_tribulationis.wig") %>% summarise(mean = mean(val))
tropicalis = import_wig("./raw_data/wig/halAlignmentDepth_caenorhabditis_tropicalis.wig") %>% summarise(mean = mean(val))
Alignment_Mean = mean(c(as.numeric(becei), 
                        as.numeric(bovis),
                        as.numeric(briggsae),
                        as.numeric(elegans),
                        as.numeric(inopinata),
                        as.numeric(latens),
                        as.numeric(nigoni),
                        as.numeric(panamensis),
                        as.numeric(remanei),
                        as.numeric(tribulationis),
                        as.numeric(tropicalis)
                        ))



#' Fig01.E
#' .ad file processed by a custom script add mean alignment depth for each pair of coordinates.

AD_gtf = read.table(file = "./raw_data/C.elegans.gtf.ad", sep = "\t", comment.char = "#",
                    fill = T, col.names = c("Chr", "source", "feature", "Start", "End", "score", "strand", "frame", "attribute", "AD")) %>% 
  mutate(WB_ID = str_extract(attribute, "WBGene[:digit:]+"))
AD_intron = read.table(file = "./raw_data/intron.bed.ad", col.names = c("Chr", "Start", "End", "AD")) %>% mutate(attribute = "intron")
AD_intergenic_region = read.table(file = "./raw_data/intergenic.bed.ad", col.names = c("Chr", "Start", "End", "AD")) %>% mutate(attribute = "intergenic")
AD_exon_bed = AD_gtf %>% filter(feature == c("exon", "three_prime_utr", "five_prime_utr")) %>% mutate(attribute = feature) %>%
  select(Chr, Start, End, AD, attribute) 
cat = rbind(AD_exon_bed, AD_intron, AD_intergenic_region) %>% mutate(color = case_when(attribute == "exon" ~"#808836",
                                                                                       attribute == "five_prime_utr" ~ "#F9D689",
                                                                                       attribute == "three_prime_utr" ~ "#E0A75E",
                                                                                       attribute == "intron" ~ "#E65C19",
                                                                                       attribute == "intergenic" ~ "#B51B50"
                                                                                       ))
cat$attribute = factor(cat$attribute, levels = c("exon", "five_prime_utr", "three_prime_utr", "intron", "intergenic"))
Fig01.E = ggplot(data =cat) + geom_boxplot(aes(y = AD, x = attribute, fill = attribute), width = 0.70, size = 0.4)  + 
  theme_few(base_size = 16, base_family = "Arial")+ labs(x = "Alignment Depth", y = "Density") + theme(legend.position="right", legend.title = element_blank()) + ylim(c(-2,22)) + 
  scale_fill_manual(values = c("#808836", "#F9D689", "#E0A75E", "#FD9B63", "#BC5A94"))

#' Fig01.F
#' duplication table from the paper https://www.cell.com/cell-genomics/pdf/S2666-979X(23)00311-7.pdf

duplication_table = read_xlsx("~/Downloads/1-s2.0-S2666979X23003117-mmc5.xlsx", skip = 1, col_names = T) %>% select(1, 7) %>% rename(categories = Type...7)
AD_gtf_new = inner_join(AD_gtf, duplication_table, by = join_by(WB_ID == WBGene)) %>% 
  filter(feature == "gene") %>% 
  mutate(categories2 = if_else(categories %in% c("Recently duplicated genes", "Cluster 1 genes", "Cluster 2 genes", "Cluster 3 genes"), "Recently duplicated genes", categories))
AD_gtf_new$categories2 = factor(AD_gtf_new$categories2, levels = c("Single copy genes", "Ancient genes", "Old genes", 
                                                                 "Recently duplicated genes", "Other genes")) 
Fig01.F = ggplot() + geom_boxplot(data =AD_gtf_new, aes(y = AD, x = categories2, fill = categories2)) + 
  theme_few(base_size = 16, base_family = "Arial")+ labs(x = "Alignment Depth", y = "Density") +
  theme(legend.position="right", legend.title = element_blank()) + ylim(c(-2,22)) + 
  scale_fill_manual(values = c("#808836", "#F9D689", "#E0A75E", "#FD9B63", "#BC5A94"))






