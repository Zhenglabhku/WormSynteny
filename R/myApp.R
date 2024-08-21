#' myApp function
#'
#' Launch the WormSynteny app
#'
#' @return The WormSynteny app
#' @export
#'
#' @importFrom dplyr %>%
#' @import shiny
#' @import shinyjs
#' @import shinydashboard
#' @import shinyFeedback
#' @import shinycssloaders
#' @import flextable
#' @import readr
#' @import ggplot2
#' @import grDevices
#'
#' @examples
#' #...
myApp <- function() {

  resources <- system.file("app/www", package = "WormSyntenyPackage")
  addResourcePath("www", resources)

  purpose_of_the_app <- tags$html(
    tags$head(
      tags$meta(
        charset = "utf-8"
      ),
      tags$title(
        "Tutorials"
      )
    ),
    tags$body(
      tags$h1(
        "I-Introduction"
      ),
      tags$h2(
        "I.I-Purpose of the App"
      ),
      tags$p(
        "We built the Wormsynteny App to visualize the synteny or genomic conservation among eleven nematode Caenorhabditis species. Our previous study identified orthologous genes in the eleven species and assigned them into orthogroups based on protein sequence similarities (Ma, Lau, and Zheng, Cell Genomics, 2024). Those analyses, however, did not consider synteny. Thus, one of the purposes of this App is to integrate the orthology and synteny information by mapping the genomic position of the orthologs across species to assess the conservation of the genomic blocks they are located in. In the alignment plot, we provide the orthogroup information for protein sequence conservation and the synteny information by the links among aligned regions. Genomic alignment is done using Cactus, a reference-free whole-genome alignment program, and the output is visualized in an alignment plot and several downloadable tables."
      ),
      tags$p("The Wormsynteny App is built in a C. elegans-centric manner. It only takes C. elegans genes or genomic regions and aligns them with the other ten Caenorhabditis species to find syntenic regions. The App then annotates genes within the region and maps them to orthogroups. Although not possible in its current form, future updates may allow the inquiry using sequences from the other ten species. We also plan to include more nematode species in the App. Given that Wormbase does not have a function to visualize synteny across multiple species, Wormsynteny App may be useful for evolutionary studies in the worm community and in genome biology in general."
      )
    )
  )

  input_data <- tags$html(
    tags$head(
      tags$meta(
        charset = "utf-8"
      ),
      tags$title(
        "Tutorials"
      )
    ),
    tags$body(
      tags$h1(
        "I-Introduction"
      ),
      tags$h2(
        "I.II-Input data (Progressive Cactus output)"
      ),
      tags$p(
        "The following 11 genome assemblies and a guide tree (according to Ma et al., Cell Genomics, 2024; PMID: 38190105) are used as the input for Progressive Cactus (v2.2.0), which was developed by Armstrong et al., Nature 2020 (PMID: 33177663)."),
      tags$p(id = "not_justify",
             "The software was ran using a step-by-step protocol provided at",
             tags$a(
               href = "https://github.com/ComparativeGenomicsToolkit/cactus/blob/master/doc/progressive.md",
               "https://github.com/ComparativeGenomicsToolkit/cactus/blob/master/doc/progressive.md"
             )
      ),
      tags$div(class = "first-list",
               tags$p(
                 tags$em(
                   "Caenorhabditis becei"
                 ),
                 "(PRJEB28243)"
               ),
               tags$p(
                 tags$em(
                   "Caenorhabditis bovis"
                 ),
                 "(PRJEB34497)"
               ),
               tags$p(
                 tags$em(
                   "Caenorhabditis briggsae"
                 ),
                 "(PRJNA10731)"
               ),
               tags$p(
                 tags$em(
                   "Caenorhabditis elegans"
                 ),
                 "(PRJNA13758)"
               ),
               tags$p(
                 tags$em(
                   "Caenorhabditis inopinata"
                 ),
                 "(PRJDB5687)"
               ),
               tags$p(
                 tags$em(
                   "Caenorhabditis latens"
                 ),
                 "(PRJNA248912)"
               ),
               tags$p(
                 tags$em(
                   "Caenorhabditis nigoni"
                 ),
                 "(PRJNA384657)"
               ),
               tags$p(
                 tags$em(
                   "Caenorhabditis panamensis"
                 ),
                 "(PRJEB28259)"
               ),
               tags$p(
                 tags$em(
                   "Caenorhabditis remanei"
                 ),
                 "(PRJNA57750)"
               ),
               tags$p(
                 tags$em(
                   "Caenorhabditis tribulationis"
                 ),
                 "(PRJEB12608)"
               ),
               tags$p(
                 tags$em(
                   "Caenorhabditis tropicalis"
                 ),
                 "(PRJNA53597)"
               )
      ),
      tags$p(
        "The output file of Progressive Cactus is a HAL file, which is a graph-based representation of multi-genome alignment. The HAL file can be queried using the HAL toolkit (v2.2) (Hickey et al., Bioinformatics, 2013; PMID: 23505295) given a set of genomic coordinates. The WormSynteny App takes C. elegans genomic coordinates, searches the HAL file, and exports pairwise alignment information into PSL files using the halLiftover function in the HAL toolkit. Ten pairwise alignment PSL files are generated for the alignment between C. elegans and the other ten Caenorhabditis species; these PSL files are then fed into the pipeline to be assembled into long homologous fragments.
        The links between sister species are created by taking the coordinates of the fragments aligned to C. elegans query sequences as query sequences to search the HAL file. Ten additional pairwise alignment PSL files are thus generated storing the alignment between sister species."
      )
    )
  )

  pipeline_for_visualizing_the_alignment <- tags$html(
    tags$head(
      tags$meta(
        charset = "utf-8"
      ),
      tags$title(
        "Tutorials"
      )
    ),
    tags$body(
      tags$h1(
        "I-Introduction"
      ),
      tags$h2(
        "I.III-Pipeline for visualizing the alignment"
      ),
      tags$p(
        "To visualize the alignment, the Wormsynteny App uses a versatile graphics package for comparative genomics : gggenomes. It extends the popular R visualization package ggplot2 which makes it quicker to learn and allows us to map genes to orthogroups using different colors in one comprehensive and elegant plot."
      ),
      tags$p(
        "A gggenomes plot typically consists of three types of data tracks, each of which contains a single dataset :"),
      tags$ul(
        tags$li(class = "paragraph-list",
                "seqs: sequences such as contigs or chromosomes"
        ),
        tags$li(class = "paragraph-list",
                "feats or genes tracks: annotations of locations on sequences"
        ),
        tags$li(class = "paragraph-list",
                "links: annotations that connect two locations between two different sequences"
        )
      ),
      tags$p(
        "The Wormsynteny App uses a pipeline to transform the inputs data (that is a Cactus generated HAL file storing the alignments between species and GTF files storing genes data about our eleven species) into aligned genes, aligned sequences and links tables. Then, we passed those three tables to the gggenomes function to create our plots."
      ),
      tags$p(
        "The aligned sequences table contains the regions representing the total alignment length of consecutive homologous fragments from the same chromosome (that is the length between the minimum start and maximum end). Then, the aligned genes table contains the intersected genes inside the aligned regions as well as orthogroups data. Here is the orthogroups table mapped to genes for the eleven Caenorhabditis species. Finally, the links table contains all the link coordinates between phylogenetically adjacent species."
      ),
      tags$p(
        "The process of data transformation involves three primary steps. Initially, we retrieve the fragments of the ten other species that are homologous to the query sequence of C. elegans from the HAL file. Then, the links table is created from the raw homologous fragments by recovering the homologous fragments between phylogenetically adjacent species."
      ),
      tags$p(
        "Furthermore, two main hurdles could interfere with the accurate visualization of genes. Firstly, the presence of diminutive homologous fragments may render them indiscernible on the plot, thereby cluttering it into superfluous information. Additionally, certain fragments that are considerably distanced from other fragments on the same chromosome may result in a sequence length that is disproportionate to the actual length of genes, thereby impeding the precise visualization of these genes."
      ),
      tags$p(
        "To address this intricate visualization challenge, we assemble neighboring homologous fragments by considering a predetermined maximum length value, referred to as the gap value. Simultaneously, we eliminate fragments that fall below a specified minimum length requirement which is expressed as a percentage of the overall inquiry length (filtering percentage)."
      ),
      tags$p("Once the homologous fragments filtered, the last step is to generate the tables: "),
      tags$ul(
        tags$li(class = "paragraph-list",
                "by intersecting genes from GTF files in the homologous fragments (aligned genes table)"
        ),
        tags$li(class = "paragraph-list",
                "by taking the total length of homologous fragments for a same chromosome (aligned sequences table)"
        )
      ),
      tags$p(
        "A flowchart below summarizes the details of the pipeline."
      ),
      tags$img(
        class = "type-flowchart-img",
        src = "www/WormSynteny_pipeline_flowchart.png",
        alt = "Image 0"
      )
    )
  )


  outputs_data <- tags$html(
    tags$head(
      tags$meta(
        charset = "utf-8"
      ),
      tags$title(
        "Tutorials"
      )
    ),
    tags$body(
      tags$h1(
        "I-Introduction"
      ),
      tags$h2(
        "I.IV-Data ouptuts"
      ),
      tags$p(
        "The Wormsynteny pipeline generates three tables : aligned genes, aligned sequences and links as seen in the previous section. Those tables are then introduce into gggenomes function from gggenomes package which create the synteny plot."
      ),
      tags$h4(
        "Table outputs"
      ),
      tags$p(
        "The aligned genes table contains the following columns:"),
      tags$ul(
        tags$li(class = "paragraph-list",
                "bin_id : the species names"
        ),
        tags$li(class = "paragraph-list",
                "seqname: the sequences names (chromosomes names)"
        ),
        tags$li(class = "paragraph-list",
                "start: the starts of the aligned genes"
        ),
        tags$li(class = "paragraph-list",
                "start: the starts of the aligned genes"
        ),
        tags$li(class = "paragraph-list",
                "end:  the ends of the aligned genes"
        ),
        tags$li(class = "paragraph-list",
                "length: the length of aligned genes"
        ),
        tags$li(class = "paragraph-list",
                "type: the DNA types (either genes, or CDS here)"
        ),
        tags$li(class = "paragraph-list",
                "gene_id: the Wormbase genes ids"
        ),
        tags$li(class = "paragraph-list",
                "genes_name: the Wormbase C. elegans gene names"
        ),
        tags$li(class = "paragraph-list",
                "gene_id: the Wormbase genes ids"
        ),
        tags$li(class = "paragraph-list",
                "gene_biotype: the biological functions (protein coding, etc.)"
        ),
        tags$li(class = "paragraph-list",
                "orthogroup:  the Wormbase aligned genes orthologous number"
        ),
        tags$li(class = "paragraph-list",
                "coverages (see III.II-Finding the Ends of Genes section)"
        )
      ),
      tags$p(
        "The aligned sequences table contains the following columns:"
      ),
      tags$ul(
        tags$li(class = "paragraph-list",
                "bin_id : the species names"
        ),
        tags$li(class = "paragraph-list",
                "seqname: the sequences names (chromosomes names)"
        ),
        tags$li(class = "paragraph-list",
                "start: the starts of the aligned genes"
        ),
        tags$li(class = "paragraph-list",
                "start: the starts of the aligned genes"
        ),
        tags$li(class = "paragraph-list",
                "end:  the ends of the aligned genes"
        ),
        tags$li(class = "paragraph-list",
                "length: the length of aligned genes"
        )
      ),
      tags$p(
        "The links table contains the following columns:"
      ),
      tags$ul(
        tags$li(class = "paragraph-list",
                "bin_id : the species names of the firsts paired species"
        ),
        tags$li(class = "paragraph-list",
                "seqname: the sequences names (chromosomes names) of the firsts paired species"
        ),
        tags$li(class = "paragraph-list",
                "start: the starts of the homologous fragments of the firsts paired species"
        ),
        tags$li(class = "paragraph-list",
                "end:  the ends of the homologous fragments of the firsts paired species"
        ),
        tags$li(class = "paragraph-list",
                "length: the length of the homologous fragments of the firsts paired species"
        ),
        tags$li(class = "paragraph-list",
                "strand: the coding strands (+) and the noncoding strands (-) of the firsts paired species"
        ),
        tags$li(class = "paragraph-list",
                "bin_id2 : the species names of the seconds paired species"
        ),
        tags$li(class = "paragraph-list",
                "seqname2: the sequences names (chromosomes names) of the seconds paired species"
        ),
        tags$li(class = "paragraph-list",
                "start2: the starts of the homologous fragments of the seconds paired species"
        ),
        tags$li(class = "paragraph-list",
                "end2:  the ends of the homologous fragments of the seconds paired species"
        ),
        tags$li(class = "paragraph-list",
                "length2: the length of the homologous fragments of the seconds paired species"
        ),
        tags$li(class = "paragraph-list",
                "strand2: the coding strands (+) and the noncoding strands (-) of the seconds paired species"
        )
      ),
      tags$h4(
        "Plot outputs"
      ),
      tags$p(
        "There are two types of plot outputs : macrosynteny plot and microsynteny plot."
      ),
      tags$p(
        "Below is an example with the gene gap-1 (gap = 200, filter = 16) at macrosynteny level."
      ),
      tags$img(
        class = "type-plot-img",
        src = "www/image29.png",
        alt = "Image 0"
      ),
      tags$p(
        "The scale is based on the C. elegans inquiry sequence position in its chromosome indicated in the title.
The aligned sequences follow the phylogenetic tree order and are marked at the left by the species names (bin_ids).
The aligned genes are represented by the colored loci in each sequence whose corresponding orthogroups are written in the Orthogroups legend.
C. elegans genes legend is the C.elegans genes names of the same colored orthogroups.
Finally, in between sequences gray areas highlight the links thus the homology between two fragments.
"
      ),
      tags$p(
        "Below is an example with the gene gap-1 (gap = 200, filter = 16) at microsynteny level."
      ),
      tags$img(
        class = "type-plot-img",
        src = "www/image30.png",
        alt = "Image 0"
      ),
      tags$p(
        "Similarly, the aligned sequences are sorted in the phylogenetic tree order tagged by their respective species names.
However here, the orthogroup colored loci are CDS types which means coding exons. The spaces between exons are introns. All together, they form the genes."
      ),
      tags$p(
        "Another example below with the gene Y39A3CL.7 (gap = 500, filter = 5) at macrosynteny level which have added fragments represented by the (filtered) tags next to the sequences. Those added fragments are also sorted against the phylogenetic tree."
      ),
      tags$img(
        class = "type-plot-img",
        src = "www/image31.png",
        alt = "Image 0"
      )
    )
  )

  key_definitions <- tags$html(
    tags$head(
      tags$meta(
        charset = "utf-8"
      ),
      tags$title(
        "Tutorials"
      )
    ),
    tags$body(
      tags$h1(
        "II-Inquiry parameters"
      ),
      tags$h2(
        "II.I-Key definitions"
      ),
      tags$h4(
        tags$em(
          "The region of inquiry")
      ),
      tags$p(
        "The Wormsynteny App is used to create plots that show the alignment of genes among eleven Caenorhabditis species with Caenorhabditis elegans as the query species. Thus, the user needs to specify a sequence of inquiry from the C. elegans genome to align with the other ten species and to retrieve homologous fragments from them. This sequence can be inputted by the genomic coordinates (chromosome, start, end) or by selecting a C. elegans gene in the dropdown list. If a gene is selected, its coding region will be used as the inquiry sequences."
      ),
      tags$p("By setting the region of inquiry and clicking the “Plot” button, the alignment pipeline will be launched to generate a plot. Two other important parameters that affect the pipeline are “Gap value” and “Filtering percentage” (explained below)."
      ),
      tags$img(
        class = "type-region-img",
        src = "www/image1.png",
        alt = "Image 1"
      ),
      tags$h4(tags$em(
        "The history list")
      ),
      tags$p(
        "The app is built around a history list that stores plots and data generated by the pipeline. The data stored includes the genomic coordinates for the region of inquiry, the plot, aligned genes, aligned sequences, and links. The plot can be modified (e.g., select certain species to display) using the data stored in the history list."
      ),
      tags$p(
        "Clicking the “Plot” button will always create a new set of alignment data based on the region of inquiry and load the new data into the history list."
      ),
      tags$img(
        class = "type-long-img",
        src = "www/image2.png",
        alt = "Image 2"
      )
    )
  )

  generate_plot <- tags$html(
    tags$head(
      tags$meta(
        charset = "utf-8"
      ),
      tags$title(
        "Tutorials"
      )
    ),
    tags$body(
      tags$h1(
        "II-Inquiry parameters"
      ),
      tags$h2(
        "II.II-Generate a plot"
      ),
      tags$p(
        "There are two ways to select a region of inquiry for the alignment plot."
      ),
      tags$h4(
        "Typing in genomic coordinates"
      ),
      tags$p(
        "The inquiry section is organized such as :"
      ),
      tags$img(
        class = "type-long-img",
        src = "www/image3.png",
        alt = "Image 3"
      ),
      tags$p(
        "In the above inquiry section: "),
      tags$ul(
        tags$li(class = "paragraph-list",
                "Chromosome is the",tags$em("C. elegans"), "chromosome where the region of inquiry is located"
        ),
        tags$li(class = "paragraph-list",
                "Start is the start of the inquiry"
        ),
        tags$li(class = "paragraph-list",
                "End is the end of the inquiry"
        ),
        tags$li(class = "paragraph-list",
                "Gene selection (see next paragraph)"
        ),
        tags$li(class = "paragraph-list",
                "Gap value is the maximum length (bp) allowed for two adjacent genomic fragments (that are aligned to the inquiry) to be assembled into one longer fragment; increasing gap values increases the chance of assembling all the aligned fragments into one long aligned region"
        ),
        tags$li(class = "paragraph-list",
                "Filtering percentage is the minimal length requirement (in the percentage of the inquiry length) for the assembled fragments to be included in the final output; increasing filtering percentage increases the chance of removing small assembled fragments."
        )
      ),
      tags$p(
        "When the website is first loaded, the coordinates are pre-filled with the mec-17 gene coordinates as an example, and one of the optimal gap values and filtering percentages are inputted (see the Wormsynteny article for details). You can change the settings to the desired parameters before clicking the “Plot” button to launch the pipeline. A plot will be generated in 10-20 seconds."
      ),
      tags$img(
        class = "type-long-img",
        src = "www/image4.png",
        alt = "Image 4"
      ),
      tags$p(
        "After the plot is generated, you can retrieve the coordinates of the inquiry and the other parameters in the “Explanations” section below the plot."
      ),
      tags$img(
        class = "type-explanations-img",
        src = "www/image5.png",
        alt = "Image 5"
      ),
      tags$h4(
        "Selecting a gene"
      ),
      tags$img(
        class = "type-long-img",
        src = "www/image6.png",
        alt = "Image 6"
      ),
      tags$p(
        "A second way to input a region of inquiry is to select one of the ~47,000 C. elegans genes in the dropdown list at “Gene selection”. Start typing the gene name until the desired gene appears. Select the gene and click the “Plot” button. The coding region of the gene will be used for the alignment."
      ),
      tags$p(
        "Please note that when a gene (e.g., mec-2) is selected, the field for genomic coordinates is ignored. After the plot is generated, the gene field is emptied, and the coordinates field is automatically updated to show the coordinates of the coding region of the gene selected for the alignment."
      ),
      tags$img(
        class = "type-long-img",
        src = "www/image7.png",
        alt = "Image 7"
      ),
      tags$p(
        "After the plot is generated, you can retrieve the gene name and the other parameters in the “Explanations” section below the plot. "
      ),
      tags$img(
        class = "type-explanations-img",
        src = "www/image8.png",
        alt = "Image 8"
      )
    )
  )

  changing_inquiry <- tags$html(
    tags$head(
      tags$meta(
        charset = "utf-8"
      ),
      tags$title(
        "Tutorials"
      )
    ),
    tags$body(
      tags$h1(
        "II-Inquiry parameters"
      ),
      tags$h2(
        "II.III- Changing inquiry coordinates"
      ),
      tags$h4(
        "Via Zoom buttons"
      ),
      tags$p(
        "You can use the Zoom function to modify the inquiry coordinates. Clicking the “Zoom In” and “Zoom Out” buttons will automatically shrink and enlarge the region of inquiry by 500 bp at both ends, respectively. So, there is no need to change the coordinates manually. "),
      tags$p(
        "For example, the default mec-17 coordinates are IV: 7983802 … 7987183."
      ),
      tags$img(
        class = "type-long-img",
        src = "www/image9.png",
        alt = "Image 9"
      ),
      tags$p(
        "After clicking on “Zoom Out” button, the start position is reduced by 500, whereas the end position is increased by 500. The “Zoom In” function will do the opposite."
      ),
      tags$img(
        class = "type-long-img",
        src = "www/image10.png",
        alt = "Image 10"
      ),
      tags$p(
        "The “Explanations” panel records updated coordinates after the Zooming."
      ),
      tags$img(
        class = "type-explanations-img",
        src = "www/image11.png",
        alt = "Image 11"
      ),
      tags$h4(
        "Via back and reset"
      ),
      tags$p(
        "Pressing the “Back” button brings back the last entry in the history list and displays all the data from the previous inquiry, including the plot, aligned genes, sequences, and links. It also automatically fills in the coordinates of the last inquiry."),
      tags$p(
        "For example, when the current inquiry corresponds to mec-2 coordinates."
      ),
      tags$img(
        class = "type-long-img",
        src = "www/image12.png",
        alt = "Image 12"
      ),
      tags$p(
        "Pressing the back button brings back the mec-17 coordinates from the last inquiry."
      ),
      tags$img(
        class = "type-long-img",
        src = "www/image13.png",
        alt = "Image 13"
      ),
      tags$p(
        "The “Reset” button will simply fill the coordinates with the mec-17 default coordinates."
      )
    )
  )

  synteny_scale <- tags$html(
    tags$head(
      tags$meta(
        charset = "utf-8"
      ),
      tags$title(
        "Tutorials"
      )
    ),
    tags$body(
      tags$h1(
        "III-Plot control"
      ),
      tags$h2(
        "III.I- Synteny scale"
      ),
      tags$p(
        "Wormsynteny can display two levels of synteny, macrosynteny at the gene level and microsynteny at the exon/intron level. Synteny at the macro level does not necessarily indicate synteny at the micro level (illustrated by the examples below adapted from the Genomics and Comparative Genomics website)."
      ),
      tags$img(
        class = "micro-macro-img",
        src = "www/image14.png",
        alt = "Image 14"
      ),
      tags$p(
        "Moreover, the microsynteny option allows the analysis of genomic changes at a small scale (e.g., intron elongation, exon number changes) among species. To switch between visualization of synteny at the gene and exon/intron levels, click the “Macrosynteny” or “Microsynteny” button. Please note that the default plot will be in the macrosynteny style for a new inquiry."
      ),
      tags$img(
        class = "plot-features-img",
        src = "www/image15.png",
        alt = "Image 15"
      ),
      tags$p(
        "Below is the gap-1 gene plot as an example of using the microsynteny button going from macrosynteny level to microsynteny level."
      ),
      tags$img(
        class = "type-32-img",
        src = "www/image32.png",
        alt = "Image 32"
      ),
      tags$p(
        "Introns are represented by spaces between exons which are colored loci on each sequence."
      )
    )
  )

  end_of_genes <- tags$html(
    tags$head(
      tags$meta(
        charset = "utf-8"
      ),
      tags$title(
        "Tutorials"
      )
    ),
    tags$body(
      tags$h1(
        "III-Plot control"
      ),
      tags$h2(
        "III.II- Finding the Ends of Genes"
      ),
      tags$p(
        "The output of the pipeline is a set of aligned regions that are assembled from smaller aligned fragments based on the gap value and are filtered for length based on the filtering percentage. Genes were then annotated on these aligned regions, which may or may not include the entirety of the genes. This cannot be easily discerned from the plot, so we added a column called Coverage in the “Aligned genes” tab of the output to indicate if the genes are fully covered in the aligned region or not."
      ),
      tags$p(
        "If we select the gene mec-1 as the inquiry, we will have the “Aligned genes” table below in the output. If the last column Coverage shows “FULL”, it means the displayed gene block shows the entire gene. Otherwise, “PARTIAL” is shown with the percentages of the gene shown in the gene block."
      ),
      tags$img(
        class = "type-table-img",
        src = "www/image16.png",
        alt = "Image 16"
      ),
      tags$p(
        "To find the ends of the genes and display them in their entiery in the aligned region, we created a “Gene-centered visualizaiton” button to extend any gene that is present in the plot to their full size.
"
      ),
      tags$img(
        class = "gene-centered-img",
        src = "www/image17.png",
        alt = "Image 17"
      ),
      tags$p(
        "Once the button is clicked, the plot is updated and the “Aligned genes” table showed “FULL” coverage for every gene in the aligned region (see below). Please note that this button does not change the alignment results and does not adjust the gap value or the filtering percentages. It simply extends the aligned regions to include full genes."
      ),
      tags$img(
        class = "type-table-img",
        src = "www/image18.png",
        alt = "Image 18"
      )
    )
  )

  alignments_managment <- tags$html(
    tags$head(
      tags$meta(
        charset = "utf-8"
      ),
      tags$title(
        "Tutorials"
      )
    ),
    tags$body(
      tags$h1(
        "III-Plot control"
      ),
      tags$h2(
        "III.III- Alignements managment"
      ),
      tags$p(
        "In this section, you will discuss how to plot selected alignment when there are multiple aligned regions in some species even after the length-filtering step, which is controlled by the filtering percentage. Due to visualization constrains, we can only plot one aligned region per species and show synteny links among them. When there are multiple regions from one species aligned to the C. elegans inquiry, the default setting is to select the longest aligned region for the plot and place the others at the bottom and label them with “(filtered)”, because these fragments have passed the filtering criteria although they are not the longest."
      ),
      tags$p(
        "These extra fragments could be adjacent to the plotted fragment and can be connected with the longest aligned region if gap value is increased. Alternatively, they could also be located in regions far away from the plotted fragment and thus representing true second aligned site for the sequence of inquiry."
      ),
      tags$p(
        "We created two ways to manage the display of those extra fragments using the “Hide” and “Switch” buttons."
      ),
      tags$h4(
        "Hide button"
      ),
      tags$p(
        "When it is not necessary to show the shorter fragments. One can click the “Hide” button to hide them in the plot and only display the longest aligned region from each species. Below is an example for the gene bath-38 (gap value = 200, filtering value = 16). The pipleline generated extra aligned regions in three species, which contain genes from orthogroups that are different from the genes in the main aligned region."
      ),
      tags$img(
        class = "type-plot-img",
        src = "www/image19.png",
        alt = "Image 19"
      ),
      tags$p(
        "By clicking on the “Hide” button, all the extra alignments are removed and only one aligned region and one gene from each species is shown."
      ),
      tags$img(
        class = "type-plot-img",
        src = "www/image20.png",
        alt = "Image 20"
      ),
      tags$p(
        "To have them back, press the “Back” button or go through the history list by clicking the “History” button. "
      ),
      tags$h4(
        "Switch button"
      ),
      tags$p(
        "In some cases, the desired fragment may not be the longest one and is thus placed at the bottom with the “(filtered)” label. To use these shorter fragments in the alignment plot, you can click the “Switch” button to replace the default, longest aligned fragment with the first extra fragment. If there are multiple extra fragments, you can keep clicking the “Switch” button until the desired fragment is used in the alignment plot to show the links."
      ),
      tags$p(
        "Below is an example with the gene Y39A3CL.7 (gap = 500, filter = 5). "
      ),
      tags$img(
        class = "type-plot-img",
        src = "www/image21.png",
        alt = "Image 21"
      ),
      tags$p(
        "If the user wants to use the extra C. remanei fragment into the alignment, they can select “C. remanei” in the selection panel (1) and then press the “Switch” button (2) as below. "
      ),
      tags$img(
        class = "type-select-img",
        src = "www/image22.png",
        alt = "Image 22"
      ),
      tags$p(
        "After clicking the “Switch” button, the original aligned fragment will be switched with the extra fragment and be placed at the bottom of the plot and be labeled as “(filtered)”. Click the “Switch” button again can switch the fragment back into the alignment."
      ),
      tags$img(
        class = "type-plot-img",
        src = "www/image23.png",
        alt = "Image 23"
      ),
      tags$h4(
        "Show button"
      ),
      tags$p(
        "The default pipeline shows the alignment of C. elegans query sequences with all ten other nematode species. If the user wants to show only selected species, they can select the desired species in the selection panel and click the “Show” button to display only these species in the plot. The order of the species in the plot still follows the phylogenetic tree."
      ),
      tags$p(
        "Here is an example with the", tags$em("mec-17"), "gene (gap = 200, filter = 5)."),
      tags$img(
        class = "type-plot-img",
        src = "www/image24.png",
        alt = "Image 24"
      ),
      tags$p(
        "One can select species in the selection panel (1) and press the Show button (2) as below."),
      tags$img(
        class = "type-select-img",
        src = "www/image25.png",
        alt = "Image 25"
      ),
      tags$p(
        "The plot is then updated to only show the aligned regions from the selected species. Please, note that you do not need to select C. elegans since it is the query species and will always be shown."),
      tags$img(
        class = "type-plot-img",
        src = "www/image26.png",
        alt = "Image 26"
      )
    )
  )

  download_outputs <- tags$html(
    tags$head(
      tags$meta(
        charset = "utf-8"
      ),
      tags$title(
        "Tutorials"
      )
    ),
    tags$body(
      tags$h1(
        "IV-Download outputs"
      ),
      tags$p(
        "The app offers several options to download the output."
      ),
      tags$h4(
        "Download current data"
      ),
      tags$p(
        "The app allows the user to download the plot in pdf format as well as the tables (aligned genes, aligned sequences, links, etc.) in csv format. To do so, simply click on the download button below the plot and the tables."
      ),
      tags$h4(
        "Download historical data"
      ),
      tags$p(
        "When the users adjust the parameters to modify the plots, each version of the plot and associated data are stored in the Hisotry list, which allows the user to go back to previous version without remaking the plot. To download a specific version of the data, press the “History” tab to see the stored data in a table format. The table is built with the coordinates information as well as the last adjustment on the plot in the", tags$em("Modifications"), "column, which helps to differentiate plots."
      ),
      tags$p(
        "Using the", tags$em("mec-17"),"gene as an example, we ran a few variations of the plot, which are shown in the History table. Notice that the coordinates and pipeline parameters did not change because we only made adjustments to the plot display. The type of adjustments are shown in the", tags$em("Modifications"), "column."
      ),
      tags$img(
        class = "type-table-img",
        src = "www/image27.png",
        alt = "Image 27"
      ),
      tags$p(
        "To download data, select a plot version by selecting a row in the table, then press one or more “download links” below the table to retrieve relevant data."
      ),
      tags$img(
        class = "type-table-img",
        src = "www/image28.png",
        alt = "Image 28"
      )
    )
  )

  sidebar <- dashboardSidebar(
    width = 300,
    sidebarMenu(class = "menu",
                menuItem("App", tabName = "app"),
                menuItem("Introduction",
                         menuSubItem("Purpose of the App", tabName = "purpose_of_the_app"),
                         menuSubItem("Input data (Progressive Cactus output)", tabName = "input_data"),
                         menuSubItem("Pipeline for visualizing the alignment", tabName = "pipeline_for_visualizing_the_alignment"),
                         menuSubItem("Orthogroups table", tabName = "orthogroups_table"),
                         menuSubItem("Outputs data", tabName = "outputs_data")),
                menuItem("Inquiry parameters",
                         menuSubItem("Key definitions", tabName = "key_definitions"),
                         menuSubItem("Generate a plot", tabName = "generate_plot"),
                         menuSubItem("Changing inquiry coordinates", tabName = "changing_inquiry")),
                menuItem("Plot control",
                         menuSubItem("Synteny scale", tabName = "synteny_scale"),
                         menuSubItem("Finding the Ends of Genes", tabName = "end_of_genes"),
                         menuSubItem("Alignments managment", tabName = "alignments_managment")
                ),
                menuItem("Download outputs",tabName = "download_outputs")
    ))


  body <- dashboardBody(
    useShinyFeedback(),
    waiter::use_waiter(),
    tags$head(
      # CSS resources
      lapply(
        list.files(resources, pattern = "\\.css$", recursive = TRUE),
        function(x) tags$link(href = file.path("www", x), rel = "stylesheet")
      )
    ),
    tabItems(
      tabItem(tabName = "app",
              fluidRow(
                column(width = 3 ,
                       box( title ="Plot features", width = NULL, height = "auto", solidHeader = TRUE, status = "primary",
                            p(class = "p_plot_features","Use microsynteny button to visualize introns and exons."),
                            fluidRow(
                              column(12, align = "center",
                                     actionButton(class = "first_three_buttons_plot_features","Macro", "Macrosynteny", title = "Display genes as gene type"),

                                     actionButton(class = "first_three_buttons_plot_features", "Micro","Microsynteny", title = "Display genes as exons and introns  types"),

                                     actionButton(class = "first_three_buttons_plot_features", "Full_genes_extension",HTML("Full Genes Extension<br>visualization"), title = "Extend displayed genes to their full length")
                              )
                            ),
                            p(class = "p_plot_features","Select species sequences and click either on Show or Switch buttons."),
                            div(class = "checkboxes",
                                checkboxGroupInput("species", "Select Species:",
                                                   choiceNames = list(
                                                     HTML("<em>C. bovis</em>"),
                                                     HTML("<em>C. becei</em>"),
                                                     HTML("<em>C. panamensis</em>"),
                                                     HTML("<em>C. inopinata</em>"),
                                                     HTML("<em>C. tropicalis</em>"),
                                                     HTML("<em>C. remanei</em>"),
                                                     HTML("<em>C. latens</em>"),
                                                     HTML("<em>C. tribulationis</em>"),
                                                     HTML("<em>C. briggsae</em>"),
                                                     HTML("<em>C. nigoni</em>")),
                                                   choiceValues = list(
                                                     "C. bovis",
                                                     "C. becei",
                                                     "C. panamensis",
                                                     "C. inopinata",
                                                     "C. tropicalis",
                                                     "C. remanei",
                                                     "C. latens",
                                                     "C. tribulationis",
                                                     "C. briggsae",
                                                     "C. nigoni")
                                )
                            ),
                            p(class = "p_plot_features", "Switch or Show genes from selected species."),
                            fluidRow(
                              column(12, align = "center",
                                     actionButton(class = "other_three_buttons_plot_features", "switch", "Switch", title = "Switch filtered and non-filtered genes by selecting species names"),

                                     actionButton(class = "other_three_buttons_plot_features special_button", "show", "Show", title = "Show alignment between selected sequences by selecting species names")
                              )
                            ),
                            fluidRow(
                              column(12, align = "center",
                                     p(class = "p_plot_features", "Hide additonal alignments"),
                                     actionButton(class = "other_three_buttons_plot_features", "hide", "Hide", title = "Hide additonal alignments")
                              )
                            )
                       )
                ),
                column(width = 9,
                       box(title ="Inquiry Features", width = NULL, solidHeader = TRUE, status = "primary",
                           fluidRow( align = "center",
                                     column(2,
                                            div(class = "inquiry_features_region",
                                                selectInput( "Chr",
                                                             "Chromosome",
                                                             choices = c("I", "II", "III", "IV", "V", "X", "MtDNA"),
                                                             selected = "X"
                                                )
                                            )),
                                     column(2,
                                            div(class = "inquiry_features_region",
                                                numericInput("Start", "Start position", value = 4271165)
                                            )),
                                     column(2,
                                            div(class = "inquiry_features_region",
                                                numericInput("End", "End position", value = 4274216)
                                            )),
                                     column(2,
                                            div(class = "inquiry_features_region",
                                                selectizeInput(
                                                  inputId = "Genes",
                                                  label = "Gene selection",
                                                  multiple = FALSE,
                                                  choices = character(0),
                                                  options = list(
                                                    create = FALSE,
                                                    placeholder = "enter gene",
                                                    maxItems = '1'
                                                  )
                                                )
                                            )),
                                     column(2,
                                            div(class = "inquiry_features_region",
                                                numericInput("Gap","Gap value", value = 200,min = 0)
                                            )),
                                     column(2,
                                            div(class = "inquiry_features_region",
                                                numericInput("Filter", "Filtering (%)", value = 16, min= 0, max = 100)
                                            ))
                           ),
                           fluidRow(
                             column(2,
                                    actionButton(class = "inquiry_features_second_line", "Back", "Back", title = "Display data from rigth to left into the history list")),
                             column(2,
                                    actionButton(class = "inquiry_features_second_line","Forward", "Forward", title = "Display plots and data from left to right in the history list")
                             ),
                             column(2,
                                    actionButton(class = "inquiry_features_second_line","Reset", "Reset", title = "Reset the coordinates back to mec-17 gene coordinates and gap_value = 200, filtered_value = 5%")
                             ),
                             column(2,
                                    actionButton(class = "inquiry_features_second_line","ZoomIn","Zoom In", title = "Zoom in by 500 bp at both sides (-1000 bp in total)")
                             ),
                             column(2,
                                    actionButton(class = "inquiry_features_second_line","ZoomOut","Zoom Out", title = "Zoom out by 500 bp at both sides (+1000 bp in total)")
                             ),
                             column(2,
                                    actionButton(class = "inquiry_features_second_line","Search","Plot", title = paste("Plot the selected", tags$em("C. elegans"),"gene or inquiry"))
                             )
                           )
                       ),
                       tabBox(
                         title = "",
                         width = NULL,
                         height = "auto",
                         tabPanel(tags$p("Plot", class = "tabset-panel"),
                                  plotOutput("gggenomes_plot"),
                                  downloadButton(class = "download_buttons","download4", "Download_plot .pdf")
                         ),
                         tabPanel(tags$p("Aligned genes", class = "tabset-panel"),
                                  tableOutput("genes"),
                                  downloadButton(class = "download_buttons", "download1", "Download_genes .csv")
                         ),
                         tabPanel(tags$p("Aligned sequences", class = "tabset-panel"),
                                  tableOutput("seqs"),
                                  downloadButton(class = "download_buttons", "download2", "Download_sequences .csv")),
                         tabPanel(tags$p("Links", class = "tabset-panel"),
                                  tableOutput("links"),
                                  downloadButton(class = "download_buttons","download3", "Download_links .csv")
                         ),
                         tabPanel(tags$p("History table", class = "tabset-panel"),
                                  DT::DTOutput("history_table"),
                                  fluidRow(class = "history",
                                           column(3, downloadLink("downloadPlot", "Download Plot")),
                                           column(3, downloadLink("downloadGenes", "Download Aligned genes")),
                                           column(3, downloadLink("downloadSeqs", "Download Aligned sequences")),
                                           column(3, downloadLink("downloadLinks", "Download Links"))
                                  )
                         )
                       ),
                       box(title = "Explanations", width = NULL, height = "auto", solidHeader = TRUE, status = "primary",
                           fluidRow(
                             column(12, class = "explanations",
                                    verbatimTextOutput("explanation")
                             )
                           ),
                           fluidRow(
                             column(12,class = "explanations",
                                    verbatimTextOutput("others")
                             )
                           )
                       )
                )
              )
      ),
      tabItem(tabName = "purpose_of_the_app",
              fluidRow(
                column(width = 10, offset = 1,
                       box(width = NULL,
                           purpose_of_the_app)
                )
              )
      ),
      tabItem(tabName = "input_data",
              fluidRow(
                column(width = 10, offset = 1,
                       box(width = NULL,
                           input_data)
                )
              )
      ),
      tabItem(tabName = "pipeline_for_visualizing_the_alignment",
              fluidRow(
                column(width = 10, offset = 1,
                       box(width = NULL,
                           pipeline_for_visualizing_the_alignment)
                )
              )
      ),
      tabItem(tabName = "orthogroups_table",
              fluidRow(
                column(width = 12, offset = 0.25,
                       box(width = NULL,
                           DT::DTOutput("dt_table"))
                )
              )
      ),
      tabItem(tabName = "outputs_data",
              fluidRow(
                column(width = 10, offset = 1,
                       box(width = NULL,
                           outputs_data)
                )
              )
      ),
      tabItem(tabName = "key_definitions",
              fluidRow(
                column(width = 10, offset = 1,
                       box(width = NULL,
                           key_definitions)
                )
              )
      ),
      tabItem(tabName = "generate_plot",
              fluidRow(
                column(width = 10, offset = 1,
                       box(width = NULL,
                           generate_plot)
                )
              )
      ),
      tabItem(tabName = "changing_inquiry",
              fluidRow(
                column(width = 10,offset = 1,
                       box(width = NULL,
                           changing_inquiry)
                )
              )
      ),
      tabItem(tabName = "synteny_scale",
              fluidRow(
                column(width = 10,offset = 1,
                       box(width = NULL,
                           synteny_scale)
                )
              )
      ),
      tabItem(tabName = "end_of_genes",
              fluidRow(
                column(width = 10,offset = 1,
                       box(width = NULL,
                           end_of_genes)
                )
              )
      ),
      tabItem(tabName = "alignments_managment",
              fluidRow(
                column(width = 10,offset = 1,
                       box(width = NULL,
                           alignments_managment)
                )
              )
      ),
      tabItem(tabName = "download_outputs",
              fluidRow(
                column(width = 10,offset = 1,
                       box(width = NULL,
                           download_outputs)
                )
              )
      )
    )
  )

  ui <- dashboardPage(
    dashboardHeader(
      title = "WormSynteny",
      titleWidth = 300
    ),
    sidebar,
    body
  )


shinyApp(ui, server)

}

