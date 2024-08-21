#' Server function
#'
#' @param input A Shiny app input session
#' @param output A Shiny app output session
#' @param session A Shiny app session
#'
#' @return The server part of a Shiny R session
#' @export
#'
#' @examples
#' #
server <- function(input, output, session) {

  #Create the historic_state reactive value
  historic_state <- reactiveVal()

  #Initialize it
  historic_state(FALSE)

  #Initialize a list for files history
  filesHistory <- reactiveValues(plots = list(), genes = list(), seqs = list(), aligned_sequences = list(), links = list(), region = list(), prev_colors = list(), modifications = list())

  # Initialize current index to 0 (i.e., no plots have been displayed yet)
  currentIndex <- reactiveVal(0)

  #Load the Genes selection with the list of genes
  load_genesSelectize(session)

  #Dependent of Reset button : reset the coordinates
  observeEvent(input$Reset, {
    resetCoordinates(session)
  })

  output$download1 <- downloadHandler(
    filename = function() {
      "genes.csv"
    },
    content = function(file) {
      new_genes_seqs <- display_infos(filesHistory$genes[[length(filesHistory$genes)]], filesHistory$seqs[[length(filesHistory$seqs)]], filesHistory$links[[length(filesHistory$links)]], filesHistory$region[[length(filesHistory$region)]])
      write_csv(new_genes_seqs$genes, file)
    }
  )

  output$download2 <- downloadHandler(
    filename = function() {
      "seqs.csv"
    },
    content = function(file) {
      new_genes_seqs <- display_infos(filesHistory$genes[[length(filesHistory$genes)]], filesHistory$seqs[[length(filesHistory$seqs)]], filesHistory$links[[length(filesHistory$links)]], filesHistory$region[[length(filesHistory$region)]])
      write_csv(new_genes_seqs$seqs, file)
    }
  )

  output$download3 <- downloadHandler(
    filename = function() {
      "links.csv"
    },
    content = function(file) {
      new_genes_seqs <- display_infos(filesHistory$genes[[length(filesHistory$genes)]], filesHistory$seqs[[length(filesHistory$seqs)]], filesHistory$links[[length(filesHistory$links)]], filesHistory$region[[length(filesHistory$region)]])
      write_csv(new_genes_seqs$links, file)
    }
  )

  output$download4 <- downloadHandler(
    filename = function() {
      "Wormsynteny_plot.pdf"
    },
    content = function(file) {
      plot <- filesHistory$plots[length(filesHistory$plots)]
      pdf(file, width = 18, height = 7)
      print(plot)
      dev.off()
    }
  )

  ##Code to generate the DataTable here
  output$dt_table <- DT::renderDataTable({
    orthogroups
  },options = list(
    autowidth = TRUE,
    scrollX = '2000px'
  ))


  observeEvent(input$Search, {

    waiter::Waiter$new(id = "gggenomes_plot")$show()
    waiter::Waiter$new(id = "genes")$show()
    waiter::Waiter$new(id = "seqs")$show()
    waiter::Waiter$new(id = "links")$show()
    waiter::Waiter$new(id = "history_table")$show()

    #Start and End checking using ShinyFeedback
    order <- input$Start <= input$End
    shinyFeedback::feedbackDanger(inputId = "Start", !order, "Start must be smaller than end.")
    req(order)

    #Gap and filtering values checking using ShinyFeedback
    sign_gap <- input$Gap >= 0
    shinyFeedback::feedbackDanger(inputId = "Gap", !sign_gap, "The gap value must be positive.")
    req(sign_gap)

    sign_filter <- input$Filter >= 0
    shinyFeedback::feedbackDanger(inputId = "Filter", !sign_filter , "The filtering value must be positive.")
    req(sign_filter)

    #Create region of C.elegans
    region <- create_region(input$Chr, input$Start, input$End, input$Gap, input$Filter, input$Genes)
    gene_inquiry <- input$Genes

    #Update coordinates on the UI (chr, start, end, genes, gap and filter)
    updateCoordinates(session, region)

    #Run pipeline by calling the reactive function region
    data <- pipeline_fct(region)
    genes <- data$genes
    seqs <- data$seqs
    links <- data$links
    aligned_sequences <- data$aligned_sequences

    #Displayed infos on the UI (add coverage column)
    new_genes_seqs <- display_infos(genes, aligned_sequences, links, region)
    gene <- new_genes_seqs$genes
    aligned_sequence <- new_genes_seqs$aligned_sequences
    link <- new_genes_seqs$links

    #Render data
    output$genes <-  renderUI({
      gene %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$seqs <-  renderUI({
      aligned_sequence %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$links <-  renderUI({
      link %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = c(1,6), italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    #Initialize the colors
    prev_colors <- c()

    ##Assign colors to orthogroups
    prev_colors <- assign_colors(genes, prev_colors)

    ##Plot
    plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans)

    #Render plot image
    output$gggenomes_plot <- renderImage({
      width  <- session$clientData$output_gggenomes_plot_width
      height <- session$clientData$output_gggenomes_plot_height

      png("myplot.png", width = width*5.5, height = height*5.5, res=400)
      print(plot)
      dev.off()

      list(src = "myplot.png",
           width = width,
           height= height,
           alt = "Description of the image")
    }, deleteFile = FALSE)

    #Add a modification clue
    modifications <- "Plot"

    #Save data
    history(filesHistory, plot, genes, seqs, aligned_sequences, links, region, prev_colors, modifications)

    #Explanations
    render_explanations(output, genes, seqs)
    render_others(output, region, gene_inquiry)
    currentIndex(currentIndex() + 1)
    historic_state(TRUE)

  })

  observeEvent(input$Macro, {

    #Lists checking using ShinyFeedback
    if(historic_state() == FALSE){
      showNotification("The plot historic is empty. Please, generate a plot in the inquiry section first.")
    }
    req(historic_state() == TRUE)

    #Read previous genes, seqs, links and region
    genes <- filesHistory$genes[length(filesHistory$genes)]
    links <- filesHistory$links[length(filesHistory$links)]
    seqs <- filesHistory$seqs[length(filesHistory$seqs)]
    aligned_sequences <- filesHistory$aligned_sequences[length(filesHistory$aligned_sequences)]
    region <- filesHistory$region[length(filesHistory$region)]
    prev_colors <- filesHistory$prev_colors[length(filesHistory$prev_colors)]

    waiter::Waiter$new(id = "gggenomes_plot")$show()
    waiter::Waiter$new(id = "genes")$show()
    waiter::Waiter$new(id = "seqs")$show()
    waiter::Waiter$new(id = "links")$show()
    waiter::Waiter$new(id = "history_table")$show()

    #Let everything be a data frame
    genes <- as.data.frame(genes)
    seqs <- as.data.frame(seqs)
    aligned_sequences <- as.data.frame(aligned_sequences)
    links <- as.data.frame(links)
    region <- as.data.frame(region)

    #Change genes into macro genes
    genes <- macro(genes, seqs, species_macro)
    gene_inquiry <- input$Genes

    #Displayed infos on the UI (add coverage column)
    new_genes_seqs <- display_infos(genes, aligned_sequences, links, region)
    gene <- new_genes_seqs$genes
    aligned_sequence <- new_genes_seqs$aligned_sequences
    link <- new_genes_seqs$links

    #Render data
    output$genes <-  renderUI({
      gene %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$seqs <-  renderUI({
      aligned_sequence %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$links <-  renderUI({
      link %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = c(1,6), italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    ##Assign colors to orthogroups
    prev_colors <- assign_colors(genes, prev_colors$colors)

    ##Plot
    plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans)

    #Render plot image
    output$gggenomes_plot <- renderImage({
      width  <- session$clientData$output_gggenomes_plot_width
      height <- session$clientData$output_gggenomes_plot_height

      png("myplot.png", width = width*5.5, height = height*5.5, res=400)
      print(plot)
      dev.off()

      list(src = "myplot.png",
           width = width,
           height= height,
           alt = "Description of the image")
    }, deleteFile = FALSE)

    #Add a modification clue
    modifications <- "Macrosynteny"

    #Save data
    history(filesHistory, plot, genes, seqs, aligned_sequences, links, region, prev_colors, modifications)

    #Explanations
    render_explanations(output, genes, seqs)
    render_others(output, region, gene_inquiry)
    currentIndex(currentIndex() + 1)
  })

  #Control the click
  observeEvent(input$Micro, {

    #Lists checking using ShinyFeedback
    if(historic_state() == FALSE){
      showNotification("The plot historic is empty. Please, generate a plot in the inquiry section first.")
    }
    req(historic_state() == TRUE)

    waiter::Waiter$new(id = "gggenomes_plot")$show()
    waiter::Waiter$new(id = "genes")$show()
    waiter::Waiter$new(id = "seqs")$show()
    waiter::Waiter$new(id = "links")$show()
    waiter::Waiter$new(id = "history_table")$show()

    #Read previous genes, seqs, links and region
    genes <- filesHistory$genes[length(filesHistory$genes)]
    links <- filesHistory$links[length(filesHistory$links)]
    seqs <- filesHistory$seqs[length(filesHistory$seqs)]
    aligned_sequences <- filesHistory$aligned_sequences[length(filesHistory$aligned_sequences)]
    region <- filesHistory$region[length(filesHistory$region)]
    prev_colors <- filesHistory$prev_colors[length(filesHistory$prev_colors)]

    #Let everything be a data frame
    genes <- as.data.frame(genes)
    seqs <- as.data.frame(seqs)
    aligned_sequences <- as.data.frame(aligned_sequences)
    links <- as.data.frame(links)
    region <- as.data.frame(region)

    #Change genes into micro genes
    genes <- micro(genes, species_micro)
    gene_inquiry <- input$Genes

    #Displayed infos on the UI (add coverage column)
    new_genes_seqs <- display_infos(genes, aligned_sequences, links, region)
    gene <- new_genes_seqs$genes
    aligned_sequence <- new_genes_seqs$aligned_sequences
    link <- new_genes_seqs$links

    #Render data
    output$genes <-  renderUI({
      gene %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$seqs <-  renderUI({
      aligned_sequence %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$links <-  renderUI({
      link %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = c(1,6), italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    ##Assign colors to orthogroups
    prev_colors <- assign_colors(genes, prev_colors$colors)

    ##Plot
    plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans_micro)

    #Render plot image
    output$gggenomes_plot <- renderImage({
      width  <- session$clientData$output_gggenomes_plot_width
      height <- session$clientData$output_gggenomes_plot_height

      png("myplot.png", width = width*5.5, height = height*5.5, res=400)
      print(plot)
      dev.off()

      list(src = "myplot.png",
           width = width,
           height= height,
           alt = "Description of the image")
    }, deleteFile = FALSE)


    #Add a modification clue
    modifications <- "Microsynteny"

    #Save data
    history(filesHistory, plot, genes, seqs, aligned_sequences, links, region, prev_colors, modifications)

    #Explanations
    render_explanations(output, genes, seqs)
    render_others(output, region, gene_inquiry)
    currentIndex(currentIndex() + 1)
  })

  #Dependent of ZoomOut button
  observeEvent(input$ZoomOut, {

    #Lists checking using ShinyFeedback
    if(historic_state() == FALSE){
      showNotification("The plot historic is empty. Please, generate a plot in the inquiry section first.")
    }
    req(historic_state() == TRUE)

    waiter::Waiter$new(id = "gggenomes_plot")$show()
    waiter::Waiter$new(id = "genes")$show()
    waiter::Waiter$new(id = "seqs")$show()
    waiter::Waiter$new(id = "links")$show()
    waiter::Waiter$new(id = "history_table")$show()

    #Read previous genes, seqs, links and region
    genes <- filesHistory$genes[length(filesHistory$genes)]
    links <- filesHistory$links[length(filesHistory$links)]
    seqs <- filesHistory$seqs[length(filesHistory$seqs)]
    aligned_sequences <- filesHistory$aligned_sequences[length(filesHistory$aligned_sequences)]
    region <- filesHistory$region[length(filesHistory$region)]
    prev_colors <- filesHistory$prev_colors[length(filesHistory$prev_colors)]

    #Let everything be a data frame
    genes <- as.data.frame(genes)
    seqs <- as.data.frame(seqs)
    aligned_sequences <- as.data.frame(aligned_sequences)
    links <- as.data.frame(links)
    region <- as.data.frame(region)

    #Zoom out : change start and end coordinates (-500 bp and +500 bp)
    region <- zoom_out(region)
    gene_inquiry <- input$Genes

    #Update the displayed coordinates
    updateCoordinates(session,region)

    #If genes exists
    if(nrow(genes)!=0){
      #And if it's type CDS (micro-synteny)
      if("CDS" %in% genes$types){

        #Run the pipeline
        data <- pipeline_fct(region)

        genes <- data$genes
        seqs <- data$seqs
        aligned_sequences <- data$aligned_sequences
        links <- data$links

        #Run the micro function to get the introns/exons that overlap genes
        genes <- micro(genes, species_micro)

        ##Assign colors to orthogroups
        prev_colors <- assign_colors(genes, prev_colors$colors)

        #Plot
        plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans_micro)

        #Type gene (Macro-synteny)
      }else{

        #Run the pipeline
        data <- pipeline_fct(region)

        genes <- data$genes
        seqs <- data$seqs
        aligned_sequences <- data$aligned_sequences
        links <- data$links

        ##Assign colors to orthogroups
        prev_colors <- assign_colors(genes, prev_colors$colors)

        #Use gggenomes_plot function to plot the result using gggenomes package
        plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans)

      }
      #No genes
    }else{
      #Run the pipeline
      data <- pipeline_fct(region)

      genes <- data$genes
      seqs <- data$seqs
      aligned_sequences <- data$aligned_sequences
      links <- data$links

      ##Assign colors to orthogroups
      prev_colors <- assign_colors(genes, prev_colors$colors)

      #Use gggenomes_plot function to plot the result using gggenomes package
      plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans)
    }

    #Displayed infos on the UI (add coverage column)
    new_genes_seqs <- display_infos(genes, aligned_sequences, links, region)
    gene <- new_genes_seqs$genes
    aligned_sequence <- new_genes_seqs$aligned_sequences
    link <- new_genes_seqs$links

    #Render data
    output$genes <-  renderUI({
      gene %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$seqs <-  renderUI({
      aligned_sequence %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$links <-  renderUI({
      link %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = c(1,6), italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    #Render plot image
    output$gggenomes_plot <- renderImage({
      width  <- session$clientData$output_gggenomes_plot_width
      height <- session$clientData$output_gggenomes_plot_height

      png("myplot.png", width = width*5.5, height = height*5.5, res=400)
      print(plot)
      dev.off()

      list(src = "myplot.png",
           width = width,
           height= height,
           alt = "Description of the image")
    }, deleteFile = FALSE)

    #Add a modification clue
    modifications <- "Zoom out"

    #Save data
    history(filesHistory, plot, genes, seqs, aligned_sequences, links, region, prev_colors, modifications)

    #Explanations
    render_explanation_out(output,region)
    render_others(output, region, gene_inquiry)
    currentIndex(currentIndex() + 1)
  })


  #Dependent of ZoomIn button : must be click to start plotting process
  observeEvent(input$ZoomIn, {

    #Lists checking using ShinyFeedback
    if(historic_state() == FALSE){
      showNotification("The plot historic is empty. Please, generate a plot in the inquiry section first.")
    }
    req(historic_state() == TRUE)

    waiter::Waiter$new(id = "gggenomes_plot")$show()
    waiter::Waiter$new(id = "genes")$show()
    waiter::Waiter$new(id = "seqs")$show()
    waiter::Waiter$new(id = "links")$show()
    waiter::Waiter$new(id = "history_table")$show()

    #Read previous genes, seqs, links and region
    genes <- filesHistory$genes[length(filesHistory$genes)]
    links <- filesHistory$links[length(filesHistory$links)]
    seqs <- filesHistory$seqs[length(filesHistory$seqs)]
    aligned_sequences <- filesHistory$aligned_sequences[length(filesHistory$aligned_sequences)]
    region <- filesHistory$region[length(filesHistory$region)]
    prev_colors <- filesHistory$prev_colors[length(filesHistory$prev_colors)]

    #Let everything be a data frame
    genes <- as.data.frame(genes)
    seqs <- as.data.frame(seqs)
    aligned_sequences <- as.data.frame(aligned_sequences)
    links <- as.data.frame(links)
    region <- as.data.frame(region)

    #Zoom in : change start and end coordinates (+500 bp and -500 bp)
    region <- zoom_in(region)
    gene_inquiry <- input$Genes

    #Update the displayed coordinates
    updateCoordinates(session,region)

    #If genes exists
    if(nrow(genes)!=0){
      #And if it's type CDS (micro-synteny)
      if("CDS" %in% genes$types){

        #Run the pipeline
        data <- pipeline_fct(region)

        genes <- data$genes
        seqs <- data$seqs
        aligned_sequences <- data$aligned_sequences
        links <- data$links

        #Run the micro function to get the introns/exons that overlap genes
        genes <- micro(genes, species_micro)

        ##Assign colors to orthogroups
        prev_colors <- assign_colors(genes, prev_colors$colors)

        #Plot
        plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans_micro)

        #Type gene (Macro-synteny)
      }else{

        #Run the pipeline
        data <- pipeline_fct(region)

        genes <- data$genes
        seqs <- data$seqs
        aligned_sequences <- data$aligned_sequences
        links <- data$links

        ##Assign colors to orthogroups
        prev_colors <- assign_colors(genes, prev_colors$colors)

        #Use gggenomes_plot function to plot the result using gggenomes package
        plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans)
      }
      #No genes
    }else{
      #Run the pipeline
      data <- pipeline_fct(region)

      genes <- data$genes
      seqs <- data$seqs
      aligned_sequences <- data$aligned_sequences
      links <- data$links

      ##Assign colors to orthogroups
      prev_colors <- assign_colors(genes, prev_colors$colors)

      #Use gggenomes_plot function to plot the result using gggenomes package
      plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans)
    }

    #Displayed infos on the UI (add coverage column)
    new_genes_seqs <- display_infos(genes, aligned_sequences, links, region)
    gene <- new_genes_seqs$genes
    aligned_sequence <- new_genes_seqs$aligned_sequences
    link <- new_genes_seqs$links

    #Render data
    output$genes <-  renderUI({
      gene %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$seqs <-  renderUI({
      aligned_sequence %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$links <-  renderUI({
      link %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = c(1,6), italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})


    #Render plot image
    output$gggenomes_plot <- renderImage({
      width  <- session$clientData$output_gggenomes_plot_width
      height <- session$clientData$output_gggenomes_plot_height

      png("myplot.png", width = width*5.5, height = height*5.5, res=400)
      print(plot)
      dev.off()

      list(src = "myplot.png",
           width = width,
           height= height,
           alt = "Description of the image")
    }, deleteFile = FALSE)

    #Add a modification clue
    modifications <- "Zoom in"

    #Save data
    history(filesHistory, plot, genes, seqs, aligned_sequences, links, region, prev_colors, modifications)

    #Explanations
    render_explanation_in(output,region)
    render_others(output, region, gene_inquiry)
    currentIndex(currentIndex() + 1)
  })

  #Dependent of switch button : must be click to start plotting process
  observeEvent(input$switch, {

    #Lists checking using ShinyFeedback
    if(is.null(input$species)){
      showNotification("Select sequences that you want to switch by selecting species names.")
    }

    #Lists checking using ShinyFeedback
    if(historic_state() == FALSE){
      showNotification("The plot historic is empty. Please, generate a plot in the inquiry section first.")
    }
    req(input$species)
    req(historic_state() == TRUE)

    waiter::Waiter$new(id = "gggenomes_plot")$show()
    waiter::Waiter$new(id = "genes")$show()
    waiter::Waiter$new(id = "seqs")$show()
    waiter::Waiter$new(id = "links")$show()
    waiter::Waiter$new(id = "history_table")$show()

    #Read previous genes, seqs, links and region
    genes <- filesHistory$genes[length(filesHistory$genes)]
    links <- filesHistory$links[length(filesHistory$links)]
    seqs <- filesHistory$seqs[length(filesHistory$seqs)]
    aligned_sequences <- filesHistory$aligned_sequences[length(filesHistory$aligned_sequences)]
    region <- filesHistory$region[length(filesHistory$region)]
    prev_colors <- filesHistory$prev_colors[length(filesHistory$prev_colors)]

    #Let everything be a data frame
    genes <- as.data.frame(genes)
    seqs <- as.data.frame(seqs)
    aligned_sequences <- as.data.frame(aligned_sequences)
    links <- as.data.frame(links)
    region <- as.data.frame(region)

    #Initialize a list to store the selected species (checked boxes)
    selected_species <- input$species
    gene_inquiry <- input$Genes

    if(nrow(genes)!= 0){

      #Update aligned_sequences table
      aligned_sequences <- update_sequences_table(selected_species, aligned_sequences)

      #Update genes and seqs tables
      df <- switch_fct(selected_species, seqs, genes, aligned_sequences)
      genes <- df$genes
      seqs <- df$seqs

    }

    #Displayed infos on the UI (add coverage column)
    new_genes_seqs <- display_infos(genes, aligned_sequences, links, region)
    gene <- new_genes_seqs$genes
    aligned_sequence <- new_genes_seqs$aligned_sequences
    link <- new_genes_seqs$links

    #Render data
    output$genes <-  renderUI({
      gene %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$seqs <-  renderUI({
      aligned_sequence %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$links <-  renderUI({
      link %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = c(1,6), italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    ##Assign colors to orthogroups
    prev_colors <- assign_colors(genes, prev_colors$colors)

    ##Plot
    plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans)

    #Render plot image
    output$gggenomes_plot <- renderImage({
      width  <- session$clientData$output_gggenomes_plot_width
      height <- session$clientData$output_gggenomes_plot_height

      png("myplot.png", width = width*5.5, height = height*5.5, res=400)
      print(plot)
      dev.off()

      list(src = "myplot.png",
           width = width,
           height= height,
           alt = "Description of the image")
    }, deleteFile = FALSE)

    #Add a modification clue
    modifications <- "Switch"

    #Save data
    history(filesHistory, plot, genes, seqs, aligned_sequences, links, region, prev_colors, modifications)

    #Explanations
    render_explanations(output,genes, seqs)
    render_others(output, region, gene_inquiry)
    currentIndex(currentIndex() + 1)
  })

  #Dependent of show button
  observeEvent(input$show, {

    #Lists checking using ShinyFeedback
    if(is.null(input$species)){
      showNotification(HTML("Select sequences to check out links between them and <em>C. elegans</em> by selecting species names."))
    }

    #Lists checking using ShinyFeedback
    if(historic_state() == FALSE){
      showNotification("The plot historic is empty. Please, generate a plot in the inquiry section first.")
    }
    req(input$species)
    req(historic_state() == TRUE)

    req(input$species)

    waiter::Waiter$new(id = "gggenomes_plot")$show()
    waiter::Waiter$new(id = "genes")$show()
    waiter::Waiter$new(id = "seqs")$show()
    waiter::Waiter$new(id = "links")$show()
    waiter::Waiter$new(id = "history_table")$show()

    gene_inquiry <- input$Genes

    #Read previous genes, seqs, links and region
    genes <- filesHistory$genes[length(filesHistory$genes)]
    links <- filesHistory$links[length(filesHistory$links)]
    seqs <- filesHistory$seqs[length(filesHistory$seqs)]
    aligned_sequences <- filesHistory$aligned_sequences[length(filesHistory$aligned_sequences)]
    region <- filesHistory$region[length(filesHistory$region)]
    prev_colors <- filesHistory$prev_colors[length(filesHistory$prev_colors)]

    #Let everything be a data frame
    genes <- as.data.frame(genes)
    seqs <- as.data.frame(seqs)
    aligned_sequences <- as.data.frame(aligned_sequences)
    links <- as.data.frame(links)
    region <- as.data.frame(region)

    selected_species <- input$species

    #Update genes, seqs and links
    l <- show_fct(genes, seqs, aligned_sequences, selected_species)
    genes <- l$genes
    seqs <- l$seqs
    aligned_sequences <- l$aligned_sequences
    links <- l$links

    #Displayed infos on the UI (add coverage column)
    new_genes_seqs <- display_infos(genes, aligned_sequences, links, region)
    gene <- new_genes_seqs$genes
    aligned_sequence <- new_genes_seqs$aligned_sequences
    link <- new_genes_seqs$links

    #Render data
    output$genes <-  renderUI({
      gene %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$seqs <-  renderUI({
      aligned_sequence %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$links <-  renderUI({
      link %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = c(1,6), italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    ##Assign colors to orthogroups
    prev_colors <- assign_colors(genes, prev_colors$colors)

    ##Plot
    plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans)

    #Render plot image
    output$gggenomes_plot <- renderImage({
      width  <- session$clientData$output_gggenomes_plot_width
      height <- session$clientData$output_gggenomes_plot_height

      png("myplot.png", width = width*5.5, height = height*5.5, res=400)
      print(plot)
      dev.off()

      list(src = "myplot.png",
           width = width,
           height= height,
           alt = "Description of the image")
    }, deleteFile = FALSE)

    #Add a modification clue
    modifications <- "Show"

    #Save data
    history(filesHistory, plot, genes, seqs, aligned_sequences, links, region, prev_colors, modifications)

    #Explanations
    render_explanations(output, genes, seqs)
    render_others(output, region, gene_inquiry)
    currentIndex(currentIndex() + 1)
  })


  #Dependent of hide button
  observeEvent(input$hide, {

    #Lists checking using ShinyFeedback
    if(historic_state() == FALSE){
      showNotification("The plot historic is empty. Please, generate a plot in the inquiry section first.")
    }
    req(historic_state() == TRUE)

    waiter::Waiter$new(id = "gggenomes_plot")$show()
    waiter::Waiter$new(id = "genes")$show()
    waiter::Waiter$new(id = "seqs")$show()
    waiter::Waiter$new(id = "links")$show()
    waiter::Waiter$new(id = "history_table")$show()

    #Read previous genes, seqs, links and region
    genes <- filesHistory$genes[length(filesHistory$genes)]
    links <- filesHistory$links[length(filesHistory$links)]
    seqs <- filesHistory$seqs[length(filesHistory$seqs)]
    aligned_sequences <- filesHistory$aligned_sequences[length(filesHistory$aligned_sequences)]
    region <- filesHistory$region[length(filesHistory$region)]
    prev_colors <- filesHistory$prev_colors[length(filesHistory$prev_colors)]
    gene_inquiry <- input$Genes

    #Let everything be a data frame
    genes <- as.data.frame(genes)
    seqs <- as.data.frame(seqs)
    aligned_sequences <- as.data.frame(aligned_sequences)
    links <- as.data.frame(links)
    region <- as.data.frame(region)

    #Filter the filtered data
    l <- hide_fct(seqs, aligned_sequences, genes)
    genes <- l$genes
    aligned_sequences <- l$aligned_sequences
    seqs <- l$seqs

    #Displayed infos on the UI (add coverage column)
    new_genes_seqs <- display_infos(genes, aligned_sequences, links, region)
    gene <- new_genes_seqs$genes
    aligned_sequence <- new_genes_seqs$aligned_sequences
    link <- new_genes_seqs$links

    #Render data
    output$genes <-  renderUI({
      gene %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$seqs <-  renderUI({
      aligned_sequence %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$links <-  renderUI({
      link %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = c(1,6), italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    ##Assign colors to orthogroups
    prev_colors <- assign_colors(genes, prev_colors$colors)

    ##Plot
    plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans)

    #Render plot image
    output$gggenomes_plot <- renderImage({
      width  <- session$clientData$output_gggenomes_plot_width
      height <- session$clientData$output_gggenomes_plot_height

      png("myplot.png", width = width*5.5, height = height*5.5, res=400)
      print(plot)
      dev.off()

      list(src = "myplot.png",
           width = width,
           height= height,
           alt = "Description of the image")
    }, deleteFile = FALSE)

    #Add a modification clue
    modifications <- "Hide"

    #Save data
    history(filesHistory, plot, genes, seqs, aligned_sequences, links, region, prev_colors, modifications)

    #Explanations
    render_explanations(output,genes, seqs)
    render_others(output, region, gene_inquiry)
    currentIndex(currentIndex() + 1)
  })

  #Dependent of gene_centered button
  observeEvent(input$Full_genes_extension, {

    #Lists checking using ShinyFeedback
    if(historic_state() == FALSE){
      showNotification("The plot historic is empty. Please, generate a plot in the inquiry section first.")
    }
    req(historic_state() == TRUE)

    req(input$Full_genes_extension)

    waiter::Waiter$new(id = "gggenomes_plot")$show()
    waiter::Waiter$new(id = "genes")$show()
    waiter::Waiter$new(id = "seqs")$show()
    waiter::Waiter$new(id = "links")$show()
    waiter::Waiter$new(id = "history_table")$show()

    #Read previous genes, seqs, links and region
    genes <- filesHistory$genes[length(filesHistory$genes)]
    links <- filesHistory$links[length(filesHistory$links)]
    seqs <- filesHistory$seqs[length(filesHistory$seqs)]
    aligned_sequences <- filesHistory$aligned_sequences[length(filesHistory$aligned_sequences)]
    region <- filesHistory$region[length(filesHistory$region)]
    prev_colors <- filesHistory$prev_colors[length(filesHistory$prev_colors)]

    #Let everything be a data frame
    genes <- as.data.frame(genes)
    seqs <- as.data.frame(seqs)
    aligned_sequences <- as.data.frame(aligned_sequences)
    links <- as.data.frame(links)
    region <- as.data.frame(region)

    #Genes
    gene_inquiry <- input$Genes

    #If genes exists
    if(nrow(genes)!=0){
      #And if it's type CDS (microsynteny)
      if("CDS" %in% genes$types){

        #Change the visualization (full gene view)
        genes <- full_genes_fct(genes, species_micro)
        seqs <- seqs_change(seqs, genes)
        aligned_sequences <- aligned_sequences_extension(aligned_sequences, seqs)

        ##Assign colors to orthogroups
        prev_colors <- assign_colors(genes, prev_colors$colors)

        #Plot
        plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans_micro)

        #Type gene (Macrosynteny)
      }else{

        #Change the visualization (full gene view)
        genes <- full_genes_fct(genes, species_macro)
        seqs <- seqs_change(seqs, genes)
        aligned_sequences <- aligned_sequences_extension(aligned_sequences, seqs)

        ##Assign colors to orthogroups
        prev_colors <- assign_colors(genes, prev_colors$colors)

        #Use gggenomes_plot function to plot the result using gggenomes package
        plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans)

      }
      #No genes
    }else{

      #Change the visualization (full gene view)
      genes <- full_genes_fct(genes, species_macro)
      seqs <- seqs_change(seqs, genes)
      aligned_sequences <- aligned_sequences_extension(aligned_sequences, seqs)

      ##Assign colors to orthogroups
      prev_colors <- assign_colors(genes, prev_colors$colors)

      #Use gggenomes_plot function to plot the result using gggenomes package
      plot <- ggg_plot(seqs, prev_colors$genes, links, region, elegans)

    }

    #Displayed infos on the UI (add coverage column)
    new_genes_seqs <- display_infos(genes, aligned_sequences, links, region)
    gene <- new_genes_seqs$genes
    aligned_sequence <- new_genes_seqs$aligned_sequences
    link <- new_genes_seqs$links

    #Render data
    output$genes <-  renderUI({
      gene %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$seqs <-  renderUI({
      aligned_sequence %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = 1, italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    output$links <-  renderUI({
      link %>%
        flextable() %>%
        theme_zebra() %>%
        italic(j = c(1,6), italic = TRUE) %>%
        autofit() %>%
        htmltools_value()})

    #Render plot image
    output$gggenomes_plot <- renderImage({
      width  <- session$clientData$output_gggenomes_plot_width
      height <- session$clientData$output_gggenomes_plot_height

      png("myplot.png", width = width*5.5, height = height*5.5, res=400)
      print(plot)
      dev.off()

      list(src = "myplot.png",
           width = width,
           height= height,
           alt = "Description of the image")
    }, deleteFile = FALSE)

    #Add a modification clue
    modifications <- "Full genes extension"

    #Save data
    history(filesHistory, plot, genes, seqs, aligned_sequences, links, region, prev_colors, modifications)

    #Explanations
    render_explanations(output, genes, seqs)
    render_others(output, region, gene_inquiry)
    currentIndex(currentIndex() + 1)
  })


  # When the back button is clicked, update the current index and display the previous plot
  observeEvent(input$Back, {

    waiter::Waiter$new(id = "gggenomes_plot")$show()
    waiter::Waiter$new(id = "genes")$show()
    waiter::Waiter$new(id = "seqs")$show()
    waiter::Waiter$new(id = "links")$show()
    waiter::Waiter$new(id = "history_table")$show()

    if (currentIndex() > 1) {

      # There are previous plots to display
      currentIndex(currentIndex() - 1)

      # Render the current plot and data
      # Get the current plot from the history list
      currentPlot <- filesHistory$plots[[currentIndex()]]

      #Render plot image
      output$gggenomes_plot <- renderImage({
        width  <- session$clientData$output_gggenomes_plot_width
        height <- session$clientData$output_gggenomes_plot_height

        png("myplot.png", width = width*5.5, height = height*5.5, res=400)
        print(currentPlot)
        dev.off()

        list(src = "myplot.png",
             width = width,
             height= height,
             alt = "Description of the image")
      }, deleteFile = FALSE)

      # Get the current file data from the history lists
      currentGenes <- filesHistory$genes[[currentIndex()]]
      currentSeqs <- filesHistory$aligned_sequences[[currentIndex()]]
      currentLinks <- filesHistory$links[[currentIndex()]]
      currentRegion <- filesHistory$region[[currentIndex()]]
      currentPrevColors <- filesHistory$prev_colors[[currentIndex()]]

      # Displayed infos on the UI (add coverage column)
      new_genes_seqs <- display_infos(currentGenes, currentSeqs, currentLinks, currentRegion)
      gene <- new_genes_seqs$genes
      aligned_sequence <- new_genes_seqs$aligned_sequences
      link <- new_genes_seqs$links

      #Render data
      output$genes <- renderUI({
        gene %>%
          flextable() %>%
          theme_zebra() %>%
          italic(j = 1, italic = TRUE) %>%
          autofit() %>%
          htmltools_value()})

      output$seqs <- renderUI({
        aligned_sequence %>%
          flextable() %>%
          theme_zebra() %>%
          italic(j = 1, italic = TRUE) %>%
          autofit() %>%
          htmltools_value()})

      output$links <- renderUI({
        link %>%
          flextable() %>%
          theme_zebra() %>%
          italic(j = c(1,6), italic = TRUE) %>%
          autofit() %>%
          htmltools_value()})

      #Update coordinates on the UI (chr, start, end, genes, gap and filter)
      updateCoordinates(session, currentRegion)

    }else {
      # No more plots in history, render default plot or clear the plot
      output$explanation <- renderText({
        paste0("No more plots and data stored in history.")
      })
      output$others  <- renderText({
        paste0("No inquiry search launched.")

      })

      output$gggenomes_plot <- renderPlot(NULL)
      output$genes <-  renderTable(NULL)
      output$seqs <-  renderTable(NULL)
      output$links <-  renderTable(NULL)

      #Put currentIndex to zero
      currentIndex(0)

    }
  })

  # When the forward button is clicked, update the current index and display the next plot
  observeEvent(input$Forward, {

    waiter::Waiter$new(id = "gggenomes_plot")$show()
    waiter::Waiter$new(id = "genes")$show()
    waiter::Waiter$new(id = "seqs")$show()
    waiter::Waiter$new(id = "links")$show()
    waiter::Waiter$new(id = "history_table")$show()

    if (currentIndex() < length(filesHistory$plots)) {

      #There are next plots to display
      currentIndex(currentIndex() + 1)

      #Render the current plot and data
      # Get the current plot from the history list
      currentPlot <- filesHistory$plots[[currentIndex()]]

      #Render plot image
      output$gggenomes_plot <- renderImage({
        width  <- session$clientData$output_gggenomes_plot_width
        height <- session$clientData$output_gggenomes_plot_height

        png("myplot.png", width = width*5.5, height = height*5.5, res=400)
        print(currentPlot)
        dev.off()

        list(src = "myplot.png",
             width = width,
             height= height,
             alt = "Description of the image")
      }, deleteFile = FALSE)

      # Get the current file data from the history lists
      currentGenes <- filesHistory$genes[[currentIndex()]]
      currentSeqs <- filesHistory$aligned_sequences[[currentIndex()]]
      currentLinks <- filesHistory$links[[currentIndex()]]
      currentRegion <- filesHistory$region[[currentIndex()]]
      currentPrevColors <- filesHistory$prev_colors[[currentIndex()]]

      # Displayed infos on the UI (add coverage column)
      new_genes_seqs <- display_infos(currentGenes, currentSeqs, currentLinks, currentRegion)
      gene <- new_genes_seqs$genes
      aligned_sequence <- new_genes_seqs$aligned_sequences
      link <- new_genes_seqs$links

      #Render data
      output$genes <- renderUI({
        gene %>%
          flextable() %>%
          theme_zebra() %>%
          italic(j = 1, italic = TRUE) %>%
          autofit() %>%
          htmltools_value()})

      output$seqs <- renderUI({
        aligned_sequence %>%
          flextable() %>%
          theme_zebra() %>%
          italic(j = 1, italic = TRUE) %>%
          autofit() %>%
          htmltools_value()})

      output$links <- renderUI({
        link %>%
          flextable() %>%
          theme_zebra() %>%
          italic(j = c(1,6), italic = TRUE) %>%
          autofit() %>%
          htmltools_value()})

      #Update coordinates on the UI (chr, start, end, genes, gap and filter)
      updateCoordinates(session, currentRegion)

    }else {
      # No more plots in history, render default plot or clear the plot
      output$explanation <- renderText({
        paste0("No more plots and data stored in history.")
      })
      output$others  <- renderText({
        paste0("No inquiry search launched.")
      })

      output$gggenomes_plot <- renderPlot(NULL)
      output$genes <-  renderTable(NULL)
      output$seqs <-  renderTable(NULL)
      output$links <-  renderTable(NULL)

    }
  })

  observeEvent(filesHistory$plots, {

    if(length(filesHistory$plots) > 0) {

      # Assign the output of as.data.frame to a new variable
      temp_df_1 <- as.data.frame(do.call(rbind,filesHistory$region))
      temp_df_2 <- as.data.frame(do.call(rbind,filesHistory$modifications))
      colnames(temp_df_2)[1] <- "modifications"
      temp_df <- cbind(temp_df_1, temp_df_2)

      # Generate the DataTable
      output$history_table <- DT::renderDT({
        # Create the final data frame with the Plots column
        df <- data.frame(
          Seqnames = temp_df$seqnames,
          Start = temp_df$start,
          End = temp_df$end,
          Length = temp_df$length,
          Gap = temp_df$gap,
          Filtering_percentage = temp_df$filtering_percentage,
          Modifications = temp_df$modifications
        )

        # Use the DT package to display the data frame as a DataTable
        DT::datatable(df, escape = FALSE, selection = "single", options = list(
          searching = FALSE,
          paging=FALSE,
          rowCallback = JS('
        function(row, data, index) {
          $(row).on("click", function() {
            Shiny.setInputValue("selectedRow", index);
          });
        }
      ')
        ))
      })

      # Handle row click event
      observeEvent(input$selectedRow, {
        selectedRow <- input$selectedRow

        # Check if a row is selected
        if (!is.null(selectedRow)) {
          # Get the index of the selected row
          rowIndex <- as.integer(selectedRow) +1

          # Update the download plot
          output$downloadPlot <- downloadHandler(
            filename = function() {
              name <- paste0("Wormsynteny_plot_version_", rowIndex, ".pdf")
            },
            content = function(file) {
              # Download the corresponding plot
              ggsave(file, filesHistory$plots[[rowIndex]], width = 6500, height = 2500, units = "px", device = "pdf")

            }
          )

          # Update the download genes
          output$downloadGenes <- downloadHandler(
            filename = function() {
              name <- paste0("Wormsynteny_genes_version_", rowIndex, ".csv")
            },
            content = function(file) {
              new_df <- display_infos(filesHistory$genes[[rowIndex]], filesHistory$seqs[[rowIndex]], filesHistory$links[[rowIndex]], filesHistory$region[[rowIndex]])
              write_csv(new_df$genes, file)
            }
          )

          # Update the download aligned sequences
          output$downloadSeqs <- downloadHandler(
            filename = function() {
              name <- paste0("Wormsynteny_seqs_version_", rowIndex, ".csv")
            },
            content = function(file) {
              new_df <- display_infos(filesHistory$genes[[rowIndex]], filesHistory$aligned_sequences[[rowIndex]], filesHistory$links[[rowIndex]], filesHistory$region[[rowIndex]])
              write_csv(new_df$aligned_sequences, file)
            }
          )

          # Update the download links
          output$downloadLinks <- downloadHandler(
            filename = function() {
              name <- paste0("Wormsynteny_links_version_", rowIndex, ".csv")
            },
            content = function(file) {
              new_df <- display_infos(filesHistory$genes[[rowIndex]], filesHistory$seqs[[rowIndex]], filesHistory$links[[rowIndex]], filesHistory$region[[rowIndex]])
              write_csv(new_df$links, file)

            }
          )
        }

      })
    }
  })

}
