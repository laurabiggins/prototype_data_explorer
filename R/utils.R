
papercard <- #list(
  card(
    max_height = 350,
    full_screen = TRUE,
    card_header("An integrated proteome and transcriptome of B cell maturation defines poised activation states of transitional and mature B cells"),
    p("Fiamma Salerno, Andrew J. M. Howden, Louise S. Matheson, Özge Gizlenci, Michael Screen, Holger Lingel, Monika C. Brunner-Weinzierl & Martin Turner"),
    a("DOI: 10.1038/s41467-023-40621-2", target = "_blank", href = "https://doi.org/10.1038/s41467-023-40621-2"), 
    h3("Abstract"),
    p("During B cell maturation, transitional and mature B cells acquire cell-intrinsic features that determine their ability to exit quiescence and mount effective immune responses. Here we use label-free proteomics to quantify the proteome of B cell subsets from the mouse spleen and map the differential expression of environmental sensing, transcription, and translation initiation factors that define cellular identity and function. Cross-examination of the full-length transcriptome and proteome identifies mRNAs related to B cell activation and antibody secretion that are not accompanied by detection of the encoded proteins. In addition, proteomic data further suggests that the translational repressor PDCD4 restrains B cell responses, in particular those from marginal zone B cells, to a T-cell independent antigen. In summary, our molecular characterization of B cell maturation presents a valuable resource to further explore the mechanisms underpinning the specialized functions of B cell subsets, and suggest the presence of ‘poised’ mRNAs that enable expedited B cell responses."
    )
  )
#)
#
value_boxes <- list(
  single_cell = value_box(
    title = NULL,
    value = actionButton("single_cell", "Single cell"),
    showcase = tags$img(src = "single_cell_umap.PNG", width = "100", height = "100")
  ),
  proteomics = value_box(
    title = NULL,
    value = "Proteomics",
    showcase = tags$img(src = "proteomics_pic.PNG", width = "100", height = "100")
    #tags$img(src = "bioinformatics_logo_small.png", width = "200", height = "71")
  ),
  imaging = value_box(
    title = NULL,
    value = "Imaging",
    showcase = tags$img(src = "imaging_pic.PNG", width = "100", height = "100")
  )
)











