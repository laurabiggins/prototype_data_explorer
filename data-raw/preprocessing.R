library(tidyverse)
x <- read_delim("data-raw/S2_proteomics_41467_2023_40621_MOESM5_ESM.txt")

annot <- x |>
  select(1:3)

x_tidy <- x |>
  pivot_longer(-(1:3)) |>
  mutate(name = str_replace(name, "Copy.number", "Copy_number")) |>
  separate_wider_delim(name, names = c("data_type", "cell_type", "rep"), delim = ".")
  
  
summarised <- x_tidy |>
  group_by(`Protein IDs`, data_type, cell_type) |>
  summarise(
   # mean_val = mean(value),
    median_val = log10(median(value))
  ) |>
  ungroup() |>
  mutate(median_val = replace(median_val, is.infinite(median_val), 0)) |>
  pivot_wider(names_from = data_type, values_from = median_val)

# summarised_gp <- summarised |>
#   group_by(cell_type) 
#   
# fig4 <- group_split(summarised_gp )
# names(fig4) <- group_keys(summarised_gp )

fig4 <- summarised |>
  nest(.by = cell_type)

saveRDS(fig4, file = "Data/fig4.rds")  

fig4 <- readRDS("data/fig4.rds")

fig4 |>
  nplyr::nest_filter(data, `Protein IDs` == "A0A0R4J094")



# p <- fig4 |>
#   ggplot(mapping = aes(x=TPM, y=Copy_number)) +
#   scattermore::geom_scattermore(pointsize=2, colour = "grey") +
#   facet_wrap(vars(cell_type)) +
#   theme_bw()
# 
# filt1 <- function(fig4){
#   start_time <- Sys.time()
#   subset <- fig4 |>
#     filter(`Protein IDs` %in% proteins )
#   end_time <- Sys.time()
#   duration <- end_time - start_time
#   
#   print(duration)
#   
#   return(subset)
#   
# }
#  
# filt2 <- function(fig4){
#   start_time <- Sys.time()
#   subset <- fig4 |>
#     lazy_dt() |>
#     filter(`Protein IDs` %in% proteins )
#   end_time <- Sys.time()
#   duration <- end_time - start_time
#   
#   print(duration)
#   
#   return(subset)
#   
# }
# 
# #proteins <- sample(fig4$`Protein IDs`, size = 20)
#         
# x1 <- filt1(fig4)
# x2 <- filt2(fig4)
# 
# p + geom_point(data=subset, colour = "red")

# test1 <- function(){
#   start_time <- Sys.time()
#   
#   fig4[[2]] |>
#     purrr::map(purrr::pluck(1)) |>
#     unlist() |>
#     unique()
#   
#   end_time <- Sys.time()
#   duration <- end_time - start_time
#   
#   print(duration)
# }
# Time difference of 0.0008888245 secs
# 
# 
# test2 <- function(){
#   start_time <- Sys.time()
#   
#   fig4 |>
#     unnest_wider(col=data) |>
#     select(`Protein IDs`) |>
#     unnest_longer(everything()) |>
#     distinct() |>
#     pull(1)
#   
#   end_time <- Sys.time()
#   duration <- end_time - start_time
#   
#   print(duration)
# }
# # Time difference of 0.01906991 secs




