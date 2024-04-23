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
  pivot_wider(names_from = data_type, values_from = median_val)

fig4 <- summarised
saveRDS(fig4, file = "Data/fig4.rds")  


fig4 <- readRDS("data/fig4.rds")
p <- fig4 |>
  ggplot(mapping = aes(x=TPM, y=Copy_number)) +
  scattermore::geom_scattermore(pointsize=2, colour = "grey") +
  facet_wrap(vars(cell_type)) +
  theme_bw()

filt1 <- function(fig4){
  start_time <- Sys.time()
  subset <- fig4 |>
    filter(`Protein IDs` %in% proteins )
  end_time <- Sys.time()
  duration <- end_time - start_time
  
  print(duration)
  
  return(subset)
  
}
 
filt2 <- function(fig4){
  start_time <- Sys.time()
  subset <- fig4 |>
    lazy_dt() |>
    filter(`Protein IDs` %in% proteins )
  end_time <- Sys.time()
  duration <- end_time - start_time
  
  print(duration)
  
  return(subset)
  
}

#proteins <- sample(fig4$`Protein IDs`, size = 20)
        
x1 <- filt1(fig4)
x2 <- filt2(fig4)

p + geom_point(data=subset, colour = "red")





