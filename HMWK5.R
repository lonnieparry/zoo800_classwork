HOMEWORK 5

###PROBLEM 1###
fish<-read.csv("https://raw.githubusercontent.com/xucamel/Quantitative_course_week5/refs/heads/main/HW/Data/fish.csv")
print(fish)
fishrds <- readRDS("C:/Users/egill/Downloads/fish (3).rds")
print(fishrds)
library(readxl)
fishxl<-read_excel("fish.xlsx")
print(fishxl)

###PROBLEM 2###
write.csv(fish, "fish.csv", row.names = FALSE)
saveRDS(fish, "fish.rds")
write_xlsx(fish, "fish.xlsx")

#files from largest to smallest are the csv, the excel file, and the RDS. for sharing a csv file is better, for storage an RDS is better.
fish_1 <- fish %>% # fish_1 is not a good naming strategy in practice 
  filter(Species == "Walleye", Lake == "Erie") %>%
  select(Species, Lake, Year, Length_cm, Weight_g) 
head(fish_1) 

fish_2 <- fish %>%
  filter(Species == "Walleye", Lake == "Erie") %>%
  select(-Age_years) 
head(fish_2) 


###PROBLEM 3###
fish_output<-read.csv("fish.csv")
library(dplyr)

fish_output <- fish_output %>%
  filter(
    Species %in% c("Walleye", "Yellow Perch", "Smallmouth Bass"),
    Lake %in% c("Erie", "Michigan")
  ) %>%
  select(Species, Lake, Year, Length_cm, Weight_g)%>%
  mutate(
    Length_mm = Length_cm * 10,
    Length_group = cut(
      Length_mm,
      breaks = c(-Inf, 200, 400, 600, Inf),
      labels = c("≤200", "200–400", "400–600", ">600"),
      right = TRUE
    )
  )
  count(Lake,Species, Length_group, Weight_g, Year)
  
  yearly_stats <- fish_output %>%
    filter(
      Species %in% c("Walleye", "Yellow Perch", "Smallmouth Bass"),
      Lake %in% c("Erie", "Michigan")
    ) %>%
    select(Species, Year, Weight_g) %>%
    group_by(Species, Year) %>%
    summarise(
      mean_weight = mean(Weight_g, na.rm = TRUE),
      median_weight = median(Weight_g, na.rm = TRUE),
      sample_size = n(),
      .groups = "drop"
    )%>%
    write.csv(yearly_stats, "ZOO800/yearly_stats.csv")

####PROBLEM 4####
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
  
  data_path <- "C:/Users/egill/Downloads"  
  fish_files <- list.files(path = data_path,
                           pattern = "^fish_\\d{4}\\.csv$",
                           full.names = TRUE)
  fish_output <- map_dfr(fish_files, function(file) {
    year <- str_extract(basename(file), "\\d{4}")
    
    read_csv(file) %>%
      mutate(Year = as.numeric(year))
  })

###PROBLEM 5###
  library(parallel)
  

  fish <- read.csv("fish_bootstrap_parallel_computing.csv")
  fish <- subset(fish, Lake == "Erie")
  species <- unique(fish$Species)     
  boot_mean <- function(species_name, n_boot = 10000, sample_size = 200) {
  
x <- fish$Weight_g[fish$Species == species_name]
sample_size <- min(sample_size, length(x))
    
# Resample WITH replacement and compute mean weight each time
    means <- replicate(n_boot, mean(sample(x, size = sample_size, replace = TRUE)))
    
    # Return the mean of those bootstrap means
    mean(means)
  }
  
  t_serial <- system.time({
    res_serial <- lapply(
      species,
      boot_mean,
      n_boot = 10000,
      sample_size = 200
    )
  })
  
  n_cores <- max(1, detectCores() - 1)    # use all but one core
  cl <- makeCluster(n_cores)
  
  clusterSetRNGStream(cl, iseed = 123)
  clusterExport(cl, varlist = c("fish", "boot_mean", "species"), envir = environment())
  
  t_parallel <- system.time({
    res_parallel <- parLapply(
      cl,
      species,
      boot_mean,
      n_boot = 10000,
      sample_size = 200
    )
  })
  
  stopCluster(cl)
  
  elapsed_serial   <- unname(t_serial["elapsed"])
  elapsed_parallel <- unname(t_parallel["elapsed"])
  speedup <- elapsed_serial / elapsed_parallel
  
  cat("Serial elapsed (s):   ", round(elapsed_serial, 3), "\n")
  cat("Parallel elapsed (s): ", round(elapsed_parallel, 3), " using ", n_cores, " cores\n", sep = "")
  cat("Speedup:               ", round(speedup, 2), "x\n", sep = "")
  