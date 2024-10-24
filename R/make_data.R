# read toydata.txt and save as internal dataset
toydata <- read.table("raw_data/toydata.txt", header = TRUE)

# save the dataset in the correct format
usethis::use_data(toydata, overwrite = TRUE)
