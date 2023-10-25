### This code was created with the objective to download, clean and organize occurrence data in order to create files that can be processed by the Biogeographical reconstruction software PhyGeo.

## download libraries and log on GBIF ----

#files provided necessary to run the script: "species_TaxonKey.csv", "SpeciesList.csv" and "World_Continents.geojson"

# Change the working directory to where you want your data stored
# setwd("/path/to/your/data/folder")

# List of packages to download
packages <- c(
  "rgeos",
  "raster",
  "rgdal",
  "rgbif",
  "stringr",
  "dplyr",
  "countrycode",
  "CoordinateCleaner",
  "ggplot2",
  "sp",
  "sf",
  "here",
  "maptools"
)

# Install required packages (only needed once)
#install.packages(packages)

# Load all packages
lapply(packages, library, character.only=TRUE)

# Cleanup
remove(packages)

# Log in to GBIF (uncomment and fill in your credentials, only once)
# GBIF_USER <- "your_username"
# GBIF_PWD <- "your_password"
# GBIF_EMAIL <- "your_email"


## Download Preserved specimen data from GBIF ----

# Search for the "TaxonKey" for the desired taxon
# name_backbone("Turdidae") #For "Turdidae" Taxonkey = 5290

# Request data from GBIF
 dl <- occ_download(
   pred("hasGeospatialIssue", FALSE),
   pred("hasCoordinate", TRUE),
   pred("occurrenceStatus", "PRESENT"),
   pred_gte("distanceFromCentroidInMeters", "2000"),
   pred("basisOfRecord", "PRESERVED_SPECIMEN"),
   pred("taxonKey", 5290),
   format = "SIMPLE_CSV"
 )

# Create a citation info file for Preserved Specimens
 sink(here("citation_info_PS.txt"))
 dl
 sink()

# Queue the download
 occ_download_wait(dl)

# Download the data
 file <- occ_download_get(dl, overwrite = FALSE)
#file <- occ_download_get("0004468-230918134249559", overwrite = FALSE) BORRAR DESPUES

# Import the data as a data frame
df <- file %>%
  occ_download_import()

# Cleanup
 remove(dl)
remove(file)


## Download data of specific species from eBird ----

# Load the list of species for which you want to download additional data (You will need to search for the taxonkey of each one using the name_backbone function)
 species_taxkey <- read.csv(here("species_TaxonKey.csv"), stringsAsFactors = FALSE)

# Convert species list to numeric for occ_download
 species_taxkey <- as.numeric(unlist(lapply(species_taxkey[2], function(x) {
   if (is.factor(x)) as.numeric(as.character(x)) else x
 })))

# Download data from GBIF for the specified species
 dl2 <- occ_download(
   pred("hasGeospatialIssue", FALSE),
   pred("hasCoordinate", TRUE),
   pred("occurrenceStatus", "PRESENT"),
   pred_gte("distanceFromCentroidInMeters", "2000"),
   pred("basisOfRecord", "HUMAN_OBSERVATION"),
   pred("collectionCode", "EBIRD"),
   pred_in("taxonKey", species_taxkey),
   format = "SIMPLE_CSV"
 )

# Create a citation info file for Human Observations
 sink(here("citation_info_HO.txt"))
 dl2
 sink()

# Queue the download
 occ_download_wait(dl2)

# Download the data
 file2 <- occ_download_get(dl2, overwrite = FALSE)
#file2 <- occ_download_get("0004531-230918134249559", overwrite = FALSE) BORRAR DESPUES

# Import the data as a data frame
df2 <- file2 %>%
  occ_download_import()

# Cleanup
 remove(species_taxkey)
 remove(dl2)
 remove(file2)

 
## Download more additional data of specific species from INaturalist ----

# Species of interest: Turdus ludoviciae=5789051 , Turdus lherminieri=7341977 , Geokichla erythronota=6100872 , Geokichla dohertyi=6100887, Geokichla schistacea=6100878
 
# Create a list of taxkeys of specific species
  INat_taxkey <- c(5789051, 7341977, 6100872, 6100887, 6100878)

# Download data from GBIF for the specified species
  dl3 <- occ_download(
    pred("hasGeospatialIssue", FALSE),
    pred("hasCoordinate", TRUE),
    pred("occurrenceStatus", "PRESENT"),
    pred_gte("distanceFromCentroidInMeters", "2000"),
    pred("basisOfRecord", "HUMAN_OBSERVATION"),
    pred("collectionCode", "Observations"),
    pred("datasetKey", "50c9509d-22c7-4a22-a47d-8c48425ef4a7"),
    pred_in("taxonKey", INat_taxkey),
    format = "SIMPLE_CSV"
  )

# Create a citation info file for Human Observations INaturalist
  sink(here("citation_info_INat.txt"))
  dl3
  sink()

# Queue the download
  occ_download_wait(dl3)

# Download the data
  file3 <- occ_download_get(dl3, overwrite = FALSE)
 #file3 <- occ_download_get("0006554-230918134249559", overwrite = FALSE) BORRAR DESPUES

# Import the data as a data frame
  df3 <- file3 %>%
  occ_download_import()

# Cleanup
 remove(INat_taxkey)
 remove(dl3)
 remove(file3)

## Merge all downloaded GBIF data ----

# Combine dataframes
dat <- rbind(df, df2, df3)

# Remove unused variables
remove(df)
remove(df2)
remove(df3)


## Organize species list ----

# Only keep necessary columns
dat <- dplyr::select(dat, c("gbifID","species","decimalLatitude","decimalLongitude", "countryCode", "verbatimScientificName"))

# Save the combined data to a CSV file
 write.csv(dat, here("TurdidaeData.csv"), row.names = FALSE)

# Remove species not present in the tree
blacklist <- c("", "Alethe castanea", "Alethe diademata", "Pseudalethe choloensis", "Pseudalethe fuelleborni", "Pseudalethe poliocephala", "Pseudalethe poliophrys", "Cochoa azurea", "Cochoa purpurea", "Geokichla dumasi", "Myadestes lanaiensis", "Myadestes palmeri", "Turdus confinis", "Turdus flavirostris", "Turdus ravidus", "Zoothera major")

dat_cl <- subset(dat, !(dat$species %in% blacklist))

# Create a list of unique species
species <- sort(unique(dat_cl$species))

# Correct list (Several of the species appear with their old names in the "Species" category. We use "verbatimScientificName" to correct it)

# List of species names you want to search for
species_to_search <- c("Catharus maculatus", "Turdus simensis", "Turdus niveiceps", "Zoothera griseiceps")  # Add your species names here

# Dictionary associating search terms to rename species to their current accepted names
species_dict <- c(
  "eremita"="Turdus eremita",
  "turdoides"="Turdus turdoides",
  "heinrichi"="Zoothera heinrichi",
  "litsitsirupa"="Turdus litsitsirupa",
  "litsipsirupa"="Turdus litsitsirupa"
)

# Loop through each species name
for (species_name in species_to_search) {
  # Search through verbatimScientificName for the current species name
  matching_rows <- str_detect(dat_cl$verbatimScientificName, species_name)
  
  # Update the "species" column for the matching rows
  dat_cl$species[matching_rows] <- species_name
}

# Loop through each species name again, using dictionary to rename
for (species_name in names(species_dict)) {
  # Search through verbatimScientificName for the obsolete species name
  matching_rows <- str_detect(dat_cl$verbatimScientificName, species_name)
  
  # Update the "species" column for the matching rows
  dat_cl$species[matching_rows] <- species_dict[species_name]
}

# Update species list (again)
species <- sort(unique(dat_cl$species))

## Remove column "verbatimScientificName"
dat_cl <- dplyr::select(dat_cl, -verbatimScientificName)

# Cleanup
remove(dat)
remove(blacklist)
remove(matching_rows)
remove(species_name)
remove(species_to_search)
remove(species_dict)


## Data Cleanup ----

# Tristan de Cunha Island is not in rnaturalearth::ne_countries(scale = "medium"), the default country_ref for clean_coordinates.
# We don't want all Nesocichla eremita entries to be wiped out.
# Ideally, we should provide a better suited country_ref to clean_coordinates.
# But for now, bypass country identity test for it.
# Same goes for Somalia and Somaliland as the latter one has no ISO2/3 code to speak of.
dat_test <- subset(dat_cl, !(dat_cl$countryCode %in% c("SH", "SO")))
dat_excl <- subset(dat_cl, (dat_cl$countryCode %in% c("SH", "SO")))

# Convert country code from ISO2c to ISO3c
dat_test$countryCode <- countrycode(dat_test$countryCode, origin = 'iso2c', destination = 'iso3c')
dat_excl$countryCode <- countrycode(dat_excl$countryCode, origin = 'iso2c', destination = 'iso3c')

# Clean coordinates
flags <- clean_coordinates(
  x = dat_test,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  countries = "countryCode",
  species = "species",
  tests = c("capitals", "centroids", "equal", "gbif", "institutions", "zeros", "countries")
)

# Filter data based on cleaning flags
dat_test <- dat_test[flags$.summary,]
dat_flags <- flags[!flags$.summary,]

# Merge entries back into dat_cl and dat_flags
dat_cl <- rbind(dat_test, dat_excl)

## Remove column "countryCode"
dat_cl <- dplyr::select(dat_cl, -countryCode)
dat_flags <- dplyr::select(dat_flags, -countryCode)

# Save cleaned data to CSV
 write.csv(dat_cl, here("TurdidaeData_Clean.csv"), row.names = FALSE)

 # Save data with flags to CSV
 write.csv(dat_flags, here("TurdidaeData_Flagged.csv"), row.names = FALSE)

# Cleanup
remove(dat_excl)
remove(dat_test)
remove(dat_flags)
remove(flags)


## Filter by distribution ----

# You must provide 'ranges' folder in your working directory with all species range polygons as KML files (obtained from Bird Life International)
# All names must match SpeciesList.csv, but with underscores instead of spaces

# Create directories
dir.create(here("maps"))  # To save generated maps

# Create an empty data frame to store results
dat_cl2 <- data.frame()

# Read the list of Species present in the tree
SPlist <- read.csv(here("SpeciesList.csv"))

SPlistcount <- nrow(SPlist)

# Loop through species
for (i in 1:SPlistcount) {
  SPname <- SPlist[i,]
  print(paste("[", i, "/", SPlistcount, "]", SPname))
  
  SPnameunderscore <- gsub(' ', '_', SPname)
  
  SP <- dat_cl[dat_cl$species %in% SPname, ] # Extract data for the selected species
  
  # Load species range .KML file
  SPrange <- here(paste("ranges/", SPnameunderscore, ".kml", sep = ""))
  
  if (!file.exists(SPrange)) {
    next
  }
  
  if (!(SPname %in% species)) {
    next
  }
  
  ogrListLayers(dsn = SPrange) # Find the layer name
  SPdata <- readOGR(dsn = SPrange)
  summary(SPdata)
  
  # Load .gpkg files
  st_layers(here("World_Continents.geojson"))
  WorlMap <- st_read(dsn = here("World_Continents.geojson"))
  
  # Remove points outside the area
  
  # Create a SpatialPointsDataFrame (SPDF) with occurrence data
  SPDF <- SpatialPointsDataFrame(
    coords = SP[, c("decimalLongitude", "decimalLatitude")],
    data = SP,
    proj4string = CRS(SPdata@proj4string@projargs)
  )
  
  # Filter points within the area
  SPDF <- SPDF[!is.na(over(SPDF, as(SPdata, "SpatialPolygons"))), ]
  
  # Convert from SpatialPointsDataFrame to a dataframe
  SP_clean <- as.data.frame(SPDF)
  
  # Map cleaned datapoints over species range
  WorlMap %>%
    ggplot() +
    geom_sf() +
    geom_polygon(SPdata, mapping = aes(x = long, y = lat, group = group), fill = "darkolivegreen1") +
    geom_point(data = SP_clean, aes(x = decimalLongitude, y = decimalLatitude), colour = "red", size = 0.5) +
    ggtitle("Species Occurrence", subtitle = SPname) +
    labs(y = "Latitude", x = "Longitude") +
    theme_bw() -> Dist2
  
  Dist2
  
  dat_cl2 <- bind_rows(dat_cl2, SP_clean)
  
  while (!is.null(dev.list())) dev.off()
  
  # Save maps as images
  ggsave(here(paste("maps/", SPnameunderscore, "_map.png", sep = "")), plot = Dist2)
}

# This maps allows us to see the amount and distribution of our occurrence points for each species, but are not strictly necessary for running PhyGeo


## GBIF to PhyGeo: ----

# Create a CSV file in the format required by PhyGeo

# Reorder columns
dat_cl2 <- dat_cl2[, c(2, 4, 3, 1)]

# Save the final cleaned data to a CSV file
write.csv(dat_cl2, here("TurdidaeData_final.csv"), row.names = FALSE)
