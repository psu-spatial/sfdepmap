#######################################################
# SLR Autocorrelation example

#######################################################


#============================================================================
# SET-UP
#============================================================================
rm(list = ls())

library(tidyverse)   # data wrangling
library(tidycensus)  # download census data (needs a census API key)
library(tigris)      # download census boundaries
library(sf)          # spatial data
library(sfdep)       # neighbours, LISA and Getis-Ord
library(tmap)        # maps

# Load the autocorrelation plot functions
library(sfdepmap)   # Dr G's Moran, LISA and Getis-Ord functions



#============================================================================
# YOUR CHOICES
#
# To run this script for a different city, edit these two lines,
# then the two lines marked "EDIT" in section 2
#============================================================================
MyState      <- "IL"    # The two-letter code for your state

MyProjection <- 26916   # The code for your map projection.
#   26916 is UTM zone 16N, which suits Chicago.
#   For another city, use the UTM zone your city is in,
#   or use 5070 (Albers Equal Area), which works for any US city




#============================================================================
# 1. DOWNLOAD AMERICAN COMMUNITY SURVEY DATA
#============================================================================

#----------------------------------------------------------------------
# Download ACS data for every census tract in your state in 2024
# This will only work if you have signed up for an API key
#----------------------------------------------------------------------
State_Census.sf <- get_acs( survey = "acs5", year = 2024,
                            geography = "tract",
                            state     = MyState,
                            variables = c(
                              total_pop          = "B01003_001", # total population
                              total_house        = "B25001_001", # total housing units
                              income.total       = "B06010_001", # people aged 15+
                              income.gt75        = "B06010_011", # ...who have an income of $75,000 or more
                              edu.total          = "B06009_001", # people aged 25+
                              edu.bachelors      = "B06009_005", # ...whose highest degree is a bachelor's
                              edu.graduate       = "B06009_006", # ...whose highest degree is above a bachelor's
                              work.total         = "B08101_001", # workers aged 16+
                              work.home          = "B08101_049", # ...who work from home
                              internet.total     = "B28002_001", # households
                              internet.broadband = "B28002_004"),# ...that have broadband
                            geometry = TRUE,
                            output   = "wide")



#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#  The get_acs() command downloads TWO columns for each variable
#  To record this, tidycensus adds a letter to the end of every column name:
#        E = The best guess of the variable in that tract (Estimate)
#        M = How certain the census bureau are about that guess (Margin of error)
#
#  QUICK CHECK: See the column names here
#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
glimpse(State_Census.sf)



#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# To make life neater for this lab, we will select the estimates
# columns only & overwrite the original data.
#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
State_Census.sf <- dplyr::select(State_Census.sf,
                                 GEOID,
                                 NAME,
                                 total_pop      = total_popE,
                                 total_house    = total_houseE,
                                 income_total   = income.totalE,
                                 income_gt75    = income.gt75E,
                                 edu_total      = edu.totalE,
                                 edu_bachelors  = edu.bachelorsE,
                                 edu_graduate   = edu.graduateE,
                                 work_total     = work.totalE,
                                 work_home      = work.homeE,
                                 internet_total = internet.totalE,
                                 internet_broadband = internet.broadbandE)


# QUICK CHECK: Make sure it works
glimpse(State_Census.sf)







#============================================================================
# 2. DOWNLOAD CITY LIMITS/BORDERS
#
# Now, choose your spatial domain. I have selected the city of Chicago
# (You can also download borders from online GIS portals & load them using st_read)
#
# Note, dplyr:: means 'use the dplyr package', because otherwise R often tries to use
#       a different package for the select command and crashes.
#============================================================================


#----------------------------------------------------------------------
# Download the border for every town/city/village in your state
# Filter the list to find every row that has "Chicago" in the NAME column
#----------------------------------------------------------------------
State_Places  <- places(state = MyState, cb = TRUE, year = 2024)
CityBorder.sf <- dplyr::filter(State_Places, grepl("Chicago", NAME))     # EDIT: your city's name



#----------------------------------------------------------------------
# Then check that you are left with only one row.
# If nrow=0, the search is case sensitive!
#----------------------------------------------------------------------
nrow(CityBorder.sf)

#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Problem! There are 6 places containing the name Chicago.
#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CityBorder.sf

#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# I want the first row: City of Chicago, GEOID=1714000
# So, I adjust command filter to search for that instead
#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CityBorder.sf <- dplyr::filter(State_Places, grepl("1714000", GEOID))   # EDIT: your city's GEOID




#----------------------------------------------------------------------
# Final check: should be one row only
#    & make a map to check its what you expect
#----------------------------------------------------------------------
nrow(CityBorder.sf)
tmap_mode("view")
qtm(CityBorder.sf, fill="white", fill_alpha=0.3, col="blue", lwd=2,basemaps="OpenStreetMap")








#============================================================================
# 3. GEOGRAPHIC WRANGLING
#    Change the map projection
#    Crop the census data to your spatial domain
#    Calculate the area of each census tract
#============================================================================

#----------------------------------------------------------------------
# Change the map projection (I chose UTM zone 16N, see YOUR CHOICES at the top)
# and use st_make_valid to fix any errors
#----------------------------------------------------------------------
State_Census.sf <- st_transform(State_Census.sf, MyProjection) %>% st_make_valid()
CityBorder.sf   <- st_transform(CityBorder.sf,   MyProjection) %>% st_make_valid()


#----------------------------------------------------------------------
# Crop the State level census data to your city limits
#
#   We keep a tract if its centre point is inside the city border.
#   This keeps every tract WHOLE, so its population and percentages still match its shape.
#   (R gives a warning about "attributes are constant". It is safe to ignore)
#----------------------------------------------------------------------
# Find the centre point of every tract
Tract_Centres   <- st_centroid(State_Census.sf)

# Keep the centre points that are inside the city border
Centres_In_City <- st_filter(Tract_Centres, CityBorder.sf)

# Keep the tracts that match those centre points
Census.sf       <- dplyr::filter(State_Census.sf, GEOID %in% Centres_In_City$GEOID)


#----------------------------------------------------------------------
# Find the area of each tract in km2
# & save as a new column called AREA_km2
#----------------------------------------------------------------------
Census.sf$AREA_km2 <- as.numeric(st_area(Census.sf))/1000000





#============================================================================
# 4. WORK OUT DENSITIES AND PERCENTAGES
#============================================================================
#----------------------------------------------------------------------
# Now we can choose/calculate the variables we want
#----------------------------------------------------------------------
Census.sf <- transmute(Census.sf,
                       GEOID, NAME, AREA_km2,
                       total_pop,
                       total_house,
                       pop_density_km2        = total_pop / AREA_km2,
                       house_density_km2      = total_house / AREA_km2,
                       percent_income_gt75    = 100 * income_gt75 / income_total,
                       percent_BAdegree       = 100 *(edu_bachelors + edu_graduate) / edu_total,
                       percent_workhome       = 100 * work_home / work_total,
                       percent_housebroadband = 100 * internet_broadband / internet_total)




#============================================================================
# 5. REMOVE MISSING DATA
#
#   Moran's I and LISA stop with an error if even one tract has a missing value.
#
#   Missing values happen when:
#      - a tract has no people (parks, airports, the lake), because 0 / 0 has no answer
#      - the census did not publish a number for that tract
#
#   This step comes AFTER the percentages are calculated, so that it
#   also removes any tract where a percentage could not be worked out.
#============================================================================
#----------------------------------------------------------------------
# This code:
#  Removes any "empty" tract
#  Removes any tract with zero area
#  Removes census tracts with very few people
#     (Small totals mess up percentages, so I'm removing tracts with <30 people)
#  Removes any tract containing ANY missing value
#  (Re)-fixes any broken shapes
#----------------------------------------------------------------------
Census.sf <- Census.sf %>%
  dplyr::filter(!st_is_empty(geometry)) %>%
  dplyr::filter(AREA_km2 > 0) %>%
  dplyr::filter(total_pop >= 30)  %>%
  drop_na() %>%
  st_make_valid()


#----------------------------------------------------------------------
# Because we are looking at spatial contiguity neighbours
# Remove "islands": tracts that do not touch any other tract
#----------------------------------------------------------------------
# Count how many other tracts each tract touches
Census.sf$n_touching <- lengths(st_touches(Census.sf))
table(Census.sf$n_touching)

# Keep the tracts that touch at least one other tract
Census.sf <- dplyr::filter(Census.sf, n_touching > 0)


#----------------------------------------------------------------------
# Update the borders to match
#----------------------------------------------------------------------
CityBorder.sf <- st_union(Census.sf)




#============================================================================
# 6. QUICK CHECKS
# Optional to keep these in your final report,
#============================================================================
#----------------------------------------------------------------------
# Make sure everything looks reasonable
# (e.g. percentages range from 0-100, no missing data etc)
#----------------------------------------------------------------------
glimpse(Census.sf)
summary(Census.sf)


#----------------------------------------------------------------------
# Map a few variables & check the spatial patterns look as expected
#----------------------------------------------------------------------
tm_basemap("OpenStreetMap") +
  tm_shape(Census.sf) +
  tm_polygons(fill = "percent_income_gt75",col=NULL,fill_alpha=.7) +
  tm_shape(CityBorder.sf) +
  tm_borders(col = "black")



################################################################################
# AUTOCORRELATION
#
# Sections 8 to 12 use the functions in the sfdepmap package
# (loaded by the source() line at the top of this script).
#
# EVERY ONE OF THOSE FUNCTIONS IS GIVEN THE SAME SIX THINGS:
#
#     data        your sf object                               Census.sf
#     variable    the NAME of the column to study, in "quotes" "percent_income_gt75"
#     label       the NAME of a column that labels each tract  "GEOID"
#     neighbours  the neighbours you make in section 7         NeighbourMatrix
#     weights     the weights you make in section 7            WeightsMatrix
#     title       a description of your variable for the plots "Percentage of people making >$75K"
#
# To study a different variable, change "variable" and "title". That is all.
################################################################################


#============================================================================
# 7. SPATIAL WEIGHTS MATRIX
#============================================================================
#----------------------------------------------------------------------
# STEP 1: Decide who is a neighbour of who.
#
# I am using QUEEN contiguity: tracts that share a border or a corner.
#----------------------------------------------------------------------
NeighbourMatrix <- st_contiguity(Census.sf, queen = TRUE)

#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# OTHER WAYS TO CHOOSE NEIGHBOURS
# To use one, put a # in front of the line above, and remove the #
# from the lines you want below. Everything after this works the same.
#
#   ROOK contiguity: tracts that share a border (corners do not count)
#       NeighbourMatrix <- st_contiguity(Census.sf, queen = FALSE)
#
#   The next two measure the distance between the centre points of the tracts,
#   so first find the centre points:
#       Tract_Centres <- st_centroid(st_geometry(Census.sf))
#
#   K NEAREST NEIGHBOURS: every tract gets exactly 6 neighbours, the 6 closest
#       NeighbourMatrix <- st_knn(Tract_Centres, k = 6)
#
#   DISTANCE BAND: every tract whose centre is within 2 km.
#   (The distance is in the units of your map projection. That is metres
#    for UTM and for 5070, so 2000 = 2 km)
#       NeighbourMatrix <- st_dist_band(Tract_Centres, upper = 2000)
#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


#----------------------------------------------------------------------
# STEP 2: Calculate spatial weights:
#   style = "W": each tract's neighbours get an equal share, adding up to 1.
#   allow_zero = TRUE stops errors if a tract has no neighbours
#----------------------------------------------------------------------
WeightsMatrix <- st_weights(NeighbourMatrix, style = "W", allow_zero = TRUE)

#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ANOTHER WAY TO CHOOSE WEIGHTS
#
#   style = "B" (binary): every neighbour gets a weight of 1
#       WeightsMatrix <- st_weights(NeighbourMatrix, style = "B", allow_zero = TRUE)
#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


#----------------------------------------------------------------------
# STEP 3: Plot the neighbours, to check they look sensible.
# Each line joins two tracts that are neighbours.
#----------------------------------------------------------------------
plot(st_geometry(Census.sf), border = 'grey')
plot(NeighbourMatrix, st_geometry(Census.sf),
     col = 'black', lwd = 1, cex = .2, add = TRUE)



#============================================================================
# 8. GLOBAL MORAN'S I
#    Is there clustering across the WHOLE study area?
#============================================================================
#----------------------------------------------------------------------
# Moran's scatterplot
#   x axis: the value in each tract
#   y axis: the average value in that tract's neighbours
#   The most unusual tracts are labelled with their GEOID
#----------------------------------------------------------------------
moran_scatterplot(data       = Census.sf,
                  variable   = "percent_income_gt75",
                  label      = "GEOID",
                  neighbours = NeighbourMatrix,
                  weights    = WeightsMatrix,
                  title      = "Percentage of people making >$75K")


#----------------------------------------------------------------------
# Moran's test
#----------------------------------------------------------------------
global_moran_test(Census.sf$percent_income_gt75,
                  NeighbourMatrix,
                  WeightsMatrix,
                  zero.policy = T)



#============================================================================
# 9. LISA (Local Moran's I)
#    WHERE are the clusters, and where are the outliers?
#
#      High-High = high value, surrounded by high values   (a cluster)
#      Low-Low   = low value,  surrounded by low values    (a cluster)
#      High-Low  = high value, surrounded by low values    (an outlier)
#      Low-High  = low value,  surrounded by high values   (an outlier)
#============================================================================

#----------------------------------------------------------------------
# THE SIMPLEST VERSION. This does two things:
#    1. prints a table of how many tracts are in each group
#    2. saves the finished map, here with the name LISAmap
#
# The p-values come from random shuffles of the data, so the map changes a
# little each time you run this. The "random seed" that was used is always
# written under the map.
#----------------------------------------------------------------------
LISAmap <- lisa_map(data       = Census.sf,
                    variable   = "percent_income_gt75",
                    label      = "GEOID",
                    neighbours = NeighbourMatrix,
                    weights    = WeightsMatrix,
                    title      = "Percentage of people making >$75K")

# To see the map, type its name
LISAmap


#----------------------------------------------------------------------
# GETTING THE SAME MAP EVERY TIME
# Give a seed (any whole number). The same seed gives the same map.
# To re-make a map you made earlier, use the seed written under it.
#----------------------------------------------------------------------
LISAmap <- lisa_map(data       = Census.sf,
                    variable   = "percent_income_gt75",
                    label      = "GEOID",
                    neighbours = NeighbourMatrix,
                    weights    = WeightsMatrix,
                    title      = "Percentage of people making >$75K",
                    seed       = 364)

LISAmap


#----------------------------------------------------------------------
# THE SAME MAP WITH EXTRA OPTIONS
# Every option is optional. Add the ones you want, in any order.
#
#   data_source   a line saying where your data came from, shown under the map
#   mode          "plot" = a static map for a report (the default)
#                 "view" = a map you can zoom and click on
#   significance  the p-value cut-off. The default is 0.05
#   nsim          how many random shuffles are used for the p-values (default 499)
#   global_stats  TRUE adds Moran's I, Geary's C and the Getis-Ord General G
#                 for the whole study area to the notes under the map
#   palette       a palette number (1 to 6), a palette name, or 5 colours of
#                 your own. To see the palettes, run ?lisa_map
#   basemap       FALSE = no background map (the default)
#                 TRUE  = tmap's standard background map
#                 "OpenStreetMap" (or another name) = that background map
#   seed          a whole number = use that random seed (the same map every time)
#                 left out       = R picks a new seed each time
#   silent        TRUE = do not print the table
#   return_data   TRUE = give back the LISA results as data, in place of the map
#                 (your label column, your variable, the new columns, the geometry)
#----------------------------------------------------------------------
LISAmap <- lisa_map(data         = Census.sf,
                    variable     = "percent_income_gt75",
                    label        = "GEOID",
                    neighbours   = NeighbourMatrix,
                    weights      = WeightsMatrix,
                    title        = "Percentage of people making >$75K",
                    data_source  = "Data: American Community Survey 2020-2024 (5-year estimates), census tracts, 1st order Queens.",
                    mode         = "plot",
                    significance = 0.01,
                    global_stats = TRUE,
                    palette      = 3,
                    seed         = 364)

LISAmap


#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# To explore the results, make a "view" map and click on the tracts
#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
lisa_map(data       = Census.sf,
         variable   = "percent_income_gt75",
         label      = "GEOID",
         neighbours = NeighbourMatrix,
         weights    = WeightsMatrix,
         title      = "Percentage of people making >$75K",
         mode       = "view",
         seed       = 364,
         silent     = TRUE)


#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# To get the numbers themselves, ask for the data in place of the map.
# The new columns are: LocalMoran, LocalMoran_pvalue,
#                      LocalMoran_Quadrant and Quadrant_sig
#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LISAdata <- lisa_map(data        = Census.sf,
                     variable    = "percent_income_gt75",
                     label       = "GEOID",
                     neighbours  = NeighbourMatrix,
                     weights     = WeightsMatrix,
                     seed        = 364,
                     silent      = TRUE,
                     return_data = TRUE)

glimpse(LISAdata)



#============================================================================
# 10. GETIS-ORD Gi* ("G-i-star"): hot spots and cold spots
#
#     Hot spot  = high values clustered together
#     Cold spot = low values clustered together
#
#     Gi* counts each tract TOGETHER WITH its neighbours.
#     It asks: "is this tract part of a cluster?"
#     This is the version used by the Esri Hot Spot Analysis tool.
#
#     The p-values come from random shuffles of the data, so give a seed
#     to get the same map every time. The options are the same as for
#     lisa_map(), apart from significance and global_stats, plus:
#        adjust_p = TRUE   adjust the p-values for the number of tests
#                          (Benjamini-Hochberg). Stricter: fewer hot and cold spots.
#     There are 5 palettes (run ?getisstar_map to see them).
#============================================================================

GETISSTARmap <- getisstar_map(data        = Census.sf,
                              variable    = "percent_income_gt75",
                              label       = "GEOID",
                              neighbours  = NeighbourMatrix,
                              weights     = WeightsMatrix,
                              title       = "Percentage of people making >$75K",
                              data_source = "Data: American Community Survey 2020-2024 (5-year estimates), census tracts, 1st order Queens.",
                              seed        = 364)

GETISSTARmap



#============================================================================
# 11. GETIS-ORD Gi (no star)
#
#     Gi leaves each tract's own value OUT, and uses only its neighbours.
#     It asks: "is this tract surrounded by a cluster?"
#
#     Compare this map with the Gi* map. They are usually similar. The tracts
#     that change are the ones whose own value is unlike their neighbours'.
#
#     NOTE THE NAMES:   getisstar_map() = Gi*      getis_map() = Gi
#============================================================================

GETISmap <- getis_map(data        = Census.sf,
                      variable    = "percent_income_gt75",
                      label       = "GEOID",
                      neighbours  = NeighbourMatrix,
                      weights     = WeightsMatrix,
                      title       = "Percentage of people making >$75K",
                      data_source = "Data: American Community Survey 2020-2024 (5-year estimates), census tracts, 1st order Queens.",
                      palette     = 3,
                      seed        = 364)

GETISmap



#============================================================================
# 12. THE FOUR-MAP SUMMARY FIGURE
#
#     One command makes one figure with four maps, using the standard settings:
#        A. your data          B. LISA clusters
#        C. Gi* hot spots      D. Gi hot spots
#     and one caption underneath, which includes the global statistics.
#
#     Options: data_source, mode, basemap, significance, nsim and seed
#
#     An option for map A (the map of your data):
#        units = "Percentage"   what your numbers are, used as the legend title
#
#     Two more options choose what is written in the caption under the figure:
#        explanation  = FALSE   leave out the lines that explain maps A to D
#        global_stats = FALSE   leave out Moran's I, Geary's C and the General G
#     The random seed and your data source are always written.
#
#     In R Markdown, give this chunk a big plot:  {r, fig.width=9, fig.height=11}
#============================================================================
autocorrelation_summary(data        = Census.sf,
                        variable    = "percent_income_gt75",
                        label       = "GEOID",
                        neighbours  = NeighbourMatrix,
                        weights     = WeightsMatrix,
                        title       = "Percentage of people making >$75K",
                        data_source = "Data: American Community Survey 2020-2024 (5-year estimates), census tracts, 1st order Queens.",
                        units       = "Percentage",
                        seed        = 364)


#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# The same four maps as "view" maps. They are linked, so when you
# zoom or move one map, the other three follow.
#-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
autocorrelation_summary(data       = Census.sf,
                        variable   = "percent_income_gt75",
                        label      = "GEOID",
                        neighbours = NeighbourMatrix,
                        weights    = WeightsMatrix,
                        title      = "Percentage of people making >$75K",
                        mode       = "view",
                        seed       = 364)
