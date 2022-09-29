
########################################################################################

# Size Classification for Wolman Pebble Count data 

########################################################################################

# This script can be used as a template to convert field measurements of substrate
# size into substrate categories according to the USGS manual for sediment classification
# and characterization (https://pubs.usgs.gov/sir/2019/5073/sir20195073.pdf). Further,
# this script provides a template for calculating percent substrate composition for
# each substrate group for a given sampling area.

# This template provides code to sort substrate according to more general categories such
# as "cobble", "boulder", "gravel", "sand"; however, this code is easily adaptable to any 
# grain size classification depending on your desired resolution.


# As always, please reach out if you have any questions

# - Spencer

# sgwomble42@tntech.edu

# Citation:
# Valentine, P.C., 2019, Sediment classification and the characterization, identification, and mapping of geologic
# substrates for the glaciated Gulf of Maine seabed and other terrains, providing a physical framework for ecological
# research and seabed management: U.S. Geological Survey Scientific Investigations Report 2019-5073, 37 p.,
# https://doi.org/10.3133/sir20195073.

###########################################################################################

# we will be working with the tidyverse package for this template
library(tidyverse)

#######################################################

# first we'll create some data1 to work with

# creating a vector with our rock measurements in millimeters  
size_mm<-c(1,360,	60,	450,	170,	900,	3262,	1,	130,	785,	430,	135,	1,	200,	3000,	55,	3000,
4500,	170,	440,	1,	1,	270,	180,	1,	3000,	1,	250,	250,	1,	1,	2996,	2500,	1753,
3000,	4000,	4443,	260,	1,	270,	1,	320,	74,	3178,	52,	185,	60,	85,	79)

# creating a vector for our sample ID
sample_ID<- c(1:49)

# creating site name
site<-"Broad_River"

data1<-data.frame(site,sample_ID,size_mm) # merging vectors into a dataframe
print(data1)

##########################################################

# Now, we're going to use a series of "ifelse" statements to set up our criteria to 
# filter our rock measurements in to discrete categories


# "ifelse" statements set a condition and then compares the observations to the 
# condition to determine if it is "TRUE" or "FALSE". These conditions can be specified 
# and linked to create a series of conditions. Here, we are creating a series of 
# conditional statements that tells R how to categorize the raw measurements into 
# discrete categories.

# First, we are going to create a column with each observation categorized according 
# to it's size
data1$substrate_type <- # creates new column in data11
  ifelse(data1$size_mm >=0.1 & data1$size_mm <= 2, 'sand', # conditional statement saying if an observation falls 
         # within the 0.1 tp 2 mm size range, call it "sand". This follows through the rest of the conditions
  ifelse(data1$size_mm >= 2 & data1$size_mm <= 64, 'pebble_gravel',
  ifelse(data1$size_mm >64 & data1$size_mm <=256, 'cobble_gravel',
  ifelse(data1$size_mm >256 & data1$size_mm <=4096, 'boulder',
  ifelse(data1$size_mm >=5000 & data1$size_mm <=10000,'bedrock', 'none'))))) # the "none" here is the final
    # portion of the ifelse statement. It tells R, "if any observations lay outside of this 
#     criteria, label it as "none". NOTE: the code will not run without an ending "none" statement
print(data1)

# now we see we have our size classification categories

###################################################################################

# Now, let's use these data to calculate a percent substrate composition for our
# fake stream

# First, we'll create a series of dummy columns. 
# This is needed to apply the functions below as we want to work with numbers 
# when calculating percent substrate compositions for our sampling
# area. These dummies columns will put a "1" by each row that contains the respective
# substrate classification of the new dummy column. For example: for the dummy column
# "gravel_value", all rows that contain the character string "gravel" will get a "1",
# all other rows will get a "0" and so on for each column. The names of these 
# dummy columns should correspond to the substrate classifications that were determined
# the above code. 
data1<- data1 %>% mutate(sand_value = ifelse(data1$substrate_type == "sand", 1,0)) %>%
  mutate(gravel_value = ifelse(data1$substrate_type == "gravel", 1,0)) %>%
  mutate(cobble_value = ifelse(data1$substrate_type == "cobble", 1,0)) %>%
  mutate(boulder_value = ifelse(data1$substrate_type == "boulder", 1,0)) %>%
  mutate(bedrock_value = ifelse(data1$substrate_type == "bedrock", 1,0))
str(data1)


# Now, we are summarizing what percentage of each dummy column has a "1"
# value for the rows (sum of all the "1's" divided by the number of 
# rows * 100)

percent_substrate_composition<- data1 %>% # if performing these functions across 
  # multiple sites, use the "group_by(Site)" to calculate percent substrate coverage 
  # for each respective site  
  summarize(percent_sand = (sum(sand_value)/n()*100), # n() is telling R to use all rows in the dataframe
            percent_gravel = (sum(gravel_value)/n()*100),
            percent_cobble = (sum(cobble_value)/n()*100),
            percent_boulder = (sum(boulder_value)/n()*100),
            percent_bedrock = (sum(bedrock_value)/n()*100)) %>% 
  mutate_if(is.numeric, round, 0)
print(percent_substrate_composition)

# you can write these dataframes to an Excel sheet (data1 or percent_substrate_composition) 
# using the write.csv command
