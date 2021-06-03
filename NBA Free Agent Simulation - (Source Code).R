library(dplyr)
library(tidyr)
library(ggplot2)
library(pwr)
library(readr)
library(tidyverse)
library(broom)
library(caret)

NBA_Free_Agent_Data <- read.csv(file = "NBA Free Agent Data.csv")

#Dataset Name Change
freeagents <- NBA_Free_Agent_Data

#Part 1: Research Proposal

#2020 Miami Heat Team Score Index

miamiheat_freeagents <- c("Goran Dragic","Derrick Jones Jr.","Meyers Leonard","Udonis Haslem","Jae Crowder","Solomon Hill")
miamiheat_playerscore <-  c(25.1,15.2,13.1,7.2,21.4,11.2)
miamiheat <- sum(miamiheat_playerscore)

#Randomly Selected Sample Combinations

combination1_freeagents <- c("Robin Lopez”, “Khyri Thomas”, “D.J. Augustin”, “Josh Reaves”, “Quinndary Weatherspoon”, “Marco Belinelli")
combination1_playerscore <- c(8.9,3.2,17.5,3.6,2.7,10.3)
combination1 <- sum(combination1_playerscore)
combination1

combination2_freeagents <- c("Tristan Thompson”, “Lamine Diane”, “Devon Hall”, “Kevin Hervey”, “Trey Burke”, “Carmelo Anthony")
combination2_playerscore <- c(24.1,0,4,3.9,12.4,24.3)
combination2 <- sum(combination2_playerscore)
combination2

combination3_freeagents <- c("Robin Lopez”, “Khyri Thomas”, “D.J. Augustin”, “Josh Reaves”, “Quinndary Weatherspoon”, “Marco Belinelli")
combination3_playerscore <- c(8.9,3.2,17.5,3.6,2.7,10.3)
combination3 <- sum(combination3_playerscore)
combination3

combination4_freeagents <- c("Tristan Thompson”, “Lamine Diane”, “Devon Hall”, “Kevin Hervey”, “Trey Burke”, “Carmelo Anthony")
combination4_playerscore <- c(24.1,0,4,3.9,12.4,24.3)
combination4 <- sum(combination4_playerscore)
combination4

combination5_freeagents <- c("Kostas Antetokounmpo”, “Jontay Porter”, “Avery Bradley”, “Kyle Alexander”, “Skal Labissiere”, “Andre Roberson")
combination5_playerscore <- c(2.2,0,13.5,2,12.5,8)
combination5 <- sum(combination5_playerscore)
combination5

combination6_freeagents <- c("DeMarre Carroll”, “Sindarius Thornwell”, “Ryan Broekhoff”, “P.J. Dozier”, “Nicolas Batum”, “Jae Crowder")
combination6_playerscore <- c(7,13,8.6,10.3,11.9,21.4)
combination6 <- sum(combination6_playerscore)
combination6

combination7_freeagents <- c("KyleO' Quinn”, “Furkan Korkmaz”, “Meyers Leonard”, “Kyle Alexander”, “Dewayne Dedmon”, “Marvin Williams")
combination7_playerscore <- c(9.8,15.2,13.1,2,12.7,11.6)
combination7 <- sum(combination7_playerscore)
combination7

combination8_freeagents <- c("Denzel Valentine”, “Jose Barea”, “Michael Kidd-Gilchrist”, “Ray Spalding”, “Anthony Tolliver", "Nik Stauskas")
combination8_playerscore <- c(11.6,13.5,5.6,0,8,0)
combination8 <- sum(combination8_playerscore)
combination8
  
combination9_freeagents <- c("Rajon Rondo”, “Kris Dunn”, “Devon Hall”, “Justin Holiday”, “Jerian Grant”, “Sterling Brown")
combination9_playerscore <- c(14.9,15.9,4,15.9,7.2,10.3)
combination9 <- sum(combination9_playerscore)
combination9
  
combination10_freeagents <- c("De'Anthony Melton”, “Kentavious Caldwell-Pope”, “Moses Brown”, “Dwight Howard”, “Thon Maker”, “Kelan Martin")
combination10_playerscore <- c(15.1,14.4,2.8,15.8,8.9,10.9)
combination10 <- sum(combination10_playerscore)
combination10

combination11_freeagents <- c("Moses Brown”, “Anthony Davis”, “Ray Spalding”, “Justin Patton”, “Thon Maker”, “Carmelo Anthony")
combination11_playerscore <- c(2.8,41.1,0,3,8.9,24.3)
combination11 <- sum(combination11_playerscore)
combination11

combination12_freeagents <- c("Gary Clark”, “Matthew Dellavedova”, “Michael Beasley”, “Taj Gibson”, “Anthony Tolliver”, “Jordan McRae")
combination12_playerscore <- c(8,7.4,0,11.4,8,18.4)
combination12 <- sum(combination12_playerscore)
combination12
  
combination13_freeagents <- c("Zhaire Smith”, “Moses Brown”, “Chimezie Metu”, “Rondae Hollis-Jefferson”, “Paul Watson”, “Solomon Hill")
combination13_playerscore <- c(1.8,2.8,5.5,13.8,6.2,11.2)
combination13 <- sum(combination13_playerscore)
combination13
  
combination14_freeagents <- c("Johnathan Motley”, “Jerian Grant”, “Yogi Ferrell”, “Robin Lopez”, “Ray Spalding”, “Anthony Tolliver")
combination14_playerscore <- c(3.5,7.2,7.2,8.9,0,8)
combination14 <- sum(combination14_playerscore)
combination14
  
combination15_freeagents <- c("Glenn Robinson III”, “Frank MasonIII”, “Kyle Korver”, “Bobby Portis”, “Frank Kaminsky”, “Malik Beasley")
combination15_playerscore <- c(19.1,12.4,11.5,17.4,17,17)
combination15 <- sum(combination15_playerscore)
combination15

#Selected Combination 15 as this is the most optimal combination with the highest player score index under the $51mm salary cap restriction treatment
  
#We reject the Null Hypothesis as combination 15 is greater than 2020 Miami Heat Team Score Index

combination15 > miamiheat

#Part 2: Simulation of Effects

#Scenario 1 & 2 Simulations:

#Variable B, specifies the amount of times we want to run the simulation. Monte Carlo Simulation will be 
#used to run experiment 1,000 times.

B <- 1000

# Using the `set.seed` function to make sure our answer matches the expected result after random sampling.
set.seed(1)

#Simulation
simulation <- replicate(B, {
  freeagents %>%
    slice(sample(c(1:222),size = 6, replace = FALSE))
})

#Unlist done in order to take out character/numeric vectors out of simulation list for use to determine best Player Score Index from simulation. 
#Note: If your data is in a list, you can't do any arithmetic code to the vector regardless if it's numeric. This is a technical R problem we faced due to our experiment design in analyzing 1000 simulations specifically by 6 player free agents combinations so the following code below is a way to get around that.

group1_players <- as.character(unlist(simulation[1]))
group1_players
group1_playerscore <- as.numeric(unlist(simulation[10]))
sum(group1_playerscore)
group1 <- sum(group1_playerscore)
group1_salarycap <- as.numeric(unlist(simulation[2]))
group1_salary <- sum(group1_salarycap)

group2_players <- as.character(unlist(simulation[11]))
group2_players
group2_playerscore <- as.numeric(unlist(simulation[20]))
sum(group2_playerscore)
group2 <- sum(group2_playerscore)
group2_salarycap <- as.numeric(unlist(simulation[12]))
group2_salary <- sum(group2_salarycap)

group3_players <- as.character(unlist(simulation[21]))
group3_players
group3_playerscore <- as.numeric(unlist(simulation[30]))
sum(group3_playerscore)
group3 <- sum(group3_playerscore)

group4_players <- as.character(unlist(simulation[31]))
group4_players
group4_playerscore <- as.numeric(unlist(simulation[40]))
sum(group4_playerscore)
group4 <- sum(group4_playerscore)

group5_players <- as.character(unlist(simulation[41]))
group5_players
group5_playerscore <- as.numeric(unlist(simulation[50]))
sum(group5_playerscore)
group5 <- sum(group5_playerscore)

group6_players <- as.character(unlist(simulation[51]))
group6_players
group6_playerscore <- as.numeric(unlist(simulation[60]))
sum(group6_playerscore)
group6 <- sum(group6_playerscore)

group7_players <- as.character(unlist(simulation[61]))
group7_players
group7_playerscore <- as.numeric(unlist(simulation[70]))
sum(group7_playerscore)
group7 <- sum(group7_playerscore)
group7_salarycap <- as.numeric(unlist(simulation[62]))
group7_salary <- sum(group7_salarycap)

group8_players <- as.character(unlist(simulation[71]))
group8_players
group8_playerscore <- as.numeric(unlist(simulation[80]))
group8 <- sum(group8_playerscore)

group9_players <- as.character(unlist(simulation[81]))
group9_players
group9_playerscore <- as.numeric(unlist(simulation[90]))
group9 <- sum(group9_playerscore)

group10_players <- as.character(unlist(simulation[91]))
group10_players
group10_playerscore <- as.numeric(unlist(simulation[100]))
group10 <- sum(group10_playerscore)

group11_players <- as.character(unlist(simulation[101]))
group11_players
group11_playerscore <- as.numeric(unlist(simulation[110]))
group11 <- sum(group11_playerscore)

group12_players <- as.character(unlist(simulation[111]))
group12_players
group12_playerscore <- as.numeric(unlist(simulation[120]))
group12 <- sum(group12_playerscore)

group13_players <- as.character(unlist(simulation[121]))
group13_players
group13_playerscore <- as.numeric(unlist(simulation[130]))
group13 <- sum(group13_playerscore)
group13_salarycap <- as.numeric(unlist(simulation[122]))
group13_salary <- sum(group13_salarycap)

group14_players <- as.character(unlist(simulation[131]))
group14_players
group14_playerscore <- as.numeric(unlist(simulation[140]))
group14 <- sum(group14_playerscore)

group15_players <- as.character(unlist(simulation[141]))
group15_players
group15_playerscore <- as.numeric(unlist(simulation[150]))
group15 <- sum(group15_playerscore)
group15_salarycap <- as.numeric(unlist(simulation[142]))
group15_salary <- sum(group15_salarycap)

group16_players <- as.character(unlist(simulation[151]))
group16_players
group16_playerscore <- as.numeric(unlist(simulation[160]))
group16 <- sum(group16_playerscore)

group17_players <- as.character(unlist(simulation[161]))
group17_players
group17_playerscore <- as.numeric(unlist(simulation[170]))
group17 <- sum(group17_playerscore)

group18_players <- as.character(unlist(simulation[171]))
group18_players
group18_playerscore <- as.numeric(unlist(simulation[180]))
group18 <- sum(group18_playerscore)

group19_players <- as.character(unlist(simulation[181]))
group19_players
group19_playerscore <- as.numeric(unlist(simulation[190]))
group19 <- sum(group19_playerscore)

group20_players <- as.character(unlist(simulation[191]))
group20_players
group20_playerscore <- as.numeric(unlist(simulation[200]))
group20 <- sum(group20_playerscore)

group21_players <- as.character(unlist(simulation[201]))
group21_players
group21_playerscore <- as.numeric(unlist(simulation[210]))
sum(group21_playerscore)
group21 <- sum(group21_playerscore)

group22_players <- as.character(unlist(simulation[211]))
group22_players
group22_playerscore <- as.numeric(unlist(simulation[220]))
sum(group22_playerscore)
group22 <- sum(group22_playerscore)
group22_salarycap <- as.numeric(unlist(simulation[212]))
group22_salary <- sum(group22_salarycap)

group23_players <- as.character(unlist(simulation[221]))
group23_players
group23_playerscore <- as.numeric(unlist(simulation[230]))
sum(group23_playerscore)
group23 <- sum(group23_playerscore)
group23_salarycap <- as.numeric(unlist(simulation[222]))
group23_salary <- sum(group23_salarycap)

group24_players <- as.character(unlist(simulation[231]))
group24_players
group24_playerscore <- as.numeric(unlist(simulation[240]))
sum(group24_playerscore)
group24 <- sum(group24_playerscore)

group25_players <- as.character(unlist(simulation[241]))
group25_players
group25_playerscore <- as.numeric(unlist(simulation[250]))
sum(group25_playerscore)
group25 <- sum(group25_playerscore)

group26_players <- as.character(unlist(simulation[251]))
group26_players
group26_playerscore <- as.numeric(unlist(simulation[260]))
sum(group26_playerscore)
group26 <- sum(group26_playerscore)

group27_players <- as.character(unlist(simulation[261]))
group27_players
group27_playerscore <- as.numeric(unlist(simulation[270]))
sum(group27_playerscore)
group27 <- sum(group27_playerscore)

group28_players <- as.character(unlist(simulation[271]))
group28_players
group28_playerscore <- as.numeric(unlist(simulation[280]))
group28 <- sum(group28_playerscore)

group29_players <- as.character(unlist(simulation[281]))
group29_players
group29_playerscore <- as.numeric(unlist(simulation[290]))
group29 <- sum(group29_playerscore)

group30_players <- as.character(unlist(simulation[291]))
group30_players
group30_playerscore <- as.numeric(unlist(simulation[300]))
group30 <- sum(group30_playerscore)

group31_players <- as.character(unlist(simulation[301]))
group31_players
group31_playerscore <- as.numeric(unlist(simulation[310]))
sum(group31_playerscore)
group31 <- sum(group31_playerscore)

group32_players <- as.character(unlist(simulation[311]))
group32_players
group32_playerscore <- as.numeric(unlist(simulation[320]))
sum(group32_playerscore)
group32 <- sum(group32_playerscore)

group33_players <- as.character(unlist(simulation[321]))
group33_players
group33_playerscore <- as.numeric(unlist(simulation[330]))
sum(group33_playerscore)
group33 <- sum(group33_playerscore)

group34_players <- as.character(unlist(simulation[331]))
group34_players
group34_playerscore <- as.numeric(unlist(simulation[340]))
sum(group34_playerscore)
group34 <- sum(group34_playerscore)

group35_players <- as.character(unlist(simulation[341]))
group35_players
group35_playerscore <- as.numeric(unlist(simulation[350]))
sum(group35_playerscore)
group35 <- sum(group35_playerscore)

group36_players <- as.character(unlist(simulation[351]))
group36_players
group36_playerscore <- as.numeric(unlist(simulation[360]))
sum(group36_playerscore)
group36 <- sum(group36_playerscore)
group36_salarycap <- as.numeric(unlist(simulation[352]))
group36_salary <- sum(group36_salarycap)

group37_players <- as.character(unlist(simulation[361]))
group37_players
group37_playerscore <- as.numeric(unlist(simulation[370]))
sum(group37_playerscore)
group37 <- sum(group37_playerscore)

group38_players <- as.character(unlist(simulation[371]))
group38_players
group38_playerscore <- as.numeric(unlist(simulation[380]))
group38 <- sum(group38_playerscore)

group39_players <- as.character(unlist(simulation[381]))
group39_players
group39_playerscore <- as.numeric(unlist(simulation[390]))
group39 <- sum(group39_playerscore)
group39_salarycap <- as.numeric(unlist(simulation[382]))
group39_salary <- sum(group39_salarycap)

group40_players <- as.character(unlist(simulation[391]))
group40_players
group40_playerscore <- as.numeric(unlist(simulation[400]))
group40 <- sum(group40_playerscore)

group41_players <- as.character(unlist(simulation[401]))
group41_players
group41_playerscore <- as.numeric(unlist(simulation[410]))
sum(group41_playerscore)
group41 <- sum(group41_playerscore)

group42_players <- as.character(unlist(simulation[411]))
group42_players
group42_playerscore <- as.numeric(unlist(simulation[420]))
sum(group42_playerscore)
group42 <- sum(group42_playerscore)

group43_players <- as.character(unlist(simulation[421]))
group43_players
group43_playerscore <- as.numeric(unlist(simulation[430]))
sum(group43_playerscore)
group43 <- sum(group43_playerscore)

group44_players <- as.character(unlist(simulation[431]))
group44_players
group44_playerscore <- as.numeric(unlist(simulation[440]))
sum(group44_playerscore)
group44 <- sum(group44_playerscore)

group45_players <- as.character(unlist(simulation[441]))
group45_players
group45_playerscore <- as.numeric(unlist(simulation[450]))
sum(group45_playerscore)
group45 <- sum(group45_playerscore)
group45_salarycap <- as.numeric(unlist(simulation[442]))
group45__salary <- sum(group45_salarycap)

group46_players <- as.character(unlist(simulation[451]))
group46_players
group46_playerscore <- as.numeric(unlist(simulation[460]))
sum(group46_playerscore)
group46 <- sum(group46_playerscore)

group47_players <- as.character(unlist(simulation[461]))
group47_players
group47_playerscore <- as.numeric(unlist(simulation[470]))
sum(group47_playerscore)
group47 <- sum(group47_playerscore)

group48_players <- as.character(unlist(simulation[471]))
group48_players
group48_playerscore <- as.numeric(unlist(simulation[480]))
group48 <- sum(group48_playerscore)
group48_salarycap <- as.numeric(unlist(simulation[472]))
group48_salary <- sum(group48_salarycap)

group49_players <- as.character(unlist(simulation[481]))
group49_players
group49_playerscore <- as.numeric(unlist(simulation[490]))
group49 <- sum(group49_playerscore)

group50_players <- as.character(unlist(simulation[491]))
group50_players
group50_playerscore <- as.numeric(unlist(simulation[500]))
group50 <- sum(group50_playerscore)
group50_salarycap <- as.numeric(unlist(simulation[492]))
group50_salary <- sum(group50_salarycap)

group51_players <- as.character(unlist(simulation[501]))
group51_players
group51_playerscore <- as.numeric(unlist(simulation[510]))
sum(group51_playerscore)
group51 <- sum(group51_playerscore)

group52_players <- as.character(unlist(simulation[511]))
group52_players
group52_playerscore <- as.numeric(unlist(simulation[520]))
sum(group52_playerscore)
group52 <- sum(group52_playerscore)

group53_players <- as.character(unlist(simulation[521]))
group53_players
group53_playerscore <- as.numeric(unlist(simulation[530]))
sum(group53_playerscore)
group53 <- sum(group53_playerscore)

group54_players <- as.character(unlist(simulation[531]))
group54_players
group54_playerscore <- as.numeric(unlist(simulation[540]))
sum(group54_playerscore)
group54 <- sum(group54_playerscore)

group55_players <- as.character(unlist(simulation[541]))
group55_players
group55_playerscore <- as.numeric(unlist(simulation[550]))
sum(group55_playerscore)
group55 <- sum(group55_playerscore)

group56_players <- as.character(unlist(simulation[551]))
group56_players
group56_playerscore <- as.numeric(unlist(simulation[560]))
sum(group56_playerscore)
group56 <- sum(group56_playerscore)

group57_players <- as.character(unlist(simulation[561]))
group57_players
group57_playerscore <- as.numeric(unlist(simulation[570]))
sum(group57_playerscore)
group57 <- sum(group57_playerscore)
group57_salarycap <- as.numeric(unlist(simulation[562]))
group57_salary <- sum(group57_salarycap)

group58_players <- as.character(unlist(simulation[571]))
group58_players
group58_playerscore <- as.numeric(unlist(simulation[580]))
group58 <- sum(group58_playerscore)

group59_players <- as.character(unlist(simulation[581]))
group59_players
group59_playerscore <- as.numeric(unlist(simulation[590]))
group59 <- sum(group59_playerscore)

group60_players <- as.character(unlist(simulation[591]))
group60_players
group60_playerscore <- as.numeric(unlist(simulation[600]))
group60 <- sum(group60_playerscore)

group61_players <- as.character(unlist(simulation[601]))
group61_players
group61_playerscore <- as.numeric(unlist(simulation[610]))
sum(group61_playerscore)
group61 <- sum(group61_playerscore)

group62_players <- as.character(unlist(simulation[611]))
group62_players
group62_playerscore <- as.numeric(unlist(simulation[620]))
sum(group62_playerscore)
group62 <- sum(group62_playerscore)

group63_players <- as.character(unlist(simulation[621]))
group63_players
group63_playerscore <- as.numeric(unlist(simulation[630]))
sum(group63_playerscore)
group63 <- sum(group63_playerscore)

group64_players <- as.character(unlist(simulation[631]))
group64_players
group64_playerscore <- as.numeric(unlist(simulation[640]))
sum(group64_playerscore)
group64 <- sum(group64_playerscore)

group65_players <- as.character(unlist(simulation[641]))
group65_players
group65_playerscore <- as.numeric(unlist(simulation[650]))
sum(group65_playerscore)
group65 <- sum(group65_playerscore)

group66_players <- as.character(unlist(simulation[651]))
group66_players
group66_playerscore <- as.numeric(unlist(simulation[660]))
sum(group66_playerscore)
group66 <- sum(group66_playerscore)

group67_players <- as.character(unlist(simulation[661]))
group67_players
group67_playerscore <- as.numeric(unlist(simulation[670]))
sum(group67_playerscore)
group67 <- sum(group67_playerscore)

group68_players <- as.character(unlist(simulation[671]))
group68_players
group68_playerscore <- as.numeric(unlist(simulation[680]))
group68 <- sum(group68_playerscore)

group69_players <- as.character(unlist(simulation[681]))
group69_players
group69_playerscore <- as.numeric(unlist(simulation[690]))
group69 <- sum(group69_playerscore)
group69_salarycap <- as.numeric(unlist(simulation[682]))
group69_salary <- sum(group69_salarycap)

group70_players <- as.character(unlist(simulation[691]))
group70_players
group70_playerscore <- as.numeric(unlist(simulation[700]))
group70 <- sum(group70_playerscore)


group71_players <- as.character(unlist(simulation[701]))
group71_players
group71_playerscore <- as.numeric(unlist(simulation[710]))
sum(group71_playerscore)
group71 <- sum(group71_playerscore)

group72_players <- as.character(unlist(simulation[711]))
group72_players
group72_playerscore <- as.numeric(unlist(simulation[720]))
sum(group72_playerscore)
group72 <- sum(group72_playerscore)

group73_players <- as.character(unlist(simulation[721]))
group73_players
group73_playerscore <- as.numeric(unlist(simulation[730]))
sum(group73_playerscore)
group73 <- sum(group73_playerscore)

group74_players <- as.character(unlist(simulation[731]))
group74_players
group74_playerscore <- as.numeric(unlist(simulation[740]))
sum(group74_playerscore)
group74 <- sum(group74_playerscore)

group75_players <- as.character(unlist(simulation[741]))
group75_players
group75_playerscore <- as.numeric(unlist(simulation[750]))
sum(group75_playerscore)
group75 <- sum(group75_playerscore)
group75_salarycap <- as.numeric(unlist(simulation[742]))
group75_salary <- sum(group75_salarycap)

group76_players <- as.character(unlist(simulation[751]))
group76_players
group76_playerscore <- as.numeric(unlist(simulation[760]))
sum(group76_playerscore)
group76 <- sum(group76_playerscore)

group77_players <- as.character(unlist(simulation[761]))
group77_players
group77_playerscore <- as.numeric(unlist(simulation[770]))
sum(group77_playerscore)
group77 <- sum(group77_playerscore)

group78_players <- as.character(unlist(simulation[771]))
group78_players
group78_playerscore <- as.numeric(unlist(simulation[780]))
group78 <- sum(group78_playerscore)
group78_salarycap <- as.numeric(unlist(simulation[772]))
group78_salary <- sum(group78_salarycap)

group79_players <- as.character(unlist(simulation[781]))
group79_players
group79_playerscore <- as.numeric(unlist(simulation[790]))
group79 <- sum(group79_playerscore)

group80_players <- as.character(unlist(simulation[791]))
group80_players
group80_playerscore <- as.numeric(unlist(simulation[800]))
group80 <- sum(group80_playerscore)
group80_salarycap <- as.numeric(unlist(simulation[792]))
group80_salary <- sum(group80_salarycap)

group81_players <- as.character(unlist(simulation[801]))
group81_players
group81_playerscore <- as.numeric(unlist(simulation[810]))
sum(group81_playerscore)
group81 <- sum(group81_playerscore)

group82_players <- as.character(unlist(simulation[811]))
group82_players
group82_playerscore <- as.numeric(unlist(simulation[820]))
sum(group82_playerscore)
group82 <- sum(group82_playerscore)

group83_players <- as.character(unlist(simulation[821]))
group83_players
group83_playerscore <- as.numeric(unlist(simulation[830]))
sum(group83_playerscore)
group83 <- sum(group83_playerscore)

group84_players <- as.character(unlist(simulation[831]))
group84_players
group84_playerscore <- as.numeric(unlist(simulation[840]))
sum(group84_playerscore)
group84 <- sum(group84_playerscore)

group85_players <- as.character(unlist(simulation[841]))
group85_players
group85_playerscore <- as.numeric(unlist(simulation[850]))
sum(group85_playerscore)
group85 <- sum(group85_playerscore)

group86_players <- as.character(unlist(simulation[851]))
group86_players
group86_playerscore <- as.numeric(unlist(simulation[860]))
sum(group86_playerscore)
group86 <- sum(group86_playerscore)

group87_players <- as.character(unlist(simulation[861]))
group87_players
group87_playerscore <- as.numeric(unlist(simulation[870]))
sum(group87_playerscore)
group87 <- sum(group87_playerscore)

group88_players <- as.character(unlist(simulation[871]))
group88_players
group88_playerscore <- as.numeric(unlist(simulation[880]))
group88 <- sum(group88_playerscore)
group88_salarycap <- as.numeric(unlist(simulation[872]))
group88_salary <- sum(group88_salarycap)

group89_players <- as.character(unlist(simulation[881]))
group89_players
group89_playerscore <- as.numeric(unlist(simulation[890]))
group89 <- sum(group89_playerscore)
group89_salarycap <- as.numeric(unlist(simulation[882]))
group89_salary <- sum(group89_salarycap)

group90_players <- as.character(unlist(simulation[891]))
group90_players
group90_playerscore <- as.numeric(unlist(simulation[900]))
group90 <- sum(group90_playerscore)
group90_salarycap <- as.numeric(unlist(simulation[892]))
group90_salary <- sum(group90_salarycap)

group91_players <- as.character(unlist(simulation[901]))
group91_players
group91_playerscore <- as.numeric(unlist(simulation[910]))
sum(group91_playerscore)
group91 <- sum(group91_playerscore)

group92_players <- as.character(unlist(simulation[911]))
group92_players
group92_playerscore <- as.numeric(unlist(simulation[920]))
sum(group92_playerscore)
group92 <- sum(group92_playerscore)
group92_salarycap <- as.numeric(unlist(simulation[912]))
group92_salary <- sum(group92_salarycap)

group93_players <- as.character(unlist(simulation[921]))
group93_players
group93_playerscore <- as.numeric(unlist(simulation[930]))
sum(group93_playerscore)
group93 <- sum(group93_playerscore)

group94_players <- as.character(unlist(simulation[931]))
group94_players
group94_playerscore <- as.numeric(unlist(simulation[940]))
sum(group94_playerscore)
group94 <- sum(group94_playerscore)

group95_players <- as.character(unlist(simulation[941]))
group95_players
group95_playerscore <- as.numeric(unlist(simulation[950]))
sum(group95_playerscore)
group95 <- sum(group95_playerscore)

group96_players <- as.character(unlist(simulation[951]))
group96_players
group96_playerscore <- as.numeric(unlist(simulation[960]))
sum(group96_playerscore)
group96 <- sum(group96_playerscore)

group97_players <- as.character(unlist(simulation[961]))
group97_players
group97_playerscore <- as.numeric(unlist(simulation[970]))
sum(group97_playerscore)
group97 <- sum(group97_playerscore)

group98_players <- as.character(unlist(simulation[971]))
group98_players
group98_playerscore <- as.numeric(unlist(simulation[980]))
group98 <- sum(group98_playerscore)
group98_salarycap <- as.numeric(unlist(simulation[972]))
group98_salary <- sum(group98_salarycap)

group99_players <- as.character(unlist(simulation[981]))
group99_players
group99_playerscore <- as.numeric(unlist(simulation[990]))
group99 <- sum(group99_playerscore)

group100_players <- as.character(unlist(simulation[991]))
group100_players
group100_playerscore <- as.numeric(unlist(simulation[1000]))
group100 <- sum(group100_playerscore)
group100_salarycap <- as.numeric(unlist(simulation[992]))
group100_salary <- sum(group100_salarycap)

#No Effect Group: Randomly selected 15 samples from Monte Carlo Simulation
noeffect_group <- c(group100,group22,group75,group88,group98,group13,group7,group23,group69,group1,group80,group92,group89,group78,group15)
summary(noeffect_group)
mean(noeffect_group)
sd(noeffect_group)

#Effect Group: Randomly selected 15 samples from Monte Carlo Simulation with $51mm salary cap restriction treatment
effect_group <- c(group100,group88,group90,group57,group36,group48,group13,group7,group23,group69,group80,group92,group89,group78,group15)
summary(effect_group)
mean(effect_group)
sd(effect_group)

#Two Sample T-Test
group_noeffect <- rnorm(15,81.57,17.10)
group_effect <- rnorm(15,71.55,18.15)
t_results <- t.test(group_noeffect,group_effect,var.equal = TRUE)
print(t_results)

#Two Sample T-Test with repeating of simulation 1,000 times
for(i in 1:1000){
  group_noeffect <- rnorm(15,81.57,17.10)
  group_effect <- rnorm(15,71.55,18.15)
  t_results <- t.test(group_noeffect,group_effect,var.equal = TRUE)
}
print(t_results)

#Simulation of Type I and Type II Error

#Scenario 1
n=1000 # testing 1,000 times
t1err=0
for (i in 1:n){
  x=rnorm(15, 81.57, 17.10)
  if (((t.test(x, mu=81.57))$p.value)<=0.05) (t1err=t1err+1) 
}
cat("Type I error rate in percentage is", (t1err/n)*100,"%")

n=1000 # testing 1,000 times
t2err=0
for (i in 1:n){
  x=rnorm(15, 81.57, 17.10)
  if (((t.test(x, mu=81.57))$p.value)>0.05) (t2err=t2err+1) 
}
cat("Type II error rate in percentage is", (t2err/n)*100,"%")

#Scenario 2
n=1000 # testing 1,000 times
t1err=0
for (i in 1:n){
  x=rnorm(15, 71.55, 18.15)
  if (((t.test(x, mu=71.55))$p.value)<=0.05) (t1err=t1err+1) 
}
cat("Type I error rate in percentage is", (t1err/n)*100,"%")

n=1000 # testing 1,000 times
t2err=0
for (i in 1:n){
  x=rnorm(15, 71.55, 18.15)
  if (((t.test(x, mu=71.55))$p.value)>0.05) (t2err=t2err+1) 
}
cat("Type II error rate in percentage is", (t2err/n)*100,"%")

#Scenario 3 & 4 Simulations:

#No Effect Group: 2020 Miami Heat Team Score Index

miamiheat_freeagents <- c("Goran Dragic","Derrick Jones Jr.","Meyers Leonard","Udonis Haslem","Jae Crowder","Solomon Hill")
miamiheat_playerscore <-  c(25.1,15.2,13.1,7.2,21.4,11.2)
miamiheat <- sum(miamiheat_playerscore)
miamiheat
mean(miamiheat_playerscore)
sd(miamiheat_playerscore)

#Effect Group: Selected most optimal combination from new randomly selected 15 samples from Monte Carlo Simulation with $51mm salary cap restriction treatment
scenario4 <- c(group2,group24,group34,group30,group50,group88,group93,group49,group34,group24,group23,group17,group14,group70,group82)

#Selected Group 50 as the most optimal combination since it has the highest player score index from the new randomly selected 15 samples from Monte Carlo Simulation with $51mm salary cap restriction treatment
group50_players
optimalfreeagents <- sum(group50_playerscore)
optimalfreeagents
mean(group50_playerscore)
sd(group50_playerscore)

#Two Sample T-Test
miamiheat_ttest <- rnorm(15, 93.2, 6.64)
optimalfreeagents_ttest <- rnorm(15,94.3,10.80)
t_results2 <- t.test(miamiheat_ttest,optimalfreeagents_ttest,var.equal = TRUE)
print(t_results2)

#Two Sample T-Test with repeating of simulation 1,000 times
for(i in 1:1000){
  miamiheat_ttest <- rnorm(1, 93.2, 6.64)
  optimalfreeagents_ttest <- rnorm(1,94.3,10.80)
  t_results2 <- t.test(miamiheat_ttest,optimalfreeagents_ttest,var.equal = TRUE)
}
print(t_results2)

#Simulation of Type I and Type II Error

#Scenario 3
n=1000 # testing 1,000 times
t1err=0
for (i in 1:n){
  x=rnorm(1, 93.2, 6.64)
  if (((t.test(x, mu=93.2))$p.value)<=0.05) (t1err=t1err+1) 
}
cat("Type I error rate in percentage is", (t1err/n)*100,"%")

n=1000 # testing 1,000 times
t2err=0
for (i in 1:n){
  x=rnorm(1, 93.2, 6.64)
  if (((t.test(x, mu=93.2))$p.value)>0.05) (t2err=t2err+1) 
}
cat("Type II error rate in percentage is", (t2err/n)*100,"%")

#Scenario 4
n=1000 # testing 1,000 times
t1err=0
for (i in 1:n){
  x=rnorm(1, 94.3, 10.80)
  if (((t.test(x, mu=94.3))$p.value)<=0.05) (t1err=t1err+1) 
}
cat("Type I error rate in percentage is", (t1err/n)*100,"%")

n=1000 # testing 1,000 times
t2err=0
for (i in 1:n){
  x=rnorm(1, 94.3, 10.80)
  if (((t.test(x, mu=94.3))$p.value)>0.05) (t2err=t2err+1) 
}
cat("Type II error rate in percentage is", (t2err/n)*100,"%")