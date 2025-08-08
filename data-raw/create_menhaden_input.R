# Creating menhadenEOF.rds to be used in get_comdat.R
# 
# Edited from Sarah G's script: https://github.com/NOAA-EDAB/totalcatchNEUS/blob/main/menhaden.Rmd
#
# Sarah asked that no additional data be committed to github as it may be confidential
#
# Once I can reproduce output, I'll find a better way of including this in the workflow

# Overview

# Atlantic menhaden catch data is confidential (single vertically integrated catcher fleet and processor). I signed a non-disclosure agreement to receive the menhaden catch data relevant to the NEUS shelf for EOF indicator development.
# 
# Here I take the data received by secure email in two different spreadsheets and assemble it into coastwide and MAB and GOM datasets.
# 
# We assume menhaden catches are not taking place on Georges Bank after conversation with the Amy Schueller, the stock assessment lead.
# 
# The match is not perfect because the bait fishery data for MA-ME is confidential so that portion of the catch is not split out.

# Calculations ------------------

# Reduction fishery from Ray Mroch, split to VA-RI (MAB) and MA-ME (GOM):
#   Updated with 2022 and 2023 data by Sydney Alhale - NOAA Federal <sydney.alhale@noaa.gov>

library(readxl)
library(dplyr)

#file1 <- "/AM Landings by Gaichas Regions 1955-2018.xlsx" #for 2023 report
#file1 <- "/AM Landings by Gaichas Regions 1967-2022.xlsx" #for 2024 report
# file1 <- "/AM Landings by Gaichas Regions 1967-2023.xlsx" #for 2025 report
file1 <- "/home/mgrezlik/EDAB_Datasets/AM Landings by Gaichas Regions 1967-2024.xlsx" #for workflow

reduction <- read_excel(path = file1,
                        range = "A7:F64")   # HARDCODED RANGE NEED TO UPDATE EACH YEAR

reductiontons <- reduction[,c(1, 5:6)]
names(reductiontons) <- c("year", "MAB", "GOM")

reductiontons$NEUS <- reductiontons$MAB + reductiontons$GOM
reductiontons$units <- "metric tons"



# Bait fishery from Amy Schueller via ACCSP --------------------

baitold <- read_excel("/home/mgrezlik/Maxwell.Grezlik/SOE/comdat/menhadenforEOF/menhadenforEOF/Atlantic Menhaden Bait Landings ME-VA 1985-2020.xlsx")

lbstotons <- 0.45359237 / 1000

baittonsold <- baitold  |> 
  mutate(MEVAbait.tons = `ME-VA Bait Landings (lbs)`* lbstotons,
         units = "metric tons")  |> 
  select(year = Year, MEVAbait.tons)



# Compare to ACCSP bait ----------------

baitfile <- "/home/mgrezlik/Maxwell.Grezlik/SOE/comdat/menhadenforEOF/menhadenforEOF/1967-2022 Yearly Menhaden Landings separated between Gulf of Maine and Mid-Atlantic from ME to NC.xlsx"

bait <- read_excel(path = (baitfile),
                   sheet = "Data")

lbstotons <- 0.45359237 / 1000

baittonsACCSP <- bait  |> 
  mutate(bait.tons = TOTAL_LIVE_POUNDS * lbstotons,
         units = "metric tons") |> 
  select(year = YEAR, REGION, bait.tons) |>
  tidyr::pivot_wider(names_from = REGION, values_from = bait.tons) |>
  mutate(across(`Gulf of Maine`, ~ replace(., is.na(.), 0))) |>
  rename(GOM.ACCSP = `Gulf of Maine`,
         MAB.ACCSP = `Mid-Atlantic`) |>
  mutate(totbaitACCSP.tons = GOM.ACCSP + MAB.ACCSP)

compare <- baittonsold |>
  full_join(baittonsACCSP) |>
  mutate(difference = totbaitACCSP.tons - MEVAbait.tons)

# write.csv(compare, "comparebait.csv")

# ACCSP (darkgrey, green for Mid) has an entry for 1998 that really looks like a typo; 200000 tons off. 
# Other discrepancies between previous bait (black) and current (darkgrey total) as well. GOM from ACCSP is blue. 

compare |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = MEVAbait.tons)) +
  ggplot2::geom_line() +
  ggplot2::geom_line(ggplot2::aes(x = year, y = GOM.ACCSP), col="blue") +
  ggplot2::geom_line(ggplot2::aes(x = year, y = MAB.ACCSP), col="green") +
  ggplot2::geom_line(ggplot2::aes(x = year, y = totbaitACCSP.tons), col="darkgray") + 
  ggplot2::theme_bw()


# Read in slightly updated assessment bait series from Kristen Anstead ASMFC. 
# It is missing 2022 so let's temporarily say the bait total from ACCSP is right for 2022. It does align well with previous years. 

baitfileKA <- "/home/mgrezlik/Maxwell.Grezlik/SOE/comdat/menhadenforEOF/menhadenforEOF/comparebait_KA.csv"

baitKA <- readr::read_csv(file = (baitfileKA),
                          skip = 2,
                          col_select = c("Year...14", "ME-VA (mt)")) |>
  dplyr::rename(year = "Year...14", 
                MEVAbait.tons = "ME-VA (mt)")

baitKA <-dplyr::bind_rows(baitKA,
                          c(year = 2022,
                            MEVAbait.tons = baittonsACCSP$totbaitACCSP.tons[baittonsACCSP$year==2022])) |>
  na.omit()


# Methods used in 2022, blended in temporarily for 2023, and still used in 2024

## ACCSP bait landings don't include all that are in the assessment

# Attempt to split bait landings for GOM using MA and ME websites.
# 
# MA source: https://www.mass.gov/service-details/historic-commercial-fishing-quotas-and-landings, https://www.mass.gov/media/1486/download
# 
# Menhaden landings do not appear in posted MA data prior to 2013 (goes back to 2007). 
# 
# But we do have GOM landings from ACCSP prior that do not appear in ME landings below. So use them?

# MA menhaden ---------------

# by hand cut and paste from https://www.mass.gov/media/1486/download for 2013-2016
# by hand cut and paste from https://www.mass.gov/service-details/historic-commercial-fishing-quotas-and-landings for 2017-2024

# 2021 3 landings 5,479,487 + 1,962,248 + 2,302,187 = 9743922
# 2022 3 landings 7,117,065 + 2,026,112 + 3,054,424 = 12197601
# 2023 landings reported only for Menhaden (quota) 2,980,815 
# 2024 landings reported only for Menhaden (quota) 12,346,376
# 
MAbait <- data.frame(year = c(2013:2024),
                     lbs = c(2290956, #2013
                             2218570, #2014
                             2928427, #2015
                             3061930, #2016
                             3696974, #2017
                             5714256, #2018
                             6964323, #2019
                             8360307, #2020
                             9743922, #2021
                             12197601, #2022
                             2980815, #2023
                             12346376) #2024
)

MAbait$MAtons = MAbait$lbs * lbstotons




# ME source: New for 2024: https://www.maine.gov/dmr/sites/maine.gov.dmr/files/inline-files/menhaden.table__0.pdf
# 
# Site used previously https://www.maine.gov/dmr/sites/maine.gov.dmr/files/docs/menhaden.table.pdf, https://www.maine.gov/dmr/sites/maine.gov.dmr/files/docs/menhaden.graph.pdf

# ME menhaden ----------------

library(pdftools)
# download.file("https://www.maine.gov/dmr/sites/maine.gov.dmr/files/inline-files/menhaden.table__0.pdf", "menhaden.table.pdf", mode = "wb")
temp_pdf <- tempfile(fileext = ".pdf")
download.file("https://www.maine.gov/dmr/sites/maine.gov.dmr/files/inline-files/menhaden.table__0.pdf", 
              destfile = temp_pdf, mode = "wb")
txt <- pdf_text(temp_pdf)

rows<-scan(textConnection(txt), what="character",
           sep = "\n")  
#datME <- rows[c(2:67, 70:76)] #old file
datME <- rows[c(2:76)]
year <- c()
tons <- c()
for (i in 1:length(datME)){
  row = unlist(strsplit(datME[i]," \\s+ "))
  if (!is.na(row[3])){
    year <- c(year, 
              as.numeric((gsub("\\*", "", row[1]))))
    tons<- c(tons,
             as.numeric((gsub(",", "", row[3]))))
  }
}

MEbait19502023 <- data.frame(year = year[-1], tons=tons[-1])

# 2023 updated landings 2018-2022 https://www.maine.gov/dmr/sites/maine.gov.dmr/files/inline-files/LandingsBySpecies.Table_.pdf

# MG updated this in 2025. Values changed slightly and now only go back to 2020

MEbait20202024 <- data.frame(year = c(2020:2024),
                             lbs = c(
                                     27172212, #2020
                                     24156872, #2021
                                     25947085, #2022
                                     26414423, #2023
                                     25638230) #2024 prelim
)

MEbait20202024$MEtons = MEbait20202024$lbs * lbstotons

#MEtot <- data.frame(year = year,
#                    MEtons = tons)
#MEtot <- MEtot[-1,]

MEbait19502023 <- data.frame(year = year[-1], MEtons=tons[-1])

# in 2023, numbers for 2018-2020 are close between the two datasets. 2021 preliminary in pdf
# 2023 decision: use 2021 and 2022 tons from MEbait20182022 and leave the rest alone

# 2024 decision is to use the MEbait19502023 dataset because it exceeds the prelim numbers from 
# from last year for 2020-2022 and includes 2023.

#MEtot$MEtons[MEtot$year==2021] <- MEbait20182022$MEtons[MEbait20182022$year==2021]

MEtot <- MEbait19502023
#dplyr::bind_rows(MEtot,
#        c(year = 2022,
#          MEtons = MEbait20182022$MEtons[MEbait20182022$year==2022]))




# ME landings go back to 1950. I assume that reduction fishery landings are covered in the reduction catch for MA-ME, which spans 1970-1988 in our data. I will subtract total reduction catch from ME landings and assume the rest is a bait fishery for MA-ME.
# 
# ACCSP GOM bait landings are probably a better source than my rebuilt bait data from ME and MA websites, becasue MA only goes back to 2013. Where there are no ME bait landings, ACCSP matches the info from the MA website (2013-2015) but numbers diverge after 2015 so I may stick with state summaries subtracted from Kristen's total bait MEVA for 2016-2022. Implemented this logic below.
# 
# For 2024, add Mid Atlantic bait landings from ACCSP as best I can using the public website, comparing with what I got for 2021-2022 last year
# 
# Query on public warehouse page: https://safis.accsp.org/accsp_prod/f?p=1490:1:25405069699995:::::
# Click non-confidential
# Start year 2021 end year 2023
# States ME through NC
# Species Menhadens and Menhaden, Atlantic


ACCSPcomp <- read.csv(here::here("/home/mgrezlik/Maxwell.Grezlik/SOE/comdat/menhadenforEOF/menhadenforEOF/ACCSPNonConfidential_CommercialLandings2021-2023AllNEstates.csv"))

ACCSPcomptons <- ACCSPcomp |>
  dplyr::mutate(Tons = Pounds * lbstotons) |>
  dplyr::group_by(Year, State) |>
  dplyr::summarise(sumtons = sum(Tons, na.rm = TRUE)) |>
  tidyr::pivot_wider(names_from = State, values_from = sumtons)

ACCSP_regioncomp <- ACCSPcomp |>
  dplyr::mutate(Tons = Pounds * lbstotons) |>
  #dplyr::filter(!State=="VIRGINIA") |> # approximate this all as reduction catch
  dplyr::group_by(Year, Subregion) |>
  dplyr::summarise(sumtons = sum(Tons, na.rm = TRUE)) |>
  tidyr::pivot_wider(names_from = Subregion, values_from = sumtons)

# build ACCSP 2023 as follows
Reg.ACCSP <- ACCSPcomp |>
  dplyr::mutate(Tons = Pounds * lbstotons) |>
  dplyr::mutate(Reg = dplyr::case_when(State %in% c("MAINE",
                                                    "`NEW HAMPSHIRE`",
                                                    "MASSACHUSETTS",
                                                    "`RHODE ISLAND`",
                                                    "CONNECTICUT") ~ "GOM.ACCSP",
                                       TRUE ~ "MAB.ACCSP")) |>
  dplyr::group_by(Year, Reg) |>
  dplyr::summarise(sumtons = sum(Tons, na.rm = TRUE)) |>
  tidyr::pivot_wider(names_from = Reg, values_from = sumtons) |>
  dplyr::left_join(reductiontons, by=c("Year"="year")) |>
  dplyr::mutate(MAB.ACCSP = MAB.ACCSP-MAB) |> # subtract reduction catch from ACCSP MAB catch to get bait MAB
  dplyr::select(Year, GOM.ACCSP, MAB.ACCSP)

# as with 2022, sub in ACCSP 2023 for MEVAbait.tons
baitKA <-dplyr::bind_rows(baitKA,
                          c(year = 2023,
                            MEVAbait.tons = Reg.ACCSP$GOM.ACCSP[Reg.ACCSP$Year==2023] + Reg.ACCSP$MAB.ACCSP[Reg.ACCSP$Year==2023])) 





# Combine into total NEUS catch, estimate GOM catch (reduction plus bait estimated by adding recent MA-ME), and estimate MAB catch (reduction plus bait estimated by subtracting recent MA-ME): 

# NEUS catch -----------------------

menhadencatch <- reductiontons  |> 
  left_join(baitKA)  |> 
  left_join(MEtot) |> 
  left_join(MAbait)  |> 
  left_join(baittonsACCSP) |>
  mutate(MEbait = case_when(MEtons-GOM >= 0 ~ MEtons-GOM,
                            MEtons-GOM < 0 ~0))  |> 
  rowwise() |> 
  mutate(GOMbait = case_when(year < 1990 | year > 2015 ~ sum(MEbait, MAtons, na.rm = TRUE),
                             # ~ sum(MEbait, MAtons, na.rm = TRUE),
                             TRUE ~ GOM.ACCSP))  |> 
  mutate(MABbait = case_when(MEVAbait.tons - GOMbait >= 0 ~ MEVAbait.tons - GOMbait,
                             MEVAbait.tons - GOMbait < 0 ~0))  |> 
  mutate(NEUScatch = sum(NEUS, GOMbait, MABbait, na.rm=TRUE),
         MABcatch = sum(MAB, MABbait, na.rm = TRUE),
         GOMcatch = sum(GOM, GOMbait, na.rm = TRUE))



# Anything falling off a cliff in the final year?

ggplot2::ggplot(menhadencatch, ggplot2::aes(x = year, y=NEUScatch)) +
  ggplot2::geom_line() +
  ggplot2::geom_line(ggplot2::aes(x = year, y = GOM), col="blue") +
  ggplot2::geom_line(ggplot2::aes(x = year, y = MAB), col="green") +
  ggplot2::geom_line(ggplot2::aes(x = year, y = NEUS), col="purple") +
  ggplot2::ggtitle("Reduction") +
  ggplot2::theme_bw()


ggplot2::ggplot(menhadencatch, ggplot2::aes(x = year, y=NEUScatch)) +
  ggplot2::geom_line() +
  ggplot2::geom_line(ggplot2::aes(x = year, y = GOMbait), col="blue") +
  ggplot2::geom_line(ggplot2::aes(x = year, y = MABbait), col="green") +
  
  ggplot2::geom_line(ggplot2::aes(x = year, y = MEtons), col="blue", lty=3) +
  ggplot2::geom_line(ggplot2::aes(x = year, y = MAtons), col="green", lty=3) +
  ggplot2::geom_line(ggplot2::aes(x = year, y = GOM.ACCSP), col="blue", lty=2) +
  ggplot2::geom_line(ggplot2::aes(x = year, y = MAB.ACCSP), col="green", lty=2) +
  
  ggplot2::geom_line(ggplot2::aes(x = year, y = MEVAbait.tons), col="purple") +
  ggplot2::ggtitle("Bait") +
  ggplot2::theme_bw()




# Make an R data object with all three estimates -------------

menhadenEOF <- menhadencatch  |> 
  dplyr::select(year, NEUScatch, MABcatch, GOMcatch, units)

saveRDS(menhadenEOF,"/home/mgrezlik/EDAB_Dev/grezlik/menhadenEOF.rds")


# This R data object will be transferred securely to Andy Beet for incorporation into the Ecosystem Overfishing indicators.
# 
# Plot total catch black, MABcatch green, GOMcatch blue

ggplot2::ggplot(menhadenEOF, ggplot2::aes(x = year, y=NEUScatch)) +
  ggplot2::geom_line() +
  ggplot2::geom_line(ggplot2::aes(x = year, y = GOMcatch), col="blue") +
  ggplot2::geom_line(ggplot2::aes(x = year, y = MABcatch), col="green") +
  ggplot2::theme_bw()

