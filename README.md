### Analyses and data from **A 26-year time series of mortality and growth of the Pacific oyster C. gigas recorded along French coasts**

DOI of paper:

DOI of analyses and data sets: <https://doi.org/10.5281/zenodo.5744977>

### Outline

This repository contains the data sets, analyses and figures of the above-mentioned paper. It can recreate all analyses and figures in the text (except fig. 1 which was created with QGIS).

### Licensing

Shield: [![CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by/4.0/)

This work is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/).

[![CC BY 4.0](https://i.creativecommons.org/l/by/4.0/88x31.png)](http://creativecommons.org/licenses/by/4.0/)

### Running the scripts and analyses

-   The project can be `cloned` or downloaded by clicking "Clone or download" at the top right of this page.

-   Open the R project file in the downloaded folder. R project automatically assigns the root directory to the directory in which the project is. All analyses can thus be run without changing paths. All the scripts for the analyses can be found in `code` folder.

-   `code/1_cleaning_data.R` contains the code for data cleaning. This script also computes the mean cumulative mortality and mean mass of oysters per date x site x class age combination.

-   `code/2_analysis_sigmoide.Rmd` does the logistic and Gompertz models of the mean cumulative mortality and mean mass for spat and half-grown oysters.

-   All the data needed to run the analyses are stored in `data` folder.

-   All figures produced by the analysis are saved in `figs` folder and are labelled as they are in the manuscript.

### Overview of data files

This repository contains four data sets:

-   `AllDataresco.csv` is a csv file containing the raw observations of oyster growth and mortality recorded within the REMORA, RESCO and ECOSCOPA programs. This data set is a modified extraction (carried out on 2021-07-20) of the RESCO REMORA Database (<https://doi.org/10.17882/53007>) available in SEANOE, an academic publisher or marine research data. The table contains 571101 rows and 34 columns. The first thing we did in this project is to keep only 18 columns and rename them. Description of renamed columns:

    -   `program`: the name of the program. Blank cells indicate that this information was not available.

    -   `mnemonic_site`: the mnemonic is a unique identifier of the site and is constructed as follows: code of the marine area - P (for monitoring point) - order number of the monitoring location in the marine area. For example, 014-P-055.

    -   `site`: the name of the site.

    -   `classe_age`: the age class of the oyster: N0 (spat), J1 (half-grown) or A2 (commercial size). Blank cells indicate that this information was not available.

    -   `ploidy`: the ploidy of the oysters: diploïdes or triploïdes (in English: diploid or triploid). Blank cells indicate that this information was not available.

    -   `date`: the date of data collection (format DD/MM/YYYY).

    -   `mnemonic_passage`: mnemonic of the visit. The name of the quarterly operation (P0, P1, P2, P3 or RF: last data collection). For intermediate operations, we use the previous name of the operation followed by an underscore and the number of the week. For example, data collection on 2019-05-06 corresponds to P1_S19. Biométrie initiale (in English: initial biometrics) is equivalent to P0 (first data collected during the campaign).

    -   `param`: the name of the measured parameter: Nombre d'individus morts, Nombre d'individus vivants, Poids de l'individu or Poids total des individus vivants (in English: number of dead oysters, number of alive oysters, mass of the individual and total mass of alive individuals).

    -   `code_param`: code of the measured parameter. INDVVIVNB = number of alive oysters, INDVMORNB = number of dead oysters,
    INDVPOID = mass of the individual, TOTVIVPOI = total mass of alive individuals (i.e. mass of the bag).

    -   `unit_measure`: the unit of measurement: Gramme or Unité de dénombrement (d'individus, de cellules, ...) (in English: grams or counting of individuals, cells ...).

    -   `fraction`: either the measure was made at the bag level on which case the fraction is "Sans objet" = Not applicable or the measure was made at the individual level (code_param = INDVPOID), in which case the fraction indicates the part of the oyster that was measured: Chair totale égouttée or coquille (in English: total flesh drained or shell).

    -   `method`: the method used to obtain the data. For the number of alive and dead oysters (code_param = INDVVIVNB and INDVMORNB), the method used is comptage macroscopique (in English: macroscopic count). For mass taken at the individual level (code_param = INDVPOID), the method is Pesée après lyophilisation or Pesée simple sans préparation (in English: weighing without preparation or weighing after lyophilization).

    -   `id_ind`: the id of the individual oyster when code_param is INDVPOID or the id of the bag when code_param is INDVVIVNB, INDVPOID and TOTVIVPOI.

    -   `value`: numeric value of the measurement.

    -   `mnemonic_prelevement`: this is a concatenated field. Its coding is not consistent throughout the dataset. Indeed, it is sometimes composed of the first letter of the program name attached to 2 numbers indicating the year of data collection and the age class (gj: spat, ga: half-grown or commercial size oysters) - 2 letters indicating the region attached to a 4-character site identifier- mnemonic passage. For example, R05gj-NOBV02-P0 corresponds to data collected in the program REMORA in 2005 on gigas spat (gj) in Normandy (NO) in the site Géfosse 02 (BV02) in the 1st quarter. Other times the mnemonic_prelevement is composed of the first two letters of the program name attached to 2 numbers indicating the year of data collection \_ the age class (GJ: spat, GA18: half-grown, GA30: commercial size oysters) attached to the origin of the initial spat group (this information is not always indicated)(CN + number: identifier of wild-caught site, ET + character: identifier of the hatchery, NSI: Argenton hatchery via a standardized protocol) \_ a 4-character identifier for the site. For example, RE12_GJET2_BV02 corresponds to data collected in the program REMORA in 2012 (RE12) on gigas spat born in hatchery 2 (GJET2) in the site Géfosse 02 (BV02). Finally, mnemonic_prelevement is sometimes: Biométrie initiale (initial biometrics), Biométrie initiale 6 mois (initial biometrics of spat), Biométrie initiale 18 mois and Biométrie initiale adulte (both correspond to initial biometrics of half-grown oysters), Biométrie initiale 30 mois (initial biometrics of commercial size oysters), Biométrie initiale NSI (initial biometrics of spat batch produced in Argenton Ifremer hatchery via a standardized protocol).

    -   `long`: The longitudinal coordinate of the site given in decimal in the WGS 84 system.

    -   `lat`: The latitudinal coordinate of the site given in decimal in the WGS 84 system.

    -   `pop_init_batch`: this is a concatenated field. It is composed of the two first letters of the name of the program name attached to 2 numbers indicating the year of data collection \_ the age class code (GJ: gigas spat, GA: gigas half-grown, GA30: gigas commercial size) \_ the origin of the initial spat group (CN: wild-caught, ET: hatchery, NSI: Argenton hatchery) attached to two numbers indicating the year of birth of the initial spat group \_ the birth place of the initial spat group (this one is optionnal). For example, RE00_GJ_CN99_AR corresponds to data collected in the program REMORA in 2000 on spat oysters (GJ) born in 1999 and wild-caught (CN99) in the Bay of Arcachon (AR). Blank cells indicate that this information was not available.

        <br />

-   `sites.csv` is a csv file of 7 columns and 13 rows containing information about the 13 sites. Description of the columns found in the data set:

    -   `num`: a unique identifier for each site. Ranges between 1 and 13.

    -   `site`: the abbreviated name of the site.

    -   `name`: the full name of the site.

    -   `zone_fr`: the French name of the zone where data collection took place.

    -   `zone_en`: the English name of the zone where data collection took place.

    -   `lat`: the latitudinal coordinate of the site given in decimal in the WGS 84 system.

    -   `long`: the longitudinal coordinate of the site given in decimal in the WGS 84 system.

    <br />

-   `DataResco_clean.csv` is the curated data set of oyster growth and mortality (csv file). The table contains 5178 rows and 13 columns. Each row corresponds to the mean cumulative mortality and mean mass of oysters for a specific date x site x age class combination. This is the data set we used to fit logistic and Gompertz models to describe mean mass and cumulative mortality at time *t*. Description of the columns found in the data set:

    -   `num`, `site`, `name`, `zone_en`, `lat`, `long`: see the description above for the data set `sites.csv`.

    -   `campaign`: the year of data collection. Ranges between 1993 and 2018.

    -   `classe_age`: the age class of the oyster (i.e. spat: N0 or half-grown: J1).

    -   `batch`: the identifier of the batch (group of oysters born from the same reproductive event, having experienced strictly the same zootechnical route). It is a field that concatenates the campaign, the age class of oysters (spat: N0 or half-grown: J1), the origin of the initial spatgroup (wild-caught: CAPT or Ifremer hatchery: ECLO), ploidy (diploid: 2n) and birthplace of the original spatgroup (AR: Bay of Arcachon or E4: Ifremer hatchery of Argenton).

    -   `date`: the day of data collection (format YYYY-MM-DD).

    -   `date_jj`: the Julian day of data collection (count of days since the beginning of the year). It ranges between 46 and 354.

    -   `mean_CM`: the mean cumulative mortality of oysters. Ranges between 0 and 0.956 (i.e. between 0-95.6 %). We first calculated the cumulative mortality for each bag x date x site x age class combination according to the following formula: *CM~t~* = 1 -- ((1 -- *CM~t-1~*) × (1 -- *IM~t~*)). *CM~t~* = Cumulative mortality at time *t*; *CM~t-1~* = Cumulative mortality at time *t*-1; *IM~t~* = Mortality rate at time *t*. *IM~t~* was obtained by dividing the number of dead oysters by the sum of alive and dead oysters at time *t*. When several bags were followed, we then averaged the cumulative mortality by date x site x age class combination. NA values indicate that this information was not available.

    -   `mean_mass`: the mean mass of oysters in grams. Ranges between 0.28 and 122.51 g . For mass data collected until 2008, we calculated the mean of the individual mass per date x site x age class combination by averaging the mass of the individuals. In other cases (mass data collected since 2009), we calculated the mean mass of individuals for each bag x date x site x class age combination by dividing the total mass of living oysters by the number of living individuals and then averaged data by date x site x age class combination. The mean mass is thus the mean of the individual mass until 2008 and the mean mass of individuals since 2009. NA values indicate that this information was not available.

    <br />

-   `data/clean/DataResco_predicted.csv` is a csv file containing the cumulative mortality and mass of oysters predicted by the best sigmoid model. The table contains 19005 rows and 11 columns. Each row corresponds to the cumulative mortality and mass predicted for a specific Julian day x campaign x site x age class combination.

    -   `num`, `site`, `name`, `zone_en`, `lat`, `long`: see the description above for the data set `sites.csv`.
    -   `campaign`: the year of the data prediction. Ranges between 1993 and 2018.
    -   `classe_age`: the age class of the oyster (i.e. spat: N0 or half-grown: J1).
    -   `date_jj`: the Julian day of the prediction (count of days since the beginning of the year). It ranges from 65 (median day of seeding date) to 337 (median day of the end of the monitoring).
    -   `CM_pred`: cumulative mortality predicted by the best model (i.e. Gompertz model).
    -   `mass_pred`: mass predicted (in grams) by the best model (i.e. logistic model).
