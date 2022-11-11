The supplemental .zip archive for the research article “Gender and educational
inequalities in disability-free life expectancy among older adults living in Italian
regions” by Margherita Moretti (Department of Statistical Sciences, Department of
Public Health and Infectious Diseases, Sapienza University of Rome, Rome, Italy)
and Cosmo Strozza (Interdisciplinary Centre on Population Dynamics, University of
Southern Denmark, Odense, Denmark) published in Demographic Research contains the
following files.

R version used: 4.1.3.

1. R program file to replicate the analysis and the plots: code_repr.R

Metadata:

For mortality data, this study draws on mortality rates coming from life tables based
on census-linked mortality data by five-year age classes, gender, educational attainment,
and region of residence for 2011, including mortality records of the following three years (2012–2014).
In Italy, these tables represents the last available data with such level of detail.
They are freely accessible and can be found with full metadata on the Italian National
Institute of Statistics (Istat) website: https://www.istat.it/it/archivio/212512

At the same level of detail, disability prevalence is computed based on data from
the Italian survey “Aspects of daily living” (AVQ). The total sample includes 30738 individuals,
pooled over the three-year period 2012–2014, aged 65 years and older living in household.
Disability prevalence is computed from the Global Activity Limitation Indicator (GALI)
question, differentiating individuals without disability from those with mild or severe
disability. Robust prevalence estimates are obtained by averaging over the three-year period
2012–2014 and applying sample weights.
AVQ data is accessible, upon request, on Istat website: https://www.istat.it/it/archivio/4630
for 2012, and https://www.istat.it/it/archivio/129916
for 2013 and 2014.


From AVQ survey, we selected and manipulated the following variables:
1. sesso
  variable indicating gender, differentiating 1="Men" and 2="Women"
2. eta 
  variable indicating the age class, differentiating "65-69", "70-74", "75-79",
  "80-84", "85+"
3. reg
  variable indicating region of residence, differentiating
  010=Piemonte, 020=Valle d'Aosta, 030=Lombardia, 041=Bolzano, 042=Trento,
  050=Veneto, 060=Friuli-Venezia Giulia, 070=Liguria, 080=Emilia-Romagna,
  090=Toscana, 100=Umbria, 110=Marche, 120=Lazio, 130=Abruzzo, 140=Molise,
  150=Campania, 160=Puglia, 170=Basilicata, 180=Calabria, 190=Sicilia, 200=Sardegna.
  Due to the high level of detail of our analysis, some assumptions to deal with
  missing strata are made: Piemonte and Valle d'Aosta, Molise and Abruzzo,
  Basilicata and Puglia regions and Trento and Bolzano autonomous provinces,
  are assumed to have, in pairs, the same health prevalence.
  Regions are also classified by geographical area according to a grouping of NUTS-1:
  North (North-Est and North-West together): Piemonte, Valle d'Aosta, Lombardia,
        Bolzano, Trento, Veneto, Friuli-Venezia Giulia, Liguria, Emilia-Romagna
  Centre: Toscana, Umbria, Marche, Lazio
  South and Islands: Abruzzo, Molise, Campania, Puglia, Basilicata, Calabria, 
        Sicilia, Sardegna
4. istr
  variable indicating the level of education, differentiating, taking into account
  the age groups in analysis, low-educated as those with primary school diploma
  or less (levels 10 and 11), mid-educated as those with lower secondary school diploma
  (level 09) and high-educated as those with upper secondary school diploma or higher
  (levels 01, 02 and 07)
5. limita
  variable indicating the disability status, defined according to the Global Activity
  Limitation Indicator (GALI) that is based on the question: “For at least the past
  6 months, to what extent have you been limited because of health problem in
  activities people usually do? Would you say you have been: 1 severely limited;
  2 limited but not severely; 3 not limited at all”.
  We differentiate individuals without disability (level 3) from those with mild or
  severe disability (levels 1 and 2)
6. coefin
  variable indicating the sample weights.To use it we divided it by 10000, as 
  suggested by Istat



