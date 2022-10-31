# ------------------------------------------------------------------
# Code to replicate the analysis of the paper "Gender and educational
# inequalities in disability-free life expectancy among older adults
# living in Italian regions" by Moretti M. & Strozza C.
# ------------------------------------------------------------------

# We use R version 4.1.3 (2022-03-10) -- "One Push-Up"
# Copyright (C) 2022 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)

# As part of the data is only accessible upon request, we provide
# the main functions used in the analysis.
# The data input for this code is the microdata from AVQ surveys
# (2012-2014) and abridged life tables (2012-2014), by region of
# residence, age class, sex, level of education, and disability



# Step 0: load the required libraries ----
library(tidyverse)
library(dplyr)
library(janitor)
library(sf)
library(eurostat)
library(geofacet)
library(DemoDecomp)


# Step 1: compute the prevalence starting from microdata ----
AVQ <- AVQ %>%
  group_by(reg,ageclass,sex,education,disability) %>% 
  summarise(n = sum(n), # unweighted count in each strata
            n_w = sum(n_w)) %>%  # weighted count in each strata 
  group_by(reg,ageclass,sex,education) %>% 
  mutate(n_denom = sum(n), # unweighted denominator
         n_denom_w = sum(n_w), # weighted denominator
         prev = n/n_denom, # unweighted prevalence
         prev_w = n_w/n_denom_w, # weighted prevalence
         var_prev = (prev_w*(1-prev_w)) / n) %>% # variance of prevalence
  ungroup()


# Step 2: join mortality and morbidity data ----
mort_prev <- left_join(AVQ, TABLES,
                       by = c("reg","ageclass","sex","education"))%>% 
  select(reg,ageclass,sex,education,disability,
         n,n_w,n_denom,n_denom_w,prev,prev_w,var_prev,
         mx)


# Step 3: functions to compute LE and DFLE ----

# Life expectancy (LE)
LE_fun <- function (mx) {
  # 5 year age classes from 65 to 85+
  age = seq(65,85,by=5)
  # n and ax
  n = c(diff(age), 1)
  ax = 0.5 * n
  # probability of death
  qx = (n * mx)/(1 + (n - ax) * mx)
  qx = c(qx[-(length(qx))], 1)
  qx[qx > 1] = 1
  # survival probability
  px = 1 - qx
  # survivors 
  lx = c(100000,rep(0,(length(mx)-1)))
  for (i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i] }
  # deaths 
  dx = lx * qx
  # person-years 
  Lx = rep(0,length(mx))
  for (i in 1:length(mx) -1){
    Lx[i] = lx[i+1]*n[i] + ax[i]*dx[i] }
  Lx[length(mx)] = lx[length(mx)]/mx[length(mx)]  
  # life expectancy
  LE = rev(cumsum(rev(Lx)))/lx
  # life table
  LT <- data.frame(ax=ax, mx=mx, qx=qx, dx=dx, lx=lx, Lx=Lx, LE=LE)
  # return life expectancy
  return(LE)
}


# Disability-free life expectancy (DFLE)
DFLE_fun <- function (MxP) {
  # function of the vector made by mx and prev
  # mx in the first positions
  mx = MxP[1:(length(MxP)/2)]
  # prev after mx
  prev = MxP[(length(MxP)/2+1):length(MxP)]
  # 5 year age classes from 65 to 85+
  age = seq(65,85,by=5)
  # n and ax
  n = c(diff(age), 1)
  ax = 0.5 * n
  # probability of death
  qx = (n * mx)/(1 + (n - ax) * mx)
  qx = c(qx[-(length(qx))], 1)
  qx[qx > 1] = 1
  # survival probability
  px = 1 - qx
  # survivors 
  lx = c(100000,rep(0,(length(mx)-1)))
  for (i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i] }
  # deaths 
  dx = lx * qx
  # person-years
  Lx = rep(0,length(mx))
  for (i in 1:length(mx) -1){
    Lx[i] = lx[i+1]*n[i] + ax[i]*dx[i] }
  Lx[length(mx)] = lx[length(mx)]/mx[length(mx)]  
  # life expectancy
  LE = rev(cumsum(rev(Lx)))/lx
  # disability(free) life expectancy
  Lx_DF = Lx*prev
  DFLE = rev(cumsum(rev(Lx_DF)))/lx
  # life table
  LT <- data.frame(ax=ax, mx=mx, qx=qx, dx=dx, lx=lx, Lx=Lx, LE=LE,
                   prev=prev, Lx_DF=Lx_DF, DFLE=DFLE)
  # return disability(free) life expectancy
  return(DFLE)
}

# NB the function inside the stepwise replacement for the decomposition
# requires the first element of DFLE, set return to DFLE[1]
DFLE_fun_65 <- function (MxP) { 
  # function of the vector made by mx and prev 
  # mx in the first positions
  mx = MxP[1:(length(MxP)/2)]
  # prev after mx
  prev = MxP[(length(MxP)/2+1):length(MxP)]
  # 5 year age classes from 65 to 85+
  age = seq(65,85,by=5)
  # n and ax
  n = c(diff(age), 1)
  ax = 0.5 * n
  # probability of death
  qx = (n * mx)/(1 + (n - ax) * mx)
  qx = c(qx[-(length(qx))], 1)
  qx[qx > 1] = 1
  # survival probability
  px = 1 - qx
  # survivors 
  lx = c(100000,rep(0,(length(mx)-1)))
  for (i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i] }
  # deaths 
  dx = lx * qx
  # person-years
  Lx = rep(0,length(mx))
  for (i in 1:length(mx) -1){
    Lx[i] = lx[i+1]*n[i] + ax[i]*dx[i] }
  Lx[length(mx)] = lx[length(mx)]/mx[length(mx)]  
  # life expectancy
  LE = rev(cumsum(rev(Lx)))/lx
  # disability(free) life expectancy
  Lx_DF = Lx*prev
  DFLE = rev(cumsum(rev(Lx_DF)))/lx
  # life table
  LT <- data.frame(ax=ax, mx=mx, qx=qx, dx=dx, lx=lx, Lx=Lx, LE=LE,
                   prev=prev, Lx_DF=Lx_DF, DFLE=DFLE)
  # return disability(free) life expectancy at 65
  DFLE <- LT$DFLE[1]
  return(DFLE)
}


# Standard deviation of DFLE
SE_DFLE_fun <- function (MxP,var_prev) {
  # function of the vector made by mx and prev and the variance of prev
  # mx in the first positions
  mx = MxP[1:(length(MxP)/2)]
  # prev after mx
  prev = MxP[(length(MxP)/2+1):length(MxP)]
  # 5 year age classes from 65 to 85+
  age = seq(65,85,by=5)
  # n and ax
  n = c(diff(age), 1)
  ax = 0.5 * n
  # probability of death
  qx = (n * mx)/(1 + (n - ax) * mx)
  qx = c(qx[-(length(qx))], 1)
  qx[qx > 1] = 1
  # survival probability
  px = 1 - qx
  # survivors 
  lx = c(100000,rep(0,(length(mx)-1)))
  for (i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i] }
  # deaths 
  dx = lx * qx
  # person-years
  Lx = rep(0,length(mx))
  for (i in 1:length(mx) -1){
    Lx[i] = lx[i+1]*n[i] + ax[i]*dx[i] }
  Lx[length(mx)] = lx[length(mx)]/mx[length(mx)]  
  # life expectancy
  LE = rev(cumsum(rev(Lx)))/lx
  # disability(free) life expectancy
  Lx_DF = Lx*prev
  DFLE = rev(cumsum(rev(Lx_DF)))/lx
  # std error of DFLE
  SE_DFLE = sqrt(sum(Lx^2*var_prev^2)/(lx^2)) # or cumsum?
  LT <- data.frame(ax=ax, mx=mx, qx=qx, dx=dx, lx=lx, Lx=Lx, LE=LE,
                   prev=prev, Lx_DF=Lx_DF, DFLE=DFLE, SE_DFLE)
  return(SE_DFLE)
}


# Step 3: application of the functions to the groups ----
DFLE_ITA <- mortprev_ITA %>% 
  group_by(reg,ageclass,sex,education,disability) %>% 
  mutate(LE = LE_fun(mx = mx),
         DFLE = DFLE_fun(MxP = c(mx,prev_w)),
         SE_DFLE = SE_DFLE_fun(MxP = c(mx,prev_w),var_prev = var_prev),
         sx_CI_DFLE = DFLE - 1.96 * SE_DFLE, # lower bound of DFLE's CIs
         dx_CI_DFLE = DFLE + 1.96 * SE_DFLE, # upper bound of DFLE's CIs
         HR = 100*DFLE/LE) # DFLE ratio over LE


# Step 4: plot DFLE ----
# convert data to long format
DFLE_65_long <- 
  DFLE_ITA %>%
  select(reg,ageclass,sex,education,disability,
         DFLE,sx_CI_DFLE,dx_CI_DFLE)%>%
  gather(HE,value,DFLE)%>%
  mutate(HE = ifelse(limit=="Limitazioni non gravi","MDLE",
                     ifelse(limit=="Limitazioni gravi","SDLE",HE))) %>%
  select(reg,ageclas,sesso,edu,limit,HE,value,sx_CI_DFLE,dx_CI_DFLE) %>%
  filter(ageclas=="65-69")%>%
  group_by(reg,ageclass,sex,education) %>%
  mutate(LE = sum(value)) %>%
  ungroup()

DFLE_65_long <- DFLE_65_long %>%
  arrange(desc(value)) %>% # order the regs according to DFLE values
  mutate(lev_order_reg = as.factor(unique(reg)))

DFLE_65_long %>% # ggplot
  ggplot(aes(x = value,
             y = fct_rev(factor(reg,
                                levels = lev_order_reg,
                                labels = lev_order_reg)))) +
  geom_vline(data=DFLE_65_long, # vertical line for italian values
             aes(xintercept = value_ITA,
                 color = edu),
             alpha = 0.6)+
  geom_point(aes(color = education, shape = education),
             size = 2,
             alpha = 0.9)+
  facet_wrap(~ fct_rev(as.factor(sex)))+
  scale_color_viridis_d(option = "B",
                        begin = 0.1,
                        end = 0.9) +
  scale_x_continuous(limits = c(5.5, 13.5)) +
  labs(x = "DFLE",
       y = element_blank(),
       color = "Level of education",
       shape = "Level of education") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        legend.position = "bottom")


# Step 5: compute the gaps in DFLE ----
# here shown for educational gap
# (gender gap can be computed and plotted similarly)

edu_gap_DFLE <- DFLE_ITA %>% # educational gap
  filter(limit == "Nessuna limitazione") %>%
  spread(edu,DFLE) %>% 
  group_by(reg,ageclas,sesso) %>% 
  mutate(DFLE_edu_gap = alto-basso) # High education's DFLE - Low's


# Step 6: map of the gaps ----
map_ita <- eurostat_geodata_60_2016 %>% # dataset of EU boundaries
  janitor::clean_names() %>% # transform the one suitable for Europe
  st_transform(crs = 3035) %>%
  filter(cntr_code == "IT", # select Italy
         levl_code == 2) # at regional level

map_edu_gap <- left_join(edu_gaps, map_ita, # join data  data
                            by = "reg") 

no_classes <- 5 # compute quitiles
quantiles <- quantile(map_edu_gap$DFLE_edu_gap, 
                      probs = seq(0, 1, length.out = no_classes + 1),
                      na.rm = T)
labels <- c()
for(idx in 1:length(quantiles)){ # define custom labels 
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))
}
labels <- labels[1:length(labels)-1] # remove the last label 

# create a new variable on the dataset with the quantiles
map_edu_gap$DFLE_edu_gap_quantiles <- cut(map_edu_gap$DFLE_edu_gap, 
                                           breaks = quantiles, 
                                           labels = labels, 
                                           include.lowest = T)

map_edu_gap %>% 
  ggplot() + # ggplot of edu gap
  geom_sf(aes(fill = DFLE_edu_gap_quantiles,
              geometry = geometry),
          color = "black", size = 0.1)+
  coord_sf(datum = NA)+
  labs(x = element_blank(),
       y = element_blank(),
       fill = "Titolo") +
  scale_fill_manual(values = c("#b3cde0","#6497b1",
                               "#005b96","#03396c",
                               "#011f4b")) +
  facet_grid(cols=vars(factor(fct_rev(sex),
                              labels = c("Women","Men"))))+
  theme_minimal(base_size = 12) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) 


# Step 7: decomposition of the gaps ----
# here shown for educational gap (gender gap can be decomposed similarly)
sw_decomp_edu_data <- mortprev_ITA %>%
  group_by(reg,ageclass,sex,education,disability) %>% 
  select(mx,prev_w,ageclass) %>% 
  gather(par,value,mx,prev_w) %>% 
  ungroup() %>% 
  filter(disability=="Nessuna limitazione",
         education!="medio") %>% 
  group_by(reg,sex,disability) %>% 
  select(ageclass,education,reg,sex,disability,par,value) %>% 
  spread(education,value) %>% 
  ungroup()
# to have ordered parameters
sw_decomp_edu_data <- sw_decomp_edu_data[order(sw_decomp_edu_data$par),] 
sw_decomp_edu_data <- sw_decomp_edu_data[order(sw_decomp_edu_data$sex),]
sw_decomp_edu_data <- sw_decomp_edu_data[order(sw_decomp_edu_data$reg),]  %>% 
  group_by(reg,sex) %>% 
  mutate(contribution = stepwise_replacement(func = DFLE_fun_65,
                                             pars1 = basso,
                                             pars2 = alto),
         type = ifelse(par=="mx","mortality","morbidity"))  

sw_decomp_edu_data <- sw_decomp_edu_data %>% 
  group_by(reg,sex) %>%  # add variable with total gap
  mutate(gap = round(sum(contribution),1))

sw_decomp_edu_data <- sw_decomp_edu_data %>% # 10 year  age classes
  mutate(ageclas_2 = case_when(ageclas=="65-69"~"65-74",
                               ageclas=="70-74"~"65-74",
                               ageclas=="75-79"~"75-84",
                               ageclas=="80-84"~"75-84",
                               ageclas=="85+"~"85+"))

sw_decomp_edu_data_10y <- sw_decomp_edu_data %>%
  group_by(ageclas_2,reg,sex,disability,par,type,gap) %>% 
  summarise(contribution = sum(contribution)) # sum the contributions


# Step 8: plot of mortality/morbidity contributions to the gap ----
# here showned for Men (similarly for women)
sw_decomp_edu_data_10y %>% 
  filter(sesso == "M")%>%
  ggplot(aes(x=as.factor(ageclas_2),y=contribution,
             fill=type))+
  coord_flip()+
  facet_geo(facets = vars(reg), # geofaceting
            grid = "italy_grid3")+ 
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_discrete(type = c("chartreuse4","grey10"))+
  scale_x_discrete(labels=c("65-74","75-84","85+"))+
  geom_hline(yintercept=0)+
  scale_y_continuous(limits = c(-1,3),
                     breaks=seq(-0.5,2.5,by=1))+
  xlab("Age-classes")+
  labs(fill = "Effect")+
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))+
  geom_text(aes(label = gap,
                x=1.2, y = -0.6),
            color = "black")

