library(librarian)
librarian::shelf(here, readxl, metafor, martinlaw/dex)

#### Load data ####
dat <- read_excel(here("data", "Dex_metan.xlsx"), range = "B2:T11")
names(dat) <- c("study", "n1i", "n2i",
                "ai_atel", "bi_atel", "ci_atel", "di_atel",
                "ai_pneu", "bi_pneu", "ci_pneu", "di_pneu",
                "ai_hypo", "bi_hypo", "ci_hypo", "di_hypo",
                "ai_ards", "bi_ards", "ci_ards", "di_ards"
                )
dat

# There is no data for Lee et al. (final row)


#### Primary Analyses ####

#### Atelectasis ####

dat.atelectasis <- escalc(measure="OR",
                          ai=ai_atel,
                          bi=bi_atel,
                          ci=ci_atel,
                          di=di_atel,
                          data=dat,
                          slab = study)

pm.atelectasis <- rma(yi, vi, data=dat.atelectasis, method="PM")
forest(pm.atelectasis,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))

# Sensitivity analysis: HKSJ correction results in narrower CIs:
pmhk.atelectasis <- rma(yi, vi, data=dat.atelectasis, method="PM", test="knha")
forest(pmhk.atelectasis,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))


#### Pneumonia ####

dat.pneumonia <- escalc(measure="OR",
                          ai=ai_pneu,
                          bi=bi_pneu,
                          ci=ci_pneu,
                          di=di_pneu,
                          data=dat,
                          slab = study)

pm.pneumonia <- rma(yi, vi, data=dat.pneumonia, method="PM")
forest(pm.pneumonia,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))

# Sensitivity analysis: HKSJ correction results in narrower CIs:
pmhk.pneumonia <- rma(yi, vi, data=dat.pneumonia, method="PM", test="knha")
forest(pmhk.pneumonia,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))


#### Hypoxemia ####

dat.hypoxemia <- escalc(measure="OR",
                        ai=ai_hypo,
                        bi=bi_hypo,
                        ci=ci_hypo,
                        di=di_hypo,
                        data=dat,
                        slab = study)

pm.hypoxemia <- rma(yi, vi, data=dat.hypoxemia, method="PM")
forest(pm.hypoxemia,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))

# Sensitivity analysis: HKSJ correction results in narrower CIs:
pmhk.hypoxemia <- rma(yi, vi, data=dat.hypoxemia, method="PM", test="knha")
forest(pmhk.hypoxemia,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))



#### ARDS ####

dat.ards <- escalc(measure="OR",
                        ai=ai_ards,
                        bi=bi_ards,
                        ci=ci_ards,
                        di=di_ards,
                        data=dat,
                        slab = study)

pm.ards <- rma(yi, vi, data=dat.ards, method="PM")
forest(pm.ards,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))

# Sensitivity analysis: HKSJ correction results in narrower CIs:
pmhk.ards <- rma(yi, vi, data=dat.ards, method="PM", test="knha")
forest(pmhk.ards,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))

### NOTE: n=3 here, but n=2 in forest plot in PDF. Find out why.



#### Secondary analyses: Plateau pressure, peak pressure, resp compliance ####

#### Load data ####
dat.second <- read_excel(here("data", "Dex_metan.xlsx"), range = "B17:I64")
names(dat.second) <- c("study",
                       "m1i", "sd1i", "m2i", "sd2i",
                       "n1i", "n2i",
                       "outcome")
# Plateau pressure:
dat.plat <- escalc(measure="MD",
                   m1i=m1i,
                   sd1i=sd1i,
                   n1i=n1i,
                   m2i=m2i,
                   sd2i=sd2i,
                   n2i=n2i,
                   data=dplyr::filter(dat.second, outcome=="Plateau Pressure"),
                   slab = study)

pm.plat <- rma(yi, vi, data=dat.plat, method="PM")
forest(pm.plat,
       at=seq(from=-8, to =2, by=2))

# Peak pressure:
dat.peak <- escalc(measure="MD",
                   m1i=m1i,
                   sd1i=sd1i,
                   n1i=n1i,
                   m2i=m2i,
                   sd2i=sd2i,
                   n2i=n2i,
                   data=dplyr::filter(dat.second, outcome=="Peak Pressure"),
                   slab = study)

pm.peak <- rma(yi, vi, data=dat.peak, method="PM")
forest(pm.peak,
       at=seq(from=-8, to =2, by=2))

# Respiratory Compliance:
dat.resp <- escalc(measure="MD",
                   m1i=m1i,
                   sd1i=sd1i,
                   n1i=n1i,
                   m2i=m2i,
                   sd2i=sd2i,
                   n2i=n2i,
                   data=dplyr::filter(dat.second, outcome=="Respiratory Compliance"),
                   slab = study)

pm.resp <- rma(yi, vi, data=dat.resp, method="PM")
forest(pm.resp,
       at=seq(from=0, to =8, by=2))


# POD1:
dat.pod1<- escalc(measure="MD",
                   m1i=m1i,
                   sd1i=sd1i,
                   n1i=n1i,
                   m2i=m2i,
                   sd2i=sd2i,
                   n2i=n2i,
                   data=dplyr::filter(dat.second, outcome=="FEV1 POD 1"),
                   slab = study)

pm.pod1 <- rma(yi, vi, data=dat.pod1, method="PM")
forest(pm.pod1)

# POD2:
dat.pod2<- escalc(measure="MD",
                  m1i=m1i,
                  sd1i=sd1i,
                  n1i=n1i,
                  m2i=m2i,
                  sd2i=sd2i,
                  n2i=n2i,
                  data=dplyr::filter(dat.second, outcome=="FEV1 POD 2"),
                  slab = study)

pm.pod2 <- rma(yi, vi, data=dat.pod2, method="PM")
forest(pm.pod2)


#### Funnel plots ####

### carry out trim-and-fill analysis
taf.atelectasis <- trimfill(pm.atelectasis)
### draw funnel plot with missing studies filled in
funnel(taf.atelectasis, legend=TRUE)

taf.pneumonia <- trimfill(pm.pneumonia)
funnel(taf.pneumonia, legend=TRUE)


#usethis::use_vignette("Secondary_outcomes")
