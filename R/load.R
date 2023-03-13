#### Load data ####
dat <- readxl::read_excel(here::here("data", "Dex_metan.xlsx"), range = "B2:T11")
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

dat.atelectasis <- metafor::escalc(measure="OR",
                          ai=ai_atel,
                          bi=bi_atel,
                          ci=ci_atel,
                          di=di_atel,
                          data=dat,
                          slab = study)

pm.atelectasis <- metafor::rma(yi, vi, data=dat.atelectasis, method="PM")
metafor::forest(pm.atelectasis,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))

# Sensitivity analysis: HKSJ correction results in narrower CIs:
pmhk.atelectasis <- metafor::rma(yi, vi, data=dat.atelectasis, method="PM", test="knha")
metafor::forest(pmhk.atelectasis,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))


#### Pneumonia ####

dat.pneumonia <- metafor::escalc(measure="OR",
                          ai=ai_pneu,
                          bi=bi_pneu,
                          ci=ci_pneu,
                          di=di_pneu,
                          data=dat,
                          slab = study)

pm.pneumonia <- metafor::rma(yi, vi, data=dat.pneumonia, method="PM")
metafor::forest(pm.pneumonia,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))

# Sensitivity analysis: HKSJ correction results in narrower CIs:
pmhk.pneumonia <- metafor::rma(yi, vi, data=dat.pneumonia, method="PM", test="knha")
metafor::forest(pmhk.pneumonia,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))


#### Hypoxemia ####

dat.hypoxemia <- metafor::escalc(measure="OR",
                        ai=ai_hypo,
                        bi=bi_hypo,
                        ci=ci_hypo,
                        di=di_hypo,
                        data=dat,
                        slab = study)

pm.hypoxemia <- metafor::rma(yi, vi, data=dat.hypoxemia, method="PM")
metafor::forest(pm.hypoxemia,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))

# Sensitivity analysis: HKSJ correction results in narrower CIs:
pmhk.hypoxemia <- metafor::rma(yi, vi, data=dat.hypoxemia, method="PM", test="knha")
metafor::forest(pmhk.hypoxemia,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))



#### ARDS ####

dat.ards <- metafor::escalc(measure="OR",
                        ai=ai_ards,
                        bi=bi_ards,
                        ci=ci_ards,
                        di=di_ards,
                        data=dat,
                        slab = study)

pm.ards <- metafor::rma(yi, vi, data=dat.ards, method="PM")
metafor::forest(pm.ards,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))

# Sensitivity analysis: HKSJ correction results in narrower CIs:
pmhk.ards <- metafor::rma(yi, vi, data=dat.ards, method="PM", test="knha")
metafor::forest(pmhk.ards,
       atransf=exp,
       at=log(c(0.002, 0.01, 0.03, 0.1, 0.4, 2, 5, 10)))

### NOTE: n=3 here, but n=2 in metafor::forest plot in PDF. Find out why.



#### Secondary analyses: Plateau pressure, peak pressure, resp compliance ####

#### Load data ####
dat.second <- readxl::read_excel(here::here("data", "Dex_metan.xlsx"), range = "B17:I64")
names(dat.second) <- c("study",
                       "m1i", "sd1i", "m2i", "sd2i",
                       "n1i", "n2i",
                       "outcome")
# Plateau pressure:
dat.plat <- metafor::escalc(measure="MD",
                   m1i=m1i,
                   sd1i=sd1i,
                   n1i=n1i,
                   m2i=m2i,
                   sd2i=sd2i,
                   n2i=n2i,
                   data=dplyr::filter(dat.second, outcome=="Plateau Pressure"),
                   slab = study)

pm.plat <- metafor::rma(yi, vi, data=dat.plat, method="PM")
metafor::forest(pm.plat,
       at=seq(from=-8, to =2, by=2))

# Peak pressure:
dat.peak <- metafor::escalc(measure="MD",
                   m1i=m1i,
                   sd1i=sd1i,
                   n1i=n1i,
                   m2i=m2i,
                   sd2i=sd2i,
                   n2i=n2i,
                   data=dplyr::filter(dat.second, outcome=="Peak Pressure"),
                   slab = study)

pm.peak <- metafor::rma(yi, vi, data=dat.peak, method="PM")
metafor::forest(pm.peak,
       at=seq(from=-8, to =2, by=2))

# Respiratory Compliance:
dat.resp <- metafor::escalc(measure="MD",
                   m1i=m1i,
                   sd1i=sd1i,
                   n1i=n1i,
                   m2i=m2i,
                   sd2i=sd2i,
                   n2i=n2i,
                   data=dplyr::filter(dat.second, outcome=="Respiratory Compliance"),
                   slab = study)

pm.resp <- metafor::rma(yi, vi, data=dat.resp, method="PM")
metafor::forest(pm.resp,
       at=seq(from=0, to =8, by=2))


# POD1:
dat.pod1<- metafor::escalc(measure="MD",
                   m1i=m1i,
                   sd1i=sd1i,
                   n1i=n1i,
                   m2i=m2i,
                   sd2i=sd2i,
                   n2i=n2i,
                   data=dplyr::filter(dat.second, outcome=="FEV1 POD 1"),
                   slab = study)

pm.pod1 <- metafor::rma(yi, vi, data=dat.pod1, method="PM")
metafor::forest(pm.pod1)

# POD2:
dat.pod2<- metafor::escalc(measure="MD",
                  m1i=m1i,
                  sd1i=sd1i,
                  n1i=n1i,
                  m2i=m2i,
                  sd2i=sd2i,
                  n2i=n2i,
                  data=dplyr::filter(dat.second, outcome=="FEV1 POD 2"),
                  slab = study)

pm.pod2 <- metafor::rma(yi, vi, data=dat.pod2, method="PM")
metafor::forest(pm.pod2)


#### Funnel plots ####

### carry out trim-and-fill analysis
taf.atelectasis <- metafor::trimfill(pm.atelectasis)
### draw funnel plot with missing studies filled in
metafor::funnel(taf.atelectasis, legend=TRUE)

taf.pneumonia <- metafor::trimfill(pm.pneumonia)
metafor::funnel(taf.pneumonia, legend=TRUE)


#usethis::use_vignette("Secondary_outcomes")
