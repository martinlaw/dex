librarian::shelf(here, readxl, metafor)

dat <- read_excel(here("data", "Dex_metan.xlsx"))
names(dat) <- c("study", "n1i", "n2i",
                "ai_atel", "bi_atel", "ci_atel", "di_atel",
                "ai_pneu", "bi_pneu", "ci_pneu", "di_pneu",
                "ai_hypo", "bi_hypo", "ci_hypo", "di_hypo",
                "ai_ards", "bi_ards", "ci_ards", "di_ards"
                )
dat <- dat[2:11, ]
dat

# First problem: there is no data for Lee et al. (2)
