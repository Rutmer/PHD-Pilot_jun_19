install.packages("lavaan", dependencies = TRUE); install.packages("svMisc")
library(foreign); library(lavaan); library(knitr); library(svMisc);
options(knitr.kable.NA = '') # this will hide missing values in the kable table

pilot <- read.spss("Pilot - Juli 2019.sav", to.data.frame = TRUE)
pilot [,c(11:89)] <- lapply(pilot[,c(11:88)], ordered) # Ordering ordinal data

# Checking assumptions (outliers and multivariate normality) was done in spss

# CFA of effort questionnaire
effort <- '
effort =~ q35 + q36 + q37 + q38 
self_efficacy =~ q1 + q2 + q4 + q5 + q3'
fit_ef <- cfa(effort, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_ef, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .078), CFI (0.991) is high, TLI (0.988) is high, RMSEA (0.051, p = .454) is low, SRMR (.065) is low 
modificationIndices(fit_ef, sort.=TRUE, minimum.value = 3) # to reduce items: q35 and q36 load on both scales so are deleted

effort <- '
effort =~ q37 + q38 
self_efficacy =~ q1 + q2 + q4 + q5 + q3'
fit_ef <- cfa(effort, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_ef, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .140), CFI (0.995) is high, TLI (0.993) is high, RMSEA (0.051, p = .438) is low, SRMR (.043) is low 
modificationIndices(fit_ef, sort.=TRUE, minimum.value = 3) # to reduce items: q3 has modification index so is deleted

effort <- '
effort =~ q37 + q38 
self_efficacy =~ q1 + q2 + q4 + q5'
fit_ef <- cfa(effort, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_ef, fit.measures = TRUE) # Good fit because: Chi square is not significant (p = .495), CFI (1.000) is high, TLI (1.001) is high, RMSEA (0.000, p = .741) is low, SRMR (.035) is low 
modificationIndices(fit_ef, sort.=TRUE, minimum.value = 3) # no more
parameterEstimates(fit_ef, standardized=TRUE) # to reduce items: standardized factor loadings are somewhat low for q1 (0.770) and q5 (0.749) so are deleted

effort <- '
effort =~ q37 + q38 
self_efficacy =~ q2 + q4
demand =~ q6 '
fit_ef <- cfa(effort, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_ef, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .936), CFI (1.000) is high, TLI (1.010) is high, RMSEA (0.000, p = .947) is low, SRMR (.001) is low 
modificationIndices(fit_ef, sort.=TRUE, minimum.value = 3)
parameterEstimates(fit_ef, standardized=TRUE) # model is parsimonious and has good fit, so modification stops

# CFA of demands questionnaire
demand <- 'demand =~ q6 ' # how do we deal with single indicator CFA?
fit_de <- cfa(demand, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_de, fit.measures = TRUE, standardized=TRUE) # No fit ...


# CFA of emotions questionnaire
emotion <- '
anger =~ q9 + q13 + q17
anxious =~ q10 + q14 + q18
boredom =~ q11 + q15 + q19
hopelessness =~ q12 + q16 + q20 '
fit_em <- cfa(emotion, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_em, fit.measures = TRUE) # NAANVULLEN
modificationIndices(fit_em, sort.=TRUE, minimum.value = 3) # q9, q18 and q12 load on multiple scales so are deleted

emotion <- '
anger =~ q13 + q17
anxious =~ q10 + q14
boredom =~ q11 + q15 + q19
hopelessness =~ q16 + q20 '
fit_em <- cfa(emotion, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_em, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .652), CFI (1.000) is high, TLI (1.002) is high, RMSEA (0.000, p = .924) is low, SRMR (.056) is low  
modificationIndices(fit_em, sort.=TRUE, minimum.value = 3) # no more
parameterEstimates(fit_em, standardized=TRUE) # to reduce items: standardized factor loading is somewhat low for q11 (0.831) so is deleted

emotion <- '
anger =~ q13 + q17
anxious =~ q10 + q14
boredom =~ q15 + q19
hopelessness =~ q16 + q20 '
fit_em <- cfa(emotion, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_em, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .722), CFI (1.000) is high, TLI (1.005) is high, RMSEA (0.000, p = .922) is low, SRMR (.044) is low  
modificationIndices(fit_em, sort.=TRUE, minimum.value = 3) # no more
parameterEstimates(fit_em, standardized=TRUE) # model is parsimonious and has good fit, so modification stops


# CFA of motivation questionnaire
motivation <- '
external =~ q21 + q26 + q31
introjected =~ q22 + q27 + q32 + q23
identified =~ q28 + q33 + q24 + q29
intrinsic =~ q34 + q25 + q30 '
fit_mo <- cfa(motivation, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_mo, fit.measures = TRUE) # Bad fit
modificationIndices(fit_mo, sort.=TRUE, minimum.value = 3) # q21, q22, and q23 load on multiple scales so are deleted

motivation <- '
external =~  q26 + q31
introjected =~  q27 + q32
identified =~ q28 + q33 + q24 + q29
intrinsic =~ q34 + q25 + q30 '
fit_mo <- cfa(motivation, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_mo, fit.measures = TRUE) # Bad fit
modificationIndices(fit_mo, sort.=TRUE, minimum.value = 3) # still a mess, maybe it improves if we collapse external and introjected

motivation <- '
external =~  q26 + q31 + q27 + q32
identified =~ q28 + q33 + q24 + q29
intrinsic =~ q34 + q25 + q30 '
fit_mo <- cfa(motivation, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_mo, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p = .032), yet CFI (0.993) is high, TLI (0.991) is high, RMSEA (0.053, p = .411) is low, SRMR (.066) is low  
modificationIndices(fit_mo, sort.=TRUE, minimum.value = 3) # q32 loads on multiple scales so deleted

motivation <- '
external =~  q26 + q31 + q27
identified =~ q28 + q33 + q24 + q29
intrinsic =~ q34 + q25 + q30 '
fit_mo <- cfa(motivation, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_mo, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .245), CFI (0.999) is high, TLI (0.997) is high, RMSEA (0.032, p = .752) is low, SRMR (.050) is low  
modificationIndices(fit_mo, sort.=TRUE, minimum.value = 3) # to reduce items: from identified scale q24 is worst (has mod index) so is dropped

motivation <- '
external =~  q26 + q31 + q27
identified =~ q28 + q33 + q29
intrinsic =~ q34 + q25 + q30 '
fit_mo <- cfa(motivation, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_mo, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .248), CFI (0.998) is high, TLI (0.997) is high, RMSEA (0.033, p = .696) is low, SRMR (.052) is low  
modificationIndices(fit_mo, sort.=TRUE, minimum.value = 3) # I leave it at this for now, even though there is quite some high mod indices left.


# CFA of SRL questionnaire
SRL <- '
cogn.pl =~ q39 + q43
emo.pl =~ q40 + q44 + q41 + q45
res.pl =~ q42
cogn.mo =~ q46 + q49 + q52
emo.mo =~ q47 + q50 + q53 + q48
res.mo =~ q51
cogn.co =~ q54 + q58 + q62
emo.co =~ q55 + q59 + q63 + q66 + q69
mot.co =~ q56 + q60 + q64 + q67 + q68 + q70
res.co =~ q57 + q61 + q65 + q71
cogn.re =~ q72 + q76
emo.re =~ q73 + q77 + q74 + q78
res.re =~ q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # no fit
lavInspect(fit_SRL, "cov.lv") # planning scales correlate too highly, so are collapsed

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
cogn.mo =~ q46 + q49 + q52
emo.mo =~ q47 + q50 + q53 + q48
res.mo =~ q51
cogn.co =~ q54 + q58 + q62
emo.co =~ q55 + q59 + q63 + q66 + q69
mot.co =~ q56 + q60 + q64 + q67 + q68 + q70
res.co =~ q57 + q61 + q65 + q71
cogn.re =~ q72 + q76
emo.re =~ q73 + q77 + q74 + q78
res.re =~ q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # no fit
lavInspect(fit_SRL, "cov.lv") # reflecting scales correlate too highly, so are collapsed

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
cogn.mo =~ q46 + q49 + q52
emo.mo =~ q47 + q50 + q53 + q48
res.mo =~ q51
cogn.co =~ q54 + q58 + q62
emo.co =~ q55 + q59 + q63 + q66 + q69
mot.co =~ q56 + q60 + q64 + q67 + q68 + q70
res.co =~ q57 + q61 + q65 + q71
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # no fit
lavInspect(fit_SRL, "cov.lv") # Cognitive control and resource control have negative covariance, so are collapsed

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
cogn.mo =~ q46 + q49 + q52
emo.mo =~ q47 + q50 + q53 + q48
res.mo =~ q51
cogn.co =~ q54 + q58 + q62 + q57 + q61 + q65 + q71
emo.co =~ q55 + q59 + q63 + q66 + q69
mot.co =~ q56 + q60 + q64 + q67 + q68 + q70
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # no fit
lavInspect(fit_SRL, "cov.lv") #
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # monitoring scales appear problematic, so are collapsed

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
monitor =~ q46 + q49 + q52 + q47 + q50 + q53 + q48 + q51
cogn.co =~ q54 + q58 + q62 + q57 + q61 + q65 + q71
emo.co =~ q55 + q59 + q63 + q66 + q69
mot.co =~ q56 + q60 + q64 + q67 + q68 + q70
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # no fit
lavInspect(fit_SRL, "cov.lv") #
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # Emotion control and motivation control correlate too highly, so are collapsed

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
monitor =~ q46 + q49 + q52 + q47 + q50 + q53 + q48 + q51
cogn.co =~ q54 + q58 + q62 + q57 + q61 + q65 + q71
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q64 + q67 + q68 + q70
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # no fit
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # q62 loads on multiple scales, so is deleted

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
monitor =~ q46 + q49 + q52 + q47 + q50 + q53 + q48 + q51
cogn.co =~ q54 + q58 + q57 + q61 + q65 + q71
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q64 + q67 + q68 + q70
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # no fit
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # q71 loads on multiple scales, so is deleted

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
monitor =~ q46 + q49 + q52 + q47 + q50 + q53 + q48 + q51
cogn.co =~ q54 + q58 + q57 + q61 + q65
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q64 + q67 + q68 + q70
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # no fit
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # q64 loads on multiple scales, so is deleted

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
monitor =~ q46 + q49 + q52 + q47 + q50 + q53 + q48 + q51
cogn.co =~ q54 + q58 + q57 + q61 + q65
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q67 + q68 + q70
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # no fit
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # q46 loads on multiple scales, so is deleted

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
monitor =~ q49 + q52 + q47 + q50 + q53 + q48 + q51
cogn.co =~ q54 + q58 + q57 + q61 + q65
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q67 + q68 + q70
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # no fit
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # q52 loads on multiple scales, so is deleted

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
monitor =~ q49 + q47 + q50 + q53 + q48 + q51
cogn.co =~ q54 + q58 + q57 + q61 + q65
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q67 + q68 + q70
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # no fit
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # q67 loads on multiple scales, so is deleted

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
monitor =~ q49 + q47 + q50 + q53 + q48 + q51
cogn.co =~ q54 + q58 + q57 + q61 + q65
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q68 + q70
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # no fit
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) #q51 loads on multiple scales, so is deleted

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
monitor =~ q49 + q47 + q50 + q53 + q48
cogn.co =~ q54 + q58 + q57 + q61 + q65
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q68 + q70
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # no fit
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # q61 and q65 appear to belong to a seperate scale, so are deleted

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
monitor =~ q49 + q47 + q50 + q53 + q48
cogn.co =~ q54 + q58 + q57
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q68 + q70
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # no fit
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # q70 has multiple mod indices, so is deleted

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
monitor =~ q49 + q47 + q50 + q53 + q48
cogn.co =~ q54 + q58 + q57
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q68
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p = .005), yet CFI (0.989) is high, TLI (0.987) is high, RMSEA (0.035, p = .988) is low, SRMR (.068) is low
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # q56 loads on multipe scales, so is deleted

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
monitor =~ q49 + q47 + q50 + q53 + q48
cogn.co =~ q54 + q58 + q57
emo.co =~ q55 + q59 + q63 + q66 + q69 + q60 + q68
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # No fit
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # q57 loads on multiple scales, so is deleted

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
monitor =~ q49 + q47 + q50 + q53 + q48
cogn.co =~ q54 + q58
emo.co =~ q55 + q59 + q63 + q66 + q69 + q60 + q68
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p = .020), yet CFI (0.991) is high, TLI (0.990) is high, RMSEA (0.032, p = .992) is low, SRMR (.064) is low
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # q50 loads on multiple scales, so is deleted

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45 + q42
monitor =~ q49 + q47 + q53 + q48
cogn.co =~ q54 + q58
emo.co =~ q55 + q59 + q63 + q66 + q69 + q60 + q68
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75 '
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p = .049), yet CFI (0.993) is high, TLI (0.992) is high, RMSEA (0.029, p = .995) is low, SRMR (.063) is low
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # q42 loads on multiple scales, so is deleted

SRL <- '
plan =~ q39 + q43 + q40 + q44 + q41 + q45
monitor =~ q49 + q47 + q53 + q48
cogn.co =~ q54 + q58
emo.co =~ q55 + q59 + q63 + q66 + q69 + q60 + q68
reflect =~ q72 + q76 + q73 + q77 + q74 + q78 + q75'
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .062), CFI (0.993) is high, TLI (0.993) is high, RMSEA (0.029, p = .995) is low, SRMR (.063) is low
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) 


