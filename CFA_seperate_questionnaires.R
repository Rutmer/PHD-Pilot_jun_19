install.packages("lavaan", dependencies = TRUE); install.packages("svMisc")
library(foreign); library(lavaan); library(knitr); library(svMisc); library(knitr)
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
self_efficacy =~ q2 + q4'
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
summary(fit_em, fit.measures = TRUE) # No good fit 
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
summary(fit_SRL, fit.measures = TRUE) # model does not converge. Might be because of single indicator scales. I delete those.

SRL <- '
cogn.pl =~ q39 + q43
emo.pl =~ q40 + q44 + q41 + q45
cogn.mo =~ q46 + q49 + q52
emo.mo =~ q47 + q50 + q53 + q48
cogn.co =~ q54 + q58 + q62
emo.co =~ q55 + q59 + q63 + q66 + q69
mot.co =~ q56 + q60 + q64 + q67 + q68 + q70
res.co =~ q57 + q61 + q65 + q71
cogn.re =~ q72 + q76
emo.re =~ q73 + q77 + q74 + q78'
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # Bad fit
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # Still a mess. I will look at each scale in isolation.

planning <- '
cogn.pl =~ q39 + q43
emo.pl =~ q40 + q44 + q41 + q45'
fit_pl <- cfa(planning, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_pl, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p = .034), yet CFI (0.997) is high, TLI (0.995) is high, RMSEA (0.083, p = .148) is low, SRMR (.052) is low
modificationIndices(fit_pl, sort.=TRUE, minimum.value = 3) # q44 has mod index so if dropped

planning <- '
cogn.pl =~ q39 + q43
emo.pl =~ q40 + q41 + q45'
fit_pl <- cfa(planning, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_pl, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .234), CFI (0.999) is high, TLI (0.998) is high, RMSEA (0.050, p = .415) is low, SRMR (.022) is low
modificationIndices(fit_pl, sort.=TRUE, minimum.value = 3) # No more
parameterEstimates(fit_pl, standardized=TRUE) # All standardized factor loadings are high, so modification stops

monitoring <- '
cogn.mo =~ q46 + q49 + q52
emo.mo =~ q47 + q50 + q53 + q48'
fit_mo <- cfa(monitoring, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_mo, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .102), CFI (0.994) is high, TLI (0.991) is high, RMSEA (0.057, p = .366) is low, SRMR (.046) is low
modificationIndices(fit_mo, sort.=TRUE, minimum.value = 3) # q49 loads on multiple scales so is deleted

monitoring <- '
cogn.mo =~ q46 + q52
emo.mo =~ q47 + q50 + q53 + q48'
fit_mo <- cfa(monitoring, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_mo, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .487), CFI (1.000) is high, TLI (1.001) is high, RMSEA (0.000, p = .733) is low, SRMR (.030) is low
modificationIndices(fit_mo, sort.=TRUE, minimum.value = 3) # No more
parameterEstimates(fit_mo, standardized=TRUE) # All standardized factor loadings are high, so modification stops

controlling <- '
cogn.co =~ q54 + q58 + q62
emo.co =~ q55 + q59 + q63 + q66 + q69
mot.co =~ q56 + q60 + q64 + q67 + q68 + q70
res.co =~ q57 + q61 + q65 + q71'
fit_co <- cfa(controlling, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_co, fit.measures = TRUE) # No fit
modificationIndices(fit_co, sort.=TRUE, minimum.value = 3) # Emotion and motivation control have impossible correlations, so are collapsed

controlling <- '
cogn.co =~ q54 + q58 + q62
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q64 + q67 + q68 + q70
res.co =~ q57 + q61 + q65 + q71'
fit_co <- cfa(controlling, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_co, fit.measures = TRUE) # q65 has negative variance, so is dropped

controlling <- '
cogn.co =~ q54 + q58 + q62
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q64 + q67 + q68 + q70
res.co =~ q57 + q61 + q71'
fit_co <- cfa(controlling, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_co, fit.measures = TRUE) # No fit
modificationIndices(fit_co, sort.=TRUE, minimum.value = 3) # q71 has multiple mod indices, so is dropped

controlling <- '
cogn.co =~ q54 + q58 + q62
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q64 + q67 + q68 + q70
res.co =~ q57 + q61'
fit_co <- cfa(controlling, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_co, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p < .001), yet CFI (0.908) is high, TLI (0.891) is high, RMSEA (0.071, p = .0.022) is low but significant, SRMR (.105) is low
modificationIndices(fit_co, sort.=TRUE, minimum.value = 3) # res.co stays problematic, so I drop the entire scale

controlling <- '
cogn.co =~ q54 + q58 + q62
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q64 + q67 + q68 + q70'
fit_co <- cfa(controlling, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_co, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p < .001), yet CFI (0.939) is high, TLI (0.927) is high, RMSEA (0.065, p = .0.100) is low, SRMR (.098) is low
modificationIndices(fit_co, sort.=TRUE, minimum.value = 3) # q68 loads on multiple scales, so is deleted

controlling <- '
cogn.co =~ q54 + q58 + q62
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q64 + q67 + q70'
fit_co <- cfa(controlling, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_co, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p = .001), yet CFI (0.951) is high, TLI (0.940) is high, RMSEA (0.064, p = .0.146) is low, SRMR (.088) is low
modificationIndices(fit_co, sort.=TRUE, minimum.value = 3) # q58 has multiple mod indices, so is deleted

controlling <- '
cogn.co =~ q54 + q62
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q64 + q67 + q70'
fit_co <- cfa(controlling, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_co, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p < .001), yet CFI (0.957) is high, TLI (0.947) is high, RMSEA (0.065, p = .0.140) is low, SRMR (.084) is low
modificationIndices(fit_co, sort.=TRUE, minimum.value = 3) # q64 has multiple mod indices, so is deleted

controlling <- '
cogn.co =~ q54 + q62
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q67 + q70'
fit_co <- cfa(controlling, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_co, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p = .037), yet CFI (0.979) is high, TLI (0.973) is high, RMSEA (0.051, p = .0.449) is low, SRMR (.074) is low
modificationIndices(fit_co, sort.=TRUE, minimum.value = 3) # q67 has multiple mod indices, so is deleted

controlling <- '
cogn.co =~ q54 + q62
emo.co =~ q55 + q59 + q63 + q66 + q69 + q56 + q60 + q70'
fit_co <- cfa(controlling, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_co, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .488), CFI (1.000) is high, TLI (1.001) is high, RMSEA (0.000, p = .907) is low, SRMR (.059) is low
modificationIndices(fit_co, sort.=TRUE, minimum.value = 3) # to reduce items: q55 has mod index, so is deleted

controlling <- '
cogn.co =~ q54 + q62
emo.co =~ q59 + q63 + q66 + q69 + q56 + q60 + q70'
fit_co <- cfa(controlling, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_co, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .774), CFI (1.000) is high, TLI (1.017) is high, RMSEA (0.000, p = .970) is low, SRMR (.051) is low
modificationIndices(fit_co, sort.=TRUE, minimum.value = 3) #

controlling <- '
cogn.co =~ q54 + q62
emo.co =~ q59 + q63 + q66 + q69 + q56 + q60 + q70'
fit_co <- cfa(controlling, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_co, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .774), CFI (1.000) is high, TLI (1.017) is high, RMSEA (0.000, p = .970) is low, SRMR (.051) is low
modificationIndices(fit_co, sort.=TRUE, minimum.value = 3)
parameterEstimates(fit_co, standardized=TRUE) # To reduce items, q70 has mod index and low standardized factor loading, so deleted

controlling <- '
cogn.co =~ q54 + q62
emo.co =~ q59 + q63 + q66 + q69 + q56 + q60'
fit_co <- cfa(controlling, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_co, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .887), CFI (1.000) is high, TLI (1.022) is high, RMSEA (0.000, p = .983) is low, SRMR (.036) is low
modificationIndices(fit_co, sort.=TRUE, minimum.value = 3) # No more
parameterEstimates(fit_co, standardized=TRUE) # Model is parsimonious and has good fit. Modification stops.

reflecting <- '
cogn.re =~ q72 + q76 
emo.re =~ q73 + q77 + q74 + q78'
fit_re <- cfa(reflecting, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_re, fit.measures = TRUE) # cogn.re and emo.re correlate too high, so are collapsed

reflecting <- '
reflect =~ q72 + q76 + q73 + q77 + q74 + q78'
fit_re <- cfa(reflecting, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_re, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p = .036), yet CFI (0.997) is high, TLI (0.994) is high, RMSEA (0.079, p = .0.166) is low, SRMR (.029) is low
modificationIndices(fit_re, sort.=TRUE, minimum.value = 3) # q74 has mod index so is deleted

reflecting <- '
reflect =~ q72 + q76 + q73 + q77 + q78'
fit_re <- cfa(reflecting, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_re, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .099), CFI (0.998) is high, TLI (0.995) is high, RMSEA (0.073, p = .249) is low, SRMR (.027) is low
modificationIndices(fit_re, sort.=TRUE, minimum.value = 3) 
parameterEstimates(fit_re, standardized=TRUE) # to reduce items: q72 has mod index and lower standardized factor loading, so is deleted

reflecting <- '
reflect =~ q76 + q73 + q77 + q78'
fit_re <- cfa(reflecting, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_re, fit.measures = TRUE) # Good fit: Chi square is not significant (p = .537), CFI (1.000) is high, TLI (1.002) is high, RMSEA (0.000, p = .654) is low, SRMR (.012) is low
modificationIndices(fit_re, sort.=TRUE, minimum.value = 3) # no more
parameterEstimates(fit_re, standardized=TRUE) # Model is parsimonious and has good fit. Modification stops.

#Adding al "good fit models" of the seperate SRL scales into one model
SRL <- '
cogn.pl =~ q39 + q43
emo.pl =~ q40 + q41 + q45
cogn.mo =~ q46 + q52
emo.mo =~ q47 + q50 + q53 + q48
cogn.co =~ q54 + q62
emo.co =~ q59 + q63 + q66 + q69 + q56 + q60
reflect =~ q76 + q73 + q77 + q78'
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # No fit
lavInspect(fit_SRL, "cov.lv") # cogn.pl and emo.pl correlate too highly, so are collapsed

SRL <- '
plan =~ q39 + q43 + q40 + q41 + q45
cogn.mo =~ q46 + q52
emo.mo =~ q47 + q50 + q53 + q48
cogn.co =~ q54 + q62
emo.co =~ q59 + q63 + q66 + q69 + q56 + q60
reflect =~ q76 + q73 + q77 + q78'
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p = .001), yet CFI (0.984) is high, TLI (0.981) is high, RMSEA (0.044, p = .0.740) is low, SRMR (.065) is low
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # q43 loads on multiple scales, so is deleted

SRL <- '
plan =~ q39 + q40 + q41 + q45
cogn.mo =~ q46 + q52
emo.mo =~ q47 + q50 + q53 + q48
cogn.co =~ q54 + q62
emo.co =~ q59 + q63 + q66 + q69 + q56 + q60
reflect =~ q76 + q73 + q77 + q78'
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p = .003), yet CFI (0.984) is high, TLI (0.981) is high, RMSEA (0.044, p = .0.748) is low, SRMR (.065) is low
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) 
parameterEstimates(fit_SRL, standardized=TRUE) # q50 has mod index and low factor loading, so is deleted

SRL <- '
plan =~ q39 + q40 + q41 + q45
cogn.mo =~ q46 + q52
emo.mo =~ q47 + q53 + q48
cogn.co =~ q54 + q62
emo.co =~ q59 + q63 + q66 + q69 + q56 + q60
reflect =~ q76 + q73 + q77 + q78'
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p = .006), yet CFI (0.986) is high, TLI (0.983) is high, RMSEA (0.042, p = .0.780) is low, SRMR (.065) is low
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # q48 has mod index so is deleted

SRL <- '
plan =~ q39 + q40 + q41 + q45
cogn.mo =~ q46 + q52
emo.mo =~ q47 + q53
cogn.co =~ q54 + q62
emo.co =~ q59 + q63 + q66 + q69 + q56 + q60
reflect =~ q76 + q73 + q77 + q78'
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # Ambiguous fit: Chi square is significant (p = .024), yet CFI (0.989) is high, TLI (0.986) is high, RMSEA (0.038, p = .861) is low, SRMR (.062) is low
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) # 

#testing some more collapsing
SRL <- '
plan =~ q39 + q43 + q40 + q41 + q45
mon =~ q46 + q52 + q47 + q50 + q53 + q48
cogn.co =~ q54 + q62
emo.co =~ q59 + q63 + q66 + q69 + q56 + q60
reflect =~ q76 + q73 + q77 + q78'
fit_SRL <- cfa(SRL, data = pilot, ordered = TRUE, missing = "pairwise") 
summary(fit_SRL, fit.measures = TRUE) # 
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) 

# back to square one, before doing CFA on each SRL scale seperately
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
modificationIndices(fit_SRL, sort.=TRUE, minimum.value = 3) #
