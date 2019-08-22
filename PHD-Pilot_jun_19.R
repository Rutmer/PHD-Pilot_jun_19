install.packages("lavaan", dependencies = TRUE)
install.packages("svMisc")
library(foreign); library(lavaan); library(knitr); library(svMisc)

pilot <- read.spss("Pilot - Juli 2019.sav", to.data.frame = TRUE)

# Checking assumptions (outliers and multivariate normality) was done in spss

pilot [,c(11:89)] <- lapply(pilot[,c(11:88)], ordered)


srl_model <- ' 
self_efficacy =~ q1 + q2 + q3 + q4 + q5 

anger =~ q9 + q13 + q17
anxious =~ q10 + q14 + q18
boredom =~ q11 + q15 + q19
hopelessness =~ q12 + q16 + q20

external =~ q21 + q26 + q31
introjected =~ q22 + q27 + q32 + q23
identified =~ q28 + q33 + q24 + q29
intrinsic =~ q34 + q25 + q30

motivation =~ q35 + q36 + q37 + q38

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
res.re =~ q75
'

# Second model with planning, monitoring and reflecting aggregated to single variables. 
srl_model2 <- ' 
self_efficacy =~ q1 + q2 + q3 + q4 + q5 

anger =~ q9 + q13 + q17
anxious =~ q10 + q14 + q18
boredom =~ q11 + q15 + q19
hopelessness =~ q12 + q16 + q20

external =~ q21 + q26 + q31
introjected =~ q22 + q27 + q32 + q23
identified =~ q28 + q33 + q24 + q29
intrinsic =~ q34 + q25 + q30

motivation =~ q35 + q36 + q37 + q38

planning =~ q39 + q43 + q40 + q44 + q41 + q45 + q42

monitoring =~ q46 + q49 + q52 + q47 + q50 + q53 + q48 + q51

cogn.co =~ q54 + q58 + q62
emo.co =~ q55 + q59 + q63 + q66 + q69
mot.co =~ q56 + q60 + q64 + q67 + q68 + q70
res.co =~ q57 + q61 + q65 + q71

reflecting =~ q72 + q76 + q73 + q77 + q74 + q78 + q75
'

# Third model based on EFA (try12: 16 factoren). 
srl_model3 <- ' 
self_efficacy =~ q1 + q4 + q5 

anger =~ q9 + q13 + q17
anxious =~ q10 + q14
boredom =~ q11 + q15 + q19
hopelessness =~ q12 + q16 + q20

external1 =~ q21 + q32 + q23
exeternal2 =~ q26 + q22 + q27
identified =~ q28 + q33 + q29
intrinsic =~ q34 + q25 + q30

motivation =~ q35 + q37 + q38

planning =~ q39 + q43 + q40 + q44 + q41 + q45

monitoring =~ q46 + q49 + q52 + q47 + q50 + q53

cogn.co =~ q54 + q58 + q62 
rest1.co =~ q55 + q56 + q60
rest2.co =~ q59 + q63+ q66 + q64 + q67

reflecting =~ q72 + q76 + q73 + q77 + q74 + q78
'

# fourth model based on EFA (try12: 16 factoren) + dropped heywood case q23. 
srl_model4 <- ' 
self_efficacy =~ q1 + q4 + q5 

anger =~ q9 + q13 + q17
anxious =~ q10 + q14
boredom =~ q11 + q15 + q19
hopelessness =~ q12 + q16 + q20

external1 =~ q21 + q32
exeternal2 =~ q26 + q22 + q27
identified =~ q28 + q33 + q29
intrinsic =~ q34 + q25 + q30

motivation =~ q35 + q37 + q38

planning =~ q39 + q43 + q40 + q44 + q41 + q45

monitoring =~ q46 + q49 + q52 + q47 + q50 + q53

cogn.co =~ q54 + q58 + q62 
rest1.co =~ q55 + q56 + q60
rest2.co =~ q59 + q63+ q66 + q64 + q67

reflecting =~ q72 + q76 + q73 + q77 + q74 + q78
'

fit <- cfa(srl_model4, data = pilot, ordered = TRUE, missing = "pairwise") 

summary(fit, fit.measures = TRUE)
lavInspect(fit, "cov.lv")
lavInspect(fit, "vcov")
