# quantitative data
r_a = c(13,11,10,10,10,9,9,9,8,8,8,7,7,7,7,7,7,7,7,7,7,6,6,6,6,6,5,5,5,5,5,5,5,5,5,5,4,4,4,4,4,4,4,3,3,2,0,0)
r_b = c(2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
r_c = c(2,2,2,2,1,2,2,1,2,1,2,2,999,1,1,2,2,1,2,1,2,1,2,2,1,2,2,2,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,1,2)
r_d = c(34,28,32,30,33,30,31,31,33,28,29,32,28,29,999,30,999,33,28,30,32,29,999,32,999,31,30,28,28,32,29,999,31,31,36,32,33,31,31,31,35,31,29,32,32,30,33,30)
r_e = c(3,1,3,2,3,2,3,1,3,2,2,4,1,1,2,2,3,2,1,2,1,2,3,1,2,2,2,1,1,4,1,3,3,3,2,3,2,1,2,2,3,3,2,1,3,3,1,1)
r_f = c(4,2,1,3,4,1,3,4,1,2,3,1,1,1,2,2,2,2,3,4,4,1,2,2,2,4,1,1,1,1,2,2,3,3,3,4,1,1,2,2,2,3,3,2,2,1,3,4)

# categorical data
accprov = factor(r_a)
survey = factor(r_b)
sex = factor(r_c)
age = factor(r_d)
year = factor(r_e)
school = factor(r_f)

# quatitative analysis
Yq = cbind(r_c,r_d,r_e,r_f)
fit_q = manova(Yq ~ r_b)
q1 = summary.aov(fit_q)  # univariate ANOVA tables
q2 = summary(fit_q, test = "Wilks")  # ANOVA table of Wilks' lambda
q3 = summary(fit_q)

# categorical analysis
Yc = cbind(sex,age,year,school)
fit_c = manova(Yc ~ survey)
c1 = summary.aov(fit_c)  # univariate ANOVA tables
c2 = summary(fit_c, test = "Wilks")  # ANOVA table of Wilks' lambda
c3 = summary(fit_c)
