
lapop.trend.inter <- read.csv("data/dataLAPOPinter.csv")
table(lapop.trend.inter$ctrycollect, useNA = "ifany")
lapop.trend.inter$ctrycollectyear <- paste0(lapop.trend.inter$ctrycollect, lapop.trend.inter$year)

# q10a	 Receives Remittances
table(lapop.trend.inter$q10a, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# infrax	 Police Response Time
table(lapop.trend.inter$infrax, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# 
# 
# l1	 Ideology (Left / Right)
table(lapop.trend.inter$l1, lapop.trend.inter$ctrycollectyear, useNA = "ifany")

# 
# ur	 	Urban/Rural
table(lapop.trend.inter$ur, lapop.trend.inter$ctrycollectyear, useNA = "ifany")

# q1	 Sex
table(lapop.trend.inter$q1, lapop.trend.inter$ctrycollectyear, useNA = "ifany")

# 
# etid	 	Ethnicity
table(lapop.trend.inter$etid, lapop.trend.inter$ctrycollectyear, useNA = "ifany")

# leng1	 	Mother Tongue
table(lapop.trend.inter$leng1, lapop.trend.inter$ctrycollectyear, useNA = "ifany")

# 

# q11n	 	Marital Status
table(lapop.trend.inter$q11n, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# q11n	1	Single
# q11n	2	Married
# q11n	3	Common law marriage (living together)
# q11n	4	Divorced
# q11n	5	Separated
# q11n	6	Widowed
# q11n	7	Civil Union (where applicable)
# 
# ocup1a Occupation

table(lapop.trend.inter$ocup1a, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# ocup1a	1	Salaried employee of government or state-owned enterprise
# ocup1a	2	Salaried employee in the private sector
# ocup1a	3	Owner or partner in a business
# ocup1a	4	Self-employed
# ocup1a	5	Unpaid worker
# 
# ocup4a Employment Status

table(lapop.trend.inter$ocup4a, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# ocup4a	1	Working
# ocup4a	2	Not working at the moment, but have a job
# ocup4a	3	Actively looking for a job
# ocup4a	4	Student
# ocup4a	5	Taking care of the home
# ocup4a	6	Retired, pensioner or permanently disabled to work
# ocup4a	7	Not working and not looking for a job
# 
# 
# 
# q10new	 	Monthly Household Income
table(lapop.trend.inter$q10new, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# q10e	 	Household Income Over Last Two Years
table(lapop.trend.inter$q10e, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# 
# 
# vic1exta	 	Victim of Crime in the Last 12 Months (Frequency)
table(lapop.trend.inter$vic1exta, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# exc20	 Soldier Requested a Bribe
table(lapop.trend.inter$exc20, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# exc11	Asked to Pay Bribe to Process Document in Municipality
table(lapop.trend.inter$exc11, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# exc13	Asked to Pay a Bribe at Work
table(lapop.trend.inter$q11n, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# exc14	Asked to Pay a Bribe to the Courts
table(lapop.trend.inter$exc14, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# exc15	Asked to Pay a Bribe to use Public Health Services
table(lapop.trend.inter$exc15, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# exc16	Asked to Pay a Bribe at School
table(lapop.trend.inter$exc16, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# 
# prot3	 Participated in a Protest
table(lapop.trend.inter$prot3, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# 
# 
# cp13	 Attendance at Meetings of Political Parties
table(lapop.trend.inter$cp13, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# cp6	 Attendance at Meetings of Religious Organization
table(lapop.trend.inter$cp6, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# cp7	 Attendance at Meetings of Parent Association
table(lapop.trend.inter$cp7, lapop.trend.inter$ctrycollectyear, useNA = "ifany")
# cp8	 Attendance at Meetings for Community Improvements
table(lapop.trend.inter$cp8, lapop.trend.inter$ctrycollectyear, useNA = "ifany")


#3
table(lapop.trend.inter$b1, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Courts Guarantee a Fair Trial (1=Not at all, 7=A lot)
table(lapop.trend.inter$b2, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Respect for Political Institutions (1=Not at all, 7=A lot)
table(lapop.trend.inter$b3, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Respect for Basic Rights (1=Not at all, 7=A lot)
table(lapop.trend.inter$b4, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Pride in Political System (1=Not at all, 7=A lot)
table(lapop.trend.inter$b6, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #People Should Support the Political System (1=Not at all, 7=A lot)
#table(lapop.trend.inter$b10a, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Trust in Judicial System (1=Not at all, 7=A lot)
table(lapop.trend.inter$b12, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Trust in Armed Forces (1=Not at all, 7=A lot)
table(lapop.trend.inter$b13, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Trust in National Legislature (1=Not at all, 7=A lot)
table(lapop.trend.inter$b18, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Trust in National Police (1=Not at all, 7=A lot)
#table(lapop.trend.inter$b21, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Trust in Political Parties (1=Not at all, 7=A lot)
#table(lapop.trend.inter$b21a, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Trust in Executive (1=Not at all, 7=A lot)
table(lapop.trend.inter$b32, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Trust in Local Government (1=Not at all, 7=A lot)
# table(lapop.trend.inter$b47a, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Trust in Elections (1=Not at all, 7=A lot)
# table(lapop.trend.inter$n9, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Evaluation of Administration's Handling of Corruption (1=Not at all, 7=A lot)
# table(lapop.trend.inter$n11, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Evaluation of Administration's Handling of Citizen Security (1=Not at all, 7=A lot)
# table(lapop.trend.inter$n15, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Evaluation of Administration's Handling of Economy (1=Not at all, 7=A lot)
# table(lapop.trend.inter$b3milx, lapop.trend.inter$ctrycollectyear, useNA = "ifany") #Armed Forces Respect Human Rights (1=Not at all, 7=A lot)

