
//------------- 1 Import Data -------------

clear 

set more off

cd "G:\Meine Ablage\Uni Köln\NCCU\Data Science and Inference\Term Paper"

import delimited using  titanic.csv, clear varnames(1)

//------------- 2 Data Preperation -------------

describe // get data type for each variable

destring age ticketno fare sibsp parch, force replace

// Delete observations of crew 
keep if class == "1st" | class == "2nd" | class=="3rd"

// Create dummy variable of survived (0 = passanger died and 1 = passanger survived)
generate survived_dummy = 0
replace survived_dummy = 1 if survived  == "yes"

// Create dummy variable of died (0 = passanger survived and 1 = passanger died)
generate died_dummy = 0
replace died_dummy = 1 if survived  == "no"

// Create dummy variable female (0 = male  and 1 = female)
generate female = 0 
replace female = 1 if gender  == "female"

// Create dummy variable for passengers in first class (in 1st class=1, in 2nd or 3rd class = 0 )
generate firstclass = 0
replace firstclass = 1 if class  == "1st"

// Create individula dummy variables for passenger classes
generate class1 = 0
replace class1 = 1 if class  == "1st"

generate class2 = 0
replace class2 = 1 if class  == "2nd"

generate class3 = 0
replace class3 = 1 if class  == "3rd"

// Create dummy variables to group country (citizenship) variable
generate USA = 0
replace USA = 1 if country  == "United States"

generate United_Kingdom = 0 
replace United_Kingdom = 1 if country == "England" | country  == "Ireland" | country  == "Northern Ireland" | country == "Wales" | country == "Scotland"

generate  British_Empire = 0
replace British_Empire = 1 if country  == "Australia" | country  == "Canada" | country  == "Channel Islands" | country  == " China/Hong Kong" | country  == "Egypt" | country  == "India"| country  == "South Africa" 

generate  rest_Europe = 0
replace rest_Europe = 1 if country  == "Austria" | country  == "Belgium" | country  == "Bosnia" | country  == "Bulgaria" | country  == "Croatia"| country  == "Croatia (Modern)" | country  == "Denmark" | country  == "Finland" | country  == "France" | country  == "Germany" | country  == "Greece" | country  == "Hungary" | country  == "Italy" | country  == "Lativa" | country  == "Netherlands" | country  == "Norway" | country  == "Poland" | country  == "Russia" | country  == "Slovakia (Modern day)" | country  == "Slovenia" | country  == "Spain" | country  == "Sweden" | country  == "Switzerland" | country  == "Turkey" | country  == "Yugoslavia"

generate  others = 0
replace others = 1 if USA != 1 & United_Kingdom != 1 & British_Empire !=1  & rest_Europe != 1

// Create dummy variables for embarkment
generate  Southampton = 0
replace Southampton = 1 if embarked == "S" | embarked == "B"

generate  Cherbourg = 0
replace Cherbourg = 1 if embarked == "C"

generate  Queensland = 0
replace Queensland = 1 if embarked == "Q"

// Create squared age variable
gen age2 = age^2

// Create numeric versions of string variables
encode class, gen(class_num)

encode embarked, gen(embarked_num)

encode gender, gen(gender_num)


//------------- 3 Data Exploration -------------

bysort class: sum survived_dummy // mean survival rate by class

sum age, detail // mean and median

tab survived_dummy

tab country

tab class_num

codebook survived_dummy //checking for missing values

duplicates report // checking if all variables have the same number of rows

duplicates report name // check for duplicates of the identifying variable

duplicates list name

// Bar plot of passanger classes over survival count
graph bar (sum) survived_dummy died_dummy, over(class_num) stack ytitle("") bar(1, fcolor(navy)) bar(2, fcolor(maroon)) legend(label(1 "survived")) legend(label(2 "died"))


//-------------  4 Linear Regression Base Model and Tests -------------

// Base model 
regress survived_dummy firstclass, vce(robust)

// Test for homoscedastisity (Breusch-Pagan)
estat hettest

// Test indicates no heteroscedastic error terms i.e. no need for robust standart errors to correct for that
regress survived_dummy class1 class2 class3, vce(robust)

// Test for non-linearities of age covariate 
regress survived_dummy firstclass female age age2

// Testing for potential endogeneity
reg age firstclass, vce(robust)

//-------------  5 Linear Regression with covariates -------------

// Approach A: Controlling for: age and female 
regress survived_dummy firstclass age female, vce(robust)

// Use homoscedastic-robust standard errors since Breusch-Pagan positive
//command : estat hettest


// Approach B: Controlling for: age, female and embarkment  
regress survived_dummy firstclass age female Southampton Cherbourg, vce(robust)

// F-test for significance of controlling for embarkment
test Southampton Cherbourg // significant


// Approach C: Controlling for: age, female, embarkment and citizenship 
regress survived_dummy firstclass age female Southampton Cherbourg USA United_Kingdom British_Empire rest_Europe, vce(robust)

// F-test for significance of controlling for citzenship categories
test USA United_Kingdom British_Empire rest_Europe // not significant


//------------- 6 Logit/Probit Regression -------------

// Approach A: Controlling for: first class
logit survived_dummy firstclass, vce(robust)

margins, dydx(firstclass)

// Approach B: Controlling for: age and female 
logit survived_dummy firstclass age female, vce(robust) or

margins, dydx(firstclass age female)

// Approach C: Controlling for: age, female, embarkment 
logit survived_dummy firstclass age female Southampton Cherbourg, vce(robust) or

margins, dydx(firstclass age female Southampton Cherbourg)

// Approach D: Controlling for: age, female, embarkment and citizenship 
logit survived_dummy firstclass age female Southampton Cherbourg USA United_Kingdom British_Empire rest_Europe, vce(robust) or

margins, dydx(firstclass age female Southampton Cherbourg USA United_Kingdom British_Empire rest_Europe)


//------------- 7 Matching -------------

// Step 1: Examine Difference in Covariate age in Pre-matching Data (Test if we need matching approach)

// Regression of covariates on treatment
reg age firstclass

// t-test to test differences in covariates in Pre-matchin data 
ttest age, by(firstclass)

ttest female, by(firstclass)


// Density plot of age over passangers in first class or not in first class
kdensity age if firstclass == 1, addplot(kdensity age if firstclass == 0) ///
legend(ring(0) pos(1) label(1 "1st class") label(2 "not 1st class"))

// Step 2: Matching 

//Approach A: Matching on specific covariate (1st class as treatment)

teffects nnmatch (survived_dummy age female) (firstclass), vce(robust)
 
teffects nnmatch (survived_dummy age female Southampton Cherbourg) (firstclass), vce(robust)

teffects nnmatch (survived_dummy age female Southampton Cherbourg USA United_Kingdom British_Empire rest_Europe) (firstclass), vce(robust)


// Approach B: Propensity Score Matching (1st class as treatment)

teffects psmatch (survived_dummy) (firstclass age female, logit), vce(robust) nn(1)  ate

teffects psmatch (survived_dummy) (firstclass age female Southampton Cherbourg, logit), vce(robust) nn(1)  ate

teffects psmatch (survived_dummy) (firstclass age female Southampton Cherbourg USA United_Kingdom British_Empire rest_Europe, logit), vce(robust) nn(1)  ate  


// Step 3: Post-Matching Analysis

// Checking for covariate balance after matching

// Balance of age 
tebalance density age

tebalance box age

// Balance of Propensity Score
tebalance density

// Matching also on citizenship ???? (dummies for citzenship were not significant in normal regression...)











