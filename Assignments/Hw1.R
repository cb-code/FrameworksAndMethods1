#Assignment 1: Data Exploration

#Question 1.1: Compute 12345678 multiplied by 87654321 (Copy-paste answer from R)
#Answer 1.1: 1.082152e+15

12345678*87654321

#Question 1.2: What is the square root of 111222333444555666. Do not round answer from R.
#Answer 1.2: 333,500,125

sqrt(111222333444555666)

#Question 1.3: What is the log to base 10 of 111222333444555666. Do not round answer from R.
#Answer 1.3: 17.0462

log10(111222333444555666)

#Question 1.4: How many IDs does the following code generate?
#Answer 1.4: 10

paste('ID',seq(from=10,to=100,by=10))

#Question 1.5: How many observations does the following code generate?
#Hint: You could either look at the function argument or run length() on the entire expression
#Answer 1.5: 10,243

length(rnorm(n=10243,mean = 10,sd = 5))

#Question 1.6: What is the class for 5?
#Answer 1.6: "numeric"

class(5)

#Question 1.7: What is the class for 'FIVE'?
#Answer 1.7: "character"

class('FIVE')

#Question 1.8: What is the class for FALSE?
#Answer 1.8: "logical"

class(FALSE)

#Question 1.9: What class will the following statement return? Please read question carefully.
#Answer 1.9: "logical"

class(45 == 56)

#Question 1.10: What class will the following statement return? Please read question carefully.
#Answer 1.10: "numeric"

as.numeric(F)

#Question 1.11: What data structure is list('dog' , 5, 'sloth') ?
#Answer 1.11: "list"

#List of 3
#$ : chr "dog"
#$ : num 5
#$ : chr "sloth"

str(list('dog', 5, 'sloth'))

#Question 1.12: What will the following code yield?
#Answer 1.12: FALSE TRUE TRUE TRUE

c(10,20,30,40) > 15

#Question 1.13: What will the following code yield?
#Answer 1.13: FALSE FALSE FALSE FALSE

c(10,20,30,40) > c(15,25,35,45)

#Question 1.14: What will the following code yield?
#Answer 1.14: FALSE FALSE TRUE TRUE

#Note: This question illustrates recycling behavior in R

c(10,20,30,40) > c(15,25)

#Question 1.15:
#Consider the following code which describes smartwatch sales at an electronics website for a
week.
#The watch was discounted on the weekend. Run the code below to create the objects.
#What is the total number of smartwatches sold during the week?
#Hint: Use sum()
#Answer 1.15: 235

day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')

number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
price_per_smartwatch = c(200,200,200,200,200,150,180)

df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)
sum(number_of_smartwatches_sold)

#Question 1.16:
#Consider the following code which describes smartwatch sales at an electronics website for a
week.
#The watch was discounted on the weekend. Run the code below to create the objects.
#Which of the following will extract price on Sunday?
#Select all answers that apply.
#Answer 1.16: A, B, C

day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')

number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
price_per_smartwatch = c(200,200,200,200,200,150,180)

df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

#A
df[df$day_of_week=='Sunday','price_per_smartwatch']

#B
df[df$day_of_week=='Sunday',3]

#C
df$price_per_smartwatch[df$day_of_week=='Sunday']

#D
df$price_per_smartwatch['Sunday']

#Question 1.17:
#Consider the following code which describes smartwatch sales at an electronics website for a
week.
#The watch was discounted on the weekend.
#Run the code below to create the objects.
#What is the total sales revenue from smartphones sold in this week?
#Total sales revenue is the product of unit sales and price for all days of the week.
#Hint: multiply vectors and do sum()
#Answer 1.17: 41,800

day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')

number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
price_per_smartwatch = c(200,200,200,200,200,150,180)

df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)
sum(price_per_smartwatch*number_of_smartwatches_sold)

#Question 1.18:
#Consider the following code which describes smartwatch sales at an electronics website for a
week.
#The watch was discounted on the weekend.
#Run the code below to create the objects.
#On how many days were the number of smartwatches sold greater than 25?
#Hint: use a greater than sign to see which unit sales are greater than 25.
#This will result in a vector of logicals (i.e, TRUE/FALSE).
#Then apply sum() to these logicals to coerce them to numeric
#After, the sum() function will add them up to give you the answer.
#Answer 1.18: 3

day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')

number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
price_per_smartwatch = c(200,200,200,200,200,150,180)

df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)
sum(number_of_smartwatches_sold > 25)

#Question 1.19:
#Consider the following code which describes smartwatch sales at an electronics website for a
week.
#The watch was discounted on the weekend.
#Run the code below to create the objects.
#What will the following code return?
#Answer 1.19: 150 180

day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')

number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
price_per_smartwatch = c(200,200,200,200,200,150,180)

df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)
price_per_smartwatch[c(6,7)]

#Question 1.20
#Consider the following code which describes smartwatch sales at an electronics website for a
week.
#The watch was discounted on the weekend.
#Run the code below to create the objects.
#Which of the following will return the days when sales were greater than average?
#Select all answers that apply.
#Answer 1.20: A, B, C

day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')

number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
price_per_smartwatch = c(200,200,200,200,200,150,180)

df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

#A
df$day_of_week[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold)]

#B
df[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold),"day_of_week"]

#C
df[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold),1]

#D
df[df$number_of_smartwatches_sold>min(df$number_of_smartwatches_sold),1]
