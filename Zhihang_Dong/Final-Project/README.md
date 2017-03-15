# Two Functions of "How likely someone dies before another?" Presentation
## This is not esoteric at all...

For full information and mathematical proof, please see my presentation slides in the folder.

## 1.1 How likely your mother is still alive when you are at age x?

**This function currently works for female only, sorry...**

For a girl of age x, and her mother born her at age y, and the earliest fertility age of her mother 
is a, the latest is the b. The population growth rate is at r, we then have the function:
```r
mdeaths<-function(yob,mage,dage, rate, alpha,beta)
```

Now, suppose your mother is born at the year 1869 (so historic) and your mother had you born at hear age *y=19*
and you are now age *x=8*. The population growth is at *r = 0.02* (can have a separate function to calculate this).And 
fertility information are as specified, you may write something like:

```r
mdeaths(yob=1869,mage=19,dage=18,r=0.02, alpha=16, beta= 32)
```
This will return something like
```r
The likelihood of your mother is still alive when you aged 18 is 0.4752119 with absolute error < 5.3e-15
```

*Long Live Mama!!*

## 1.2 Catch Bad Values
If you don't know the cohort lifetable has year of birth at max 1923 (that's why our project is so unuseful for modern human beings)
, and you try year of birth at **1989**, then... something will happen
```r
mdeaths(yob=1969,mage=19,dage=18,r=0.002, alpha=16, beta= 32)
```
This will return something like
```r
"The years you selected is not available in this country... Perhaps yuz
        gonna wait for hundreds of years until this lifetable is published, or
        just give up on your research career haha..."
```
Of course, this is not esoteric at all but if you think someone can be pregnant before her age of 10, then the system
will be very mean-spirited to return something like
```r
"Don't make fun of me... To say you are a demographer, please have some
        common sense to realize that modern human beings cannot be pregnant and 
        successfully get a child born before age 10... Is it too hard?"
```
I designed the most mean-spirited function I have ever written in my life.

## 2.1 How likely your wife/husband will die before you?

The function requires you to specify the year you wanna test in (*year* variable, the husband and wife variable is just
for you to enter the age, *t* is how many years elapsed as specified by you, and *choice* is on whoever side you want to
test for. "h" is husband, "w" is wife. 

**but I don't know how this works for same-sex couples... This is not discrimination on sexual orientation...**

```r
partner<-function(year,husband,wife,t,choice)
```

## 2.2 Examples

```r
partner(1909,59,61,6,"w")
```

I am not going to explain how it works, the results explained in the way too wordy, like:

```r
partner(1909,59,61,6,"w")
```

It will return:
```r
"In 1909 ,if your wife is 61 years old and you are 59 years old. After  6 years, the probability of your  
 wife die before you is approximately 0.02372049" 
```

## 2.3 Catch the errors
Great News! This function does not have initial mean-spirited **warnings**.

Now, if you type in something not make sense, like you refuse to specify husband or wife, but to type in Aww

```r
partner(1940,58,42,4,"aww")
```

It will return:
```r
"Your selection does not match our expectations."
```
多么的高冷 :smile:

# 3.1 ...
# That's it!
