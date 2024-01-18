# Imort of the libraries we need

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.interpolate import interp1d

# Create the data frame

data=pd.read_csv(r"yourfolder\Data analysis with python final assigment AUEB data.csv",header=0)

df=pd.DataFrame(data)

# We set the program to show all rows/columns

pd.set_option('display.max_columns',None)
pd.set_option('display.max_rows',None)

# Check that the dataframe is passed correctly 

df.head()

# If we are working on spyder, we can check the dataframe at any time
# double-clicking on it in Variable Explorer 

# 1. In the maximum and minimum temperature columns there are some days that we did not have 
# data (NaN). Use cubic interpolation of the three before and three 
# after prices to approximate these prices. Also, the gaps that exist in the 
# MONTH column should be replaced with "DEC" characters.

# Solution :

# We use the command df.isna().sum() to see how many nan values we have per column

print(df.isna().sum())

# We notice that there are no NaN cells 
# We check the unique values of these columns to see how the problem cells are represented 

for column in df.loc[:,['MONTH','HIGH','LOW']]:
    unique_values = df[column].unique()
    print(f"Mοναδικές τιμές στην στήλη {column}: {unique_values}")
    
# We notice that nan cells are represented by either ' ' (space) or ' ' (double space). 
# We change the specific values to NaN

df.replace([' ', '  '], pd.NA, inplace=True)

# We recheck the columns for NaN values

print(df.isna().sum())

# We replace the NaN cells in the MONTH column with DEC

df['MONTH'].fillna('DEC', inplace=True)

# We check and see that the values have been passed to the column MONTH

df.iloc[:,0]

# To fill the blank values in the columns HIGH and LOW we create 
# a function that applies interp1d to their three previous and three
# next values

def cubic_interpolation(column):
    missing_values = column.index[column.isna()]
    for index in missing_values:
        if index >= 3 and index <= len(column) - 4:
            values_before = column[index - 3 : index]
            values_after = column[index + 1 : index + 4]
            
            # We create X and Y for the interpolation
            x = np.concatenate([np.arange(index - 3, index), np.arange(index + 1, index + 4)])
            y = np.concatenate([values_before.dropna(), values_after.dropna()])
            
            
            # We apply interpolation
            ci = interp1d(x, y, kind='cubic')
            y_ci =  np.round(ci(index),1)
            
            # We return the value of the interpolation to the blank value of the column
            column[index] = y_ci
    return column

# We apply the equation to the HIGH and LOW columns

cubic_interpolation(df['HIGH'])
cubic_interpolation(df['LOW'])

# We check and see that the values have been passed to the HIGH and LOW columns

print(df.isna().sum())
df.loc[:,['HIGH','LOW']]

# 2. Add a row at the end of the table to calculate absolute maximum temperature in the column 
#  with maximum temperatures, absolute minimum temperature in the column with minimum 
# , and similar for the column with the strongest wind intensity (wind high) . 
# In the case of the column of average temperatures, the row will have their average value.  
# The row will be summing centimeters of rainfall, HDD Heating Degree Days and CDD Cooling 
# Degree Days in the corresponding columns. The rest will remain empty.

# Solution :
    
# We check the data types for the columns of the data frame  

df.dtypes

# We notice that in columns HIGH and LOW we have object type instead of float
# We convert the two columns to float data type

df['HIGH'] = df['HIGH'].astype(float)
df['LOW'] = df['LOW'].astype(float)
    
# We make a new line with the values requested:
    
df.loc['Επισκόπηση'] = [np.nan, np.nan, df['TEMP'].mean(),
df['HIGH'].max(), np.nan, df['LOW'].min(), np.nan, df['HDD'].sum(), df['CDD'].sum(), df['RAIN'].sum(), np.nan,
df['WINDHIGH'].max(), np.nan, np.nan]

# 3. Find the median and standard deviation of mean temperatures.

# Solution :
    
# Calculation of the two sizes :
    
median = df["TEMP"][:-1].median()
std = df["TEMP"][:-1].std()
    
print(f'The median of mean temperatures is {median}')
print(f'The standard deviation of mean temperatures is {std}')

# 4. Show how many days the wind blew from each of the possible 
# addresses. Make a pie chart to illustrate the previous 
# distribution of days in wind direction blowing.
    
# Solution :
    
# We count days with a specific air direction  

df.groupby('DIR').size()

# We create the specific pie chart

value_counts = df.groupby('DIR').size()

plt.figure(figsize=(12, 12))
plt.pie(value_counts, labels=value_counts.index, autopct='%1.1f%%')
plt.title('Distribution of days in accordance to wind direction')
plt.show()

# 5. Find the time when the most maximum temperatures have occurred 
# and the time with the most minimum temperatures in the year.

# Solution :

# Calculation of the two sizes :
  
ora1 = df.groupby('TIME').size().idxmax()
ora2 = df.groupby('TIME.1').size().idxmax()

print(f'The time with the most high temperatures is {ora1}')
print(f'The time with the most low temperatures is {ora2}')

# (Instead of df.groupby('TIME').size() we could also use 
# df['TIME'].value_counts(). The same applies in question 4)

# 6. Find the day of the year that had the greatest temperature variation.

# Solution :
    
# We create a new data frame with the columns from df: MONTH, DAY and VARIANCE
# where VARIANCE is the difference between maximum and minimum temperature. We remove
# the last row with the overview

df2 = pd.DataFrame({
    'MONTH': df.iloc[:-1, 0],
    'DAY': df.iloc[:-1, 1],
    'VARIANCE': df.iloc[:-1, 3] - df.iloc[:-1, 5]
})

# We create a third data frame which will contain aonly one line with the values 
# of columns for maximum temperature variation   

df3 = df2[df2['VARIANCE'] == df2['VARIANCE'].max()]
print('Below is the day with the greatest temperature variance:')
print(df3)

# 7. From which address did it blow most days of the year?

# Solution :
    
# We find the value of the DIR column that is repeated most of the times
    

df['DIR'].describe()

# or

df.groupby('DIR').size().idxmax()

# or

df['DIR'].value_counts().idxmax()

# 8. Find the wind direction that gave the greatest wind intensity.

# Solution :
    
# We create an object a with the maximum wind speeds, grouped by direction
# wind
    

a = df['W_SPEED'].groupby(df['DIR']).max()

# From the specific item, we display the index for the highest value

print(a.idxmax())

# 9. What was the average temperature for each wind direction? Using 
# the generated table and appropriate commands on it,find the addresses 
# of winds that gave the highest and lowest average temperature.

# Solution :

# We create a table with the average temperatures per wind direction
    
b = df['TEMP'].groupby(df['DIR']).mean()
print(b)

# From the specified object, we display the index for the largest and smallest
# value

print(b.idxmax())
print(b.idxmin())

# 10. Create, in the form of a bar graph, the distribution of the amount of rainfall per month.

# Solution :
    
# We create a table with the average temperatures per month

c = df['RAIN'].groupby(df['MONTH']).mean()

# We create a list of months in the year from first to last

new_order = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JOU', 'JUL', 'AUG', 'SEP', 'OCT', 
             'NOV', 'DEC']

# We rearrange the rows of table c based on the order above

c = c.reindex(new_order)

# We create the bar chart

c.plot(kind='bar',x='MONTH',y='RAIN')

# 11. Create the linear regression for the average temperatures of December 
# of 2017 and predict through this, the temperature on 25/12 2018.

# Solution :
    
# We create an X table with one line containing the days in December.

X = df.loc[df['MONTH']== 'DEC', ['DAY']]

# We create an array Y with one row containing the average temperatures
# of December per day.

Y = df.loc[df['MONTH']== 'DEC', ['TEMP']]

# We create an array I, with 31 one values and finally an array A with the values of X 
# as a first column and the values of I as second 

I = np.ones(31,)
A=np.c_[X, I]

# We create the formula for the least squares line

np.linalg.lstsq(A,Y,rcond='warn')

# With the values we get for a and b, we make a prediction for Y when X is 25

xp=25
yp = -0.18048387*xp+ 14.08129032
print(yp)

#To result is 9.569193570000001

# 12. Create 4 graphic sub-representations that each will relate to a season: 
# Winter(Dec17-Feb17)-Spring (Mar-May), Summer(Jun-Aug), Autumn (Sep-Nov). 
# Each will show the average temperature in green, the maximum in red 
# and the minimum in blue.

# Solution :
    
# We create a subtable from df in which the month is December, January 
# or Frebruary

filter1 = ['DEC', 'JAN','FEB']
condition1 = df['MONTH'].isin(filter1)
fdf1 = df[condition1]

# We do the same for the rest of the seasons

filter2 = ['MAR', 'APR','MAY']
condition2 = df['MONTH'].isin(filter2)
fdf2 = df[condition2]

filter3 = ['JOU', 'JUL','AUG']
condition3 = df['MONTH'].isin(filter3)
fdf3 = df[condition3]

filter4 = ['SEP', 'OCT','NOV']
condition4 = df['MONTH'].isin(filter4)
fdf4 = df[condition4]

# To visualize temperatures by season, we will make 4 column charts. 
# We create categories, color for each category and prices for each season

Categories = [ 'Maximum', 'Medium', 'Low']
colors = ['red', 'green', 'blue']
T1 = [fdf1['HIGH'].mean(), fdf1['TEMP'].mean(),fdf1['LOW'].mean()]
T2 = [fdf2['HIGH'].mean(), fdf2['TEMP'].mean(),fdf2['LOW'].mean()]
T3 = [fdf3['HIGH'].mean(), fdf3['TEMP'].mean(),fdf3['LOW'].mean()]
T4 = [fdf4['HIGH'].mean(), fdf4['TEMP'].mean(),fdf4['LOW'].mean()]

#  We create the chart

plt.subplot(221)

plt.bar(Categories, T1, color= colors)
plt.ylabel('Temperatures')
plt.title('Winter')

plt.subplot(222)

plt.bar(Categories, T2, color= colors)
plt.ylabel('Temperatures')
plt.title('Spring')

plt.subplot(223)

plt.bar(Categories, T3, color= colors)
plt.ylabel('Temperatures')
plt.title('Summer')

plt.subplot(224)

plt.bar(Categories, T4, color= colors)
plt.ylabel('Temperatures')
plt.title('Autumn')

plt.tight_layout()
plt.show()
    
# 13. Construct a function that takes as an argument the sum of the amounts of rainfall. 
# If it is < 400 it will return "Water scarcity", >=400 & <600 "Satisfactory amounts of rain" 
# while over 600 will read "Excessive rainfall"

# Solution :

def rain_condition(RAIN):
    if RAIN < 400:
        return "Λειψυδρία"
    elif 400 <= RAIN < 600:
        return "Ικανοποιητικά ποσά βροχής"
    elif RAIN >= 600:
        return "Υπερβολική βροχόπτωση"
    
RAIN = df['RAIN'].sum()

rain_condition(RAIN)
 
