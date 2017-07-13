# Data Mining Homework 1 solution in iPython

# Import libraries
import numpy as np
import pandas as pd


# Read the data set
abalone = pd.read_csv("abalone.csv")

# The column names of the dataset
abalone.columns

# How many observations (i.e. rows) are in this data frame?
len(abalone)

# first 4 lines from the dataset
abalone.head(4)

# What are the values of feature rings of the printed observations?
abalone.head(4).Rings

# Extract the last 3 rows of the data frame
tl = abalone.tail(3)

# the weight of these abalones
tl.Weight

# the value of diameter in the row 755
abalone.Diameter[754]

# missing values in the height column?
abalone.Height.isnull().sum()

# What is the mean of the height column? Exclude missing values from this calculation
abalone.Height.mean()

# Extract the subset of rows of the data frame where gender is M and weight values are below 0.75
subset1 = abalone[(abalone["Gender"] == "M") & (abalone["Weight"] < 0.75)]

# the mean of diameter in this subset
subset1.Diameter.mean()

# the most frequent rings value
abalone.Rings.mode()

# the minimum of length when rings is equal to 18
abalone[(abalone["Rings"] == 18)].Height.min()

