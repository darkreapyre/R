# Readme  

This repository contains the the code and background information/references for creating a next word prediction, __Shiny__ application in `R`.  

The primary objective is to test various forms of a __Neaural Network__ to accomplish this. As a last resort the `n-grams` package will be used.

## Methadology  

### Option 1: Neural Network  

A Neural Network will leverage the `R.matlab` package (and potentially the `darch` package) to work on the Octave/MATLAB code from my Neural Network programming assignment from the [__Neural Networks for Machine Learning Coursera__](https://www.coursera.org/course/neuralnets) course. Should this method fail, then the second task will be to manually port the code by rewriting the various functions in `R`.  

Depending the success and optimization results of these tasks, an alternative option is involve reconstructing a [__Simple Recurrent Network__](https://web.stanford.edu/group/pdplab/pdphandbook/handbookch8.html) to capture the structure of the word sequences and hence create a model to predict the next word. 

### Option 2: P-chain  

As an alternative methodology to the above __Neaural Network__ testing, a unified framework called [__P-chain__](http://rstb.royalsocietypublishing.org/content/369/1634/20120394) will be explored. __P-chain__  is an experimental and connectionist modelling work that shows how learning can play a role in explaining language production phenomena like sentence structure. 

### Option 3: N-grams  

Depending the success and optimization results of both options one and two, a last resort methodology of testing the standard word prediction packages from __CRAN__ is tested.