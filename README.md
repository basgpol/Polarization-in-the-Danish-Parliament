# Polarization in the Danish Parliament
Project analyzing parliamentary polarization in the Danish Parliament

## Short Description

This project analyze voting behaviour from the Danish Parliament in order to test to what extent we see  parliamentary polarization in Denmark. The analyze revealed several interesting findings. First, the analysis identified that the Danish parliament has reported the voting data wrong for all votes before 2014. One impact of the paper is thus that the parliament will change the voting data available in their API. Second, I find no overall increase or decrease in graph density and modularity between 2004 and 2018. In other words, we see no overall movement towards parliamentary polarization or convergence in Denmark. Third, I find a small movement toward parliamentary convergence in the recession years 2007-2012. This indicates that members of the Danish parliament united slightly during the economic crisis and hence resisted the short-term interest of vote-seeking. All in all, this project finds no empirical evidence for a significant movement towards either the consensus model or the majoritarian model of democracy.

## Dependencies

Software that my code depends on:

1. R, version 3.2.
2. Python, version 3.6, Anaconda distribution.

## Files

List of files in the repo:

#### Data

- voting_data_corrected.csv: Final dataset after correction of mistakes from the Danish Parliament.
- voting_polarization.csv: Dataset with polarizationmeasures on data from the Danish Parliament.
- voting_votetypes.csv: Dataset with the number of yes-votes, no-votes, neither-votes for each parliamentary vote.

#### Code

1. DATA IMPORT_Python.ipynb: Script scraping voting data from the API of the Danish Parliament.
2. Data import.R: Script scraping meta data from the API of the Danish Parliament.
3. Analysis.R: Script calculating polarization measures for each vote. 
4. Visualization.R: Script producing all visualization in the project.

#### Results
1. Conflict_or_cooperation.pdf: Final paper in PDF-format
2. Conflict_or_cooperation.Rmd: Final paper as Rmarkdown-file

## More Information
This project was created as an exam project for a graduate course (Social Network Analysis) at University of Copenhagen. The course repo is available here: https://github.com/golovchenko/sna2018.
