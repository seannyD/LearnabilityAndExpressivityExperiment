# Learnability And Expressivity Experiment

# Overview


There are 3 phases to the experiment:
-  Phase 1.  Identify a suitable starting language, in the middle of the ExL space
-  Phase 2.  Subject such a language to 3 different types of engagement: L-only, E-only, L+E
-  Phase 3.  Measure and plot the learnability of the languages that come out of these three different types of engagement by running another generation of learners on each.


## Stage 1

-  Generate a large number of possible starting languages.
-	Identify four candidate languages. These will all have the same scores for E and S.
-	Run the L condition of the experiment on 4 Ps for each of the 4 languages.
-	Pick whichever L is closest to the middle of the ExL space, considering proximity to mean and variance in learnability. Do this with harmonic mean (we expect at least one language to be close).


##  Stage 2

-  Ps randomly assigned to one of three conditions: L-only, E-only, L+E. Each one run 4 times i.e. 4 Ps in L-only, 8 Ps in E-only, 8 Ps in L+E.
-  In L-only there are four rounds of training, followed by an Output Phase (see below).
-  In E-only there are four rounds of communication, followed by an Output Phase.
-  In L+E there are two rounds of training, then two rounds of communication, all followed by an Output Phase.
-  The Output Phase consists of showing an image of one member of the stimuli set, with the question “What word describes this?”. For the L+E condition, the Ps will also see the dictionary they had used through the condition.
-  This gives us 20 output languages. Question: Do we choose one of the two output languages for each pair in the E-only and L+E conditions? Or do we test both output languages for L? The latter will cost more Ps, but seems sensible. Because more data.



##  Stage 3

-	Measure the *experimental learnability* of each of the output languages from stage 2 by running 4 additional L-only condition on the output languages.


# File Structure

## Data

`Filenames.csv`: How the filenames link up to conditions and seed languages.

`LEX_ExperimentsSoFar.pdf`: Visualisation of how the experiments relate to each other.

`ExperimentData.csv`: Raw output from each experiment.

`ExperimentData_with_mean_Learnability.csv`: One row for each participant/pair's language, with the mean experimental learnability.

`ExperimentData_with_all_Learnability.csv`: One row for each experimental learnability measure (and one row for all other experiments).

## Processing

`checkExperimentParents.R`:  Match up the experiment files to their parents and children, sort into phases, produce `data/Filenames.csv`.


`getData.R`:  Open all relevant files and extract data in long form.  Produces `ExperimentData.csv`.

`getMeasures.R`: Get the learnability measure derived from actual participant responses (phase 3). Produces `ExperimentData_with_mean_Learnability.csv` and `ExperimentData_with_all_Learnability.csv`

## Analysis

`plotData.R`: Plot the mean learnability and expressivity in each condition.

## ChooseStartingLanguages

These files deal with phase 1: Generating lots of possible starting languages, picking 4 to assess experimentally and choosing the final language that would be used in phase 2.  This folder replicates some of the data in the main Backups.

It also has a file `ChooseStartingLanguages/analysis/utils.R`, which has some methods for calculating expressivity and systematicity.

## Results

Various outputs from the stats, and graphs.

## Methods

Some notes on the methods.

## ExperimentAdmin

Information sheets etc.

## Backups

Several backups of the experimental results over the course of the experiment

