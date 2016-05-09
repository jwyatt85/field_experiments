### Application for Political Field Experiments 

##### Functions:
* Calculates the ITT/CACE for a randomized controlled experiment
* Randomizer: 
    + Assign individuals (with unique ID) into treatment/control (or placebo) 
    + Blocked Randomization and SRA options available (with balance checks)
    + Runs logistic regression to see if covariates are predictive of condition placement
    + Allows a user to download the randomized list
    + Allows a user to set a proportion to treatment/control (Option only works for SRA currently)
* Calculates the power - minimial detectable effect - of an experiment (pre-experimental analysis)
* Adding Statistical modeling capabilities: Mixed effects, logistic regression, OLS, and Regression trees.  Decided to skip NNets and RandomF
* Added chropleth mapping abilities for state, county, and zip codes (removed alaska and hawaii - nobody does anything with those states anyways xD):


![alt tag](https://github.com/jwyatt85/field_experiments/blob/master/data/mapping_pic.png)




