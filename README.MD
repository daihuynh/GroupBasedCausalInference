# Group-based causal inference

Introduction about this paper

## Cite us

## EXPERIMENT
This is the specification of the computer which built run the experiment

1. CPU: AMD Ryzen 5 2600 - 6 cores
2. RAM: 32GB ram - 6 virtual threads


### Prerequisites

1. RStudio - version 4.x preferable
2. Rtools - compulsory
3. You might cannot install miRLAB because of Roleswitch package if using Windows. Please download version 1.25.0 zip file and install it from RStudio if you are using RStudio 4.x

#### Experiments

There are three main experiments: 

1. 
2.
3.

Please run experiments in order.

Note that, Module Network always return the same result but in different orders. For example, we want to group data into 5 groups and get A, B, C, D, and E as results. However, if we re-run the grouping algorithm again, it will return those 5 groups but with this order: E, B, D, A, and C. Therefore, please only run grouping once and store results into files so as to re-use them if re-training is needed.

#### 1. Using top 30 miRNAs to build a group DAG

#### 2. Using EMT markers to build a group DAG

#### 3. Lung cancer

### Time for building DAGs

1. The variable dag: 15 minutes. It used a single thread.
2. The group dag: around 3.5 hours. It used 8 threads, and 10GBs RAM.

