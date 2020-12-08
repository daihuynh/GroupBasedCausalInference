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
3. You might cannot install miRLAB because of Roleswitch package if using Windows. Please download version 1.25.0 zip file and install it from RStudio if you are using RStudio 4.x.
4. Please install Git Large Storage (lfs) to get data files by using these commands.

```
cd <project_root>
git lfs install
git lfs pull
```

#### Experiments

There are three main experiments: 

1. breast_cancer_top30miRNAs.Rmd. This Rmarkdown is the experiment using breast cancer data to build a group DAG having 5 groups of top 30 miRNAs from a MAD function, 5 groups of Epithelial genes, and 5 groups of Mesenchymal genes.
2. breast_cancer_EMTmarkers.Rmd. This is really similar to #1, but instead of taking 30 top miRNAs, miRNAs which are markers of Epithelial and Mesenchymal genes selected. There are 3 group DAGs correspoding to Epithelial markers, Mesenchymal markers, and both-two-type markers.
3. lung_cancer.Rmd. This experiment uses Lung cancer dataset to build a group DAG.

Besides experiments, there are 3 miscellaneous fies:

1. visualization.Rmd is written to visualize results from those 3 experiments.
2. miRNA_target_validation.Rmd uses miRNA-target interaction validation databases to confirm the results.
3. tf_mRNA_validation.Rmd uses TF-mRNA binding validation databases to confirm the results.

Please run experiments in order.

Note that, Module Network always return the same result but in different orders. For example, we want to group data into 5 groups and get A, B, C, D, and E as results. However, if we re-run the grouping algorithm again, it will return those 5 groups but with this order: E, B, D, A, and C. Therefore, please only run grouping once and store results into files so as to re-use them if re-training is needed.

#### 1. Using top 30 miRNAs to build a group DAG

#### 2. Using EMT markers to build a group DAG

#### 3. Lung cancer

### Time for building DAGs

1. The variable dag: 15 minutes. It used a single thread.
2. The group dag: around 3.5 hours. It used 8 threads, and 10GBs RAM.

