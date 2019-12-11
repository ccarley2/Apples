# Apples

BCB 546x Final Project paper reproduction 
### An Apple a Day: Which Bacteria Do We Eat With Organic and Conventional Apples?
##### https://doi.org/10.3389/fmicb.2019.01629

Team: Abby Schaefer, Clayton Carley, Grant Nickles, Jazelli Muetherthies, and Matt Kohane 

The goal of this project is to reproduce the analysis in the paper though using the provided 16S data which is available at https://www.ebi.ac.uk/ena/browser/view/PRJEB32455. 

Using this Repository:

1. Familiarize yourself with the original paper.
2. Install R studio.
3. Use read_phyloseq.R to read the phyloseq object containing the data (seeds.RDS or apples.RDS) into R.
4. Use Build_Pyloseq_Obj_Pipeline.rmd to input the raw unzipped FASTQ files from the ENA website and convert them to phyloseq objects with or without phylogenic trees. 
5. Use Clean_out_low_abundance_taxa.rmd to threshold taxa which have above .01% read counts and convert the phyloseq objects to dataframes.
6. Use each 'figure' folder to observe the pipelines build for recreating each figure and thier outputs. 

Information about Taxonomy Assignment:

1. Download zipped fastq files from the ENA.
2. Download the Silva taxonomic training data ("silva_nr_v132_train_set.fa.gz")for dada2: https://zenodo.org/record/1172783#.Xefhh-hKi71
3. Follow the instructions in alignment_dada2.Rmd to create a phyloseq object for downstream analysis.
