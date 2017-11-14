---
title: "RGMQL: GenoMetric Query Language for R/Bioconductor"
author: 
- "Simone Pallotta" 
- "Marco Masseroli"
date: "2017-11-14"
bibliography: bibliography.bib
output: BiocStyle::pdf_document
vignette: >
    %\VignetteIndexEntry{Vignette Title}
    %\VignetteEngine{knitr::rmarkdown}
    %\VignetteEncoding{UTF-8}
link-citations: true
---

# Introduction

Recent years have seen a tremendous increase in the volume of data generated 
in the life sciences, especially propelled by the rapid progress of 
Next Generation Sequencing (NGS) technologies. 
This high-throughput technologies can produce billions of short DNA or RNA 
fragments in excess of a few terabytes of data in a single run.
Next-generation sequencing refers to the deep, in-parallel DNA sequencing 
technologies providing massively parallel analysis and extremely 
high-throughput from multiple samples at much reduced cost. 
Improvement of sequencing technologies and data processing pipelines 
is rapidly providing sequencing data, with associated high-level features, 
of many individual genomes in multiple biological and clinical conditions. 
To make effective use of the produced data, the design of big data algorithms 
and their efficient implementation on modern high performance 
computing infrastructures, such as clouds, CPU clusters 
and network infrastructures, is required in order to achieve scalability 
and performance. 
For this purpose the GenoMetric Query Language (GMQL) has been proposed 
as high-level, declarative language to process, query, 
and compare multiple and heterogeneous genomic datasets for biomedical 
knowledge discovery [@Bioinformatics2015]

## Purpose

A very important emerging problem is to make sense of the enormous amount and 
variety of NGS data becoming available, i.e. to discover how different genomic 
regions and their products interact and cooperate with each other. 
To this aim, the integration of several heterogeneous DNA feature data 
is required.
Such big genomic feature data are collected within numerous and 
heterogeneous files, usually distributed within different repositories, 
lacking an attribute-based organization and a systematic description 
of their metadata. 
These heterogeneous data can contain the hidden answer to very important 
biomedical questions.
To inveil them, standard tools already available for knowledge extraction 
are too specialized or present powerful features, but have a rough interface 
not well-suited for scientists/biologists.
GMQL addresses these aspects using cloud-based technologies 
(including Apache Hadoop, mapReduce, and Spark), and focusing on genomic data 
operations written as simple queries with implicit iterations over thousands 
of heterogeneous samples, computed efficiently [@IEEE7484654].
This RGMQL package makes easy to take advantage of GMQL functionalities also 
to scientists and biologists with limited knowledge of query and 
programming languages, but used to the R/Bioconductor environment. 
This package is built over a GMQL scalable data management engine 
written in Scala programming language, released as Scala API [@githubrepo] 
providing a set of functions to combine, manipulate, compare, and extract 
genomic data from different datasources both from local and remote datasets.
These functions allow performing complex GMQL processing and queries without 
knowledge of GMQL syntax, but leveraging on R idiomatic paradigm and logic.


# Genomic Data Model

The Genomic Data Model (GDM) is based on the notions of datasets 
and samples[@modeling2016] 
Datasets are collections of samples, and each sample consists of two parts, 
the region data, which describe portions of the genome, and the metadata, 
which describe sample general properties and how observations are collected.
In contrast to other data models, it clearly divides, and comprehensively 
manages, observations about genomic regions and metadata.
GDM provides a flat attribute based organization, just requiring that 
each dataset is associated with a given data schema, which specifies 
the attributes and their type of region data.
The first attributes of such schema are fixed (chr, start, end, strand); 
they represent the genomic region identifying coordinates.
In addition, metadata have free attribute-value pair format.

## Genomic Region 

Genomic region data describe a broad variety of biomolecular aspects and are 
very valuable for biomolecular investigation.
A genomic region is a portion of a genome, qualified by a quadruple of values 
called region coordinates:
$$< chr, left, right, strand >$$
Regions can have an arbitrary number of associated values, according to 
the processing of DNA, RNA or epigenomic sequencing reads that determined 
the region.

## Metadata

Metadata describe the biological and clinical properties associated with 
each sample.
They are usually collected in a broad variety of data structures and formats 
that constitute barriers to their use and comparison GDM models metadata 
simply as arbitrary semi-structured attribute-value pairs, 
where attributes may have multiple values.

## Genomic Sample

Formally, a sample s is a collection of genomic regions modeled as 
the following triple: $$< id, {< r_i,v_i >}, {m_j} >$$ where:

* id is the sample identifier
* Each region is a pair of coordinates $r_i$ and values $v_i$
* Metadata $m_j$ are attribute-value pairs $< a_j,v_j >$

Note that the sample id attribute provides a many-to-many connection between 
regions and metadata of a sample.
Through the use of a data type system to express region data, and of arbitrary 
attribute-value pairs for metadata, GDM provides interoperability across 
datasets in multiple formats produced by different experimental techniques.

## Dataset

A dataset is a collection of samples uniquely identified, with the same region 
schema and with each sample consisting of two parts:

* region data: describing characteristics and location of genomic portions
* metadata: expressing general properties of the sample

Each dataset is typically produced within the same project by using the same 
or equivalent technology and tools, but with different experimental 
conditions, described by metadata.

Datasets contain large number of information describing regions of a genome, 
with data encoded in human readable format using plain text files.

GMQL datasets are materialized in a standard layout composed of three 
types of files:

1. genomic region tab-delimited text files with extension .gdm, or .gtf 
if in standard GTF format
2. metadata attribute-value tab-delimited text files with the same fullname 
(name and extension) of the correspondent region file and extension .meta
3. schema XML file containing region attribute names and types

All these files reside in unique folder called files.

<!-- ![GMQL dataset folder](dataset_gmql.png) -->

In RGMQL package dataset files are considered read-only.
Once read, genomic information is represented in abstract structure inside 
the package, mapped to a R GRanges data structure at occurency.


# GenoMetric Query Language

The GenoMetric Query Language name stems from the language ability to deal 
with genomic distances, which are measured as number of nucleotide bases 
between genomic regions (aligned to the same reference genome) and computed 
using arithmetic operations between region coordinates.
GMQL is a high-level, declarative language that allows expressing queries 
easily over genomic regions and their metadata, in a way similar to what can 
be done with the Structured Query Language (SQL) over a relational database.
GMQL approach exhibits two main differences with respect to other tools 
based on Hadoop, mapReduce framework, and Spark engine technologies 
to address similar biomedical problems:\newline

* GMQL:

    1. reads from processed datasets
    2. supports metadata management
    
* Others:

    1. read generally from raw or alligned data from NGS machines
    2. provide no support for metadata management

GMQL is the appropriate tool for querying numerous processed genomic datasets 
and very many samples that are becoming available.
Note however that GMQL performs worse than some other available systems on a 
small number of small-scale datasets, but these other systems are not 
cloud-based; hence, they are not adequate for efficient big data processing 
and, in some cases, they are inherently limited in their 
data management capacity, as they only work as RAM memory resident processes.

## Query structure

A GMQL operation is expressed as a sequence of GMQL operations with the 
following structure:
$$< variable > = operator(< parameters >) < variable >;$$
where each $< variable >$ stands for a GDM dataset

This RGMQL package brings GMQL functionalities into R environemnt, 
allowing users to build directly a GMQL query without knowing the GMQL syntax.
In RGMQL every GMQL operations is translated into a R function 
and expressed as:
$$ variable = operator(variable, parameters)$$

It is very similar to the GMQL syntax for operation expression although 
expressed with the R idiomatic paradigm and logic, with parameters totaly 
builded using R native data structures such as lists, matrices, 
vectors or R logic conditions.


# Processing Environments

In this section, we show how GMQL processing is built in R, which operations 
are available in RGMQL, and the difference beetween local 
and remote dataset processing.

## Local Processing

RGMQL local processing consumes computational power directly from local 
CPUs/system while managing datasets (both GMQL or generic text plain datasets).

### Initialization

Load and attach the GMQL package in a R session using library function:

```r
library('RGMQL')
```
Before starting using any GMQL operation we need to initialise the GMQL 
context with the following code:

```r
init_gmql()
```
The function *init_gmql()* initializes the context of scalable data management 
engine laid upon Spark and Hadoop.
Details on this and all other functions are provided in the R documentation 
for this package (e.g., help(RGMQL)).

### Read Dataset

After initialization we need to read datasets.
We already defined above the formal definition of dataset and the power of 
GMQL to deal with data in a variety of standard tab-delimited text formats.
In the following, we show how to get data from different sources.\newline
We distinguish two different cases:

1. Local dataset:\newline
A local dataset is a folder with sample files (region files and correspondent 
metadata files) on the user computer.
As data are already in the user computer, we simply execute:


```r
gmql_dataset_path <- system.file("example", "EXON", package = "RGMQL")
data_out = read_dataset(gmql_dataset_path)
```
In this case we are reading a dataset named EXON specified by path.
It doens't matter what kind of format the data are, *read_dataset()* read many 
standard tab-delimited text formats without specified any paramter at input.

2. GRangesList:\newline
For better integration in the R environment and with other packages, 
we provide a *read()* function to read directly from R memory/environment 
using GRangesList as input.













































