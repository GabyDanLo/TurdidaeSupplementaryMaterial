# Turdidae Supplementary Material: Tree creation

Inside, you will find the various steps of the creation of our final phylogenetic tree for the Turdidae family: 

## 1. MrBayes Analysis Results

These are all the output files of the MrBayes analysis, processed with the help of the CIPRES analysis servers.

## 2. TreeAnnotator

We used the TreeAnnotator program to create the maximum clade credibility tree to summarise our MrBayes results. The file TurdidaeTreesNoBurnIn.t is the input, and TreeAnnotator generated TurdidaeMCCTree.phy as output, which was used in the next step.

## 3. TreePL

We used TreePL to date our resulting MCC tree. In this folder you'll find the necessary config file and the output of our run of the software.

## 4. Final Tree (PhyGeo)

Here, you can find the resulting dated tree used for the PhyGeo reconstruction as well as a graphical representation of the tree in SVG format.