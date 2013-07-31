Raptor
======

An Online Retail Recommendation System developed in Haskell.

Raptor learns from customer behaviors when they do online shopping: View an item, Add an item to Card, and Purchase an item.

Currently there are 3 different algorithms - specified in raptor.hs:
- Original Jaccard Distance, based heavily on information about items that has been purchased / added to cart together.
- Bayes Jaccard Distance, a modified version of Jaccard Distance, but still very similar.
- VTD, a slightly different approach. The idea of this approach is to look closer as customer Views data. 
Basically tries to answer the question: if a customer view this item, what are the most likely items that the customer is going to buy.

Base on the nature of each algorithms, Original and Bayes Jaccard are more applicable for a Cart Based Recommendation System, while VTD is more  applicable for the Product Based Recommendation System

The scoring function for these approach is powered by Wilson Score (for simplicity, Wilson score of 95% confidence):
http://www.evanmiller.org/how-not-to-sort-by-average-rating.html

