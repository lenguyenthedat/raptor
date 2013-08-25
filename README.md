Raptor
======

An Online Retail Recommendation System developed in Haskell.

Raptor learns from customer behaviors when they do online shopping: 
- View an item.
- Add an item to Card. 
- Purchase an item.

Currently there are 3 different algorithms - specified in raptor.hs:
- Original Jaccard Distance, based heavily on information about items that has been purchased / added to cart together.
- Bayes Jaccard Distance, a modified version of Jaccard Distance, but still very similar.
- VTD, a slightly different approach. The idea of this approach is to look closer as customer Views data. 
Basically tries to answer the question: if a customer view this item, what are the most likely items that the customer is going to buy.

Base on the nature of each algorithms, Original and Bayes Jaccard are more applicable for a Cart Based Recommendation System, while VTD is more  applicable for the Product Based Recommendation System.

The scoring function for these approach is powered by Wilson Score (for simplicity, Wilson score of 95% confidence):
http://www.evanmiller.org/how-not-to-sort-by-average-rating.html


Sample Input
---------

    $ cat Data/VTD_view_sg.csv 
    sku0	cust1 cust2 cust3 cust4
    sku1	cust1 cust2 cust3 cust4 cust5
    sku2	cust2 cust4 cust5
    sku3	cust1 cust2
    sku4	cust4 cust5 cust1 cust3
    sku5	cust1 cust2 cust3
    sku6	cust5 cust3
    sku7	cust4 cust5
    sku8	cust1 cust4
    sku9	cust2 cust4

    $ cat Data/VTD_cart_sg.csv 
    sku0	cust1 cust2
    sku1	cust1 cust2 cust3
    sku2	cust2 cust4
    sku3	cust1 cust2
    sku4	cust4 cust5 cust1 cust3
    sku5	cust1
    sku6	cust5 cust3
    sku7	cust4
    sku8	cust1 cust4
    sku9	cust2

    $ cat Data/VTD_purchased_sg.csv 
    sku0	cust1 cust2
    sku1	cust1 cust2 cust3
    sku2	cust2
    sku3	cust1 cust2
    sku4	cust4 cust5 cust3
    sku5	cust1
    sku6	cust5 cust3
    sku7	cust4
    sku8	cust1 cust4
    sku9	cust2

    $ cat Data/sku_male_sg.csv 
    sku5
    sku6
    sku7
    sku8
    sku9

    $ cat Data/sku_female_sg.csv 
    sku0
    sku1
    sku2
    sku3
    sku4

    $ cat Data/instock_skus_sg.csv 
    sku0
    sku1
    sku2
    sku3
    sku4
    sku5
    sku6
    sku7
    sku8

    $ cat Data/valid_skus_sg.csv 
    sku1
    sku2
    sku3
    sku4
    sku5
    sku6
    sku7
    sku8

Sample Output
-----------
    
    $ ghc raptor.hs
    [1 of 1] Compiling Main             ( raptor.hs, raptor.o )
    Linking raptor ...
    
    $ ghc Joining-Raptors.hs 
    [1 of 1] Compiling Main             ( Joining-Raptors.hs, Joining-Raptors.o )
    Linking Joining-Raptors ...

original
--------

    $ ./raptor sg 3 original
    $ cat Result/original/Raptor_sg.csv 
    sg	sku5	7.38-sku8
    sg	sku6	
    sg	sku7	7.38-sku8
    sg	sku8	7.38-sku5	7.38-sku7
    sg	sku9	
    sg	sku0	18.00-sku3	14.11-sku1	7.06-sku2
    sg	sku1	14.11-sku3	5.28-sku2	4.65-sku4
    sg	sku2	7.06-sku3	5.28-sku1	0.60-sku4
    sg	sku3	14.11-sku1	7.06-sku2	0.60-sku4
    sg	sku4	4.65-sku1	0.60-sku2	0.60-sku3

bayes
-----

    $ ./raptor sg 3 bayes
    $ cat Result/bayes/Raptor_sg.csv 
    sg	sku5	6.15-sku8
    sg	sku6	
    sg	sku7	6.15-sku8
    sg	sku8	6.15-sku5	6.15-sku7
    sg	sku9	
    sg	sku0	9.68-sku3	8.22-sku1	6.15-sku2
    sg	sku1	9.68-sku3	6.15-sku2	3.01-sku4
    sg	sku2	6.15-sku1	6.15-sku3
    sg	sku3	9.68-sku1	6.15-sku2
    sg	sku4	3.01-sku1

vtd
---

    $ ./raptor sg 3 vtd
    $ cat Result/vtd/Raptor_sg.csv 
    sg	sku5	9.45-sku6	9.45-sku8
    sg	sku6	
    sg	sku7	9.45-sku6	9.45-sku8
    sg	sku8	9.45-sku5	9.45-sku7
    sg	sku9	9.45-sku7	9.45-sku8
    sg	sku0	15.82-sku1	15.00-sku3	11.76-sku4
    sg	sku1	15.82-sku4	15.00-sku3	4.56-sku2
    sg	sku2	15.00-sku4	9.45-sku3	4.56-sku1
    sg	sku3	15.00-sku1	9.45-sku2
    sg	sku4	9.68-sku1	9.45-sku3

joining_raptors
---------------

    $ ./Joining-Raptors sg 3 Result/joined.csv Result/bayes/Raptor_sg.csv Result/vtd/Raptor_sg.csv Result/original/Raptor_sg.csv 

    $ cat Result/joined.csv 
    sg	sku5	22.98-sku8	9.45-sku6
    sg	sku6
    sg	sku7	22.98-sku8	9.45-sku6
    sg	sku8	22.98-sku5	22.98-sku7
    sg	sku9	9.45-sku7	9.45-sku8
    sg	sku0	42.68-sku3	38.15-sku1	19.36-sku2
    sg	sku1	38.79-sku3	23.48-sku4	15.99-sku2
    sg	sku2	22.66-sku3	15.99-sku1	15.60-sku4
    sg	sku3	38.79-sku1	22.66-sku2	0.60-sku4
    sg	sku4	17.34-sku1	10.05-sku3	0.60-sku2
