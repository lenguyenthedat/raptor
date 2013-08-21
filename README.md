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

    $ ./Jaccard sg 3 vtd
    $ cat Result/Raptor_sg.csv 
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

