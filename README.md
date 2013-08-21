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


Input Data
---------

$ VTD_*_sg.csv: 
24781HXMSG	000021376129098
24782HXLSG	000021375080006
24790HXXSG	000021365009921 0000213699263
24791HXWSG	000021370301204 0000213755444 0000213657975
24H01CPASG	000021700835144 0000213705070 0000213652707
24H05COWSG	000021372732319 0000213735379 0000213747632 00002137382123 0000213757652

$ *item*_sg.csv:
24A92HXVSG
24H00CPBSG
24783CPASG
24783COZSG
24703COYSG
24707UPCSG
24758UPBSG
24729XBQSG

Output
-----------
$ Raptor_sg.csv:
UN821I1DSG	9.45-AL803S1NSG	6.86-VE594C1LSG	4.56-SO927L1JSG
UN821I1ESG	9.45-AL803S1NSG
UN821I1FSG	9.45-JO835J1USG	9.45-RI909X1OSG	9.25-RI909X1TSG	7.28-RI909X1USG	6.45-RI909Y1BSG	2.42-RI909Y1ESG
UN821I1GSG	6.15-AL803S1NSG	4.56-EM883Y1GSG
UN821I1HSG	4.56-SO927L1ISG
UN821I1ISG	6.15-SO927L1ISG	6.15-EM883Y1GSG	3.62-UN821H1YSG
UN821I1JSG	9.45-CA652P1VSG	9.25-SO927L1OSG	7.67-VE594B1YSG	6.22-VE594E1OSG	1.28-VE594H1FSG


