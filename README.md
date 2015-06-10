Raptor
======

<a href="https://raw.githubusercontent.com/lenguyenthedat/raptor/master/assets/raptor.png"><img src="./assets/raptor.png" align="right" height="169" width="220" ></a>

An Online Retail Recommendation Engine developed in Haskell.

Raptor learns from customer behaviors when they do online shopping: 
- View an item.
- Add an item into cart. 
- Purchase an item.

Currently there are 3 different algorithms - specified in raptor.hs:
- Original Jaccard Distance, based heavily on information about items that has been purchased / added to cart together.
- Bayes Jaccard Distance, a modified version of Jaccard Distance, but still very similar.
- VTD, a slightly different approach. The idea of this approach is to look closer as customer Views data. 
Basically tries to answer the question: if a customer view this item, what are the most likely items that the customer is going to buy.

Base on the nature of each algorithms, Original and Bayes Jaccard are more applicable for a Cart Based Recommendation System, while VTD is more  applicable for the Product Based Recommendation System.

[Wilson Score](http://www.evanmiller.org/how-not-to-sort-by-average-rating.html) (of 95% confidence) is used to take care of significance in the approaches.

Raptor In Action
----------------

The below real-world results were achieved by using a combination of Raptor as a core Collaborative Filtering engine together with a numerous of domain specific knowledge engines and tweaks:

<a href="https://raw.githubusercontent.com/lenguyenthedat/raptor/master/assets/raptor-in-action.png"><img src="./assets/raptor-in-action.png"></a>


Sample Input
------------

    $ cat test/Data/VTD_view_sg.csv 
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

    $ cat test/Data/VTD_cart_sg.csv 
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

    $ cat test/Data/VTD_purchased_sg.csv 
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

    $ cat test/Data/sku_male_sg.csv 
    sku5
    sku6
    sku7
    sku8
    sku9

    $ cat test/Data/sku_female_sg.csv 
    sku0
    sku1
    sku2
    sku3
    sku4

    $ cat test/Data/instock_skus_sg.csv 
    sku0
    sku1
    sku2
    sku3
    sku4
    sku5
    sku6
    sku7
    sku8

    $ cat test/Data/valid_skus_sg.csv 
    sku1
    sku2
    sku3
    sku4
    sku5
    sku6
    sku7
    sku8

Clone and build:
----------------

Clone the repo:

	$ https://github.com/lenguyenthedat/raptor.git

Build with Docker

	$ cd raptor/
	$ sudo docker build --rm=true -t raptor .

Build with cabal sandbox:

    $ cabal install
    Resolving dependencies...
    Notice: installing into a sandbox located at
    /Users/datle/GitHub/cabinet/raptor/.cabal-sandbox
    Configuring split-0.2.2...
    Downloading strict-0.3.2...
    Configuring text-1.2.0.0...
    Configuring strict-0.3.2...
    Building text-1.2.0.0...
    Building split-0.2.2...
    Building strict-0.3.2...
    Installed split-0.2.2
    Installed strict-0.3.2
    Downloading nlp-scores-0.6.2...
    Configuring nlp-scores-0.6.2...
    Building nlp-scores-0.6.2...
    Installed nlp-scores-0.6.2
    Installed text-1.2.0.0
    Configuring hashable-1.2.2.0...
    Building hashable-1.2.2.0...
    Installed hashable-1.2.2.0
    Configuring unordered-containers-0.2.5.1...
    Building unordered-containers-0.2.5.1...
    Installed unordered-containers-0.2.5.1
    Configuring raptor-0.1.0.0...
    Building raptor-0.1.0.0...
    Installed raptor-0.1.0.0

Build with cabal-dev:

    $ cabal-dev install
    Resolving dependencies...
    Configuring raptor-0.1.0.0...
    Building raptor-0.1.0.0...
    Preprocessing executable 'raptor' for raptor-0.1.0.0...
    Warning: No documentation was generated as this package does not contain a
    library. Perhaps you want to use the --executables flag.
    Installing executable(s) in /Users/datle/GitHub/cabinet/Raptor/cabal-dev//bin
    Installed raptor-0.1.0.0
    Warning: could not create symlinks in /Users/datle/Library/Haskell/bin for
    raptor because the files exist there already and are
    not managed by cabal. You can create symlinks for these executables manually
    if you wish. The executable files have been installed at
    /Users/xxx/GitHub/cabinet/Raptor/cabal-dev/bin/raptor

Build with ghc:

    $ ghc raptor.hs
    [1 of 1] Compiling Main             ( raptor.hs, raptor.o )
    Linking raptor ...

Sample output:
--------------

Run with docker: use `docker run raptor` as a prefix for every command, for example:

	$ docker run raptor .cabal-sandbox/bin/raptor sg 3 original test/

original

    $ .cabal-sandbox/bin/raptor sg 3 original test/
    
    $ cat test/Result/original/Raptor_sg.csv 
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

    $ .cabal-sandbox/bin/raptor sg 3 bayes test/
    
    $ cat test/Result/bayes/Raptor_sg.csv 
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

    $ .cabal-sandbox/bin/raptor sg 3 vtd test/
    
    $ cat test/Result/vtd/Raptor_sg.csv 
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
