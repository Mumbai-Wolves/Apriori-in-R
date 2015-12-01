Kindly install the package R.matlab if not installed. This package is used to read the matlab file.
If you are using Windows , then I would suggest you to install the package using RStudio's install packages. 

Make sure that 'transaction.mat' is in the same folder as the program.The file 'transaction.mat'  contains all the transactions. You can also change the path to the file by changing the contents of path variable.

The program will generate the following in the same folder
1) frequentitemsets.txt			- Contains all the frequent itemsets
2) maximalfrequentitemsets.txt		- Contains all the maximal frequent itemsets
3) rules.txt				- Contains all the rules

c and l are vectors of dataframe which represents candidate and frequent itemsets respectively. To view k-item candidate set use c[[k]]. Similarly for k-item frequent set use l[[k]]