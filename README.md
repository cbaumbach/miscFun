Installation
============

1. git clone https://github.com/cbaumbach/miscFun.git miscFun
2. R CMD build miscFun
3. R CMD check miscFun_x.y.tar.gz
4. R CMD INSTALL [-l /path/to/local/library/tree] miscFun_x.y.tar.gz
