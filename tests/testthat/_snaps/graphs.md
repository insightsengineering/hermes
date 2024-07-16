# draw_libsize_hist works as expected

    Code
      ggplot2::layer_data(result)
    Output
         y count       x    xmin    xmax      density ncount ndensity flipped_aes
      1  3     3 4675339 4529234 4821443 5.133318e-07    0.6      0.6       FALSE
      2  4     4 4967547 4821443 5113652 6.844424e-07    0.8      0.8       FALSE
      3  3     3 5259756 5113652 5405860 5.133318e-07    0.6      0.6       FALSE
      4  5     5 5551965 5405860 5698069 8.555530e-07    1.0      1.0       FALSE
      5  0     0 5844173 5698069 5990278 0.000000e+00    0.0      0.0       FALSE
      6  2     2 6136382 5990278 6282486 3.422212e-07    0.4      0.4       FALSE
      7  2     2 6428591 6282486 6574695 3.422212e-07    0.4      0.4       FALSE
      8  0     0 6720799 6574695 6866904 0.000000e+00    0.0      0.0       FALSE
      9  0     0 7013008 6866904 7159112 0.000000e+00    0.0      0.0       FALSE
      10 1     1 7305217 7159112 7451321 1.711106e-07    0.2      0.2       FALSE
         PANEL group ymin ymax colour fill linewidth linetype alpha
      1      1    -1    0    3     NA blue       0.5        1    NA
      2      1    -1    0    4     NA blue       0.5        1    NA
      3      1    -1    0    3     NA blue       0.5        1    NA
      4      1    -1    0    5     NA blue       0.5        1    NA
      5      1    -1    0    0     NA blue       0.5        1    NA
      6      1    -1    0    2     NA blue       0.5        1    NA
      7      1    -1    0    2     NA blue       0.5        1    NA
      8      1    -1    0    0     NA blue       0.5        1    NA
      9      1    -1    0    0     NA blue       0.5        1    NA
      10     1    -1    0    1     NA blue       0.5        1    NA

# draw_libsize_qq works as expected

    Code
      ggplot2::layer_data(result)
    Output
                   x       y  sample theoretical PANEL group shape colour size fill
      1  -1.95996398 4632496 4632496 -1.95996398     1    -1    19  black  1.5   NA
      2  -1.43953147 4642530 4642530 -1.43953147     1    -1    19  black  1.5   NA
      3  -1.15034938 4778014 4778014 -1.15034938     1    -1    19  black  1.5   NA
      4  -0.93458929 4923925 4923925 -0.93458929     1    -1    19  black  1.5   NA
      5  -0.75541503 4990556 4990556 -0.75541503     1    -1    19  black  1.5   NA
      6  -0.59776013 5016838 5016838 -0.59776013     1    -1    19  black  1.5   NA
      7  -0.45376219 5026848 5026848 -0.45376219     1    -1    19  black  1.5   NA
      8  -0.31863936 5154958 5154958 -0.31863936     1    -1    19  black  1.5   NA
      9  -0.18911843 5286953 5286953 -0.18911843     1    -1    19  black  1.5   NA
      10 -0.06270678 5314431 5314431 -0.06270678     1    -1    19  black  1.5   NA
      11  0.06270678 5417510 5417510  0.06270678     1    -1    19  black  1.5   NA
      12  0.18911843 5426566 5426566  0.18911843     1    -1    19  black  1.5   NA
      13  0.31863936 5474927 5474927  0.31863936     1    -1    19  black  1.5   NA
      14  0.45376219 5533212 5533212  0.45376219     1    -1    19  black  1.5   NA
      15  0.59776013 5560720 5560720  0.59776013     1    -1    19  black  1.5   NA
      16  0.75541503 6080233 6080233  0.75541503     1    -1    19  black  1.5   NA
      17  0.93458929 6139497 6139497  0.93458929     1    -1    19  black  1.5   NA
      18  1.15034938 6408552 6408552  1.15034938     1    -1    19  black  1.5   NA
      19  1.43953147 6464043 6464043  1.43953147     1    -1    19  black  1.5   NA
      20  1.95996398 7262374 7262374  1.95996398     1    -1    19  black  1.5   NA
         alpha stroke
      1     NA    0.5
      2     NA    0.5
      3     NA    0.5
      4     NA    0.5
      5     NA    0.5
      6     NA    0.5
      7     NA    0.5
      8     NA    0.5
      9     NA    0.5
      10    NA    0.5
      11    NA    0.5
      12    NA    0.5
      13    NA    0.5
      14    NA    0.5
      15    NA    0.5
      16    NA    0.5
      17    NA    0.5
      18    NA    0.5
      19    NA    0.5
      20    NA    0.5

# draw_libsize_densities works as expected

    Code
      result
    Output
               y     x density scaled ndensity count    n flipped_aes group PANEL
      1  0.00097 -2.50 0.00097 0.0042   0.0042   4.9 5100       FALSE     1     1
      2  0.00120 -2.50 0.00120 0.0051   0.0051   6.0 5100       FALSE     1     1
      3  0.00140 -2.40 0.00140 0.0062   0.0062   7.3 5100       FALSE     1     1
      4  0.00170 -2.40 0.00170 0.0075   0.0075   8.8 5100       FALSE     1     1
      5  0.00210 -2.30 0.00210 0.0090   0.0090  11.0 5100       FALSE     1     1
      6  0.00250 -2.30 0.00250 0.0110   0.0110  13.0 5100       FALSE     1     1
      7  0.00300 -2.20 0.00300 0.0130   0.0130  15.0 5100       FALSE     1     1
      8  0.00350 -2.20 0.00350 0.0150   0.0150  18.0 5100       FALSE     1     1
      9  0.00420 -2.10 0.00420 0.0180   0.0180  21.0 5100       FALSE     1     1
      10 0.00490 -2.10 0.00490 0.0210   0.0210  25.0 5100       FALSE     1     1
      11 0.00580 -2.00 0.00580 0.0250   0.0250  29.0 5100       FALSE     1     1
      12 0.00680 -2.00 0.00680 0.0300   0.0300  34.0 5100       FALSE     1     1
      13 0.00790 -2.00 0.00790 0.0350   0.0350  40.0 5100       FALSE     1     1
      14 0.00920 -1.90 0.00920 0.0400   0.0400  47.0 5100       FALSE     1     1
      15 0.01100 -1.90 0.01100 0.0470   0.0470  54.0 5100       FALSE     1     1
      16 0.01200 -1.80 0.01200 0.0540   0.0540  63.0 5100       FALSE     1     1
      17 0.01400 -1.80 0.01400 0.0620   0.0620  72.0 5100       FALSE     1     1
      18 0.01600 -1.70 0.01600 0.0710   0.0710  83.0 5100       FALSE     1     1
      19 0.01900 -1.70 0.01900 0.0810   0.0810  95.0 5100       FALSE     1     1
      20 0.02100 -1.60 0.02100 0.0930   0.0930 110.0 5100       FALSE     1     1
      21 0.02400 -1.60 0.02400 0.1100   0.1100 120.0 5100       FALSE     1     1
      22 0.02700 -1.60 0.02700 0.1200   0.1200 140.0 5100       FALSE     1     1
      23 0.03100 -1.50 0.03100 0.1300   0.1300 160.0 5100       FALSE     1     1
      24 0.03500 -1.50 0.03500 0.1500   0.1500 180.0 5100       FALSE     1     1
      25 0.03900 -1.40 0.03900 0.1700   0.1700 200.0 5100       FALSE     1     1
      26 0.04300 -1.40 0.04300 0.1900   0.1900 220.0 5100       FALSE     1     1
      27 0.04800 -1.30 0.04800 0.2100   0.2100 240.0 5100       FALSE     1     1
      28 0.05300 -1.30 0.05300 0.2300   0.2300 270.0 5100       FALSE     1     1
      29 0.05900 -1.20 0.05900 0.2600   0.2600 300.0 5100       FALSE     1     1
      30 0.06500 -1.20 0.06500 0.2800   0.2800 330.0 5100       FALSE     1     1
      31 0.07100 -1.10 0.07100 0.3100   0.3100 360.0 5100       FALSE     1     1
      32 0.07700 -1.10 0.07700 0.3400   0.3400 390.0 5100       FALSE     1     1
      33 0.08400 -1.10 0.08400 0.3700   0.3700 430.0 5100       FALSE     1     1
      34 0.09100 -1.00 0.09100 0.4000   0.4000 470.0 5100       FALSE     1     1
      35 0.09900 -0.96 0.09900 0.4300   0.4300 500.0 5100       FALSE     1     1
      36 0.11000 -0.92 0.11000 0.4600   0.4600 540.0 5100       FALSE     1     1
      37 0.11000 -0.87 0.11000 0.5000   0.5000 580.0 5100       FALSE     1     1
      38 0.12000 -0.83 0.12000 0.5300   0.5300 620.0 5100       FALSE     1     1
      39 0.13000 -0.78 0.13000 0.5700   0.5700 660.0 5100       FALSE     1     1
      40 0.14000 -0.74 0.14000 0.6000   0.6000 700.0 5100       FALSE     1     1
         ymin    ymax fill weight colour alpha linewidth linetype
      1     0 0.00097   NA      1  black    NA       0.5        1
      2     0 0.00120   NA      1  black    NA       0.5        1
      3     0 0.00140   NA      1  black    NA       0.5        1
      4     0 0.00170   NA      1  black    NA       0.5        1
      5     0 0.00210   NA      1  black    NA       0.5        1
      6     0 0.00250   NA      1  black    NA       0.5        1
      7     0 0.00300   NA      1  black    NA       0.5        1
      8     0 0.00350   NA      1  black    NA       0.5        1
      9     0 0.00420   NA      1  black    NA       0.5        1
      10    0 0.00490   NA      1  black    NA       0.5        1
      11    0 0.00580   NA      1  black    NA       0.5        1
      12    0 0.00680   NA      1  black    NA       0.5        1
      13    0 0.00790   NA      1  black    NA       0.5        1
      14    0 0.00920   NA      1  black    NA       0.5        1
      15    0 0.01100   NA      1  black    NA       0.5        1
      16    0 0.01200   NA      1  black    NA       0.5        1
      17    0 0.01400   NA      1  black    NA       0.5        1
      18    0 0.01600   NA      1  black    NA       0.5        1
      19    0 0.01900   NA      1  black    NA       0.5        1
      20    0 0.02100   NA      1  black    NA       0.5        1
      21    0 0.02400   NA      1  black    NA       0.5        1
      22    0 0.02700   NA      1  black    NA       0.5        1
      23    0 0.03100   NA      1  black    NA       0.5        1
      24    0 0.03500   NA      1  black    NA       0.5        1
      25    0 0.03900   NA      1  black    NA       0.5        1
      26    0 0.04300   NA      1  black    NA       0.5        1
      27    0 0.04800   NA      1  black    NA       0.5        1
      28    0 0.05300   NA      1  black    NA       0.5        1
      29    0 0.05900   NA      1  black    NA       0.5        1
      30    0 0.06500   NA      1  black    NA       0.5        1
      31    0 0.07100   NA      1  black    NA       0.5        1
      32    0 0.07700   NA      1  black    NA       0.5        1
      33    0 0.08400   NA      1  black    NA       0.5        1
      34    0 0.09100   NA      1  black    NA       0.5        1
      35    0 0.09900   NA      1  black    NA       0.5        1
      36    0 0.11000   NA      1  black    NA       0.5        1
      37    0 0.11000   NA      1  black    NA       0.5        1
      38    0 0.12000   NA      1  black    NA       0.5        1
      39    0 0.13000   NA      1  black    NA       0.5        1
      40    0 0.14000   NA      1  black    NA       0.5        1

# draw_nonzero_boxplot works as expected with default options

    Code
      ggplot2::layer_data(result)
    Output
        ymin   lower middle   upper ymax outliers notchupper notchlower x flipped_aes
      1 2863 2996.25   3032 3116.75 3265     2600   3074.572   2989.428 1       FALSE
        PANEL group ymin_final ymax_final  xmin  xmax xid newx new_width weight
      1     1     1       2600       3265 0.625 1.375   1    1      0.75      1
        colour  fill alpha shape linetype linewidth
      1 grey20 white    NA    19    solid       0.5

---

    Code
      ggplot2::layer_data(result, 2)
    Output
        ymin   lower middle   upper ymax outliers notchupper notchlower x relvarwidth
      1 2863 2996.25   3032 3116.75 3265     2600   3074.572   2989.428 1    4.472136
        flipped_aes PANEL group  xmin  xmax xid newx new_width colour linewidth
      1       FALSE     1     1 0.625 1.375   1    1      0.75  black       0.5
        linetype width alpha
      1        1   0.5    NA

---

    Code
      ggplot2::layer_data(result, 3)
    Output
                 x        y PANEL group shape colour size fill alpha stroke
      1  0.9509281 2911.890     1     1    19  black  1.5   NA  0.25    0.5
      2  1.0870438 3115.329     1     1    19  black  1.5   NA  0.25    0.5
      3  1.1126466 3121.926     1     1    19  black  1.5   NA  0.25    0.5
      4  1.1722427 3159.743     1     1    19  black  1.5   NA  0.25    0.5
      5  1.0316643 3011.771     1     1    19  black  1.5   NA  0.25    0.5
      6  0.8684829 3075.733     1     1    19  black  1.5   NA  0.25    0.5
      7  1.0115864 2966.896     1     1    19  black  1.5   NA  0.25    0.5
      8  0.8889332 3043.250     1     1    19  black  1.5   NA  0.25    0.5
      9  0.8701430 3015.711     1     1    19  black  1.5   NA  0.25    0.5
      10 0.9092406 3101.033     1     1    19  black  1.5   NA  0.25    0.5
      11 1.1837041 2912.623     1     1    19  black  1.5   NA  0.25    0.5
      12 1.1671752 3136.353     1     1    19  black  1.5   NA  0.25    0.5
      13 1.0535626 3021.206     1     1    19  black  1.5   NA  0.25    0.5
      14 1.0045885 2599.827     1     1    19  black  1.5   NA  0.25    0.5
      15 0.9452316 3010.244     1     1    19  black  1.5   NA  0.25    0.5
      16 1.0192162 3265.007     1     1    19  black  1.5   NA  0.25    0.5
      17 0.9702712 3107.241     1     1    19  black  1.5   NA  0.25    0.5
      18 0.8265099 2862.772     1     1    19  black  1.5   NA  0.25    0.5
      19 1.1518527 3006.086     1     1    19  black  1.5   NA  0.25    0.5
      20 0.9440135 3176.225     1     1    19  black  1.5   NA  0.25    0.5

# draw_nonzero_boxplot works as expected with custom options

    Code
      ggplot2::layer_data(result)
    Output
        ymin   lower middle   upper ymax outliers notchupper notchlower x flipped_aes
      1 2863 2996.25   3032 3116.75 3265     2600   3074.572   2989.428 1       FALSE
        PANEL group ymin_final ymax_final  xmin  xmax xid newx new_width weight
      1     1     1       2600       3265 0.625 1.375   1    1      0.75      1
        colour  fill alpha shape linetype linewidth
      1 grey20 white    NA    19    solid       0.5

---

    Code
      ggplot2::layer_data(result, 2)
    Output
        ymin   lower middle   upper ymax outliers notchupper notchlower x relvarwidth
      1 2863 2996.25   3032 3116.75 3265     2600   3074.572   2989.428 1    4.472136
        flipped_aes PANEL group  xmin  xmax xid newx new_width colour linewidth
      1       FALSE     1     1 0.625 1.375   1    1      0.75  black       0.5
        linetype width alpha
      1        1   0.5    NA

---

    Code
      ggplot2::layer_data(result, 3)
    Output
         x    y PANEL group shape colour size fill alpha stroke
      1  1 2912     1     1    19  black  1.5   NA     1    0.5
      2  1 3115     1     1    19  black  1.5   NA     1    0.5
      3  1 3122     1     1    19  black  1.5   NA     1    0.5
      4  1 3160     1     1    19  black  1.5   NA     1    0.5
      5  1 3012     1     1    19  black  1.5   NA     1    0.5
      6  1 3076     1     1    19  black  1.5   NA     1    0.5
      7  1 2967     1     1    19  black  1.5   NA     1    0.5
      8  1 3043     1     1    19  black  1.5   NA     1    0.5
      9  1 3016     1     1    19  black  1.5   NA     1    0.5
      10 1 3101     1     1    19  black  1.5   NA     1    0.5
      11 1 2913     1     1    19  black  1.5   NA     1    0.5
      12 1 3136     1     1    19  black  1.5   NA     1    0.5
      13 1 3021     1     1    19  black  1.5   NA     1    0.5
      14 1 2600     1     1    19  black  1.5   NA     1    0.5
      15 1 3010     1     1    19  black  1.5   NA     1    0.5
      16 1 3265     1     1    19  black  1.5   NA     1    0.5
      17 1 3107     1     1    19  black  1.5   NA     1    0.5
      18 1 2863     1     1    19  black  1.5   NA     1    0.5
      19 1 3006     1     1    19  black  1.5   NA     1    0.5
      20 1 3176     1     1    19  black  1.5   NA     1    0.5

# draw_genes_barplot works as expected with default options

    Code
      ggplot2::layer_data(result)
    Output
            fill   y count prop  x flipped_aes PANEL group ymin ymax  xmin  xmax
      1  #F8766D 477   207    1  1       FALSE     1     1  270  477  0.55  1.45
      2  #F8766D 326   133    1  2       FALSE     1     2  193  326  1.55  2.45
      3  #F8766D 254    88    1  3       FALSE     1     3  166  254  2.55  3.45
      4  #F8766D 185    68    1  4       FALSE     1     4  117  185  3.55  4.45
      5  #F8766D 218    94    1  5       FALSE     1     5  124  218  4.55  5.45
      6  #F8766D 266   108    1  6       FALSE     1     6  158  266  5.55  6.45
      7  #F8766D 259   103    1  7       FALSE     1     7  156  259  6.55  7.45
      8  #F8766D 212    95    1  8       FALSE     1     8  117  212  7.55  8.45
      9  #F8766D 189    70    1  9       FALSE     1     9  119  189  8.55  9.45
      10 #F8766D 180    74    1 10       FALSE     1    10  106  180  9.55 10.45
      11 #F8766D 297   120    1 11       FALSE     1    11  177  297 10.55 11.45
      12 #F8766D 222    92    1 12       FALSE     1    12  130  222 11.55 12.45
      13 #F8766D 114    46    1 13       FALSE     1    13   68  114 12.55 13.45
      14 #F8766D 156    77    1 14       FALSE     1    14   79  156 13.55 14.45
      15 #F8766D 196    85    1 15       FALSE     1    15  111  196 14.55 15.45
      16 #F8766D 188    76    1 16       FALSE     1    16  112  188 15.55 16.45
      17 #F8766D 281   113    1 17       FALSE     1    17  168  281 16.55 17.45
      18 #F8766D  85    31    1 18       FALSE     1    18   54   85 17.55 18.45
      19 #F8766D 323   130    1 19       FALSE     1    19  193  323 18.55 19.45
      20 #F8766D 127    41    1 20       FALSE     1    20   86  127 19.55 20.45
      21 #F8766D  87    28    1 21       FALSE     1    21   59   87 20.55 21.45
      22 #F8766D 117    47    1 22       FALSE     1    22   70  117 21.55 22.45
      23 #F8766D 186    75    1 23       FALSE     1    23  111  186 22.55 23.45
      24 #F8766D 110    51    1 24       FALSE     1    24   59  110 23.55 24.45
      25 #F8766D  11     7    1 25       FALSE     1    25    4   11 24.55 25.45
      26 #F8766D  19     5    1 26       FALSE     1    26   14   19 25.55 26.45
      27 #00BFC4 270   270    1  1       FALSE     1    27    0  270  0.55  1.45
      28 #00BFC4 193   193    1  2       FALSE     1    28    0  193  1.55  2.45
      29 #00BFC4 166   166    1  3       FALSE     1    29    0  166  2.55  3.45
      30 #00BFC4 117   117    1  4       FALSE     1    30    0  117  3.55  4.45
      31 #00BFC4 124   124    1  5       FALSE     1    31    0  124  4.55  5.45
      32 #00BFC4 158   158    1  6       FALSE     1    32    0  158  5.55  6.45
      33 #00BFC4 156   156    1  7       FALSE     1    33    0  156  6.55  7.45
      34 #00BFC4 117   117    1  8       FALSE     1    34    0  117  7.55  8.45
      35 #00BFC4 119   119    1  9       FALSE     1    35    0  119  8.55  9.45
      36 #00BFC4 106   106    1 10       FALSE     1    36    0  106  9.55 10.45
      37 #00BFC4 177   177    1 11       FALSE     1    37    0  177 10.55 11.45
      38 #00BFC4 130   130    1 12       FALSE     1    38    0  130 11.55 12.45
      39 #00BFC4  68    68    1 13       FALSE     1    39    0   68 12.55 13.45
      40 #00BFC4  79    79    1 14       FALSE     1    40    0   79 13.55 14.45
      41 #00BFC4 111   111    1 15       FALSE     1    41    0  111 14.55 15.45
      42 #00BFC4 112   112    1 16       FALSE     1    42    0  112 15.55 16.45
      43 #00BFC4 168   168    1 17       FALSE     1    43    0  168 16.55 17.45
      44 #00BFC4  54    54    1 18       FALSE     1    44    0   54 17.55 18.45
      45 #00BFC4 193   193    1 19       FALSE     1    45    0  193 18.55 19.45
      46 #00BFC4  86    86    1 20       FALSE     1    46    0   86 19.55 20.45
      47 #00BFC4  59    59    1 21       FALSE     1    47    0   59 20.55 21.45
      48 #00BFC4  70    70    1 22       FALSE     1    48    0   70 21.55 22.45
      49 #00BFC4 111   111    1 23       FALSE     1    49    0  111 22.55 23.45
      50 #00BFC4  59    59    1 24       FALSE     1    50    0   59 23.55 24.45
      51 #00BFC4   4     4    1 25       FALSE     1    51    0    4 24.55 25.45
      52 #00BFC4  14    14    1 26       FALSE     1    52    0   14 25.55 26.45
         colour linewidth linetype alpha
      1      NA       0.5        1    NA
      2      NA       0.5        1    NA
      3      NA       0.5        1    NA
      4      NA       0.5        1    NA
      5      NA       0.5        1    NA
      6      NA       0.5        1    NA
      7      NA       0.5        1    NA
      8      NA       0.5        1    NA
      9      NA       0.5        1    NA
      10     NA       0.5        1    NA
      11     NA       0.5        1    NA
      12     NA       0.5        1    NA
      13     NA       0.5        1    NA
      14     NA       0.5        1    NA
      15     NA       0.5        1    NA
      16     NA       0.5        1    NA
      17     NA       0.5        1    NA
      18     NA       0.5        1    NA
      19     NA       0.5        1    NA
      20     NA       0.5        1    NA
      21     NA       0.5        1    NA
      22     NA       0.5        1    NA
      23     NA       0.5        1    NA
      24     NA       0.5        1    NA
      25     NA       0.5        1    NA
      26     NA       0.5        1    NA
      27     NA       0.5        1    NA
      28     NA       0.5        1    NA
      29     NA       0.5        1    NA
      30     NA       0.5        1    NA
      31     NA       0.5        1    NA
      32     NA       0.5        1    NA
      33     NA       0.5        1    NA
      34     NA       0.5        1    NA
      35     NA       0.5        1    NA
      36     NA       0.5        1    NA
      37     NA       0.5        1    NA
      38     NA       0.5        1    NA
      39     NA       0.5        1    NA
      40     NA       0.5        1    NA
      41     NA       0.5        1    NA
      42     NA       0.5        1    NA
      43     NA       0.5        1    NA
      44     NA       0.5        1    NA
      45     NA       0.5        1    NA
      46     NA       0.5        1    NA
      47     NA       0.5        1    NA
      48     NA       0.5        1    NA
      49     NA       0.5        1    NA
      50     NA       0.5        1    NA
      51     NA       0.5        1    NA
      52     NA       0.5        1    NA

# draw_genes_barplot works as expected with custom options

    Code
      ggplot2::layer_data(result)
    Output
           fill   y count prop x flipped_aes PANEL group ymin ymax xmin xmax colour
      1 #F8766D 254    88    1 1       FALSE     1     1  166  254 0.55 1.45     NA
      2 #F8766D 297   120    1 2       FALSE     1     2  177  297 1.55 2.45     NA
      3 #00BFC4 166   166    1 1       FALSE     1     3    0  166 0.55 1.45     NA
      4 #00BFC4 177   177    1 2       FALSE     1     4    0  177 1.55 2.45     NA
        linewidth linetype alpha
      1       0.5        1    NA
      2       0.5        1    NA
      3       0.5        1    NA
      4       0.5        1    NA

