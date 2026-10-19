# draw_libsize_hist works as expected

    Code
      ggplot2::layer_data(result)
    Output
         count       x    xmin    xmax      density ncount ndensity flipped_aes PANEL
      1      3 4632496 4486392 4778600 5.133318e-07    0.6      0.6       FALSE     1
      2      4 4924705 4778600 5070809 6.844424e-07    0.8      0.8       FALSE     1
      3      3 5216913 5070809 5363018 5.133318e-07    0.6      0.6       FALSE     1
      4      5 5509122 5363018 5655226 8.555530e-07    1.0      1.0       FALSE     1
      5      0 5801331 5655226 5947435 0.000000e+00    0.0      0.0       FALSE     1
      6      2 6093539 5947435 6239644 3.422212e-07    0.4      0.4       FALSE     1
      7      2 6385748 6239644 6531852 3.422212e-07    0.4      0.4       FALSE     1
      8      0 6677957 6531852 6824061 0.000000e+00    0.0      0.0       FALSE     1
      9      0 6970165 6824061 7116270 0.000000e+00    0.0      0.0       FALSE     1
      10     1 7262374 7116270 7408478 1.711106e-07    0.2      0.2       FALSE     1
         group y ymin ymax colour fill linewidth linetype alpha width
      1     -1 3    0    3     NA blue       0.5        1    NA   0.9
      2     -1 4    0    4     NA blue       0.5        1    NA   0.9
      3     -1 3    0    3     NA blue       0.5        1    NA   0.9
      4     -1 5    0    5     NA blue       0.5        1    NA   0.9
      5     -1 0    0    0     NA blue       0.5        1    NA   0.9
      6     -1 2    0    2     NA blue       0.5        1    NA   0.9
      7     -1 2    0    2     NA blue       0.5        1    NA   0.9
      8     -1 0    0    0     NA blue       0.5        1    NA   0.9
      9     -1 0    0    0     NA blue       0.5        1    NA   0.9
      10    -1 1    0    1     NA blue       0.5        1    NA   0.9

# draw_libsize_qq works as expected

    Code
      ggplot2::layer_data(result)
    Output
          sample theoretical PANEL group           x       y shape colour fill size
      1  4632496 -1.95996398     1    -1 -1.95996398 4632496    19  black   NA  1.5
      2  4642530 -1.43953147     1    -1 -1.43953147 4642530    19  black   NA  1.5
      3  4778014 -1.15034938     1    -1 -1.15034938 4778014    19  black   NA  1.5
      4  4923925 -0.93458929     1    -1 -0.93458929 4923925    19  black   NA  1.5
      5  4990556 -0.75541503     1    -1 -0.75541503 4990556    19  black   NA  1.5
      6  5016838 -0.59776013     1    -1 -0.59776013 5016838    19  black   NA  1.5
      7  5026848 -0.45376219     1    -1 -0.45376219 5026848    19  black   NA  1.5
      8  5154958 -0.31863936     1    -1 -0.31863936 5154958    19  black   NA  1.5
      9  5286953 -0.18911843     1    -1 -0.18911843 5286953    19  black   NA  1.5
      10 5314431 -0.06270678     1    -1 -0.06270678 5314431    19  black   NA  1.5
      11 5417510  0.06270678     1    -1  0.06270678 5417510    19  black   NA  1.5
      12 5426566  0.18911843     1    -1  0.18911843 5426566    19  black   NA  1.5
      13 5474927  0.31863936     1    -1  0.31863936 5474927    19  black   NA  1.5
      14 5533212  0.45376219     1    -1  0.45376219 5533212    19  black   NA  1.5
      15 5560720  0.59776013     1    -1  0.59776013 5560720    19  black   NA  1.5
      16 6080233  0.75541503     1    -1  0.75541503 6080233    19  black   NA  1.5
      17 6139497  0.93458929     1    -1  0.93458929 6139497    19  black   NA  1.5
      18 6408552  1.15034938     1    -1  1.15034938 6408552    19  black   NA  1.5
      19 6464043  1.43953147     1    -1  1.43953147 6464043    19  black   NA  1.5
      20 7262374  1.95996398     1    -1  1.95996398 7262374    19  black   NA  1.5
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

# draw_nonzero_boxplot works as expected with default options

    Code
      ggplot2::layer_data(result)
    Output
        ymin   lower middle   upper ymax outliers notchupper notchlower x flipped_aes
      1 2863 2996.25   3032 3116.75 3265     2600   3074.572   2989.428 1       FALSE
        PANEL group ymin_final ymax_final  xmin  xmax order xid newx new_width weight
      1     1     1       2600       3265 0.625 1.375     1   1    1      0.75      1
           colour  fill size alpha shape linetype linewidth width
      1 #333333FF white  1.5    NA    19        1       0.5   0.9

---

    Code
      ggplot2::layer_data(result, 2)
    Output
        ymin   lower middle   upper ymax outliers notchupper notchlower x relvarwidth
      1 2863 2996.25   3032 3116.75 3265     2600   3074.572   2989.428 1    4.472136
        flipped_aes PANEL group  xmin  xmax order xid newx new_width colour linewidth
      1       FALSE     1     1 0.625 1.375     1   1    1      0.75  black       0.5
        linetype width alpha
      1        1   0.9    NA

---

    Code
      ggplot2::layer_data(result, 3)
    Output
                 x        y PANEL group shape colour fill size alpha stroke
      1  0.9509281 2911.890     1     1    19  black   NA  1.5  0.25    0.5
      2  1.0870438 3115.329     1     1    19  black   NA  1.5  0.25    0.5
      3  1.1126466 3121.926     1     1    19  black   NA  1.5  0.25    0.5
      4  1.1722427 3159.743     1     1    19  black   NA  1.5  0.25    0.5
      5  1.0316643 3011.771     1     1    19  black   NA  1.5  0.25    0.5
      6  0.8684829 3075.733     1     1    19  black   NA  1.5  0.25    0.5
      7  1.0115864 2966.896     1     1    19  black   NA  1.5  0.25    0.5
      8  0.8889332 3043.250     1     1    19  black   NA  1.5  0.25    0.5
      9  0.8701430 3015.711     1     1    19  black   NA  1.5  0.25    0.5
      10 0.9092406 3101.033     1     1    19  black   NA  1.5  0.25    0.5
      11 1.1837041 2912.623     1     1    19  black   NA  1.5  0.25    0.5
      12 1.1671752 3136.353     1     1    19  black   NA  1.5  0.25    0.5
      13 1.0535626 3021.206     1     1    19  black   NA  1.5  0.25    0.5
      14 1.0045885 2599.827     1     1    19  black   NA  1.5  0.25    0.5
      15 0.9452316 3010.244     1     1    19  black   NA  1.5  0.25    0.5
      16 1.0192162 3265.007     1     1    19  black   NA  1.5  0.25    0.5
      17 0.9702712 3107.241     1     1    19  black   NA  1.5  0.25    0.5
      18 0.8265099 2862.772     1     1    19  black   NA  1.5  0.25    0.5
      19 1.1518527 3006.086     1     1    19  black   NA  1.5  0.25    0.5
      20 0.9440135 3176.225     1     1    19  black   NA  1.5  0.25    0.5

# draw_nonzero_boxplot works as expected with custom options

    Code
      ggplot2::layer_data(result)
    Output
        ymin   lower middle   upper ymax outliers notchupper notchlower x flipped_aes
      1 2863 2996.25   3032 3116.75 3265     2600   3074.572   2989.428 1       FALSE
        PANEL group ymin_final ymax_final  xmin  xmax order xid newx new_width weight
      1     1     1       2600       3265 0.625 1.375     1   1    1      0.75      1
           colour  fill size alpha shape linetype linewidth width
      1 #333333FF white  1.5    NA    19        1       0.5   0.9

---

    Code
      ggplot2::layer_data(result, 2)
    Output
        ymin   lower middle   upper ymax outliers notchupper notchlower x relvarwidth
      1 2863 2996.25   3032 3116.75 3265     2600   3074.572   2989.428 1    4.472136
        flipped_aes PANEL group  xmin  xmax order xid newx new_width colour linewidth
      1       FALSE     1     1 0.625 1.375     1   1    1      0.75  black       0.5
        linetype width alpha
      1        1   0.9    NA

---

    Code
      ggplot2::layer_data(result, 3)
    Output
         x    y PANEL group shape colour fill size alpha stroke
      1  1 2912     1     1    19  black   NA  1.5     1    0.5
      2  1 3115     1     1    19  black   NA  1.5     1    0.5
      3  1 3122     1     1    19  black   NA  1.5     1    0.5
      4  1 3160     1     1    19  black   NA  1.5     1    0.5
      5  1 3012     1     1    19  black   NA  1.5     1    0.5
      6  1 3076     1     1    19  black   NA  1.5     1    0.5
      7  1 2967     1     1    19  black   NA  1.5     1    0.5
      8  1 3043     1     1    19  black   NA  1.5     1    0.5
      9  1 3016     1     1    19  black   NA  1.5     1    0.5
      10 1 3101     1     1    19  black   NA  1.5     1    0.5
      11 1 2913     1     1    19  black   NA  1.5     1    0.5
      12 1 3136     1     1    19  black   NA  1.5     1    0.5
      13 1 3021     1     1    19  black   NA  1.5     1    0.5
      14 1 2600     1     1    19  black   NA  1.5     1    0.5
      15 1 3010     1     1    19  black   NA  1.5     1    0.5
      16 1 3265     1     1    19  black   NA  1.5     1    0.5
      17 1 3107     1     1    19  black   NA  1.5     1    0.5
      18 1 2863     1     1    19  black   NA  1.5     1    0.5
      19 1 3006     1     1    19  black   NA  1.5     1    0.5
      20 1 3176     1     1    19  black   NA  1.5     1    0.5

# draw_genes_barplot works as expected with default options

    Code
      ggplot2::layer_data(result)
    Output
         count prop  x flipped_aes    fill PANEL group   y ymin ymax  xmin  xmax
      1    207    1  1       FALSE #F8766D     1     1 477  270  477  0.55  1.45
      2    133    1  2       FALSE #F8766D     1     2 326  193  326  1.55  2.45
      3     88    1  3       FALSE #F8766D     1     3 254  166  254  2.55  3.45
      4     68    1  4       FALSE #F8766D     1     4 185  117  185  3.55  4.45
      5     94    1  5       FALSE #F8766D     1     5 218  124  218  4.55  5.45
      6    108    1  6       FALSE #F8766D     1     6 266  158  266  5.55  6.45
      7    103    1  7       FALSE #F8766D     1     7 259  156  259  6.55  7.45
      8     95    1  8       FALSE #F8766D     1     8 212  117  212  7.55  8.45
      9     70    1  9       FALSE #F8766D     1     9 189  119  189  8.55  9.45
      10    74    1 10       FALSE #F8766D     1    10 180  106  180  9.55 10.45
      11   120    1 11       FALSE #F8766D     1    11 297  177  297 10.55 11.45
      12    92    1 12       FALSE #F8766D     1    12 222  130  222 11.55 12.45
      13    46    1 13       FALSE #F8766D     1    13 114   68  114 12.55 13.45
      14    77    1 14       FALSE #F8766D     1    14 156   79  156 13.55 14.45
      15    85    1 15       FALSE #F8766D     1    15 196  111  196 14.55 15.45
      16    76    1 16       FALSE #F8766D     1    16 188  112  188 15.55 16.45
      17   113    1 17       FALSE #F8766D     1    17 281  168  281 16.55 17.45
      18    31    1 18       FALSE #F8766D     1    18  85   54   85 17.55 18.45
      19   130    1 19       FALSE #F8766D     1    19 323  193  323 18.55 19.45
      20    41    1 20       FALSE #F8766D     1    20 127   86  127 19.55 20.45
      21    28    1 21       FALSE #F8766D     1    21  87   59   87 20.55 21.45
      22    47    1 22       FALSE #F8766D     1    22 117   70  117 21.55 22.45
      23    75    1 23       FALSE #F8766D     1    23 186  111  186 22.55 23.45
      24    51    1 24       FALSE #F8766D     1    24 110   59  110 23.55 24.45
      25     7    1 25       FALSE #F8766D     1    25  11    4   11 24.55 25.45
      26     5    1 26       FALSE #F8766D     1    26  19   14   19 25.55 26.45
      27   270    1  1       FALSE #00BFC4     1    27 270    0  270  0.55  1.45
      28   193    1  2       FALSE #00BFC4     1    28 193    0  193  1.55  2.45
      29   166    1  3       FALSE #00BFC4     1    29 166    0  166  2.55  3.45
      30   117    1  4       FALSE #00BFC4     1    30 117    0  117  3.55  4.45
      31   124    1  5       FALSE #00BFC4     1    31 124    0  124  4.55  5.45
      32   158    1  6       FALSE #00BFC4     1    32 158    0  158  5.55  6.45
      33   156    1  7       FALSE #00BFC4     1    33 156    0  156  6.55  7.45
      34   117    1  8       FALSE #00BFC4     1    34 117    0  117  7.55  8.45
      35   119    1  9       FALSE #00BFC4     1    35 119    0  119  8.55  9.45
      36   106    1 10       FALSE #00BFC4     1    36 106    0  106  9.55 10.45
      37   177    1 11       FALSE #00BFC4     1    37 177    0  177 10.55 11.45
      38   130    1 12       FALSE #00BFC4     1    38 130    0  130 11.55 12.45
      39    68    1 13       FALSE #00BFC4     1    39  68    0   68 12.55 13.45
      40    79    1 14       FALSE #00BFC4     1    40  79    0   79 13.55 14.45
      41   111    1 15       FALSE #00BFC4     1    41 111    0  111 14.55 15.45
      42   112    1 16       FALSE #00BFC4     1    42 112    0  112 15.55 16.45
      43   168    1 17       FALSE #00BFC4     1    43 168    0  168 16.55 17.45
      44    54    1 18       FALSE #00BFC4     1    44  54    0   54 17.55 18.45
      45   193    1 19       FALSE #00BFC4     1    45 193    0  193 18.55 19.45
      46    86    1 20       FALSE #00BFC4     1    46  86    0   86 19.55 20.45
      47    59    1 21       FALSE #00BFC4     1    47  59    0   59 20.55 21.45
      48    70    1 22       FALSE #00BFC4     1    48  70    0   70 21.55 22.45
      49   111    1 23       FALSE #00BFC4     1    49 111    0  111 22.55 23.45
      50    59    1 24       FALSE #00BFC4     1    50  59    0   59 23.55 24.45
      51     4    1 25       FALSE #00BFC4     1    51   4    0    4 24.55 25.45
      52    14    1 26       FALSE #00BFC4     1    52  14    0   14 25.55 26.45
         colour linewidth linetype alpha width
      1      NA       0.5        1    NA   0.9
      2      NA       0.5        1    NA   0.9
      3      NA       0.5        1    NA   0.9
      4      NA       0.5        1    NA   0.9
      5      NA       0.5        1    NA   0.9
      6      NA       0.5        1    NA   0.9
      7      NA       0.5        1    NA   0.9
      8      NA       0.5        1    NA   0.9
      9      NA       0.5        1    NA   0.9
      10     NA       0.5        1    NA   0.9
      11     NA       0.5        1    NA   0.9
      12     NA       0.5        1    NA   0.9
      13     NA       0.5        1    NA   0.9
      14     NA       0.5        1    NA   0.9
      15     NA       0.5        1    NA   0.9
      16     NA       0.5        1    NA   0.9
      17     NA       0.5        1    NA   0.9
      18     NA       0.5        1    NA   0.9
      19     NA       0.5        1    NA   0.9
      20     NA       0.5        1    NA   0.9
      21     NA       0.5        1    NA   0.9
      22     NA       0.5        1    NA   0.9
      23     NA       0.5        1    NA   0.9
      24     NA       0.5        1    NA   0.9
      25     NA       0.5        1    NA   0.9
      26     NA       0.5        1    NA   0.9
      27     NA       0.5        1    NA   0.9
      28     NA       0.5        1    NA   0.9
      29     NA       0.5        1    NA   0.9
      30     NA       0.5        1    NA   0.9
      31     NA       0.5        1    NA   0.9
      32     NA       0.5        1    NA   0.9
      33     NA       0.5        1    NA   0.9
      34     NA       0.5        1    NA   0.9
      35     NA       0.5        1    NA   0.9
      36     NA       0.5        1    NA   0.9
      37     NA       0.5        1    NA   0.9
      38     NA       0.5        1    NA   0.9
      39     NA       0.5        1    NA   0.9
      40     NA       0.5        1    NA   0.9
      41     NA       0.5        1    NA   0.9
      42     NA       0.5        1    NA   0.9
      43     NA       0.5        1    NA   0.9
      44     NA       0.5        1    NA   0.9
      45     NA       0.5        1    NA   0.9
      46     NA       0.5        1    NA   0.9
      47     NA       0.5        1    NA   0.9
      48     NA       0.5        1    NA   0.9
      49     NA       0.5        1    NA   0.9
      50     NA       0.5        1    NA   0.9
      51     NA       0.5        1    NA   0.9
      52     NA       0.5        1    NA   0.9

# draw_genes_barplot works as expected with custom options

    Code
      ggplot2::layer_data(result)
    Output
        count prop x flipped_aes    fill PANEL group   y ymin ymax xmin xmax colour
      1    88    1 1       FALSE #F8766D     1     1 254  166  254 0.55 1.45     NA
      2   120    1 2       FALSE #F8766D     1     2 297  177  297 1.55 2.45     NA
      3   166    1 1       FALSE #00BFC4     1     3 166    0  166 0.55 1.45     NA
      4   177    1 2       FALSE #00BFC4     1     4 177    0  177 1.55 2.45     NA
        linewidth linetype alpha width
      1       0.5        1    NA   0.9
      2       0.5        1    NA   0.9
      3       0.5        1    NA   0.9
      4       0.5        1    NA   0.9

