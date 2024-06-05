package org.openmole.spatialdata.application.urbanmorphology

import org.openmole.spatialdata.grid.real.OSMGridGenerator
import org.openmole.spatialdata.utils.io.PNG

import scala.util.Random

object RunGenerator {


  def runOutputPng() = {


    /**
      * Centroid 1
      */

    // Synthetic
    /*
     "1 pc1 = -0.401568005874794 ; pc2 = -0.139034657366536"
[1] "generator = blocks"
[1] " min = 0.00135782942380691"
           moran avgDistance density components avgDetour avgBlockArea avgComponentArea fullDilationSteps
124373 0.1602641    1.041462  0.2832         97  9.109906           26               69                16
       fullErosionSteps lon lat generator blocksMaxSize blocksMinSize blocksNumber expMixtureCenters
124373                5   0   0    blocks      4.702276      12.96444      12.4645          4.006979
       expMixtureRadius expMixtureThreshold percolationBordPoints percolationLinkWidth percolationProba
124373         10.77812           0.1704082               34.0386             2.610625        0.3449138
       randomDensity replication
124373     0.3793886 -1577939060
     */
    //PNG.write(GridGeneratorLauncher("blocks",50,0.0,1,1.0,1.0,12,13,5,0.0,1,1.0).getGrid(new Random(-1577939060)), File("data") / "res" / "examples"/ "closest1_blocks.png")

    // Real
    /*
     " min = 0.00430747545506705"
           lon      lat
      2.434169 48.54942 ;  -2.450622 53.19037 ; 19.007137 49.91587
     */
    //PNG.write(OSMGridGenerator(19.007137,49.91587,500,50).generateGrid(new Random(0)), File("data") / "res" / "examples"/ "closest1_real.png")


    /**
      * Centroid 2
      * "2 pc1 = 0.307899227735927 ; pc2 = -0.357540683059979"
      */

    /*
    "generator = expMixture"
[1] " min = 0.00189547404428551"
          moran avgDistance density components avgDetour avgBlockArea avgComponentArea fullDilationSteps
192135 0.147895   0.4852531  0.0808         31  6.239738           23              116                43
       fullErosionSteps lon lat  generator blocksMaxSize blocksMinSize blocksNumber expMixtureCenters
192135                6   0   0 expMixture      10.48669      8.777851     8.426846          6.250709
       expMixtureRadius expMixtureThreshold percolationBordPoints percolationLinkWidth percolationProba
192135           3.8163           0.4153495              33.95562             3.383109         0.750464
       randomDensity replication
192135     0.9683222 -1315169452
     */
    //PNG.write(GridGeneratorLauncher("expMixture",50,0.0,6,3.8163,0.4153495,12,13,5,0.0,1,1.0).getGrid(new Random(-1315169452)), File("data") / "res" / "examples"/ "closest2_expMixture.png")

    //real
    /*
     min = 0.00525322533870854"
          lon      lat
8985 10.63513 51.16891
     */
    //PNG.write(OSMGridGenerator(10.63513,51.16891,500,50).generateGrid(new Random(0)), File("data") / "res" / "examples"/ "closest2_real.png")


    /**
      * Centroid 3
      *
      *pc1 = -0.00169022591544116 ; pc2 = -0.0933680483743268"
      *
      */

    // synthetic
    /*
    "generator = blocks"
[1] " min = 0.000549280232203691"
           moran avgDistance density components avgDetour avgBlockArea avgComponentArea fullDilationSteps
534243 0.1078281   0.7620847  0.1156         43  5.329797           22               95                21
       fullErosionSteps lon lat generator blocksMaxSize blocksMinSize blocksNumber expMixtureCenters
534243                4   0   0    blocks      14.27284      1.245443     10.87651          5.311592
       expMixtureRadius expMixtureThreshold percolationBordPoints percolationLinkWidth percolationProba
534243         7.604864           0.8362122              45.05266             1.508391        0.5563269
       randomDensity replication
534243     0.1159083  -342078397
     */
    //PNG.write(GridGeneratorLauncher("blocks",50,0.0,1,1.0,1.0,11,1,14,0.0,1,1.0).getGrid(new Random(-342078397)), File("data") / "res" / "examples"/ "closest3_blocks.png")


    // real
    /*
    min = 0.00514020745950694"
      * lon      lat
      * 836 2.789109 50.54815 ; 24.796939 59.42104
     */
    //PNG.write(OSMGridGenerator(24.796939,59.42104,500,50).generateGrid(new Random(0)), File("data") / "res" / "examples"/ "closest3_real.png")


    /**
      * Centroid 4
      *
      * pc1 = -0.773530217615314 ; pc2 = -0.294339386091329"
      *
      */


    /*
       [1] "generator = percolation"
[1] " min = 0.00295658748459472"
            moran avgDistance   density components avgDetour avgBlockArea avgComponentArea fullDilationSteps
447144 0.07123989   0.9909884 0.4971165        147  14.74765           28               35                 4
521692 0.07123989   0.9909884 0.4971165        147  14.74765           28               35                 4
       fullErosionSteps lon lat   generator blocksMaxSize blocksMinSize blocksNumber expMixtureCenters
447144                4   0   0 percolation     18.633342      8.358712    12.829109          3.997188
521692                4   0   0 percolation      9.800127     18.158160     7.525642          6.728917
       expMixtureRadius expMixtureThreshold percolationBordPoints percolationLinkWidth percolationProba
447144         18.48754          0.46921320             15.481693             3.514229        0.3584211
521692         13.44539          0.08324059              5.455781             3.709316        0.3483628
       randomDensity replication
447144     0.5892340   -43871746
521692     0.1219495   -43871746
     */
    //PNG.write(GridGeneratorLauncher("percolation",50,0.0,1,1.0,1.0,11,1,14,0.3483628,5,3.709316).getGrid(new Random(-43871746)), File("data") / "res" / "examples"/ "closest4_percolation.png")
    //PNG.write(GridGeneratorLauncher("percolation",50,0.0,1,1.0,1.0,11,1,14,0.3584211,15,3.514229).getGrid(new Random(-43871746)), File("data") / "res" / "examples"/ "closest4_percolation.png")


    // real
    /*
    min = 0.00225664725672657"
    lon      lat
      12174 5.289788 52.06052 ; 24.373365 44.43003 ; 11.874241 43.46805 ; 25.254261 54.67903 ; 17.111480 40.58929
      ->  11.874241 43.46805
    */
    PNG.write(OSMGridGenerator(11.874241,43.46805,500,50).generateGrid(new Random(0)), new java.io.File("data/res/examples/closest4_real.png"))





  }


}
