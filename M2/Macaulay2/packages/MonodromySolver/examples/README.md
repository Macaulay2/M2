* Experiments

    * katsuraComparison.py
        * Exists to work around the memory consumption issue. Calls katsuraComparison.m2, which performs the katsura tests used for completion section.

    * comparison-template.m2
        * Used to test monodromySolve for cyclic, katsura, nash, and crn. Doesn't deal with memory issue, so timings may not be valid. Also means that it shouldn't be used for extensive experiments.

    * M2-test-wrapper.py
        * Poorly named, calls software-comparison.m2 which compares Bertini/PHCpack/MonodromySolve/HOM4PS2. Again, this python file exists to deal with the memory leak.

    * cyclic10.m2, cyclic11.m2, noon10.m2
        * Files that do software comparisons for single systems. Also has output from Anton's office machine

    * dynamicFlowerTest.m2
        * Used to get the average number of paths for dynamicFlowerTest for cyclic7.
        
    * cyclic-batchsize-test.m2
        * Test varying batchsizes with monodromySolve

* System Definitions
    * example-CRN.m2, example-Nash.m2, cyclic.m2, katsura.m2

* Alpha test application:
    * example-NashCertify.m2: certifying solutions for a a particular problem

* Possibly unnecessary
    * comparison-CRN, comparison-cyclic: both should have been superceded by comparison-template

