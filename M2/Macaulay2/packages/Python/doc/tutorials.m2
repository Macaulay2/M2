---------------
-- tutorials --
---------------

doc ///
  Key
    "Python tutorial: creating a virtual environment and installing NumPy"
  Description
    Text
      In this tutorial, we demonstrate how to create a Python virtual
      environment and install the @HREF("https://numpy.org", "numpy")@
      module within it. A virtual environment allows us to isolate
      Python packages from the system-wide installation, ensuring that
      dependencies do not interfere with other projects.

      @HEADER2 "Step 1: Create a Virtual Environment"@

      First, we choose a directory where we want to create our virtual
      environment. We then call @TO setupVirtualEnvironment@ with that
      directory. This command initializes a new virtual environment,
      which includes a dedicated Python interpreter and an isolated
      package installation directory.
    CannedExample
      i1 : needsPackage "Python";

      i2 : dir = temporaryFileName()

      o2 = /tmp/M2-2158442-1/0

      i3 : setupVirtualEnvironment dir
    Text
      @HEADER2 "Step 2: Reload the Python Package with the Virtual Environment"@

      Next we restart Macaulay2 so we can initialize the Python binary in
      the new virtual environment.
    Example
      i4 : restart
    Text
      To ensure that the Python interface uses the newly created
      virtual environment, we load the Python package while
      specifying the virtual environment’s Python interpreter. This is
      done by setting the "executable" configuration option to point
      to the @CODE "python3"@ binary inside our virtual environment.
    CannedExample
      i1 : loadPackage("Python", Configuration => {
               "executable" => "/tmp/M2-2158442-1/0/bin/python3"})

      o1 = Python

      o1 : Package
    Text
      At this point, all Python commands will use the virtual
      environment's interpreter rather than the system-wide Python
      installation.

      @HEADER2 "Step 3: Install NumPy"@

      Next, we install the NumPy module using @TO pipInstall@. This
      downloads and installs NumPy into the virtual environment.
    CannedExample
      i2 : pipInstall "numpy"
      Collecting numpy
        Downloading numpy-2.2.4-cp312-cp312-manylinux_2_17_x86_64.manylinux2014_x86_64.whl.metadata (62 kB)
           ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 62.0/62.0 kB 2.5 MB/s eta 0:00:00

      Downloading numpy-2.2.4-cp312-cp312-manylinux_2_17_x86_64.manylinux2014_x86_64.whl (16.1 MB)
         ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 16.1/16.1 MB 47.9 MB/s eta 0:00:00

      Installing collected packages: numpy
      Successfully installed numpy-2.2.4
    Text
      Once this step completes, NumPy is fully installed and ready for use.

      @HEADER2 "Step 4: Import and Use NumPy"@

      Now that NumPy is installed, we can import it and perform
      numerical computations.
    CannedExample
      i3 : np = import "numpy"

      o3 = <module 'numpy' from '/tmp/M2-2158442-1/0/lib/python3.12/site-packages/
           numpy/__init__.py'>

      o3 : PythonObject of class module
    Text
      To verify that NumPy is working correctly, we perform matrix multiplication using NumPy arrays:
    CannedExample
      i4 : np@@array {{1, 2}, {3, 4}} @ np@@array {{5, 6}, {7, 8}}

      o4 = [[19 22]
            [43 50]]

      o4 : PythonObject of class numpy.ndarray
///

doc ///
  Key
    "Python tutorial: matplotlib"
  Description
    Text
      In this tutorial, we use @HREF{"https://matplotlib.org/", "matplotlib"}@
      to plot the twisted cubic.

      First, we import the necessary modules using @TO import@.  Note that
      we can essentially replace the Python @CODE "import foo as bar"@ with
      @CODE "bar = import \"foo\""@.
    CannedExample
      i2 : plt = import "matplotlib.pyplot"

      o2 = <module 'matplotlib.pyplot' from
           '/usr/lib/python3/dist-packages/matplotlib/pyplot.py'>

      o2 : PythonObject of class module

      i3 : np = import "numpy"

      o3 = <module 'numpy' from '/usr/lib/python3/dist-packages/numpy/__init__.py'>

      o3 : PythonObject of class module
    Text
      Next, we begin to create the various Python objects needed for our
      plot.  This example is heavily inspired by the
      @HREF{"https://matplotlib.org/stable/gallery/mplot3d/lines3d.html",
	  "Parametric curve"}@ example in the matplotlib documentation.

      Note that we basically replace the Python @CODE "foo.bar"@ with
      @CODE "foo\x40\x40bar"@
      (see @TO (symbol \@\@, PythonObject, Thing)@).
      -- TODO: rework this -- we don't need so many quotes
      We need to be careful for attributes that include underscores.
      They must given as strings, i.e., delimited using quotes.  This
      is also the case for keyword arguments.
    CannedExample
      i4 : fig = plt@@figure()

      o4 = Figure(640x480)

      o4 : PythonObject of class matplotlib.figure.Figure

      i5 : ax = fig@@"add_subplot"("projection" => "3d")

      o5 = Axes3DSubplot(0.125,0.11;0.775x0.77)

      o5 : PythonObject of class matplotlib.axes._subplots.Axes3DSubplot

      i6 : t = np@@linspace(-10, 10, 100)

      o6 = [-10.          -9.7979798   -9.5959596   -9.39393939  -9.19191919
             -8.98989899  -8.78787879  -8.58585859  -8.38383838  -8.18181818
             -7.97979798  -7.77777778  -7.57575758  -7.37373737  -7.17171717
             -6.96969697  -6.76767677  -6.56565657  -6.36363636  -6.16161616
             -5.95959596  -5.75757576  -5.55555556  -5.35353535  -5.15151515
             -4.94949495  -4.74747475  -4.54545455  -4.34343434  -4.14141414
             -3.93939394  -3.73737374  -3.53535354  -3.33333333  -3.13131313
             -2.92929293  -2.72727273  -2.52525253  -2.32323232  -2.12121212
             -1.91919192  -1.71717172  -1.51515152  -1.31313131  -1.11111111
             -0.90909091  -0.70707071  -0.50505051  -0.3030303   -0.1010101
              0.1010101    0.3030303    0.50505051   0.70707071   0.90909091
              1.11111111   1.31313131   1.51515152   1.71717172   1.91919192
              2.12121212   2.32323232   2.52525253   2.72727273   2.92929293
              3.13131313   3.33333333   3.53535354   3.73737374   3.93939394
              4.14141414   4.34343434   4.54545455   4.74747475   4.94949495
              5.15151515   5.35353535   5.55555556   5.75757576   5.95959596
              6.16161616   6.36363636   6.56565657   6.76767677   6.96969697
              7.17171717   7.37373737   7.57575758   7.77777778   7.97979798
              8.18181818   8.38383838   8.58585859   8.78787879   8.98989899
              9.19191919   9.39393939   9.5959596    9.7979798   10.        ]

      o6 : PythonObject of class numpy.ndarray
    Text
      Now we construct the twisted cubic.  Note that even though Python itself
      uses @CODE "**"@ for exponentiation, we use @TO symbol ^@ for consistency
      with the rest of Macaulay2.
    CannedExample
      i7 : ax@@plot(t, t^2, t^3)

      o7 = [<mpl_toolkits.mplot3d.art3d.Line3D object at 0x78bdf578a000>]

      o7 : PythonObject of class list
    Text
      Finally, we show our plot.  It will appear in a separate window.
    CannedExample
      i8 : plt@@show()

      o8 = None

      o8 : PythonObject of class NoneType
    Text
      @IMG("src" => Python#"auxiliary files" | "twisted-cubic.png",
	  "alt" => "parametric plot of the twisted cubic")@
///

-- TODO: more tutorials!
