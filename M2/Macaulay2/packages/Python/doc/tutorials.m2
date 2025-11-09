---------------
-- tutorials --
---------------

-* code for canned example

needsPackage "Python"
dir = applicationDirectory() | "venv"
setupVirtualEnvironment dir

restart

loadPackage("Python", Configuration => {
    "executable" => applicationDirectory() | "venv/bin/python3"})

pipInstall "numpy"

installNumPyMethods()
A = toPython matrix {{1, 2}, {3, 4}}
B = toPython matrix {{5, 6}, {7, 8}}
A @ B

*-

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

      i2 : dir = applicationDirectory() | "venv"

      o2 = /home/m2user/.Macaulay2/venv

      i3 : setupVirtualEnvironment dir
    Text
      @HEADER2 "Step 2: Reload the Python Package with the Virtual Environment"@

      Next we restart Macaulay2 so we can initialize the Python binary in
      the new virtual environment.
    CannedExample
      i4 : restart
    Text
      To ensure that the Python interface uses the newly created
      virtual environment, we load the Python package while
      specifying the virtual environment’s Python interpreter. This is
      done by setting the "executable" configuration option to point
      to the @CODE "python3"@ binary inside our virtual environment.
    CannedExample
      i1 : loadPackage("Python", Configuration => {
               "executable" => applicationDirectory() | "venv/bin/python3"})

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

      Now that NumPy is installed, we can call @TO installNumPyMethods@
      and begin using it.  Let's verify that it works by multiplying two
      matrices.
    CannedExample
      i3 : installNumPyMethods()

      o3 = <module 'numpy' from '/home/profzoom/.Macaulay2/venv/lib/python3.10/site-
           packages/numpy/__init__.py'>

      o3 : PythonObject of class module

      i4 : A = toPython matrix {{1, 2}, {3, 4}}

      o4 = [[1 2]
            [3 4]]

      o4 : PythonObject of class numpy.ndarray

      i5 : B = toPython matrix {{5, 6}, {7, 8}}

      o5 = [[5 6]
            [7 8]]

      o5 : PythonObject of class numpy.ndarray

      i6 : A @ B

      o6 = [[19 22]
            [43 50]]

      o6 : PythonObject of class numpy.ndarray
  SeeAlso
    setupVirtualEnvironment
    pipInstall
    installNumPyMethods
///

-* code for canned example

pycode = get(Python#"auxiliary files" | "doc/matplotlib-example.py")
pythonRunScript pycode

matplotlib = PythonContext "import matplotlib.pyplot as plt"
matplotlib "import numpy as np"
matplotlib "fig = plt.figure()"
matplotlib "ax = fig.add_subplot(projection='3d')"
matplotlib "t = np.linspace(-10, 10, 100)"
matplotlib "ax.plot(t, t**2, t**3)"
matplotlib "plt.show()"

plt = import "matplotlib.pyplot"
np = import "numpy"
fig = plt@@figure()
ax = fig@@"add_subplot"(projection => "3d")
t = np@@linspace(-10, 10, 100);
ax@@plot(t, t^2, t^3)
plt@@show()

*-

doc ///
  Key
    "Python tutorial: plotting the twisted cubic with Matplotlib"
  Description
    Text
      In this tutorial, we use @HREF{"https://matplotlib.org/", "Matplotlib"}@
      to plot the twisted cubic in three different ways.  We are assuming
      that Matplotlib is already installed.  See @TO pipInstall@ to learn
      how to install Python modules.

      This example is heavily inspired by the
      @HREF{"https://matplotlib.org/stable/gallery/mplot3d/lines3d.html",
	  "Parametric curve"}@ example in the matplotlib documentation.

      @HEADER2 "Running Python code directly"@

      If we just want to run some Python code directly, then the simplest
      way is with the @TO pythonRunScript@ method.

      For this example, we have our code saved as a .py file.
    CannedExample
      i1 : pycode = get(Python#"auxiliary files" | "doc/matplotlib-example.py")

      o1 = import matplotlib.pyplot as plt
           import numpy as np

           fig = plt.figure()
           ax = fig.add_subplot(projection='3d')
           t = np.linspace(-10, 10, 100)
           ax.plot(t, t**2, t**3)
           plt.show()


      i2 : pythonRunScript pycode
    Text
      @HEADER2 "Using a PythonContext"@

      @TO PythonContext@ objects allow us to run Python code one line at a
      time, just like working in the Python REPL.
    CannedExample
      i3 : matplotlib = PythonContext "import matplotlib.pyplot as plt"

      o3 = matplotlib

      o3 : PythonContext

      i4 : matplotlib "import numpy as np"

      i5 : matplotlib "fig = plt.figure()"

      i6 : matplotlib "ax = fig.add_subplot(projection='3d')"

      i7 : matplotlib "t = np.linspace(-10, 10, 100)"

      i8 : matplotlib "ax.plot(t, t**2, t**3)"

      o8 = [<mpl_toolkits.mplot3d.art3d.Line3D object at 0x7b8243b7e5f0>]

      o8 : PythonObject of class list

      i9 : matplotlib "plt.show()"

      o9 = None

      o9 : PythonObject of class NoneType
    Text
      @HEADER2 "Using PythonObjects directly"@

      Finally, we can write our code in the Macaulay2 language, but making
      calls to Python as needed.

      First, we import the necessary modules using @TO import@.  Note that
      we can essentially replace the Python @CODE "import foo as bar"@ with
      @CODE "bar = import \"foo\""@.
    CannedExample
      i10 : plt = import "matplotlib.pyplot"

      o10 = <module 'matplotlib.pyplot' from
            '/usr/lib/python3/dist-packages/matplotlib/pyplot.py'>

      o10 : PythonObject of class module

      i11 : np = import "numpy"

      o11 = <module 'numpy' from '/usr/lib/python3/dist-packages/numpy/__init__.py'>

      o11 : PythonObject of class module
    Text
      Next, we begin to create the various Python objects needed for our
      plot.

      Note that we replace the Python @CODE "foo.bar"@ with
      @CODE "foo\x40\x40bar"@
      (see @TO (symbol \@\@, PythonObject, Thing)@).
      We need to be careful for attributes that include underscores.
      They must given as strings, i.e., delimited using quotes.
    CannedExample
      i12 : fig = plt@@figure()

      o12 = Figure(640x480)

      o12 : PythonObject of class matplotlib.figure.Figure

      i13 : ax = fig@@"add_subplot"(projection => "3d")

      o13 = Axes3DSubplot(0.125,0.11;0.775x0.77)

      o13 : PythonObject of class matplotlib.axes._subplots.Axes3DSubplot

      i14 : t = np@@linspace(-10, 10, 100);
    Text
      Now we construct the twisted cubic.  Note that even though Python itself
      uses @CODE "**"@ for exponentiation, we may use @TO symbol ^@ for
      consistency with the rest of Macaulay2.
    CannedExample
      i15 : ax@@plot(t, t^2, t^3)

      o15 = [<mpl_toolkits.mplot3d.art3d.Line3D object at 0x73a51970eb00>]

      o15 : PythonObject of class list
    Text
      Finally, we show our plot.
    CannedExample
      i16 : plt@@show()
    Text
      All three of the above methods should have resulted in a window
      appearing containing the following image.

      @IMG("src" => replace("PKG", "Python",
	      currentLayout#"package" | "doc/twisted-cubic.png"),
	  "alt" => "parametric plot of the twisted cubic")@
///

-- TODO: more tutorials!
