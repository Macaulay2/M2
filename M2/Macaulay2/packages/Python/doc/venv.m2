-* code for canned examples
dir = applicationDirectory() | "venv"
setupVirtualEnvironment dir

loadPackage("Python", Configuration => {
	"executable" => applicationDirectory() | "venv/bin/python3"})
pipInstall "matplotlib"
import "matplotlib"
*-

doc ///
  Key
     setupVirtualEnvironment
    (setupVirtualEnvironment, String)
  Headline
    set up a virtual environment
  Usage
    setupVirtualEnvironment dir
  Inputs
    dir:String
  Description
    Text
      This sets up a Python virtual environment.  It is equivalent to calling
      @CODE "python3 -m venv dir"@ on the command line.
    CannedExample
      i1 : dir = applicationDirectory() | "venv"

      o1 = /home/profzoom/.Macaulay2/venv

      i2 : setupVirtualEnvironment dir
  Caveat
    To use the virtual environment, you must restart Macaulay2 and
    then load the Python package with the "executable" configuration
    option set to point to the Python executable in @VAR "dir"@.
  SeeAlso
    pipInstall
    "Python tutorial: creating a virtual environment and installing NumPy"
///

doc ///
  Key
     pipInstall
    (pipInstall, String)
  Headline
    install a Python package
  Usage
    pipInstall pkg
  Inputs
    pkg:String
  Description
    Text
      This method installs a Python package.  It is equivalent to calling
      @CODE "pip install pkg"@ on the command line.

      You likely want to create a virtual environment first.  See
      @TO setupVirtualEnvironment@.

      Let's suppose that one exists in
      @M2CODE "applicationDirectory() | \"venv\""@.
    CannedExample
      i1 : loadPackage("Python", Configuration => {
                "executable" => applicationDirectory() | "venv/bin/python3"})

      o1 = Python

      o1 : Package

      i2 : pipInstall "matplotlib"
      Collecting matplotlib
        Using cached matplotlib-3.10.7-cp310-cp310-manylinux2014_x86_64.manylinux_2_17_x86_64.whl (8.7 MB)
      Collecting fonttools>=4.22.0
        Using cached fonttools-4.60.1-cp310-cp310-manylinux2014_x86_64.manylinux_2_17_x86_64.whl (4.8 MB)
      Collecting packaging>=20.0
        Using cached packaging-25.0-py3-none-any.whl (66 kB)
      Collecting python-dateutil>=2.7
        Using cached python_dateutil-2.9.0.post0-py2.py3-none-any.whl (229 kB)
      Collecting cycler>=0.10
        Using cached cycler-0.12.1-py3-none-any.whl (8.3 kB)
      Collecting kiwisolver>=1.3.1
        Using cached kiwisolver-1.4.9-cp310-cp310-manylinux_2_12_x86_64.manylinux2010_x86_64.whl (1.6 MB)
      Collecting pyparsing>=3
        Using cached pyparsing-3.2.5-py3-none-any.whl (113 kB)
      Collecting contourpy>=1.0.1
        Using cached contourpy-1.3.2-cp310-cp310-manylinux_2_17_x86_64.manylinux2014_x86_64.whl (325 kB)
      Collecting numpy>=1.23
        Using cached numpy-2.2.6-cp310-cp310-manylinux_2_17_x86_64.manylinux2014_x86_64.whl (16.8 MB)
      Collecting pillow>=8
        Using cached pillow-12.0.0-cp310-cp310-manylinux_2_27_x86_64.manylinux_2_28_x86_64.whl (7.0 MB)
      Collecting six>=1.5
        Using cached six-1.17.0-py2.py3-none-any.whl (11 kB)
      Installing collected packages: six, pyparsing, pillow, packaging, numpy, kiwisolver, fonttools, cycler, python-dateutil, contourpy, matplotlib
      Successfully installed contourpy-1.3.2 cycler-0.12.1 fonttools-4.60.1 kiwisolver-1.4.9 matplotlib-3.10.7 numpy-2.2.6 packaging-25.0 pillow-12.0.0 pyparsing-3.2.5 python-dateutil-2.9.0.post0 six-1.17.0

      i3 : import "matplotlib"

      o3 = <module 'matplotlib' from
           '/home/m2user/.Macaulay2/venv/lib/python3.10/site-packages/matplotlib/__
           init__.py'>

      o3 : PythonObject of class module
  SeeAlso
    setupVirtualEnvironment
    import
    "Python tutorial: creating a virtual environment and installing NumPy"
///
