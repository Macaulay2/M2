doc ///
    Key
        "assertLevel"
    Headline
        controls whether constructors check for well-definedness
    Description
        Text
            This global variable controls whether the @TO("makeCpMackeyFunctor")@ and @TO("map(CpMackeyFunctor,CpMackeyFunctor,Matrix,Matrix)")@ methods check whether the input data yields a well-defined object. If @TO("assertLevel")@ is set to 0 or lower, then no checks are made, and the methods will return a Mackey functor or homomorphism (which may be malformed) regardless of the input data. If @TO("assertLevel")@ is set to 1 or higher, then the methods will check whether the input data yields a well-defined Mackey functor or homomorphism, and if not, they will throw an error.

            The default value is 1.
///