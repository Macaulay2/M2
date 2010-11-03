The use of "setup.exe" with signatures is explained here:

    http://www.cygwin.com/ml/cygwin-announce/2008-08/msg00001.html

The relevant part for custom mirrors is this:

    CUSTOM MIRRORS
    ==============

      Maintainers of standard mirrors of the upstream cygwin.com/sourceware.org
    public repository need take no action.  There will be no impact from these
    changes apart from the presence of the new .sig files alongside the existing
    setup.ini/setup.bz2 et. al.

      Maintainers of customised repositories will be impacted.  Read on for
    details and mitigation.

      Without taking action, the new version of setup.exe will refuse to install
    from your repositories when it fails to find a valid signature for your
    customised setup.ini files.  There are a number of option open to you and
    your users to deal with this situation.

      Unfortunately this is only the first release of this feature and currently
    requires the use of command-line options to modify the
    signature-verification behaviour; we apologise for the pressure of time and
    manpower resources that has not allowed us to develop more user-friendly
    features initially, and would like to work with package repository
    maintainers to improve the usability of future versions of setup.exe for
    them and their users.  Please direct suggestions for improved mechanisms,
    bug-reports, and (especially!) offers of help to the cygwin-apps list.


      This list summarizes the main possibilities, in decreasing order of
    worstness:

    1)  Tell your users that they must retain and use an old version of
    setup.exe to access your mirror.  This old version will not complain about
    the lack of signature files.

    2)  Tell your users to supply the new -X (--no-verify) command-line flag
    when using setup.exe to download from your mirror.  This can be added into
    the command-line invocation in a Windows shortcut, for convenience.

    3)  Start signing your custom-generated setup.ini and setup.bz2 files with
    gpg, and either

    -   i)  Convert your public key to s-expr format using the script
    gpg-key-to-s-expr.sh from the setup.exe sources[5] (requires an installation
    of pgpdump[6]), distribute it to your users, and ask them to specify it as
    the argument to the -S command-line option (can be done using a shortcut to
    save repetition).
    -  ii)  Convert your public key to s-expr format, distribute it to your
    users, and tell them either to use the -S option once to load it into the
    untrusted keys cache and the -U option subsequently.
    - iii)  Distribute your public key file to users in binary gpg format, and
    tell them to use the -K command-line option to point at it, either every
    time, or just initially to load it into the untrusted keys cache, followed
    by use of -U on subsequent occasions.
