doc ///
Node
  Key
    "using Macaulay2 with Emacs"
    "setting up the Macaulay2 Emacs interface"
    "teaching Emacs how to find M2-init.el"
    "teaching Emacs how to find M2"
    "running Macaulay2 in Emacs"
    "using Macaulay2 with Emacs after it has been set up"
    "editing Macaulay2 code with Emacs"
  Description
    Text
      The Emacs interface is maintained and installed separately from Macaulay2.
      See @HREF {"https://github.com/Macaulay2/M2-emacs#installation", "M2-emacs installation and usage instructions"}@.
      Install it through an Emacs package manager; @TO setupEmacs@ is deprecated and
      no longer edits initialization files. In Emacs, @TT "M-x M2"@ starts a session
      and @TT "M-x M2-help"@ opens the bundled guide. Configure @TT "M2-exe"@ for a
      local executable or @TT "M2-command"@ for an SSH or container command.


Node
  Key
    "using Macaulay2 with TeXmacs"
  Description
    Text
      TeXmacs is a free visual text editor for mathematics that can be used to produce $\TeX$ output.
      It also supports interactive use with various symbolic algebra programs, such as Macaulay2.
      TeXmacs is available from @HREF{"https://www.texmacs.org", "TeXmacs.org"}@.
    Text
      Using TeXmacs as an interface to Macaulay2 is described in the TeXmacs
      @HREF{"http://www.texmacs.org/tmweb/manual/webman-interface.en.html", "online manual"}@.
      The basic procedure is to pull down the @KBD "Insert"@ menu, select @KBD "Session"@ from it,
      then select @KBD "Macaulay2"@ from the resulting submenu. Alternatively, you can click on
      the icon that looks like a computer and select @KBD "Macaulay2"@.
    Text
      The interface is implemented by a special top level mode in Macaulay2
      that causes the output to be converted to @TO mathML@, see @TO TeXmacs@.
      Note that MathML conversion methods have not been implemented yet for all data types.
///
