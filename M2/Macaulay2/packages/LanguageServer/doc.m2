doc ///
  Key
    LanguageServer
  Headline
    language server
  Description
    Text
      This package provides a language server for Macaulay2 implementing the
      @HREF("https://microsoft.github.io/language-server-protocol/",
          "Language Server Protocol (LSP)")@.
      It is used via the @CODE "M2-language-server"@ script, which is
      distributed with Macaulay2 and starts the server with standard input and
      output as the communication channel.

      The language server is supported out of the box by the following editors:
    Code
      UL {
          LI {"the Macaulay2 major mode for ",
              HREF("https://github.com/Macaulay2/M2-emacs", "Emacs"),
              ", using eglot or lsp-mode"},
          LI {"the Macaulay2 extension for ",
              HREF("https://github.com/Macaulay2/vscode-macaulay2", "VS Code")},
          LI {"the Macaulay2 plugin for ",
              HREF("https://github.com/Macaulean/macaulay2.nvim", "Neovim")}}
  Subnodes
    LSPServer
///

doc ///
  Key
     LSPServer
    (NewMethod, LSPServer)
  Headline
    LSP server class
  Usage
    new LSPServer
  Description
    Text
      @CODE "LSPServer"@ is the main class of the package.  It wraps a
      JSON-RPC server and handles the LSP lifecycle: initialize, text document
      synchronization, and shutdown.
    Example
      server = new LSPServer
  Subnodes
    start
    (setLogger, LSPServer, Function)
///

doc ///
  Key
     start
    (start, LSPServer)
  Headline
    start the language server
  Usage
    start server
  Inputs
    server:LSPServer
  Description
    Text
      Starts the language server, reading LSP messages from @TO stdio@ and
      writing responses back to @TO stdio@.  This is called automatically
      by the @CODE "M2-language-server"@ script and is not typically
      invoked directly by the user.
///

doc ///
  Key
    (setLogger, LSPServer, Function)
  Headline
    set up logging for an LSP server
  Usage
    setLogger(server, f)
  Inputs
    server:LSPServer
    f:Function
  Description
    Text
      Sets a custom logging function for @CODE "server"@.  The function
      @CODE "f"@ should accept a single string.  For example, to log
      to standard error:
    Example
      server = new LSPServer
      setLogger(server, printerr)
///
