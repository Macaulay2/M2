            Editing Macaulay 2 code with emacs

       -- this file has been automatically generated -- 
                      -- do not edit -- 


In this section we learn how to use emacs to edit Macaulay 2 code.  Assuming you
have set up your emacs init file as described in 'running Macaulay 2 in emacs'
when you visit a file whose name ends with '.m2' 
you will see on the mode line the name 'Macaulay 2' in
parentheses, indicating that the file is being edited in Macaulay 2 mode.

To see how electric parentheses, electric semicolons, and indentation work,
move to a blank line of this file and type the following text.

f = () -> (
a := 4;
b := {6,7};
a+b)

Observe carefully how matching left parentheses are indicated briefly when a
right parenthesis is typed.

Now position your cursor in between the 6 and 7.  Notice how
pressing 'M-C-u' moves you up out of the list to its left.  Do it 
again.  Experiment with 'M-C-f' and 'M-C-b' to move forward
and back over complete parenthesized
expressions.  (In the emacs manual a complete parenthesized expression is
referred to as an sexp, which is an abbreviation for S-expression.)  Try out
'C-U 2 M-C-@' as a way of marking the next two complete parenthesized
expression, and see how to use 'C-W' to kill them and 'C-Y' to yank 
them back.  Experiment with 'M-C-K' to kill the next complete parenthesized 
expression.

Position your cursor on the 4 and observe how 'M-;' will start a comment 
for you with two hyphens, and position the cursor at the point where commentary
may be entered.

Type 'res' somewhere and then press 'C-C TAB' to bring up the
possible completions of the word to documented Macaulay 2 symbols.

Notice how 'C-H m' will display the keystrokes peculiar to the mode in 
a help window.