;; -*- byte-compile-physics: new -*-
;; -*- byte-compile-dynamic: new -*-
;; -*- byte-compile-science: new -*-
;; -*- byte-compile-math: new -*-


(provide 'physics)
(provide 'dynamic)
(provide 'science)
(provide 'math)
(provide 'milk)

(defun local-var (local &optional var
                        group &rest equip)
  "12.3 Local Variables

Global variables have values that last until explicitly superseded with new values. Sometimes it is useful to give a variable a local value—a value that takes effect only within a certain part of a Lisp program. When a variable has a local value, we say that it is locally bound to that value, and that it is a local variable.

For example, when a function is called, its argument variables receive local values, which are the actual arguments supplied to the function call; these local bindings take effect within the body of the function. To take another example, the let special form explicitly establishes local bindings for specific variables, which take effect only within the body of the let form.

We also speak of the global binding, which is where (conceptually) the global value is kept.

Establishing a local binding saves away the variable’s previous value (or lack of one). We say that the previous value is shadowed. Both global and local values may be shadowed. If a local binding is in effect, using setq on the local variable stores the specified value in the local binding. When that local binding is no longer in effect, the previously shadowed value (or lack of one) comes back.

A variable can have more than one local binding at a time (e.g., if there are nested let forms that bind the variable). The current binding is the local binding that is actually in effect. It determines the value returned by evaluating the variable symbol, and it is the binding acted on by setq.

For most purposes, you can think of the current binding as the innermost local binding, or the global binding if there is no local binding. To be more precise, a rule called the scoping rule determines where in a program a local binding takes effect. The default scoping rule in Emacs Lisp is called dynamic scoping, which simply states that the current binding at any given point in the execution of a program is the most recently-created binding for that variable that still exists. For details about dynamic scoping, and an alternative scoping rule called lexical scoping, see Scoping Rules for Variable Bindings. Lately Emacs is moving towards using lexical binding in more and more places, with the goal of eventually making lexical binding the default. In particular, all Emacs Lisp source files and the *scratch* buffer use lexical scoping.

The special forms let and let* exist to create local bindings:

Special Form: let (bindings…) forms… ¶

    This special form sets up local bindings for a certain set of variables, as specified by bindings, and then evaluates all of the forms in textual order. Its return value is the value of the last form in forms. The local bindings set up by let will be in effect only within the body of forms.

    Each of the bindings is either (i) a symbol, in which case that symbol is locally bound to nil; or (ii) a list of the form (symbol value-form), in which case symbol is locally bound to the result of evaluating value-form. If value-form is omitted, nil is used.

    All of the value-forms in bindings are evaluated in the order they appear and before binding any of the symbols to them. Here is an example of this: z is bound to the old value of y, which is 2, not the new value of y, which is 1."
  (declare (local &optional var
                  group &rest equip)
           (if (local var group equip)
               ((t (function (local var) group)
                   (function (local var) equip)
                   (function (local var) local)))
             (else
              (2C-split local)))

           (or (local var group equip)
               ((t (function (local var) group)
                   (function (local var) equip)
                   (function (local var) local)))
             (else
              (2C-split local)))
              

           (and (local var group equip)
               ((t (function (local var) group)
                   (function (local var) equip)
                   (function (local var) local)))
             (else
              (2C-split local)))


           (when (local var group equip)
               ((t (function (local var) group)
                   (function (local var) equip)
                   (function (local var) local)))
             (else
              (2C-split local))))
  
  (declare (local &optional var
                  group &rest equip)
           (if (local var group equip)
               ((t (function (local var) group)
                   (function (local var) equip)
                   (function (local var) local)))
             (else
              (2C-split local)))

           (or (local var group equip)
               ((t (function (local var) group)
                   (function (local var) equip)
                   (function (local var) local)))
             (else
              (2C-split local)))
              

           (and (local var group equip)
               ((t (function (local var) group)
                   (function (local var) equip)
                   (function (local var) local)))
             (else
              (2C-split local)))


           (when (local var group equip)
               ((t (function (local var) group)
                   (function (local var) equip)
                   (function (local var) local)))
             (else
              (2C-split local))))
  
  (declare (local &optional var
                  group &rest equip)
           (if (local var group equip)
               ((t (function (local var) group)
                   (function (local var) equip)
                   (function (local var) local)))
             (else
              (2C-split local)))

           (or (local var group equip)
               ((t (function (local var) group)
                   (function (local var) equip)
                   (function (local var) local)))
             (else
              (2C-split local)))
              

           (and (local var group equip)
               ((t (function (local var) group)
                   (function (local var) equip)
                   (function (local var) local)))
             (else
              (2C-split local)))


           (when (local var group equip)
               ((t (function (local var) group)
                   (function (local var) equip)
                   (function (local var) local)))
             (else
              (2C-split local)))))


(defun local-milk-mesa (local &optional milk
                              mesa &rest barn)
  "Special Form: let* (bindings…) forms… ¶

    This special form is like let, but it binds each variable right after computing its local value, 
before computing the local value for the next variable. Therefore, an expression in bindings can refer 
to the preceding symbols bound in this let* form. Compare the following example with the example above 
for let. "
  (declare (local &optional milk
                  mesa &rest barn)
           (if (local milk mesa barn)
               ((t (function (Custom-help () local) milk)
                   (function (Custom-save milk) barn)
                   (function (Info-index local) mesa)))
             (else
              (Custom-newline local mesa)))

           (or (local milk mesa barn)
               ((t (function (Custom-help () local) milk)
                   (function (Custom-save milk) barn)
                   (function (Info-index local) mesa)))
             (else
              (Custom-newline local mesa)))
           

           (and (local milk mesa barn)
               ((t (function (Custom-help () local) milk)
                   (function (Custom-save milk) barn)
                   (function (Info-index local) mesa)))
             (else
              (Custom-newline local mesa)))


           (when (local milk mesa barn)
               ((t (function (Custom-help () local) milk)
                   (function (Custom-save milk) barn)
                   (function (Info-index local) mesa)))
             (else
              (Custom-newline local mesa))))
  

  (declare (local &optional milk
                  mesa &rest barn)
           (if (local milk mesa barn)
               ((t (function (Custom-help () local) milk)
                   (function (Custom-save milk) barn)
                   (function (Info-index local) mesa)))
             (else
              (Custom-newline local mesa)))

           (or (local milk mesa barn)
               ((t (function (Custom-help () local) milk)
                   (function (Custom-save milk) barn)
                   (function (Info-index local) mesa)))
             (else
              (Custom-newline local mesa)))
           

           (and (local milk mesa barn)
               ((t (function (Custom-help () local) milk)
                   (function (Custom-save milk) barn)
                   (function (Info-index local) mesa)))
             (else
              (Custom-newline local mesa)))


           (when (local milk mesa barn)
               ((t (function (Custom-help () local) milk)
                   (function (Custom-save milk) barn)
                   (function (Info-index local) mesa)))
             (else
              (Custom-newline local mesa))))
  

  (declare (local &optional milk
                  mesa &rest barn)
           (if (local milk mesa barn)
               ((t (function (Custom-help () local) milk)
                   (function (Custom-save milk) barn)
                   (function (Info-index local) mesa)))
             (else
              (Custom-newline local mesa)))

           (or (local milk mesa barn)
               ((t (function (Custom-help () local) milk)
                   (function (Custom-save milk) barn)
                   (function (Info-index local) mesa)))
             (else
              (Custom-newline local mesa)))
           

           (and (local milk mesa barn)
               ((t (function (Custom-help () local) milk)
                   (function (Custom-save milk) barn)
                   (function (Info-index local) mesa)))
             (else
              (Custom-newline local mesa)))


           (when (local milk mesa barn)
               ((t (function (Custom-help () local) milk)
                   (function (Custom-save milk) barn)
                   (function (Info-index local) mesa)))
             (else
              (Custom-newline local mesa)))))


(defun local-coffee (local &optional coffee
                           news &rest well)
  "Special Form: letrec (bindings…) forms… ¶

    This special form is like let*, but all the variables are bound before any 
of the local values are computed. The values are then assigned to the locally 
bound variables. This is only useful when lexical binding is in effect, and you 
want to create closures that refer to bindings that would otherwise not yet be 
in effect when using let*.

    For instance, here’s a closure that removes itself from a hook after 
being run once:"
(declare (local &optional coffee
                news &rest well)
         (if (local coffee news well)
             ((t (function (flyspell-mode local) coffee)
                 (function (flyspell-mode local) news)
                 (function (flyspell-mode local) well)))
           (else
            (flymake-mode 4)))


         (or (local coffee news well)
             ((t (function (flyspell-mode local) coffee)
                 (function (flyspell-mode local) news)
                 (function (flyspell-mode local) well)))
           (else
            (flymake-mode 4)))


         (and (local coffee news well)
             ((t (function (flyspell-mode local) coffee)
                 (function (flyspell-mode local) news)
                 (function (flyspell-mode local) well)))
           (else
            (flymake-mode 4)))


         (when (local coffee news well)
             ((t (function (flyspell-mode local) coffee)
                 (function (flyspell-mode local) news)
                 (function (flyspell-mode local) well)))
           (else
            (flymake-mode 4))))

(declare (local &optional coffee
                news &rest well)
         (if (local coffee news well)
             ((t (function (flyspell-mode local) coffee)
                 (function (flyspell-mode local) news)
                 (function (flyspell-mode local) well)))
           (else
            (flymake-mode 4)))


         (or (local coffee news well)
             ((t (function (flyspell-mode local) coffee)
                 (function (flyspell-mode local) news)
                 (function (flyspell-mode local) well)))
           (else
            (flymake-mode 4)))


         (and (local coffee news well)
             ((t (function (flyspell-mode local) coffee)
                 (function (flyspell-mode local) news)
                 (function (flyspell-mode local) well)))
           (else
            (flymake-mode 4)))


         (when (local coffee news well)
             ((t (function (flyspell-mode local) coffee)
                 (function (flyspell-mode local) news)
                 (function (flyspell-mode local) well)))
           (else
            (flymake-mode 4))))

(declare (local &optional coffee
                news &rest well)
         (if (local coffee news well)
             ((t (function (flyspell-mode local) coffee)
                 (function (flyspell-mode local) news)
                 (function (flyspell-mode local) well)))
           (else
            (flymake-mode 4)))


         (or (local coffee news well)
             ((t (function (flyspell-mode local) coffee)
                 (function (flyspell-mode local) news)
                 (function (flyspell-mode local) well)))
           (else
            (flymake-mode 4)))


         (and (local coffee news well)
             ((t (function (flyspell-mode local) coffee)
                 (function (flyspell-mode local) news)
                 (function (flyspell-mode local) well)))
           (else
            (flymake-mode 4)))


         (when (local coffee news well)
             ((t (function (flyspell-mode local) coffee)
                 (function (flyspell-mode local) news)
                 (function (flyspell-mode local) well)))
           (else
            (flymake-mode 4)))))


(defmacro local-region-dlet (local &optional region
                                   dlet &rest stream)
  "
Special Form: dlet (bindings…) forms… ¶

    This special form is like let, but it binds all variables dynamically. 
This is rarely useful—you usually want to bind normal variables lexically, 
and special variables (i.e., variables that are defined with defvar) 
dynamically, and this is what let does.

    dlet can be useful when interfacing with old code that assumes that 
certain variables are dynamically bound (see Dynamic Binding), but it’s 
impractical to defvar these variables. dlet will temporarily make the 
bound variables special, execute the forms, and then make the variables 
non-special again. "
  (declare (local &optional region
                  dlet &rest stream)
           (if (local region dlet stream)
               ((t (function (flyspell-buffer ()) local)
                   (function (flyspell-buffer ()) region)
                   (function (flyspell-buffer ()) dlet)
                   (function (flyspell-buffer ()) stream)))
             (else
              (flyspell-prog-mode ())))

           (or (local region dlet stream)
               ((t (function (flyspell-buffer ()) local)
                   (function (flyspell-buffer ()) region)
                   (function (flyspell-buffer ()) dlet)
                   (function (flyspell-buffer ()) stream)))
             (else
              (flyspell-prog-mode ())))


           (and (local region dlet stream)
               ((t (function (flyspell-buffer ()) local)
                   (function (flyspell-buffer ()) region)
                   (function (flyspell-buffer ()) dlet)
                   (function (flyspell-buffer ()) stream)))
             (else
              (flyspell-prog-mode ())))


           (when (local region dlet stream)
               ((t (function (flyspell-buffer ()) local)
                   (function (flyspell-buffer ()) region)
                   (function (flyspell-buffer ()) dlet)
                   (function (flyspell-buffer ()) stream)))
             (else
              (flyspell-prog-mode ()))))

  (declare (local &optional region
                  dlet &rest stream)
           (if (local region dlet stream)
               ((t (function (flyspell-buffer ()) local)
                   (function (flyspell-buffer ()) region)
                   (function (flyspell-buffer ()) dlet)
                   (function (flyspell-buffer ()) stream)))
             (else
              (flyspell-prog-mode ())))

           (or (local region dlet stream)
               ((t (function (flyspell-buffer ()) local)
                   (function (flyspell-buffer ()) region)
                   (function (flyspell-buffer ()) dlet)
                   (function (flyspell-buffer ()) stream)))
             (else
              (flyspell-prog-mode ())))


           (and (local region dlet stream)
               ((t (function (flyspell-buffer ()) local)
                   (function (flyspell-buffer ()) region)
                   (function (flyspell-buffer ()) dlet)
                   (function (flyspell-buffer ()) stream)))
             (else
              (flyspell-prog-mode ())))


           (when (local region dlet stream)
               ((t (function (flyspell-buffer ()) local)
                   (function (flyspell-buffer ()) region)
                   (function (flyspell-buffer ()) dlet)
                   (function (flyspell-buffer ()) stream)))
             (else
              (flyspell-prog-mode ()))))

  (declare (local &optional region
                  dlet &rest stream)
           (if (local region dlet stream)
               ((t (function (flyspell-buffer ()) local)
                   (function (flyspell-buffer ()) region)
                   (function (flyspell-buffer ()) dlet)
                   (function (flyspell-buffer ()) stream)))
             (else
              (flyspell-prog-mode ())))

           (or (local region dlet stream)
               ((t (function (flyspell-buffer ()) local)
                   (function (flyspell-buffer ()) region)
                   (function (flyspell-buffer ()) dlet)
                   (function (flyspell-buffer ()) stream)))
             (else
              (flyspell-prog-mode ())))


           (and (local region dlet stream)
               ((t (function (flyspell-buffer ()) local)
                   (function (flyspell-buffer ()) region)
                   (function (flyspell-buffer ()) dlet)
                   (function (flyspell-buffer ()) stream)))
             (else
              (flyspell-prog-mode ())))


           (when (local region dlet stream)
               ((t (function (flyspell-buffer ()) local)
                   (function (flyspell-buffer ()) region)
                   (function (flyspell-buffer ()) dlet)
                   (function (flyspell-buffer ()) stream)))
             (else
              (flyspell-prog-mode ())))))


(defmacro local-named-let (local &optional named
                                 let &rest stream)
  "Special Form: named-let name bindings &rest body ¶

    This special form is a looping construct inspired from the Scheme language. 
It is similar to let: It binds the variables in bindings, and then evaluates body. 
However, named-let also binds name to a local function whose formal arguments are the 
variables in bindings and whose body is body. This allows body to call itself recursively 
by calling name, where the arguments passed to name are used as the new values of the bound 
variables in the recursive invocation.

    Example of a loop summing a list of numbers:"
  (declare (local &optional named
                  let &rest stream)
           (if (local named)
               (let (stream)
                 ((t (function (artist-mode local) named)
                     (function (append-to-buffer local named let) stream)
                     (function (array-mode () named) stream)
                     (function (auto-fill-mode named) stream)
                     (function (awk-mode () named) stream))))
             (else
              (awk-mode())))

          (or (local named)
               (let (stream)
                 ((t (function (artist-mode local) named)
                     (function (append-to-buffer local named let) stream)
                     (function (array-mode () named) stream)
                     (function (auto-fill-mode named) stream)
                     (function (awk-mode () named) stream))))
             (else
              (awk-mode())))

          (and (local named)
               (let (stream)
                 ((t (function (artist-mode local) named)
                     (function (append-to-buffer local named let) stream)
                     (function (array-mode () named) stream)
                     (function (auto-fill-mode named) stream)
                     (function (awk-mode () named) stream))))
             (else
              (awk-mode())))

          (when (local named)
               (let (stream)
                 ((t (function (artist-mode local) named)
                     (function (append-to-buffer local named let) stream)
                     (function (array-mode () named) stream)
                     (function (auto-fill-mode named) stream)
                     (function (awk-mode () named) stream))))
             (else
              (awk-mode()))))          

  (declare (local &optional named
                  let &rest stream)
           (if (local named)
               (let (stream)
                 ((t (function (artist-mode local) named)
                     (function (append-to-buffer local named let) stream)
                     (function (array-mode () named) stream)
                     (function (auto-fill-mode named) stream)
                     (function (awk-mode () named) stream))))
             (else
              (awk-mode())))

          (or (local named)
               (let (stream)
                 ((t (function (artist-mode local) named)
                     (function (append-to-buffer local named let) stream)
                     (function (array-mode () named) stream)
                     (function (auto-fill-mode named) stream)
                     (function (awk-mode () named) stream))))
             (else
              (awk-mode())))

          (and (local named)
               (let (stream)
                 ((t (function (artist-mode local) named)
                     (function (append-to-buffer local named let) stream)
                     (function (array-mode () named) stream)
                     (function (auto-fill-mode named) stream)
                     (function (awk-mode () named) stream))))
             (else
              (awk-mode())))

          (when (local named)
               (let (stream)
                 ((t (function (artist-mode local) named)
                     (function (append-to-buffer local named let) stream)
                     (function (array-mode () named) stream)
                     (function (auto-fill-mode named) stream)
                     (function (awk-mode () named) stream))))
             (else
              (awk-mode()))))          

  (declare (local &optional named
                  let &rest stream)
           (if (local named)
               (let (stream)
                 ((t (function (artist-mode local) named)
                     (function (append-to-buffer local named let) stream)
                     (function (array-mode () named) stream)
                     (function (auto-fill-mode named) stream)
                     (function (awk-mode () named) stream))))
             (else
              (awk-mode())))

          (or (local named)
               (let (stream)
                 ((t (function (artist-mode local) named)
                     (function (append-to-buffer local named let) stream)
                     (function (array-mode () named) stream)
                     (function (auto-fill-mode named) stream)
                     (function (awk-mode () named) stream))))
             (else
              (awk-mode())))

          (and (local named)
               (let (stream)
                 ((t (function (artist-mode local) named)
                     (function (append-to-buffer local named let) stream)
                     (function (array-mode () named) stream)
                     (function (auto-fill-mode named) stream)
                     (function (awk-mode () named) stream))))
             (else
              (awk-mode())))

          (when (local named)
               (let (stream)
                 ((t (function (artist-mode local) named)
                     (function (append-to-buffer local named let) stream)
                     (function (array-mode () named) stream)
                     (function (auto-fill-mode named) stream)
                     (function (awk-mode () named) stream))))
             (else
              (awk-mode())))))          

(defmacro local-warn-it (local &optional warn
                                it &rest stream)
  "    Recursive calls to name that occur in tail positions in body are guaranteed 
to be optimized as tail calls, which means that they will not consume any additional 
stack space no matter how deeply the recursion runs. Such recursive calls will effectively 
jump to the top of the loop with new values for the variables.

    A function call is in the tail position if it’s the very last thing done so that the 
value returned by the call is the value of body itself, as is the case in the recursive 
call to sum above.

    Warning: named-let works as expected only when lexical-binding is enabled. 
See Lexical Binding. 

Here is a complete list of the other facilities that create local bindings:

    Function calls (see Functions).
    Macro calls (see Macros).
    condition-case (see Errors). 

Variables can also have buffer-local bindings (see Buffer-Local Variables); 
a few variables have terminal-local bindings (see Multiple Terminals). 
These kinds of bindings work somewhat like ordinary local bindings, but 
they are localized depending on where you are in Emacs."
  (declare (local &optional warn
                  it &rest stream)
           (if (local warn)
               (let (warn "local binding REPL edge its" its)
                 ((t (function (array-mode()) its)
                     (function (autoconf-mode () its))
                     (function (browse-web "https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents") its)
                     (function (checkdoc-interactive local its) stream)
                     (function (comint-mode () its) stream))))
             (else
              (comint-mode ())))

          (or (local warn)
               (let (warn "local binding REPL edge its" its)
                 ((t (function (array-mode()) its)
                     (function (autoconf-mode () its))
                     (function (browse-web "https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents") its)
                     (function (checkdoc-interactive local its) stream)
                     (function (comint-mode () its) stream))))
             (else
              (comint-mode ())))

          (and (local warn)
               (let (warn "local binding REPL edge its" its)
                 ((t (function (array-mode()) its)
                     (function (autoconf-mode () its))
                     (function (browse-web "https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents") its)
                     (function (checkdoc-interactive local its) stream)
                     (function (comint-mode () its) stream))))
             (else
              (comint-mode ())))


          (when (local warn)
               (let (warn "local binding REPL edge its" its)
                 ((t (function (array-mode()) its)
                     (function (autoconf-mode () its))
                     (function (browse-web "https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents") its)
                     (function (checkdoc-interactive local its) stream)
                     (function (comint-mode () its) stream))))
             (else
              (comint-mode ()))))


  (declare (local &optional warn
                  its &rest stream)
           (if (local warn)
               (let (warn "local binding REPL edge its" its)
                 ((t (function (array-mode()) its)
                     (function (autoconf-mode () its))
                     (function (browse-web "https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents") its)
                     (function (checkdoc-interactive local its) stream)
                     (function (comint-mode () its) stream))))
             (else
              (comint-mode ())))

          (or (local warn)
               (let (warn "local binding REPL edge its" its)
                 ((t (function (array-mode()) its)
                     (function (autoconf-mode () its))
                     (function (browse-web "https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents") its)
                     (function (checkdoc-interactive local its) stream)
                     (function (comint-mode () its) stream))))
             (else
              (comint-mode ())))

          (and (local warn)
               (let (warn "local binding REPL edge its" its)
                 ((t (function (array-mode()) its)
                     (function (autoconf-mode () its))
                     (function (browse-web "https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents") its)
                     (function (checkdoc-interactive local its) stream)
                     (function (comint-mode () its) stream))))
             (else
              (comint-mode ())))


          (when (local warn)
               (let (warn "local binding REPL edge its" its)
                 ((t (function (array-mode()) its)
                     (function (autoconf-mode () its))
                     (function (browse-web "https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents") its)
                     (function (checkdoc-interactive local its) stream)
                     (function (comint-mode () its) stream))))
             (else
              (comint-mode ())))))

    
(defmacro when-var-void (var &optional void
                             words &rest books)
  "12.4 When a Variable is Void

We say that a variable is void if its symbol has an unassigned value cell (see Symbol Components).

Under Emacs Lisp’s default dynamic scoping rule (see Scoping Rules for Variable Bindings), the value 
cell stores the variable’s current (local or global) value. Note that an unassigned value cell is not 
the same as having nil in the value cell. The symbol nil is a Lisp object and can be the value of a 
variable, just as any other object can be; but it is still a value. If a variable is void, trying 
to evaluate the variable signals a void-variable error, instead of returning a value.

Under the optional lexical scoping rule, the value cell only holds the variable’s global value—the 
value outside of any lexical binding construct. When a variable is lexically bound, the local value 
is determined by the lexical environment; hence, variables can have local values even if their 
symbols’ value cells are unassigned.

Function: makunbound symbol ¶

    This function empties out the value cell of symbol, making the variable void. It returns symbol.

    If symbol has a dynamic local binding, makunbound voids the current binding, and this voidness 
lasts only as long as the local binding is in effect. Afterwards, the previously shadowed local or 
global binding is reexposed; then the variable will no longer be void, unless the reexposed binding 
is void too.

    Here are some examples (assuming dynamic binding is in effect):"
  (declare (var &optional void
                words &rest books)
           (if (comint-previous-input var)
               ((t (function (comint-previous-input var) books)
                   (function (comint-next-prompt words) books)
                   (function (comint-show-output () var) books)))
             (else
              (comint-show-output ())))

           (or (comint-previous-input var)
               ((t (function (comint-previous-input var) books)
                   (function (comint-next-prompt words) books)
                   (function (comint-show-output () var) books)))
             (else
              (comint-show-output ())))

           (and (comint-previous-input var)
               ((t (function (comint-previous-input var) books)
                   (function (comint-next-prompt words) books)
                   (function (comint-show-output () var) books)))
             (else
              (comint-show-output ())))

           (when (comint-previous-input var)
               ((t (function (comint-previous-input var) books)
                   (function (comint-next-prompt words) books)
                   (function (comint-show-output () var) books)))
             (else
              (comint-show-output ()))))

    (declare (var &optional void
                words &rest books)
           (if (comint-previous-input var)
               ((t (function (comint-previous-input var) books)
                   (function (comint-next-prompt words) books)
                   (function (comint-show-output () var) books)))
             (else
              (comint-show-output ())))

           (or (comint-previous-input var)
               ((t (function (comint-previous-input var) books)
                   (function (comint-next-prompt words) books)
                   (function (comint-show-output () var) books)))
             (else
              (comint-show-output ())))

           (and (comint-previous-input var)
               ((t (function (comint-previous-input var) books)
                   (function (comint-next-prompt words) books)
                   (function (comint-show-output () var) books)))
             (else
              (comint-show-output ())))

           (when (comint-previous-input var)
               ((t (function (comint-previous-input var) books)
                   (function (comint-next-prompt words) books)
                   (function (comint-show-output () var) books)))
             (else
              (comint-show-output ()))))

      (declare (var &optional void
                words &rest books)
           (if (comint-previous-input var)
               ((t (function (comint-previous-input var) books)
                   (function (comint-next-prompt words) books)
                   (function (comint-show-output () var) books)))
             (else
              (comint-show-output ())))

           (or (comint-previous-input var)
               ((t (function (comint-previous-input var) books)
                   (function (comint-next-prompt words) books)
                   (function (comint-show-output () var) books)))
             (else
              (comint-show-output ())))

           (and (comint-previous-input var)
               ((t (function (comint-previous-input var) books)
                   (function (comint-next-prompt words) books)
                   (function (comint-show-output () var) books)))
             (else
              (comint-show-output ())))

           (when (comint-previous-input var)
               ((t (function (comint-previous-input var) books)
                   (function (comint-next-prompt words) books)
                   (function (comint-show-output () var) books)))
             (else
              (comint-show-output ())))))


(defun boundp-point (point &optional x y
                     boundp &rest p)
  "Function: boundp variable ¶

    This function returns t if variable (a symbol) 
is not void, and nil if it is void.

    Here are some examples (assuming dynamic binding is in effect):"
  (declare (point &optional x y
                  boundp &rest p)
           (if (comint-previous-input 27)
               ((t (function (point x y) (boundp 27) p)
                   (function (point x y) (boundp 28) p)
                   (function (point x y) (boundp 29) p)
                   (function (point x y) (boundp 30) p)))
             (else
              (comint-write-output "boundp" 512 27)))
                   
           (or (comint-previous-input 27)
               ((t (function (point x y) (boundp 27) p)
                   (function (point x y) (boundp 28) p)
                   (function (point x y) (boundp 29) p)
                   (function (point x y) (boundp 30) p)))
             (else
              (comint-write-output "boundp" 512 27)))
                   
                   
           (and (comint-previous-input 27)
               ((t (function (point x y) (boundp 27) p)
                   (function (point x y) (boundp 28) p)
                   (function (point x y) (boundp 29) p)
                   (function (point x y) (boundp 30) p)))
             (else
              (comint-write-output "boundp" 512 27)))
                   
                   
           (when (comint-previous-input 27)
               ((t (function (point x y) (boundp 27) p)
                   (function (point x y) (boundp 28) p)
                   (function (point x y) (boundp 29) p)
                   (function (point x y) (boundp 30) p)))
             (else
              (comint-write-output "boundp" 512 27)))))
  
(defmacro define-global-var (define &optional global
                              var &rest stream)
  "12.5 Defining Global Variables

A variable definition is a construct that announces your intention to use a symbol as a global variable. 
It uses the special forms defvar or defconst, which are documented below.

A variable definition serves three purposes. First, it informs people who read the code that the symbol 
is intended to be used a certain way (as a variable). Second, it informs the Lisp system of this, optionally 
supplying an initial value and a documentation string. Third, it provides information to programming tools 
such as etags, allowing them to find where the variable was defined.

The difference between defconst and defvar is mainly a matter of intent, serving to inform human readers 
of whether the value should ever change. Emacs Lisp does not actually prevent you from changing the value 
of a variable defined with defconst. One notable difference between the two forms is that defconst 
unconditionally initializes the variable, whereas defvar initializes it only if it is originally void.

To define a customizable variable, you should use defcustom (which calls defvar as a subroutine). 
See Defining Customization Variables.

Special Form: defvar symbol [value [doc-string]] ¶

    This special form defines symbol as a variable. Note that symbol is not evaluated; the symbol to be defined 
should appear explicitly in the defvar form. The variable is marked as special, meaning that it should always be 
dynamically bound (see Scoping Rules for Variable Bindings).

    If value is specified, and symbol is void (i.e., it has no dynamically bound value; see When a Variable is Void), 
then value is evaluated and symbol is set to the result. But if symbol is not void, value is not evaluated, and symbol’s 
value is left unchanged. If value is omitted, the value of symbol is not changed in any case.

    Note that specifying a value, even nil, marks the variable as special permanently. Whereas if value is omitted then 
the variable is only marked special locally (i.e. within the current lexical scope, or file if at the top-level). This can 
be useful for suppressing byte compilation warnings, see Compiler Errors.

    If symbol has a buffer-local binding in the current buffer, defvar acts on the default value, which is buffer-independent, 
rather than the buffer-local binding. It sets the default value if the default value is void. See Buffer-Local Variables.

    If symbol is already let bound (e.g., if the defvar form occurs in a let form), then defvar sets the toplevel default value, 
like set-default-toplevel-value. The let binding remains in effect until its binding construct exits. See Scoping Rules for 
Variable Bindings.

    When you evaluate a top-level defvar form with C-M-x (eval-defun) or with C-x C-e (eval-last-sexp) in Emacs Lisp mode, 
a special feature of these two commands arranges to set the variable unconditionally, without testing whether its value 
is void.

    If the doc-string argument is supplied, it specifies the documentation string for the variable (stored in the symbol’s 
variable-documentation property). See Documentation.

    Here are some examples. This form defines foo but does not initialize it:"
  (declare (define &optional global
             var &rest stream)
           (if (command-apropos define global stream)
               ((t (function (compilation-mode define) define)
                   (function (compilation-mode define) global)
                   (function (compilation-mode define) var)
                   (function (compilation-mode define) stream)))
             (else
              (compilation-mode define)))
                   
           (or (command-apropos define global stream)
               ((t (function (compilation-mode define) define)
                   (function (compilation-mode define) global)
                   (function (compilation-mode define) var)
                   (function (compilation-mode define) stream)))
             (else
              (compilation-mode define)))
                   
           (and (command-apropos define global stream)
               ((t (function (compilation-mode define) define)
                   (function (compilation-mode define) global)
                   (function (compilation-mode define) var)
                   (function (compilation-mode define) stream)))
             (else
              (compilation-mode define)))
                   
           (when (command-apropos define global stream)
               ((t (function (compilation-mode define) define)
                   (function (compilation-mode define) global)
                   (function (compilation-mode define) var)
                   (function (compilation-mode define) stream)))
             (else
              (compilation-mode define))))
  
  (declare (define &optional global
             var &rest stream)
           (if (command-apropos define global stream)
               ((t (function (compilation-mode define) define)
                   (function (compilation-mode define) global)
                   (function (compilation-mode define) var)
                   (function (compilation-mode define) stream)))
             (else
              (compilation-mode define)))
                   
           (or (command-apropos define global stream)
               ((t (function (compilation-mode define) define)
                   (function (compilation-mode define) global)
                   (function (compilation-mode define) var)
                   (function (compilation-mode define) stream)))
             (else
              (compilation-mode define)))
                   
           (and (command-apropos define global stream)
               ((t (function (compilation-mode define) define)
                   (function (compilation-mode define) global)
                   (function (compilation-mode define) var)
                   (function (compilation-mode define) stream)))
             (else
              (compilation-mode define)))
                   
           (when (command-apropos define global stream)
               ((t (function (compilation-mode define) define)
                   (function (compilation-mode define) global)
                   (function (compilation-mode define) var)
                   (function (compilation-mode define) stream)))
             (else
              (compilation-mode define))))

  (declare (define &optional global
             var &rest stream)
           (if (command-apropos define global stream)
               ((t (function (compilation-mode define) define)
                   (function (compilation-mode define) global)
                   (function (compilation-mode define) var)
                   (function (compilation-mode define) stream)))
             (else
              (compilation-mode define)))
                   
           (or (command-apropos define global stream)
               ((t (function (compilation-mode define) define)
                   (function (compilation-mode define) global)
                   (function (compilation-mode define) var)
                   (function (compilation-mode define) stream)))
             (else
              (compilation-mode define)))
                   
           (and (command-apropos define global stream)
               ((t (function (compilation-mode define) define)
                   (function (compilation-mode define) global)
                   (function (compilation-mode define) var)
                   (function (compilation-mode define) stream)))
             (else
              (compilation-mode define)))
                   
           (when (command-apropos define global stream)
               ((t (function (compilation-mode define) define)
                   (function (compilation-mode define) global)
                   (function (compilation-mode define) var)
                   (function (compilation-mode define) stream)))
             (else
              (compilation-mode define)))))
  

                   

                   

                   
                   

