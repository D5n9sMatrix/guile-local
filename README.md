<!DOCTYPE html>
<html><!-- Created by GNU Texinfo 7.0.3, https://www.gnu.org/software/texinfo/ --><head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8">
<title>Local Variables (GNU Emacs Lisp Reference Manual)</title>

<meta name="description" content="Local Variables (GNU Emacs Lisp Reference Manual)">
<meta name="keywords" content="Local Variables (GNU Emacs Lisp Reference Manual)">
<meta name="resource-type" content="document">
<meta name="distribution" content="global">
<meta name="Generator" content="makeinfo">
<meta name="viewport" content="width=device-width,initial-scale=1">

<link rev="made" href="mailto:bug-gnu-emacs@gnu.org">
<link rel="icon" type="image/png" href="https://www.gnu.org/graphics/gnu-head-mini.png">
<meta name="ICBM" content="42.256233,-71.006581">
<meta name="DC.title" content="gnu.org">
<style type="text/css">
@import url('/software/emacs/manual.css');
</style>
</head>

<body lang="en">
<div class="section-level-extent" id="Local-Variables">
<div class="nav-panel">
<p>
Next: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Void-Variables.html" accesskey="n" rel="next">When a Variable is Void</a>, Previous: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Constant-Variables.html" accesskey="p" rel="prev">Variables that Never Change</a>, Up: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Variables.html" accesskey="u" rel="up">Variables</a> &nbsp; [<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents" title="Table of contents" rel="contents">Contents</a>][<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Index.html" title="Index" rel="index">Index</a>]</p>
</div>
<hr>
<h3 class="section" id="Local-Variables-1">12.3 Local Variables</h3>
<a class="index-entry-id" id="index-binding-local-variables"></a>
<a class="index-entry-id" id="index-local-variables"></a>
<a class="index-entry-id" id="index-local-binding"></a>
<a class="index-entry-id" id="index-global-binding"></a>

<p>Global variables have values that last until explicitly superseded
with new values.  Sometimes it is useful to give a variable a
<em class="dfn">local value</em>—a value that takes effect only within a certain
part of a Lisp program.  When a variable has a local value, we say
that it is <em class="dfn">locally bound</em> to that value, and that it is a
<em class="dfn">local variable</em>.
</p>
<p>For example, when a function is called, its argument variables
receive local values, which are the actual arguments supplied to the
function call; these local bindings take effect within the body of the
function.  To take another example, the <code class="code">let</code> special form
explicitly establishes local bindings for specific variables, which
take effect only within the body of the <code class="code">let</code> form.
</p>
<p>We also speak of the <em class="dfn">global binding</em>, which is where
(conceptually) the global value is kept.
</p>
<a class="index-entry-id" id="index-shadowing-of-variables"></a>
<p>Establishing a local binding saves away the variable’s previous
value (or lack of one).  We say that the previous value is
<em class="dfn">shadowed</em>.  Both global and local values may be shadowed.  If a
local binding is in effect, using <code class="code">setq</code> on the local variable
stores the specified value in the local binding.  When that local
binding is no longer in effect, the previously shadowed value (or lack
of one) comes back.
</p>
<a class="index-entry-id" id="index-current-binding"></a>
<p>A variable can have more than one local binding at a time (e.g., if
there are nested <code class="code">let</code> forms that bind the variable).  The
<em class="dfn">current binding</em> is the local binding that is actually in effect.
It determines the value returned by evaluating the variable symbol,
and it is the binding acted on by <code class="code">setq</code>.
</p>
<p>For most purposes, you can think of the current binding as the
innermost local binding, or the global binding if there is no local
binding.  To be more precise, a rule called the <em class="dfn">scoping rule</em>
determines where in a program a local binding takes effect.  The
default scoping rule in Emacs Lisp is called <em class="dfn">dynamic scoping</em>,
which simply states that the current binding at any given point in the
execution of a program is the most recently-created binding for that
variable that still exists.  For details about dynamic scoping, and an
alternative scoping rule called <em class="dfn">lexical scoping</em>, see <a class="pxref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Variable-Scoping.html">Scoping Rules for Variable Bindings</a>.  Lately Emacs is moving towards using lexical binding in
more and more places, with the goal of eventually making lexical
binding the default.  In particular, all Emacs Lisp source files and
the <samp class="file">*scratch*</samp> buffer use lexical scoping.
</p>
<p>The special forms <code class="code">let</code> and <code class="code">let*</code> exist to create local
bindings:
</p>
<dl class="first-deffn first-defspec-alias-first-deffn">
<dt class="deffn defspec-alias-deffn" id="index-let"><span class="category-def">Special Form: </span><span><strong class="def-name">let</strong> <var class="def-var-arguments">(bindings…) forms…</var><a class="copiable-link" href="#index-let"> ¶</a></span></dt>
<dd><p>This special form sets up local bindings for a certain set of
variables, as specified by <var class="var">bindings</var>, and then evaluates all of
the <var class="var">forms</var> in textual order.  Its return value is the value of
the last form in <var class="var">forms</var>.  The local bindings set up by <code class="code">let</code>
will be in effect only within the body of <var class="var">forms</var>.
</p>
<p>Each of the <var class="var">bindings</var> is either (i)&nbsp;a<!-- /@w --> symbol, in which case
that symbol is locally bound to <code class="code">nil</code>; or (ii)&nbsp;a<!-- /@w --> list of the
form <code class="code">(<var class="var">symbol</var> <var class="var">value-form</var>)</code>, in which case
<var class="var">symbol</var> is locally bound to the result of evaluating
<var class="var">value-form</var>.  If <var class="var">value-form</var> is omitted, <code class="code">nil</code> is used.
</p>
<p>All of the <var class="var">value-form</var>s in <var class="var">bindings</var> are evaluated in the
order they appear and <em class="emph">before</em> binding any of the symbols to them.
Here is an example of this: <code class="code">z</code> is bound to the old value of
<code class="code">y</code>, which is 2, not the new value of <code class="code">y</code>, which is 1.
</p>
<div class="example">
<div class="group"><pre class="example-preformatted">(setq y 2)
     ⇒ 2
</pre></div><pre class="example-preformatted">
</pre><div class="group"><pre class="example-preformatted">(let ((y 1)
      (z y))
  (list y z))
     ⇒ (1 2)
</pre></div></div>

<p>On the other hand, the order of <em class="emph">bindings</em> is unspecified: in the
following example, either 1 or 2 might be printed.
</p>
<div class="example">
<pre class="example-preformatted">(let ((x 1)
      (x 2))
  (print x))
</pre></div>

<p>Therefore, avoid binding a variable more than once in a single
<code class="code">let</code> form.
</p></dd></dl>

<dl class="first-deffn first-defspec-alias-first-deffn">
<dt class="deffn defspec-alias-deffn" id="index-let_002a"><span class="category-def">Special Form: </span><span><strong class="def-name">let*</strong> <var class="def-var-arguments">(bindings…) forms…</var><a class="copiable-link" href="#index-let_002a"> ¶</a></span></dt>
<dd><p>This special form is like <code class="code">let</code>, but it binds each variable right
after computing its local value, before computing the local value for
the next variable.  Therefore, an expression in <var class="var">bindings</var> can
refer to the preceding symbols bound in this <code class="code">let*</code> form.
Compare the following example with the example above for <code class="code">let</code>.
</p>
<div class="example">
<div class="group"><pre class="example-preformatted">(setq y 2)
     ⇒ 2
</pre></div><pre class="example-preformatted">
</pre><div class="group"><pre class="example-preformatted">(let* ((y 1)
       (z y))    ; <span class="r">Use the just-established value of <code class="code">y</code>.</span>
  (list y z))
     ⇒ (1 1)
</pre></div></div>

<p>Basically, the <code class="code">let*</code> binding of <code class="code">x</code> and <code class="code">y</code> in the
previous example is equivalent to using nested <code class="code">let</code> bindings:
</p>
<div class="example">
<pre class="example-preformatted">(let ((y 1))
  (let ((z y))
    (list y z)))
</pre></div>

</dd></dl>

<dl class="first-deffn first-defspec-alias-first-deffn">
<dt class="deffn defspec-alias-deffn" id="index-letrec"><span class="category-def">Special Form: </span><span><strong class="def-name">letrec</strong> <var class="def-var-arguments">(bindings…) forms…</var><a class="copiable-link" href="#index-letrec"> ¶</a></span></dt>
<dd><p>This special form is like <code class="code">let*</code>, but all the variables are bound
before any of the local values are computed.  The values are then
assigned to the locally bound variables.  This is only useful when
lexical binding is in effect, and you want to create closures that
refer to bindings that would otherwise not yet be in effect when using
<code class="code">let*</code>.
</p>
<p>For instance, here’s a closure that removes itself from a hook after
being run once:
</p>
<div class="example lisp">
<pre class="lisp-preformatted">(letrec ((hookfun (lambda ()
                    (message "Run once")
                    (remove-hook 'post-command-hook hookfun))))
  (add-hook 'post-command-hook hookfun))
</pre></div>
</dd></dl>

<a class="index-entry-id" id="index-dynamic-binding_002c-temporarily"></a>
<a class="index-entry-id" id="index-dynamic-let_002dbinding"></a>
<dl class="first-deffn first-defspec-alias-first-deffn">
<dt class="deffn defspec-alias-deffn" id="index-dlet"><span class="category-def">Special Form: </span><span><strong class="def-name">dlet</strong> <var class="def-var-arguments">(bindings…) forms…</var><a class="copiable-link" href="#index-dlet"> ¶</a></span></dt>
<dd><p>This special form is like <code class="code">let</code>, but it binds all variables
dynamically.  This is rarely useful—you usually want to bind normal
variables lexically, and special variables (i.e., variables that are
defined with <code class="code">defvar</code>) dynamically, and this is what <code class="code">let</code>
does.
</p>
<p><code class="code">dlet</code> can be useful when interfacing with old code that assumes
that certain variables are dynamically bound (see <a class="pxref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Binding.html">Dynamic Binding</a>), but it’s impractical to <code class="code">defvar</code> these variables.
<code class="code">dlet</code> will temporarily make the bound variables special, execute
the forms, and then make the variables non-special again.
</p></dd></dl>

<dl class="first-deffn first-defspec-alias-first-deffn">
<dt class="deffn defspec-alias-deffn" id="index-named_002dlet"><span class="category-def">Special Form: </span><span><strong class="def-name">named-let</strong> <var class="def-var-arguments">name bindings &amp;rest body</var><a class="copiable-link" href="#index-named_002dlet"> ¶</a></span></dt>
<dd><p>This special form is a looping construct inspired from the
Scheme language.  It is similar to <code class="code">let</code>: It binds the variables in
<var class="var">bindings</var>, and then evaluates <var class="var">body</var>.  However,
<code class="code">named-let</code> also binds <var class="var">name</var> to a
local function whose formal arguments are the variables in <var class="var">bindings</var>
and whose body is <var class="var">body</var>.  This allows <var class="var">body</var> to call itself
recursively by calling
<var class="var">name</var>, where the arguments passed to <var class="var">name</var> are used as the
new values of the bound variables in the recursive invocation.
</p>
<p>Example of a loop summing a list of numbers:
</p>
<div class="example lisp">
<pre class="lisp-preformatted">(named-let sum ((numbers '(1 2 3 4))
                (running-sum 0))
  (if numbers
      (sum (cdr numbers) (+ running-sum (car numbers)))
    running-sum))
⇒ 10
</pre></div>

<a class="anchor" id="Tail-recursion"></a><p>Recursive calls to <var class="var">name</var> that occur in <em class="emph">tail
positions</em> in <var class="var">body</var> are guaranteed to be optimized as <em class="emph">tail
calls</em>, which means that they will not consume any additional stack
space no matter how deeply the recursion runs.  Such recursive calls
will effectively jump to the top of the loop with new values for the
variables.
</p>
<p>A function call is in the tail position if it’s the very last thing
done so that the value returned by the call is the value of <var class="var">body</var>
itself, as is the case in the recursive call to <code class="code">sum</code> above.
</p>
<p><strong class="strong">Warning:</strong> <code class="code">named-let</code> works as expected only when
lexical-binding is enabled.  See <a class="xref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html">Lexical Binding</a>.
</p></dd></dl>

<p>Here is a complete list of the other facilities that create local
bindings:
</p>
<ul class="itemize mark-bullet">
<li>Function calls (see <a class="pxref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Functions.html">Functions</a>).

</li><li>Macro calls (see <a class="pxref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Macros.html">Macros</a>).

</li><li><code class="code">condition-case</code> (see <a class="pxref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Errors.html">Errors</a>).
</li></ul>

<p>Variables can also have buffer-local bindings (see <a class="pxref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer_002dLocal-Variables.html">Buffer-Local Variables</a>); a few variables have terminal-local bindings
(see <a class="pxref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Multiple-Terminals.html">Multiple Terminals</a>).  These kinds of bindings work somewhat
like ordinary local bindings, but they are localized depending on
where you are in Emacs.
</p>
</div>
<hr>
<div class="nav-panel">
<p>
Next: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Void-Variables.html">When a Variable is Void</a>, Previous: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Constant-Variables.html">Variables that Never Change</a>, Up: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Variables.html">Variables</a> &nbsp; [<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents" title="Table of contents" rel="contents">Contents</a>][<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Index.html" title="Index" rel="index">Index</a>]</p>
</div>





</body></html>