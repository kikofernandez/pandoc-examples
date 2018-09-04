Structure:

* Introduction to Pandoc
    * Installation and requirements
      * Examples
        * Website example
        * Reveal.js example
        * Agreement document example
      * Meta-information

This series is designed to help developers to produce documentation
in multiple formats from a single markup language (in this series Markdown).
We will show this using Pandoc.

In this first part of the Pandoc series I will guide you through the installation process (briefly),
show examples (that I have used in my lectures at Uppsala University, Sweden)
and we will end up with tips on
how to write documentation that is easy to port to other formats.
To do this last part,
I will argue about why the use of meta-information files
creates a separation of concerns between the content and the
meta-information[^meta] of your documentation.


[^meta]: Meta-information refers to the authors name, template used, bibliographic style, etc.

# Introduction to Pandoc

Pandoc is a command-line tool for converting files from
one markup language into another markup language.
Markup languages are languages that use tags to annotate
sections of the document in question. Commonly used markup
languages are Markdown, ReStructuredText, HTML, LaTex, ePub
or Microsoft Word docx among others.

In plain English, Pandoc allows you to convert a bunch of
files from one markup language into another one. Typical examples include
converting a Markdown file into a presentation, LaTeX, PDF or even ePub.

Now, lets continue with the installation process, examples and advanced
uses of Pandoc.

## Installation and requirements

Pandoc is already installed (by default) in most Linux distributions.
This tutorial uses `pandoc-2.2.3.2` and
`pandoc-citeproc-0.14.3`.  If you are not going to generate PDFs,
these two packages are enough. However, I recommend to also install `texlive`
or you will not be able to generate PDFs.
To install these programs, type the following command in the command-line prompt:

```
sudo apt-get install pandoc pandoc-citeproc texlive
```

(Installation instructions for other platforms can be found [here](http://pandoc.org/installing.html))

I highly recommend the installation of `pandoc-crossref`.
The easiest option is to download a [pre-built executable](https://github.com/lierdakil/pandoc-crossref/releases/tag/v0.3.2.1) but you can install it from the Haskell package manager ,`cabal`,
by typing:

```
cabal update
cabal install pandoc-crossref
```

If you need further information on the installation process, check out
the [installation information](https://github.com/lierdakil/pandoc-crossref#installation).


## Examples

In this first part, we are going to show the following examples: how to...

* produce a website from a LaTeX file containing math formulas,
* produce a reveal.js slideshow from a Markdown file and
* produce an agreement document that mixes Markdown and LaTeX.


### Website example

I found that one of the best cases where pandoc excels is
at displaying math formulas into different output format files. For instance,
lets take a LaTeX document, named `math.tex`, containing some math symbols
(written in LaTeX) and generate a website.

The `math.tex` document looks like:

```latex
% Pandoc math demos

$a^2 + b^2 = c^2$

$v(t) = v_0 + \frac{1}{2}at^2$

$\gamma = \frac{1}{\sqrt{1 - v^2/c^2}}$

$\exists x \forall y (Rxy \equiv Ryx)$

$p \wedge q \models p$

$\Box\diamond p\equiv\diamond p$

$\int_{0}^{1} x dx = \left[ \frac{1}{2}x^2 \right]_{0}^{1} = \frac{1}{2}$

$e^x = \sum_{n=0}^\infty \frac{x^n}{n!} = \lim_{n\rightarrow\infty} (1+x/n)^n$
```

We convert the LaTeX document into a website, named `mathMathML.html`,
by typing:

```
pandoc math.tex -s --mathml  -o mathMathML.html
```

The flags `-s` tells `pandoc` to generate a *standalone* website (instead of
a fragment, so it is going to include all dependencies in a single file) and
the `--mathml` forces Pandoc to convert the math in LaTeX
to MathML, which can be rendered by modern browsers.

![](https://raw.githubusercontent.com/kikofernandez/pandoc-examples/master/math/pandoc-math-demo.png)

The result of the website can be found
[here](http://pandoc.org/demo/mathMathML.html)

The code for this example can be found in this
[repository](https://github.com/kikofernandez/pandoc-examples/tree/master/math),
which contains a `Makefile` as well to make things even simpler.

### Reveal.js example

Generating simple presentations from a Markdown file is easy as well.
The slides contain top-level slides and nested slides within the top-level
one. The presentation can be controlled from the keyboard and
one can jump from one top-level slide to the next top-level slide or
show the nested slides on a per-top-level basis.
This structure is typical in HTML-based presentation frameworks.

Lets create a slide document, named `SLIDES`
([all the repository code is here](https://github.com/kikofernandez/pandoc-examples/tree/master/slides)).
The first thing one needs to add is the meta-information about the slides,
e.g. title, author and date, which are prepended by the `%` symbol:

```
% Case Study 3
% Kiko Fernandez Reyes
% Sept 27, 2017
```

This meta-information also creates your first slide.
To add more slides, you need to declare top-level slides using a Markdown heading H1
(line 5 in example below, [heading 1 in Markdown](https://daringfireball.net/projects/markdown/syntax#header)).

For example, if we want to create a presentation with title *Case Study* that
starts with a top-level slide with title *Wine Management System*,
we can expand the previous example and write:

```
% Case Study
% Kiko Fernandez Reyes
% Sept 27, 2017

# Wine Management System
```

To put content inside this top-level section, such as slides that
explain the idea for a new management system and its implementation,
we use a Markdown header H2. Lets expand the example and add
two more slides (lines 7 and 14 below, [heading 2 in Markdown](https://daringfireball.net/projects/markdown/syntax#header)):

* the first slide has as title *Idea* and shows an image of the Swiss flag,
* the second slide has the title *Implementation*


```
% Case Study
% Kiko Fernandez Reyes
% Sept 27, 2017

# Wine Management System

## <img src="img/SwissFlag.png" style="vertical-align:middle"/> Idea

## Implementation
```

We now have a top-level slide (`# Wine Management System`)  that contains
(inside) these to slides, the *Idea* and the *Implementation*.

Lets put some content in these two slides such as incremental bulleted lists.
To do this, you can simply create a Markdown list prepended by the symbol `>`.
Continuing with the example, we add a list with two items in the first slide (lines 9 -- 10) and
five items in the second slide (lines 16 -- 20):

```
% Case Study
% Kiko Fernandez Reyes
% Sept 27, 2017

# Wine Management System

## <img src="img/SwissFlag.png" style="vertical-align:middle"/> Idea

>- Swiss love their **wine** and cheese
>- Let's create a *simple* wine tracker system

![](img/matterhorn.jpg)

## Implementation

>- Bottles have a RFID tag
>- RFID reader (emits and read signal)
>- **Raspberry Pi**
>- **Server (online shop)**
>- Mobile app
```

In this example, we have also added an image of the Matterhorn mountain.
Your slides can be improved by using plain Markdown or adding
plain HTML.

To generate the slides,
Pandoc needs to point to the `reveal.js` library, for which we
include it in the same folder as the `SLIDES` file.
The command to generate the slides is:

```
pandoc -t revealjs -s --self-contained SLIDES \
-V theme=white -V slideNumber=true -o index.html
```

![](https://raw.githubusercontent.com/kikofernandez/pandoc-examples/master/slides/slides-result.png)

Pandoc uses the following flags:

* `-t revealjs` specifies that we are going to output `revealjs` presentation,
* `-s` tells pandoc to generate a standalone document,
* `--self-contained` produces the HTML with no external dependencies,
* `-V` is simply used to set the following variables:
    * `theme=white` sets the theme of the slideshow to `white`,
    * `slideNumber=true` shows the slide number
* `-o index.html` generates the slides in the file named `index.html`.

To make things simpler and not have to type this long command,
we create the following simple `Makefile`:

```
all: generate

generate:
    pandoc -t revealjs -s --self-contained SLIDES \
    -V theme=white -V slideNumber=true -o index.html

clean: index.html
    rm index.html

.PHONY: all clean generate
```

All the code can be found in [this repository](https://github.com/kikofernandez/pandoc-examples/tree/master/slides).


### Agreement document example

Lets say that you are preparing a document and, as things are nowadays,
some people would like to get the document in Microsoft Word docx,
some others use free software and would like an odp and others
a PDF. You do not have to use OpenOffice nor LibreOffice to generate a Microsoft Word docx
document or PDF. You can create your document in Markdown (with some bits
of LaTeX if you need more advanced formatting) and generate all these files.

In this example we proceed as before, declaring the meta-information of the document
which includes title, author and date:

```
% Contract Agreement for Software X
% Kiko Fernandez-Reyes
% August 28th, 2018
```

After that, we can write the document in Markdown (and add LaTeX
if we require some advanced formatting). For example, lets create a table
that needs to have some fixed separation space (declared in LaTeX with
`\hspace{3cm}`) and a line where a client and a contractor should sign
(declared in LaTeX with `\hrulefill`). After that, we add a table
written in Markdown.  We are going to create this document:

![](https://raw.githubusercontent.com/kikofernandez/pandoc-examples/master/agreement/advanced-formatting.png)

The code for the creation of the document is written below:

```
% Contract Agreement for Software X
% Kiko Fernandez-Reyes
% August 28th, 2018

...

### Work Order

\begin{table}[h]
\begin{tabular}{ccc}
The Contractor & \hspace{3cm} & The Customer \\
& & \\
& & \\
\hrulefill & \hspace{3cm} & \hrulefill \\
%
Name & \hspace{3cm} & Name \\
& & \\
& & \\
\hrulefill & \hspace{3cm} & \hrulefill \\
...
\end{tabular}
\end{table}

\vspace{1cm}

+--------------------------------------------+----------+-------------+
| Type of Service                            | Cost     |     Total   |
+:===========================================+=========:+:===========:+
| Game Engine                                | 70.0     | 70.0        |
|                                            |          |             |
+--------------------------------------------+----------+-------------+
|                                            |          |             |
+--------------------------------------------+----------+-------------+
| Extra: Comply with defined API functions   | 10.0     | 10.0        |
|        and expected returned format        |          |             |
+--------------------------------------------+----------+-------------+
|                                            |          |             |
+--------------------------------------------+----------+-------------+
| **Total Cost**                             |          | **80.0**    |
+--------------------------------------------+----------+-------------+
```

To generate the three different output formats for this document, we write a `Makefile`:

```
DOCS=contract-agreement.md

all: $(DOCS)
    pandoc -s $(DOCS) -o $(DOCS:md=pdf)
    pandoc -s $(DOCS) -o $(DOCS:md=docx)
    pandoc -s $(DOCS) -o $(DOCS:md=odp)

clean:
    rm *.pdf *.docx *.odp

.PHONY: all clean
```

As we can observe, lines 4 -- 7 contain the commands to generate different outputs.

If you would like to split your document into multiple small documents,
create multiple Markdown files and simply type the documents in the order
in which you would like them to appear:

```
pandoc -s introduction.md examples.md advanced-uses.md -o document.pdf
```

In this case, I have three documents: an introduction document, then examples and
finally advanced uses. These files will be merged together by Pandoc in the
specified order and produce a PDF named `document.pdf`.

## Meta-information

Writing a complex document is no easy task: you need to stick to a
set of rules that are independent from your content, such as using a specific template,
writing an abstract, embedding specific fonts and maybe even declaring keywords.
All of this has nothing to do with your content: simply put, it is meta-information.

Pandoc uses templates to generate different output formats. For instance, there is a
template for the LaTeX output, another for ePub, etc. These templates
have unfulfilled variables that are set with the meta-information given to Pandoc.
To find out which meta-information is available in the Pandoc templates, type:

```
pandoc -D FORMAT
```

For example, the template for LaTeX would be:

```
pandoc -D latex
```

Which outputs something along these lines:

```latex
...

$if(title)$
\title{$title$$if(thanks)$\thanks{$thanks$}$endif$}
$endif$
$if(subtitle)$
\providecommand{\subtitle}[1]{}
\subtitle{$subtitle$}
$endif$
$if(author)$
\author{$for(author)$$author$$sep$ \and $endfor$}
$endif$
$if(institute)$
\providecommand{\institute}[1]{}
\institute{$for(institute)$$institute$$sep$ \and $endfor$}
$endif$
\date{$date$}
$if(beamer)$
$if(titlegraphic)$
\titlegraphic{\includegraphics{$titlegraphic$}}
$endif$
$if(logo)$
\logo{\includegraphics{$logo$}}
$endif$
$endif$

\begin{document}
...
```

As you can observe, there is a `title`, `thanks`,  `author`,
`subtitle` and `institute` template
variables among many others. These are easily set using YAML metablocks.
In the example below, lines 1--5, we declared a YAML metablock and set some of
those variables (using the agreement example from before):

```
---
title: Contract Agreement for Software X
author: Kiko Fernandez-Reyes
date: August 28th, 2018
---

(continue writing document as in the previous example)
```

This works like a charm  and it is equivalent to the previous code:

```
% Contract Agreement for Software X
% Kiko Fernandez-Reyes
% August 28th, 2018
```

However, ties the meta-information to the content
i.e. Pandoc will always use this information to output files in the new format.
If you know you have to produce multiple file formats, then
you better be careful. For example,
what do you think that could happen if we had to produce the contract
in ePub and in HTML, and the ePub and HTML need specific and different
styling rules?

Lets consider the cases:

* If you simply try to embed the YAML variable `css: style-epub.css`
you would be excluding the one from the HTML version, **this does not work**.
* Duplicating the document is obviously **no a good solution** either,
changes in one version are not in sync with the other copy.
* You can add variables to the `pandoc` command-line as follows:

        pandoc -s -V css=style-epub.css document.md document.epub
        pandoc -s -V css=style-html.css document.md document.html

My personal opinion is that it is easy to overlook these variables from the command-line,
specially when you need to set tens of these (which can happen in more complex documents).
Now, if you put them all together under the same roof, a `meta.yaml` file, then
you only need to update or create a new meta-information file to produce
the desired output. You would then write:

```
pandoc -s meta-pub.yaml document.md document.epub
pandoc -s meta-html.yaml document.md document.html
```

This is a much cleaner version and all the meta-information can be updated from
a single file, without having to ever update the content of your document.
