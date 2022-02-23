module Data.TestDoc exposing (text)


text =
    """


\\begin{title}
MicroLaTeX Technical Notes
\\end{title}

This is a \\i{test} and so is \\b{this}.

Pythagoras sez: $a^2 + b^2 = c^2$

\\begin{theorem}
There are infinitely many primes $p \\equiv 1\\ mod\\ 4$
\\end{theorem}


\\begin{equation}
\\int_0^1 x^n dx = \\frac{1}{n+1}
\\end{equation}

\\b{Groceries}

\\item
Eggs

\\item
Bacon

\\b{Errands}

\\numbered
Gas up car

\\numbered
Two bottles of wine


"""



--| defs
--[lambda bi x [blue [i x]]]
