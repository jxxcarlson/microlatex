module Data.TestDoc exposing (text)


text =
    """


\\begin{title}
MicroLaTeX Technical Notes
\\end{title}

This is a \\i{test} and so is \\b{this}.

\\begin{theorem}
There are infinitely many primes $p \\equiv 1\\ mod\\ 4$
\\end{theorem}


\\begin{equation}
\\int_0^1 x^n dx = \\frac{1}{n+1}
\\end{equation}

"""



--| defs
--[lambda bi x [blue [i x]]]
