# Senna (Overloaded Lists)

Lisp lists can be defined in four axioms as generalized elements of a list-model object internal to a category.

One can "horizontally categorize" these kind of objects to obtain what this library calls in the comments and docstrings **abstract list model** which provides Lisp with a feature that can be thought of as a kind of **semantic multiplicity**.

By **semantic multiplicity** I mean that one can use the same function to operate on different types as if they were lists according to methods defined as a particular list-model-category. Sort of automatic multi-method dispatch, which is actually the way its implemented. Making a higher version of `cons` wich is a kind of bifunctor from the list-model-category to itself.

This makes it so that consing numbers or lists like `(cons! 1 0)`, `(cons! 1 '())` makes sense, up to a particular list-model-category structure. This list-model-category structures are entirely defined and delimited **method dispatch schemes** without knowing if that term exists and wether I'm using it right or wrong.

This severly reduces the need for libraries that deal with different list-like structures, since most successor-encodable data types are... well that. One, clearly, can even `cons!` multi-dimensional data types.

This library provides utilities to define abstract list models for use in code, and a way to manage different abstract list models so that one can switch between them. So it kind of provides the interface for the entire 2-category of list-model-categories without, at this time, fully understanding what that means 😃.

Furthermore, one can remove one of the axioms to obtain weak list-model-categories, wich provide an interface to infinite data structure manipulation. Probably best served with lazy list-model-categories. This library implicitly supports those... I think...

# ⚠️ Warning

This library is clearly not complete.
