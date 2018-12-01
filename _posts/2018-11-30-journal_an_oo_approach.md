---
published: true
---
## Finally starting to wrap my head around how to factor this

It always seemed like overkill to write classes in R because I never realized how easy R makes it to write modular code compared to other languages.

In Python, even with list/dict comprehension it still takes way more code to iterate over a data structure. So perhaps people use encapsulation because otherwise their scripts become too complex to maintain. So they write all the nested `for` loops they need to and hide them behind some accessor method that let's them do that task as a one-liner in the rest of their code.

Let me try that too.

So far so good. In [today's edits](https://github.com/UTHSCSA-CIRD/datafinisher/compare/7893283...dfb7a6f) I created a `DFMeta` object which will hold all information about a given CSV input file, mostly in `DFCol` objects that hold information about individual columns of that file. It's the `DFCol`s that will be responsible for not only constructing the column headers and metadata row for the output file but also for processing the input data and generating output values for their respective base columns.

It sucks to have to have a hundred different objects doing the same thing, but Python (and almost everything else) lacks the vectorized functions of R and forces you to choose between indexing by position and indexing by name in data structures. I could import Pandas but I am trying to avoid dependencies outside the standard library. I could write this in R, but that too would become an obstacle to deployment and would run slower. So instead I'm going to try what 'normal' people seem to be doing. It's good to learn new things.

Next: Areating a data structure that communicates user input from an interactive session (or simulates user input for testing). Also, the long-awaited multi-output-column extractor. And, of course, testing all this vaporware on the actual data rows to see what else I'm overlooking.
