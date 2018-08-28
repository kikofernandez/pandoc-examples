# Example of pandoc's power

This folder contains a research paper that mixes markdown syntax and LaTex.
The main idea was to move everything from LaTex to markdown but, due to lack
of time, I have fixed the parts that I considered more difficult and the others
can be worked out in the same way. For example, I have cross-referenced sections.
You can cross-reference figures and tables in the same way, although I have
not done those two.

# Structure

The `Makefile` compiles the markdown `main.md` file into `main.pdf`.

To separate concerns, we use the `meta.yaml` and the `main.md`.
`meta.yaml` contains meta-information such as, authors, format of the paper, etc.
`main.md` contains the information written in the paper.

With this simple separation, one creates a separation of concerns and can focus
on one thing at a time. If you would like to create an ePub version, you would
simply have to create a new `epub.yaml` file, add the required fields and
update the `Makefile` to create an ePub, instead of a PDF.
