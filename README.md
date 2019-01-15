# pandoc by examples

This repository contains examples of Pandoc usage.
All the documents are written in Markdown and compiled to PDF, DOCX and ODT.

- The folder `agreement` contains an example of how an agreement document could
  have been written in Markdown. Uses `pandoc` to generate a PDF, DOCX and ODT versions.
- The folder `book` contains a link to an example that generates a book in epub format.
- The folder `filters` contains an example of how to use `pandoc` filters.
- The folder `math` contains an example of how to write math code and produce an HTML page.
- The folder `research-paper` contains a published paper written mostly in
  Markdown, and generates a PDF. To deal better with the formatting, we go through
  an intermediate step: `Markdown  ---->  LaTeX ----> PDF`.
- The folder `slides` contains an example of slides written in Markdown, and
  generate a `revealjs` presentation.
- The folder `writing` contains various examples using math, bullet lists and simple things.
- The folder `zenformatting` contains the implementation of a `pandoc` filter
  that applies many of the formatting rules described in ["Zen of eBook Formatting: A
  Step-by-step Guide To Format eBooks for Kindle and EPUB"](https://www.amazon.com/Zen-eBook-Formatting-Step-step-ebook/dp/B00KJAH4HS), automatically, in
  constrast to the manual approach used by the author.
