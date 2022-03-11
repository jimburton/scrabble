# Slides based on the book

This is how I produce Beamer slides from the org files with `pandoc`:

```
pandoc --pdf-engine=xelatex -V theme:Boadilla -V mainfont="DejaVu Sans" -V sansfont="DejaVu Sans Mono" -t beamer part1.org -o part1.pdf
```
