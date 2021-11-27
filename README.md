# People vs. Algorithms: Data Ethics in the 21st Century

This is the source code for the course "People vs. Algorithms: Data Ethics in the 21st Century," taught in the Department of Statistics at Columbia University, in Spring 2021. 

**Please refer to the course website for more information: https://data-ethics.jonreeve.com**

## Repository Contents

 - content/index.md - The course syllabus, in Markdown
 - content/references.bib - The course bibliography, in Biblatex
 - content/chicago-author-date.csl - A custom CSL file for formatting bibliographic citations as they appear in a syllabus. Based on Chicago style.
 - dist/ - The output directory. This is where the HTML files end up. 
 - src/ - The Haskell source code for building the site
 - default.nix - The Nix file which specifies the dependencies needed. Loads the cabal file. 
 - course-data-ethics.cabal - The Cabal file which actually specifies the Haskell dependencies. 

## Technical Details 

**Nota bene: students need not do any of this. These are just instructions for building the website from scratch.**

This site is created in Haskell, using Shake, Pandoc, and Tufte-CSS.

To build this site manually, ensure you have the the [Nix package manager](https://nixos.org/nix/) installed, and then run:

```bash
nix-shell --run 'cabal run'
```

To serve it on a local webserver: 

```
nix-shell --run 'cabal run course-data-ethics -- serve' 
```

(You can omit `nix-shell --run` if you have `direnv` installed, since the included .envrc automatically runs `nix-shell` for you.)


