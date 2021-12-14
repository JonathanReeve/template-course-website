# Course Website Template

This is a template website for a university course, semantically marked-up by default.  

## Repository Contents

 - content/index.md - The course syllabus, in Markdown
 - content/references.bib - The course bibliography, in Biblatex
 - content/chicago-author-date.csl - A custom CSL file for formatting bibliographic citations as they appear in a syllabus. Based on Chicago style.
 - dist/ - The output directory. This is where the HTML files end up. 
 - src/ - The Haskell source code for building the site
 - default.nix - The Nix file which specifies the dependencies needed. Loads the cabal file. 
 - template-course-website.cabal - The Cabal file which actually specifies the Haskell dependencies. 

## Technical Details 

**Nota bene: students need not do any of this. These are just instructions for building the website from scratch.**

This site is created in Haskell, using Shake, Pandoc, and Tufte-CSS.

To build this site manually, ensure you have the the [Nix package manager](https://nixos.org/nix/) installed, and then run:

```bash
nix-shell --run 'cabal run'
```

To serve it on a local webserver: 

```
nix-shell --run 'cabal run template-course-website -- serve' 
```

(You can omit `nix-shell --run` if you have `direnv` installed, since the included .envrc automatically runs `nix-shell` for you.)


