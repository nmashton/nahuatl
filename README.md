# Classical Nahuatl grammar

## Intro

A [Grammatical Framework](http://www.grammaticalframework.org) library for the Classical Nahuatl language.

This early version of the library is already fairly complete for verb and noun inflectional morphology, as well as handling the core cases of verbal derivation (causatives, applicatives, and honorifics). The core paradigm-generating functions are "smart", for the most part able to guess all forms on the basis of a single principal part.

The very basics of Classical Nahuatl syntax are implemented in `GrammarNah.gf`, covering the "miniature grammar" detailed in the Grammatical Framework handbook.

Much of the basic lexicon is covered in `LexiconNah.gf`. The words that are missing are in general missing for "interesting" reasons (i.e. they are not handled by the basic paradigm functions, and I have been too lazy to code them in manually so far).

## Sources

This grammar implements the grammatical analysis of [Launey & Mackay 2011](http://books.google.com/books?id=nL5cMgEACAAJ&dq=0521732298&hl=en&sa=X&ei=2_BJUf-AIfW24AONhYCQBA&ved=0CFAQ6AEwBg). Additional lexical data is drawn from the University of Oregon's [Nahuatl Dictionary](http://whp.uoregon.edu/dictionaries/nahuatl/).

## License

The Classical Nahuatl grammar is released under the GNU GPL.