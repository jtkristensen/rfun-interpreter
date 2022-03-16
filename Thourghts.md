
# About the name.

As opposed to `Janus`, which allows for expressions that are not
bidirectional in its inputs and outputs.  `RFun` seems to have this
bidirectionality, which seems to be stronger property than being reversible.
First, I want to define bidirectionality and reversibility, and if we the
former is stronger, and we really need it to be efficient, it would be more
concise to call it the language `BCPL` for `B`idirectional `C`oncurrent
`P`rogramming `L`anguage.

# Todo.

Some things are initially unnessesary (such as a type checker). We will deal
with introducing reversible concurrency/actors first, and then deal with
such things later. However, it is nice to have a list of such things, as the
ideas for them might pop up during development of the main features.

- [ ] A type system?
- [ ] Typed communication?
- [ ] Unspawn.
- [ ] Unbox.
- [ ] A typeconstuctor per Recive statement?
- [ ] Two different semantics for unrecieve/unsend.
- [ ] What will the first paper be about?
