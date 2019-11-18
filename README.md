# Domain Modeling Made Functional - in ReasonML!

This repository contains an attempt to convert all the examples of the book "Domain Modeling Made Functional - Tackle Software Complexity with Domain-Driven Design and F#" into ReasonML code (BuckleScript).

I wanted to prove the point that functional DDD (Domain-Driven Design) can also be done in ReasonML, which makes sense when you think about the roots of F# and ReasonML being both in OCaml. Still, F# grew it's own feet since then, and is especially powered by the .NET ecosystem and Microsoft sheer financial- and manpower. It therefore _may_ have a better take at being used for writing enterprise software. Let's see if we can do the same in ReasonML via [BuckleScript](https://bucklescript.github.io/).

For the original F# source code please refer to the corresponding [The Pragmatic Programmer page](https://pragprog.com/titles/swdddf/source_code).

Similar to the original, the folder structure is as follows: `/src` contains the folders `Chapters`, `OrderTaking` and `OrderTakingEvolved`. For the sake of simplicity, I only put any of them in their own namespace module and did not make three different projects out of them. `Chapters` files are prefixed with `Ch_`, `OrderTaking` files are prefixed with `OT_` and `OrderTakingEvolved` files are prefixed with `OTE_` accordingly.

Every F# `.fs`and `.fsx` file has an equivalent `.re` counterpart. Besides BuckleScript itself I use the available `Belt` or `Js` modules, as well as `MongoDB` as an Document Storage System and `PostgreSQL` as a relational database. For serialization and deserialization, `bs-json` is used.

## Usage

To compile the whole project, just execute

```
npm run bs:world
```

or

```
yarn bs:world
```

and execute one of the entrypoints with `node`.

## Status

- [ ] Chapters (0 %)
- [ ] OrderTaking (60 %)
- [ ] OrderTakingEvolved (0 %)
