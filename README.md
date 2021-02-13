# eff

[![Join the chat at https://gitter.im/atnos-org/eff](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/atnos-org/eff?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Maven Central](https://img.shields.io/maven-central/v/org.atnos/eff_2.13.svg)](https://maven-badges.herokuapp.com/maven-central/org.atnos/eff_2.13)
![CI](https://github.com/atnos-org/eff/workflows/CI/badge.svg)

Extensible effects are an alternative to monad transformers for computing with effects in a functional way.
This library is based on the "free-er" monad and extensible effects described in
Oleg Kiselyov in [Freer monads, more extensible effects](http://okmij.org/ftp/Haskell/extensible/more.pdf).

You can learn more in the User Guide:

 - [your first effects](https://atnos-org.github.io/eff/org.atnos.site.Introduction.html)
 - [included effects: `Reader`, `Writer`, `Eval`, `State`,...](https://atnos-org.github.io/eff/org.atnos.site.OutOfTheBox.html)
 - [create your own effects](https://atnos-org.github.io/eff/org.atnos.site.CreateEffects.html)
 - [use Member implicits](https://atnos-org.github.io/eff/org.atnos.site.MemberImplicits.html)
 - [working with different effect stacks](https://atnos-org.github.io/eff/org.atnos.site.TransformStack.html)
 - [a tutorial similar to the cats' tutorial for Free monads](https://atnos-org.github.io/eff/org.atnos.site.Tutorial.html)

You can also check out [this presentation](https://bit.ly/eff_flatmap_2016) at flatMap Oslo 2016 ([slides](https://www.slideshare.net/etorreborre/the-eff-monad-one-monad-to-rule-them-all)).

## Installation

Eff is published for Scala 2.12 and 2.13. `eff` core is available for the JVM and ScalaJS. Sbt dependency:

```scala
// check maven badge above for latest version
libraryDependencies += "org.atnos" %% "eff" % "5.14.0"

// to write types like Reader[String, *]
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)

// to get types like Reader[String, *] (with more than one type parameter) correctly inferred for scala 2.12.x
scalacOptions += "-Ypartial-unification"
```

# Contributing

[eff](https://github.com/atnos-org/eff/) is a [Typelevel](https://typelevel.org) project. This means we embrace pure, typeful, functional programming,
and provide a safe and friendly environment for teaching, learning, and contributing as described in the [Scala Code of Conduct](https://www.scala-lang.org/conduct/).

Feel free to open an issue if you notice a bug, have an idea for a feature, or have a question about the code. Pull requests are also gladly accepted.

# Extensions

## Modules in this repository

- [scalaz](./scalaz)
- [monix](./monix)
- [doobie](./doobie)
- [twitter](./twitter) (deprecated)

## External

- [eff-zio](https://github.com/takayahilton/eff-zio) eff extension for ZIO effects.
