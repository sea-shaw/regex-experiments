# Type-Safe Regex API
This repo contains my initial implementation of a type-safe matching API for the Oregano regex library.
It implements the `r` string interpolator which constructs a `Regex[?]` from a string, determining its type from the number of capture groups and the operators used on them.
This allows the compiler to catch unreachable cases in `match` expressions and warn the user when a certain case is not handled.
## Typechecking
```scala
val regex: Regex[(String, String)] = r"(a)"
-- [E007] Type Mismatch Error: -------------------------------------------------
1 |val regex: Regex[(String, String)] = r"(a)"
  |                                     ^^^^^^
  |                Found:    experiments.macros.regex.Regex[String]
  |                Required: experiments.macros.regex.Regex[(String, String)]
```
## Unreachable Cases
```scala
val regex = r"(a)(b)"
"abc" match { case regex(x, y, z) => }
-- [E030] Match case Unreachable Error: ----------------------------------------
2 |"abc" match { case regex(x, y, z) => }
  |                         ^
  |                         Unreachable case
```
## Exhaustivity Warning
```scala
val regex = r"(a)?"
regex.unapply("").map { case Some(x) => } 
-- [E029] Pattern Match Exhaustivity Warning: ----------------------------------
2 |regex.unapply("").map { case Some(x) => }
  |                        ^
  |                        match may not be exhaustive.
  |
  |                        It would fail on pattern case: None
  |-----------------------------------------------------------------------------
```
## Options
```scala
val regex: Regex[Option[String]] = r"(a)?"
```
## Alternatives
```scala
val dateOrTime: Regex[Either[(String, String, String), (String, String)] = r"(\d\d)-(\d\d)-(\d\d\d\d)|(\d\d):(\d\d)"
"22-09-2026" match {
  case dateOrTime(Left(day, month, year)) => ...
  case dateOrTime(Right(hour, minute))    => ...
}
```
