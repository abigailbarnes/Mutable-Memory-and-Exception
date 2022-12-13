# Mutable-Memory-and-Exception


Homework 6 combines two recent topics: mutable memory and exceptions. These topics are treated separately in Chapters 13 and 14 in the text. (Please read both!) Combining these two basically unrelated features into the same language all at once is an extra challenge, because you have to grasp the semantics of mutable memory and the semantics of exceptions together at once. You will find, however, that these two distinct formalisms can be combined easily enough (as is the case with TaPL chapters, generally). As a result, this exercise should give you a working understanding of the "plug-and-play" nature of the book's separate topics (a more academic phrasing would be "compositional").

The framework for HW6 (see below) includes a scanner and parser, and implementations of fv and subst over the current Term data structure (I didn't think you needed to write those operations yet again). You have three main tasks to accomplish for this exercise: 1) implement a data structure to represent memory that supports allocation, reading, and writing, 2) implement a rewrite operation to desugar certain term forms in the surface language, and 3) implement a small-step evaluation step function. Notably, there is no type-checking this week. We may (depending on time constraints) circle back to write a type-checker for this language at some future date, but, for now, we won't.

Before discussing these three tasks in more detail, here is this week's language. The concrete syntax of types is this:

tau ::= N
      | B
      | U
      | [tau -> tau]
      | [Ref tau]
N, B, and U are Nat, Bool, and Unit, respectively; we also have function types and reference types.

The concrete syntax of terms is this:

t ::= 0
    | true
    | false
    | unit
    | (if t t t)
    | x
    | [x:tau.t]
    | (t t)
    | e
    | (raise t)
    | (try t t)
    | (eq t t)
    | (ref t)
    | (!t)
    | (t := t)
    | (seq t ... t)
    | (let x t t)

Most of these forms should be self-explanatory. The metavariable e stands in for exception constants. In the concrete syntax, these are written with prefix E_ and followed by one or more letters. For example, E_FileNotFound and E_NullPointer are exception constants. There is no predetermined set of these; as long as you write E_ followed by one or more letters, the scanner will understand it as an exception constant.

The form (seq t ... t) means that there are one or more terms following the seq keyword (the ellipsis ... is not to be taken literally). The expressions in a seq are to be evaluated one at a time, from left to right.

What constitutes a value in this language is given in the following supplied function:

def isV(t: Term): Boolean = t match {
  case TmZero | TmTrue | TmFalse | TmUnit 
     | TmAbs(_,_,_) | UAbs(_,_) | TmExn(_) | Loc(_) => true
  case _ => false
}
As per Chapter 14, an expression of the form (raise e) is a non-value normal form. It is not considered a stuck term. This means (raise e) creates a new category of normal form, neither a value nor stuck.

The task of implementing the Store type is to allow programs to allocate, read and write at runtime. Any kind of simple map data structure will do for this; you decide how a store is to be represented in code, and build operations accordingly. Note that locations (Loc terms) in Term carry an integer with them; that integer points to a particular memory location.

The rewriting/desugaring operation in HW6 should do three things: discard the types in abstractions; rewrite sequences into function applications; and rewrite let-expressions into function applications. Comments in the code framework below tell you where to look in the text for the relevant rewriting rules.

Finally, small-step evaluation rules for the present exercises are complicated by the facts that a) every evaluation rule needs to include store metavariables (the Greek letter μ in the formal systems), and b) we need to think about how raise expressions interact with every other term form. This means the basic complexity of the small-step evaluation system is effectively multiplied by the complexity of adding μ and by the inclusion of raise rules.

First, for the eq operation, you may depend on Scala's == operator to determine whether terms are equal or not once they are both values. Therefore, the small-step evaluation rules for eq say this: evaluate t1 until it's a value v1, then evaluate t2 until it's a value v2, then compare v1 and v2 with Scala's == to determine whether to step to true or false. This is complicated by the fact that eq must step to a raise if either t1 or t2 evaluates to a normal-form raise along the way.

Second, depending on which shell you are running in the terminal, you might run into issues writing the read operation (!r) in code at the command line. This is because, in bash and possibly other shells, a double-quoted string that includes the character ! will search the command history and substitute text accordingly, certainly not what you want to do in code. (Refer to https://www.thegeekstuff.com/2011/08/bash-history-expansion/.) Use single quotes to enclose your code at the command line instead of double quotes and you will avoid this issue.
