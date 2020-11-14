---
geometry: top=2cm
---

Project Proposal: A Language for Securing Sensitive Data Against Spectre Attacks
===

David Young
---

Spectre attacks are carried out by observing microarchitectural changes that
result from the speculative execution of instructions. The result of such
attacks is that memory can be leaked. This memory could include sensitive
information, such as passwords.

It is frequently the case that the programmer will know ahead of time which
variables will store particularly sensitive data, as is the case with
password-entry fields. A hardware- and OS-level mechanism for programmers
to store data in memory which is resistant to some versions of Spectre has been
proposed in [@SpectreGuard].

Programming language tools which both provide access to these constructs and
provide a degree of static analysis that information does not cross this barrier
(from Spectre-resistant memory locations to vulnerable memory locations) is
desirable, to avoid Spectre attacks against particularly sensitive data.

One possible route to this goal is to create a deep embedded domain specific
language (eDSL) in a strongly, statically typed language like Haskell and then
generates C code which leverages the anti-Spectre memory mechanism given in
[@SpectreGuard]. Compared to the bare C-level anti-Spectre memory construct,
there is a two-fold advantage:

  1. Security-critical variables can be marked at a type-level
using Haskell's type system. This allows some security properties to be verified using
the GHC Haskell compiler's existing type checker.

  2. As this would be a
*deeply embedded* eDSL, the C code generator (and any passes that might be put
in place before the code generation stage) has full access to the abstract
syntax tree (AST) of programs written in the eDSL. This allows static analysis
passes to be naturally written as functions in standard Haskell, without the
need to interface with any existing compiler API.

Prior work regarding the use of strong, static types in a functional language to
limit access to certain variables at a programming language-level includes
[@LightweightIF; @FormalIF], as well as the recent work in [@Lifty].  Some of
this prior work is likely to be more heavyweight than necessary for solving the
immediate problem at hand. For example, in the system described in [@Lifty],
complex security policies can be expressed by the programmer as statements in a
decidable system of logic and it even has a repair engine which can
automatically patch some insecure programs as part of a compilation stage.

Possible limitations of the approach suggested in this proposal include:

  - The unfamiliarity many programmers have of Haskell compared to C. This can
    be mitigated to some extent by providing a nice, clear interface for the eDSL, so
    that programmers are only required to know about a relatively small subset
    of the language.  Additionally, this project could function as a
    proof-of-concept which could then be implemented in other programming
    languages.

  - To be entirely practical, a large subset of the C language might need to be
    reflected through the Haskell eDSL, potentially including a new C foreign
    function interface. However, this does not limit the ability of this project
    to function as a proof-of-concept for the constructs and analysis in
    question.

As a proof-of-concept project, these limitations are unlikely to present a
significant problem.

References
---

