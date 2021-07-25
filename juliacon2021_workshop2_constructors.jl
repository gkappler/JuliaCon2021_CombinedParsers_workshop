# # Part 2: CombinedParser Constructors
using CombinedParsers                   ## JuliaCon 2021


using TextParse

# ### AnyChar
@doc AnyChar

# ### ValueMatcher

# #### CharIn
@doc CharIn

# #### CharNotIn
@doc CharNotIn

# #### Unicode classes
@doc CombinedParsers.unicode_classes

# #### ValueMatcher internal

@doc CombinedParsers.ismatch

# calls

@doc CombinedParsers._ismatch

# ### Repeat

@doc Repeat

# #### Optional

@doc Optional

# #### Lazy

@doc Lazy

# #### Repeat stop
# using `NegativeLookahead`

@doc Repeat_stop

# #### Repeat until

@doc Repeat_until

# #### join
# Oriented at `Base.join` (keyword arg `last` not supported yet)
@doc Base.join


# ### Atomic

@doc Atomic

# is useful to suppress iterating matches

parse_all(Sequence(Repeat(AnyChar()), Repeat(AnyChar())),"ab") |> collect

# in this case forcing first `Repeat` to be greedy

parse_all(Sequence(Atomic(Repeat(AnyChar())), Repeat(AnyChar())),"ab") |> collect

# ### Sequence

@doc Sequence

# Simplifying sequences is used for flattening `*` operator calls 
# (will probably be refactored to keyword in `Sequence`)

@doc sSequence

# Flexible `Sequence` state:
@doc CombinedParsers.sequence_state_type

# `Sequence` results in a `Tuple` by default:
@doc CombinedParsers.sequence_result_type

# ### Either
# More later for recursive parsing
@doc Either

# ### Assertions
@doc AtStart

#

@doc AtEnd

#

@doc Always

#

@doc Never

# #### Look behind

@doc PositiveLookbehind

#

@doc NegativeLookbehind

# #### Look ahead
@doc PositiveLookahead

#

@doc NegativeLookahead

# ### Logging and Side-Effects

@doc with_name

#

@doc @with_names

#

@doc log_names

#

@doc with_log

#

@doc with_effect


# ### Caching match states
@doc CombinedParsers.WithMemory


# ## Recursive Parsers

# # Arithmetical terms for rational numbers
# Parsing is reading and transforming a sequence of characters.
# This example reads and evaluates arithmetical terms for rational numbers.
# Subterms can use algebraic operators `+-*/` that will be evaluated with 
function evaluate( (start, operation_values) )
    aggregated_value::Rational{Int} = start
    for (op,val) in operation_values
        aggregated_value = eval( Expr(:call, Symbol(op), 
			              aggregated_value, val
			              ))
    end
    return aggregated_value
end
evaluate( (0, [ ('+',1), ('+',1) ]) ), 
evaluate( (1, [ ('*',2), ('*',3) ]) )


# Term expressions are sequences of subterms interleaved with operators.
# Sub terms are [`Either`](@ref) fast `TextParse.Numeric(Int)` integer numbers, converted to `Rational{Int}`,
subterm = Either{Rational{Int}}(Any[]);
# A subterm can also be a nested term in parentheses
mult = evaluate |> join(subterm, CharIn("*/"), infix=:prefix )
term = evaluate |> join(mult,    CharIn("+-"), infix=:prefix )
parenthesis = Sequence(2,'(',term,')')

push!(subterm, parenthesis)
push!(subterm, TextParse.Numeric(Int))


term("(1+2)/5")

# Term expressions are sequences of subterms interleaved with operators.
# Sub terms are [`Either`](@ref) fast `TextParse.Numeric(Int)` integer numbers, converted to `Rational{Int}`,
@syntax subterm = Either{Rational{Int}}(Any[map(Rational{Int},parser(TextParse.Numeric(Int)))]);
# A subterm can also be a nested term in parentheses
@syntax for parenthesis in subterm
    mult = evaluate |> join(subterm, CharIn("*/"), infix=:prefix )
    adds = evaluate |> join(mult,    CharIn("+-"), infix=:prefix )
    Sequence(2,'(',adds,')')
end;



# This `CombinedParser` definition in 5,5 lines is sufficient for doing arithmetics:
# [`Base.join`](@ref)(x,infix; infix=:prefix) is shorthand for `x `[`(*)`](@ref)` `[`Repeat`](@ref)`( infix * x  )`,
# and `f |> parser` is shorthand for [`map`](@ref)(f,parser)`.
@syntax term = adds;
# registers a `@term_string` macro for parsing and transforming.
term"(1+2)/5"

# The defined `CombinedParser` `term` can be used as a function for colorful logging of the parsing process.
term("1/((1+2)*4+3*(5*2))",log = [:parenthesis])
# [Is every rational answer ultimately the inverse of a universal question in life?](https://en.wikipedia.org/wiki/Phrases_from_The_Hitchhiker%27s_Guide_to_the_Galaxy#Answer_to_the_Ultimate_Question_of_Life,_the_Universe,_and_Everything_(42))
#
# The parser representation can be printed as a tree
term

# ### Benchmarks
# Parsing times for Int, operators, brackets are
using BenchmarkTools
@benchmark match(term,"(1+2)/5") 

# in unfair benchmark-comparison with the more expressive Julia syntax parser
@benchmark Meta.parse("(1+2)/5")

# Parsing and transforming (here `eval`)
@benchmark term("(1+2)/5") 

# compared to Julia 
@benchmark eval(Meta.parse("(1+2)/5"))



# recursive parsers can be built with Either
@doc push!

# new options can also take precendence
@doc pushfirst!

# The `@syntax for option in either` can also be used
@doc @syntax



# ## EBNF support
# BNF parser draft:
# - left recursion draft not published
# If you want to invest time or resources I am very happy to collaborate.









#nb # %% [markdown] {"slideshow": {"slide_type": "notes"}}
# Parsing is ubiquitous in all software that reads data.
# Countless parsing software exists.
# So why another parsing package?
# 
# I learned julia, decided to commit to the language,
# and so I had to write another bunch of small and larger parsers.
# Julia is young, and no existing package met my needs:
# - reuse regexps I wrote in my professional life, hundreds or thousands, I don't know.
# - reduce time spent building parsers.
# I extended TextParse with `Base.PCRE` to its limit,
# added backtracking,
# and released CombinedParsers alpha back in April.
#
# Julia's compiler and types are amazing, three weeks of optimization and pattern matching made the library competitive with PCRE C library, for many patterns.
# Straightforwardly, parsing type patterns can be optimized.
# I was and am determined to master Julia's parametric types and compiler optimizations.


#nb # %% Outline [markdown] {"slideshow": {"slide_type": "subslide"}}
# # Presentation Outline

#nb # %% Outline [markdown] {"cell_style": "split"}
# #### 1. Regular Expressions
# - PCRE-compliance
# - building blocks
# - inter-operability
#
# #### 2. Design and Standards
# - brevity ← type inference
# - iteration combinatorics
# - custom parser API

#nb # %% Outline [markdown] {"slideshow": {"slide_type": "fragment"},"cell_style": "split"}
# #### 3. Performance
# - PCRE benchmarks
# - parametric types
# - dispatch optimization
# - prefix trees
# - memoization

#nb # %% [markdown] {"slideshow": {"slide_type": "slide"}}
# # Regular Expressions
# the standard to parse (`Base.PCRE` C library).
# #### **Construct** `CombinedParsers` from `Regex` string:

speaker = "Gregor Kappler
psychometric scientist and programmer, independent";

using CombinedParsers.Regexp

import CombinedParsers.Regexp: word, words, whitespace # hide

person_background = Sequence(
    :name => !words, whitespace, :lastname => !word, "\n",
    :background => !words)

r = re"(?<name>[\w\h]+)\h+(?<lastname>\w+)\n(?<background>[\w\h]+)"
# #### **Understand structure** of `Regex` side-by-side with `CombinedParsers` API.


#nb # %% `match` [markdown] {"slideshow": {"slide_type": "subslide"}}
# ## PCRE compliance: `match` API
# Use `CombinedParsers`
# like `Base.RegexMatch`
m = match(r, speaker)
m[:name], m.stop

# Transform strings into any Julia type e.g. `NamedTuple`
parse(person_background, speaker)

#nb # %% [markdown] {"slideshow": {"slide_type": "notes"}}
# When captures in `Regex` repeatedly match, only the last match is captured.
# 
# With `CombinedParsers` you can
# transform  with `map` a String into any Julia representation with nesting and capturing all repeated values.
#
# Julia `result_type` inference does make
# parsers brief without redundant type definitions.
# In fact, a CombinedParser defines the types of a data domain from the syntax definition.


#nb # %% [markdown] {"slideshow": {"slide_type": "slide"}}
# ## PCRE compliance: Features

#nb # %% [markdown] {"cell_style": "split"}
# ✅ 3071 on 972 patterns,<br/>
# ❌ 25 on 17 patterns <br/>
# (PCRE C library unit tests).

#nb # %% [markdown] {"slideshow": {"slide_type": "fragment"},"cell_style": "split"}
# **not supported**<br/>
# ❌ `ACCEPT, SKIP, COMMIT, THEN, PRUNE, \K`
# <br/>
# ❌ capture, lookaheads in lookbehinds

#nb # %% [markdown] {"cell_style": "split"}
# **Characters**<br/>
# ✅ escapes, character types
# <br/>
# ✅ character ranges (`[]`)
# 
# **Basics**<br/>
# ✅ sequences, `|`, `?`
# <br/>
# ✅ (lazy) repetitions
# <br/>
# ✅ atomic groups
# <br/>
# ✅ options

#nb # %% [markdown] {"cell_style": "split"}
# **Groups**<br/>
# ✅ non-capturing `(?:)`
# <br/>
# ✅ capturing groups `()`
# <br/>
# ✅ backreferences `\1` & subroutines `(?1)`
# <br/>
# ✅ conditional expressions
#
# **Assertions**<br/>
# ✅ assertions `\z\Z\b\B^$`...
# <br/>
# ✅ look-aheads & -behinds


#nb # %% [markdown] {"slideshow": {"slide_type": "slide"}}
# # Regex ↔ Building Blocks
# #### `@re_str::CombinedParser` conforming with `Base.PCRE` [spec](https://www.pcre.org/original/doc/html/pcrepattern.html)
# - reading PCRE regular expressions and
# - resulting in a `CombinedParser` usable as plug-in `Regex` replacement


#nb # %% Design and Standards [markdown] {"slideshow": {"slide_type": "slide"}}
# # Brevity
# Julia's type inference
# saves you time and inconsistencies of writing type information repeatedly during data-preparation.

#nb # %% [markdown] {"slideshow": {"slide_type": "notes"}}
# For me parsing was often tedious, involving redundant definitions.
# Even fluent, parsing stays some sort pain, yes or no?
# `CombinedParsers` feel much more fun at greater output.
# `CombinedParsers` requires very little little repeated information,
# Julia type inference is so useful!

#nb # %% Design and Standards [markdown] {"slideshow": {"slide_type": "slide"}}
# # Iteration combinatorics
# #### lazy `parse_all`
[ e for e in parse_all(re"^(a|ab|b)+$","abab") ]

#nb # %% [markdown] {"slideshow": {"slide_type": "notes"}}
# If a parsing is not uniquely defined
# Julia's `iterate` API lazily `parse`s with backtracking combinatorics,

# #### lazy `match_all`
[ (e.start,get(e)) for e in match_all(re"(a|ab|b)+","ab") ]


#nb # %% [markdown] {"slideshow": {"slide_type": "slide"}}
# ## Parametric Types & Dispatch Optimization
# Parsers and states have custom types.
# This allows method dispatch
# to insert fast matching code for special-cases.
#
# This is used in lazy construction of results based on an appropriate (sub-)state.
#
# Supports currently
rp = Repeat(AnyChar())
match(rp,"0.1.2.3").state

# State is just the chars matched

match(rp,"0.1.2.3")[3]

# and
sp = Sequence(AnyChar(),AnyChar(),AnyChar(),AnyChar())
match(sp,"0.1.2.3").state

# State is `MatchState`

match(sp,"0.1.2.3")[3]

# The implementation shows how state is managed in CombinedParsers: <https://github.com/gkappler/CombinedParsers.jl/blob/master/src/lazy.jl>

# Another Example: custom palindrome parser API in [docs](https://gkappler.github.io/CombinedParsers.jl/dev/man/example-palindromes/).

# Optimization is ongoing.

#nb # %% Optimizing [markdown] {"slideshow": {"slide_type": "slide"}}
# # Prefix Trees

#nb %% {"slideshow": {"slide_type": "notes"}}
using Random
using BenchmarkTools
s = [ randstring(5) for _ in 1:5000 ];

#nb # %% [markdown] {"slideshow": {"slide_type": "notes"}}
# Matching one of 5000 `String`s in `s` is slow in PCRE (5 characters each):

re = Regex(join(s,"|"));
median(@benchmark match($re,$s[end]))

#nb # %% [markdown] {"slideshow": {"slide_type": "notes"}}
# PCRE fails when matching any of 10000 Strings.
# `CombinedParsers` dispatches `Either(::Vector{<:AbstractString})`
# to create an `Either{Trie{Char}}` (no state machines)
# with a fast `_iterate` implementation, and no limit to the number of strings.

pc = Either(s);
median(@benchmark match($pc,$s[end]))



#nb # %% [markdown] {"slideshow": {"slide_type": "slide"}}
# # CombinedParsers Packages
# - [Tries.jl](https://github.com/gkappler/Tries.jl) backend of fast prefix-tree matching (see [docs](https://gkappler.github.io/CombinedParsers.jl/dev/man/example-either-trie/))
# - [CombinedParserTools.jl](https://github.com/gkappler/CombinedParserTools.jl) for re-useable parsers, markup and annotation types.
# - [WikitextParser.jl](https://github.com/gkappler/WikitextParser.jl) for [wikitext syntax](https://en.wikipedia.org/wiki/Help:Wikitext), with markup types.
# - OrgmodeParser.jl for [org mode](https://orgmode.org/) syntax.



#nb # %% {"slideshow": {"slide_type": "notes"}}
# If you want to invest in creating a message broker with `CombinedParsers`, I will gladly collaborate.
# If you want to work with `CombinedParsers`, I will gladly provide professional support.
# If you are writing your own recursive `CombinedParser` and seek inspiration, you might find these comprehensive examples interesting.


#nb # %% [markdown] {"slideshow": {"slide_type": "slide"}}
# # `CombinedParsers` Design 
# - **fast**: optimizable with Julia method dispatch, generated functions,
# - **brief**: type-inference defines the domain data types,
# - **inter-operable**: composable with `Regex`, `TextParse`...
# - **general**: provides flexible public API for matching, parsing, iteration


#nb # %% [markdown] {"slideshow": {"slide_type": "fragment"}}
# ### Next
# - Syntax freeze (no `Char`)
# - Optimization experiments
# - error tracing (debugging?)

